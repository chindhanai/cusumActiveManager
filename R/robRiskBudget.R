#' @title Simple and Robust Risk Budgeting with Expected Shortfall
#'
#' @description We implement the tracking error allocation in both correlated and noncorrelated
#' alpha sources
#'
#' @details (TBA)
#'
#' @param returns matrix with a time series of returns for each strategy
#' @param rf a vector of the risk free rates in each time period, but could also be the returns of a common benchmark
#' @param ER a vector of exogeneously derived prospective expeted returns. Either IR or ER must be specified.
#' @param IR a vector of exogeneously derived prospective information ratios. Either IR or ER must be specified.
#' @param TE a vector of exogneously derived prospective volatilites (tracking errors). If it is not specified, it will be estimated from the time series of asset / strategy returns.
#' @param corMat Exogneously derived correlation matrix. If it is not specified, it will be estimated from the time series of asset / strategy returns.
#' @param ES the user's speculated expected shortfall. If it is not specified, it will be estimated from the time series of asset / strategy returns.
#' @param ESMethod the method used to compute ES from PerformanceAnalytics
#' @param corMatMethod the method used to estimate the robust correlation matrix, default = "auto"
#' @param targetVol the target volatility
#' @param shrink logical value that determines whether or not we want to shrink the expected returns to their grand mean
#' @param avgCor logical value that determines if we want to set each off-diagonal element to the average of all the off-diagonal elements
#' @param p specified probability used to compute ES
#'
#'
#' @return \code{robRiskBudget} returns a \code{list} containing the following objects:
#' \item{riskBudget}{The vector of final risk budgets in each asset after the optimization}
#' \item{excessReturns}{The achieved volatility from the risk budgets}
#' \item{avgOffDiag}{The difference between the achieved volatility and the targeted volatility}
#' \item{targetVol}{The matrix of risk budgets in each iteration columnwise}
#' \item{avgCor}{The matrix whose columns represent the vector of normalized distances in each iteration}
#' \item{TE}{The vector of tracking errors}
#' \item{ES}{The vector of estimated ES}
#' \item{modIR}{The vector od the modified IR by the fraction TEES}
#'
#' @author Thomas Philips, Chindhanai Uthaisaad
#'
#' @references Thomas Philips and Michael Liu. "Simple and Robust Risk Budgeting with Expected Shortfall",
#' The Journal of Portfolio Management, Fall 2011, pp. 1-13.
#'
#' @examples
#' data("RussellData")
#' RussellData = data
#' rf = RussellData[, 16]
#' robRiskData = RussellData[, 1:15]
#'
#' riskBudget = robRiskBudget(robRiskData, rf = rf, shrink = TRUE, avgCor = TRUE,
#' ESMethod = "historical", corMatMethod = "mcd")
#' robRiskBudget(robRiskData, shrink = TRUE, corMatMethod = "pairwiseQC", avgCor = FALSE)
#' robRiskBudget(robRiskData, shrink = TRUE, corMatMethod = "mcd", avgCor = TRUE)
#'
#' @export


robRiskBudget = function(returns, rf = 0, ER = NULL, IR = NULL, TE = NULL, corMat = NULL,
                         ES = NULL, ESMethod = c("modified", "gaussian", "historical"),
                         corMatMethod = c("auto", "mcd", "pairwiseQC", "pairwiseGK"),
                         targetVol = 0.02, shrink = FALSE, avgCor = FALSE,
                         p = 0.95){


  if (missing(returns)) {
    return("Missing arg: returns are missing with no default")
  }

  if (missing(ESMethod)) {
    ESMethod = ESMethod[1]
  }

  if (missing(corMatMethod)) {
    corMatMethod = corMatMethod[1]
  }

  if (!(corMatMethod %in% c("auto", "mcd","pairwiseQC"))) {
    return("Invalid arg: corMatMethod unidentified")
  }

  if (!(ESMethod %in% c("modified", "gaussian", "historical", "kernel"))) {
    return("Invalid arg: ESMethod unidentified")
  }

  if (!(is.null(ER) || is.null(IR))) {
    return("Invalid arg: IR and ER cannot be both fed to the function")
  }

  # Compute excess returns
  excessReturns = returns - as.vector(rf)
  k = length(colnames(excessReturns))

  if (length(IR) == 1) {
    IR = rep(IR, k)
  }

  if (length(ER) == 1) {
    ER = rep(ER, k)
  }

  if (length(TE) == 1) {
    TE = rep(TE, k)
  }

  if (!is.null(IR) && length(IR) != k) {
    return("Invalid arg: the length of the vector of IR is not the same as the number of assets")
  }

  if (!is.null(ER) && length(ER) != k) {
    return("Invalid arg: the length of the vector of expected returns is not the same as the number of assets")
  }

  if (!is.null(TE) && length(TE) != k) {
    return("Invalid arg: the length of the vector of volatilities is not the same as the number of assets")
  }


  # Compute the TE if TE is NULL
  if (is.null(TE)) {
    TE = apply(coredata(excessReturns), MARGIN = 2, FUN = sd, na.rm = TRUE)
  }

  # Compute the ER if ER is NULL
  if (is.null(ER)) {
    excessMu = apply(coredata(excessReturns), MARGIN = 2, FUN = mean, na.rm = TRUE)
  } else {
    excessMu = ER
  }

  # Compute the IR if IR = NULL
  if (is.null(IR)) {

    # Perform means shrinkage
    if (shrink == TRUE) {

      # The mean of excess mean returns used in the shrinkage
      centeredMean = mean(excessMu)

      # Compute the length of observations in each asset
      n = c()
      for (i in 1:length(TE)){
        n[i] = sum(!is.na(excessReturns[, i]))
      }

      # Compute common variance across the assets
      opt_MLE = optimize(function(var_hat) {var_hat_mle(as.vector(excessMu), as.vector(TE ^ 2), var_hat)},
                            interval = c(0, 1e6), maximum = TRUE)
      sigmaSq = opt_MLE$maximum

      # Update the excess mean returns
      centeredReturns = sqrt(n) * (excessMu - centeredMean)
      excessMu = centeredMean + (1 - (k - 3) * sigmaSq / c(centeredReturns %*% centeredReturns)) * (excessMu - centeredMean)
    }
    IR = excessMu / TE
  }

  # Compute the correlation matrix if corr = NULL
  if (is.null(corMat)) {
    corMat = covRob(data = coredata(excessReturns), corr = TRUE, estim = corMatMethod,
                    na.action = na.omit)$cov
  }

  # Average the off-diagonal element if avgCor = TRUE
  avgOffDiag = mean(corMat[col(corMat) < row(corMat)])
  if (avgCor == TRUE) {
    corMat = matrix(avgOffDiag, nrow = k, ncol = k)
    diag(corMat) = 1
  }

  # Compute the ES if ES = NULL
  if (is.null(ES)) {
    ETL = - apply(coredata(excessReturns), MARGIN = 2, FUN = ES, p = p,
                method = ESMethod)
  }
  TEES = TE / ETL

  # Modify the IR to incorporate the tail risk
  IRPrime = IR * TEES

  # Reallocate the risk budgets using the modified IR
  riskBudget =  as.vector(solve(corMat) %*% IRPrime) * (targetVol / sqrt(c(IRPrime %*% solve(corMat) %*% IRPrime)))

  results = list(riskBudget = riskBudget,
                 excessReturns = excessMu,
                 corMat = corMat,
                 avgOffDiag = avgOffDiag,
                 targetVol = targetVol,
                 avgCor = avgCor,
                 TE = TE,
                 ES = ETL,
                 modIR = IRPrime)
  class(results) <- "riskBudget"
  return(results)
}

# Computes common variance across the assets
# Used in the optimization of the common variance
var_hat_mle <- function(R, vars, var_hat){
  L = 1
  for(i in 1:length(R)){
    L = L - (((R[i] ^ 2) / vars[i]) * (vars[i] / (vars[i] + var_hat))) +
      log(vars[i] / (vars[i] + var_hat)) / 2
  }
  return(L)
}
