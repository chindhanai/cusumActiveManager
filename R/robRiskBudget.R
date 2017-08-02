#' @title Simple and Robust Risk Budgeting with Expected Shortfall
#'
#' @description This function immplements the Philips / Liu method to compute an optimal set of
#' risk budgets that acheive #' a target level of volatility, but with lower expected shortfall.
#' It works best with weakly correlated strategies, as one might #' find in absolute return
#' portfolios, and takes into account both the volatility and the tail risk of strategies to
#' createa portfolio with a targeted level of volatility but with a lower level of tail risk.
#'
#' In the absence of any constraints, mean-variance risk budgets are given by
#'
#' {\vec{\sigma} = \frac{{C}^{-1}}}{\sqrt{(IR)^T {C}^{-1} IR^}}\cdot \sigma_{Target}
#' where IR is the Information Ratio: IR = \frac{E[r]}{\sigma} and C is the correlation
#' (not covariance) matrix. In general, it is extremely hard to allocate Expected Shortfall
#' between strategies (or securities) in a way that achievees a target level of Expected
#' Shortfall for the portfolio. This algorithm finds a pragmatic middle way - it allocates
#' risk using volatility as the measure of risk, but uses Expected Shortfall to guide its
#' allocations.
#'
#' To do so, we start with the observation that Information Ratio is the ratio of return to
#' risk, and define tne Modified Information Ratio {(IR^\prime) = \frac{E[r]}{\ES} } to be the
#' ratio of Expected Return to Expected Shortfall. For distributions that are characterized by
#' a scale parameter, the Expected Shortfall will be a constant multiple of the standard
#' deviation, and the Modified Information Ratio is proportional to the Information Ratio, so
#' that the constant cancels out from the numerator and denominator and leaves the risk budgets
#' unchnaged.
#' {(IR^\prime) = \frac{E[r]}{ES} = IR \cdot frac{{\sigma} {ES}} = frac{IR}{frac{{ES} {\sigma} }}}.
#' We call {frac{{ES} {\sigma} }} the Tail Risk Ratio. we want to bias our risk budgets away
#' from #' strategies or securiites with high tail risk ratios, and towards startegies and
#' securities with #' low tail risk ratios. We therefore make an ad-hoc substitution and
#' rewrite the expression for the risk budgets as follows:
#'
#' {\vec{\sigma} = \frac{{C}^{-1}}}{\sqrt{(IR^\prime)^T {C}^{-1} IR^\prime}}\cdot \sigma_{Target}
#'
#' in some risk budgeing applications, the various strategies / securities are only weakly
#' correlated, and in these cases, the solution can be made even more robust by averaging the
#' off-diagonal entries in the correlation matrix, so that
#'
#' {\vec{\sigma} = \frac{\bar{C}^{-1}}}{\sqrt{(IR^\prime)^T \bar{C}^{-1} IR^\prime}}\cdot \sigma_{Target}
#'
#' This simple closed form solution with the inclusion of tail risk and with a stabilized
#' correlation matrix, hardly ever results in negative solutions, and there is no need in
#' practice to include a long-only constraint - we just round up to 0 on the few occasions when
#' a risk budget appears. However,in the general case,if we want to bound \sigma_i between
#' \sigma_i^U and \sigma_i^L, we either have to solve a full mean-variance optimization
#' (and have to ignore  tail risk) or create a simulation based or historical optimization.
#' Instead, we can approximate the problem using an iterative scheme that implements a
#' heuristic for addiing to or subracting from all risk budgets in a way that allows
#' the process to convergence in just one or two iterations
#'
#' @details This function starts by computing an unconstrained meean variance optimmal portfolio in
#' which the risk budgets are expressed as a function of the various information ratios
#' (Information Ratio = Excess Return / Volatility (Excess Return) ). This can be solved in closed form.
#' We then replace each Information Ratio with its corresponding Modified Information Ratio
#' (Modified Information Ratio = Excess Return / Expected Shortfall (Excess Return) ).
#' In the special case when all the returns are drawn from the same scale family (e.g. Gaussian),
#' ES is just a constant multiple of \sigma, and the solution is unchanged. But if some strategies have
#' a higher level of tail risk relative to others, their risk budget will decline. In effect, risk
#' is allocated away from strategies with high tail risk and to strategies with low tail risk.
#'
#'
#' @param returns A matrix with a time series of returns for each asset / strategy
#' @param rf A vector with the risk free rate in each time period; could also be the returns of a common benchmark
#' @param ER A vector of exogeneously derived prospective expected returns. Either IR or ER must be specified.
#' @param IR A vector of exogeneously derived prospective information ratios. Either IR or ER must be specified.
#' @param TE A vector of exogneously derived prospective volatilites (tracking errors). If it is not specified,
#' it will be estimated from the time series of asset / strategy returns.
#' @param corMat Exogneously derived correlation matrix. If it is not specified, it will be estimated
#' from the time series of asset / strategy returns.
#' @param ES The user's speculated expected shortfall. If it is not specified, it will be estimated
#' from the time series of asset / strategy returns using one of 3 user-selectable methods.
#' @param ESMethod The method used to compute ES (the methods are available in PerformanceAnalytics)
#' @param corMatMethod the method used to estimate the robust correlation matrix,
#' (the methods are available in library robust, and the default is "auto")
#' @param targetVol The target volatility
#' @param shrink Logical value that determines whether or not we want to shrink the expected returns toward their grand mean
#' Uses James-Stein shrinkage, but is applicable only when expected returns are estimated by averaging historical returns,
#' @param avgCor Logical value that determines if we want to set each off-diagonal element in the correlation matrix
#' to the average of all its off-diagonal elements
#' @param p User-specified confidence level for computing ES
#' @param lower A numeric or vector of lower bounds \vec{\sigma^L} of risk budgets. Default is 0.
#' @param upper A numeric or vector of upper bounds \vec{\sigma^L} of risk budgets. Default is 1.
#' @param K A tuning factor for the iterative algorithm. Default is 100
#' @param maxit A number of maximum iterations. Default is 50.
#' @param tol An accuracy of the estimation with respect to the targeted volatility. Default is {10^{-5}}
#'
#'
#' @return \code{robRiskBudget} returns a \code{list} containing the following objects:
#' \item{initialRiskBudget}{The vector of unconstrained risk budgets}
#' \item{finalRiskBudget}{The vector of final risk budgets for each asset}
#' \item{iterativeRiskBudget}{The matrix of risk budgets in each iteration columnwise}
#' \item{targetVol}{The target volatility for the portfolio}
#' \item{avgCor}{The average of all off-diagonal correlations}
#' \item{ER}{The average returns (shrunk if called for) or the user supplied expected returns}
#' \item{TE}{The estimated tracking errors or the user supplied tracking errors}
#' \item{ES}{The vector of estimated ESs or the user supplied ESs}
#' \item{IR}{The shrunk average IRs or the user supplied IRs}
#' \item{modIR}{The vector of modified IRs}
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
#' robRiskBudget(robRiskData, shrink = TRUE, corMatMethod = "mcd", avgCor = TRUE, upper = 0.0123)
#'
#' @export


robRiskBudget = function(returns = NULL, rf = 0, ER = NULL, IR = NULL, TE = NULL, corMat = NULL,
                         ES = NULL, ESMethod = c("modified", "gaussian", "historical"),
                         corMatMethod = c("auto", "mcd", "pairwiseQC", "pairwiseGK"),
                         targetVol = 0.02, shrink = FALSE, avgCor = FALSE,
                         p = 0.95, lower = 0, upper = 1, K = 100, maxit = 50, tol = 1e-5){


  if (missing(returns) && (missing(corMat) || missing(IR) || missing(ER) || missing(TE) || missing(ES))) {
    return("Invalid arg: returns are required when ER, IR, TE, corMat, or ES is missing")
  }

  corMatMethod = corMatMethod[1]
  if (!(corMatMethod %in% c("auto", "mcd","pairwiseQC"))) {
    return("Invalid arg: corMatMethod unidentified")
  }

  ESMethod = ESMethod[1]
  if (!(ESMethod %in% c("modified", "gaussian", "historical", "kernel"))) {
    return("Invalid arg: ESMethod unidentified")
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

  # Check input parameter for the bounding
  if (length(lower) == 1) {
    lower = rep(lower, k)
  }

  if (length(upper) == 1) {
    upper = rep(upper, k)
  }

  if (length(lower) != length(riskBudget)) {
    return("Invalid arg: lower bound length mismatch")
  }

  if (length(upper) != length(riskBudget)) {
    return("Invalid arg: upper bound length mismatch")
  }

  # Iteration zero: trim out-of-bound risk budgeting and assign the logical values to L and U
  # depending on whether the budgets hit the bounds or not
  # L = {i | L_i = 1} represents the set of indices of fixed risk budgets that hit the lower bounds
  # U = {i | U_i = 1} represents the set of indices of fixed risk budgets that hit the upper bounds
  adjRiskBudget = mapply(function(x, y, z) ifelse(x < y, y, ifelse(x > z, z, x)),
                         riskBudget, lower, upper)
  L = mapply(function(x, y, z) ifelse(x == y, 1, ifelse(x == z, 0, 0)),
             adjRiskBudget, lower, upper)
  U = mapply(function(x, y, z) ifelse(x == y, 0, ifelse(x == z, 1, 0)),
             adjRiskBudget, lower, upper)

  # Compute the ratio of the realized volatility to the targeted volatility
  d = sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget)) / targetVol

  # Set the first column of the matrix of iterations to be the original risk budgets
  iterativeRiskBudget = matrix(c(riskBudget, adjRiskBudget), nrow = k)
  directedMat = c()
  colnames(iterativeRiskBudget) = c("OriginalRB", "ConstrainedRB")


  # Iterative algorithm: while the difference of the targeted volatility and the realized volatility exceeds
  # the tolerance level, we iterate the algorithm
  count = 1
  while ((count <= maxit) && (tol <= abs(targetVol - sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget))))) {

    # Consider the sets of indices of the frozen risk budgets of U and L
    frozenU = which(U == 1)
    frozenL = which(L == 1)

    # Compute the vector of normalized distances of the risk budgets to the bound
    if (d < 1) {

      normDist =  sapply(1:k, function(i) {ifelse(i %in% frozenU, 0, 1 / (1 + K * max(0, (adjRiskBudget[i] - riskBudget[i])) / targetVol))})

    } else if (d > 1) {

      normDist =  sapply(1:k, function(i) {ifelse(i %in% frozenL, 0, 1 / (1 + K * max(0, (riskBudget[i] - adjRiskBudget[i])) / targetVol))})

    } else {
      return("The input risk budgets are optimal")
      break
    }

    directedMat = cbind(directedMat, normDist)

    # Solve for the constant mult that matches the portfolio volatility after the addition
    # to the targeted volatility
    sigmaSqP1 = c(adjRiskBudget %*% corMat %*% adjRiskBudget)
    sigmaSqP2 = c(normDist %*% corMat %*% normDist)
    corrP1P2 = c(normDist %*% corMat %*% adjRiskBudget)
    mult = (-2 * corrP1P2 + sqrt(4 * corrP1P2 ^ 2 - 4 * (sigmaSqP1 - targetVol ^ 2) * sigmaSqP2)) / (2 * sigmaSqP2)

    # Update the risk budgets and portfolio volatility
    adjRiskBudget = adjRiskBudget + mult * normDist
    adjRiskBudget = mapply(function(x, y, z) ifelse(x < y, y, ifelse(x > z, z, x)),
                           adjRiskBudget, lower, upper)
    L = mapply(function(x, y, z) ifelse(x == y, 1, ifelse(x == z, 0, 0)),
               adjRiskBudget, lower, upper)
    U = mapply(function(x, y, z) ifelse(x == y, 0, ifelse(x == z, 1, 0)),
               adjRiskBudget, lower, upper)

    # Combine columnwise the updated risk budget and update the ratio d
    iterativeRiskBudget = cbind(iterativeRiskBudget, adjRiskBudget)
    colnames(iterativeRiskBudget) = c(colnames(iterativeRiskBudget)[-(count+2)], paste("Iteration", count))
    d = sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget)) / targetVol
    count = count + 1
  }

  names(riskBudget) <- names(adjRiskBudget) <- rownames(iterativeRiskBudget) <- colnames(corMat) <- rownames(corMat) <- names(excessMu)

  results = list(initialRiskBudget = riskBudget,
                 finalRiskBudget = adjRiskBudget,
                 iterativeRiskBudget = iterativeRiskBudget,
                 excessReturns = excessMu,
                 corMat = corMat,
                 avgCor = avgOffDiag,
                 targetVol = targetVol,
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
