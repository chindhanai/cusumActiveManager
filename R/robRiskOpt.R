#' @title Robust Risk Budgeting Optimization
#'
#' @description This function conmputes an optimal set of risk budgets that acheive a target level of volatility,
#' but with a lower expected shortfall.
#'
#' @details This robust ooptimization algorithm takes into account both volatility and tail risk to create
#' a portfolio with a targeted level of volatility but with lower tail risk.
#' In the general case, and in the absence of any constraints, the risk budgets are given by
#'
#' {\vec{\sigma} = \frac{{C}^{-1}}}{\sqrt{(IR)^T {C}^{-1} IR^}}\cdot \sigma_{Target}
#' where IR is the Information Ratio: IR = \frac{E[r]}{\sigma} and C is the correlation (not covariance) matrix
#' In general, it is extremely hard to allocate Expected Shortfall between strategies (or securities)in a way that
#' achievees a target level of Expected Shortfall for the portfolio. This algorithm finds a pragmatic middle way -
#' it allocates risk using volatility as the measure of risk, but uses Expected Shortfall to guide its allocations.
#' To do so, we start with the observaction that Information Ratio is a ratio of return to risk, and define tne
#' Modified Information Ratio {(IR^\prime) = \frac{E[r]}{\ES} } to be the ratio of Expected Return to Expected Shortfall.
#' For distributions that are characterized by a scale parameter, the Expected Shortfall will be a constant multiple of the
#' standard deviation, and the Modified Information Ratio is proportional to the Information Ratio.
#' {(IR^\prime) = \frac{E[r]}{ES}  = IR \cdot frac{{\sigma} {ES}} = frac{IR}{frac{{ES} {\sigma} }}    }. We call
#' {frac{{ES} {\sigma} }} the Tail Risk Ratio. we want to bias our risk budgets away from strategies or securiites with
#' high tail risk ratios, and towards startegies and securities with low tail risk ratios. We therefore make an ad-hoc
#' substitution and rewrite the expression for the risk budgets as follows:
#'
#' {\vec{\sigma} = \frac{{C}^{-1}}}{\sqrt{(IR^\prime)^T {C}^{-1} IR^\prime}}\cdot \sigma_{Target}
#'
#' in some risk budgeing applications, the various strategies / securities are only weakly correlated, and in these cases
#' the solution can be made even more robust by averaging the off-diagonal entries in the correlation matrix, so that
#'
#' {\vec{\sigma} = \frac{\bar{C}^{-1}}}{\sqrt{(IR^\prime)^T \bar{C}^{-1} IR^\prime}}\cdot \sigma_{Target}
#'
#' This simple closed form solution with the inclusion of tail risk and with a stabilized correlation matrix,
#' hardly ever results in negative solutions, and there is no need in practice to include
#' a long-only constraint - we just round up to 0 on the few occasionswhen a negative risk budget appears. However,
#' In the genenal case,if we want to bound \sigma_i between \sigma_i^U and \sigma_i^L, we either have to solve a full mean-
#' covariance optimization (and have to ignore the tail risk) or create a simulation based or historical optimization.
#' Instead, we can solve the problem using an approximate iterative scheme, with convergence in just one or two iterations
#'
#'
#' @param riskBudgetObj a riskBidget object from \code{robRiskBudget} function
#' @param lower a numeric or vector of lower bounds \vec{\sigma^L} of risk budgets
#' @param upper a numeric or vector of upper bounds \vec{\sigma^L} of risk budgets
#' @param K a tuning factor for the iterative algorithm
#' @param maxit a number of maximum iterations
#' @param tol accuracy of the estimation with respect to the targeted volatility
#'
#' @return \code{robRiskOpt} returns a \code{list} containing the following objects:
#'
#' \item{adjRiskBudget}{The vector of final risk budgets in each asset after the optimization}
#' \item{volatility}{The achieved volatility from the risk budgets}
#' \item{difference}{The difference between the achieved volatility and the targeted volatility}
#' \item{iterativeRiskBudget}{The matrix of risk budgets in each iteration columnwise}
#' \item{directedMat}{The matrix whose columns represent the vector of normalized distances in each iteration}
#'
#' @author Thomas Phillips, Chindhanai Uthaisaad
#'
#' @examples
#' data("RussellData")
#' RussellData = data
#' rf = RussellData[, 16]
#' robRiskData = RussellData[, 1:15]
#'
#' riskBudget = robRiskBudget(robRiskData, rf = rf, shrink = TRUE, avgCor = TRUE,
#'                            ESMethod = "historical", corMatMethod = "mcd")
#  robRiskOpt(riskBudget, upper = 0.012, type = "allequal", tol = 1e-8)
#'
#' @export

robRiskOpt = function(riskBudgetObj, lower = 0, upper = 1, K = 100, maxit = 50, tol = 1e-5) {

  riskBudget = riskBudgetObj$riskBudget
  corMat = riskBudgetObj$corMat
  targetVol = riskBudgetObj$targetVol
  avgCor = riskBudgetObj$avgCor
  avgOffDiag = riskBudgetObj$avgOffDiag
  n = length(riskBudget)

  if (length(lower) == 1) {
    lower = rep(lower, n)
  }

  if (length(upper) == 1) {
    upper = rep(upper, n)
  }

  if (length(lower) != length(riskBudget)) {
    return("Invalid arg: lower bound length mismatch")
  }

  if (length(upper) != length(riskBudget)) {
    return("Invalid arg: upper bound length mismatch")
  }

  if (missing(type)) {
    type = type[1]
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
  iterativeRiskBudget = matrix(c(riskBudget, adjRiskBudget), nrow = n)
  directedMat = c()
  colnames(iterativeRiskBudget) = c("OriginalRB", "AdjustedRB")


  # Iterative algorithm: while the difference of the targeted volatility and the realized volatility exceeds
  # the tolerance level, we iterate the algorithm
  count = 1
  while ((count <= maxit) && (tol <= abs(targetVol - sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget))))) {

      # Consider the sets of indices of the frozen risk budgets of U and L
      frozenU = which(U == 1)
      frozenL = which(L == 1)

      # Compute the vector of normalized distances of the risk budgets to the bound
      if (d < 1) {

        normDist =  sapply(1:n, function(i) {ifelse(i %in% frozenU, 0, 1 / (1 + K * max(0, (adjRiskBudget[i] - riskBudget[i])) / targetVol))})

      } else if (d > 1) {

        normDist =  sapply(1:n, function(i) {ifelse(i %in% frozenL, 0, 1 / (1 + K * max(0, (riskBudget[i] - adjRiskBudget[i])) / targetVol))})

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

  return(list(adjRiskBudget = adjRiskBudget,
              volatility = sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget)),
              difference = abs(targetVol - sqrt(c(adjRiskBudget %*% corMat %*% adjRiskBudget))),
              iterativeRiskBudget = iterativeRiskBudget,
              directedMat = directedMat))

}
