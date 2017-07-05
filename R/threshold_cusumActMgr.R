#' @title Using Statistical Process Control to Monitor Active Management
#'
#' @description Compute the log-likelihood ratio given the performance measure,
#' which is the information ratios of the active managers' portfolios, to gauge
#' the performance of the active managers with respect to specific thresholds.
#' An object of class \code{"tsfm"} is returned.
#'
#' @details
#' It is hard to assess the performance of the active managers. Thus investors have to continually
#' estimate the current performance of their portfolios and rigorously reevaluate
#' each manager’s strategy. Change point detection, such as CUSUM, is closely related to the dynamic performance
#' measurement in sequential analysis and statistical process control (SPC), and
#' the sequential anaysis can speed up the detection of change in systems.
#'
#' CUSUM is a procedure to rapidly detect the changes in the mean of a noisy random
#' process and it is robust to the distribution of the portfolio returns. CUSUM offers
#' the user the fastest way to detect a change in performance and the best efficiency
#' over the Wald’s sequential probability ratio test (SPRT) and t-test of both stationary
#' performance and time-varying performance. In this case, IR is used as a measure of
#' performance as it incorporates both risk (portfolio’s tracking error) and return
#' (portfolio’s excess return relative to the benchmark). Also, IR variance
#' is lower than that of the Sharpe ratio and the Trynor ratio.
#'
#'
#' \subsection{Data Processing}
#'
#' @param portfolioName a character representing the name of the fund.
#' It is a required argument with no default
#' @param benchmarkName a character representing the name of the benchamark
#' It is a required argument with no default
#' @param data an xts object containing the columns \code{portfolioName} and
#' \code{benchmarkName} of monthly returns. This argument is required with no default.
#' @param upperIR a numeric value representing the information ratio of a
#' good performance. The default is set to 0.5
#' @param lowerIR a numeric value representing the information ratio of a
#' bad performance. The default is set to 0
#' @param lambda_in the exponential weighting constant when the data
#'  seems consistent with the current estimate of volatility.
#' The default is set to 0.1
#' @param lambda_out the exponential weighting constant when the data
#' seems inconsistent with the current estimate of volatility.
#' The default is set to 0.2
#' @param winsorize the numeric value, greater than 1, of standard deviations
#' at which we winsorize. The default is set to 4.
#' @param filterStd the logical value representing the filter of the estimated
#' standard deviations. The default is set to \code{FALSE}.
#'
#'
#' @return \code{cusumActMgr} returns a \code{list} containing the following xts objects:
#' \item{Logarithmic_Excess_Returns}{Logarithmic excess returns of
#' the fund relative to the benchmark}
#' \item{Annual_Moving_Average}{The vector of annual moving average returns}
#' \item{Tracking_Error}{The monthly tracking error of the logarithmic excess returns}
#' \item{Information_Ratios}{The vector of monthly information ratios}
#' \item{Lindley's_Recursion}{The vector  Lindley's recursion with a reset after the detection threshold (6.81) is passed.}
#' \item{Annualized_Cusum_IR}{The vector annualized CUSUM of the information ratios}
#' \item{Means}{The xts matrix of estimated means of the fund in the first columns,
#' the benchmark in the second column, and the excess logarithmic returns in the third column}
#' \item{Standard_deviations}{The xts matrix of estimated standard deviations of the fund in the first columns,
#' the benchmark in the second column, and the excess logarithmic returns in the third column.
#' It will not be filtered unless \code{filterStd = TRUE} is specified.}
#' \item{Protractor}{The xts matrix of the rays from IR = -3 in the first column
#' to IR = 3 in the seventh column used in the CUSUM IR as a protractor.}
#' \item{Excess_Volatility}{The annualized Standard deviations}
#'
#' @author Chindhanai Uthaisaad.
#'
#' @references
#' Philips, T. K., Yashchin, E. and Stein, D. M. (2003). “Using Statistical
#' Process Control to Monitor Active Managers”, Journal of Portfolio Management,
#' Fall 2003, pp. 86-94.
#' @export

############################< MAIN CODE >###############################
# This function simulates the threshold for Lindley's recursion

simulateARL = function(mu, Threshold, delta, h, EW_constant, Fixed_Sigma){
  N_Events          = 0
  Sum               = 0
  SumSq             = 0
  #Set it high so that it doesn't exit the loop right away
  ThreeSigmaOverMu  = delta + 1

  while(ThreeSigmaOverMu > delta){
    L         = 0
    N         = 0
    r_n_1     = 0
    sigma_n   = 1
    sigma_n_1 = 1

    while (L < Threshold) {
      r_n = rnorm(n = 1, mean = mu, sd = 1)
      if ((N > 1) && (Fixed_Sigma == 1)) {
        sigma_n = sqrt( EW_constant * sigma_n_1^2 +
                        (1 - EW_constant) * 0.5 * (r_n - r_n_1)^2 )
      } else {
        sigma_n = 1
      }

      IR_hat    = r_n / sigma_n_1
      sigma_n_1 = sigma_n
      r_n_1     = r_n
      L         = max(0, L - IR_hat + h)
      N         <- N + 1
    }

    N_Events <- N_Events + 1
    Sum      <- Sum + N
    SumSq    <- SumSq + N^2
    ARL <- Sum / N_Events

    Sigma = ifelse(N_Events < 10, 100000, sqrt( (SumSq - Sum^2 / N_Events) / (N_Events * (N_Events - 1))) )
    ThreeSigmaOverMu = 3 * Sigma / ARL
    print(ThreeSigmaOverMu)
  }

  #Std. deviation of the ARLs
  s = Sigma * sqrt(N_Events)
  return(c(ARL, s))
}

