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
#' \code{variable.selection="lars"} corresponds to least angle regression
#' using \code{\link[lars]{lars}} with variants "lasso" (default), "lar",
#' "stepwise" or "forward.stagewise". Note: If \code{variable.selection="lars"},
#' \code{fit.method} will be ignored.
#'
#' Argument \code{mkt.name} can be used to add market-timing factors to any of
#' the above methods. Please refer to \code{\link{fitTsfmMT}}, a wrapper to
#' \code{fitTsfm} for details.
#'
#' \subsection{Data Processing}{
#'
#' Note about NAs: Before model fitting, incomplete cases are removed for
#' every asset (return data combined with respective factors' return data)
#' using \code{\link[stats]{na.omit}}. Otherwise, all observations in
#' \code{data} are included.
#'
#' Note about \code{asset.names} and \code{factor.names}: Spaces in column
#' names of \code{data} will be converted to periods as \code{fitTsfm} works
#' with \code{xts} objects internally and colnames won't be left as they are.
#' }
#'
#' @param asset.names vector of asset names, whose returns are the dependent
#' variable in the factor model.
#' @param factor.names vector containing names of the factors.
#' @param mkt.name name of the column for market returns. Default is \code{NULL}.
#' @param rf.name name of the column for the risk free rate; if excess returns
#' should be calculated for all assets and factors. Default is \code{NULL}.
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object
#' containing the columns \code{asset.names}, \code{factor.names}, and
#' optionally, \code{mkt.name} and \code{rf.name}.
#' @param fit.method the estimation method, one of "LS", "DLS" or "Robust".
#' See details. Default is "LS".
#' @param variable.selection the variable selection method, one of "none",
#' "stepwise","subsets","lars". See details. Default is "none".
#' @param control list of control parameters. Refer to
#' \code{\link{fitTsfm.control}} for details.
#' @param ... arguments passed to \code{\link{fitTsfm.control}}
#'
#' @return \code{fitTsfm} returns an object of class \code{"tsfm"} for which
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist.
#'
#' The generic accessor functions \code{coef}, \code{fitted} and
#' \code{residuals} extract various useful features of the fit object.
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns
#' based on the fitted factor model.
#'
#' An object of class \code{"tsfm"} is a list containing the following
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of
#' class \code{lm} if \code{fit.method="LS" or "DLS"}, class \code{lmRob} if
#' the \code{fit.method="Robust"}, or class \code{lars} if
#' \code{variable.selection="lars"}.}
#' \item{alpha}{N x 1 data.frame of estimated alphas.}
#' \item{beta}{N x K data.frame of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{fitted}{xts data object of fitted values; iff
#' \code{variable.selection="lars"}}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the asset(s) and factor(s) returns.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{mkt.name}{mkt.name as input}
#' \item{fit.method}{fit.method as input.}
#' \item{variable.selection}{variable.selection as input.}
#' Where N is the number of assets, K is the number of factors and T is the
#' number of time periods.
#'
#' @author Chindhanai Uthaisaad.
#'
#' @references
#' Christopherson, J. A., Carino, D. R., & Ferson, W. E. (2009). Portfolio
#' performance measurement and benchmarking. McGraw Hill Professional.
#'
#' Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least angle
#' regression. The Annals of statistics, 32(2), 407-499.
#'
#' Hastie, T., Tibshirani, R., Friedman, J., Hastie, T., Friedman, J., &
#' Tibshirani, R. (2009). The elements of statistical learning (Vol. 2, No. 1).
#' New York: Springer.
#'
#' @seealso The \code{tsfm} methods for generic functions:
#' \code{\link{plot.tsfm}}, \code{\link{predict.tsfm}},
#' \code{\link{print.tsfm}} and \code{\link{summary.tsfm}}.
#'
#' And, the following extractor functions: \code{\link[stats]{coef}},
#' \code{\link[stats]{fitted}}, \code{\link[stats]{residuals}},
#' \code{\link{fmCov}}, \code{\link{fmSdDecomp}}, \code{\link{fmVaRDecomp}}
#' and \code{\link{fmEsDecomp}}.
#'
#' \code{\link{paFm}} for Performance Attribution.
#'
#' @examples
#' #Data Preprocessing
#' LargeCap = read.csv("largecap.csv", header = T, sep = "," , stringsAsFactors = FALSE)
#' LargeCap$DATE = as.yearmon(LargeCap$DATE, "%m/%d/%Y")
#' LargeCap = as.xts(LargeCap[,-1], order.by = LargeCap$DATE)
#' results = cusumActMgr(portfolioName = "ORCL", benchmarkName = "PG", data = LargeCap)
#' @export

# r = the logarithmic excess return in the current period
# mu0 = the mean excess return from the last time period
# sigma0 = the estimated volatility from the last time period.
#          If it is not available (as for example in the first period,
#          set it to 0, in which case, the mean
# win_level = the number of standard deviations at which we winsorize
#             (default: win_level =4)
# lambda = the exponential weighting constant (default: lambda = 0.1)
muEst = function(r, mu0, sigma0, win_level = 4, lambda = 0.1){
  #Winsorization
  if (abs(r - mu0) > win_level * sigma0 ) {
    r = mu0 + sign(r - mu0) * win_level * sigma0
  }
  return(lambda * r + (1 - lambda) * mu0)
}

# r = the logarithmic excess return in the current period
# mu0 = the mean excess return from the last time period
#       (this is often set to 0 in finance applications as the mean
#       is small on account of market efficiency)
# sigma0 = the estimated volatility from the last time period.
#          If it is not available (as for example in the first
#          period, set it to 0 or some initial estimate
# win_level = the number of standard deviations at which we
#             winsorize (default: win_level =4)
# lambda_in = the exponential weighting constant when the data
#             seems consistent with the current estimate of
#             volatility (default: lambda = 0.1)
# lambda_out = the exponential weighting constant when the data
#              seems inconsistent with the current estimate of
#              volatility (default: lambda = 0.2)

sigmaEst = function(r, mu0, sigma0, win_level = 4, lambda_in = 0.1,
                    lambda_out = 0.2){

  lambda = ifelse( (sigma0/win_level < abs(r-mu0)) && (abs(r-mu0) < win_level*sigma0), lambda_in, lambda_out)
  return(sqrt(lambda * (r-mu0)^2 + (1 - lambda) * sigma0^2))
}


cusumActMgr <- function(portfolioName, benchmarkName, data, upperIR = 0.5,
                      lowerIR = 0, lambda_in = 0.10, lambda_out = 0.20,
                      winsorize = 4, ...) {

  # record the call as an element to be returned
  this.call <- match.call()

  #Check the arguments validity
  if (missing(data) || !is.xts(data)) {
    stop("Invalid args: data must be an xts object")
  }

  if(missing(portfolioName) || !is.character(portfolioName)){
    stop("Invalid args: the portfolio returns must be a charactor string")
  }

  if(missing(benchmarkName) || !is.character(benchmarkName)){
    stop("Invalid args: the benchmark returns must be a charactor string")
  }

  if(winsorize < 1){
    stop("Invalid args: the winsorizing parameter should be greater than 1")
  }

  if(lambda_in < 0 || lambda_in >1 || lambda_out < 0 || lambda_out > 1){
    stop("Invalid args: the lambdas must be between 0 and 1")
  }

  #Obtain the return and benchmark
  portfolioReturns = data[,portfolioName]
  benchmarkReturns = data[,benchmarkName]
  n = length(portfolioReturns)

  if(n < 2){
    stop("Invalid args: the portfolio returns and benchmark returns must be of length at least 2")
  }

  if(n != length(benchmarkReturns)){
    stop("Invalid args: the portfolio returns and benchmark returns must be of the same length")
  }

  #Initialize logarithmic excess returns, IR, LLR and TE
  IR = cusumIR = xts(rep(0, n), order.by = index(portfolioReturns))
  priorMonth = as.yearmon(first(index(portfolioReturns)))-1/12
  index_TE = c(priorMonth, index(portfolioReturns))
  LR = xts(rep(0,n+1), order.by = index_TE)

  #Compute the Logarithmic Excess Returns
  logExcessReturns = log((1+portfolioReturns)/(1+benchmarkReturns))

  ######< Mean and Filtered Std of the fund and benchmark >#######
  Means =  uStds = fStds = matrix(0, ncol = 3, nrow = n+1)
  Means[1,1] = ifelse(n>=11, mean(portfolioReturns[1:11]), mean(portfolioReturns))
  Means[1,2] = ifelse(n>=11, mean(benchmarkReturns[1:11]), mean(benchmarkReturns))
  Means[1,3] = ifelse(n>=11, mean(logExcessReturns[1:11]), mean(logExcessReturns))
  uStds[1,1] = fStds[1,1] = ifelse(n>=6, 1.25 * median(abs(portfolioReturns[1:6])), 1.25 * median(abs(portfolioReturns)))
  uStds[1,2] = fStds[1,2] = ifelse(n>=6, 1.25 * median(abs(benchmarkReturns[1:6])), 1.25 * median(abs(benchmarkReturns)))
  uStds[1,3] = fStds[1,3] = ifelse(n>=6, 1.25 * median(abs(logExcessReturns[1:6])), 1.25 * median(abs(logExcessReturns)))


  #Update the means for the fund and benchmark
  for(i in 1:n){
    Means[i+1,1] = muEst(coredata(portfolioReturns[i]), Means[i,1], uStds[i,1], winsorize, lambda_in)
    Means[i+1,2] = muEst(coredata(benchmarkReturns[i]), Means[i,2], uStds[i,2], winsorize, lambda_in)
    Means[i+1,3] = muEst(coredata(logExcessReturns[i]), Means[i,3], uStds[i,3], winsorize, lambda_in)
  }

  #Update the unfiltered standard deviations for the fund and benchmark
  for(i in 1:n){
    uStds[i+1,1] = sigmaEst(coredata(portfolioReturns[i]), Means[i+1,1], uStds[i,1], winsorize, lambda_in, lambda_out)
    uStds[i+1,2] = sigmaEst(coredata(benchmarkReturns[i]), Means[i+1,2], uStds[i,2], winsorize, lambda_in, lambda_out)
    uStds[i+1,3] = sigmaEst(coredata(logExcessReturns[i]), Means[i+1,3], uStds[i,3], winsorize, lambda_in, lambda_out)
  }

  #Filtering the standard deviations
  for(i in 1:n){
    fStds[i+1,1] = ifelse(uStds[i+1,1] > uStds[i,1], uStds[i+1,1], 0.5*(uStds[i,1]+uStds[i+1,1]))
    fStds[i+1,2] = ifelse(uStds[i+1,2] > uStds[i,2], uStds[i+1,2], 0.5*(uStds[i,2]+uStds[i+1,2]))
    fStds[i+1,3] = ifelse(uStds[i+1,3] > uStds[i,3], uStds[i+1,3], 0.5*(uStds[i,3]+uStds[i+1,3]))
  }

  Means = xts(Means, order.by = index_TE)
  uStds = xts(uStds, order.by = index_TE)
  fStds = xts(fStds, order.by = index_TE)

  #Excess volatility
  Vol = matrix(0, ncol = 3, nrow = n+1)
  Vol[1,1] = sqrt(12)*sd(coredata(portfolioReturns))
  Vol[1,2] = sqrt(12)*sd(coredata(benchmarkReturns))

  for(i in 1:n){
    Vol[i+1,] = fStds[i+1,]*sqrt(12)
  }

  Vol[,3] = Vol[,1]-Vol[,2]
  colnames(Vol) = c("FundVol", "BenchmarkVol", "ExcessVol")
  Vol = xts(Vol, order.by = index_TE)

  #Average level of the upper and lower IR inputs
  avgIRLevel = 0.5*(upperIR + lowerIR)/sqrt(12)

  #####Begin looping through the new returns#####
  for(i in 1:length(portfolioReturns)){

    #Monthly estimate of IR
    IR[i] = coredata(logExcessReturns[i]) / coredata(fStds[i,3])

    #Lindley's recursion (Log-likelihood ratios)
    LR[i+1] = ifelse(coredata(LR[i])-coredata(IR[i])+avgIRLevel < 0, 0, ifelse(coredata(LR[i])>6.81, max(0,avgIRLevel-IR[i]), coredata(LR[i])-coredata(IR[i])+avgIRLevel))
  }

  #Yearly moving average returns
  AMA = xts(rep(0, n), order.by = index(portfolioReturns))
  for(i in 1:n){
    AMA[i] = ifelse(i<12, mean(logExcessReturns[1:i]), mean(logExcessReturns[(i-11):i]))
  }

  #CUSUM IR
  cusumIR = xts(cumsum(coredata(IR)), order.by = index(IR))

  #Information obtained from annualized IR
  annualizedIR = sqrt(12)*coredata(cusumIR)
  lowerLim = min(annualizedIR)
  upperLim = max(annualizedIR)
  spreadIR = upperLim - lowerLim
  avgIR = spreadIR/n
  upperPos = which.max(annualizedIR)
  lowerPos = which.min(annualizedIR)
  medIR = lowerLim + 0.5*spreadIR
  peakIR = spreadIR/(upperPos-lowerPos)
  maxIR = 0.5 * ceiling(abs(peakIR)+abs(avgIR))
  protractor_width = ceiling(0.9*spreadIR/(2*maxIR))
  protractor_height = abs(protractor_width*maxIR)

  #Slopes -3 to 3
  Rays = matrix(0, ncol = 7, nrow = n+1)
  Rays[1,1] = medIR + protractor_height
  for(j in 2:7){
    Rays[1,j] = Rays[1,1] - (j-1)*protractor_height/3
  }

  for(i in 2:(n+1)){
    for(j in 1:4){
      Rays[i,j] = max(Rays[i-1,j]-((Rays[1,j]-medIR)/protractor_width), medIR)
    }
    for(j in 5:7){
      Rays[i,j] = min(Rays[i-1,j]-((Rays[1,j]-medIR)/protractor_width), medIR)
    }
  }

  Rays = xts(Rays, order.by = index_TE)
  colnames(Rays) = c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")

  #Return the updated likelihood ratios exceeding the threshold
  return(list("Logarithmic_Excess_Returns" = logExcessReturns,
              "Annually_Moving_Average" = AMA,
              "Tracking_Errors" = fStds[,3],
              "Information_Ratios" = IR,
              "Lindley's_Recursion" = LR,
              "Annualized_Cusum_IR" = annualizedIR,
              "Means" = Means,
              "Protractor" = Rays,
              "Filtered_Standard_Deviations" = fStds,
              "Unfiltered_Standard_Deviations" = uStds,
              "Excess_Volatility" = Vol))
}


