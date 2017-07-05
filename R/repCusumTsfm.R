#' @title Tabular and Graphical reports from \code{cusumTsfm()}
#'
#' @description The function \code{repCusumTsfm()} provides the tabular and graphical reports
#' from the \code{tsfm} object obtained from the function \code{cusumTsfm()}.
#'
#' @importFrom lattice barchart
#' @importFrom reshape2 melt
#' @param object A fit object of class \code{tsfm}.
#' @param Cusum One of 'LogExcessReturns' (Logarithmic excess returns), 'TE' (Tracking errors),
#' 'IR' (Information ratios), 'LLR' (Loglikelihood ratios). The default is 'LogExcessReturns'.
#' @param digits The number of digits in the resulting table's numbers. The default is 3 and
#' it is used only when \code{isPrint = 'TRUE'}
#' @param nrowPrint A numerical value indicating the number of rows in the resulted vector/table to print or plot
#' @param layout A numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param stripText.cex A number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex A number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param isPlot A logical argument indicating whether or not the plots are displayed.
#' @param isPrint A logical argument indicating whether or not the numeric output is displayed.
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and
#' optional arguments passed to \code{\link[stats]{cov}}
#'
#' @return A table containing
#' \item{Cusum = 'LogExcessReturns'}{The time series of the (monthly) logarithmic excess returns}
#' \item{Cusum = 'TE'}{The time series of the (monthly) tracking errors}
#' \item{Cusum = 'IR'}{The time series of the (monthly) information ratios defined by the proportion
#' of the monthly logarithmic excess returns and the monthly volatility of the specific asset}
#'
#' @author Douglas Martin, Chindhanai Uthaisaad
#'
#' @seealso \code{\link{cusumTsfm}}
#' for the different factor model fitting functions.
#'
#'
#' @examples
#' #' # Time Series Factor Model
#' data(managers)
#' fit.macro <- factorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers[,10]), data=managers)
#' report <- repRisk(fit.macro, risk = "ES", decomp = 'FPCR',
#'                   nrowPrint = 10)
#' report
#'
#' # plot
#' repRisk(fit.macro, risk = "ES", decomp = 'FPCR', isPrint = FALSE,
#'         isPlot = TRUE)
#'
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") &
#'           dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' # Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)
#'
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat,
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER",
#'               fit.method="WLS", z.score = TRUE)
#' repRisk(fit.cross, risk = "Sd", decomp = 'FCR', nrowPrint = 10,
#'         digits = 4)
#' # get the factor contributions of risk
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = "Sd", decomp = 'FPCR',
#'         nrowPrint = 10)
#' # portfolio only decomposition
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = c("VaR", "ES"), decomp = 'FPCR',
#'         portfolio.only = TRUE)
#' # plot
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = "Sd", decomp = 'FPCR',
#'         isPrint = FALSE, nrowPrint = 15, isPlot = TRUE, layout = c(4,2))
#' @export

#' @rdname repRisk
#' @method repRisk tsfm
#' @importFrom utils head
#' @export

repCusumActMgr <- function(object, Cusum = c("LogExcessReturns",
                         "TE", "IR", "LLR"),
                         digits = 3, nrowPrint = 20, isPrint = TRUE, isPlot = FALSE,
                         layout = NULL, stripText.cex =1, axis.cex=1, ...) {

  if(Cusum == 'LogExcessReturns') {
    mainData = object[[1]]
  } else if(Cusum == 'TE') {
    mainData = object[[2]]
  } else if(Cusum == 'IR') {
    mainData = object[[3]]
  } else if(Cusum == 'LLR') {
    mainData = object[[4]]
  } else {
    stop("Invalid args: Cusum must be either 'LogExcessReturns','TE',
         'IR', or 'LLR'")
  }

  if(isPlot){
    plot(mainData)
  }

  if(isPrint){
      result = head(result, nrowPrint)
      result = round(result, digits)

      output = list(decomp = result)
      names(output) = paste(risk,decomp,sep = '')

      return(output)
  }

}
