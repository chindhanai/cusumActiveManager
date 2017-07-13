#' @title Portfolio Exposures Report
#' 
#' @description Calculate k factor time series based on fundamental factor model. This method takes fundamental factor model fit, 'ffm' object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom graphics boxplot 
#' @importFrom stats sd
#' @importFrom utils menu
#' @importFrom lattice barchart
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, 'TRUE' for drawing strips on the left of each panel, 'FALSE' for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display. Used only when isPlot = 'TRUE'
#' @param color  character specifying the plotting color for all the plots
#' @param notch logical. if notch is \code{TRUE}, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap this is strong evidence that the two medians differ (Chambers et al, 1983, p. 62).Default values is \code{FALSE}.
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param digits digits of printout numeric summary. Used only when isPrint = 'TRUE'
#' @param titleText logical varible to choose display plot title or not. Default is 'TRUE', and used only when isPlot = 'TRUE'.
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:3 for plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Time series plot of style factor exposures, \cr
#' 2 = Boxplot of style factor exposures, \cr
#' 3 = Barplot of means and vols of style factor exposures, and means of sector exposures (which have no vol). \cr \cr
#' @param type character. type of lattice plot when which=1; 'l' denotes a line, 'p' denotes a point, and 'b' and 'o' both denote both together.deafault is 'b'.
#' @param ... other graphics parameters available in tsPlotMP(time series plot only) can be passed in through the ellipses 
#' 
#' @return  
#' A list containing mean and standard deviation of all the factors
#' 
#' @author Chindhanai Uthaisaad
#' @examples 
#'
#' #Load fundamental and return data 
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") 
#'           & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' #Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#' 
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#'
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, 
#'              which = 2, add.grid = TRUE, scaleType = 'same')
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = TRUE, which = 1,
#'              add.grid = FALSE, zeroLine = TRUE, color = 'Blue')
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, 
#'              which = 3, add.grid = FALSE, zeroLine = FALSE, color = 'Blue')
#' @export


chartCusum <- function(object, digits = 3, which = NULL, ...) {
  
    par(mfrow = c(1,1))
    options(digits = digits)
    
    which.vec <- which
    which <- which[1]
    
    repeat {
      
      switch(which,
             "1L" = { 
               #Plot of log-excess returns with annually moving average returns
               logER_plot = barplot(100*object$Logarithmic_Excess_Returns, 
                                    main = "Monthly Excess Returns",
                                    xlab = "", ylab = "%", col=4, las=1)
               lines(x = logER_plot, y = 100*object$Annual_Moving_Average, 
                     col=2, lwd=2)
             }, 
             "2L" = {
               #Plot of tracking error
               plot(100*as.zoo(sqrt(12)*object$Tracking_Error), 
                    main="Annualized Tracking Error", type = 'l', las=0,
                    xlab = "", ylab = "%", col=4, las=2)
             }, 
             "3L" = {  
               #Plot of IR
               barplot(object$Information_Ratios, main="Estimated Information Ratios", col=4, las=1)
             },
             "4L" = {  
               #cusumIR
               plot(as.zoo(object$Annualized_Cusum_IR),
                    main="CUSUM Plot: Estimated Information Ratio", las=2, col=4, ylab = "", yaxt='n', lwd=2)
               lines(coredata(object$Protractor_IR[,1]), col=2, lty = 1, lwd=2)
               lines(coredata(object$Protractor_IR[,2]), col=2, lty = 3, lwd=2)
               lines(coredata(object$Protractor_IR[,3]), col=2, lty = 4, lwd=2)
               lines(coredata(object$Protractor_IR[,4]), col="purple", lty = 2, lwd=2)
               lines(coredata(object$Protractor_IR[,5]), col=3, lty = 4, lwd=2)
               lines(coredata(object$Protractor_IR[,6]), col=3, lty = 3, lwd=2)
               lines(coredata(object$Protractor_IR[,7]), col=3, lty = 1, lwd=2)
               legend("bottomright", bty = 'n', legend = format(round(seq(-2.0,2.0,2/3), 2), nsmall = 2),
                      col = c(2,2,2,"purple",3,3,3), lty = c(1,3,4,2,4,3,1), cex = 1,
                      title = "Slopes on Protractor", ncol = 2, lwd=2)
             },
             "5L" = {  
               #Lindley's Recursion
               plot(as.zoo(-object$`Lindley's_Recursion`),
                    main="Lindley's Recursion", las=2, col=4, ylab = "")
               abline(h = -3.41, col=3, lwd=2)
               abline(h = -4.33, col="green3", lwd=2)
               abline(h = -5.08, col=7, lwd=2)
               abline(h = -5.72, col="goldenrod1", lwd=2)
               abline(h = -6.29, col="orangered", lwd=2)
               abline(h = -6.81, col=2, lwd=2)
               abline(h = 0, col = 4, lty = 3)
               text(as.yearmon('2005-06', "%Y-%m"), y = -2.0, "Avg. Crossing Time", cex = 0.7)
               text(as.yearmon('2005-06', "%Y-%m"), y = -2.5, "IR = 0.5 | IR = 0", cex = 0.7)
               text(as.yearmon('2005-06', "%Y-%m"), y = -3.3, "24 | 16", cex = 0.89)
               text(as.yearmon('2005-06', "%Y-%m"), y = -4.2, "36 | 22", cex = 0.89)
               text(as.yearmon('2005-06', "%Y-%m"), y = -4.9, "48 | 27", cex = 0.89)
               text(as.yearmon('2005-06', "%Y-%m"), y = -5.6, "60 | 32", cex = 0.89)
               text(as.yearmon('2005-06', "%Y-%m"), y = -6.2, "72 | 37", cex = 0.89)
               text(as.yearmon('2005-06', "%Y-%m"), y = -6.7, "84 | 41", cex = 0.89)
             },
             "6L" = {  
               #Excess volatility plot
               plot(as.zoo(100*object$Excess_Volatility[,3]),
                    main = "Excess Volatility Relative to Benchmark", las=2, col=4,
                    xlab = "", ylab = "%")
               abline(h = 0, col = 4, lty = 3)
             },
             "7L" = {  
               
               #Scatter plot with robust regression
               portRet = 100 * coredata(object$Means[,1])
               benchRet = 100 * coredata(object$Means[,2])
               Rob_lm = rlm(benchRet ~ portRet)
               Alpha = round(Rob_lm$coefficients[1], 2)
               Beta = round(Rob_lm$coefficients[2], 2)
               
               plot(portRet, benchRet, pch = 16, col = 4, main = "Scatter Plot
                    Portfolio Returns and Benchmark Returns",
                    xlab = "Portfolio Returns (%)", ylab = "Benchmark Returns (%)", las=1)
               abline(Rob_lm, col=2)
               abline(v=0, h=0)
               legend("bottomright", legend = c(paste(expression(alpha), " = ", Alpha, "%"), paste(expression(beta), " = ", Beta, "")), cex = 0.7)
               
             },
             "8L" = {  
               
               #CUSUM for returns
               plot(as.zoo(object$Annualized_Cusum_ER),
                    main="CUSUM Plot: Annualized Excess Returns", las=2, col=4, ylab = "", lwd=2)
               lines(coredata(object$Protractor_ER[,1]), col=2, lty = 1, lwd=2)
               lines(coredata(object$Protractor_ER[,2]), col=2, lty = 3, lwd=2)
               lines(coredata(object$Protractor_ER[,3]), col=2, lty = 4, lwd=2)
               lines(coredata(object$Protractor_ER[,4]), col="purple", lty = 2, lwd=2)
               lines(coredata(object$Protractor_ER[,5]), col=3, lty = 4, lwd=2)
               lines(coredata(object$Protractor_ER[,6]), col=3, lty = 3, lwd=2)
               lines(coredata(object$Protractor_ER[,7]), col=3, lty = 1, lwd=2)
               legend("bottomright", bty = 'n', legend = format(round(seq(-2.0,2.0,2/3), 2), nsmall = 2),
                      col = c(2,2,2,"purple",3,3,3), lty = c(1,3,4,2,4,3,1), cex = 1,
                      title = "Slopes on Protractor", ncol = 2, lwd=2)
               
             },
             invisible()       
      )        
      # repeat menu if user didn't choose to exit from the plot options
      if (which == 0 || length(which.vec) == 1) {
        break
      } 
      if (length(which.vec) > 1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask = TRUE)
      } else {
        which = NULL
      }   
    }
  
}