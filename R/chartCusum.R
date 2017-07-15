#' @title cusumActMgr Plots
#'
#' @description Plot the elements of an \code{cusumActMgr} object.
#'
#' @importFrom zoo as.yearmon coredata index
#' @importFrom graphics boxplot
#' @importFrom stats sd
#' @importFrom utils menu
#' @importFrom lattice barchart
#' @importFrom MASS rlm
#'
#' @param object An object of class cusumActMgr returned by cusumActMgr.
#' @param digits The number of digits of numerical values in graphs
#' @param which a number or a vector of numbers to indicate the type of plots.
#' If a subset of the plots is required, specify a subset of the numbers 1:8
#' for plots. The numbers 1 through 8 represent: \cr \cr
#' 1 = Barplot of log-excess returns with annually moving average returns, \cr
#' 2 = Plot of tracking error, \cr
#' 3 = Barplot of the information ratio, \cr
#' 4 = Plot of cusumIR with protractors of slopes representing IRs, \cr
#' 5 = Plot of Lindley's Recursion with the thresholds, \cr
#' 6 = Plot of excess volatility, \cr
#' 7 = Scatter plot between the fund and benchmark returns with robust regression \cr
#' 8 = Plot of cusum for returns with protractors of slopes representing the annualized returns. \cr
#' @param ... other graphics parameters in plot
#'
#' @return
#' Graph(s) as specified by the user
#'
#' @author Chindhanai Uthaisaad
#'
#' @examples
#' data(cusumData)
#' results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500",
#' data = cusumData)
#' chartCusum(results, which = 8)
#' chartCusum(results, which = c(1,3,4,7))
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
               barchart(100*coredata(results$Logarithmic_Excess_Returns),
                        main = "Monthly Excess Returns", ylab="Excess Return",
                        horizontal = FALSE, col = "blue",
                        panel=function(x,y,...){
                          panel.barchart(x,y,...)
                          panel.lines(100*coredata(results$Annually_Moving_Average),
                                      col=2, lwd=2)})
             },
             "2L" = {
               #Plot of tracking error
               xyplot(100*as.zoo(sqrt(12)*results$Tracking_Error),
                      main="Annualized Tracking Error", type = 'l', las=0,
                      xlab = "", ylab = "%", col = 4, lwd = 1.5)
             },
             "3L" = {
               #Barplot of IR
               barchart(coredata(results$Information_Ratios),
                        main="Estimated Information Ratios",
                        col=4, las=1, horizontal = FALSE,
                        ylab = "IR")
             },
             "4L" = {
               #cusumIR
               xyplot(as.zoo(results$Annualized_Cusum_IR),
                      main="CUSUM Plot: Estimated Information Ratio",
                      col=4, lwd=1.5, horizontal = FALSE,
                      panel=function(x,y,...){
                        panel.xyplot(x,y,...)
                        panel.lines(results$Protractor_IR[,1], col=2, lwd=2)
                        panel.lines(results$Protractor_IR[,2], col=2, lwd=2)
                        panel.lines(results$Protractor_IR[,3], col=2, lwd=2)
                        panel.lines(results$Protractor_IR[,5], col=3, lwd=2)
                        panel.lines(results$Protractor_IR[,6], col=3, lwd=2)
                        panel.lines(results$Protractor_IR[,7], col=3, lwd=2)
                        panel.lines(results$Protractor_IR[,4], col=1, lwd=2)},
                      key=list(space = "right",
                               lines = list(col=c(2,2,2,1,3,3,3), lty=1, lwd=2),
                               text = list(c("IR = -3","IR = -2","IR = -1", "IR = 0",
                                             "IR = 1","IR = 2","IR = 3")),
                               title = "Slopes on Protractor",
                               cex = 0.5)
               )
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
               xyplot(as.zoo(100*results$Excess_Volatility[,3]),
                      main = "Excess Volatility Relative to Benchmark", las=2, col=4,
                      xlab = "", ylab = "%", lwd = 1.5,
                      panel=function(x,y,...){
                        panel.xyplot(x,y,...)
                        panel.abline(h = 0, col = 4, lty = 3)})
             },
             "7L" = {

               #Scatter plot with robust regression
               portRet = 100 * coredata(results$Means[,1])
               benchRet = 100 * coredata(results$Means[,2])
               Rob_lm = MASS::rlm(benchRet ~ portRet)
               Alpha = round(Rob_lm$coefficients[1], 3)
               Beta = round(Rob_lm$coefficients[2], 3)

               xyplot(benchRet ~ portRet, pch = 16, col = 4,
                      main = "Scatter Plot Portfolio Returns and Benchmark Returns",
                      xlab = "Portfolio Returns (%)",
                      ylab = "Benchmark Returns (%)", las=1,
                      panel=function(x,y,...){
                        panel.xyplot(x,y,...)
                        panel.abline(Rob_lm, col=2)
                        panel.abline(v=0, h=0, lty = 3)},
                      key=list(space = "right",
                               text = list(c(paste("Intercept =", Alpha, "%"),
                                             paste("Slope = ", Beta, ""))),
                               title = "Linear Fit",
                               cex = 0.5))
             },
             "8L" = {

               #CUSUM for returns
               xyplot(as.zoo(results$Annualized_Cusum_ER),
                      main="CUSUM Plot: Annualized Excess Returns",
                      las=2, col=4, ylab = "", lwd=2,
                      panel=function(x,y,...){
                        panel.xyplot(x,y,...)
                        panel.lines(results$Protractor_ER[,1], col=2, lwd=2)
                        panel.lines(results$Protractor_ER[,2], col=2, lwd=2)
                        panel.lines(results$Protractor_ER[,3], col=2, lwd=2)
                        panel.lines(results$Protractor_ER[,5], col=3, lwd=2)
                        panel.lines(results$Protractor_ER[,6], col=3, lwd=2)
                        panel.lines(results$Protractor_ER[,7], col=3, lwd=2)
                        panel.lines(results$Protractor_ER[,4], col=1, lwd=2)},
                      key=list(space = "right",
                               lines = list(col=c(2,2,2,1,3,3,3), lty=1, lwd=2),
                               text = list(c("ER = -3%","ER = -2%","ER = -1%", "ER = 0%",
                                             "ER = 1%","ER = 2%","ER = 3%")),
                               title = "Slopes on Protractor\n Excess Returns",
                               cex = 0.5))
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
    par(ask = FALSE)
}
