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
#' @param which A number or a vector of numbers to indicate the type of plots.
#' If a subset of the plots is required, specify a subset of the numbers 1:3
#' for plots. The numbers 1 to 3 represent: \cr \cr
#' 1 = Barplot of log-excess returns with annually moving average returns,
#' plot of tracking error, Plot of excess volatility, and Plot of cusum for returns
#' with protractors of slopes representing the annualized returns,\cr
#' 2 = Barplot of the information ratio, plot of cusumIR with protractors
#' of slopes with corresponding IR, and a plot of Lindley's Recursion with
#' the thresholds\cr
#' 3 = Scatter plot between the fund and benchmark returns with robust regression \cr
#' @param layouts A list containing two vectors. The first vector corresponds to
#' the layout of 'which = 1', and the second vector corresponds to that of 'which = 2'.
#' Default is list(c(2,2), c(1,3))
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
#' chartCusum(results, which = 1)
#' chartCusum(results, which = c(1,2))
#' @export


chartCusum <- function(object, digits = 3, which = NULL,
                       layouts = list(c(2,2), c(1,3)), ...) {

    options(digits = digits)

    which.vec <- which
    which <- which[1]

    repeat {
      switch(which,
             "1L" = {
               # 1: Plot of log-excess returns with annually moving average returns
               P1 = xyplot(100 * object$Logarithmic_Excess_Returns, ylab = "Excess Return",
                           main = "Montly Excess Return",
                           horizontal = FALSE, col = "blue", type = c("h", "g"), lwd = 3,
                           scales = list(y = list(rot = 0)),
                           panel = function(x, y,...) {
                            panel.xyplot(x, y,...)
                            panel.lines(100 * object$Annual_Moving_Average,
                                        col=2, lwd=2)
                           },
                           key = list(corner = c(1, 0.1),
                                     lines = list(col = 2, lty = 1, lwd = 2),
                                     text = list("Moving Average"),
                                     cex = 0.4))
               # 2: Plot of tracking error
               P2 = xyplot(100*as.zoo(sqrt(12)*object$Tracking_Error),
                          main="Annualized Tracking Error", type = 'l', las=0,
                          xlab = "", ylab = "%", col = 4, lwd = 1.5,
                          scales = list(y = list(rot = 0)))
               # 3: Excess volatility plot
               P3 = xyplot(as.zoo(100*object$Excess_Volatility[,3]),
                          main = "Excess Volatility Relative to Benchmark", las=2, col=4,
                          xlab = "", ylab = "%", lwd = 1.5,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            panel.abline(h = 0, col = 4, lty = 3)})
               colors = c("firebrick4", "firebrick3", "firebrick2", 1,
                          "green4", "green3", "green2")
               # 4: CUSUM for returns
               P4 = xyplot(as.zoo(object$Annualized_Cusum_ER),
                          main="CUSUM Plot: Annualized Excess Returns",
                          las=2, col=4, ylab = "", lwd = 1.5,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            for (i in 1:length(colors)){
                              panel.lines(object$Protractor_ER[,i], col=colors[i], lwd=1.5)
                            }
                            panel.lines(object$Protractor_ER[,4], col = 1, lwd = 1.5)
                          },
                          key = list(corner = c(1, 0),
                                   lines = list(col = colors, lty = 1, lwd = 1.5),
                                   text = list(c("ER = -3%","ER = -2%","ER = -1%", "ER = 0%",
                                                 "ER = 1%","ER = 2%","ER = 3%")),
                                   title = "Slopes on Protractor\n Excess Returns",
                                   cex = 0.25))
             if (identical(layouts[[1]], c(2,2))) {
               print(P1, split = c(1, 1, 2, 2), more = TRUE)
               print(P2, split = c(1, 2, 2, 2), more = TRUE)
               print(P3, split = c(2, 1, 2, 2), more = TRUE)
               print(P4, split = c(2, 2, 2, 2))
             } else if (identical(layouts[[1]], c(4,1))) {
               print(P1, split = c(1, 1, 4, 1), more = TRUE)
               print(P2, split = c(2, 1, 4, 1), more = TRUE)
               print(P3, split = c(3, 1, 4, 1), more = TRUE)
               print(P4, split = c(4, 1, 4, 1))
             } else if (identical(layouts[[1]], c(1,4))) {
               print(P1, split = c(1, 1, 4, 1), more = TRUE)
               print(P2, split = c(1, 2, 4, 1), more = TRUE)
               print(P3, split = c(1, 3, 4, 1), more = TRUE)
               print(P4, split = c(1, 4, 4, 1))
             } else {
               return("Invalid argument: layout of which = 1 is incorrect")
               break
             }

             },
             "2L" = {
               # 1: Plot of IR
               P1 = xyplot(as.zoo(object$Information_Ratios),
                          main="Estimated Information Ratios",
                          col=4, las=1, horizontal = FALSE, type = 'h',
                          ylab = "IR", lwd = 1.75,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            panel.abline(h = 0, col=1, lty = 1)})
               colors1 = c("firebrick4", "firebrick3", "firebrick2", 1,
                          "green4", "green3", "green2")
               # 2: cusumIR
               P2 = xyplot(as.zoo(object$Annualized_Cusum_IR),
                          main="CUSUM Plot: Estimated Information Ratio",
                          col=4, lwd=1.5, horizontal = FALSE,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            for (i in 1:length(colors)){
                              panel.lines(object$Protractor_IR[,i], col=colors1[i], lwd=1.5)
                            }
                            panel.lines(object$Protractor_IR[,4], col=1, lwd=1.5)
                          },
                          key=list(corner = c(1,0),
                                   lines = list(col=colors, lty=1, lwd=2),
                                   text = list(c("IR = -3","IR = -2","IR = -1", "IR = 0",
                                                 "IR = 1","IR = 2","IR = 3")),
                                   title = "Slopes on Protractor",
                                   cex = 0.2)
               )
               horiz = c(-3.41, -4.33, -5.08, -5.72, -6.29, -6.81)
               colors2 = c(3, "green3", 7, "goldenrod1", "orangered", 2)
               ycoor = c(-3.3, -4.2, -4.9, -5.6, -6.2, -6.7)
               thresholds = c("24 | 16", "36 | 22", "48 | 27",
                              "60 | 32", "72 | 37","84 | 41")
               # 3: Lindley's Recursion
               P3 = xyplot(as.zoo(-object$`Lindley's_Recursion`),
                          main="Lindley's Recursion", las=2, col=4,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            for (i in 1:length(horiz)) {
                              panel.abline(h = horiz[i], col=colors2[i], lwd=2)
                            }
                            panel.abline(h = 0, col = 4, lty = 3)
                            panel.text(as.yearmon('2005-06', "%Y-%m"), y = -2.0, "Avg. Crossing Time", cex = 0.4)
                            panel.text(as.yearmon('2005-06', "%Y-%m"), y = -2.5, "IR = 0.5 | IR = 0", cex = 0.4)
                            for (i in 1:length(ycoor)) {
                              panel.text(as.yearmon('2005-06', "%Y-%m"), y = ycoor[i], thresholds[i], cex = 0.4)
                            }
                          })
               if (identical(layouts[[2]], c(1,3))) {
                 print(P1, split = c(1, 1, layouts[[2]]), more = TRUE)
                 print(P2, split = c(1, 2, layouts[[2]]), more = TRUE)
                 print(P3, split = c(1, 3, layouts[[2]]))
               } else if (identical(layouts[[2]], c(3,1))) {
                 print(P1, split = c(1, 1, layouts[[2]]), more = TRUE)
                 print(P2, split = c(1, 2, layouts[[2]]), more = TRUE)
                 print(P3, split = c(1, 3, layouts[[2]]))
               } else {
                 return("Invalid argument: layout of which = 2 is incorrect")
                 break
               }

             },
             "3L" = {
               #Scatter plot with robust regression
               portRet = 100 * coredata(object$Means[,1])
               benchRet = 100 * coredata(object$Means[,2])
               Rob_lm = MASS::rlm(benchRet ~ portRet)
               Alpha = round(Rob_lm$coefficients[1], 3)
               Beta = round(Rob_lm$coefficients[2], 3)
               P = xyplot(benchRet ~ portRet, pch = 16, col = 4,
                          main = "Scatter Plot Portfolio Returns and Benchmark Returns",
                          xlab = "Portfolio Returns (%)",
                          ylab = "Benchmark Returns (%)", las=1,
                          scales = list(y = list(rot = 0)),
                          panel=function(x,y,...){
                            panel.xyplot(x,y,...)
                            panel.abline(Rob_lm, col=2)
                            panel.abline(v=0, h=0, lty = 3)},
                          key=list(corner = c(0.92, 0.08),
                                   text = list(c(paste("Intercept =", Alpha, "%"),
                                                 paste("Slope = ", Beta, ""))),
                                   title = "Linear Fit",
                                   cex = 0.5))
               print(P)
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
