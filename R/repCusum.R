#' @title Summarizing a cusumActMgr object
#'
#' @description \code{summary} method for object of class \code{cusumActMgr}.
#' Returned object is of class {summary.cusumActMgr}.
#'
#' @param object an object of class \code{cusumActMgr} returned by \code{cusumActMgr}.
#' @param digits number of significants digits to use when printing.
#' Default is 3.
#' @param ... futher arguments passed to or from other methods.
#'
#' @return Returns an object of class \code{summary.ffm}.
#'
#' Object of class \code{summary.cusumActMgr} is a list of length 10 containing:
#' \item{Logarithmic Excess Returns}{}
#' \item{Annualized Moving Average}{The annualized moving average of the 
#' logarithmic excess returns}
#' \item{Tracking Error}{}
#' \item{Information Ratios}{}
#' \item{Lindley's Recursion}{}
#' \item{Annualized Cusum IR}{}
#' \item{Annualized Cusum Excess Return}{}
#' \item{Excess Volatility}{Excess volatility of the fund, the benchmark and the excess return}
#' \item{Summary Annualized cusumIR}{}
#' \item{Summary Annualized Cusum Excess Returns}{}
#' 
#' @author Chindhanai Uthaisaad.
#'
#' @examples
#' data(cusumData)
#' results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500",
#' data = cusumData)
#' summary.cusumActMgr(results)
#'
#' @method summary cusumActMgr
#' @export

summary.cusumActMgr <- function(object, digits = 3, ...){

  # check input object validity
  if (!inherits(object, "cusumActMgr")) {
    stop("Invalid 'cusumActMgr' object")
  }

  options(digits = digits)
  # extract summary.lm objects for each factor
  sum_name <- list("Logarithmic Excess Returns",
                   "Annualized Moving Average",
                   "Tracking Error",
                   "Information Ratios",
                   "Lindley's Recursion",
                   "Annualized Cusum IR",
                   "Annualized Cusum Excess Return",
                   "Excess Volatility",
                   "Summary Annualized cusumIR",
                   "Summary Annualized Cusum Excess Returns")

  x1 = summary(coredata(object$Logarithmic_Excess_Returns))
  x2 = summary(coredata(object$Annual_Moving_Average))
  x3 = summary(coredata(object$Tracking_Error))
  x4 = summary(coredata(object$Information_Ratios))
  x5 = summary(coredata(object$"Lindley's_Recursion"))
  x6 = summary(coredata(object$Annualized_Cusum_ER))
  x7 = summary(coredata(object$Annualized_Cusum_IR))
  dimnames(x1)[[2]] = dimnames(x2)[[2]] = dimnames(x3)[[2]] = ""
  dimnames(x4)[[2]] = dimnames(x5)[[2]] = dimnames(x6)[[2]] = dimnames(x7)[[2]] = ""
  
  sum_list <- list(x1, x2, x3, x4, x5, x6, x7,
                   summary(coredata(object$Excess_Volatility)),
                   object$AIR,
                   object$AER)
  
  sum = list(sum_name = sum_name, sum_list = sum_list)
  class(sum) <- "summary.cusumActMgr"
  return(sum)
}

#' @rdname summary.cusumActMgr
