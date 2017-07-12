#' @title Summarizing a fitted fundamental factor model
#'
#' @description \code{summary} method for object of class \code{ffm}.
#' Returned object is of class {summary.ffm}.
#'
#' @details The default \code{summary} method for a fitted \code{lm} object
#' computes the standard errors and t-statistics under the assumption of
#' homoskedasticty.
#'
#' Note: This gives a summary of the fited factor returns at each time period.
#' If \code{T} is large, you might prefer the more succint summary produced by
#' \code{\link{print.ffm}}.
#'
#' @param object an object of class \code{ffm} returned by \code{fitFfm}.
#' @param x an object of class \code{summary.ffm}.
#' @param digits number of significants digits to use when printing.
#' Default is 3.
#' @param labels option to print labels and legend in the summary. Default is
#' \code{TRUE}. When \code{FALSE}, only the coefficient matrx with standard
#' errors is printed.
#' @param ... futher arguments passed to or from other methods.
#'
#' @return Returns an object of class \code{summary.ffm}.
#' The print method for class \code{summary.ffm} outputs the call,
#' coefficients (with standard errors and t-statistics), r-squared and
#' residual volatilty (under the homoskedasticity assumption) for all assets.
#'
#' Object of class \code{summary.ffm} is a list of length N + 2 containing:
#' \item{call}{the function call to \code{fitFfm}}
#' \item{sum.list}{list of summaries of the T fit objects (of class \code{lm} or
#' \code{lmRob}) for each time period in the factor model.}
#'
#' @author Sangeetha Srinivasan & Yi-An Chen.
#'
#' @seealso \code{\link{fitFfm}}, \code{\link[stats]{summary.lm}}
#'
#' @examples
#' data(Stock.df)
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP", "GICS.SECTOR")
#' fit2 <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN",
#'                date.var="DATE", exposure.vars=exposure.vars)
#'
#' # summary of factor returns estimated in each time period
#' summary(fit2)
#'
#' # summary of lm fit for a single period
#' summary(fit2$factor.fit[[1]])
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
  dimnames(x1)[[2]] = dimnames(x2)[[2]] = dimnames(x3)[[2]] = dimnames(x4)[[2]] = dimnames(x5)[[2]] = dimnames(x6)[[2]] = dimnames(x7)[[2]] = ""
  
  sum_list <- list(x1, x2, x3, x4, x5, x6, x7,
                   summary(coredata(object$Excess_Volatility)),
                   object$AIR,
                   object$AER)
  
  sum = list(sum_name = sum_name, sum_list = sum_list)
  class(sum) <- "summary.cusumActMgr"
  return(sum)
}

#' @rdname summary.cusumActMgr
