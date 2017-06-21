fitMFfm = function(data, asset_var, ret_var, date_var, exposure_vars,
                        weight.var=NULL, fit_method=c("LS","WLS","Rob","W-Rob","Ding-Martin"),
                        multi_fact = TRUE, rob_stats=FALSE, full_resid_cov=FALSE, z_score=FALSE,
                        addIntercept = FALSE,lagExposures=FALSE, resid_EWMA = FALSE,
                        lambda = 0.9,...)
{

  # record the call as an element to be returned
  this_call <- match.call()

  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data frame")
  }
  if (!(fit_method %in% c("LS","WLS","Rob","W-Rob", "Ding-Martin"))) {
    stop("Invalid args: fit.method must be 'LS', 'WLS', 'Rob', 'W-Rob', or 'Ding-Martin'")
  }
  if (missing(asset_var) || !is.character(asset_var)) {
    stop("Invalid args: asset_var must be a character string")
  }
  if (missing(date_var) || !is.character(date_var)) {
    stop("Invalid args: date_var must be a character string")
  }
  if (missing(ret_var) || !is.character(ret_var)) {
    stop("Invalid args: ret_var must be a character string")
  }
  if (missing(exposure_vars) || !is.character(exposure_vars)) {
    stop("Invalid args: exposure_vars must be a character vector")
  }
  if (ret_var %in% exposure_vars) {
    stop("Invalid args: ret_var can not also be an exposure")
  }
  if (!is.null(weight_var) && !is.character(weight_var)) {
    stop("Invalid args: weight_var must be a character string")
  }
  if (!is.logical(rob_stats) || length(rob_stats) != 1) {
    stop("Invalid args: control parameter 'rob_stats' must be logical")
  }
  if (!is.logical(full_resid_cov) || length(full_resid_cov) != 1) {
    stop("Invalid args: control parameter 'full_resid_cov' must be logical")
  }
  if (!is.logical(z_score) || length(z_score) != 1) {
    stop("Invalid args: control parameter 'z_score' must be logical")
  }

  # initialize to avoid R CMD check's NOTE: no visible binding for global var
  DATE=NULL
  W=NULL
  model.MSCI=FALSE
  model.styleOnly = FALSE
  restriction.mat = NULL
  g.cov = NULL
  # ensure dates are in required format
  data[[date.var]] <- as.Date(data[[date.var]])
  # extract unique time periods from data
  time.periods <- unique(data[[date.var]])
  TP <- length(time.periods)
  if (TP < 2) {
    stop("Invalid args: at least 2 unique time periods are required to fit the
         factor model")
  }
}
