#' ARIMIA and Kalman Filter Gap Fill
#'
#' Fill gaps in a timeseries by fitting an ARIMA model and applying
#' a Kalman filter.
#'
#' @param x A vector of data with missing values.
#' @param ... Additional arguments to [forecast::auto.arima()].
#' @return The original vector x with missing values imputed.
#'
#' @importFrom dplyr if_else
#' @importFrom stats KalmanRun
#' @export
gapfill_kalman = function(x, ...) {
  if (!requireNamespace('forecast'))
    stop('Could not find package "forecast"')
  fit = forecast::auto.arima(x, ...)
  kr = KalmanRun(x, fit$model)
  newx = sapply(1:length(x), function(i)
    fit$model$Z %*% kr$states[i,])
  if_else(is.na(x), newx, x)
}



gapfill_deep = function(d, mask) {


}