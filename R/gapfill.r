#' ARIMIA and Kalman Filter Gap Fill
#'
#' Fill gaps in a timeseries by fitting an ARIMA model and applying
#' a Kalman filter.
#'
#' @param x A vector of data with missing values.
#' @param mask A logical mask that identifies a subgroup of `x`
#'  to compute fill values for. Useful when a subset
#'  of quality-assured data is available. Default action is to
#'  only fill NA values.
#' @param max.contiguous The maximum number of contiguous values
#'   to fill.
#' @param ... Additional arguments to [forecast::auto.arima()].
#' @return The original vector x with missing values imputed.
#'
#' @importFrom dplyr if_else
#' @importFrom purrr map
#' @importFrom stats KalmanRun
#' @export
gapfill_kalman = function(x, mask = is.na(x), max.contiguous = Inf, ...) {
  if (!requireNamespace('forecast'))
    stop('Could not find package "forecast"')
  fit = forecast::auto.arima(x, ...)
  kr = KalmanRun(x, fit$model)
  newx = sapply(1:length(x), function(i)
    fit$model$Z %*% kr$states[i,])
  blocksize = unlist(map(rle(mask)$lengths, ~ rep(.x, .x)))
  if_else(mask & (blocksize <= max.contiguous), newx, x)
}


#' @keywords internal
gapfill_deep = function(d, mask) {


}


