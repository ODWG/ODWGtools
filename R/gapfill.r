#' Contiguous Blocks of Discrete Values
#' 
#' Identify contiguous periods of discrete values (i.e. QAQC Flags). 
#'
#' @inheritParams aggregate_flags
#' @return A vector of same length as `flags` where each
#'   element is the size of the block that flag is a part of.
#'
#' @examples
#' flags = sample(c(1L,3L,4L), 10, replace = TRUE)
#' blocksize(flags)
#'
#' @importFrom purrr map
#' @export
blocksize = function(flags) {
  unlist(map(rle(flags)$lengths, ~ rep(.x, .x)))
}


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
#' @param ... Additional arguments to [forecast::auto.arima()].
#' @return The original vector x with missing values imputed.
#'
#' @importFrom dplyr if_else
#' @importFrom stats KalmanRun
#' @export
gapfill_kalman = function(x, mask = is.na(x), ...) {
  if (!requireNamespace('forecast'))
    stop('Could not find package "forecast"')
  fit = forecast::auto.arima(x, ...)
  kr = KalmanRun(x, fit$model)
  newx = sapply(1:length(x), function(i)
    fit$model$Z %*% kr$states[i,])
  if_else(mask, newx, x)
}


#' @keywords internal
gapfill_deep = function(d, mask) {


}
