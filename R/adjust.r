#' Adjust for Linear Drift
#'
#' Adjust a regular time series for an assumed linear drift over a
#' known period while maintaining an assumed linear trend over the
#' entire period.
#'
#' @inheritParams gapfill_kalman
#' @param mask A logical mask that identifies a subgroup of `x`
#'   to correct for drift. Values outside the subgroup are used to
#'   determine the overall linear trend. If no mask is specified, 
#'   no overall trend is assumed.
#' @return The vector `x` with corrected values for the subgroup
#'   specified by `mask`.
#'
#' @examples
#' # no trend, only drift
#' x = rnorm(100, 10) + 0.1 * (1:100)
#' newx = adjust_linear(x)
#' if (interactive()) {
#'   plot(x, type = 'l')
#'   lines(newx, col = 'red')
#' }
#' # trend and drift
#' x = rnorm(100, 10) - 0.05 * (1:100)
#' mask = seq_along(x) %in% 20:80
#' x[mask] = x[mask] - 0.1 * cumsum(mask)[mask]
#' newx = adjust_linear(x, mask)
#' if (interactive()) {
#'   plot(x, type = 'l')
#'   lines(newx, col = 'red')
#' }
#'
#' @importFrom stats lm predict
#' @export
adjust_linear = function(x, mask) {
  # check mask
  if (missing(mask)) {
    mask = rep(TRUE, length(x))
  } else if (!any(mask)) {
    warning('All values are masked. No correction applied.')
    return(x)
  }
  # prepare for linear fits
  d = data.frame(index = seq_along(x), value = x)
  # fit drift
  drift.fit = lm(value ~ index, data = d[mask,])
  drift = predict(drift.fit, newdata = d)
  # fit trend
  if (all(mask)) {
    trend = rep(offset(drift.fit)[[c(1, 1)]], nrow(d))
  } else {
    trend.fit = lm(value ~ index, data = d[!mask,])
    trend = predict(trend.fit, newdata = d)
  }
  # apply correction
  newx = x
  newx[mask] = newx[mask] - (drift[mask] - trend[mask])
  newx
}


adjust_kalman = function(x, mask, ...) {
  if (!requireNamespace('forecast'))
    stop('Could not find package "forecast"')
  fit.x = x
  fit.x[mask] = NA
  fit = forecast::auto.arima(fit.x, ...)
#  newx = forecast::forecast(fit)

  kr = KalmanForecast(sum(mask), fit$model)
  newx = sapply(1:length(x), function(i)
    fit$model$Z %*% kr$states[i,])
  newx = x
  newx[mask] = kr$pred
  


}


# Adjust drift using a Godin Smoother and Kalman filter
adjust_godin = function(x, mask, increment, ...) {
  smooth = smooth_godin(x, increment)
  mean = mean(smooth[mask])
  resid = x - smooth
#  new = gapfill_kalman(if_else(mask, NA_real_, x), mask)
 # if_else(flag, new + resid, x)

#  center = mean(smooth)
  plot(x, type = 'l')
  lines(if_else(flag, mean + resid, x), col = "red")
  lines(smooth, col = "blue")
  lines(if_else(flag, mean, smooth), col = "green")


#  View(data.frame(x = x, new = new + resid))
}


