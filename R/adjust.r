#' @include ODWGtools.r
NULL

## TODO: correct `mask` arguments so that the mask is
## the IGNORED subgroup


#' Adjust for Known Drift
#'
#' Adjust a regular time series for a known drift over a
#' known period. The drift is assumed to increase linearly
#' in time from 0 at the beginning of the time series to
#' the specified drift value at the end of the time series.
#'
#' @inheritParams gapfill_kalman
#' @param mask A logical mask that identifies a subgroup of `x`
#'   to correct for drift. Values outside the subgroup are ignored.
#'   If no mask is specified,
#'   no overall trend is assumed.
#' @param drift The drift value to correct for. The drift
#'   is assumed to occur at the last element of the masked vector
#'   `x` and linearly shrink to zero at the first element of
#'   the masked vector `x`.
#' @return The vector `x` with corrected values for the subgroup
#'   specified by `mask`.
#'
#' @details This function assumes that `mask` specifies a
#'   contiguous block, and that the time series values are
#'   equally spaced. No consideration is given to noncontiguous
#'   masks or unequal-interval time series.
#'
#' @examples
#' x = rnorm(100, 10, 0.1) + 0.025 * (0:99)
#' newx = adjust_known(x, drift = 99*0.025)
#' if (interactive()) {
#'   plot(x, type = 'l')
#'   lines(newx, col = 'red')
#' }
#'
#' @importFrom stats approx
#' @export
adjust_known = function(x, mask, drift) {
  if (missing(mask))
    mask = rep(TRUE, length(x))
  masked.x = x[mask]

  trend = approx(x = c(1, length(masked.x)), y = c(0, drift),
    xout = seq_along(masked.x))$y

  newx = x
  newx[mask] = masked.x - trend
  newx
}

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
#' @importFrom stats lm predict offset
#' @export
adjust_linear = function(x, mask) {
  # check mask
  if (missing(mask)) {
    mask = rep(TRUE, length(x))
  } else if (!any(mask)) {
    warning("All values are masked. No correction applied.")
    return(x)
  }
  # prepare for linear fits
  d = data.frame(index = seq_along(x), value = x)
  # fit drift
  drift.fit = lm(value ~ index, data = d[mask, ])
  drift = predict(drift.fit, newdata = d)
  # fit trend
  if (all(mask)) {
    trend = rep(offset(drift.fit)[[c(1, 1)]], nrow(d))
  } else {
    trend.fit = lm(value ~ index, data = d[!mask, ])
    trend = predict(trend.fit, newdata = d)
  }
  # apply correction
  newx = x
  newx[mask] = newx[mask] - (drift[mask] - trend[mask])
  newx
}



adjust_variance = function(x, mask) {

}
