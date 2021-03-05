#' @include util.r
NULL


#' Daily Tidal Mean
#'
#' Compute the daily low-tide or high-tide average.
#'
#' @param x A vector of values from a single day.
#' @param y A vector of water surface elevations from a single day.
#' @param tide Return the high-tide ("high") or low-tide ("low")
#'  average.
#' @param smooth If `TRUE`, A loess smoother will be applied to the
#'   data prior to detecting the water surface elevation minimums
#'   or maximums.
#' @param neighborhood The neighborhood (number of points) used to
#'   determine the `span` argument to [`stats::loess()`]. A time
#'   window of 6 hours was found to work well with tidal data, which
#'   corresponds to 24 points for 15-minute data or 6 points for
#'   hourly data.
#' @param family The `family` argument to [`stats::loess()`]. The
#'   default family "symmetric" was found to work well with 15-minute
#'   tidal data.
#' @param degree The `degree` argument to [`stats::loess()`]. The
#'   default degree of 2 was found to work well with 15-minute
#'   tidal data.
#' @param ... Additional arguments to [`stats::loess()`].Note that
#'   the value of `span` is computed from the argument `neighborhood`.
#' @param plot If `TRUE`, printdiagnostic plots of the loess smoothing.
#' @return The daily tidal mean of `x`, i.e. the average of high-
#'   or low-tide values for the day.
#'
#' @importFrom stats loess predict
#' @importFrom graphics lines
#' @importFrom dplyr lag lead
#' @export
daily_tidal_mean = function(x, y, tide = c("high", "low"),
  smooth = TRUE, neighborhood = 24, family = "symmetric",
    degree = 2, ..., plot = FALSE) {
  tide = match.arg(tide, c("high", "low"))
  if (tide == "high") {
    compare.fun = `>=`
  } else {
    compare.fun = `<=`
  }
  span = neighborhood / length(y)
  # apply smoothing if specified
  if (smooth) {
    d = data.frame(t = seq_along(y), y = y)
    if (plot) {
      plot(y, type = "l")
    }
    y = tryCatch({
      ysmooth = predict(loess(y ~ t, data = d, span = span,
        degree = degree, family = family))
      if (sqrt(mean((d$y - y) ^ 2)) > 0.05) {
        warning("RMSE of loess-smoothed stage is greater than 0.05")
      }
      ysmooth
    }, error = function(e) {
      warning(paste(e))
      NA
    })
    if (plot) {
      tryCatch(lines(y, col = "red", lty = 2),
        error = function(e) warning(paste(e)))
    }
  }
  select.points = compare.fun(y, lead(y, n = 1, default = NA)) &
    compare.fun(y, lag(y, n = 1, default = NA))
  if (all(is.na(select.points))) {
    warning("Could not identify low/high tide locations.")
    NA
  } else {
    mean(x[select.points], na.rm = TRUE)
  }
}


#' Godin Smoother
#'
#' Apply a Godin filter to a vector of data.
#'
#' @param x A vector of values.
#' @param increment A character string defining the expected time
#'  increment. this consists of two parts: a numeric value defining
#'  the size of the increment, and the units of the increment.
#'  Acceptable units are "secs", "mins", "hours", "days", or "weeks".
#' @param kind what values to filter. Default is the original Godin
#'  filter which averages the data. Other options are to use the `max`
#'  or `min` values of each window.
#' @param ... Additional arguments to `mean()`, `max()`, `min()`,
#'   or `median()`, depending on the argument `kind`.
#' @return A vector of same length as x.
#'
#' @details The Godin filter is defined
#'  \deqn{F = \frac{A_{24}^2}{24^2}\frac{A_{25}}{25}}
#'
#' @importFrom slider slide_dbl
#' @importFrom dplyr lag lead
#' @export
smooth_godin = function(x, increment,
  kind = c("mean", "max", "min", "median"), ...) {
  kind = match.arg(kind, c("mean", "max", "min", "median"))
  roll_fun = switch(kind,
    "mean" = mean,
    "max" = max,
    "min" = min,
    "median" = median
  )
  increment = string_to_difftime(increment)
  inc.units = units(increment)
  d25 = as.difftime(25L, units = "hours")
  d24 = as.difftime(24L, units = "hours")

  w25 = as.numeric(d25, units = inc.units) / as.numeric(increment)
  w24 = as.numeric(d24, units = inc.units) / as.numeric(increment)
  w25.before = sum(seq(w25) < ceiling(w25 / 2))
  w25.after = sum(seq(w25) > ceiling(w25 / 2))
  w24.before = sum(seq(w24) < ceiling(w24 / 2))
  w24.after = sum(seq(w24) > ceiling(w24 / 2))

  offset = as.numeric(as.difftime(1L, units = "hours"),
    units = inc.units) / as.numeric(increment)

  x1 = slide_dbl(lag(x, offset), roll_fun, ...,
    .complete = TRUE, .before = w24.before, .after = w24.after)
  x2 = slide_dbl(lead(x, offset), roll_fun, ...,
    .complete = TRUE, .before = w24.before, .after = w24.after)
  x3 = slide_dbl(x, roll_fun, ...,
    .complete = TRUE, .before = w25.before, .after = w25.after)

  (x1 + x2 + x3) / 3.0
}

#' Lanczos Filter
#'
#smooth_lanczos = function(x, increment, cutoff, window) {
#  increment = string_to_difftime(increment)
#  inc.units = units(increment)
#
#  cutoff = string_to_difftime(cutoff)
#  inc.units = units(cutoff)
#
#  cutoff
#}

