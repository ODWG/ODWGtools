#' @include util.r
NULL

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
smooth_lanczos = function(x, increment, cutoff, window) {
  increment = string_to_difftime(increment)
  inc.units = units(increment)

  cutoff = string_to_difftime(cutoff)
  inc.units = units(cutoff)

  cutoff
}

