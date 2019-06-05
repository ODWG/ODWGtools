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
#' @return A vector of same length as x.
#'
#' @details The Godin filter is defined
#'  \deqn{F = \frac{A_{24}^2}{24^2}\frac{A_{25}}{25}}
#'
#' @importFrom RcppRoll roll_mean roll_max roll_min
#' @export
smooth_godin = function(x, increment, kind = c("mean", "max", "min")) {
  kind = match.arg(kind, c("mean", "max", "min"))
  roll_fun = switch(kind,
    "mean" = roll_mean,
    "max" = roll_max,
    "min" = roll_min
  )
  increment = string_to_difftime(increment)
  inc.units = units(increment)
  d25 = as.difftime(25L, units = "hours")
  d24 = as.difftime(24L, units = "hours")

  w25 = as.numeric(d25, units = inc.units) / as.numeric(increment)
  w24 = as.numeric(d24, units = inc.units) / as.numeric(increment)
  offset = as.numeric(as.difftime(1L, units = "hours"),
    units = inc.units)/as.numeric(increment)

  (roll_fun(lag(x, offset), w24, fill = NA_real_) +
   roll_fun(lead(x, offset), w24, fill = NA_real_) +
   roll_fun(x, w25, fill = NA_real_)) / 3.0
}
