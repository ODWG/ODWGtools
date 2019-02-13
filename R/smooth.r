#' Godin Smoother
#'
#' Apply a Godin filter to a vector of data.
#'
#' @param x A vector of values.
#' @param increment A character string defining the expected time 
#'  increment. this consists of two parts: a numeric value defining 
#'  the size of the increment, and the units of the increment. 
#'  Acceptable units are "secs", "mins", "hours", "days", or "weeks".
#' @return A vector of same length as x.
#'
#' @details The Godin filter is defined
#'  \deqn{F = \frac{A_{24}^2}{24^2}\frac{A_{25}}{25}}
#'
#' @importFrom RcppRoll roll_mean
#' @export
smooth_godin = function(x, increment) {
  increment = string_to_difftime(increment)
  inc.units = units(increment)
  d25 = as.difftime(25L, units = "hours")
  d24 = as.difftime(24L, units = "hours")

  w25 = as.numeric(d25, units = inc.units) / as.numeric(increment)
  w24 = as.numeric(d24, units = inc.units) / as.numeric(increment)
  offset = as.numeric(as.difftime(1L, units = "hours"),
    units = inc.units)/as.numeric(increment)

  (roll_mean(lag(x, offset), w24, fill = NA_real_) +
   roll_mean(lead(x, offset), w24, fill = NA_real_) +
   roll_mean(x, w25, fill = NA_real_)) / 3
}