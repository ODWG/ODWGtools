#' String to difftime
#'
#' Convert a string to a `difftime` object.
#' @param s A string
#' @return A `difftime` object.
#'
#' @keywords internal
string_to_difftime = function(s) {
  value = as.numeric(strsplit(s, " ")[[c(1, 1)]])
  increment = strsplit(s, " ")[[c(1, 2)]]
  increment = match.arg(increment,
    c("secs", "mins", "hours", "days", "weeks"))
  as.difftime(value, units = increment)
  }


#' Water Year
#'
#' Get the water year for a particular date.
#'
#' @param x A vector of DateTime values
#' @return An integer vector of water years.
#'
#' @importFrom dplyr if_else
#' @importFrom lubridate month year
#' @export
water_year = function(x) {
  as.integer(if_else(month(x) > 9, year(x) + 1, year(x)))
}

#' EC to PSU
#'
#' Convert from Electrical Conductivity (microsiemens) to to Practical
#' Salinity Units (PSU)
#'
#' @details Conversion follows the equation described in
#'   Wagner, R. J., Boulger Jr, R. W., Oblinger, C. J.,
#'   & Smith, B. A. (2006). Guidelines and standard procedures for
#'   continuous water-quality monitors: station operation, record
#'   computation, and data reporting. No. 1-D3.
#'
#' @param ec Electrical Conductivity, in microsiemens.
#' @return Salinity, in PSU.
#'
#' @export
ec_to_psu <- function(ec) {
  K1 = 0.0120
  K2 = -0.2174
  K3 = 25.3283
  K4 = 13.7714
  K5 = -6.4788
  K6 = 2.5842
  seaEC = 53.087 * 1000
  R = ec / seaEC

  S = K1 + (K2 * R ^ (1 / 2)) + (K3 * R) + (K4 * R ^ (3 / 2)) +
    (K5 * R ^ 2) + (K6 * R ^ (5 / 2))
  return(S)
}

#' List to Numeric Data Frame
#'
#' Utility function to convert a list of vectors to a numeric dataframe.
#'
#' @param xs A list of equal-length vectors.
#' @return A dataframe.
#'
#' @keywords internal
list_to_ndf = function(xs) {
  if (!is.list(xs)) {
    stop("\"xs\" must be a dataframe of list of vectors.")
  } else if (!Reduce(identical, lapply(xs, length))) {
    stop("Elements of \"xs\" must be equal length.")
  }
  # check if elements are coercible
  nas = sapply(xs, function(x) sum(is.na(x)))
  new.nas = sapply(xs, function(x) sum(is.na(as.numeric(x))))
  if (any(new.nas > nas)) {
    stop("Elements of \"xs\" must be convertible to numeric.")
  }
  as.data.frame(lapply(xs, as.numeric))
}

#' Inverse Which
#'
#' Convert output of [`base::which()`] to a logical vector.
#'
#' @param i A vector of indices.
#' @param len The length of the original vector.
#' @return A logical vector.
#'
#' @keywords internal
invwhich = function(i, len) {
  `[<-`(logical(len), i, TRUE)
}
