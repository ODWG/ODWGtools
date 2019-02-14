#' Gap Test
#' 
#' Perform a data gap test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @param x A vector of timestamps.
#' @param increment A character string defining the expected time 
#'  increment. this consists of two parts: a numeric value defining 
#'  the size of the increment, and the units of the increment. 
#'  Acceptable units are "secs", "mins", "hours", "days", or "weeks".
#' @param condition A character string specifying the required 
#'   condition. "is" specifies that the interval between observations 
#'   must be exactly the supplied increment. "less than" or 
#'   "greater than" specify that the interval must be at most or at 
#'   least the specified increment, respectively.
#' @return An integer vector of test flags.
#'
#' @examples
#' # Application to 15-minute data:
#' fake.timestamps = c(
#'   seq(as.POSIXct("2018-01-01"), by = "15 mins", length.out = 5),
#'   rep(as.POSIXct("2018-01-01 01:00:00"), 3), 
#'   as.POSIXct("2018-01-01 04:00:00")
#' )
#' gap_test(fake.timestamps, "16 mins", "less than")
#' gap_test(fake.timestamps, "15 mins", "is")
#'
#' @importFrom dplyr near case_when
#' @export 
gap_test = function(x, increment, condition = c("is", "less than", 
  "greater than")) {
  if (!any(class(x) %in% c("Date", 'POSIXt')))
    stop('argument "x" must be of class "Date" or "POSIXt"')
  condition = match.arg(condition, c("is", "less than", "greater than"))

  increment = string_to_difftime(increment)
  con.fun = switch(condition,
    "is" = near,
    "greater than" = `>`,
    "less than" = `<`
  )
  x.diff = diff(x)
  c(NA_integer_, case_when(
    is.na(x.diff) ~ NA_integer_,
    con.fun(x.diff, increment) ~ 1L,
    TRUE ~ 4L
  ))
}


#' Range Test
#' 
#' Perform a range test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @param x A vector of values.
#' @param sensor.range A length-2 numeric vector identifying the sensor
#'   measurement range. 
#' @param user.range A length-2 numeric vector identifying a reasonable
#'   measurement range for the provided data `x`. Typically specific to
#'   location and/or climate and based on expert judgment.
#' @return An integer vector of test flags.
#'
#' @examples
#' fake.data = c(rnorm(3, 10), rep(c(-5, 30), each =  3), rep(c(5, 20), each = 3))
#' range_test(fake.data, c(0, 25), c(7, 15))
#'
#' @importFrom dplyr between case_when
#' @export 
range_test = function(x, sensor.range, user.range) {
  case_when(
    is.na(x) ~ NA_integer_,
    !between(x, sensor.range[1], sensor.range[2]) ~ 4L,
    !between(x, user.range[1], user.range[2]) ~ 3L,
    TRUE ~ 1L
  )
}


#' Spike test
#' 
#' Perform a spike test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @inheritParams range_test
#' @param spike.threshold A length-2 numeric vector identifying reasonable
#'   magnitudes of difference between adjacent data points. Typically 
#'   specific to location and/or climate and based on expert judgment.
#'
#' @examples
#' fake.data = c(rnorm(10, 10,3), 25, rnorm(10, 10,3))
#' spike_test(fake.data, c(5, 10))
#'
#' @importFrom dplyr case_when lag lead
#' @export
spike_test = function(x, spike.threshold) {
  case_when(
   is.na(x) | is.na(lag(x)) | is.na(lead(x)) ~ NA_integer_,
   abs(x - 0.5 * (lag(x) + lead(x))) > spike.threshold[2] ~ 4L,
   abs(x - 0.5 * (lag(x) + lead(x))) > spike.threshold[1] ~ 3L,
   TRUE ~ 1L
  )
}


#' Rate test
#' 
#' Perform a rate of change test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @inheritParams range_test
#' @param n.dev The number of standard deviations to test against.
#' @param n.prior the number of prior observations to use for computing 
#'   the standard deviation. For example, to compute standard deviations
#'   over the previous 25 hours from 15-minute data use `n.prior = 100`.
#'
#' @examples
#' fake.data = c(rnorm(10,10), rnorm(10, 50), rnorm(10,10))
#' rate_test(fake.data, 2, 5)
#'
#' @importFrom RcppRoll roll_sdr
#' @importFrom dplyr lag case_when
#' @export
rate_test = function(x, n.dev, n.prior) {
  xsd = roll_sdr(x, n.prior + 1L, partial = FALSE,
    fill = NA, normalize = FALSE)
  case_when(
    is.na(xsd) | is.na(x) ~ NA_integer_,
    abs(x - lag(x)) > n.dev * xsd ~ 3L,
    TRUE ~ 1L
    )
}


#' Flat line test
#' 
#' Perform a flat line test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @inheritParams range_test
#' @param rep.threshold A length-2 numeric vector identifying reasonable
#'   magnitudes of difference between adjacent data points. Typically 
#'   specific to location and/or climate and based on expert judgment.
#' @param tol Numerical tolerance to flag observations as repeated values.
#'
#' @examples
#' fake.data = c(rnorm(10, 10, 2), rep(10.0, 3), rnorm(10, 10, 2), rep(7.0, 8))
#' flat_test(fake.data, c(4, 6), 0.01)
#' flat_test(fake.data, c(3, 9), 0.01)
#'
#' @importFrom dplyr lag lead case_when
#' @export
flat_test = function(x, rep.threshold, tol) {
  xx = data.frame(x = x)
  for (i in 1:max(rep.threshold))
    xx[sprintf("lag%d", i)] = abs(x - lag(x, i))
  suspect = rowSums(xx[1 + 1:rep.threshold[1]]) < rep.threshold[1] * tol
  fail = rowSums(xx[1 + 1:rep.threshold[2]]) < rep.threshold[2] * tol
  case_when(
    is.na(x) | (is.na(fail) & is.na(suspect))  ~ NA_integer_,
    fail & !is.na(fail) ~ 4L,
    suspect & !is.na(suspect) ~ 3L,
    TRUE ~ 1L
  )
}


#' Multivariate test
#' 
#' Perform a multivariate rate of change test. This function can also be
#'   used for the neighbor test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information. 
#'
#' @inheritParams rate_test
#' @param y A vector of coincident observations, e.g. from a separate sensor
#'   or nearby station.
#'
#' @examples
#' fake.data1 = c(rnorm(10,10), rnorm(10, 50), rnorm(10, 10))
#' fake.data2 = rep(10,30)
#' multivariate_test(fake.data1, fake.data2, c(1.5, 3), 5)
#' multivariate_test(fake.data2, fake.data1, c(1.5, 3), 5)
#' multivariate_test(fake.data1, fake.data1, 1.5, 5)
#'
#' @importFrom dplyr lag lead case_when
#' @export
multivariate_test = function(x, y, n.dev, n.prior) {
  n.dev = rep(n.dev, length.out = 2)
  n.prior = rep(n.prior, length.out = 2)
  xf = rate_test(x, n.dev[1], n.prior[1])
  yf = rate_test(y, n.dev[2], n.prior[2])
  case_when(
    is.na(x) ~ NA_integer_,
    (xf != 1L & yf == 1L) ~ 3L,
    TRUE ~ 1L
  )
}


attenuated_test = function(x, var.threshold, n.prior) {

}

