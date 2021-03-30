#' @include util.r
NULL

#' Real-time Quality Control Factor
#'
#' Convert real-time quality control labels to ordered factor.
#'
#' @param x A vector of integers.
#' @return A vector or ordered factors.
#'
#' @keywords internal
.rtqc_factor = function(x) {
  factor(x, c(1L, 2L, 3L, 4L),
    c("pass", "not evaluated", "suspect", "fail"),
    ordered = TRUE)
}

#' Real-time Quality Control Flags
#'
#' Return a data frame of real-time quality control flags
#' and ranks.
#'
#' @export
list_rtqc_flags = function() {
  f = .rtqc_factor(NA)
  data.frame(flag = levels(f), rank = seq_along(levels(f)))
}


#' Gap Test
#'
#' Perform a data gap test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
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
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' # Application to 15-minute data:
#' fake.timestamps = c(
#'   seq(as.POSIXct("2018-01-01"), by = "15 mins", length.out = 5),
#'   rep(as.POSIXct("2018-01-01 01:00:00"), 3),
#'   as.POSIXct("2018-01-01 04:00:00")
#' )
#' rtqc_gap(fake.timestamps, "16 mins", "less than")
#' rtqc_gap(fake.timestamps, "15 mins", "is")
#'
#' @importFrom dplyr near case_when
#' @export
rtqc_gap = function(x, increment, condition = c("is", "less than",
  "greater than")) {
  if (!any(class(x) %in% c("Date", "POSIXt")))
    stop("argument \"x\" must be of class \"Date\" or \"POSIXt\"")
  condition = match.arg(condition, c("is", "less than", "greater than"))

  increment = string_to_difftime(increment)
  con.fun = switch(condition,
    "is" = near,
    "greater than" = `>`,
    "less than" = `<`
  )
  x.diff = diff(x)
  .rtqc_factor(c(NA_integer_, case_when(
    is.na(x.diff) ~ NA_integer_,
    con.fun(x.diff, increment) ~ 1L,
    TRUE ~ 4L
  )))
}


#' Range Test
#'
#' Perform a range test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
#'
#' @param x A vector of values.
#' @param sensor.range A length-2 numeric vector identifying the sensor
#'   measurement range.
#' @param user.range A length-2 numeric vector identifying a reasonable
#'   measurement range for the provided data `x`. Typically specific to
#'   location and/or climate and based on expert judgment.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(rnorm(3, 10), rep(c(-5, 30), each =  3),
#'   rep(c(5, 20), each = 3))
#' rtqc_range(fake.data, c(7, 15), c(0, 25))
#'
#' @importFrom dplyr between case_when
#' @export
rtqc_range = function(x, user.range, sensor.range) {
  .rtqc_factor(case_when(
    is.na(x) ~ NA_integer_,
    !between(x, sensor.range[1], sensor.range[2]) ~ 4L,
    !between(x, user.range[1], user.range[2]) ~ 3L,
    TRUE ~ 1L
  ))
}

#' Spike test
#'
#' Perform a spike test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
#'
#' @inheritParams rtqc_range
#' @param spike.threshold A length-2 list specifying "suspect"
#'   and "fail" thresholds. Each threshold can be either a single
#'   value (static threshold) or a vector of same length as `x`
#'   (dynamic threshold). Thresholds are typically specific to
#'   location and/or climate and based on expert judgment.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(1, 1.1, 1, 1.2, 1, 1.3, 1, 1.4, 1, 1.5, 1)
#' rtqc_spike(fake.data, c(0.15, 0.25))
#'
#' @importFrom dplyr case_when lag lead
#' @export
rtqc_spike = function(x, spike.threshold) {
  spike.threshold = as.list(spike.threshold)
  if (length(spike.threshold) != 2L) {
    stop("\"spike.threshold\" must be a ",
      "two-element vector or list.")
  }
  if (!all(sapply(spike.threshold, length) %in% c(1L, length(x)))) {
    stop("Elements of \"spike.threshold\" must be single ",
      "values or vectors of same length as  \"x\".")
  }
  .rtqc_factor(case_when(
   is.na(x) | is.na(lag(x)) | is.na(lead(x)) ~ NA_integer_,
   abs(x - 0.5 * (lag(x) + lead(x))) > spike.threshold[[2]] ~ 4L,
   abs(x - 0.5 * (lag(x) + lead(x))) > spike.threshold[[1]] ~ 3L,
   TRUE ~ 1L
  ))
}


#' Spike Test (Alternate)
#'
#' Perform a spike test. This is a variant of [rtqc_spike()]
#' which checks the absolute difference between the current
#' measurement and the previous/next measurements.
#'
#' @inheritParams rtqc_range
#' @param spike.threshold A length-2 list specifying "suspect"
#'   and "fail" thresholds. Each threshold can be either a single
#'   value (static threshold) or a vector of same length as `x`
#'   (dynamic threshold). Thresholds are typically specific to
#'   location and/or climate and based on expert judgment.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(1, 1.1, 1, 1.2, 1, 1.3, 1, 1.4, 1, 1.5, 1)
#' rtqc_spike(fake.data, c(0.15, 0.25))
#'
#' @importFrom dplyr case_when lag lead
#' @export
rtqc_spike_alt = function(x, spike.threshold) {
  spike.threshold = as.list(spike.threshold)
  if (length(spike.threshold) != 2L) {
    stop("\"spike.threshold\" must be a ",
      "two-element vector or list.")
  }
  if (!all(sapply(spike.threshold, length) %in% c(1L, length(x)))) {
    stop("Elements of \"spike.threshold\" must be single ",
      "values or vectors of same length as  \"x\".")
  }
  .rtqc_factor(case_when(
   is.na(x) | is.na(lag(x)) | is.na(lead(x)) ~ NA_integer_,
   abs(x - lag(x)) > spike.threshold[[2]] &
     abs(x - lead(x)) > spike.threshold[[2]]  ~ 4L,
   abs(x - lag(x)) > spike.threshold[[1]] &
     abs(x - lead(x)) > spike.threshold[[1]]  ~ 3L,
   TRUE ~ 1L
  ))
}


#' Rate Test
#'
#' Perform a rate of change test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
#'
#' @inheritParams rtqc_range
#' @param n.dev The number of standard deviations to test against.
#' @param n.prior the number of prior observations to use for computing
#'   the standard deviation. For example, to compute standard deviations
#'   over the previous 25 hours from 15-minute data use `n.prior = 100`.
#' @param ... Other arguments to pass to [`stats::sd()`], i.e.,
#'   the argument `na.rm`.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(rnorm(10,10), rnorm(10, 50), rnorm(10,10))
#' rtqc_rate(fake.data, 2, 5)
#'
#' @importFrom slider slide_dbl
#' @importFrom dplyr lag case_when
#' @importFrom stats sd
#' @export
rtqc_rate = function(x, n.dev, n.prior, ...) {
  xsd = slide_dbl(x, sd, ..., .before = n.prior, .after = -1L,
    .complete = TRUE)
  .rtqc_factor(case_when(
    is.na(xsd) | is.na(x) ~ NA_integer_,
    abs(x - lag(x)) > n.dev * xsd ~ 3L,
    TRUE ~ 1L
    ))
}

#' Rate Test (Alternate)
#'
#' Perform a rate of change test. This is a variant of [`rtqc_flat()`]
#' which checks the absolute rate of change over a given number
#' of observations (defined as the absolute value of the average of
#' the rates of change between successive observations)
#'
#' @inheritParams rtqc_range
#' @param threshold The absolute rate of change over a set of
#'   observations. A rate of change above this value will flag
#'   the final data point in the set.
#' @param n.prior the number of prior observations to calculate the
#'   rate of change over, excluding the current observation.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(rnorm(10,10), rnorm(10, 50), rnorm(10,10))
#' rtqc_rate_alt(fake.data, 20, 3)
#'
#' @importFrom dplyr case_when
#' @importFrom slider slide_dbl
#' @importFrom stats sd coef lm
#' @export
rtqc_rate_alt = function(x, threshold, n.prior) {
  xdiff = slider::slide_dbl(x, ~ mean(diff(.x)), .before = n.prior,
      .complete = TRUE)
  .rtqc_factor(case_when(
    is.na(xdiff) | is.na(x) ~ NA_integer_,
    abs(xdiff) > threshold ~ 3L,
    TRUE ~ 1L
  ))
}

#' Flat Line Test
#'
#' Perform a flat line test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
#'
#' @inheritParams rtqc_range
#' @param rep.threshold A length-2 integer vector identifying reasonable
#'   counts of repeated values before a data point is considered suspect
#'   or indicates sensor failure. Typically
#'   specific to location and/or climate and based on expert judgment.
#' @param tol Numerical tolerance to consider adjacent observations as
#'   repeated values.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = c(rnorm(10, 10, 2), rep(10.0, 3), rnorm(10, 10, 2),
#'   rep(7.0, 8))
#' # flag data as "suspect" when 4 observations in a row differ by
#' # less than 0.01, and as "fail" when 6 observations in a row
#' # differ by less than 0.01.
#' rtqc_flat(fake.data, c(4, 6), 0.01)
#' # as above, but 3 repeated observations signals "suspect" while
#' # 9 observations signals "fail".
#' rtqc_flat(fake.data, c(3, 9), 0.01)
#'
#' @importFrom dplyr lag case_when
#' @export
rtqc_flat = function(x, rep.threshold, tol) {
  rep.threshold = as.integer(rep.threshold)
  xx = data.frame(x = x)
  for (i in 1:max(rep.threshold))
    xx[sprintf("lag%d", i)] = abs(x - lag(x, i)) < tol
  suspect = as.integer(rowSums(xx[1 + 1:rep.threshold[1]])) + 1L
  fail = as.integer(rowSums(xx[1 + 1:rep.threshold[2]])) + 1L
  .rtqc_factor(case_when(
    is.na(x) | (is.na(fail) & is.na(suspect))  ~ NA_integer_,
    (fail >= rep.threshold[2]) & !is.na(fail) ~ 4L,
    (suspect >= rep.threshold[1]) & !is.na(suspect) ~ 3L,
    TRUE ~ 1L
  ))
}


#' Attenuation Test
#'
#' Perform a signal attenuation test. For more information see
#' U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
#' Quality Control of In-situ Temperature and Salinity Data Version
#' 2.0: A Guide to Quality Control and Quality Assurance of In-situ
#' Temperature and Salinity Observations. 56 pp.
#' DOI: [10.7289/V5V40SD4](https://doi.org/10.7289/V5V40SD4).
#'
#' @inheritParams rtqc_range
#' @param threshold A length-2 vector specifying "suspect" and "fail"
#'   thresholds for the minimum standard deviation of a set of
#'   observations.
#' @param n.obs The number of observations, including the current
#'   observation, to use for calculating standard deviation.
#' @param ... Other arguments to pass to [`stats::sd()`], i.e.,
#'   the argument `na.rm`.
#' @return An ordered factor of test flags of same length as `x`.
#'
#' @examples
#' fake.data = sin(seq(0, 10, by = 0.1) * pi) * seq(10, 0, by = -0.1)
#' rtqc_attenuation(fake.data, c(1, 0.5), 10)
#'
#' @importFrom dplyr case_when
#' @importFrom slider slide_dbl
#' @importFrom stats sd
#' @export
rtqc_attenuation = function(x, threshold, n.obs, ...) {
  xsd = slide_dbl(x, sd, ..., .before = n.obs - 1L, .after = 0L,
    .complete = TRUE)
  .rtqc_factor(case_when(
    is.na(xsd) | is.na(x) ~ NA_integer_,
    xsd < threshold[2] ~ 4L,
    xsd < threshold[1] ~ 3L,
    TRUE ~ 1L
  ))
}
