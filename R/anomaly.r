#' @include outliers.r
NULL

#' Hampel Filter
#'
#' Anomaly detection using a Hampel filter, i.e., MAD outlier detection
#' over a moving window.
#'
#' @inheritParams outlier_mad
#' @param size A two-element integer vector specifying the number of
#'   points before and after the current data point to include in the
#'   moving window. Default is `c(5L, 5L)`, i.e., an 11-element centered
#'   moving window.
#' @param ... Additional arguments to pass to [`outlier_mad()`].
#' @return A vector of flags or scores.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rnorm(length(x), mean = 1, sd = 3)
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' anomaly_hampel(y, mask)
#'
#' @importFrom slider slide2_vec
#' @export
anomaly_hampel = function(x, mask = !is.na(x), size = c(5L, 5L), ...) {
  f = function(x, mask, ...) {
    outlier_mad(x, mask, ...)[size[1] + 1L]
  }
  slide2_vec(x, mask, f, ..., .before = size[1], .after = size[2],
    .step = 1L, .complete = FALSE)
}

anomaly_ets = function(d) {

#d = as_tsibble(data.frame(x = nsl$DateTime, y = nsl$Value), index = x) %>%
#  filter(x < as_datetime("2017-10-01", "US/Pacific"))
#
#d["gap"] = rtqc_gap(d$x, "15 min")
#d[3460:3470,]
#
#m = ETS(y ~ x),
#
#d %>%
#   model(m)
#     
#     ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#   forecast(h = "3 years")
}

