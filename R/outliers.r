#' Outlier Factor
#'
#' Convert outlier labels to ordered factor.
#'
#' @param x A vector of character labels.
#' @return A vector or ordered factors.
#'
#' @keywords internal
.outlier_factor = function(x) {
  factor(x, c(1L, 2L, 3L, 4L),
    c("not outlier", "not evaluated", "mild outlier",
      "extreme outlier"),
    ordered = TRUE)
}

#' Outlier Flags
#'
#' Return a data frame of outlier flags and ranks.
#'
#' @export
list_outlier_flags = function() {
  f = .outlier_factor(NA)
  data.frame(flag = levels(f), rank = seq_along(levels(f)))
}

#' Tukey's test for outliers
#'
#' Perform Tukey's test for mild and extreme outliers.
#'
#' @param x A vector of data.
#' @param mask A logical vector that defines which values in `x`
#' will used when computing statistics. Useful when a subset
#'  of quality-assured data is available. Default mask is non-`NA`
#'  Values.
#' @param threshold A length-two vector identifying
#'  thresholds for "mild" and "extreme" outliers.
#' @param return.score if `TRUE`, return the numeric outlier score.
#'   If `FALSE`, return an ordered factor classifying the observations
#'   as one of "not outlier" (1), "mild outlier" (2), or
#'   "extreme outlier" (3).
#' @inheritDotParams stats::quantile
#' @return A  vector the same length as `x` containing numeric
#'   scores or ordered factors.
#'
#' @details the values of `threshold` identify the multiplier of the
#'   interquartile range used to identify mild and extreme outliers.
#'   typical values are 1.5 for "mild" outliers and 3.0 for "extreme"
#'   outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rnorm(length(x), mean = 1, sd = 3)
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' outlier_tukey(y)
#' outlier_tukey(y, mask)
#' outlier_tukey(y, mask, threshold = c(2, 5))
#' outlier_tukey(y, return.score = TRUE)
#'
#' @importFrom dplyr case_when
#' @importFrom stats quantile
#' @export
outlier_tukey = function(x, mask = !is.na(x),
  threshold = c(1.5, 3), return.score = FALSE, ...) {
  lowerq = quantile(x[mask], ...)[2]
  upperq = quantile(x[mask], ...)[4]
  iqr = upperq - lowerq
  score = pmax(x - upperq, lowerq - x) / iqr
  if (return.score) {
    score
  } else {
    .outlier_factor(case_when(
      is.na(x) ~ NA_integer_,
      score > threshold[2] ~ 4L,
      score > threshold[1] ~ 3L,
      TRUE ~ 1L
    ))
  }
}


#' t-score test for outliers
#'
#' Performs a t-score test for outliers.
#'
#' @inheritParams outlier_tukey
#'
#' @details The values of `threshold` identify the quantiles of the
#'   t-distribution used to identify mild and extreme outliers.
#'   Default values are 0.9 for "mild" outliers and 0.95 for "extreme"
#'   outliers.
#'
#'   The t-score is equivalent to the z-score for sample
#'   sizes greater than 30.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rnorm(length(x), mean = 1, sd = 3)
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' outlier_tscore(y)
#' outlier_tscore(y, mask)
#' outlier_tscore(y, mask, threshold = c(0.8, 0.9))
#' outlier_tscore(y, return.score = TRUE)
#'
#' @importFrom dplyr case_when
#' @importFrom stats qt sd
#' @export
outlier_tscore = function(x, mask = !is.na(x),
  threshold = c(0.9, 0.95), return.score = FALSE) {
  df = length(x) - 1L
  crit.value = abs(qt(threshold, df))
  score = (x - mean(x[mask])) / (sd(x[mask]))
  if (return.score) {
    score
  } else {
    .outlier_factor(case_when(
      is.na(x) ~ NA_integer_,
      abs(score) > crit.value[2] ~ 4L,
      abs(score) > crit.value[1] ~ 3L,
      TRUE ~ 1L
    ))
  }
}

#' Median absolute deviation test for outliers
#'
#' Performs a median absolute deviation (MAD) test for outliers.
#'
#' @inheritParams outlier_tscore
#' @param k A scale factor, defined as the reciprocal of the 75th
#'   percentile of the underlying distribution of the data. Default
#'   assumes a normal distribution.
#'
#' @details the values of `threshold` identify the thresholds used to
#'   identify mild and extreme outliers, as a multiple of
#'   `k * median(x)`. Default values are 1.5 for "mild" outliers and
#'   3.0 for "extreme" outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rnorm(length(x), mean = 1, sd = 3)
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' outlier_mad(y)
#' outlier_mad(y, mask)
#' outlier_mad(y, mask, threshold = c(1, 2))
#' outlier_mad(y, return.score = TRUE)
#'
#' @importFrom dplyr case_when
#' @importFrom stats median mad
#' @export
outlier_mad = function(x, mask = !is.na(x),
  threshold = c(1.5, 3), k = 1 / qnorm(0.75),
  return.score = FALSE) {
  xx = x[mask]
  m = median(xx)
  m.scale = mad(x, m, constant = k)
  score = abs(x - m) / m.scale
  if (return.score) {
    score
  } else {
    .outlier_factor(case_when(
      is.na(x) ~ NA_integer_,
      score > threshold[2] ~ 4L,
      score > threshold[1] ~ 3L,
      TRUE ~ 1L
    ))
  }
}

#' Rousseeuw and Croux test for outliers
#'
#' Rousseeuw and Croux (1993) alternative to MAD.
#'
#'
#outlier_rc = function(x, mask = is.na(x), k = 1.1926) {
#  dd = as.matrix(dist(x, diag = TRUE, upper = TRUE))
#  dd[mask, ] = NA
#  dd[, mask] = NA
#  sn = k * median(as.vector(apply(dd, 1L, median, na.rm = TRUE)),
#    na.rm = TRUE)
#
#
#  xx = x[!mask]
#  d = expand.grid(xx, xx)
#  d['diff'] = abs(d[,1] - d[,2])
#
#  dd = dist(xx)
#
#
#}
