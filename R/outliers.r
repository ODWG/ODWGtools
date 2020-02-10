#' Tukey's test for outliers
#'
#' Perform Tukey's test for mild and extreme outliers.
#'
#' @param x A vector of data.
#' @param mask A logical mask that identifies a subgroup of `x`
#'  to compute non-parametric statistics on. Useful when a subset
#'  of quality-assured data is available. Default action is to
#'  remove NA values.
#' @param threshold A length-two vector identifying
#'  thresholds for "mild" and "extreme" outliers.
#' @param return.score if `TRUE`, return the numeric outlier score.
#'   If FALSE, return a label classifying the observations as one of
#'   "not outlier", "mild outlier", or "extreme outlier".
#' @return A  vector the same length as `x` containing numeric
#'   scores or character labels.
#'
#' @details the values of `threshold` identify the multiplier of the
#'   interquartile range used to identify mild and extreme outliers.
#'   typical values are 1.5 for "mild" outliers and 3.0 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats quantile
#' @export
tukey_outliers = function(x, mask = !is.na(x), threshold = c(1.5, 3), return.score = FALSE) {
  lowerq = quantile(x[mask])[2]
  upperq = quantile(x[mask])[4]
  iqr = upperq - lowerq
  score = pmax(x - upperq, lowerq - x) / iqr
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      score > threshold[2] ~ "extreme outlier",
      score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' Z-score test for outliers
#'
#' Performs a Z-score test for outliers.
#'
#' @inheritParams tukey_outliers
#'
#' @details the values of `threshold` identify the quantiles of the
#'   normal distribution used to identify mild and extreme outliers.
#'   Default values are 0.9 for "mild" outliers and 0.95 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats qnorm sd
#' @export
zscore_outliers = function(x, mask = !is.na(x), threshold = c(0.9, 0.95), return.score = FALSE) {
  score = (x - mean(x[mask])) / sd(x[mask])
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      abs(score) > qnorm(threshold[2]) ~ "extreme outlier",
      abs(score) > qnorm(threshold[1]) ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' t-score test for outliers
#'
#' Performs a t-test for outliers.
#'
#' @inheritParams zscore_outliers
#'
#' @details the values of `threshold` identify the quantiles of the
#'   t distribution used to identify mild and extreme outliers.
#'   Default values are 0.9 for "mild" outliers and 0.95 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats qt sd
#' @export
tscore_outliers = function(x, mask = !is.na(x), threshold = c(0.9, 0.95), return.score = FALSE) {
  n = length(x)
  temp = (x - mean(x[mask])) / sd(x[mask])
  score = (temp * sqrt(n - 2)) / sqrt(n - 1 - temp ^ 2)
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      abs(score) > qt(threshold[2], n - 2) ~ "extreme outlier",
      abs(score) > qt(threshold[1], n - 2) ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' Chi-squared test for outliers
#'
#' Performs a Chi-squared test for outliers.
#'
#' @inheritParams zscore_outliers
#'
#' @details the values of `threshold` identify the quantiles of the
#'   Chi-squared distribution used to identify mild and extreme outliers.
#'   Default values are 0.9 for "mild" outliers and 0.95 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats qchisq var
#' @export
chisq_outliers = function(x, mask = !is.na(x), threshold = c(0.9, 0.95), return.score = FALSE) {
  score = (x - mean(x[mask])) ^ 2 / var(x[mask])
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      abs(score) > qchisq(threshold[2], 1) ~ "extreme outlier",
      abs(score) > qchisq(threshold[1], 1) ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' Median absolute deviation test for outliers
#'
#' Performs a median absolute deviation (MAD) test for outliers.
#'
#' @inheritParams zscore_outliers
#'
#' @details the values of `threshold` identify the multiplier of the
#'   median distance used to identify mild and extreme outliers.
#'   Default values are 1.5 for "mild" outliers and 3.0 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats median
#' @export
mad_outliers = function(x, mask = !is.na(x), threshold = c(1.5, 3), return.score = FALSE) {
  xx = x[mask]
  m = median(xx)
  abs.dev = abs(xx - m)
  left.mad = median(abs.dev[xx <= m])
  right.mad = median(abs.dev[xx >= m])
  mad.dist = case_when(
    x > m ~ right.mad,
    x < m ~ left.mad,
    TRUE ~ 0.0
  )
  score = (x - m) / mad.dist
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      abs(score) > threshold[2] ~ "extreme outlier",
      abs(score) > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' Isolation Forest outlier detection
#'
#' Performs outlier detection using an Isolation Forest.
#'
#' @inheritParams zscore_outliers
#' @param ... Additional arguments to `ranger::ranger` (used by
#'   `solitude::isolationForest`).
#'
#' @details the values of `threshold` identify mild and extreme\
#'   outliers based on the Isolation Forest score in the range `[0,1]`.
#'   Default values are 0.8 for "mild" outliers and 0.9 for "extreme"
#'   outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats predict na.omit
#' @export
iforest_outliers = function(x, mask = !is.na(x), sample.size = length(mask), threshold = c(0.8, 0.9), return.score = FALSE, ...) {
  if (!requireNamespace("solitude"))
    stop("Could not find package \"solitude\"")
  d = data.frame(x = x[mask])
  mod = solitude::isolationForest$new(sample_size =sample.size, ...)
  mod$fit(d)
  p = data.frame(x = x)
  score = mod$predict(na.omit(p))$anomaly_score
  p["score"] = NA_real_
  p[!is.na(p$x), "score"] = score
  if (return.score) {
    p$score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      p$score > threshold[2] ~ "extreme outlier",
      p$score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}


#' LOF outlier detection
#'
#' Performs outlier detection using Local Outlier Factor algorithm.
#'
#' @inheritParams zscore_outliers
#' @param ... Additional arguments to `dbscan::lof`, namely
#'   `k`.
#'
#' @details the values of `threshold` identify mild and extreme
#'   outliers based on the LOF score. Scores significantly larger
#'   than 1 indicate outliers. Default values are 1.5 for "mild"
#'   outliers and 2.0 for "extreme" outliers.
#'
#' @importFrom dplyr if_else between case_when
#' @export
lof_outliers = function(x, mask = !is.na(x), threshold = c(1.5, 2), return.score = FALSE, ...) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")
  xx = as.matrix(x[mask])
  lof.omit = dbscan::lof(xx, ...)
  score = rep(NA, length(x))
  score[mask] = lof.omit
  if (return.score) {
    score
  } else {
    case_when(
      is.na(x) ~ NA_character_,
      score > threshold[2] ~ "extreme outlier",
      score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    )
  }
}



glosh_outliers = function(x, mask) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")
}


hdbscan_outliers = function(x, threshold = c(0.9, 0.95), ...) {
  # TODO
}