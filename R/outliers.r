#' Outlier Factor
#'
#' Convert outlier labels to ordered factor.
#'
#' @param x A vector of character labels.
#' @return A vector or ordered factors.
#'
#' @keywords internal
outlier_factor = function(x) {
  factor(x, c("not outlier", "mild outlier", "extreme outlier"),
    ordered = TRUE)
}

#' Outlier Flags
#'
#' Return a data frame of outlier flags and ranks.
#'
#' @export
outlier_flags = function() {
  f = outlier_factor(NA)
  data.frame(flag = levels(f), rank = seq_along(levels(f)))
}

#' Tukey's test for outliers
#'
#' Perform Tukey's test for mild and extreme outliers.
#'
#' @param x A vector of data.
#' @param mask A logical mask that identifies a subgroup of `x`
#'  to compute statistics on. Useful when a subset
#'  of quality-assured data is available. Default action is to
#'  ignore NA values.
#' @param threshold A length-two vector identifying
#'  thresholds for "mild" and "extreme" outliers.
#' @param return.score if `TRUE`, return the numeric outlier score.
#'   If FALSE, return an ordered factor classifying the observations
#'   as one of "not outlier" (1), "mild outlier" (2), or
#'   "extreme outlier" (3).
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
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_tukey(y)
#' outlier_tukey(y, mask)
#' outlier_tukey(y, mask, threshold = c(2, 5))
#' outlier_tukey(y, return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats quantile
#' @export
outlier_tukey = function(x, mask = !is.na(x),
  threshold = c(1.5, 3), return.score = FALSE) {
  lowerq = quantile(x[mask])[2]
  upperq = quantile(x[mask])[4]
  iqr = upperq - lowerq
  score = pmax(x - upperq, lowerq - x) / iqr
  if (return.score) {
    score
  } else {
    outlier_factor(case_when(
      is.na(x) ~ NA_character_,
      score > threshold[2] ~ "extreme outlier",
      score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
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
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_tscore(y)
#' outlier_tscore(y, mask)
#' outlier_tscore(y, mask, threshold = c(0.8, 0.9))
#' outlier_tscore(y, return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats qnorm sd
#' @export
outlier_tscore = function(x, mask = !is.na(x),
  threshold = c(0.9, 0.95), return.score = FALSE) {
  n = length(x)
  score = (x - mean(x[mask])) / (sd(x[mask]) / sqrt(n))
  if (return.score) {
    score
  } else {
    outlier_factor(case_when(
      is.na(x) ~ NA_character_,
      abs(score) > qt(threshold[2], n - 1) ~ "extreme outlier",
      abs(score) > qt(threshold[1], n -1) ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}


#' Chi-squared test for outliers
#'
#' Performs a Chi-squared test for outliers.
#'
#' @inheritParams outlier_tscore
#'
#' @details the values of `threshold` identify the quantiles of the
#'   Chi-squared distribution used to identify mild and extreme
#'   outliers. Default values are 0.9 for "mild" outliers and 0.95
#'   for "extreme" outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_chisq(y)
#' outlier_chisq(y, mask)
#' outlier_chisq(y, mask, threshold = c(0.8, 0.9))
#' outlier_chisq(y, return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats qchisq var
#' @export
outlier_chisq = function(x, mask = !is.na(x),
  threshold = c(0.9, 0.95), return.score = FALSE) {
  df = length(x) - 1
  score = (x - mean(x[mask])) ^ 2 / var(x[mask])
  if (return.score) {
    score
  } else {
    outlier_factor(case_when(
      is.na(x) ~ NA_character_,
      abs(score) > qchisq(threshold[2], df) ~ "extreme outlier",
      abs(score) > qchisq(threshold[1], df) ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}


#' Median absolute deviation test for outliers
#'
#' Performs a median absolute deviation (MAD) test for outliers.
#'
#' @inheritParams outlier_tscore
#'
#' @details the values of `threshold` identify the multiplier of the
#'   median distance used to identify mild and extreme outliers.
#'   Default values are 1.5 for "mild" outliers and 3.0 for "extreme"
#'   outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_mad(y)
#' outlier_mad(y, mask)
#' outlier_mad(y, mask, threshold = c(1, 2))
#' outlier_mad(y, return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats median
#' @export
outlier_mad = function(x, mask = !is.na(x),
  threshold = c(1.5, 3), return.score = FALSE) {
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
    outlier_factor(case_when(
      is.na(x) ~ NA_character_,
      abs(score) > threshold[2] ~ "extreme outlier",
      abs(score) > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}


#' Isolation Forest outlier detection
#'
#' Performs outlier detection using an Isolation Forest.
#'
#' @param xs A dataframe or list of vectors
#'   (which will be coerced to a numeric matrix).
#' @inheritParams outlier_tukey
#' @param ... Additional arguments to `solitude::isolationForest$new()`.
#'   note that the argument `sample_size` will be overwritten to use the
#'   number of unmasked data points, i.e. `length(which(mask))`.
#'
#' @details the values of `threshold` identify mild and extreme\
#'   outliers based on the Isolation Forest score in the range `[0,1]`.
#'   Default values are 0.8 for "mild" outliers and 0.9 for "extreme"
#'   outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_iforest(list(y))
#' outlier_iforest(list(x, y))
#' outlier_iforest(list(x, y), mask)
#' outlier_iforest(list(x, y), mask, threshold = c(1, 2))
#' outlier_iforest(list(x, y), return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats predict na.omit
#' @export
outlier_iforest = function(xs, mask = !Reduce("|", lapply(xs, is.na)),
  threshold = c(0.8, 0.9), return.score = FALSE, ...) {
  if (!requireNamespace("solitude"))
    stop("Could not find package \"solitude\"")
  p = list_to_ndf(xs)
  d = p[mask, ]
  p.omit = na.omit(p)
  na.mask = invwhich(as.vector(attr(p.omit, "na.action")), nrow(p))
  mod = solitude::isolationForest$new(sample_size = nrow(d), ...)
  mod$fit(d)
  score = mod$predict(p.omit)$anomaly_score
  p["score"] = NA_real_
  p[!na.mask, "score"] = score
  if (return.score) {
    p$score
  } else {
    outlier_factor(case_when(
      is.na(p$score) ~ NA_character_,
      p$score > threshold[2] ~ "extreme outlier",
      p$score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}


#' LOF outlier detection
#'
#' Performs outlier detection using Local Outlier Factor algorithm.
#'
#' @inheritParams outlier_iforest
#' @param ... Additional arguments to `dbscan::lof`, namely
#'   `k`.
#'
#' @details the values of `threshold` identify mild and extreme
#'   outliers based on the LOF score. Scores significantly larger
#'   than 1 indicate outliers. Default values are 1.5 for "mild"
#'   outliers and 2.0 for "extreme" outliers.
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rlnorm(length(x), meanlog = 1, sdlog = 3)
#' y=sin(x) + noise
#' mask = noise < 1
#'
#' outlier_lof(list(y))
#' outlier_lof(list(x, y), mask)
#' outlier_lof(list(x, y), mask, threshold = c(1, 2))
#' outlier_lof(list(x, y), return.score = TRUE)
#'
#' @importFrom dplyr if_else between case_when
#' @export
outlier_lof = function(xs, mask = !Reduce("|", lapply(xs, is.na)),
  threshold = c(1.5, 2), return.score = FALSE, ...) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")
  # need double comma to avoid vector coercion
  xx = as.matrix(list_to_ndf(xs)[mask, ])
  lof.omit = dbscan::lof(xx, ...)
  score = rep(NA_real_, nrow(xx))
  score[mask] = lof.omit
  if (return.score) {
    score
  } else {
    outlier_factor(case_when(
      is.na(score) ~ NA_character_,
      score > threshold[2] ~ "extreme outlier",
      score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}



outlier_glosh = function(x, mask) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")
}


outlier_hdbscan = function(x, threshold = c(0.9, 0.95), ...) {
  # TODO
}