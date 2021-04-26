#' @include outliers.r
NULL


#' Chi-squared test for multivariate outliers
#'
#' Performs a Chi-squared test for multivariate outliers.
#'
#' @param xs A dataframe or list of vectors
#'   (which will be coerced to a numeric matrix).
#' @inheritParams outlier_tukey
#'
#' @examples
#' x = seq(0, 34, by = 0.25)*pi
#' noise = rnorm(length(x), mean = 1, sd = 3)
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' moutlier_chisq(list(x, y))
#' moutlier_chisq(list(x, y), mask)
#' moutlier_chisq(list(x, y), mask, threshold = c(0.8, 0.9))
#' moutlier_chisq(list(x, y), return.score = TRUE)
#'
#' @importFrom dplyr case_when
#' @importFrom stats qchisq var cov mahalanobis qnorm
#' @export
moutlier_chisq = function(xs, mask = !Reduce("|", lapply(xs, is.na)),
  threshold = c(0.9, 0.95), return.score = FALSE) {
  p = list_to_ndf(xs)
  d = subset(p, mask)
  p.omit = na.omit(p)
  na.mask = invwhich(as.vector(attr(p.omit, "na.action")), nrow(p))
  center = colMeans(p.omit)
  covmat = cov(p.omit)
  score = mahalanobis(p, center, covmat)
  df = ncol(p.omit)
  if (return.score) {
    score
  } else {
    .outlier_factor(case_when(
      is.na(score) ~ NA_integer_,
      abs(score) > qchisq(threshold[2], df) ~ 4L,
      abs(score) > qchisq(threshold[1], df) ~ 3L,
      TRUE ~ 1L
    ))
  }
}


#' Isolation Forest multivariate outlier detection
#'
#' Performs outlier detection using an Isolation Forest.
#'
#' @inheritParams moutlier_chisq
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
#' y = sin(x) + noise
#' mask = noise < 1
#'
#' if (requireNamespace("solitude", quietly = TRUE)) {
#'   moutlier_iforest(list(y))
#'   moutlier_iforest(list(x, y))
#'   moutlier_iforest(list(x, y), mask)
#'   moutlier_iforest(list(x, y), mask, threshold = c(1, 2))
#'   moutlier_iforest(list(x, y), return.score = TRUE)
#' }
#'
#' @importFrom dplyr if_else between case_when
#' @importFrom stats predict na.omit
#' @export
moutlier_iforest = function(xs, mask = !Reduce("|", lapply(xs, is.na)),
  threshold = c(0.8, 0.9), return.score = FALSE, ...) {
  if (!requireNamespace("solitude"))
    stop("Could not find package \"solitude\"")
  p = list_to_ndf(xs)
  d = subset(p, mask)
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
    .outlier_factor(case_when(
      is.na(p$score) ~ NA_character_,
      p$score > threshold[2] ~ "extreme outlier",
      p$score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}


#' LOF outlier detection
#'
#' Performs multivariate outlier detection using Local Outlier Factor
#' algorithm.
#'
#' @inheritParams moutlier_chisq
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
#' if (requireNamespace("dbscan", quietly = TRUE)) {
#'   moutlier_lof(list(y))
#'   moutlier_lof(list(x, y), mask)
#'   moutlier_lof(list(x, y), mask, threshold = c(1, 2))
#'   moutlier_lof(list(x, y), return.score = TRUE)
#' }
#'
#' @importFrom dplyr if_else between case_when
#' @export
moutlier_lof = function(xs, mask = !Reduce("|", lapply(xs, is.na)),
  threshold = c(1.5, 2), return.score = FALSE, ...) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")

  #REDO THIS
  xx = as.matrix(subset(list_to_ndf(xs), mask))
  lof.omit = dbscan::lof(xx, ...)
  score = rep(NA_real_, nrow(xx))
  score[mask] = lof.omit
  if (return.score) {
    score
  } else {
    .outlier_factor(case_when(
      is.na(score) ~ NA_character_,
      score > threshold[2] ~ "extreme outlier",
      score > threshold[1] ~ "mild outlier",
      TRUE ~ "not outlier"
    ))
  }
}



moutlier_glosh = function(x, mask) {
  if (!requireNamespace("dbscan"))
    stop("Could not find package \"dbscan\"")
}


moutlier_hdbscan = function(x, threshold = c(0.9, 0.95), ...) {
  # TODO
}
