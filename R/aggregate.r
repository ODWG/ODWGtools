#' Daily Tidal Mean
#'
#' Compute the daily low-tide or high-tide average.
#'
#' @param x A vector of values from a single day.
#' @param y A vector of water surface elevations from a single day.
#' @param tide Return the high-tide ("high") or low-tide ("low")
#'  average.
#' @param smooth If `TRUE`, A loess smoother will be applied to the
#'   data prior to detecting the water surface elevation minimums
#'   or maximums.
#' @param neighborhood The neighborhood (number of points) used to
#'   determine the `span` argument to [`stats::loess()`]. A time
#'   window of 6 hours was found to work well with tidal data, which
#'   corresponds to 24 points for 15-minute data or 6 points for
#'   hourly data.
#' @param family The `family` argument to [`stats::loess()`]. The
#'   default family "symmetric" was found to work well with 15-minute
#'   tidal data.
#' @param degree The `degree` argument to [`stats::loess()`]. The
#'   default degree of 2 was found to work well with 15-minute
#'   tidal data.
#' @param ... Additional arguments to [`stats::loess()`].Note that
#'   the value of `span` is computed from the argument `neighborhood`.
#' @param plot If `TRUE`, printdiagnostic plots of the loess smoothing.
#' @return The daily tidal mean of `x`, i.e. the average of high-
#'   or low-tide values for the day.
#'
#' @importFrom stats loess predict
#' @importFrom graphics lines
#' @importFrom dplyr lag lead
#' @export
daily_tidal_mean = function(x, y, tide = c("high", "low"),
  smooth = TRUE, neighborhood = 24, family = "symmetric",
    degree = 2, ..., plot = FALSE) {
  tide = match.arg(tide, c("high", "low"))
  if (tide == "high") {
    compare.fun = `>=`
  } else {
    compare.fun = `<=`
  }
  span = neighborhood / length(y)
  # apply smoothing if specified
  if (smooth) {
    d = data.frame(t = seq_along(y), y = y)
    if (plot) {
      plot(y, type = "l")
    }
    y = tryCatch({
      ysmooth = predict(loess(y ~ t, data = d, span = span,
        degree = degree, family = family))
      if (sqrt(mean((d$y - y) ^ 2)) > 0.05) {
        warning("RMSE of loess-smoothed stage is greater than 0.05")
      }
      ysmooth
    }, error = function(e) {
      warning(paste(e))
      NA
    })
    if (plot) {
      tryCatch(lines(y, col = "red", lty = 2),
        error = function(e) warning(paste(e)))
    }
  }
  select.points = compare.fun(y, lead(y, n = 1, default = NA)) &
    compare.fun(y, lag(y, n = 1, default = NA))
  if (all(is.na(select.points))) {
    warning("Could not identify low/high tide locations.")
    NA
  } else {
    mean(x[select.points], na.rm = TRUE)
  }
}

#' Aggregate Test Results
#'
#' Aggregate the results of multiple tests.
#'
#' @param ... One of more vectors of test results.
#' @param by The method to use for aggregating. When `by = "highest"`
#'   the largest flag value from the set of tests as the indicator
#'   of the overall quality of the observation. When `by = "lowest"`
#'   the smallest flag value from the set of tests as the indicator
#'   of the overall quality of the observation. When `by = "majority"`
#'   a majority-vote algorithm is used to determine the overall quality
#'   of the observation.
#' @param na.rm Logical indicating whether missing flags should be
#'   ignored when calculating the aggregate flag.
#' @return An integer vector of test flags.
#'
#' @examples
#' flags = replicate(3, sample(c(1L,3L,4L), 10, replace = TRUE))
#' aggregate_tests(flags[,1], flags[,2], flags[,3])
#' aggregate_tests(flags[,1], flags[,2], flags[,3], by = "majority")
#'
#' @importFrom purrr pmap_int map_chr
#' @importFrom dplyr if_else
#' @export
aggregate_tests = function(..., by = c("highest", "lowest", "majority"),
  na.rm = TRUE) {

  by = match.arg(by, c("highest", "lowest", "majority"))
  colclasses = map_chr(list(...), class)
  if (any(colclasses != "integer"))
    stop("Data vector is not an integer (function argument(s) ",
      paste(which(colclasses != "integer"), collapse = ", "), ")")
  fun = switch(by,
    highest = function(..., na.rm) {
      r = suppressWarnings(max(..., na.rm = na.rm))
      if_else(is.finite(r), as.integer(r), NA_integer_)
    },
    lowest = function(..., na.rm) {
      r = suppressWarnings(min(..., na.rm = na.rm))
      if_else(is.finite(r), as.integer(r), NA_integer_)
    },
    majority = function(..., na.rm) {
      as.integer(names(which.max(table(c(...),
        useNA = if (na.rm) "no" else "ifany"))))
    }
  )
  pmap_int(list(..., na.rm = na.rm), fun)
}

#' Aggregate QAQC Flags
#'
#' A set of rules for handling flags during data aggregation. This
#' function is intended to be called within an aggregation statement,
#' e.g. within a call to `dplyr::summarize` to average 15-minute data
#' into hourly data, etc.
#'
#' @param flags a vector of QAQC Flag codes.
#' @return A single representative QAQC Flag code.
#'
#' @details The following rules are applied to aggregate QAQC Flags,
#' in the specified order of precedence:
#'
#'  - `"X"` if *any* flags in the vector are `"X"`.
#'  - `"B"` if *any* flags in the vector are `"B"`.
#'  - `"U"` if *any* flags in the vector are `"U"`.
#'  - `"A"` if *any* flags in the vector are `"A"`.
#'  - `"M"` if more than 50% of flags in the vector are `"M"`.
#'  - `"P"` if *any* non-missing flags in the vector are `"P"`.
#'  - `"Q"` if *all* non-missing flags in the vector are `"Q" or "G"`.
#'  - `"G"` if *all* non-missing flags in the vector are `"G"`.
#'  - For all other cases, use the majority flag value.
#'
#' Any result not captured by the above rules will generate an error.
#'
#' @importFrom dplyr case_when
#' @export
aggregate_flags = function(flags) {
  out = case_when(
    any(flags == "X") ~ "X",
    any(flags == "B") ~ "B",
    any(flags == "U") ~ "U",
    any(flags == "A") ~ "A",
    sum(flags == "M") / length(flags) >= 0.5 ~ "M",
    any(flags == "P") ~ "P",
    all(flags %in% c("Q", "G", "M")) ~ "Q",
    all(flags %in% c("G", "M")) ~ "G",
    TRUE ~ names(which.max(table(c(flags),
        useNA = "ifany")))
  )
  out
}



#' Flag Descriptions
#'
#' return a table of flag values with descriptions and notes.
#'
#' @param flags A subset of flag values to return.
#'
#' @importFrom dplyr filter
#' @export
flag_descriptions = function(flags) {
  if (!missing(flags))
    filter(flag.descriptions, .data$Flag %in% flags)
  else
    flag.descriptions
}
