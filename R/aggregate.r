#' @include ODWGtools.r
NULL

#' Aggregate Quality Control Factor
#'
#' Convert aggregate quality control labels to ordered factor.
#'
#' @param x A vector of integers.
#' @return A vector or ordered factors.
#'
#' @keywords internal
.aggregate_factor = function(x) {
  factor(x, c(1L, 2L, 3L, 4L),
  c("provisionally approved", "not evaluated", "in need of review",
    "rejected"),
    ordered = TRUE)
}

#' Aggregate Quality Control Flags
#'
#' Return a data frame of aggregate quality control flags
#' and ranks.
#'
#' @export
list_aggregate_flags = function() {
  f = .aggregate_factor(NA)
  data.frame(flag = levels(f), rank = seq_along(levels(f)))
}


#' Flag Aggregators
#'
#' Helper functions for aggregating flags.
#'
#' @param m A matrix of flags
#' @param na.rm Logical indicating whether missing flags should be
#'   ignored when calculating the aggregate flag.
#' @return An integer vector with same length as rows of `m`.
#'
#' @name flag-aggregators
#' @keywords internal
NULL

#' @describeIn flag-aggregators Highest flag value
#' @keywords internal
.highest = function(..., na.rm) {
  pmax.int(..., na.rm = na.rm)
}

#' @describeIn flag-aggregators Lowest flag value
#' @keywords internal
.lowest = function(..., na.rm) {
  pmin.int(..., na.rm = na.rm)
}

#' @describeIn flag-aggregators Majority flag value
#' @importFrom purrr map_if map_int
#' @keywords internal
.majority = function(..., na.rm) {
  m = cbind(...)
  tbls = apply(m, 1, table, useNA = "always")
  if (na.rm) {
    tbls = map_if(tbls, ~ any(is.na(names(.x))),
      ~ .x[!is.na(names(.x))])
  }
  map_int(tbls, ~ as.integer(names(which.max(.x))))
}

#' @describeIn flag-aggregators ODWG recommended method. Uses
#'  `.highest` when a flag value of 4 is present, otherwise
#'  uses `.majority`.
#' @importFrom purrr map_if map_int
#' @keywords internal
.odwg = function(..., na.rm) {
  m = cbind(...)
  tbls = apply(m, 1, table, useNA = "always")
  if (na.rm) {
    tbls = map_if(tbls, ~ any(is.na(names(.x))),
      ~ .x[!is.na(names(.x))])
  }
  tbls = map_if(tbls, ~ "4" %in% names(.x),
    ~ .x["4"])
  map_int(tbls, ~ as.integer(names(which.max(.x))))
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
#' @importFrom purrr map_lgl
#' @export
aggregate_tests = function(..., by = c("odwg", "highest", "lowest",
  "majority"), na.rm = TRUE) {
  by = match.arg(by, c("odwg", "highest", "lowest", "majority"))
  fun = switch(by,
    "highest" = .highest,
    "lowest" = .lowest,
    "majority" = .majority,
    "odwg" = .odwg
   )
  is_ordered = function(x) {
    is.ordered(x) || is.integer(x)
  }
  colclasses = map_lgl(list(...), is_ordered)
  if (!all(colclasses)) {
    stop("Data vector is not ordered (function argument(s) ",
      paste(which(!colclasses), collapse = ", "), ")")
  }
  .aggregate_factor(fun(..., na.rm = na.rm))
}
