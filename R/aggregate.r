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
        useNA = if(na.rm) "no" else "ifany"))))
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
    sum(flags == "M")/length(flags) >= 0.5 ~ "M",
    any(flags == "P") ~ "P",
    all(flags %in% c("Q", "G", "M")) ~ "Q",
    all(flags %in% c("G", "M")) ~ "G",
    TRUE ~ "ERROR"
  )
  if (out == "ERROR")
    stop("Could not determine aggregate flag for sequence: ",
      paste(flags, sep = ", "))
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
  if(!missing(flags))
    filter(flag.descriptions, .data$Flag %in% flags)
  else
    flag.descriptions
}
