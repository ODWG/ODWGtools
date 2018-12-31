#' Aggregate Test Results
#'
#' Aggregate the results of multiple tests.
#'
#' @param ... One of more vectors of test results.
#' @param fun The function used to aggregate. The default `max` is the
#'   most conservative approach, in that it uses the largest flag value
#'   from the set of tests as the indicator of the overall quality of the
#'   observation. 
#' @param fun.args A list of additional arguments to `fun`. By default,
#'   the argument `na.rm` is passed to `fun`.
#' @return An integer vector of test flags.
#'
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
      r = max(..., na.rm = na.rm)
      if_else(is.finite(r), as.integer(r), NA_integer_)
    },
    lowest = function(..., na.rm) {
      r = min(..., na.rm = na.rm)
      if_else(is.finite(r), as.integer(r), NA_integer_)
    },
    majority = function(..., na.rm) {
      as.integer(names(which.max(table(c(...),
        useNA = if(na.rm) "no" else "ifany"))))
    }
  )
  pmap_int(list(..., na.rm = na.rm), fun)
}




aggregate_flags = function(flags) {
  case_when(
    any(flag == "X") ~ "X",
    any(flag == "B") ~ "B",
    any(flag == "U") ~ "U",
    any(flag == "A") ~ "A",
    any(flag == "P") ~ "P",
    all(flag == "M") ~ "M",
    all(flag == "G") ~ "G",
    all(flag %in% c("Q", "G")) ~ "Q",
    all(flag %in% c("M", "G", "Q")) ~ "Q",
    TRUE ~ "Z"
  )

}