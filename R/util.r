#' String to difftime
#'
#' Convert a string to a `difftime` object.
#' @param s A string
#' @return A `difftime` object.
#'
#' @keywords internal
string_to_difftime = function(s) {
  value = as.numeric(strsplit(s, " ")[[c(1, 1)]])
  increment = strsplit(s, " ")[[c(1, 2)]]
  increment = match.arg(increment,
    c("secs", "mins", "hours", "days", "weeks"))
  as.difftime(value, units = increment)
  }

#' List to Numeric Data Frame
#'
#' Utility function to convert a list of vectors to a numeric dataframe.
#' If list is not named, default names `"X1"`, `"X2"`, etc. will be
#' assigned.
#'
#' @param xs A list of equal-length vectors.
#' @return A dataframe.
#'
#' @keywords internal
list_to_ndf = function(xs) {
  if (!is.list(xs)) {
    stop("\"xs\" must be a dataframe of list of vectors.")
  } else if (!Reduce(identical, lapply(xs, length))) {
    stop("Elements of \"xs\" must be equal length.")
  }
  # add default names if not included
  if (is.null(names(xs))) {
    names(xs) = paste0("X", seq_along(xs))
  }
  # check if elements are coercible
  nas = sapply(xs, function(x) sum(is.na(x)))
  new.nas = sapply(xs, function(x) sum(is.na(as.numeric(x))))
  if (any(new.nas > nas)) {
    stop("Elements of \"xs\" must be convertible to numeric.")
  }
  as.data.frame(lapply(xs, as.numeric))
}

#' Inverse Which
#'
#' Convert output of [`base::which()`] to a logical vector.
#'
#' @param i A vector of indices.
#' @param len The length of the original vector.
#' @return A logical vector.
#'
#' @keywords internal
invwhich = function(i, len) {
  `[<-`(logical(len), i, TRUE)
}
