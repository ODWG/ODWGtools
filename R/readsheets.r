#' Read Visit Sheet
#'
#' Read field visit sheet data.
#'
#' @param workbook The Excel workbook (.xlsx) containing 
#'   visit sheets.
#' @param sheet The visit sheet to read.
#' @param timezone The timezone assumed for the visit sheet. 
#'   Default is "US/Pacific" (PST/PDT).
#' @return A tibble of visit data.
#'
#' @importFrom rlang .data parse_expr
#' @importFrom tibble as_tibble
#' @importFrom dplyr slice select
#' @importFrom readxl read_excel
#' @importFrom lubridate as_date hm fit_to_timeline tz<-
#' @importFrom purrr map map2
#' @importFrom stats setNames
#' @export
read_visit = function(workbook, sheet, timezone = "US/Pacific") {
  # read entire workbook as table
  raw = suppressMessages(read_excel(workbook, sheet, range = "A1:AE24",
    col_names = FALSE, col_types = "text", na = "",
    trim_ws = TRUE, skip = 0))
  names(raw) = c(LETTERS, paste("A", LETTERS[1:5], sep = ""))
  # separate table into sub-tables
  cell.cols = list(
    field.date = parse_expr("F"),
    station.name = parse_expr("F"),
    performed.by = parse_expr("F"),
    arrival.time = parse_expr("P"),
    exchange.time = parse_expr("P"),
    departure.time = parse_expr("P"),
    verification.sonde = parse_expr("Z"),
    removed.sonde = parse_expr("Z"),
    installed.sonde = parse_expr("Z"),
    time = parse_expr("H:AC"),
    water.temperature = parse_expr("H:AC"),
    specific.conductivity = parse_expr("H:AC"),
    dissolved.oxygen = parse_expr("H:AC"),
    pH = parse_expr("H:AC"),
    turbidity = parse_expr("H:AC"),
    fluorescence = parse_expr("H:AC"),
    stage = parse_expr("H:AC"),
    notes = parse_expr("D")
  )
  cell.rows = list(
    field.date = 4L,
    station.name = 5L,
    performed.by = 6L,
    arrival.time = 4L,
    exchange.time = 5L,
    departure.time = 6L,
    verification.sonde = 4L,
    removed.sonde = 5L,
    installed.sonde = 6L,
    time = 10L,
    water.temperature = 11L,
    specific.conductivity = 12L,
    dissolved.oxygen = 13L,
    pH = 14L,
    turbidity = 15L,
    fluorescence = 16L,
    stage = 20L,
    notes = 22L
  )
  grouped.results = map2(cell.cols, cell.rows,
    ~ slice(select(raw, !!.x), .y))
  # extract relevant data from subtables
  ranges = list(
    field.date = 1L,
    station.name = 1L,
    performed.by = 1L,
    arrival.time = 1L,
    exchange.time = 1L,
    departure.time = 1L,
    verification.sonde = 1L,
    removed.sonde = 1L,
    installed.sonde = 1L,
    time = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    water.temperature = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    specific.conductivity = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    dissolved.oxygen = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    pH = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    turbidity = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    fluorescence = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    stage = c(1L, 4L, 7L, 10L, 13L, 16L, 19L, 22L),
    notes = 1L
  )
  subset.results = map2(grouped.results, ranges, ~ select(.x, .y))
  # reformat data in subtables
  formatter = list(
    field.date = function(x) as_date(as.integer(as.list(x)[[1]]), origin = "1899-12-30"),
    station.name = function(x) x[[1]],
    performed.by = function(x) x[[1]],
    arrival.time = function(x) hm(format(as.integer(x[[1]]) / 100, nsmall = 2)),
    exchange.time = function(x) hm(format(as.integer(x[[1]]) / 100, nsmall = 2)),
    departure.time = function(x) hm(format(as.integer(x[[1]]) / 100, nsmall = 2)),
    verification.sonde = function(x) x[[1]],
    removed.sonde = function(x) x[[1]],
    installed.sonde = function(x) x[[1]],
    time = function(x) map(setNames(as.list(x), NULL),
      ~ hm(format(as.integer(.x) / 100, nsmall = 2))),
    water.temperature = function(x) as.numeric(unlist(x, use.names = FALSE)),
    specific.conductivity = function(x) as.numeric(unlist(x, use.names = FALSE)),
    dissolved.oxygen = function(x) as.numeric(unlist(x, use.names = FALSE)),
    pH = function(x) as.numeric(unlist(x, use.names = FALSE)),
    turbidity = function(x) as.numeric(unlist(x, use.names = FALSE)),
    fluorescence = function(x) as.numeric(unlist(x, use.names = FALSE)),
    stage = function(x) as.numeric(unlist(x, use.names = FALSE)),
    notes = function(x) x[[1]]
  )
  # datetime formatting
  results = map2(subset.results, formatter, ~ .y(.x))
  results$arrival.time = fit_to_timeline(results$field.date + results$arrival.time)
  tz(results$arrival.time) = timezone
  results$exchange.time = fit_to_timeline(results$field.date + results$exchange.time)
  tz(results$exchange.time) = timezone
  results$departure.time = fit_to_timeline(results$field.date + results$departure.time)
  tz(results$departure.time) = timezone
  results$time = do.call(c, map(results$time, ~ fit_to_timeline(results$field.date + .x)))
  tz(results$time) = "US/Pacific"
  # add sample and sonde labels
  results[["sample"]] = rep(c("Arrival", "Dirty", "Clean",
    "Departure"), each = 2)
  results[["sonde"]] = c(rep(c("Outgoing", "Verification"), 3), "Installed",
    "Verification")
  # return formatted visit data
  as_tibble(results)
}


#' Read Laboratory Calibration Sheet
#'
#' Read laboratory calibration sheet data.
#'
#' @param workbook The Excel workbook (.xlsx) containing 
#'   calibration sheets.
#' @param sheet The calibration sheet to read.
#' @return A tibble of calibration data.
#'
#' @export
read_calibration = function(workbook, sheet) {
  stop("not implemented")
}
