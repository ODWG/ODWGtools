#' Read Visit Sheet
#'
#' Read field visit sheet data.
#'
#' @param workbook The Excel workbook (.xlsx) containing 
#'   visit sheets.
#' @param sheet The visit sheet to read.
#' @return A tibble of visit data.
#'
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_date hm fit_to_timeline
#' @importFrom purrr map map2
#' @importFrom stats setNames
#' @export
read_visit = function(workbook, sheet) {
  # sheet specs
  cells = list(
    field.date = "F4",
    station.name = "F5",
    performed.by = "F6",
    arrival.time = "P4",
    exchange.time = "P5",
    departure.time = "P6",
    verification.sonde = "Z4",
    removed.sonde = "Z5",
    installed.sonde = "Z6",
    time = "H10:AC10",
    water.temperature = "H11:AC11",
    specific.conductivity = "H12:AC12",
    dissolved.oxygen = "H13:AC13",
    pH = "H14:AC14",
    turbidity = "H15:AC15",
    fluorescence = "H16:AC16",
    stage = "H20:AC20",
    notes = "D22"
  )
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
    stage = c(1L, 4L, 19L, 22L),
    notes = 1L
  )
  formatter = list(
    field.date = function(x) as_date(as.list(x)[[1]]),
    station.name = function(x) x[[1]],
    performed.by = function(x) x[[1]],
    arrival.time = function(x) hm(format(x[[1]] / 100, nsmall = 2)),
    exchange.time = function(x) hm(format(x[[1]] / 100, nsmall = 2)),
    departure.time = function(x) hm(format(x[[1]] / 100, nsmall = 2)),
    verification.sonde = function(x) x[[1]],
    removed.sonde = function(x) x[[1]],
    installed.sonde = function(x) x[[1]],
    time = function(x) map(setNames(as.list(x), NULL),
      ~ hm(format(.x / 100, nsmall = 2))),
    water.temperature = function(x) unlist(x, use.names = FALSE),
    specific.conductivity = function(x) unlist(x, use.names = FALSE),
    dissolved.oxygen = function(x) unlist(x, use.names = FALSE),
    pH = function(x) unlist(x, use.names = FALSE),
    turbidity = function(x) unlist(x, use.names = FALSE),
    fluorescence = function(x) unlist(x, use.names = FALSE),
    stage = function(x) unlist(x, use.names = FALSE),
    notes = function(x) x[[1]]
  )
  # read in data
  raw = map2(cells, ranges,
    ~ suppressMessages(read_excel(workbook, sheet,
    range = .x, col_names = FALSE)[.y]))
  # post-processing
  results = map2(raw, formatter, ~ .y(.x))
  results$field.date = as_date(results$field.date)
  results$arrival.time = fit_to_timeline(results$field.date + results$arrival.time)
  results$exchange.time = fit_to_timeline(results$field.date + results$exchange.time)
  results$departure.time = fit_to_timeline(results$field.date + results$departure.time)
  results$time = map(results$time, ~ fit_to_timeline(results$field.date + .x))
  results[["sample"]] = rep(c("Arrival", "Pre-cleaned", "Post-cleaned",
    "Departure"), each = 2)
  results[["sonde"]] = c(rep(c("Outgoing", "Verification"), 3), "Installed",
    "Verification")
  # need to decide how to handle stage
  results$stage = NULL
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
