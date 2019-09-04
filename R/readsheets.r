#' Read Event Sheet
#'
#' Read event data from an electronic form.
#'
#' @param workbook The Excel workbook (.xlsx) containing 
#'   the event data. See 'details' for the required format.
#' @param timezone The timezone assumed for the visit sheet. 
#'   Default is `"Etc/GMT+8"` (PST).
#' @return A tibble of visit data.
#'
#' @details The workbook is expected to contain three sheets:
#'   - "RESULT": this sheet is formatted to contain
#'     columns that match the WQP *result* components: 
#'     "cdec_code", "interval_name", "analyte_name", 
#'     "unit_name", rank_name", "reading_type_name",
#'     "time", "value". 
#'   - "EVENT: this sheet is formatted to contain 
#'     columns that match the WQP *event* components: 
#'     "cdec_code", "event_type_name", "contact_name", 
#'     "arrival_time", "restart_time", "departure_time",
#'     "reason_name", "summary_name", "comments".
#'   - "ACTION": this sheet is formatted to contain columns
#'     that match the WQP *action* components:
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate as_datetime
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export
read_event = function(workbook, timezone = "Etc/GMT+8") {
  # read EVENT sheet as table
  if ("EVENT" %in% sheets) {
    event = suppressMessages(read_excel(workbook, "EVENT",
    col_names = TRUE, na = c("", " "), skip = 0,
    trim_ws = TRUE, col_types = "text"))
    # parse event timestamps
    event = mutate(event,
    arrival_time = as_datetime(.data$arrival_time,
      tz = timezone),
    restart_time = as_datetime(.data$restart_time,
      tz = timezone),
    departure_time = as_datetime(.data$departure_time,
      tz = timezone)
    )
    # warn about missing entries
    missing.entries = apply(event, 1, is.na)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
      " event records are missing or improperly formatted in workbook ",
      workbook, ".")
    }
  } else {
    event = NULL
  }
  # read RESULT sheet as table
  sheets = excel_sheets(workboook)
  if ("RESULT" %in% sheets) {
    result = suppressMessages(read_excel(workbook, "RESULT",
    col_names = TRUE, na = c("", " "), skip = 0,
    trim_ws = TRUE, col_types = "text"))
    # parse result timestamps
    result = mutate(result,
    time = as_datetime(.data$time, tz = timezone))
    # warn about missing entries
    missing.entries = is.na(result$time) | is.na(result$value)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
      " result records are missing or improperly formatted in workbook ",
      workbook, ".")
    }
  } else {
    result = NULL
  }
  # read ACTION sheet as table
  if ("ACTION" %in% sheets) {
    action = suppressMessages(read_excel(workbook, "ACTION",
    col_names = TRUE, na = c("", " "), skip = 0,
    trim_ws = TRUE, col_types = "text"))
    # parse ACTION timestamps
    ## TODO
    # warn about missing entries
    ##  if (any(missing.entries)) {
    ##    warning(sum(missing.entries), " of ", length(missing.entries),
    ##      " action records are missing or improperly formatted in workbook ",
    ##      workbook, ".")
    ##  }
  } else {
    action = NULL
  }
  tibble(event = list(event), result = list(result),
    action = list(action))
}
