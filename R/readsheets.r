#' Read Visit Sheet
#'
#' Read field visit sheet data.
#'
#' @param workbook The Excel workbook (.xlsx) containing 
#'   visit sheets. The workbook is expected to contain a sheet
#'   named "INTERNAL" which is formatted to contain five 
#'   columns that match the WQP result components: 
#'   "reading_type_name", "analyte_name", "time", "value", "cdec_code".
#' @param timezone The timezone assumed for the visit sheet. 
#'   Default is `"Etc/GMT+8"` (PST).
#' @return A tibble of visit data.
#'
#' @importFrom readxl read_excel
#' @importFrom lubridate as_datetime
#' @export
read_visit = function(workbook, timezone = "Etc/GMT+8") {
  # read entire workbook as table
  raw = suppressMessages(read_excel(workbook, "INTERNAL",
    col_names = TRUE, na = c("", " "), skip = 0,
    trim_ws = TRUE, col_types = "text"))
  # parse timestamps
  raw[, 3] = as_datetime(raw[[3]], tz = timezone)
  missing.rows = is.na(raw[[3]]) | is.na(raw[[4]])
  if (any(missing.rows)) {
    warning(sum(missing.rows), " of ", length(missing.rows),
      " records are missing or improperly formatted in workbook ",
      workbook, ".")
  }
  raw
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
