#' Calculate Fouling Error
#'
#' Calculate fouling error from pre/post cleaning visit data.
#'
#' @param visit.data Processed visit data, i.e. output 
#'   from [read_visit].
#' @return A tibble of fouling calculations.
#'
#' @importFrom tidyr gather spread
#' @importFrom dplyr filter select transmute left_join
#' @importFrom rlang .data
#' @export
fouling_error = function(visit.data) {
  visit.difference = gather(select(
    filter(visit.data,
      .data$sample %in% c("Pre-cleaned", "Post-cleaned")),
    "sample", "sonde", "water.temperature",
    "specific.conductivity", "dissolved.oxygen",
    "pH", "turbidity", "fluorescence"),
    "analyte", "value", - "sample", - "sonde")
  diff.within = transmute(
    spread(visit.difference, .data$sample, .data$value),
    sonde = .data$sonde, analyte = .data$analyte,
    within.diff = .data$`Post-cleaned` - .data$`Pre-cleaned`)

  diff.between = transmute(
    spread(diff.within, .data$sonde, .data$within.diff),
    analyte = .data$analyte,
    between.diff = .data$Outgoing - .data$Verification)

  transmute(left_join(diff.between,
    filter(visit.difference, .data$sonde == "Outgoing",
      .data$sample == "Pre-cleaned"),
    by = "analyte"), analyte = .data$analyte,
    fouling.error = .data$between.diff,
    fouling.percent.error = 100 * .data$between.diff / .data$value)
}

#' Calculate Calibration Drift Error
#' Calculate calibration drift error from laboratory 
#' calibration data.
#'
#' @param lab.data Processed laboratory calibration data,
#'   e.g. output from [read_calibration].
#' @return a tibble of calibration drift calculations.
#'
#' @importFrom rlang .data
#' @export
calibration_drift_error = function(lab.data) {
  # return table with columns
  # analyte | calibration.error | calibration.percent.error
  stop("not implemented")
}


#' Calculate Total Drift Error
#' Calculate total drift error from combined fouling and
#' calibration drift data.
#'
#' @param fouling.data Fouling error data,
#'   e.g. output from [fouling_error].
#' @param calibration.data Calibration drift error data,
#'   e.g. output from [calibration_drift_error].
#' @return a tibble of total drift calculations.
#'
#' @importFrom dplyr full_join transmute
#' @importFrom rlang .data
#' @export
total_drift_error = function(fouling.data, calibration.data) {
  transmute(full_join(fouling.data, calibration.data, by = "analyte"),
    analyte = .data$analyte,
    total.drift.error = abs(.data$fouling.error) +
      abs(.data$calibration.error),
    total.drift.percent.error = abs(.data$fouling.percent.error) +
      abs(.data$calibration.percent.error)
  )
}

ratings = function(drift.data) {

}