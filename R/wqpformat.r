#' Visit to WQP
#'
#' Format visit data for insertion to WQP.
#' 
#' @inheritParams fouling_error
#' @param wqp.listings A listing of result IDs from the WQP
#'   database, e.g. output of `wqpr::list_all()`. Used to 
#' @return A tibble formatted for insertion to WQP.
#'
#' @seealso [wqpr::list_all()]
#'
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom dplyr transmute case_when
#'
visit_to_wqp = function(visit.data, wqp.listings) {
  stop("not implemented")
  visit.long = gather(visit.data, .data$analyte, .data$value,
    .data$water.temperature, .data$specific.conductivity,
    .data$dissolved.oxygen, .data$pH, .data$turbidity,
    .data$fluorescence)
  visit.formatted = transmute(visit.long,
    CDEC_CODE = .data$station.name,
    READING_TYPE_NAME = case_when(
      .data$sample == "Arrival" & .data$sonde == "Outgoing" ~ "a. Arrvl Sonde Reading",
      .data$sample == "Arrival" & .data$sonde == "Verification" ~ "b. Arrvl. Verification",
      .data$sample == "Pre-cleaned" & .data$sonde == "Outgoing" ~ "c. Dirty Sonde Reading",
      .data$sample == "Pre-cleaned" & .data$sonde == "Verification" ~ "d. Dirty Verification",
      .data$sample == "Post-cleaned" & .data$sonde == "Outgoing" ~ "e. Clean Sonde Reading",
      .data$sample == "Post-cleaned" & .data$sonde == "Verification" ~ "f. Clean Verification",
      .data$sample == "Departure" & .data$sonde == "Installed" ~ "g. Dep. Sonde Reading",
      .data$sample == "Departure" & .data$sonde == "Verification" ~ "h. Dep. Verification",
      TRUE ~ "ERROR"
    ),
    CONSTITUENT_NAME = case_when(
      .data$analyte == "water.temperature" ~ "(Temp).*(YSI Sonde)",
      .data$analyte == "specific.conductivity" ~ "(SC).*(YSI Sonde)",
      .data$analyte == "dissolved.oxygen" ~ "(DO).*(YSI Sonde)",
      .data$analyte == "fluorescence" ~ "(Fluor).*(YSI Sonde)",
      .data$analyte == "pH" ~ "(pH).*(YSI Sonde)",
      .data$analyte == "turbidity" ~ "(Turbidity).*(YSI Sonde)",
      TRUE ~ "ERROR"
    ),
    time = .data$time,
    value = .data$value,
    qaqc_flag_id = "U"    
  )
  if (any(visit.formatted$READING_TYPE_NAME == "ERROR"))
    stop("Problem parsing 'sonde' and 'sample' fields.")
  if (any(visit.formatted$CONSTITUENT_NAME == "ERROR"))
    stop("Problem parsing 'analyte' field.")




  # ResultID, DATE/TIME, VALUE, QA/QC FLAG
}

