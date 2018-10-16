#' Gap Test
#' 
#' Perform a data gap test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @param x A vector of timestamps.
#' @param inc a character string defining the expected time increment.
#' @return An integer vector of test flags.
#'
#' @importFrom dplyr near case_when
#' @export 
gap_test = function(x, inc = "15 mins", con = c("near", "less than", "greater than")) {
  if (!any(class(x) %in% c("Date", 'POSIXt')))
    stop('argument "x" must be of class "Date" or "POSIXt"')
  con = match.arg(con, c("near", "less than", "greater than"))
  inc = lapply(strsplit(inc, " "), function(x) 
    as.difftime(as.numeric(x[1]), units = x[2]))[[1]]
  con.fun = switch(con,
    "near" = near,
    "greater than" = `>`,
    "less than" = `<`
  )
  x.diff = diff(x)
  c(NA_integer_, case_when(
    is.na(x.diff) ~ NA_integer_,
    con.fun(x.diff, inc) ~ 1L,
    TRUE ~ 4L
  ))
}


#' Range Test
#' 
#' Perform a range test. See 
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @param x A vector of values.
#' @param sensor.range A length-2 numeric vector identifying the sensor
#'   measurement range. 
#' @param user.range A length-2 numeric vector identifying a reasonable
#'   measurement range for the provided data `x`. Typically specific to
#'   location and/or climate and based on expert judgment.
#' @return An integer vector of test flags.
#'
#' @importFrom dplyr between case_when
#' @export 
range_test = function(x, sensor.range = c(0, 50e3), user.range = c(5, 45e3)) {
  case_when(
    is.na(x) ~ NA_integer_,
    !between(x, sensor.range[1], sensor.range[2]) ~ 4L,
    !between(x, user.range[1], user.range[2]) ~ 3L,
    TRUE ~ 1L
  )
}
