#' @include rtqc.r
NULL

#' Multivariate test
#'
#' Perform a multivariate rate of change test. This function can also be
#'   used for the neighbor test. See
#' https://cdn.ioos.noaa.gov/media/2017/12/qartod_temperature_salinity_manual.pdf
#' for more information.
#'
#' @inheritParams rtqc_rate
#' @param y A vector of coincident observations, e.g. from a separate
#'   sensor or nearby station.
#'
#' @examples
#' fake.data1 = c(rnorm(10,10), rnorm(10, 50), rnorm(10, 10))
#' fake.data2 = rep(10,30)
#' crossing_rate(fake.data1, fake.data2, c(1.5, 3), 5)
#' crossing_rate(fake.data2, fake.data1, c(1.5, 3), 5)
#' crossing_rate(fake.data1, fake.data1, 1.5, 5)
#'
#' @importFrom dplyr lag lead case_when
#' @export
crossing_rate = function(x, y, n.dev, n.prior) {
  n.dev = rep(n.dev, length.out = 2)
  n.prior = rep(n.prior, length.out = 2)
  xf = rtqc_rate(x, n.dev[1], n.prior[1])
  yf = rtqc_rate(y, n.dev[2], n.prior[2])
  case_when(
    is.na(x) ~ NA_integer_,
    (xf != 1L) & (yf == 1L) ~ 3L,
    TRUE ~ 1L
  )
}

#' Regional Regression
#'
#' Perform regional regression of two stations.
#'
#' @param x,y Vectors of station data used for regression.
#' @param season Optional vector identifying specific
#'   seasons or periods
#' @param plot If `TRUE`, print diagnostic plots.
#'
#'
#' @importFrom stats lm predict
#' @importFrom graphics lines
#' @export
crossing_regression = function(x, y, season = NULL, plot = FALSE) {
  d = data.frame(x = x, y = y)
  m = lm(y ~ x, d)

  d["yhat"] = predict(m, data = d)

  if (plot) {
    plot(d[c("x", "y")], type = "p")
    lines(d[c("x", "yhat")], col = "blue")
  }

}