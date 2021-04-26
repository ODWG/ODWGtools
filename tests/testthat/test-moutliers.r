test_that("chi-squared outlier detection works", {

  x = c(1.59, 1.65, 1.71, 1.71, 1.72, 1.73, 1.73,
    1.77, 1.8, 1.84, 1.89, 1.94, 1.95, 2.1, 10.81)
  y = c(1.59, 1.72, 10.81, 1.8, 1.84, 1.71, 1.65,
    1.94, 1.95, 1.89, 1.73, 2.1, 1.77, 1.71, 1.73)
  xs = list(x, y)

  flag.ids = rep(1L, length(x))
  flag.ids[c(3L, 15L)] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(moutlier_chisq(xs), flags)


  x = c(-0.55, -1.34, -0.36, -1.74, -0.32, 1.34, 0.19, -0.98,
    -0.06, 1.66, -0.07, 0.39, 2.05, 0.21, 1.44, -0.56, -0.46,
    -1.26, 1.83, -1.23)
  y = c(30.95, 29.92, 29.42, 30.91, 31.8, 28.63, 30.06, 29.26,
    29.98, 31.32, 28.56, 30.03, 29.79, 28.76, 29.37, 29.71,
    30.21, 30.59, 28.98, 30.77)
  xs = list(x = x, y = y)

  flag.ids = rep(1L, length(x))
  flag.ids[10L] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(moutlier_chisq(xs), flags)

})
