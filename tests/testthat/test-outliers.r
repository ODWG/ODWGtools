test_that("tukey outlier detection works", {

  x = c(54, 44, 42, 46, 87, 48, 56, 52)
  flag.ids = rep(1L, length(x))
  flag.ids[5] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(outlier_tukey(x), flags)

  x = c(87, 83, 60, 85, 97, 91, 95, 93)
  flag.ids = rep(1L, length(x))
  flag.ids[3] = 3L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(outlier_tukey(x), flags)

  mask = rep(TRUE, length(x))
  mask[3] = FALSE
  flag.ids = rep(1L, length(x))
  flag.ids[3] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(outlier_tukey(x, mask), flags)

})


test_that("t-test outlier detection works", {

  x = c(1.59, 1.65, 1.71, 1.71, 1.72, 1.73, 1.73,
    1.77, 1.8, 1.84, 1.89, 1.94, 1.95, 2.1, 10.81)

  flag.ids = rep(1L, length(x))
  flag.ids[15] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(outlier_tscore(x), flags)


  mask = rep(TRUE, length(x))
  mask[15] = FALSE
  flag.ids = rep(1L, length(x))
  flag.ids[1] = 3L
  flag.ids[14:15] = 4L
  flags = ODWGtools:::.outlier_factor(flag.ids)
  expect_identical(outlier_tscore(x, mask), flags)

})
