test_that("chi-squared outlier detection works", {

  set.seed(42)
  x = c(1.59, 1.65, 1.71, 1.71, 1.72, 1.73, 1.73,
    1.77, 1.8, 1.84, 1.89, 1.94, 1.95, 2.1, 10.81)
  y = sample(x, length(x))

#  flag.ids = rep(1L, length(x))
#  flag.ids[15] = 4L
#  flags = ODWGtools:::.outlier_factor(flag.ids)
#  expect_identical(moutlier_chisq(list(x, y)), flags)
#
#
#  mask = rep(TRUE, length(x))
#  mask[15] = FALSE
#  flag.ids = rep(1L, length(x))
#  flag.ids[1] = 3L
#  flag.ids[14:15] = 4L
#  flags = ODWGtools:::.outlier_factor(flag.ids)
#  expect_identical(outlier_tscore(x, mask), flags)

})

