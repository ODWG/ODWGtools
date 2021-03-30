test_that("gap test works", {

  x = c(seq(as.POSIXct("2018-01-01"), by = "15 mins", length.out = 20))
  flag.ids = c(NA_integer_, rep(1L, 19))
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_gap(x, "16 mins", "less than"), flags)
  expect_identical(rtqc_gap(x, "15 mins", "is"), flags)

  x = c(seq(as.POSIXct("2018-01-01"), by = "15 mins", length.out = 20))
  x = x[-15]
  flag.ids = c(NA_integer_, rep(1L, 18))
  flag.ids[15] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_gap(x, "16 mins", "less than"), flags)
  expect_identical(rtqc_gap(x, "15 mins", "is"), flags)

  flag.ids = c(NA_integer_, rep(1L, 18))
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_gap(x, "31 mins", "less than"), flags)
  expect_identical(rtqc_gap(x, "5 mins", "greater than"), flags)

  flag.ids = c(NA_integer_, rep(4L, 18))
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_gap(x, "31 mins", "greater than"), flags)

})

test_that("range test works", {

  x = 5 * sin(seq(1, 10, by = 0.2))
  flag.ids = rep(1L, length(x))
  flag.ids[abs(x) > 3] = 3L
  flag.ids[abs(x) > 4] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_range(x, c(-3, 3), c(-4, 4)), flags)

  flag.ids = rep(1L, length(x))
  flag.ids[x > 5] = 3L
  flag.ids[x < 2] = 3L
  flag.ids[x > 7] = 4L
  flag.ids[x < 0] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_range(x, c(2, 5), c(0, 7)), flags)

})


test_that("spike test works", {

  x = rep(0, 20)
  spike.locs = c(3, 7, 15)
  spike.mags = c(10, 20, 30)
  x[spike.locs] = spike.mags

  base.vals = rowMeans(cbind(c(NA_real_, head(x, -1)),
    c(tail(x, -1), NA_real_)))
  diff.vals = abs(x - base.vals)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[diff.vals > 9.5] = 3L
  flag.ids[x > 19.5] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike(x, c(9.5, 19.5)), flags)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[diff.vals > 4.5] = 3L
  flag.ids[x > 19.5] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike(x, c(4.5, 19.5)), flags)

})


test_that("alternate spike test works", {

  x = rep(0, 20)
  spike.locs = c(3, 7, 15)
  spike.mags = c(10, 20, 30)
  x[spike.locs] = spike.mags

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[x > 10] = 3L
  flag.ids[x > 20] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike_alt(x, c(10, 20)), flags)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[x > 15] = 3L
  flag.ids[x > 25] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike_alt(x, c(15, 25)), flags)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[x > 5] = 3L
  flag.ids[x > 25] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike_alt(x, c(5, 25)), flags)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[x > 25] = 3L
  flag.ids[x > 40] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike_alt(x, c(25, 40)), flags)

  flag.ids = c(NA_integer_, rep(1L, 18), NA_integer_)
  flag.ids[x > 40] = 3L
  flag.ids[x > 50] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_spike_alt(x, c(40, 50)), flags)
})


test_that("rate test works", {

  x = c(seq(0, 3, by = 1), seq(4, 8, by = 2), seq(10, 30, by = 5),
    seq(40, 60, by = 10))

  flag.ids = c(NA_integer_, rep(1L, length(x) - 1))
  flag.ids[which(diff(x) > 1) + 1] = 3L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_equal(rtqc_rate_alt(x, 1.5, 1), flags)



})


test_that("alternate rate test works", {

  x = c(seq(0, 3, by = 1), seq(4, 8, by = 2), seq(10, 30, by = 5),
    seq(40, 60, by = 10))

  flag.ids = c(NA_integer_, rep(1L, length(x) - 1))
  flag.ids[which(diff(x) > 1) + 1] = 3L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_equal(rtqc_rate_alt(x, 1.5, 1), flags)

  flag.ids = c(NA_integer_, rep(1L, length(x) - 1))
  flag.ids[which(diff(x) > 2L) + 1] = 3L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_equal(rtqc_rate_alt(x, 2.1, 1), flags)

  flag.ids = c(rep(NA_integer_, 2), rep(1L, length(x) - 2))
  diffs = slider::slide_dbl(x, ~ mean(diff(.x)), .before = 2,
    .complete = TRUE)
  flag.ids[diffs > 1.4] = 3L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_equal(rtqc_rate_alt(x, 1.4, 2), flags)

  flag.ids = c(rep(NA_integer_, 4), rep(1L, length(x) - 4))
  diffs = slider::slide_dbl(x, ~ mean(diff(.x)), .before = 4,
    .complete = TRUE)
  flag.ids[diffs > 3] = 3L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_equal(rtqc_rate_alt(x, 3, 4), flags)

})


test_that("attenuation test works", {

  x = c(rep(c(10, 20), 2), rep(c(10, 5), 2), rep(5, 2))
  flag.ids = c(NA_integer_, rep(1L, length(x) - 1))
  flag.ids[6:8] = 3L
  flag.ids[9:10] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)

  expect_identical(rtqc_attenuation(x, c(7, 3), 2), flags)

  x = c(rep(c(10, 20), 4), rep(c(10, 5), 4), rep(5, 4))
  flag.ids = rep(1L, length(x))
  flag.ids[1:2] = NA_integer_
  flag.ids[3:9] = 3L
  flag.ids[11:20] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_attenuation(x, c(7, 3), 3), flags)

  x = c(rep(c(10, 20), 4), rep(c(10, 5), 4), rep(5, 4))
  flag.ids = rep(1L, length(x))
  flag.ids[1:3] = NA_integer_
  flag.ids[4:9] = 3L
  flag.ids[12:20] = 4L
  flags = ODWGtools:::.rtqc_factor(flag.ids)
  expect_identical(rtqc_attenuation(x, c(6, 3), 4), flags)

})
