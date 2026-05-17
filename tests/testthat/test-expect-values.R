skip_if_not_installed("terra")

# expect_raster_values_between() ----------------------------------------------

test_that("expect_raster_values_between() passes when all values in range", {
  r <- create_test_raster(values = seq(0, 1, length.out = 100))
  expect_invisible(expect_raster_values_between(r, 0, 1))
})

test_that("expect_raster_values_between() passes on boundary values", {
  r <- create_test_raster(values = c(0, 0.5, 1))
  expect_invisible(expect_raster_values_between(r, 0, 1))
})

test_that("expect_raster_values_between() ignores NAs", {
  r <- create_test_raster(values = c(0.2, 0.5, NA, 0.8))
  expect_invisible(expect_raster_values_between(r, 0, 1))
})

test_that("expect_raster_values_between() passes on all-NA raster", {
  r <- create_test_raster(na_fraction = 1, seed = 1)
  expect_invisible(expect_raster_values_between(r, 0, 1))
})

test_that("expect_raster_values_between() returns r2 invisibly on success", {
  r <- create_test_raster(values = 0.5)
  out <- expect_raster_values_between(r, 0, 1)
  expect_identical(out, r)
})

test_that("expect_raster_values_between() fails when min is below lower", {
  r <- create_test_raster(values = c(-0.1, 0.5, 1))
  expect_error(expect_raster_values_between(r, 0, 1), "outside")
})

test_that("expect_raster_values_between() fails when max is above upper", {
  r <- create_test_raster(values = c(0, 0.5, 1.1))
  expect_error(expect_raster_values_between(r, 0, 1), "outside")
})

test_that("expect_raster_values_between() error includes bounds and observed range", {
  r <- create_test_raster(values = c(-2, 0, 3))
  err <- tryCatch(
    expect_raster_values_between(r, 0, 1),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "\\[0, 1\\]")
  expect_match(err, "-2")
  expect_match(err, "3")
})

# snapshot: failure message format
test_that("expect_raster_values_between() failure message reports observed range", {
  r <- create_test_raster(values = c(-2, 0, 3))
  expect_snapshot(expect_raster_values_between(r, 0, 1), error = TRUE)
})


# expect_mean_preserved() -----------------------------------------------------

test_that("expect_mean_preserved() passes when means are equal", {
  r <- create_test_raster(values = 5)
  expect_invisible(expect_mean_preserved(r, r))
})

test_that("expect_mean_preserved() passes within tolerance", {
  r1 <- create_test_raster(values = 10)
  r2 <- create_test_raster(values = 10 + 1e-8)
  expect_invisible(expect_mean_preserved(r1, r2, tolerance = 1e-6))
})

test_that("expect_mean_preserved() returns r2 invisibly on success", {
  r1 <- create_test_raster(values = 3)
  r2 <- create_test_raster(values = 3)
  out <- expect_mean_preserved(r1, r2)
  expect_identical(out, r2)
})

test_that("expect_mean_preserved() fails when difference exceeds tolerance", {
  r1 <- create_test_raster(values = 10)
  r2 <- create_test_raster(values = 12)
  expect_error(expect_mean_preserved(r1, r2, tolerance = 0.1), "Mean")
})

test_that("expect_mean_preserved() error contains both means and difference", {
  r1 <- create_test_raster(values = 10)
  r2 <- create_test_raster(values = 20)
  err <- tryCatch(
    expect_mean_preserved(r1, r2),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "10")
  expect_match(err, "20")
})

# snapshot: failure message format
test_that("expect_mean_preserved() failure message reports mean change", {
  r1 <- create_test_raster(values = 10)
  r2 <- create_test_raster(values = 20)
  expect_snapshot(expect_mean_preserved(r1, r2, tolerance = 0.01), error = TRUE)
})
