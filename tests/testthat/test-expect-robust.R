skip_if_not_installed("terra")

# expect_na_consistent() ------------------------------------------------------

test_that("expect_na_consistent() passes when both rasters have no NAs", {
  r1 <- create_test_raster()
  r2 <- create_test_raster(values = 99)
  expect_invisible(expect_na_consistent(r1, r2))
})

test_that("expect_na_consistent() passes when NA patterns are identical", {
  r1 <- create_test_raster(na_fraction = 0.2, seed = 42)
  r2 <- create_test_raster(na_fraction = 0.2, seed = 42)
  expect_invisible(expect_na_consistent(r1, r2))
})

test_that("expect_na_consistent() returns r2 invisibly on success", {
  r1 <- create_test_raster()
  r2 <- create_test_raster(values = 7)
  out <- expect_na_consistent(r1, r2)
  expect_identical(out, r2)
})

test_that("expect_na_consistent() fails when r1 has extra NAs", {
  r1 <- create_test_raster(na_fraction = 0.1, seed = 1)
  r2 <- create_test_raster()
  expect_error(expect_na_consistent(r1, r2), "NA pattern")
})

test_that("expect_na_consistent() fails when r2 has extra NAs", {
  r1 <- create_test_raster()
  r2 <- create_test_raster(na_fraction = 0.1, seed = 1)
  expect_error(expect_na_consistent(r1, r2), "NA pattern")
})

test_that("expect_na_consistent() error reports counts in both directions", {
  r1 <- create_test_raster(na_fraction = 0.1, seed = 1)
  r2 <- create_test_raster()
  err <- tryCatch(
    expect_na_consistent(r1, r2),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "r1")
  expect_match(err, "r2")
  expect_match(err, "cell")
})

# snapshot: failure message format (must match _snaps/expect-robust.md)
test_that("expect_na_consistent() failure message contains cell count", {
  r1 <- create_test_raster(nrow = 10, ncol = 10, na_fraction = 0.1, seed = 1)
  r2 <- create_test_raster(nrow = 10, ncol = 10)
  expect_snapshot(expect_na_consistent(r1, r2), error = TRUE)
})


# expect_no_inf() -------------------------------------------------------------

test_that("expect_no_inf() passes when no Inf values present", {
  r <- create_test_raster()
  expect_invisible(expect_no_inf(r))
})

test_that("expect_no_inf() passes on raster with NAs but no Inf", {
  r <- create_test_raster(na_fraction = 0.1, seed = 1)
  expect_invisible(expect_no_inf(r))
})

test_that("expect_no_inf() returns r invisibly on success", {
  r <- create_test_raster(values = 1)
  out <- expect_no_inf(r)
  expect_identical(out, r)
})

test_that("expect_no_inf() fails when Inf values are present", {
  r <- create_test_raster()
  v <- terra::values(r)
  v[1] <- Inf
  terra::values(r) <- v
  expect_error(expect_no_inf(r), "Inf")
})

test_that("expect_no_inf() fails when -Inf values are present", {
  r <- create_test_raster()
  v <- terra::values(r)
  v[1] <- -Inf
  terra::values(r) <- v
  expect_error(expect_no_inf(r), "Inf")
})

test_that("expect_no_inf() error reports counts of Inf and -Inf separately", {
  r <- create_test_raster()
  v <- terra::values(r)
  v[1:3] <- Inf
  v[4]   <- -Inf
  terra::values(r) <- v
  err <- tryCatch(
    expect_no_inf(r),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "3")
  expect_match(err, "1")
})

# snapshot: failure message format (must match _snaps/expect-robust.md)
test_that("expect_no_inf() failure message contains Inf counts", {
  r <- create_test_raster()
  v <- terra::values(r)
  v[1:3] <- Inf
  v[4]   <- -Inf
  terra::values(r) <- v
  expect_snapshot(expect_no_inf(r), error = TRUE)
})
