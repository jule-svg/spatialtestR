skip_if_not_installed("terra")

# expect_raster_dims() --------------------------------------------------------

test_that("expect_raster_dims() passes when all dims match", {
  r <- create_test_raster(nrow = 5, ncol = 8, nlyr = 2)
  expect_invisible(expect_raster_dims(r, nrow = 5, ncol = 8, nlyr = 2))
})

test_that("expect_raster_dims() passes when only nrow is checked", {
  r <- create_test_raster(nrow = 7)
  expect_invisible(expect_raster_dims(r, nrow = 7))
})

test_that("expect_raster_dims() passes when only ncol is checked", {
  r <- create_test_raster(ncol = 12)
  expect_invisible(expect_raster_dims(r, ncol = 12))
})

test_that("expect_raster_dims() passes with no args (no check performed)", {
  r <- create_test_raster()
  expect_invisible(expect_raster_dims(r))
})

test_that("expect_raster_dims() returns r invisibly on success", {
  r <- create_test_raster(nrow = 3, ncol = 3)
  out <- expect_raster_dims(r, nrow = 3)
  expect_identical(out, r)
})

test_that("expect_raster_dims() fails when nrow is wrong", {
  r <- create_test_raster(nrow = 5)
  expect_error(expect_raster_dims(r, nrow = 10), "nrow")
})

test_that("expect_raster_dims() fails when ncol is wrong", {
  r <- create_test_raster(ncol = 5)
  expect_error(expect_raster_dims(r, ncol = 99), "ncol")
})

test_that("expect_raster_dims() fails when nlyr is wrong", {
  r <- create_test_raster(nlyr = 1)
  expect_error(expect_raster_dims(r, nlyr = 4), "nlyr")
})

test_that("expect_raster_dims() reports all mismatches in one error", {
  r <- create_test_raster(nrow = 5, ncol = 5, nlyr = 1)
  err <- tryCatch(
    expect_raster_dims(r, nrow = 10, ncol = 20, nlyr = 3),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "nrow")
  expect_match(err, "ncol")
  expect_match(err, "nlyr")
})

# snapshot: failure message format
test_that("expect_raster_dims() failure message contains dimension info", {
  r <- create_test_raster(nrow = 5)
  expect_snapshot(expect_raster_dims(r, nrow = 10), error = TRUE)
})


# expect_raster_crs() ---------------------------------------------------------

test_that("expect_raster_crs() passes when CRS matches", {
  r <- create_test_raster(crs = "EPSG:32632")
  expect_invisible(expect_raster_crs(r, "EPSG:32632"))
})

test_that("expect_raster_crs() is case-insensitive on the EPSG prefix", {
  r <- create_test_raster(crs = "EPSG:4326")
  expect_invisible(expect_raster_crs(r, "epsg:4326"))
})

test_that("expect_raster_crs() returns r invisibly on success", {
  r <- create_test_raster(crs = "EPSG:4326")
  out <- expect_raster_crs(r, "EPSG:4326")
  expect_identical(out, r)
})

test_that("expect_raster_crs() fails when CRS does not match", {
  r <- create_test_raster(crs = "EPSG:4326")
  expect_error(expect_raster_crs(r, "EPSG:32632"), "CRS")
})

test_that("expect_raster_crs() error message includes expected and actual CRS", {
  r <- create_test_raster(crs = "EPSG:4326")
  err <- tryCatch(
    expect_raster_crs(r, "EPSG:32632"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "32632")
  expect_match(err, "4326")
})

# snapshot: failure message format
test_that("expect_raster_crs() failure message contains both CRS codes", {
  r <- create_test_raster(crs = "EPSG:4326")
  expect_snapshot(expect_raster_crs(r, "EPSG:32632"), error = TRUE)
})

test_that("expect_raster_crs() accepts a WKT string as expected CRS", {
  r   <- create_test_raster(crs = "EPSG:32632")
  wkt <- terra::crs(r)
  expect_invisible(expect_raster_crs(r, wkt))
})

test_that("expect_raster_crs() matches WKT against an EPSG-derived raster", {
  r   <- create_test_raster(crs = "EPSG:4326")
  wkt <- terra::crs(r)                       # WKT version of EPSG:4326
  expect_invisible(expect_raster_crs(r, wkt))
})


# expect_same_dims() ----------------------------------------------------------

test_that("expect_same_dims() passes when dimensions match", {
  r1 <- create_test_raster(nrow = 5, ncol = 8, nlyr = 2)
  r2 <- create_test_raster(nrow = 5, ncol = 8, nlyr = 2)
  expect_invisible(expect_same_dims(r1, r2))
})

test_that("expect_same_dims() returns r_out invisibly on success", {
  r1  <- create_test_raster()
  r2  <- create_test_raster()
  out <- expect_same_dims(r1, r2)
  expect_identical(out, r2)
})

test_that("expect_same_dims() fails when nrow differs", {
  r1 <- create_test_raster(nrow = 5)
  r2 <- create_test_raster(nrow = 10)
  expect_error(expect_same_dims(r1, r2), "nrow")
})

test_that("expect_same_dims() fails when ncol differs", {
  r1 <- create_test_raster(ncol = 5)
  r2 <- create_test_raster(ncol = 7)
  expect_error(expect_same_dims(r1, r2), "ncol")
})

test_that("expect_same_dims() fails when nlyr differs", {
  r1 <- create_test_raster(nlyr = 1)
  r2 <- create_test_raster(nlyr = 3)
  expect_error(expect_same_dims(r1, r2), "nlyr")
})

test_that("expect_same_dims() reports all mismatches in one error", {
  r1 <- create_test_raster(nrow = 5,  ncol = 5,  nlyr = 1)
  r2 <- create_test_raster(nrow = 10, ncol = 20, nlyr = 3)
  err <- tryCatch(
    expect_same_dims(r1, r2),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "nrow")
  expect_match(err, "ncol")
  expect_match(err, "nlyr")
})

test_that("expect_same_dims() error mentions input vs output", {
  r1 <- create_test_raster(nrow = 5)
  r2 <- create_test_raster(nrow = 10)
  err <- tryCatch(
    expect_same_dims(r1, r2),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "input")
  expect_match(err, "output")
})


# expect_same_crs() -----------------------------------------------------------

test_that("expect_same_crs() passes when both rasters share an EPSG code", {
  r1 <- create_test_raster(crs = "EPSG:32632")
  r2 <- create_test_raster(crs = "EPSG:32632")
  expect_invisible(expect_same_crs(r1, r2))
})

test_that("expect_same_crs() returns r_out invisibly on success", {
  r1  <- create_test_raster(crs = "EPSG:4326")
  r2  <- create_test_raster(crs = "EPSG:4326")
  out <- expect_same_crs(r1, r2)
  expect_identical(out, r2)
})

test_that("expect_same_crs() fails when CRSes differ", {
  r1 <- create_test_raster(crs = "EPSG:4326")
  r2 <- create_test_raster(crs = "EPSG:32632")
  expect_error(expect_same_crs(r1, r2), "CRS")
})

test_that("expect_same_crs() error mentions input/output and both CRS codes", {
  r1 <- create_test_raster(crs = "EPSG:4326")
  r2 <- create_test_raster(crs = "EPSG:32632")
  err <- tryCatch(
    expect_same_crs(r1, r2),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "input")
  expect_match(err, "output")
  expect_match(err, "4326")
  expect_match(err, "32632")
})
