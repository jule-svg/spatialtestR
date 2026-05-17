skip_if_not_installed("terra")

# create_test_raster() --------------------------------------------------------

test_that("create_test_raster() returns a SpatRaster", {
  r <- create_test_raster()
  expect_s4_class(r, "SpatRaster")
})

test_that("create_test_raster() default dimensions are 10x10x1", {
  r <- create_test_raster()
  expect_equal(terra::nrow(r), 10)
  expect_equal(terra::ncol(r), 10)
  expect_equal(terra::nlyr(r), 1)
})

test_that("create_test_raster() respects nrow, ncol, nlyr", {
  r <- create_test_raster(nrow = 5, ncol = 8, nlyr = 3)
  expect_equal(terra::nrow(r), 5)
  expect_equal(terra::ncol(r), 8)
  expect_equal(terra::nlyr(r), 3)
})

test_that("create_test_raster() sets CRS correctly", {
  r <- create_test_raster(crs = "EPSG:32632")
  expect_equal(terra::crs(r, describe = TRUE)$code, "32632")
})

test_that("create_test_raster() default values are sequential 1 to nrow*ncol", {
  r <- create_test_raster(nrow = 5, ncol = 4)
  vals <- as.numeric(terra::values(r))
  expect_equal(vals, 1:20)
})

test_that("create_test_raster() fills all cells with a single constant value", {
  r <- create_test_raster(values = 7)
  expect_true(all(terra::values(r) == 7, na.rm = TRUE))
})

test_that("create_test_raster() uses a supplied value vector", {
  v <- rep(c(1, 2), 50)
  r <- create_test_raster(values = v)
  expect_equal(as.numeric(terra::values(r)), v)
})

test_that("create_test_raster() na_fraction = 0 produces no NAs", {
  r <- create_test_raster(na_fraction = 0)
  expect_false(anyNA(terra::values(r)))
})

test_that("create_test_raster() na_fraction introduces approximately correct NA count", {
  r <- create_test_raster(nrow = 10, ncol = 10, na_fraction = 0.2, seed = 1)
  n_na <- sum(is.na(terra::values(r)))
  8
  expect_equal(n_na, 20)
})

test_that("create_test_raster() seed makes NA placement reproducible", {
  r1 <- create_test_raster(na_fraction = 0.3, seed = 42)
  r2 <- create_test_raster(na_fraction = 0.3, seed = 42)
  expect_equal(terra::values(r1), terra::values(r2))
})

test_that("create_test_raster() sets extent correctly", {
  r <- create_test_raster(xmin = 10, xmax = 20, ymin = 50, ymax = 60)
  expect_equal(terra::xmin(r), 10)
  expect_equal(terra::xmax(r), 20)
  expect_equal(terra::ymin(r), 50)
  expect_equal(terra::ymax(r), 60)
})

# --- input validation --------------------------------------------------------

test_that("create_test_raster() rejects nrow < 1", {
  expect_error(create_test_raster(nrow = 0), "`nrow`")
})

test_that("create_test_raster() rejects ncol < 1", {
  expect_error(create_test_raster(ncol = -1), "`ncol`")
})

test_that("create_test_raster() rejects nlyr < 1", {
  expect_error(create_test_raster(nlyr = 0), "`nlyr`")
})

test_that("create_test_raster() rejects na_fraction outside [0, 1]", {
  expect_error(create_test_raster(na_fraction = 1.5), "`na_fraction`")
  expect_error(create_test_raster(na_fraction = -0.1), "`na_fraction`")
})

test_that("create_test_raster() rejects xmin >= xmax", {
  expect_error(create_test_raster(xmin = 5, xmax = 5), "`xmin`")
  expect_error(create_test_raster(xmin = 6, xmax = 5), "`xmin`")
})

test_that("create_test_raster() rejects ymin >= ymax", {
  expect_error(create_test_raster(ymin = 2, ymax = 1), "`ymin`")
})


# create_test_multiband() -----------------------------------------------------

test_that("create_test_multiband() returns a SpatRaster", {
  ms <- create_test_multiband()
  expect_s4_class(ms, "SpatRaster")
})

test_that("create_test_multiband() default has 4 layers", {
  ms <- create_test_multiband()
  expect_equal(terra::nlyr(ms), 4)
})

test_that("create_test_multiband() default layer names are blue/green/red/nir", {
  ms <- create_test_multiband()
  expect_equal(names(ms), c("blue", "green", "red", "nir"))
})

test_that("create_test_multiband() respects custom band_names", {
  ms <- create_test_multiband(band_names = c("red", "nir"))
  expect_equal(terra::nlyr(ms), 2)
  expect_equal(names(ms), c("red", "nir"))
})

test_that("create_test_multiband() band_values set correct cell values", {
  ms <- create_test_multiband(
    band_names = c("red", "nir"),
    band_values = list(red = rep(0.1, 100), nir = rep(0.5, 100))
  )
  expect_equal(unique(as.numeric(terra::values(ms[["red"]]))), 0.1)
  expect_equal(unique(as.numeric(terra::values(ms[["nir"]]))), 0.5)
})

test_that("create_test_multiband() rejects non-character band_names", {
  expect_error(create_test_multiband(band_names = 1:3), "`band_names`")
})

test_that("create_test_multiband() rejects band_values missing a band entry", {
  expect_error(
    create_test_multiband(
      band_names = c("red", "nir"),
      band_values = list(red = rep(1, 100)) # nir missing
    ),
    "`band_values`"
  )
})

# --- na_pattern --------------------------------------------------------------

test_that("create_test_multiband() na_pattern = 'shared' produces identical NA positions across bands", {
  ms <- create_test_multiband(
    band_names  = c("red", "nir", "swir"),
    na_fraction = 0.2,
    na_pattern  = "shared",
    seed        = 42
  )
  na_red  <- is.na(as.numeric(terra::values(ms[["red"]])))
  na_nir  <- is.na(as.numeric(terra::values(ms[["nir"]])))
  na_swir <- is.na(as.numeric(terra::values(ms[["swir"]])))

  expect_equal(na_red, na_nir)
  expect_equal(na_red, na_swir)
})

test_that("create_test_multiband() na_pattern = 'independent' produces different NA positions across bands", {
  ms <- create_test_multiband(
    band_names  = c("red", "nir"),
    na_fraction = 0.3,
    na_pattern  = "independent",
    seed        = 42
  )
  na_red <- is.na(as.numeric(terra::values(ms[["red"]])))
  na_nir <- is.na(as.numeric(terra::values(ms[["nir"]])))

  expect_false(identical(na_red, na_nir))
})

test_that("create_test_multiband() default na_pattern is 'independent'", {
  ms_default     <- create_test_multiband(na_fraction = 0.3, seed = 42)
  ms_independent <- create_test_multiband(na_fraction = 0.3, seed = 42,
                                          na_pattern  = "independent")
  expect_equal(terra::values(ms_default), terra::values(ms_independent))
})

test_that("create_test_multiband() rejects invalid na_pattern values", {
  expect_error(
    create_test_multiband(na_pattern = "schmierig"),
    "should be one of"
  )
})

test_that("create_test_multiband() na_fraction = 0 produces no NAs in either pattern", {
  ms_shared      <- create_test_multiband(na_fraction = 0, na_pattern = "shared")
  ms_independent <- create_test_multiband(na_fraction = 0, na_pattern = "independent")
  expect_false(anyNA(terra::values(ms_shared)))
  expect_false(anyNA(terra::values(ms_independent)))
})

test_that("create_test_multiband() produces correct NA count per band in both patterns", {
  # shared: every band has exactly floor(0.2 * 100) = 20 NAs
  ms_shared <- create_test_multiband(
    nrow = 10, ncol = 10, na_fraction = 0.2,
    na_pattern = "shared", seed = 1
  )
  for (band in names(ms_shared)) {
    n_na <- sum(is.na(terra::values(ms_shared[[band]])))
    expect_equal(n_na, 20)
  }

  # independent: every band also has exactly 20 NAs (just at different positions)
  ms_independent <- create_test_multiband(
    nrow = 10, ncol = 10, na_fraction = 0.2,
    na_pattern = "independent", seed = 1
  )
  for (band in names(ms_independent)) {
    n_na <- sum(is.na(terra::values(ms_independent[[band]])))
    expect_equal(n_na, 20)
  }
})

test_that("create_test_multiband() na_pattern = 'shared' is reproducible with seed", {
  ms1 <- create_test_multiband(na_fraction = 0.3, na_pattern = "shared", seed = 7)
  ms2 <- create_test_multiband(na_fraction = 0.3, na_pattern = "shared", seed = 7)
  expect_equal(terra::values(ms1), terra::values(ms2))
})
