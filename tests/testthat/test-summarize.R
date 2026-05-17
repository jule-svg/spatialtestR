skip_if_not_installed("terra")

# validate_resampling() -------------------------------------------------------

test_that("validate_resampling() accepts NULL (auto-detect)", {
  out <- validate_resampling(NULL)
  expect_null(out$kind)
  expect_equal(out$display, "auto-detect")
})

test_that("validate_resampling() accepts TRUE / FALSE", {
  out_t <- validate_resampling(TRUE)
  out_f <- validate_resampling(FALSE)
  expect_true(out_t$kind)
  expect_false(out_f$kind)
})

test_that("validate_resampling() accepts canonical method names", {
  for (m in c("nearest_neighbor", "bilinear", "cubic", "lanczos", "mode", "average")) {
    out <- validate_resampling(m)
    expect_equal(out$kind, m)
    expect_true(is.numeric(out$expected_r2))
    expect_length(out$expected_r2, 2)
  }
})

test_that("validate_resampling() accepts aliases", {
  expect_equal(validate_resampling("nearest")$kind,   "nearest_neighbor")
  expect_equal(validate_resampling("ngb")$kind,       "nearest_neighbor")
  expect_equal(validate_resampling("nn")$kind,        "nearest_neighbor")
  expect_equal(validate_resampling("linear")$kind,    "bilinear")
  expect_equal(validate_resampling("bicubic")$kind,   "cubic")
  expect_equal(validate_resampling("majority")$kind,  "mode")
  expect_equal(validate_resampling("mean")$kind,      "average")
})

test_that("validate_resampling() is case-insensitive", {
  expect_equal(validate_resampling("Bilinear")$kind, "bilinear")
  expect_equal(validate_resampling("NEAREST")$kind,  "nearest_neighbor")
})

test_that("validate_resampling() rejects unknown method strings", {
  expect_error(validate_resampling("schmierig"), "Unknown resampling method")
})

test_that("validate_resampling() rejects invalid types", {
  expect_error(validate_resampling(42),    "Invalid value")
  expect_error(validate_resampling(list()), "Invalid value")
  expect_error(validate_resampling(NA),     "Invalid value")
})


# get_paired_values() — geometry match (pixel-wise 1:1) -----------------------

test_that("get_paired_values() returns pixel-wise pairs when geometry matches", {
  r1 <- create_test_raster(nrow = 10, ncol = 10, values = 1:100)
  r2 <- create_test_raster(nrow = 10, ncol = 10, values = (1:100) * 2)
  out <- get_paired_values(r1, r2)

  expect_equal(out$method, "pixel")
  expect_true(out$geometry_match)
  expect_equal(out$n_used, 100)
  expect_equal(out$v_in,  as.numeric(1:100))
  expect_equal(out$v_out, as.numeric((1:100) * 2))
})

test_that("get_paired_values() works with resampling = FALSE on matching geometry", {
  r1 <- create_test_raster()
  r2 <- create_test_raster(values = 99)
  out <- get_paired_values(r1, r2, resampling = FALSE)
  expect_equal(out$method, "pixel")
})

test_that("get_paired_values() errors when resampling = FALSE but geometry differs", {
  r1 <- create_test_raster(nrow = 10, ncol = 10)
  r2 <- create_test_raster(nrow = 5,  ncol = 5)
  expect_error(
    get_paired_values(r1, r2, resampling = FALSE),
    "resampling = FALSE"
  )
})

test_that("get_paired_values() warns when resampling = TRUE but geometry matches", {
  r1 <- create_test_raster()
  r2 <- create_test_raster()
  expect_warning(
    get_paired_values(r1, r2, resampling = TRUE),
    "no resampling appears to have happened"
  )
})

test_that("get_paired_values() warns when resampling = 'bilinear' but geometry matches", {
  r1 <- create_test_raster()
  r2 <- create_test_raster()
  expect_warning(
    get_paired_values(r1, r2, resampling = "bilinear"),
    "no resampling appears to have happened"
  )
})


# get_paired_values() — geometry differs (geographic sampling) ----------------

test_that("get_paired_values() falls back to geographic sampling when dims differ", {
  r1 <- create_test_raster(nrow = 50, ncol = 50, values = 1:2500)
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")   # 25x25
  out <- get_paired_values(r1, r2, resampling = TRUE, n_sample = 500, seed = 1)

  expect_equal(out$method, "sampled")
  expect_false(out$geometry_match)
  expect_equal(out$n_used, 500)
  expect_length(out$v_in,  500)
  expect_length(out$v_out, 500)
})

test_that("get_paired_values() uses sampling automatically with resampling = NULL", {
  r1 <- create_test_raster(nrow = 20, ncol = 20)
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")
  out <- get_paired_values(r1, r2, n_sample = 200, seed = 1)   # default NULL
  expect_equal(out$method, "sampled")
})

test_that("get_paired_values() is reproducible with seed", {
  r1 <- create_test_raster(nrow = 20, ncol = 20)
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")
  o1 <- get_paired_values(r1, r2, resampling = "average", n_sample = 100, seed = 42)
  o2 <- get_paired_values(r1, r2, resampling = "average", n_sample = 100, seed = 42)
  expect_equal(o1$v_in,  o2$v_in)
  expect_equal(o1$v_out, o2$v_out)
})

test_that("get_paired_values() respects n_sample", {
  r1 <- create_test_raster(nrow = 50, ncol = 50)
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")
  for (n in c(100, 500, 2000)) {
    out <- get_paired_values(r1, r2, resampling = TRUE, n_sample = n, seed = 1)
    expect_equal(out$n_used, n)
  }
})

test_that("get_paired_values() caps n_sample at ncell(r_in) when too large", {
  r1 <- create_test_raster(nrow = 10, ncol = 10)         # 100 cells
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")     # 5x5
  out <- get_paired_values(r1, r2, resampling = TRUE, n_sample = 10000, seed = 1)
  expect_equal(out$n_used, 100)   # capped at ncell(r_in)
})

test_that("get_paired_values() passes the validated resampling info through", {
  r1 <- create_test_raster(nrow = 20, ncol = 20)
  r2 <- terra::aggregate(r1, fact = 2, fun = "mean")
  out <- get_paired_values(r1, r2, resampling = "bilinear", n_sample = 100, seed = 1)
  expect_equal(out$resampling$kind,    "bilinear")
  expect_equal(out$resampling$display, "bilinear")
  expect_true(is.numeric(out$resampling$expected_r2))
})


# describe_geom_diff() --------------------------------------------------------

test_that("describe_geom_diff() reports dimension differences", {
  r1 <- create_test_raster(nrow = 10, ncol = 10)
  r2 <- create_test_raster(nrow = 5,  ncol = 5)
  txt <- describe_geom_diff(r1, r2)
  expect_match(txt, "dimensions")
})

test_that("describe_geom_diff() reports CRS differences", {
  r1 <- create_test_raster(crs = "EPSG:4326")
  r2 <- create_test_raster(crs = "EPSG:32632")
  txt <- describe_geom_diff(r1, r2)
  expect_match(txt, "CRS")
})

test_that("describe_geom_diff() reports identical rasters as no differences", {
  r1 <- create_test_raster()
  r2 <- create_test_raster()
  txt <- describe_geom_diff(r1, r2)
  expect_match(txt, "no visible differences")
})


# diag_structure() ------------------------------------------------------------

test_that("diag_structure() flags everything equal for identical rasters", {
  r1 <- create_test_raster(crs = "EPSG:32632")
  r2 <- create_test_raster(crs = "EPSG:32632")
  s  <- diag_structure(r1, r2)
  expect_true(s$dims$equal)
  expect_true(s$crs$equal)
  expect_true(s$extent$equal)
  expect_true(s$resolution$equal)
  expect_true(s$origin$equal)
  expect_true(s$layers$equal)
})

test_that("diag_structure() detects different dimensions", {
  r1 <- create_test_raster(nrow = 10, ncol = 10)
  r2 <- create_test_raster(nrow = 5,  ncol = 5)
  s  <- diag_structure(r1, r2)
  expect_false(s$dims$equal)
  expect_equal(s$dims$in_,  c(10, 10, 1))
  expect_equal(s$dims$out_, c(5, 5, 1))
})

test_that("diag_structure() detects different CRS", {
  r1 <- create_test_raster(crs = "EPSG:4326")
  r2 <- create_test_raster(crs = "EPSG:32632")
  s  <- diag_structure(r1, r2)
  expect_false(s$crs$equal)
})

test_that("diag_structure() detects different extent", {
  r1 <- create_test_raster(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  r2 <- create_test_raster(xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  s  <- diag_structure(r1, r2)
  expect_false(s$extent$equal)
  expect_false(s$resolution$equal)   # different extent on same nrow/ncol → different res
})

test_that("diag_structure() reports layer counts", {
  r1 <- create_test_raster(nlyr = 1)
  r2 <- create_test_raster(nlyr = 3)
  s  <- diag_structure(r1, r2)
  expect_false(s$layers$equal)
  expect_equal(s$layers$in_,  1)
  expect_equal(s$layers$out_, 3)
})


# diag_values() ---------------------------------------------------------------

test_that("diag_values() computes basic range, mean, sd", {
  v_in  <- 1:100
  v_out <- (1:100) * 2
  d <- diag_values(v_in, v_out)
  expect_equal(d$range_in,  c(1, 100))
  expect_equal(d$range_out, c(2, 200))
  expect_equal(d$mean_in,   mean(v_in))
  expect_equal(d$mean_out,  mean(v_out))
  expect_equal(d$sd_in,     stats::sd(v_in))
})

test_that("diag_values() counts NAs in both directions", {
  v_in  <- c(1, NA, 3, 4, NA)
  v_out <- c(1, 2, NA, 4, NA)
  d <- diag_values(v_in, v_out)
  expect_equal(d$n_na_in,   2)
  expect_equal(d$n_na_out,  2)
  expect_equal(d$n_na_added, 1)   # input has 3, output has NA at idx 3
  expect_equal(d$n_na_lost,  1)   # input has NA, output has 2 at idx 2
})

test_that("diag_values() counts Inf and -Inf separately", {
  v_in  <- c(1, 2, 3, 4)
  v_out <- c(Inf, -Inf, Inf, 4)
  d <- diag_values(v_in, v_out)
  expect_equal(d$n_inf_pos, 2)
  expect_equal(d$n_inf_neg, 1)
})

test_that("diag_values() counts NaN", {
  v_in  <- c(1, 2, 3)
  v_out <- c(1, NaN, 3)
  d <- diag_values(v_in, v_out)
  expect_equal(d$n_nan, 1)
})

test_that("diag_values() errors if v_in and v_out have different length", {
  expect_error(diag_values(1:10, 1:5), "same length")
})


# diag_linear_fit() -----------------------------------------------------------

test_that("diag_linear_fit() classifies identity (out = in)", {
  v_in  <- 1:100
  v_out <- 1:100
  f <- diag_linear_fit(v_in, v_out)
  expect_equal(f$slope, 1, tolerance = 1e-8)
  expect_equal(f$intercept, 0, tolerance = 1e-8)
  expect_equal(f$r_squared, 1, tolerance = 1e-8)
  expect_equal(f$classification, "identity")
})

test_that("diag_linear_fit() classifies pure scaling (out = 2 * in)", {
  v_in  <- 1:100
  v_out <- 2 * v_in
  f <- diag_linear_fit(v_in, v_out)
  expect_equal(f$slope, 2, tolerance = 1e-8)
  expect_equal(f$intercept, 0, tolerance = 1e-8)
  expect_equal(f$classification, "scaling")
})

test_that("diag_linear_fit() classifies pure shift (out = in + 5)", {
  v_in  <- 1:100
  v_out <- v_in + 5
  f <- diag_linear_fit(v_in, v_out)
  expect_equal(f$slope, 1, tolerance = 1e-8)
  expect_equal(f$intercept, 5, tolerance = 1e-8)
  expect_equal(f$classification, "shift")
})

test_that("diag_linear_fit() classifies general affine (out = 2 * in + 5)", {
  v_in  <- 1:100
  v_out <- 2 * v_in + 5
  f <- diag_linear_fit(v_in, v_out)
  expect_equal(f$slope, 2, tolerance = 1e-8)
  expect_equal(f$intercept, 5, tolerance = 1e-8)
  expect_equal(f$classification, "linear")
})

test_that("diag_linear_fit() classifies non-linear (out = log(in))", {
  v_in  <- 1:100
  v_out <- log(v_in)
  f <- diag_linear_fit(v_in, v_out)
  expect_lt(f$r_squared, 0.999)
  expect_equal(f$classification, "non_linear")
})

test_that("diag_linear_fit() handles NAs and Infs gracefully", {
  v_in  <- c(1, 2, NA, 4, 5)
  v_out <- c(2, 4, 6, Inf, 10)
  f <- diag_linear_fit(v_in, v_out)
  # 3 valid pairs: (1,2), (2,4), (5,10) → slope = 2, intercept = 0
  expect_equal(f$n_valid_pairs, 3)
  expect_equal(f$slope, 2, tolerance = 1e-8)
})

test_that("diag_linear_fit() returns NA when input is constant", {
  v_in  <- rep(5, 100)
  v_out <- (1:100) * 2
  f <- diag_linear_fit(v_in, v_out)
  expect_true(is.na(f$slope))
  expect_true(is.na(f$r_squared))
  expect_match(f$note, "constant")
})

test_that("diag_linear_fit() returns NA with too few valid pairs", {
  v_in  <- c(NA, NA, NA)
  v_out <- c(1, 2, 3)
  f <- diag_linear_fit(v_in, v_out)
  expect_true(is.na(f$slope))
  expect_match(f$note, "fewer than 2")
})


# classify_fit() — internal helper --------------------------------------------

test_that("classify_fit() recognises identity within tolerance", {
  expect_equal(classify_fit(1.0,       0.0,       1.0, 1e-6, 0.999), "identity")
  expect_equal(classify_fit(1 + 1e-9,  -1e-9,     1.0, 1e-6, 0.999), "identity")
})

test_that("classify_fit() falls back to non_linear below threshold", {
  expect_equal(classify_fit(1.0, 0.0, 0.5, 1e-6, 0.999), "non_linear")
})

test_that("classify_fit() distinguishes scaling and shift", {
  expect_equal(classify_fit(2.0, 0.0, 1.0, 1e-6, 0.999), "scaling")
  expect_equal(classify_fit(1.0, 5.0, 1.0, 1e-6, 0.999), "shift")
  expect_equal(classify_fit(2.0, 5.0, 1.0, 1e-6, 0.999), "linear")
})


# summarize_transformation() — integration tests ------------------------------

test_that("summarize_transformation() returns an object of correct class", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in * 2
  s <- summarize_transformation(r_in, r_out)
  expect_s3_class(s, "transformation_summary")
  expect_named(s, c("structure", "values", "fit", "paired", "call"))
})

test_that("summarize_transformation() detects identity transformation", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$fit$classification, "identity")
  expect_equal(s$paired$method, "pixel")
})

test_that("summarize_transformation() detects scaling transformation", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in * 3
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$fit$classification, "scaling")
  expect_equal(s$fit$slope, 3, tolerance = 1e-8)
})

test_that("summarize_transformation() detects shift transformation", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in + 7
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$fit$classification, "shift")
  expect_equal(s$fit$intercept, 7, tolerance = 1e-8)
})

test_that("summarize_transformation() detects general affine transformation", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in * 2 + 5
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$fit$classification, "linear")
})

test_that("summarize_transformation() detects non-linear transformation", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- log(r_in)
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$fit$classification, "non_linear")
})

test_that("summarize_transformation() switches to sampled method when resampled", {
  r_in  <- create_test_raster(nrow = 50, ncol = 50)
  r_out <- terra::aggregate(r_in, fact = 2, fun = "mean")
  s <- summarize_transformation(r_in, r_out,
                                resampling = "average",
                                n_sample = 500, seed = 1)
  expect_equal(s$paired$method, "sampled")
  expect_false(s$paired$geometry_match)
  expect_equal(s$paired$resampling$kind, "average")
})

test_that("summarize_transformation() reports NA-pattern changes", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- create_test_raster(values = 1:100, na_fraction = 0.2, seed = 1)
  s <- summarize_transformation(r_in, r_out)
  expect_equal(s$values$n_na_in,    0)
  expect_equal(s$values$n_na_out,   20)
  expect_equal(s$values$n_na_added, 20)
  expect_equal(s$values$n_na_lost,  0)
})


# print.transformation_summary() ----------------------------------------------

test_that("print.transformation_summary() produces all main sections", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in * 2 + 5
  s <- summarize_transformation(r_in, r_out)
  out <- capture.output(print(s))
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "Raster transformation summary")
  expect_match(txt, "Geometry")
  expect_match(txt, "Sample")
  expect_match(txt, "Values")
  expect_match(txt, "Linear fit")
  expect_match(txt, "LINEAR")            # classification label appears
})

test_that("print.transformation_summary() shows resampling hint when method specified", {
  r_in  <- create_test_raster(nrow = 50, ncol = 50)
  r_out <- terra::aggregate(r_in, fact = 2, fun = "mean")
  s <- summarize_transformation(r_in, r_out,
                                resampling = "average",
                                n_sample = 500, seed = 1)
  out <- capture.output(print(s))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Resampling hint")
  expect_match(txt, "expected R")
})

test_that("print.transformation_summary() returns the object invisibly", {
  r_in  <- create_test_raster(values = 1:100)
  r_out <- r_in
  s <- summarize_transformation(r_in, r_out)
  expect_invisible(print(s))
})
