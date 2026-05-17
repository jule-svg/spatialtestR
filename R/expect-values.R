#' Assert that all values in a SpatRaster fall within a range
#' Checks that every non-NA cell value of `r` satisfies
#' `lower <= value <= upper`.  Fails with a message that reports the observed
#' minimum and maximum.
#'
#' @param r A `terra::SpatRaster`.
#' @param lower,upper Numeric. Inclusive bounds.
#'
#' @return `r`, invisibly (for pipe use).
#'
#' @examples
#' # values inside the bounds: passes
#' r <- create_test_raster(values = seq(0, 1, length.out = 100))
#' expect_raster_values_between(r, lower = 0, upper = 1)
#'
#' # NA cells are ignored
#' r_na <- create_test_raster(values = c(0.2, 0.5, NA, 0.8))
#' expect_raster_values_between(r_na, 0, 1)
#'
#' @export
# -----------------------------------------------------------------------------
expect_raster_values_between <- function(r, lower, upper) {
  check_suggested("terra")

  vals    <- as.numeric(terra::values(r))
  vals_ok <- vals[!is.na(vals)]

  if (length(vals_ok) == 0L) {
    return(invisible(r))
  }

  obs_min <- min(vals_ok)
  obs_max <- max(vals_ok)

  if (obs_min < lower || obs_max > upper) {
    rlang::abort(sprintf(
      "`r` has values outside [%g, %g]: observed min = %g, observed max = %g.",
      lower, upper, obs_min, obs_max
    ))
  }

  invisible(r)
}

# -----------------------------------------------------------------------------
#' Assert that the mean of a SpatRaster is preserved after a transformation
#'
#' Checks that `mean(r2) ≈ mean(r1)` within `tolerance` (absolute
#' difference). Useful for verifying that a function does not introduce
#' systematic bias.
#'
#' @param r1,r2 `terra::SpatRaster` objects. `r1` is the input (reference),
#'   `r2` is the output (result to check).
#' @param tolerance Numeric. Maximum allowed absolute difference between
#'   the two means. Default: `1e-6`.
#'
#' @return `r2`, invisibly (for pipe use).
#'
#' @examples
#' # identical inputs: mean is preserved exactly
#' r <- create_test_raster(values = 5)
#' expect_mean_preserved(r, r)
#'
#' # tiny numeric noise is tolerated
#' r1 <- create_test_raster(values = 10)
#' r2 <- create_test_raster(values = 10 + 1e-8)
#' expect_mean_preserved(r1, r2, tolerance = 1e-6)
#'
#' @export
# -----------------------------------------------------------------------------
expect_mean_preserved <- function(r1, r2, tolerance = 1e-6) {
  check_suggested("terra")

  m1 <- mean(as.numeric(terra::values(r1)), na.rm = TRUE)
  m2 <- mean(as.numeric(terra::values(r2)), na.rm = TRUE)

  diff <- abs(m1 - m2)

  if (diff > tolerance) {
    rlang::abort(sprintf(
      "Mean changed from %g to %g (absolute difference %g exceeds tolerance %g).",
      m1, m2, diff, tolerance
    ))
  }

  invisible(r2)
}