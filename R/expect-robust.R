#' Assert that two SpatRasters have the same NA pattern
#' Checks that `r1` and `r2` have `NA` in exactly the same cells.  Reports
#' the number of cells where the patterns disagree and which direction each
#' discrepancy goes.
#'
#' @param r1,r2 `terra::SpatRaster` objects with the same number of cells.
#'
#' @return `r2`, invisibly (for pipe use).
#'
#' @examples
#' # both rasters built with the same seed: identical NA pattern
#' r1 <- create_test_raster(na_fraction = 0.2, seed = 42)
#' r2 <- create_test_raster(na_fraction = 0.2, seed = 42)
#' expect_na_consistent(r1, r2)
#'
#' @export
# -----------------------------------------------------------------------------
expect_na_consistent <- function(r1, r2) {
  check_suggested("terra")

  na1 <- is.na(as.numeric(terra::values(r1)))
  na2 <- is.na(as.numeric(terra::values(r2)))

  only_r1 <- sum(na1 & !na2)
  only_r2 <- sum(!na1 & na2)
  total   <- only_r1 + only_r2

  if (total > 0L) {
    rlang::abort(sprintf(
      "NA pattern of `r1` and `r2` differs in %d cell(s):\n%d cell(s) are NA in `r1` but not in `r2`, %d cell(s) are NA in `r2` but not in `r1`.",
      total, only_r1, only_r2
    ))
  }

  invisible(r2)
}

# ------------------------------------------------------------------------------------
#' Assert that a SpatRaster contains no Inf or -Inf values
#'
#' Fails if any non-NA cell in `r` is infinite, reporting the separate counts
#' of `Inf` and `-Inf` values found.
#'
#' @param r A `terra::SpatRaster`.
#'
#' @return `r`, invisibly (for pipe use).
#'
#' @examples
#' # generators never produce Inf
#' r <- create_test_raster(values = 1:100)
#' expect_no_inf(r)
#'
#' # arithmetic with finite values stays finite
#' expect_no_inf(r * 2)
#'
#' @export
# -----------------------------------------------------------------------------
expect_no_inf <- function(r) {
  check_suggested("terra")

  vals    <- as.numeric(terra::values(r))
  n_pos   <- sum(vals ==  Inf, na.rm = TRUE)
  n_neg   <- sum(vals == -Inf, na.rm = TRUE)

  if (n_pos > 0L || n_neg > 0L) {
    rlang::abort(sprintf(
      "`r` contains %d Inf value(s) and %d -Inf value(s).",
      n_pos, n_neg
    ))
  }

  invisible(r)
}