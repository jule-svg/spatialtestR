# HELPER FUNCTIONS
# -----------------------------------------------------------------------------
# Get descriptive CRS info from a raster, vector, or CRS string.
# Returns the data.frame produced by terra::crs(..., describe = TRUE).
crs_describe <- function(x) {
  if (inherits(x, c("SpatRaster", "SpatVector"))) {
    return(terra::crs(x, describe = TRUE))
  }
  terra::crs(terra::rast(crs = x), describe = TRUE)
}
# -----------------------------------------------------------------------------
# Build a human-readable description of a CRS mismatch.
# Strategy: if both CRSes have an EPSG code, render those (compact).
# Otherwise fall back to the human-readable name (or "<unnamed CRS>").
# Only called when terra::same.crs() has already returned FALSE.
crs_mismatch_text <- function(actual, expected) {
  a <- crs_describe(actual)
  e <- crs_describe(expected)

  has_a_code <- !is.na(a$code) && nzchar(a$code)
  has_e_code <- !is.na(e$code) && nzchar(e$code)

  if (has_a_code && has_e_code) {
    return(sprintf("expected EPSG:%s, got EPSG:%s", e$code, a$code))
  }

  format_one <- function(info) {
    if (!is.na(info$code) && nzchar(info$code)) {
      paste0("EPSG:", info$code)
    } else if (!is.na(info$name) && nzchar(info$name)) {
      info$name
    } else {
      "<unnamed CRS>"
    }
  }

  sprintf(
    "different CRS:\n  expected: %s\n  got:      %s",
    format_one(e), format_one(a)
  )
}
# -----------------------------------------------------------------------------

#' Assert that a SpatRaster has the expected dimensions
#'
#' Checks `nrow`, `ncol`, and/or `nlyr` of `r` against expected values.
#' Any combination of the three arguments may be supplied; omitted arguments
#' are not checked. Emits a single error listing all mismatches.
#'
#' @param r A `terra::SpatRaster`.
#' @param nrow,ncol,nlyr Expected integer dimensions, or `NULL` to skip.
#'
#' @return `r`, invisibly (for pipe use).
#'
#' @examples
#' r <- create_test_raster(nrow = 10, ncol = 10)
#' expect_raster_dims(r, nrow = 10, ncol = 10)
#'
#' # check multiple dimensions at once
#' r3d <- create_test_raster(nrow = 5, ncol = 8, nlyr = 3)
#' expect_raster_dims(r3d, nrow = 5, ncol = 8, nlyr = 3)
#'
#' @export
# -----------------------------------------------------------------------------
expect_raster_dims <- function(r, nrow = NULL, ncol = NULL, nlyr = NULL) {
  check_suggested("terra")

  msgs <- character(0)

  if (!is.null(nrow)) {
    actual <- terra::nrow(r)
    if (actual != nrow) {
      msgs <- c(msgs, sprintf("nrow: expected %d, got %d", nrow, actual))
    }
  }
  if (!is.null(ncol)) {
    actual <- terra::ncol(r)
    if (actual != ncol) {
      msgs <- c(msgs, sprintf("ncol: expected %d, got %d", ncol, actual))
    }
  }
  if (!is.null(nlyr)) {
    actual <- terra::nlyr(r)
    if (actual != nlyr) {
      msgs <- c(msgs, sprintf("nlyr: expected %d, got %d", nlyr, actual))
    }
  }

  if (length(msgs) > 0) {
    rlang::abort(paste0("`r` has wrong dimensions:\n", paste(msgs, collapse = "\n")))
  }

  invisible(r)
}

# -----------------------------------------------------------------------------
#' Assert that a SpatRaster has the expected CRS
#'
#' Compares the CRS of `r` against `crs`, accepting either EPSG codes
#' (e.g. `"EPSG:32632"`) or WKT strings. Comparison is delegated to
#' [terra::same.crs()], which normalises both representations internally.
#' On mismatch, the error message reports both EPSG codes (when available)
#' or the CRS names.
#'
#' @param r A `terra::SpatRaster`.
#' @param crs Character. Expected CRS as either:
#'   * An EPSG string: `"EPSG:32632"`, `"epsg:4326"` (case-insensitive).
#'   * A WKT string (e.g. produced by `terra::crs(r)` on another raster).
#'   * Any other CRS specification that `terra::crs()` accepts.
#'
#' @return `r`, invisibly (for pipe use).
#'
#' @examples
#' # via EPSG code
#' r <- create_test_raster(crs = "EPSG:32632")
#' expect_raster_crs(r, "EPSG:32632")
#'
#' # via WKT (e.g. from another raster)
#' wkt <- terra::crs(r)
#' expect_raster_crs(r, wkt)
#'
#' @export
# -----------------------------------------------------------------------------
expect_raster_crs <- function(r, crs) {
  check_suggested("terra")

  if (terra::same.crs(r, crs)) {
    return(invisible(r))
  }

  rlang::abort(paste0(
    "`r` has wrong CRS: ", crs_mismatch_text(r, crs)
  ))
}

# -----------------------------------------------------------------------------
#' Assert that two SpatRasters have the same dimensions
#'
#' Compares `nrow`, `ncol`, and `nlyr` of two rasters. Designed for the
#' typical test pattern: build a known input with [create_test_raster()],
#' run the user's function on it, then verify the output has the same shape
#' as the input. Emits a single error listing all dimension mismatches.
#'
#' @param r_in,r_out `terra::SpatRaster` objects to compare. By convention
#'   `r_in` is the test fixture (input) and `r_out` is the function's result.
#'
#' @return `r_out`, invisibly (for pipe use).
#'
#' @examples
#' r_in  <- create_test_raster(nrow = 20, ncol = 20, nlyr = 3)
#' r_out <- r_in * 2     # any operation that should preserve shape
#' expect_same_dims(r_in, r_out)
#'
#' @export
# -----------------------------------------------------------------------------
expect_same_dims <- function(r_in, r_out) {
  check_suggested("terra")

  msgs <- character(0)

  if (terra::nrow(r_in) != terra::nrow(r_out)) {
    msgs <- c(msgs, sprintf("nrow: input has %d, output has %d",
                            terra::nrow(r_in), terra::nrow(r_out)))
  }
  if (terra::ncol(r_in) != terra::ncol(r_out)) {
    msgs <- c(msgs, sprintf("ncol: input has %d, output has %d",
                            terra::ncol(r_in), terra::ncol(r_out)))
  }
  if (terra::nlyr(r_in) != terra::nlyr(r_out)) {
    msgs <- c(msgs, sprintf("nlyr: input has %d, output has %d",
                            terra::nlyr(r_in), terra::nlyr(r_out)))
  }

  if (length(msgs) > 0) {
    rlang::abort(paste0(
      "Input and output rasters have different dimensions:\n",
      paste(msgs, collapse = "\n")
    ))
  }

  invisible(r_out)
}

# -----------------------------------------------------------------------------
#' Assert that two SpatRasters have the same CRS
#' Compares the CRS of two rasters via [terra::same.crs()]. Designed for the
#' typical test pattern: verify that the user's function has not silently
#' reprojected or stripped the CRS of the output. On mismatch, the error
#' message reports both EPSG codes (when available) or the CRS names.
#'
#' @param r_in,r_out `terra::SpatRaster` objects to compare. By convention
#'   `r_in` is the test fixture (input) and `r_out` is the function's result.
#'
#' @return `r_out`, invisibly (for pipe use).
#'
#' @examples
#' r_in  <- create_test_raster(crs = "EPSG:32632")
#' r_out <- r_in * 2     # any operation that should preserve the CRS
#' expect_same_crs(r_in, r_out)
#'
#' @export
# -----------------------------------------------------------------------------
expect_same_crs <- function(r_in, r_out) {
  check_suggested("terra")

  if (terra::same.crs(r_in, r_out)) {
    return(invisible(r_out))
  }

  text <- crs_mismatch_text(r_out, r_in)
  text <- sub("expected", "input",  text)
  text <- sub("got",      "output", text)
  rlang::abort(paste0("Input and output rasters have different CRS: ", text))
}
