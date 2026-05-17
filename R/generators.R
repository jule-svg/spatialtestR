# internal helper: check that a suggested package is installed
check_suggested <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(paste0(
      "Package '",
      pkg,
      "' is required. Please install it with: ",
      "install.packages('",
      pkg,
      "')"
    ))
  }
}

# -----------------------------------------------------------------------------
#' Create a test SpatRaster with known properties
#'
#' Generates a synthetic `terra` SpatRaster with controllable dimensions,
#' CRS, and cell values. Useful as reproducible input for unit tests of
#' raster-processing functions.
#'
#' @param nrow Integer. Number of rows. Default: 10
#' @param ncol Integer. Number of columns. Default: 10
#' @param nlyr Integer. Number of layers. Default: 1
#' @param crs Character. CRS string (EPSG code or WKT). Default: `"EPSG:4326"`
#' @param xmin,xmax,ymin,ymax Numeric. Extent of the raster
#' @param values Numeric vector or `NULL`. If `NULL` (default), cells are filled
#'   with sequential values 1 to `nrow * ncol * nlyr`. If a single value is
#'   supplied, all cells receive that value. Otherwise the vector is recycled
#' @param na_fraction Numeric in \[0, 1\]. Fraction of cells to set to `NA`
#'   Default: 0 (no NAs)
#' @param seed Integer or `NULL`. Random seed for reproducible NA placement
#'
#' @return A `terra::SpatRaster` object
#'
#' @examples
#' # simple 10x10 raster with sequential values
#' r <- create_test_raster()
#'
#' # UTM raster for tests that need concrete numeric values
#' r_utm <- create_test_raster(crs = "EPSG:32632",
#'                             xmin = 300000, xmax = 310000,
#'                             ymin = 5000000, ymax = 5010000)
#'
#' # raster with 20% NA cells, reproducible
#' r_na <- create_test_raster(na_fraction = 0.2, seed = 42)
#'
#' @export
# -----------------------------------------------------------------------------
create_test_raster <- function(nrow = 10, ncol = 10, nlyr = 1, crs = "EPSG:4326",
                               xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                               values = NULL, na_fraction = 0, seed = NULL) 
{
  check_suggested("terra")

  # input validation
  if (!is.numeric(nrow) || nrow < 1) {
    rlang::abort("`nrow` must be a positive integer.")}
  
  if (!is.numeric(ncol) || ncol < 1) {
    rlang::abort("`ncol` must be a positive integer.")}
  
  if (!is.numeric(nlyr) || nlyr < 1) {
    rlang::abort("`nlyr` must be a positive integer.")}
  
  if (!is.numeric(na_fraction) || na_fraction < 0 || na_fraction > 1) {
    rlang::abort("`na_fraction` must be a number between 0 and 1.")}
  
  if (xmin >= xmax) {
    rlang::abort("`xmin` must be less than `xmax`.")}
  
  if (ymin >= ymax) {
    rlang::abort("`ymin` must be less than `ymax`.")}

  # build raster
  n_cells <- nrow * ncol * nlyr

  r <- terra::rast(nrows = nrow, ncols = ncol, nlyr = nlyr,
                   xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   crs = crs)

  # fill values
  if (is.null(values)) {fill <- seq_len(n_cells)} 
    else {fill <- rep_len(values, n_cells)}
  
  terra::values(r) <- fill

  # introduce NAs
  if (na_fraction > 0) {if (!is.null(seed)) {set.seed(seed)}
    
    na_idx <- sample(seq_len(n_cells), size = floor(na_fraction * n_cells))
    v <- terra::values(r)
    v[na_idx] <- NA
    terra::values(r) <- v}
  r
}

# -----------------------------------------------------------------------------
#' Create a multiband test SpatRaster with named layers
#'
#' A convenience wrapper around [create_test_raster()] that produces a
#' multiband raster and assigns meaningful layer names. Useful for testing
#' spectral-index functions (NDVI, NDWI, …) that expect specific band names.
#'
#' @param nrow,ncol Integer. Raster dimensions. Defaults: 10 x 10.
#' @param crs Character. CRS string. Default: `"EPSG:4326"`.
#' @param xmin,xmax,ymin,ymax Numeric. Extent. Same defaults as
#'   [create_test_raster()].
#' @param band_names Character vector of layer names. Default:
#'   `c("blue", "green", "red", "nir")`.
#' @param band_values Named list or `NULL`. Each element gives values for one
#'   band (scalar or vector of length `nrow * ncol`). Names must match
#'   `band_names`. If `NULL`, bands receive sequential values 1 … `nrow * ncol`.
#' @param na_fraction,seed Passed to [create_test_raster()] for each band.
#' @param na_pattern Character. Controls how NA cells are distributed across
#'   bands. One of:
#'   * `"independent"` (default): each band gets its own random NA pattern
#'     (e.g. sensor-specific drop-outs, multi-source data fusion).
#'   * `"shared"`: all bands have NA in *exactly the same* pixel positions
#'     (e.g. cloud masks, geometric masks — if a pixel is invalid for one
#'     band, it is invalid for all).
#'
#' @return A multiband `terra::SpatRaster` with `length(band_names)` layers.
#'
#' @examples
#' # default 4-band raster (blue, green, red, nir)
#' ms <- create_test_multiband()
#'
#' # controlled reflectance values for NDVI = (0.5 - 0.1) / (0.5 + 0.1) = 0.667
#' ms2 <- create_test_multiband(
#'   band_names  = c("red", "nir"),
#'   band_values = list(red = rep(0.1, 100), nir = rep(0.5, 100))
#' )
#'
#' # cloud-mask-like NAs: every band NA in the SAME pixels
#' ms_cloud <- create_test_multiband(
#'   na_fraction = 0.2, na_pattern = "shared", seed = 42
#' )
#'
#' # sensor-defect-like NAs: each band has its OWN random NA pattern
#' ms_sensor <- create_test_multiband(
#'   na_fraction = 0.1, na_pattern = "independent", seed = 42
#' )
#'
#' @export
# -----------------------------------------------------------------------------
create_test_multiband <- function(nrow = 10, ncol = 10, crs = "EPSG:4326",
                                  xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                                  band_names = c("blue", "green", "red", "nir"),
                                  band_values = NULL, na_fraction = 0,
                                  na_pattern = c("independent", "shared"),
                                  seed = NULL)
{
  check_suggested("terra")
  na_pattern <- match.arg(na_pattern)

  if (!is.character(band_names) || length(band_names) < 1) {
    rlang::abort("`band_names` must be a non-empty character vector.")
  }

  # validate band_values if supplied
  if (!is.null(band_values)) {
    if (!is.list(band_values)) {rlang::abort("`band_values` must be a named list.")}

    missing_bands <- setdiff(band_names, names(band_values))

    if (length(missing_bands) > 0) {rlang::abort(paste0("`band_values` is missing entries for: ",
        paste(missing_bands, collapse = ", ")))}
  }

  # for shared pattern: pre-compute one NA mask used for all bands
  shared_na_idx <- NULL
  if (na_pattern == "shared" && na_fraction > 0) {
    if (!is.null(seed)) set.seed(seed)
    n_cells <- nrow * ncol
    shared_na_idx <- sample(seq_len(n_cells),
                            size = floor(na_fraction * n_cells))
  }

  # build each band individually, then stack
  bands <- lapply(seq_along(band_names), function(i) {
    bname <- band_names[i]
    bvals <- if (!is.null(band_values)) {
      band_values[[bname]]}
      else {seq_len(nrow * ncol)}

    if (na_pattern == "shared") {
      # build raster without NAs, then apply the precomputed shared mask
      r <- create_test_raster(
        nrow = nrow, ncol = ncol, nlyr = 1,
        crs = crs, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        values = bvals,
        na_fraction = 0
      )
      if (!is.null(shared_na_idx)) {
        v <- terra::values(r)
        v[shared_na_idx] <- NA
        terra::values(r) <- v
      }
      r
    } else {
      # independent pattern: each band gets its own NA placement via seed + i
      create_test_raster(
        nrow = nrow, ncol = ncol, nlyr = 1,
        crs = crs, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        values = bvals,
        na_fraction = na_fraction,
        seed = if (!is.null(seed)) seed + i else NULL
      )
    }
  })

  r <- terra::rast(bands)
  names(r) <- band_names
  r
}
