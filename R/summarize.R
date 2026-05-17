
# HELPER: validate_resampling() -----------------------------------------------
# Normalises the `resampling` argument passed to summarize_transformation().
# Accepts: NULL, FALSE, TRUE, or a string identifying a resampling method.
# Returns a structured list with:
#   $kind        : NULL | FALSE | TRUE | canonical method name (character)
#   $display     : human-readable label for the print output
#   $expected_r2 : c(lower, upper) of typical R^2 range for this method, or NULL

validate_resampling <- function(x) {

  if (is.null(x)) {
    return(list(kind = NULL,
                display = "auto-detect",
                expected_r2 = NULL))
  }

  if (isTRUE(x)) {
    return(list(kind = TRUE,
                display = "TRUE (method unspecified)",
                expected_r2 = c(0.85, 0.99)))
  }

  if (isFALSE(x)) {
    return(list(kind = FALSE,
                display = "FALSE (no resampling expected)",
                expected_r2 = NULL))
  }

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    # canonical names + accepted aliases
    aliases <- list(
      nearest_neighbor = c("nearest_neighbor", "nearest", "ngb", "nn"),
      bilinear         = c("bilinear", "linear"),
      cubic            = c("cubic", "bicubic", "cubicspline"),
      lanczos          = c("lanczos"),
      mode             = c("mode", "majority"),
      average          = c("average", "mean", "aggregate")
    )

    canon <- NULL
    for (key in names(aliases)) {
      if (tolower(x) %in% aliases[[key]]) {
        canon <- key
        break
      }
    }
    if (is.null(canon)) {
      rlang::abort(sprintf(
        "Unknown resampling method: '%s'. Valid options are: %s.",
        x, paste(names(aliases), collapse = ", ")
      ))
    }

    # typical R^2 ranges (rough heuristics)
    expected_r2 <- switch(canon,
      nearest_neighbor = c(0.90, 0.98),
      bilinear         = c(0.95, 0.99),
      cubic            = c(0.93, 0.99),
      lanczos          = c(0.93, 0.99),
      mode             = c(0.85, 0.98),
      average          = c(0.92, 0.99)
    )

    return(list(kind = canon,
                display = canon,
                expected_r2 = expected_r2))
  }

  rlang::abort(paste0(
    "Invalid value for `resampling`. Must be NULL (auto-detect), TRUE, FALSE, ",
    "or one of: 'nearest_neighbor', 'bilinear', 'cubic', 'lanczos', 'mode', ",
    "'average'."
  ))
}



# HELPER: describe_geom_diff() ------------------------------------------------
# Returns a compact human-readable description of how two SpatRasters differ
# geometrically.  Used for error messages.

describe_geom_diff <- function(r_in, r_out) {
  diffs <- character(0)

  if (terra::nrow(r_in) != terra::nrow(r_out) ||
      terra::ncol(r_in) != terra::ncol(r_out) ||
      terra::nlyr(r_in) != terra::nlyr(r_out)) {
    diffs <- c(diffs, sprintf(
      "  dimensions: %dx%dx%d (in)  vs  %dx%dx%d (out)",
      terra::nrow(r_in),  terra::ncol(r_in),  terra::nlyr(r_in),
      terra::nrow(r_out), terra::ncol(r_out), terra::nlyr(r_out)
    ))
  }

  if (!terra::same.crs(r_in, r_out)) {
    diffs <- c(diffs, "  CRS:        differs")
  }

  if (!identical(as.vector(terra::ext(r_in)),
                 as.vector(terra::ext(r_out)))) {
    diffs <- c(diffs, "  extent:     differs")
  }

  if (length(diffs) == 0L) return("  (no visible differences detected)")
  paste(diffs, collapse = "\n")
}


# HELPER: get_paired_values() -------------------------------------------------
# Extract paired (input, output) values from two SpatRasters.
#
# Strategy:
#   - If geometry is identical (same dims, CRS, extent, res) -> pixel-wise 1:1
#   - Otherwise -> geographic random sampling (n_sample points within r_in)
#
# Returns a structured list with:
#   $v_in           : numeric vector of input values
#   $v_out          : numeric vector of output values (same length as v_in)
#   $method         : "pixel" (1:1) or "sampled" (geographic sampling)
#   $n_used         : length of v_in / v_out
#   $resampling     : the validated `resampling` argument
#   $geometry_match : TRUE if all of dims/CRS/extent/res agreed, else FALSE

get_paired_values <- function(r_in, r_out,
                              resampling = NULL,
                              n_sample   = 10000,
                              seed       = NULL) {
  check_suggested("terra")

  # 1. Validate the resampling argument -----------------------------------
  resampling <- validate_resampling(resampling)

  # 2. Compare geometry ---------------------------------------------------
  # compareGeom returns TRUE if dims, CRS, extent, and resolution all match.
  # We deliberately leave lyrs = FALSE (layer count handled separately).
  geom_match <- terra::compareGeom(
    r_in, r_out,
    lyrs        = FALSE,
    crs         = TRUE,
    ext         = TRUE,
    rowcol      = TRUE,
    res         = TRUE,
    stopOnError = FALSE
  )

  # 3. Validate consistency between user claim and reality ----------------
  if (isFALSE(resampling$kind) && !geom_match) {
    rlang::abort(paste0(
      "You specified `resampling = FALSE`, but the geometries of `r_in` and ",
      "`r_out` differ:\n",
      describe_geom_diff(r_in, r_out),
      "\nDid you forget about an upstream resample/project/aggregate step?"
    ))
  }

  user_claims_resampling <- isTRUE(resampling$kind) || is.character(resampling$kind)
  if (user_claims_resampling && geom_match) {
    rlang::warn(paste0(
      "You specified `resampling = ", resampling$display, "`, but the ",
      "geometries of `r_in` and `r_out` are identical -- no resampling appears ",
      "to have happened.  Using pixel-wise 1:1 comparison."
    ))
  }

  # 4. Extract paired values ---------------------------------------------
  if (geom_match) {
    # pixel-wise 1:1 across the whole raster
    v_in  <- as.numeric(terra::values(r_in))
    v_out <- as.numeric(terra::values(r_out))
    method <- "pixel"
  } else {
    # geographic random sampling
    if (!is.null(seed)) set.seed(seed)

    # cap sample size at the number of available cells (sensible upper bound)
    n_effective <- min(n_sample, terra::ncell(r_in))

    # replace = TRUE guarantees exactly n_effective points (avoids
    # terra's "fewer cells returned than requested" issue at high coverage)
    pts <- terra::spatSample(
      r_in,
      size      = n_effective,
      method    = "random",
      as.points = TRUE,
      na.rm     = FALSE,
      replace   = TRUE
    )

    v_in <- terra::extract(r_in, pts, ID = FALSE)[[1]]

    # reproject points to r_out's CRS if needed; extract() needs matching CRS
    if (!terra::same.crs(r_in, r_out)) {
      pts <- terra::project(pts, terra::crs(r_out))
    }
    v_out <- terra::extract(r_out, pts, ID = FALSE)[[1]]

    method <- "sampled"
  }

  # 5. Return structured result ------------------------------------------
  list(
    v_in           = v_in,
    v_out          = v_out,
    method         = method,
    n_used         = length(v_in),
    resampling     = resampling,
    geometry_match = geom_match
  )
}


# diag_structure() ------------------------------------------------------------
# Side-by-side structural comparison of two SpatRasters.
# Reports dimensions, CRS, extent, resolution, origin, datatype, and layer
# count.  Returns a structured list that the print method later renders.

diag_structure <- function(r_in, r_out) {
  check_suggested("terra")

  dims_in  <- c(terra::nrow(r_in),  terra::ncol(r_in),  terra::nlyr(r_in))
  dims_out <- c(terra::nrow(r_out), terra::ncol(r_out), terra::nlyr(r_out))

  ext_in  <- as.vector(terra::ext(r_in))
  ext_out <- as.vector(terra::ext(r_out))

  res_in  <- terra::res(r_in)
  res_out <- terra::res(r_out)

  origin_in  <- terra::origin(r_in)
  origin_out <- terra::origin(r_out)

  list(
    dims = list(
      in_  = dims_in,
      out_ = dims_out,
      equal = identical(dims_in, dims_out)
    ),
    crs = list(
      in_  = crs_describe(r_in),
      out_ = crs_describe(r_out),
      equal = terra::same.crs(r_in, r_out)
    ),
    extent = list(
      in_  = ext_in,
      out_ = ext_out,
      equal = identical(ext_in, ext_out)
    ),
    resolution = list(
      in_  = res_in,
      out_ = res_out,
      equal = identical(res_in, res_out)
    ),
    origin = list(
      in_  = origin_in,
      out_ = origin_out,
      equal = identical(origin_in, origin_out)
    ),
    datatype = list(
      in_  = terra::datatype(r_in),
      out_ = terra::datatype(r_out),
      equal = identical(terra::datatype(r_in), terra::datatype(r_out))
    ),
    layers = list(
      in_  = terra::nlyr(r_in),
      out_ = terra::nlyr(r_out),
      equal = terra::nlyr(r_in) == terra::nlyr(r_out)
    )
  )
}


# diag_values() ---------------------------------------------------------------
# Numerical summary of paired input/output values:
#  - basic range / centre / spread statistics
#  - Inf / NaN counts on the output side
#  - NA-pattern diagnosis (input vs output)

diag_values <- function(v_in, v_out) {
  if (length(v_in) != length(v_out)) {
    rlang::abort("`v_in` and `v_out` must have the same length.")
  }

  # NA / Inf / NaN bookkeeping -------------------------------------------
  na_in   <- is.na(v_in)
  na_out  <- is.na(v_out)

  n_inf_pos <- sum(v_out ==  Inf, na.rm = TRUE)
  n_inf_neg <- sum(v_out == -Inf, na.rm = TRUE)
  n_nan     <- sum(is.nan(v_out))

  # valid pairs (for range/mean/sd) --------------------------------------
  ok_in  <- !na_in  & is.finite(v_in)
  ok_out <- !na_out & is.finite(v_out)

  vi <- v_in [ok_in]
  vo <- v_out[ok_out]

  range_in   <- if (length(vi) > 0) range(vi)        else c(NA_real_, NA_real_)
  range_out  <- if (length(vo) > 0) range(vo)        else c(NA_real_, NA_real_)
  mean_in    <- if (length(vi) > 0) mean(vi)         else NA_real_
  mean_out   <- if (length(vo) > 0) mean(vo)         else NA_real_
  sd_in      <- if (length(vi) > 1) stats::sd(vi)    else NA_real_
  sd_out     <- if (length(vo) > 1) stats::sd(vo)    else NA_real_

  # dynamic range expressed as orders of magnitude (log10 max-min)
  dyn_range_in  <- if (length(vi) > 0 && range_in[1]  > 0) log10(range_in[2]  / range_in[1])  else NA_real_
  dyn_range_out <- if (length(vo) > 0 && range_out[1] > 0) log10(range_out[2] / range_out[1]) else NA_real_

  list(
    n_total       = length(v_in),
    n_valid_in    = sum(ok_in),
    n_valid_out   = sum(ok_out),

    range_in      = range_in,
    range_out     = range_out,
    mean_in       = mean_in,
    mean_out      = mean_out,
    sd_in         = sd_in,
    sd_out        = sd_out,
    dyn_range_in  = dyn_range_in,
    dyn_range_out = dyn_range_out,

    n_na_in       = sum(na_in),
    n_na_out      = sum(na_out),
    n_na_added    = sum(!na_in &  na_out),  # valid in, NA in out
    n_na_lost     = sum( na_in & !na_out),  # NA in, valid in out

    n_inf_pos     = n_inf_pos,
    n_inf_neg     = n_inf_neg,
    n_nan         = n_nan
  )
}


# diag_linear_fit() # ---------------------------------------------------------
# Fit a simple linear regression  out = slope * in + intercept  and classify
# the relationship.  Returns NA fields when fitting is impossible (constant
# input or fewer than 2 valid pairs).

diag_linear_fit <- function(v_in, v_out,
                            tolerance        = 1e-6,
                            linear_threshold = 0.999) {

  # paired & valid (both non-NA, both finite)
  ok <- !is.na(v_in) & !is.na(v_out) &
        is.finite(v_in) & is.finite(v_out)
  vi <- v_in [ok]
  vo <- v_out[ok]

  na_fit <- function(reason) {
    list(
      slope          = NA_real_,
      intercept      = NA_real_,
      r_squared      = NA_real_,
      classification = NA_character_,
      n_valid_pairs  = length(vi),
      note           = reason
    )
  }

  if (length(vi) < 2L)         return(na_fit("fewer than 2 valid pairs"))
  if (stats::var(vi) == 0)     return(na_fit("input is constant -- slope undefined"))

  fit   <- stats::lm(vo ~ vi)
  coefs <- stats::coef(fit)

  slope     <- unname(coefs[2])
  intercept <- unname(coefs[1])

  # Compute R^2 directly from residuals to avoid summary.lm()'s
  # "essentially perfect fit" warning on identity-like transformations.
  ss_res <- sum(fit$residuals^2)
  ss_tot <- sum((vo - mean(vo))^2)
  rsq <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

  classification <- classify_fit(slope, intercept, rsq,
                                 tolerance, linear_threshold)

  list(
    slope          = slope,
    intercept      = intercept,
    r_squared      = rsq,
    classification = classification,
    n_valid_pairs  = length(vi),
    note           = NA_character_
  )
}


# classify_fit() --------------------------------------------------------------
# Map (slope, intercept, R^2) to a discrete relationship label.
#
# Returns one of:
#   "identity"   - slope ~= 1 and intercept ~= 0   (output = input)
#   "scaling"    - slope != 1 and intercept ~= 0   (output = a * input)
#   "shift"      - slope ~= 1 and intercept != 0   (output = input + b)
#   "linear"     - general affine, R^2 >= threshold
#   "non_linear" - R^2 below threshold

classify_fit <- function(slope, intercept, rsq,
                         tolerance, linear_threshold) {

  if (is.na(rsq) || rsq < linear_threshold) return("non_linear")

  unit_slope     <- abs(slope - 1)     < tolerance
  zero_intercept <- abs(intercept)     < tolerance

  if (unit_slope && zero_intercept) return("identity")
  if (zero_intercept)               return("scaling")
  if (unit_slope)                   return("shift")
  "linear"
}


# summarize_transformation()  -------------------------------------------------

#' Summarise the transformation between an input and an output SpatRaster
#'
#' Runs a full diagnostic comparison of two SpatRasters: the *input* (raster
#' before some user function was applied) and the *output* (raster after).
#' Combines structural, value-based, and regression-based diagnostics into a
#' single printable summary object.
#'
#' The function decides automatically how to pair values:
#'   - If the two rasters share the same geometry (dimensions, CRS, extent,
#'     resolution): pixel-wise 1:1 comparison using all cells.
#'   - Otherwise: random geographic sampling at `n_sample` points within
#'     `r_in` (NA cells included).
#'
#' On the paired values, a simple linear regression `output ~ input` is fitted
#' and classified as one of: `"identity"`, `"scaling"`, `"shift"`, `"linear"`
#' (affine), or `"non_linear"`.
#'
#' @param r_in,r_out `terra::SpatRaster` objects. `r_in` is the input
#'   (e.g. a fixture from [create_test_raster()]), `r_out` is the user
#'   function's output.
#' @param resampling Indicates whether and how the user resampled `r_in` to
#'   produce `r_out`. One of:
#'   * `NULL` (default): auto-detect from geometry.
#'   * `FALSE`: assert that no resampling happened. Throws an error if the
#'     two rasters' geometries actually differ.
#'   * `TRUE`: resampling happened, method unspecified.
#'   * a character string giving the resampling method:
#'     `"nearest_neighbor"`, `"bilinear"`, `"cubic"`, `"lanczos"`,
#'     `"mode"`, or `"average"` (with common aliases accepted).
#' @param n_sample Integer. Maximum number of geographic sample points used
#'   when geometry differs. Default: 10000.
#' @param seed Integer or `NULL`. Random seed for reproducible sampling.
#' @param tolerance Numeric. Tolerance for slope-vs-1 and intercept-vs-0
#'   comparisons in the linear-fit classification. Default: `1e-6`.
#' @param linear_threshold Numeric in (0, 1). Minimum R^2 for a relationship
#'   to be classified as linear at all (below this: `"non_linear"`).
#'   Default: `0.999`.
#'
#' @return An object of class `"transformation_summary"` with components
#'   `structure`, `values`, `fit`, `paired`, and `call`. Use `print()`
#'   to render a human-readable diagnosis.
#'
#' @examples
#' r_in  <- create_test_raster(values = 1:100)
#' r_out <- r_in * 2 + 5
#' summarize_transformation(r_in, r_out)
#'
#' @export
summarize_transformation <- function(r_in, r_out,
                                     resampling       = NULL,
                                     n_sample         = 10000,
                                     seed             = NULL,
                                     tolerance        = 1e-6,
                                     linear_threshold = 0.999) {
  check_suggested("terra")

  # 1. Structural diagnosis (always)
  structure <- diag_structure(r_in, r_out)

  # 2. Pair the values (validates the resampling argument internally)
  paired <- get_paired_values(r_in, r_out,
                              resampling = resampling,
                              n_sample   = n_sample,
                              seed       = seed)

  # 3. Value-level summary
  values <- diag_values(paired$v_in, paired$v_out)

  # 4. Linear fit + classification
  fit <- diag_linear_fit(paired$v_in, paired$v_out,
                         tolerance        = tolerance,
                         linear_threshold = linear_threshold)

  result <- list(
    structure = structure,
    values    = values,
    fit       = fit,
    paired    = list(
      method         = paired$method,
      n_used         = paired$n_used,
      geometry_match = paired$geometry_match,
      resampling     = paired$resampling
    ),
    call = sys.call()
  )
  class(result) <- "transformation_summary"
  result
}


# print method for transformation_summary # -----------------------------------

#' @export
print.transformation_summary <- function(x, ...) {

  flag <- function(equal) if (isTRUE(equal)) "[same]" else "[differs]"

  # Format numbers compactly. Floating-point noise (|v| < 1e-10) is rendered
  # as 0, so identity-like transformations don't print artefacts like
  # "intercept: -4.55e-14".
  fmt_num <- function(v, digits = 3, noise_zero = 1e-10) {
    if (length(v) == 0 || all(is.na(v))) return("NA")
    v <- ifelse(abs(v) < noise_zero, 0, v)
    format(v, digits = digits, trim = TRUE)
  }

  # Format datatype: returns "<in-memory>" for in-memory rasters where
  # terra::datatype() yields an empty string.
  fmt_datatype <- function(dt) {
    if (length(dt) == 0 || all(dt == "") || all(is.na(dt))) return("<in-memory>")
    if (length(dt) > 1)  paste(unique(dt), collapse = ", ")
    else                 dt
  }

  cat("Raster transformation summary\n")
  cat("=============================\n\n")

  # Geometry -------------------------------------------------------------
  cat("Geometry\n")
  s <- x$structure
  cat(sprintf("  dimensions:    %d x %d x %d   ->   %d x %d x %d   %s\n",
              s$dims$in_[1], s$dims$in_[2], s$dims$in_[3],
              s$dims$out_[1], s$dims$out_[2], s$dims$out_[3],
              flag(s$dims$equal)))

  crs_in_label  <- if (!is.na(s$crs$in_$code))  paste0("EPSG:", s$crs$in_$code)
                   else if (!is.na(s$crs$in_$name)) s$crs$in_$name
                   else "<unnamed>"
  crs_out_label <- if (!is.na(s$crs$out_$code)) paste0("EPSG:", s$crs$out_$code)
                   else if (!is.na(s$crs$out_$name)) s$crs$out_$name
                   else "<unnamed>"
  cat(sprintf("  CRS:           %s   ->   %s   %s\n",
              crs_in_label, crs_out_label, flag(s$crs$equal)))

  cat(sprintf("  extent:        [%s, %s] x [%s, %s]   ->   [%s, %s] x [%s, %s]   %s\n",
              fmt_num(s$extent$in_[1]),  fmt_num(s$extent$in_[2]),
              fmt_num(s$extent$in_[3]),  fmt_num(s$extent$in_[4]),
              fmt_num(s$extent$out_[1]), fmt_num(s$extent$out_[2]),
              fmt_num(s$extent$out_[3]), fmt_num(s$extent$out_[4]),
              flag(s$extent$equal)))

  cat(sprintf("  resolution:    %s x %s   ->   %s x %s   %s\n",
              fmt_num(s$resolution$in_[1]),  fmt_num(s$resolution$in_[2]),
              fmt_num(s$resolution$out_[1]), fmt_num(s$resolution$out_[2]),
              flag(s$resolution$equal)))

  cat(sprintf("  datatype:      %s   ->   %s   %s\n",
              fmt_datatype(s$datatype$in_),
              fmt_datatype(s$datatype$out_),
              flag(s$datatype$equal)))

  cat("\n")

  # Sampling info --------------------------------------------------------
  cat("Sample\n")
  cat(sprintf("  method:        %s\n",
              if (x$paired$method == "pixel") "pixel-wise 1:1"
              else "random geographic sampling"))
  cat(sprintf("  used:          %d paired values\n", x$paired$n_used))
  cat(sprintf("  resampling:    %s\n", x$paired$resampling$display))
  cat("\n")

  # Values ---------------------------------------------------------------
  v <- x$values
  cat("Values  (input -> output)\n")
  cat(sprintf("  range:         [%s, %s]   ->   [%s, %s]\n",
              fmt_num(v$range_in[1]),  fmt_num(v$range_in[2]),
              fmt_num(v$range_out[1]), fmt_num(v$range_out[2])))
  cat(sprintf("  mean:          %s   ->   %s\n",
              fmt_num(v$mean_in), fmt_num(v$mean_out)))
  cat(sprintf("  sd:            %s   ->   %s\n",
              fmt_num(v$sd_in),   fmt_num(v$sd_out)))
  cat("\n")

  cat("NA / Inf / NaN\n")
  cat(sprintf("  NA:            %d in input   ->   %d in output   (added %d, lost %d)\n",
              v$n_na_in, v$n_na_out, v$n_na_added, v$n_na_lost))
  if (v$n_inf_pos > 0 || v$n_inf_neg > 0) {
    cat(sprintf("  Inf:           +Inf %d,  -Inf %d  in output\n",
                v$n_inf_pos, v$n_inf_neg))
  }
  if (v$n_nan > 0) {
    cat(sprintf("  NaN:           %d  in output\n", v$n_nan))
  }
  cat("\n")

  # Linear fit -----------------------------------------------------------
  f <- x$fit
  cat("Linear fit  (output ~ input)\n")
  if (is.na(f$slope)) {
    cat(sprintf("  could not fit:  %s\n", f$note))
  } else {
    cat(sprintf("  slope:         %s\n",     fmt_num(f$slope)))
    cat(sprintf("  intercept:     %s\n",     fmt_num(f$intercept)))
    cat(sprintf("  R^2:           %s    (%d valid pairs)\n",
                fmt_num(f$r_squared, digits = 4), f$n_valid_pairs))

    label <- switch(f$classification,
      identity   = "IDENTITY        (output approx input)",
      scaling    = sprintf("SCALING         (output approx %s x input)", fmt_num(f$slope)),
      shift      = sprintf("SHIFT           (output approx input + %s)", fmt_num(f$intercept)),
      linear     = sprintf("LINEAR (affine) (output approx %s x input + %s)",
                           fmt_num(f$slope), fmt_num(f$intercept)),
      non_linear = "NOT well described by a line",
      f$classification
    )
    cat(sprintf("  ->             %s\n", label))
  }

  # Method-specific hint -------
  r <- x$paired$resampling
  if (is.character(r$kind) && !is.na(f$r_squared)) {
    cat("\n")
    cat(sprintf("Resampling hint (%s)\n", r$display))
    cat(sprintf("  expected R^2:  %s - %s\n",
                fmt_num(r$expected_r2[1]), fmt_num(r$expected_r2[2])))
    status <- if (f$r_squared >= r$expected_r2[1] &&
                  f$r_squared <= r$expected_r2[2] + 1e-9) {
      "within expected range"
    } else if (f$r_squared < r$expected_r2[1]) {
      "LOWER than expected - output may have an additional non-linear transformation"
    } else {
      "higher than expected - unusual for this method"
    }
    cat(sprintf("  observed R^2:  %s    -> %s\n",
                fmt_num(f$r_squared, digits = 4), status))
  }

  invisible(x)
}
