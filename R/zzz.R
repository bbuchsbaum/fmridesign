#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Register built-in parametric basis classes so make_term_tag() and
  # design metadata can look up prefixes/modulation from a single source.
  register_basis("Poly",         prefix = "poly", modulation = "parametric",
                 description = "Orthogonal polynomial expansion")
  register_basis("BSpline",      prefix = "bs",   modulation = "parametric",
                 description = "B-spline basis expansion")
  register_basis("Scale",        prefix = "z",    modulation = "parametric",
                 description = "Z-score scaling (single column)")
  register_basis("ScaleWithin",  prefix = "z",    modulation = "parametric",
                 description = "Within-group z-score scaling")
  register_basis("Standardized", prefix = "std",  modulation = "parametric",
                 description = "Standardized (mean/sd) scaling")
  register_basis("RobustScale",  prefix = "robz", modulation = "parametric",
                 description = "Robust scaling (median/MAD)")
  register_basis("Ident",        prefix = NULL,   modulation = "amplitude",
                 description = "Identity basis: variables become column names")
  invisible()
}
