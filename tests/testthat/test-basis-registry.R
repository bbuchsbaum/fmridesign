# Tests for the parametric basis registry (R/extension_registry.R, R/zzz.R).

test_that("built-in bases are registered with expected prefixes", {
  builtins <- list_registered_bases()
  expect_true(all(c("Poly", "BSpline", "Scale", "Standardized",
                    "ScaleWithin", "RobustScale", "Ident") %in% builtins))

  expect_equal(get_basis_entry("Poly")$prefix, "poly")
  expect_equal(get_basis_entry("BSpline")$prefix, "bs")
  expect_equal(get_basis_entry("Scale")$prefix, "z")
  expect_equal(get_basis_entry("Standardized")$prefix, "std")
  expect_equal(get_basis_entry("ScaleWithin")$prefix, "z")
  expect_equal(get_basis_entry("RobustScale")$prefix, "robz")

  # Ident has no prefix (variables become column names directly).
  expect_null(get_basis_entry("Ident")$prefix)
})

test_that("make_term_tag uses registry prefixes for built-in bases", {
  des <- data.frame(
    onset = c(0, 10, 20, 30),
    run   = 1,
    rt    = c(0.5, 0.6, 0.55, 0.4)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = 40, TR = 1)
  emod <- event_model(onset ~ hrf(Poly(rt, 2)) + hrf(Scale(rt)),
                      data = des, block = ~run, sampling_frame = sframe)
  expect_true("poly_rt" %in% names(terms(emod)))
  expect_true("z_rt"    %in% names(terms(emod)))
})

test_that("registering a new basis is reflected in .parametric_prefixes", {
  on.exit(rm("DemoBasisXYZ", envir = fmridesign:::.fmridesign_basis_registry,
             inherits = FALSE), add = TRUE)
  register_basis("DemoBasisXYZ", prefix = "demo", modulation = "parametric")
  expect_true("DemoBasisXYZ" %in% list_registered_bases())
  expect_true("demo_" %in% fmridesign:::.parametric_prefixes())
})
