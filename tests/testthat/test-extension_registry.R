# test-extension_registry.R
#
# Tests for the extension registry in extension_registry.R.
# Covers: register_hrfspec_extension, is_external_hrfspec,
#         get_external_hrfspec_info, requires_external_processing,
#         get_external_hrfspec_functions, get_all_external_hrf_functions,
#         list_external_hrfspecs.

library(testthat)

testthat::local_edition(3)

# ── Cleanup helper ──────────────────────────────────────────────────────────

# Clear any test registrations after each test to avoid cross-contamination
local_clean_registry <- function(classes, env = parent.frame()) {
  withr::defer({
    reg <- fmridesign:::.fmridesign_extensions
    for (cls in classes) {
      if (exists(cls, envir = reg, inherits = FALSE)) {
        rm(list = cls, envir = reg)
      }
    }
  }, envir = env)
}

# ── register_hrfspec_extension() ────────────────────────────────────────────

test_that("register_hrfspec_extension() registers a new class", {
  local_clean_registry("test_hrfspec_A")

  result <- register_hrfspec_extension(
    spec_class = "test_hrfspec_A",
    package = "testPkg"
  )

  expect_null(result)  # returns invisible NULL
  expect_true("test_hrfspec_A" %in% list_external_hrfspecs())
})

test_that("register_hrfspec_extension() stores all fields correctly", {
  local_clean_registry("test_hrfspec_B")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_B",
    package = "testPkg",
    convolved_class = "test_convolved_B",
    requires_external_processing = TRUE,
    formula_functions = c("test_hrf_B", "test_trial_B")
  )

  info <- get_external_hrfspec_info("test_hrfspec_B")
  expect_equal(info$spec_class, "test_hrfspec_B")
  expect_equal(info$package, "testPkg")
  expect_equal(info$convolved_class, "test_convolved_B")
  expect_true(info$requires_external_processing)
  expect_equal(info$formula_functions, c("test_hrf_B", "test_trial_B"))
  expect_true(inherits(info$registered_at, "POSIXct"))
})

test_that("register_hrfspec_extension() validates spec_class is character", {
  expect_error(
    register_hrfspec_extension(spec_class = 123, package = "pkg"),
    "single character string"
  )
  expect_error(
    register_hrfspec_extension(spec_class = c("a", "b"), package = "pkg"),
    "single character string"
  )
})

test_that("register_hrfspec_extension() validates package is character", {
  expect_error(
    register_hrfspec_extension(spec_class = "cls", package = 42),
    "single character string"
  )
})

test_that("register_hrfspec_extension() validates formula_functions", {
  expect_error(
    register_hrfspec_extension(
      spec_class = "cls", package = "pkg",
      formula_functions = 42
    ),
    "character vector"
  )
})

test_that("register_hrfspec_extension() accepts NULL formula_functions", {
  local_clean_registry("test_hrfspec_C")

  # Should not error
  register_hrfspec_extension(
    spec_class = "test_hrfspec_C",
    package = "testPkg",
    formula_functions = NULL
  )

  info <- get_external_hrfspec_info("test_hrfspec_C")
  expect_null(info$formula_functions)
})

# ── is_external_hrfspec() ──────────────────────────────────────────────────

test_that("is_external_hrfspec() detects registered class by name", {
  local_clean_registry("test_hrfspec_D")

  expect_false(is_external_hrfspec("test_hrfspec_D"))

  register_hrfspec_extension(
    spec_class = "test_hrfspec_D",
    package = "testPkg"
  )

  expect_true(is_external_hrfspec("test_hrfspec_D"))
})

test_that("is_external_hrfspec() detects registered class by object", {
  local_clean_registry("test_hrfspec_E")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_E",
    package = "testPkg"
  )

  obj <- structure(list(), class = c("test_hrfspec_E", "list"))
  expect_true(is_external_hrfspec(obj))
})

test_that("is_external_hrfspec() returns FALSE for unregistered class", {
  expect_false(is_external_hrfspec("totally_unregistered_class"))
})

test_that("is_external_hrfspec() works with multi-class objects", {
  local_clean_registry("test_hrfspec_F")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_F",
    package = "testPkg"
  )

  obj <- structure(list(), class = c("other_class", "test_hrfspec_F", "list"))
  expect_true(is_external_hrfspec(obj))
})

# ── get_external_hrfspec_info() ────────────────────────────────────────────

test_that("get_external_hrfspec_info() returns NULL for unregistered class", {
  info <- get_external_hrfspec_info("nonexistent_class")
  expect_null(info)
})

test_that("get_external_hrfspec_info() returns correct info for registered class", {
  local_clean_registry("test_hrfspec_G")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_G",
    package = "testPkg",
    requires_external_processing = TRUE
  )

  info <- get_external_hrfspec_info("test_hrfspec_G")
  expect_true(is.list(info))
  expect_equal(info$spec_class, "test_hrfspec_G")
  expect_equal(info$package, "testPkg")
  expect_true(info$requires_external_processing)
})

# ── requires_external_processing() ─────────────────────────────────────────

test_that("requires_external_processing() returns TRUE for registered external-processing spec", {
  local_clean_registry("test_hrfspec_H")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_H",
    package = "testPkg",
    requires_external_processing = TRUE
  )

  obj <- structure(list(), class = c("test_hrfspec_H", "list"))
  expect_true(requires_external_processing(obj))
})

test_that("requires_external_processing() returns FALSE for non-external spec", {
  local_clean_registry("test_hrfspec_I")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_I",
    package = "testPkg",
    requires_external_processing = FALSE
  )

  obj <- structure(list(), class = c("test_hrfspec_I", "list"))
  expect_false(requires_external_processing(obj))
})

test_that("requires_external_processing() returns FALSE for unregistered class", {
  obj <- structure(list(), class = "plain_list")
  expect_false(requires_external_processing(obj))
})

test_that("requires_external_processing() with character input falls through to FALSE", {
  # Note: requires_external_processing() uses class(x) internally, so a bare

  # character string "test_hrfspec_J" has class "character", not "test_hrfspec_J".
  # Even though is_external_hrfspec("test_hrfspec_J") is TRUE, the for-loop
  # over class(x) won't find the registered class. This is expected behavior:
  # the function is designed for objects, not class-name strings.
  local_clean_registry("test_hrfspec_J")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_J",
    package = "testPkg",
    requires_external_processing = TRUE
  )

  # Character input: is_external_hrfspec detects it, but requires_external_processing
  # cannot resolve via class() dispatch, so it returns FALSE
  expect_false(requires_external_processing("test_hrfspec_J"))
  expect_false(requires_external_processing("unregistered"))

  # Object input works correctly
  obj <- structure(list(), class = c("test_hrfspec_J", "list"))
  expect_true(requires_external_processing(obj))
})

# ── get_external_hrfspec_functions() ────────────────────────────────────────

test_that("get_external_hrfspec_functions() returns registered functions", {
  local_clean_registry("test_hrfspec_K")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_K",
    package = "testPkg",
    formula_functions = c("my_hrf", "my_trialwise")
  )

  funcs <- get_external_hrfspec_functions("test_hrfspec_K")
  expect_equal(funcs, c("my_hrf", "my_trialwise"))
})

test_that("get_external_hrfspec_functions() returns NULL for unregistered class", {
  funcs <- get_external_hrfspec_functions("nonexistent_class")
  expect_null(funcs)
})

test_that("get_external_hrfspec_functions() returns NULL for registered class with no functions", {
  local_clean_registry("test_hrfspec_L")

  register_hrfspec_extension(
    spec_class = "test_hrfspec_L",
    package = "testPkg",
    formula_functions = NULL
  )

  funcs <- get_external_hrfspec_functions("test_hrfspec_L")
  expect_null(funcs)
})

test_that("get_external_hrfspec_functions() has legacy fallback for afni_hrfspec", {
  local_clean_registry("afni_hrfspec")

  # Register without formula_functions to test fallback
  register_hrfspec_extension(
    spec_class = "afni_hrfspec",
    package = "afnireg",
    formula_functions = NULL
  )

  funcs <- get_external_hrfspec_functions("afni_hrfspec")
  expect_equal(funcs, c("afni_hrf"))
})

# ── get_all_external_hrf_functions() ────────────────────────────────────────

test_that("get_all_external_hrf_functions() aggregates from all registrations", {
  local_clean_registry(c("test_hrfspec_M", "test_hrfspec_N"))

  register_hrfspec_extension(
    spec_class = "test_hrfspec_M",
    package = "testPkgM",
    formula_functions = c("hrf_m1", "hrf_m2")
  )
  register_hrfspec_extension(
    spec_class = "test_hrfspec_N",
    package = "testPkgN",
    formula_functions = "hrf_n1"
  )

  all_funcs <- get_all_external_hrf_functions()
  expect_true("hrf_m1" %in% all_funcs)
  expect_true("hrf_m2" %in% all_funcs)
  expect_true("hrf_n1" %in% all_funcs)
})

test_that("get_all_external_hrf_functions() returns unique values", {
  local_clean_registry(c("test_hrfspec_O", "test_hrfspec_P"))

  register_hrfspec_extension(
    spec_class = "test_hrfspec_O",
    package = "testPkgO",
    formula_functions = "shared_hrf"
  )
  register_hrfspec_extension(
    spec_class = "test_hrfspec_P",
    package = "testPkgP",
    formula_functions = "shared_hrf"
  )

  all_funcs <- get_all_external_hrf_functions()
  expect_equal(sum(all_funcs == "shared_hrf"), 1)
})

# ── list_external_hrfspecs() ───────────────────────────────────────────────

test_that("list_external_hrfspecs() returns character vector of registered classes", {
  local_clean_registry(c("test_hrfspec_Q", "test_hrfspec_R"))

  register_hrfspec_extension(
    spec_class = "test_hrfspec_Q",
    package = "testPkgQ"
  )
  register_hrfspec_extension(
    spec_class = "test_hrfspec_R",
    package = "testPkgR"
  )

  listed <- list_external_hrfspecs()
  expect_true(is.character(listed))
  expect_true("test_hrfspec_Q" %in% listed)
  expect_true("test_hrfspec_R" %in% listed)
})

test_that("list_external_hrfspecs() returns names from the registry environment", {
  # Just ensure the function doesn't error and returns a character
  result <- list_external_hrfspecs()
  expect_true(is.character(result) || is.null(result))
})
