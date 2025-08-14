context("Naming Utilities")

source("helper-naming.R") # Load is_valid_heading

test_that("zeropad works correctly with min width 2", {
  expect_equal(fmridesign:::zeropad(3, 9), "03")  # Now pads to 2 digits even for n_total < 10
  expect_equal(fmridesign:::zeropad(9, 9), "09") 
  expect_equal(fmridesign:::zeropad(3, 10), "03") # Remains 2 digits for n_total = 10
  expect_equal(fmridesign:::zeropad(12, 99), "12")
  expect_equal(fmridesign:::zeropad(12, 100), "012") # Becomes 3 digits when n_total >= 100
  expect_equal(fmridesign:::zeropad(99, 100), "099")
  expect_equal(fmridesign:::zeropad(1:3, 10), c("01", "02", "03")) # Check vector input
  expect_equal(fmridesign:::zeropad(5, 0), "5") # handle n_total=0, pads to width 1 (not 2)
})

test_that("sanitize works correctly", {
  expect_equal(sanitize("a.b c"), "a.b.c")
  expect_equal(sanitize("a b.c", allow_dot = FALSE), "a_b_c")
  expect_equal(sanitize("1var"), "X1var")
  expect_equal(sanitize(c("a", "a")), c("a", "a")) # unique = FALSE
  expect_equal(sanitize("_start"), "X_start")
})

test_that("basis_suffix works correctly", {
  expect_equal(basis_suffix(1, 1), "_b1")  # Always adds suffix, even for single basis
  expect_equal(basis_suffix(1:3, 3), c("_b01", "_b02", "_b03")) # Now expects 2 digits
  expect_equal(basis_suffix(1:3, 15), c("_b01", "_b02", "_b03")) # Already 2 digits
  expect_equal(basis_suffix(1, 5), "_b01") # Now expects 2 digits
  expect_equal(basis_suffix(integer(0), 5), "_b") # Empty input returns "_b" due to paste0 behavior
})

test_that("make_unique_tags works correctly", {
  expect_equal(fmridesign:::make_unique_tags(c("a", "b")), c("a", "b"))
  expect_equal(fmridesign:::make_unique_tags(c("a", "a")), c("a", "a#1"))
  expect_equal(fmridesign:::make_unique_tags(c("a", "a", "a#1")), c("a", "a#2", "a#1"))
})

# Basic test structure for other helpers - expand later

test_that("make_term_tag generates tags", {
  # Test with explicit id
  spec1 <- list(id = "my_term", vars = list())
  expect_equal(fmridesign:::make_term_tag(spec1), "my_term")
  
  # Test with prefix
  spec2 <- list(prefix = "my_prefix", vars = list())
  expect_equal(fmridesign:::make_term_tag(spec2), "my_prefix")
  
  # Test simple variable
  spec3 <- list(vars = list(rlang::quo(RT)))
  expect_equal(fmridesign:::make_term_tag(spec3), "RT")
  
  # Test multiple variables
  spec4 <- list(vars = list(rlang::quo(RT), rlang::quo(cond)))
  expect_equal(fmridesign:::make_term_tag(spec4), "RT_cond")
  
  # Test uniqueness handling
  expect_equal(fmridesign:::make_term_tag(spec3, existing_tags = "RT"), "RT#1")
  expect_equal(fmridesign:::make_term_tag(spec3, existing_tags = c("RT", "RT#1")), "RT#2")
})

test_that("make_term_tag handles all ParametricBasis types correctly", {
  # Test Poly
  poly_spec <- list(vars = list(rlang::quo(Poly(RT, 3))))
  expect_equal(fmridesign:::make_term_tag(poly_spec), "poly_RT")
  
  # Test BSpline
  bs_spec <- list(vars = list(rlang::quo(BSpline(time, 4))))
  expect_equal(fmridesign:::make_term_tag(bs_spec), "bs_time")
  
  # Test Scale
  scale_spec <- list(vars = list(rlang::quo(Scale(RT))))
  expect_equal(fmridesign:::make_term_tag(scale_spec), "z_RT")
  
  # Test Standardized
  std_spec <- list(vars = list(rlang::quo(Standardized(response))))
  expect_equal(fmridesign:::make_term_tag(std_spec), "std_response")
  
  # Test ScaleWithin
  scale_within_spec <- list(vars = list(rlang::quo(ScaleWithin(RT, group))))
  expect_equal(fmridesign:::make_term_tag(scale_within_spec), "z_RT")
  
  # Test RobustScale
  robust_spec <- list(vars = list(rlang::quo(RobustScale(value))))
  expect_equal(fmridesign:::make_term_tag(robust_spec), "robz_value")
  
  # Test Ident (should return NULL)
  ident_spec <- list(vars = list(rlang::quo(Ident(x, y))))
  expect_null(fmridesign:::make_term_tag(ident_spec))
  
  # Test uniqueness for basis functions
  expect_equal(fmridesign:::make_term_tag(poly_spec, existing_tags = "poly_RT"), "poly_RT#1")
})

test_that("make_term_tag handles edge cases", {
  # Empty vars
  empty_spec <- list(vars = list())
  expect_equal(fmridesign:::make_term_tag(empty_spec), "X")  # Empty string gets sanitized to "X"
  
  # Nested expressions
  nested_spec <- list(vars = list(rlang::quo(log(RT))))
  expect_equal(fmridesign:::make_term_tag(nested_spec), "log_RT")  # Parentheses removed by as_label
  
  # Variable with dots
  dot_spec <- list(vars = list(rlang::quo(response.time)))
  expect_equal(fmridesign:::make_term_tag(dot_spec), "response_time")
  
  # Special characters that need sanitization
  special_spec <- list(vars = list(rlang::quo(`bad-var`)))
  expect_equal(fmridesign:::make_term_tag(special_spec), "bad_var")
})

test_that("make_term_tag sanitizes correctly", {
  # Test that dots are replaced with underscores
  spec_dots <- list(id = "term.with.dots")
  expect_equal(fmridesign:::make_term_tag(spec_dots), "term_with_dots")
  
  # Test multiple underscores are collapsed
  spec_underscores <- list(id = "term__with__underscores")
  expect_equal(fmridesign:::make_term_tag(spec_underscores), "term_with_underscores")
  
  # Test leading underscores get X prefix from make.names
  spec_trim <- list(id = "_leading_trailing_")
  expect_equal(fmridesign:::make_term_tag(spec_trim), "X_leading_trailing")
})

test_that("level_token creates Var.Level format", {
  expect_equal(fmridesign:::level_token("cond", "A"), "cond.A")
  expect_equal(fmridesign:::level_token("cond name", "Level 1"), "cond.name.Level.1")
  expect_equal(fmridesign:::level_token("Input", "20"), "Input.20")
  expect_equal(fmridesign:::level_token("Input", c("1", "2")), c("Input.1", "Input.2"))

})

test_that("continuous_token sanitizes", {
  expect_equal(fmridesign:::continuous_token("poly_RT_01"), "poly_RT_01")
  expect_equal(fmridesign:::continuous_token("z_RT by cond"), "z_RT.by.cond")
})

test_that("make_cond_tag combines with underscore", {
  expect_equal(fmridesign:::make_cond_tag(c("cond.A", "task.go")), "cond.A_task.go")
  expect_equal(fmridesign:::make_cond_tag("cond.A"), "cond.A")
})

test_that("add_basis expands tags correctly", {
  expect_equal(fmridesign:::add_basis("cond.A", 1), "cond.A")
  expect_equal(fmridesign:::add_basis("cond.A", 3), c("cond.A_b01", "cond.A_b02", "cond.A_b03"))
  expect_equal(fmridesign:::add_basis(c("t1", "t2"), 2), c("t1_b01", "t2_b01", "t1_b02", "t2_b02"))
})

test_that("make_column_names composes final names", {
  expect_equal(fmridesign:::make_column_names("term1", "cond.A", 1), "term1_cond.A")
  expect_equal(fmridesign:::make_column_names("term1", "cond.A", 3), c("term1_cond.A_b01", "term1_cond.A_b02", "term1_cond.A_b03"))
  expect_equal(fmridesign:::make_column_names("term1", c("c1", "c2"), 2), c("term1_c1_b01", "term1_c2_b01", "term1_c1_b02", "term1_c2_b02"))
  expect_error(fmridesign:::make_column_names("term__bad", "cond.A", 1)) # Double underscore guard
})

test_that("is_valid_heading works", {
    expect_true(is_valid_heading("term_cond.A"))
    expect_true(is_valid_heading("term_cond.A_b01"))
    expect_true(is_valid_heading("term#1_cond.A_b01"))
    expect_true(is_valid_heading(".internal"))
    expect_false(is_valid_heading("_term"))
    expect_true(is_valid_heading("term_"))
    expect_false(is_valid_heading("term name"))
    expect_true(is_valid_heading("term_cond.A_b##"))
}) 