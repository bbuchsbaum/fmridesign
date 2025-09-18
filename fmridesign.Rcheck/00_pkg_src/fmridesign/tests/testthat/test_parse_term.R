test_that("parse_term handles multiple variables and complex expressions", {
  vars <- list(quote(x), quote(log(y + 1)), quote(Poly(rt, 3)))
  res <- fmridesign:::parse_term(vars, "covariate")
  expect_equal(res$term, c("x", "log(y + 1)", "Poly(rt, 3)"))
  expect_equal(res$label, "covariate(x,log(y + 1),Poly(rt, 3))")
})

