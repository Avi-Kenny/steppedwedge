test_that("params() returns documented defaults", {
  p <- params()
  expect_named(p, c("offset", "n_knots_exp", "n_knots_cal", "var_est",
                    "var_est_type", "return_ncs", "re_correlated", "w"))
  expect_null(p$offset)
  expect_equal(p$n_knots_exp, 4)
  expect_equal(p$n_knots_cal, 4)
  expect_equal(p$var_est, "model")
  expect_equal(p$var_est_type, "classic")
  expect_false(p$return_ncs)
  expect_equal(p$w, 1)
})

test_that("params() stores a custom washout period", {
  expect_equal(params(w = 3)$w, 3)
})

test_that("params() accepts a NULL washout period", {
  expect_null(params(w = NULL)$w)
})

test_that("params() rejects invalid washout periods", {
  expect_snapshot(params(w = 0), error = TRUE)
  expect_snapshot(params(w = 1.5), error = TRUE)
})
