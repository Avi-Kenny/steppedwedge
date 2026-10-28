data(sw_data_example)

sw_print <- suppressMessages(load_data(
  time = "period", cluster_id = "cluster", treatment = "trt",
  outcome = "outcome_cont", data = sw_data_example
))

fit_quiet <- function(...) suppressMessages(suppressWarnings(analyze(...)))

fit_mixed <- fit_quiet(sw_print, method = "mixed", estimand_type = "TATE",
                       estimand_time = c(1, 4), exp_time = "IT")
fit_gee <- fit_quiet(sw_print, method = "GEE", estimand_type = "TATE",
                     estimand_time = c(1, 4), exp_time = "IT")
fit_exp <- fit_quiet(sw_print, method = "mixed", estimand_type = "TATE",
                     estimand_time = c(1, 4), exp_time = "IT",
                     exponentiate = TRUE)

test_that("print() describes mixed models and their estimates", {
  expect_output(print(fit_mixed),
                "Mixed model with random effects for cluster and time")
  expect_output(print(fit_mixed), 'Exp time: "IT", cal time: "categorical"')
  expect_output(print(fit_mixed), "TATE \\(IT\\) estimate: ")
  expect_output(print(fit_mixed), "TATE \\(IT\\) 95% confidence interval: ")
  expect_output(print(fit_mixed), "TATE \\(IT\\) p-value: ")
  expect_output(print(fit_mixed), "Converged: ")
})

test_that("print() describes GEE models and reports NA convergence", {
  expect_output(print(fit_gee),
                "GEE model with working exchangeable correlation structure")
  expect_output(print(fit_gee), "Converged: TRUE")
})

test_that("print() marks exponentiated estimates", {
  expect_output(print(fit_exp), "TATE \\(IT\\) estimate \\(exponentiated\\): ")
})

test_that("print() labels random effects and falls back to raw codes", {
  x <- fit_mixed
  x$re <- c("tx", "ind")
  expect_output(print(x),
                "Mixed model with random effects for treatment and individual")
  x$re <- c("clust", "foo")
  expect_output(print(x), "Mixed model with random effects for cluster and foo")
})
