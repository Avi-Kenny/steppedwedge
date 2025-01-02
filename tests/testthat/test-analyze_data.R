# Sample data for testing
sample_data <- data.frame(
  period = rep(c(1, 2, 3), each = 6),
  cluster_id = rep(c(1, 2), each = 3, times = 3),
  individual_id = rep(1:18),
  treatment = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1),
  y_bin = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
) %>%
  dplyr::arrange(cluster_id)

sw_data <- load_data(
  time = "period",
  cluster_id = "cluster_id",
  individual_id = "individual_id",
  treatment = "treatment",
  outcome = "y_bin",
  data = sample_data
)

# Invalid input

test_that("Input validation works", {
  expect_error(analyze(dat=sample_data, estimand_type="TATE"),
               "`dat` must be of class `sw_dat`.")
})

# Valid input

# Model type and estimand_type returned correctly

test_that("Correct model type and estimand_type for IT mixed model", {
  result <- analyze(dat=sw_data, method="mixed", estimand_type="TATE", exp_time="IT", family="gaussian")
  expect_equal(result$model_type, "it_mixed")
  expect_equal(result$estimand_type, "TATE/LTE")
})

test_that("Correct model type and estimand_type for ETI mixed model, TATE", {
  result <- analyze(sw_data, method="mixed", estimand_type="TATE", exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_mixed")
  expect_equal(result$estimand_type, "TATE")
})

test_that("Correct model type and estimand_type for ETI mixed model, LTE", {
  result <- analyze(sw_data, method="mixed", estimand_type="LTE", exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_mixed")
  expect_equal(result$estimand_type, "LTE")
})

test_that("Correct model type and estimand_type for IT GEE model, TATE", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE", exp_time="IT", family="gaussian")
  expect_equal(result$model_type, "it_GEE")
  expect_equal(result$estimand_type, "TATE/LTE")
})

test_that("Correct model type and estimand_type for ETI GEE model, TATE", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE", exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_GEE")
  expect_equal(result$estimand_type, "TATE")
})

test_that("Correct model type and estimand_type for ETI GEE model, LTE", {
  result <- analyze(sw_data, method="GEE", estimand_type="LTE", exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_GEE")
  expect_equal(result$estimand_type, "LTE")
})


# Model coefficients returned correctly

test_that("Model coefficients are returned correctly", {
  result <- analyze(sw_data, method="mixed", estimand_type="TATE", exp_time="IT", family="gaussian")
  expect_true("te_est" %in% names(result))
  expect_true("te_se" %in% names(result))
  expect_true("te_ci" %in% names(result))
})

# Different family and link functions

test_that("Function handles different family and link functions", {
  result_binomial <- analyze(sw_data, method="mixed", estimand_type="TATE", exp_time="IT", family="binomial")
  result_gaussian <- analyze(sw_data, method="mixed", estimand_type="TATE", exp_time="IT", family="gaussian")
  expect_true(result_binomial$model@resp$family$family == "binomial")
  expect_true(methods::is(result_gaussian$model,"lmerMod"))
})
