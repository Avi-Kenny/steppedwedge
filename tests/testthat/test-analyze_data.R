# Sample data for testing
set.seed(2278)

sample_data <- data.frame(cluster = 1:15) %>%
  # expand data_sw with period 1 through 6 for each cluster
  tidyr::expand(cluster, period = 1:6) %>%
  # generate cluster period sizes
  dplyr::mutate(cp_size = sample(15:30, dplyr::n(), replace = TRUE))

# treatment effects
b_bin <- log(2)
b_cont <- 0.5

sample_data %<>%
  # create individual observations for each cluster period
  tidyr::uncount(weights = cp_size) %>%
  # assign sequences
  dplyr::mutate(seq = ceiling(cluster / 3)) %>%
  # assign treatment indicator
  dplyr::mutate(trt = ifelse(period > seq, 1, 0)) %>%
  # generate cluster random effect
  dplyr::group_by(cluster) %>%
  dplyr::mutate(cluster_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  # generate cluster-time random effect
  dplyr::group_by(cluster, period) %>%
  dplyr::mutate(cluster_time_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  dplyr::ungroup() %>%
  # generate binary outcomes
  dplyr::rowwise() %>%
  dplyr::mutate(prob = plogis(0.2 + b_bin * trt + cluster_re + cluster_time_re)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(outcome_bin = rbinom(n = dplyr::n(), size = 1, prob = prob)) %>%
  # generate continuous outcomes
  dplyr::mutate(outcome_cont = rnorm(n = dplyr::n(), 
                                     mean = (b_cont * trt + cluster_re+cluster_time_re),
                                     sd = 1)) %>%
  # generate binomial outcomes
  dplyr::mutate(denominator = floor(runif(nrow(.), min = 5, max = 15))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(numerator = rbinom(1, denominator, prob)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(seq, dplyr::ends_with("_re"))) %>%
  # convert to non-tibble dataframe
  data.frame()

sw_data <- load_data(
  time ="period",
  cluster_id = "cluster",
  individual_id = NULL,
  treatment = "trt",
  outcome = "outcome_bin",
  data = sample_data
)

# Invalid input

test_that("Input validation works", {
  expect_error(analyze(dat=sample_data, estimand_type="TATE", estimand_time = c(1, 2)),
               "`dat` must be of class `sw_dat`.")
})

# Valid input

# Model type and estimand_type returned correctly

test_that("Correct model type and estimand_type for IT mixed model", {
  result <- analyze(dat=sw_data, method="mixed", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="IT", family="gaussian")
  expect_equal(result$model_type, "it_mixed")
  expect_equal(result$estimand_type, "TATE (IT)")
})

test_that("Correct model type and estimand_type for ETI mixed model, TATE", {
  result <- analyze(sw_data, method="mixed", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_mixed")
  expect_equal(result$estimand_type, "TATE")
})

test_that("Correct model type and estimand_type for ETI mixed model, PTE", {
  result <- analyze(sw_data, method="mixed", estimand_type="PTE",
                    estimand_time = 2, exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_mixed")
  expect_equal(result$estimand_type, "PTE")
})

test_that("Correct model type and estimand_type for IT GEE model, TATE", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="IT", family="gaussian")
  expect_equal(result$model_type, "it_GEE")
  expect_equal(result$estimand_type, "TATE (IT)")
})

test_that("Correct model type and estimand_type for ETI GEE model, TATE", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_GEE")
  expect_equal(result$estimand_type, "TATE")
})

test_that("Correct model type and estimand_type for ETI GEE model, PTE", {
  result <- analyze(sw_data, method="GEE", estimand_type="PTE",
                    estimand_time = 2, exp_time="ETI", family="gaussian")
  expect_equal(result$model_type, "eti_GEE")
  expect_equal(result$estimand_type, "PTE")
})


# Model coefficients returned correctly

test_that("Model coefficients are returned correctly", {
  result <- analyze(sw_data, method="mixed", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="IT", family="gaussian")
  expect_true("te_est" %in% names(result))
  expect_true("te_se" %in% names(result))
  expect_true("te_ci" %in% names(result))
})

# Different family and link functions

test_that("Function handles different family and link functions", {
  result_binomial <- analyze(sw_data, method="mixed", estimand_type="TATE",
                             estimand_time = c(1, 2), exp_time="IT", family="binomial")
  result_gaussian <- analyze(sw_data, method="mixed", estimand_type="TATE",
                             estimand_time = c(1, 2), exp_time="IT", family="gaussian")
  expect_true(result_binomial$model@resp$family$family == "binomial")
  expect_true(methods::is(result_gaussian$model,"lmerMod"))
})

# P-value Calculation Tests

test_that("P-value is calculated correctly for Mixed IT model", {
  # Gaussian (Identity link)
  result <- analyze(sw_data, method="mixed", estimand_type="TATE",
                    exp_time="IT", family="gaussian")
  
  # Check existence and type
  expect_true(!is.null(result$te_p))
  expect_true(is.numeric(result$te_p))
  expect_true(result$te_p >= 0 && result$te_p <= 1)
  
  # Check calculation (Wald test on linear scale)
  # formula: 2 * (1 - pnorm(|est / se|))
  manual_p <- 2 * (1 - pnorm(abs(result$te_est / result$te_se)))
  expect_equal(result$te_p, manual_p)
})

test_that("P-value is consistent when exponentiating (Mixed IT)", {
  # Use binomial/logit so exponentiation makes sense (Odds Ratio)
  # 1. Linear run (Log-Odds)
  result_lin <- analyze(sw_data, method="mixed", estimand_type="TATE",
                        exp_time="IT", family="binomial", exponentiate = FALSE)
  
  # 2. Exponentiated run (Odds Ratio)
  result_exp <- analyze(sw_data, method="mixed", estimand_type="TATE",
                        exp_time="IT", family="binomial", exponentiate = TRUE)
  
  # P-values should be identical (hypothesis test is invariant to transformation)
  expect_equal(result_lin$te_p, result_exp$te_p)
  
  # Check calculation using log of exponentiated estimate
  # Note: The function returns te_se on the linear scale even if exponentiate=TRUE
  linear_est_from_exp <- log(result_exp$te_est)
  manual_p <- 2 * (1 - pnorm(abs(linear_est_from_exp / result_exp$te_se)))
  expect_equal(result_exp$te_p, manual_p)
})

test_that("P-value is calculated correctly for Mixed ETI model (TATE)", {
  result <- analyze(sw_data, method="mixed", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="ETI", family="gaussian")
  
  expect_true(!is.null(result$te_p))
  
  # For TATE, te_est is the weighted average, te_se is the SE of that average
  manual_p <- 2 * (1 - pnorm(abs(result$te_est / result$te_se)))
  expect_equal(result$te_p, manual_p)
})

test_that("P-value is calculated correctly for Mixed ETI model (PTE)", {
  result <- analyze(sw_data, method="mixed", estimand_type="PTE",
                    estimand_time = 2, exp_time="ETI", family="gaussian")
  
  expect_true(!is.null(result$te_p))
  
  manual_p <- 2 * (1 - pnorm(abs(result$te_est / result$te_se)))
  expect_equal(result$te_p, manual_p)
})

test_that("P-value is calculated correctly for GEE IT model", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE",
                    exp_time="IT", family="gaussian")
  
  expect_true(!is.null(result$te_p))
  
  manual_p <- 2 * (1 - pnorm(abs(result$te_est / result$te_se)))
  expect_equal(result$te_p, manual_p)
})

test_that("P-value is calculated correctly for GEE ETI model", {
  result <- analyze(sw_data, method="GEE", estimand_type="TATE",
                    estimand_time = c(1, 2), exp_time="ETI", family="gaussian")
  
  expect_true(!is.null(result$te_p))
  
  manual_p <- 2 * (1 - pnorm(abs(result$te_est / result$te_se)))
  expect_equal(result$te_p, manual_p)
})

