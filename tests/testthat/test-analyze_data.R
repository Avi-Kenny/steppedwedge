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

sw_data_cont <- load_data(
  time = "period",
  cluster_id = "cluster",
  individual_id = NULL,
  treatment = "trt",
  outcome = "outcome_cont",
  data = sample_data
)

sw_data_binom <- load_data(
  time = "period",
  cluster_id = "cluster",
  individual_id = NULL,
  treatment = "trt",
  outcome = c("numerator", "denominator"),
  data = sample_data
)

# Precompute paired results used in exponentiation comparison tests
result_eti_lin <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                          estimand_time = c(1, 2), exp_time = "ETI",
                          family = "binomial", exponentiate = FALSE)
result_eti_exp <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                          estimand_time = c(1, 2), exp_time = "ETI",
                          family = "binomial", exponentiate = TRUE)

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


# Additional input validation -------------------------------------------------

test_that("Error when cal_time is misspecified", {
  expect_error(
    analyze(sw_data, cal_time = "invalid"),
    "`cal_time` misspecified."
  )
})

test_that("Error when exp_time is misspecified", {
  expect_error(
    analyze(sw_data, exp_time = "invalid"),
    "`exp_time` misspecified."
  )
})

test_that("Error when re contains an unrecognized value", {
  expect_error(
    analyze(sw_data, re = c("clust", "bad_re")),
    "Random effects must be a subset of the vector"
  )
})

test_that("Error when TATE estimand_time is not length 2", {
  expect_error(
    analyze(sw_data, estimand_type = "TATE", estimand_time = 1),
    'When estimand_type=="TATE", `estimand_time` must be a numeric vector of length 2'
  )
})

test_that("Error when PTE estimand_time is not length 1", {
  expect_error(
    analyze(sw_data, estimand_type = "PTE", estimand_time = c(1, 2)),
    'When estimand_type=="PTE", `estimand_time` must be a numeric vector of length 1'
  )
})

test_that("Error for DCT with w >= max exposure time", {
  max_et <- max(sw_data_cont$exposure_time)
  expect_error(
    analyze(sw_data_cont, exp_time = "DCT", advanced = params(w = max_et)),
    "For exp_time='DCT', you must specify a valid washout period"
  )
})


# Return structure & class ----------------------------------------------------

test_that("Result is of class sw_analysis", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "IT", family = "gaussian")
  expect_s3_class(result, "sw_analysis")
})

test_that("Result contains all expected top-level fields", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "IT", family = "gaussian")
  expected_fields <- c("model", "model_type", "estimand_type", "te_est", "te_se",
                       "te_ci", "te_p", "converged", "effect_curve", "dat",
                       "method", "exp_time", "cal_time", "re", "corstr", "exponentiated")
  expect_true(all(expected_fields %in% names(result)))
})

test_that("dat field in result matches the input sw_data object", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "IT", family = "gaussian")
  expect_identical(result$dat, sw_data)
})

test_that("CI is a numeric vector of length 2", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "IT", family = "gaussian")
  expect_length(result$te_ci, 2)
  expect_true(is.numeric(result$te_ci))
})

test_that("CI is correctly ordered (lower < upper) for ETI model", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "ETI", family = "gaussian")
  expect_lt(result$te_ci[1], result$te_ci[2])
})

test_that("exponentiated flag stored correctly in result", {
  result_no_exp <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                           estimand_time = c(1, 2), exp_time = "IT", exponentiate = FALSE)
  result_exp    <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                           estimand_time = c(1, 2), exp_time = "IT", exponentiate = TRUE)
  expect_false(result_no_exp$exponentiated)
  expect_true(result_exp$exponentiated)
})


# Effect curve structure ------------------------------------------------------

test_that("Effect curve for ETI model has correct names and length", {
  result <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "ETI", family = "gaussian")
  ec <- result$effect_curve
  expect_true(all(c("exp_time", "est", "se", "vcov", "ci_lower", "ci_upper") %in% names(ec)))
  n_unique_exp_times <- length(unique(sw_data$exposure_time))
  expect_length(ec$exp_time, n_unique_exp_times)
})

test_that("Effect curve starts at exp_time = 0 with zero-reference estimate", {
  result_lin <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                        estimand_time = c(1, 2), exp_time = "ETI", family = "gaussian",
                        exponentiate = FALSE)
  result_exp <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                        estimand_time = c(1, 2), exp_time = "ETI", family = "gaussian",
                        exponentiate = TRUE)
  expect_equal(result_lin$effect_curve$exp_time[1], 0)
  expect_equal(result_lin$effect_curve$est[1], 0)
  expect_equal(result_exp$effect_curve$est[1], 1)  # exp(0) = 1
})


# NCS model -------------------------------------------------------------------

test_that("NCS mixed model returns correct model_type and estimand_type for TATE", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "NCS",
                    advanced = params(n_knots_exp = 4))
  expect_equal(result$model_type, "ncs_mixed")
  expect_equal(result$estimand_type, "TATE")
})

test_that("NCS mixed model returns correct model_type and estimand_type for PTE", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "PTE",
                    estimand_time = 2, exp_time = "NCS",
                    advanced = params(n_knots_exp = 4))
  expect_equal(result$model_type, "ncs_mixed")
  expect_equal(result$estimand_type, "PTE")
})

test_that("NCS model with return_ncs=TRUE appends transformation matrix fields", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "NCS",
                    advanced = params(n_knots_exp = 4, return_ncs = TRUE))
  expect_true(all(c("T_mat", "V_orig", "beta_new", "V_new") %in% names(result)))
})


# TEH model -------------------------------------------------------------------

test_that("TEH mixed model returns correct model_type and estimand_type for TATE", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "TEH", re = "clust")
  expect_equal(result$model_type, "teh_mixed")
  expect_equal(result$estimand_type, "TATE")
})

test_that("TEH mixed model returns correct model_type and estimand_type for PTE", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "PTE",
                    estimand_time = 2, exp_time = "TEH", re = "clust")
  expect_equal(result$model_type, "teh_mixed")
  expect_equal(result$estimand_type, "PTE")
})


# DCT model -------------------------------------------------------------------

test_that("DCT mixed model returns correct model_type and estimand_type", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    exp_time = "DCT", advanced = params(w = 2))
  expect_equal(result$model_type, "dct_mixed")
  expect_equal(result$estimand_type, "TATE (DCT)")
})


# Continuous outcome ----------------------------------------------------------

test_that("IT mixed model works with continuous outcome (lmerMod)", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT")
  expect_equal(result$model_type, "it_mixed")
  expect_s4_class(result$model, "lmerMod")
})

test_that("ETI GEE model works with continuous outcome", {
  result <- analyze(sw_data_cont, method = "GEE", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "ETI")
  expect_equal(result$model_type, "eti_GEE")
  expect_true(is.numeric(result$te_est))
})


# Binomial (successes/trials) outcome -----------------------------------------

test_that("ETI mixed model works with binomial (successes/trials) outcome", {
  result <- analyze(sw_data_binom, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 2), exp_time = "ETI", family = "binomial")
  expect_equal(result$model_type, "eti_mixed")
  expect_equal(result$model@resp$family$family, "binomial")
})


# Exponentiation values -------------------------------------------------------

test_that("Exponentiated te_est equals exp(linear te_est) for IT mixed", {
  result_lin <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                        estimand_time = c(1, 2), exp_time = "IT",
                        family = "binomial", exponentiate = FALSE)
  result_exp <- analyze(sw_data, method = "mixed", estimand_type = "TATE",
                        estimand_time = c(1, 2), exp_time = "IT",
                        family = "binomial", exponentiate = TRUE)
  expect_equal(result_exp$te_est, exp(result_lin$te_est))
  expect_equal(result_exp$te_ci,  exp(result_lin$te_ci))
})

test_that("Exponentiated te_est equals exp(linear te_est) for ETI mixed (TATE)", {
  expect_equal(result_eti_exp$te_est, exp(result_eti_lin$te_est))
  expect_equal(result_eti_exp$te_ci,  exp(result_eti_lin$te_ci))
})


# TATE window sensitivity -----------------------------------------------------

test_that("TATE estimate differs across non-overlapping estimand_time windows (ETI)", {
  result_early <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                          estimand_time = c(1, 2), exp_time = "ETI")
  result_late  <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                          estimand_time = c(4, 5), exp_time = "ETI")
  expect_false(isTRUE(all.equal(result_early$te_est, result_late$te_est)))
})


# cal_time variations ---------------------------------------------------------

test_that("analyze() works with cal_time='linear'", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT", cal_time = "linear")
  expect_equal(result$cal_time, "linear")
  expect_true(is.numeric(result$te_est))
})

test_that("analyze() works with cal_time='none'", {
  result <- analyze(sw_data_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT", cal_time = "none")
  expect_equal(result$cal_time, "none")
  expect_true(is.numeric(result$te_est))
})


# GEE correlation structure ---------------------------------------------------

test_that("GEE IT model works with corstr='ar1'", {
  result <- analyze(sw_data_cont, method = "GEE", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT", corstr = "ar1")
  expect_equal(result$corstr, "ar1")
  expect_equal(result$model_type, "it_GEE")
})

test_that("GEE IT model works with corstr='independence'", {
  result <- analyze(sw_data_cont, method = "GEE", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT", corstr = "independence")
  expect_equal(result$corstr, "independence")
  expect_equal(result$model_type, "it_GEE")
})

