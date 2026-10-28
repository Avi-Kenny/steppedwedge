data(sw_data_example)

dat_cont <- suppressMessages(load_data(
  time = "period", cluster_id = "cluster", treatment = "trt",
  outcome = "outcome_cont", data = sw_data_example
))
dat_bin <- suppressMessages(load_data(
  time = "period", cluster_id = "cluster", treatment = "trt",
  outcome = "outcome_bin", data = sw_data_example
))

fit_quiet <- function(...) suppressMessages(suppressWarnings(analyze(...)))

it_mod <- fit_quiet(dat_cont, method = "mixed", estimand_type = "TATE",
                    estimand_time = c(1, 4), exp_time = "IT")
eti_mod <- fit_quiet(dat_cont, method = "mixed", estimand_type = "TATE",
                     estimand_time = c(1, 4), exp_time = "ETI")
ncs_mod <- fit_quiet(dat_cont, method = "mixed", estimand_type = "TATE",
                     estimand_time = c(1, 4), exp_time = "NCS",
                     advanced = params(n_knots_exp = 4))

test_that("plot_effect_curves() returns a ggplot with one curve per model", {
  p <- plot_effect_curves(it_mod, eti_mod)
  expect_s3_class(p, "ggplot")
  expect_equal(sort(unique(p$data$curve)), c("eti_mod", "it_mod"))
  expect_equal(unique(p$data$y_ref), 0)
})

test_that("custom labels replace the default curve labels", {
  p <- plot_effect_curves(it_mod, eti_mod, labels = c("IT", "ETI"))
  expect_equal(unique(p$data$curve), c("IT", "ETI"))
})

test_that("discrete models are unchanged when continuous = TRUE", {
  p <- plot_effect_curves(eti_mod, continuous = TRUE)
  expect_equal(p$data$x, eti_mod$effect_curve$exp_time)
  expect_equal(p$data$y, eti_mod$effect_curve$est)
})

test_that("continuous NCS curve matches the spline estimates at integer times", {
  p <- plot_effect_curves(ncs_mod, continuous = TRUE, resolution = 1)
  expect_equal(p$data$x, ncs_mod$effect_curve$exp_time)
  expect_equal(p$data$y, ncs_mod$effect_curve$est, tolerance = 1e-8)
  expect_equal(p$data$ci_lower, ncs_mod$effect_curve$ci_lower, tolerance = 1e-6)
})

test_that("continuous NCS curve starts at zero and uses the requested grid", {
  p <- plot_effect_curves(ncs_mod, continuous = TRUE, resolution = 0.5)
  S <- max(dat_cont$exposure_time)
  expect_equal(p$data$x[1], 0)
  expect_equal(p$data$y[1], 0)
  expect_length(p$data$x, 1 + length(seq(1, S, by = 0.5)))
})

test_that("continuous NCS curve falls back when knots or covariance are missing", {
  p1 <- plot_effect_curves(ncs_mod, continuous = TRUE, resolution = 1)

  ncs_no_knots <- ncs_mod
  ncs_no_knots$knots_exp <- NULL
  p2 <- plot_effect_curves(ncs_no_knots, continuous = TRUE, resolution = 1)
  expect_equal(p2$data$y, p1$data$y, tolerance = 1e-8)

  ncs_no_cov <- ncs_mod
  ncs_no_cov$V_orig <- NULL
  p3 <- plot_effect_curves(ncs_no_cov, continuous = TRUE, resolution = 1)
  expect_equal(p3$data$ci_upper, p1$data$ci_upper, tolerance = 1e-6)
})

test_that("exponentiated continuous NCS curve is positive with reference line at 1", {
  ncs_bin <- fit_quiet(dat_bin, family = binomial, method = "mixed",
                       estimand_type = "TATE", estimand_time = c(1, 4),
                       exp_time = "NCS", exponentiate = TRUE)
  p <- plot_effect_curves(ncs_bin, continuous = TRUE, resolution = 1)
  expect_true(all(p$data$y > 0))
  expect_equal(p$data$y[1], 1)
  expect_equal(unique(p$data$y_ref), 1)
  expect_equal(p$data$y[-1], ncs_bin$effect_curve$est[-1], tolerance = 1e-8)
})

test_that("plot_effect_curves() requires sw_analysis objects", {
  expect_snapshot(plot_effect_curves(), error = TRUE)
  expect_snapshot(plot_effect_curves(data.frame()), error = TRUE)
})

test_that("as_table() stacks effect curves with labels", {
  tbl <- as_table(it_mod, eti_mod, labels = c("IT", "ETI"))
  expect_equal(nrow(tbl),
               length(it_mod$effect_curve$est) + length(eti_mod$effect_curve$est))
  expect_equal(unique(tbl$curve), c("IT", "ETI"))
  expect_equal(unique(tbl$y_ref), 0)
})

test_that("as_table() requires sw_analysis objects", {
  expect_snapshot(as_table(data.frame()), error = TRUE)
})
