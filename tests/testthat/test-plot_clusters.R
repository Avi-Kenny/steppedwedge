# Setup -----------------------------------------------------------------------

set.seed(2278)

sample_data <- data.frame(cluster = 1:15) |>
  tidyr::expand(cluster, period = 1:6) |>
  dplyr::mutate(cp_size = sample(15:30, dplyr::n(), replace = TRUE)) |>
  tidyr::uncount(weights = cp_size) |>
  dplyr::mutate(
    seq        = ceiling(cluster / 3),
    trt        = ifelse(period > seq, 1, 0),
    outcome_bin  = rbinom(dplyr::n(), size = 1, prob = 0.2),
    outcome_cont = rnorm(dplyr::n(), mean = 0, sd = 1),
    denominator  = floor(runif(dplyr::n(), min = 5, max = 15))
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(numerator = rbinom(1, denominator, 0.2)) |>
  dplyr::ungroup() |>
  dplyr::select(-seq) |>
  data.frame()

sw_data_cont <- load_data(
  time = "period", cluster_id = "cluster", individual_id = NULL,
  treatment = "trt", outcome = "outcome_cont", data = sample_data
)

sw_data_bin <- load_data(
  time = "period", cluster_id = "cluster", individual_id = NULL,
  treatment = "trt", outcome = "outcome_bin", data = sample_data
)

sw_data_binom <- load_data(
  time = "period", cluster_id = "cluster", individual_id = NULL,
  treatment = "trt", outcome = c("numerator", "denominator"), data = sample_data
)

results_cont <- analyze(
  dat = sw_data_cont, method = "mixed", estimand_type = "TATE",
  estimand_time = c(1, 4), exp_time = "ETI"
)

results_bin <- analyze(
  dat = sw_data_bin, method = "mixed", estimand_type = "TATE",
  estimand_time = c(1, 4), exp_time = "ETI", family = "binomial"
)

results_binom <- analyze(
  dat = sw_data_binom, method = "mixed", estimand_type = "TATE",
  estimand_time = c(1, 4), exp_time = "ETI", family = "binomial"
)


# Input validation ------------------------------------------------------------

test_that("Error for non-sw_dat / non-sw_analysis input", {
  expect_error(plot_clusters(sample_data), "`object` must be of class `sw_analysis` or `sw_dat`.")
  expect_error(plot_clusters(list(a = 1, b = 2)), "`object` must be of class `sw_analysis` or `sw_dat`.")
  expect_error(plot_clusters("not an object"), "`object` must be of class `sw_analysis` or `sw_dat`.")
})


# Return structure ------------------------------------------------------------

test_that("Returns a named list with cluster_chart element", {
  result_data     <- plot_clusters(sw_data_cont)
  result_analysis <- plot_clusters(results_cont)

  expect_type(result_data, "list")
  expect_type(result_analysis, "list")
  expect_named(result_data, "cluster_chart")
  expect_named(result_analysis, "cluster_chart")
})

test_that("cluster_chart is a ggplot object", {
  expect_s3_class(plot_clusters(sw_data_cont)$cluster_chart, "ggplot")
  expect_s3_class(plot_clusters(sw_data_bin)$cluster_chart, "ggplot")
  expect_s3_class(plot_clusters(sw_data_binom)$cluster_chart, "ggplot")
  expect_s3_class(plot_clusters(results_cont)$cluster_chart, "ggplot")
})


# Data-only plots (sw_dat) ----------------------------------------------------

test_that("Data-only plot renders without error for continuous outcome", {
  expect_no_error(plot_clusters(sw_data_cont))
})

test_that("Data-only plot renders without error for binary outcome", {
  expect_no_error(plot_clusters(sw_data_bin))
})

test_that("Data-only plot renders without error for binomial (successes/trials) outcome", {
  expect_no_error(plot_clusters(sw_data_binom))
})

test_that("Data-only plot has exactly one layer (no prediction lines)", {
  p <- plot_clusters(sw_data_cont)$cluster_chart
  expect_length(p$layers, 1)

  p_bin <- plot_clusters(sw_data_bin)$cluster_chart
  expect_length(p_bin$layers, 1)

  p_binom <- plot_clusters(sw_data_binom)$cluster_chart
  expect_length(p_binom$layers, 1)
})


# Analysis object plots (sw_analysis) -----------------------------------------

test_that("Analysis plot renders without error for continuous, binary, and binomial outcomes", {
  expect_no_error(plot_clusters(results_cont))
  expect_no_error(plot_clusters(results_bin))
  expect_no_error(plot_clusters(results_binom))
})

test_that("Analysis plot has more layers than data-only plot (includes prediction lines)", {
  n_layers_data     <- length(plot_clusters(sw_data_cont)$cluster_chart$layers)
  n_layers_analysis <- length(plot_clusters(results_cont)$cluster_chart$layers)
  expect_gt(n_layers_analysis, n_layers_data)
})

test_that("Analysis plot includes a geom_line layer for predictions", {
  p <- plot_clusters(results_cont)$cluster_chart
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
})

test_that("Analysis plot for binary outcome includes caption", {
  p <- plot_clusters(results_bin)$cluster_chart
  expect_false(is.null(p$labels$caption))
  expect_gt(nchar(p$labels$caption), 0)
})

test_that("Analysis plot for binomial (successes/trials) outcome includes caption", {
  p <- plot_clusters(results_binom)$cluster_chart
  expect_false(is.null(p$labels$caption))
  expect_gt(nchar(p$labels$caption), 0)
})

test_that("Analysis plot for continuous outcome does not include caption", {
  p <- plot_clusters(results_cont)$cluster_chart
  expect_true(is.null(p$labels$caption))
})


# ncol parameter --------------------------------------------------------------

test_that("ncol parameter is passed to facet_wrap", {
  p_3 <- plot_clusters(sw_data_cont, ncol = 3)$cluster_chart
  p_5 <- plot_clusters(sw_data_cont, ncol = 5)$cluster_chart

  expect_equal(p_3$facet$params$ncol, 3)
  expect_equal(p_5$facet$params$ncol, 5)
})
