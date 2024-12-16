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

test_that("plot_design raises error for incorrect class", {
  expect_error(plot_design(data.frame()), "`dat` must be of class `sw_dat`.")
})




