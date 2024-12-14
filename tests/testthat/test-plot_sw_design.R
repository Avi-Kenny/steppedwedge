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
  period = "period",
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

# Returns correct number of clusters

test_that("plot_design calculates correct number of clusters", {
  result <- plot_design(sw_data)
  expect_equal(result$num_clusters, length(unique(sample_data$cluster_id)))
})

# Returns correct number of periods

test_that("plot_design calculates correct number of periods", {
  result <- plot_design(sw_data)
  expect_equal(result$num_periods, length(unique(sample_data$period)))
})

# Returns correct number of sequences

test_that("plot_design calculates correct number of sequences", {
  result <- plot_design(sw_data)
  expect_equal(result$num_sequences, attr(sw_data, "n_sequences"))
})

# Returns correct summary

test_that("plot_design returns correct summary", {
  result <- plot_design(sw_data)
  expected_summary <- summary(sw_data %>%
                                data.frame() %>%
                                dplyr::select(cluster_id, period, treatment) %>%
                                dplyr::group_by(cluster_id, period) %>%
                                dplyr::mutate(n = dplyr::n()) %>%
                                dplyr::distinct())
  expect_equal(result$summary, expected_summary)
})


