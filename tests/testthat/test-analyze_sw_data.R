# Sample data for testing
sample_data <- data.frame(
  period = rep(c(1, 2, 3), each = 6),
  cluster_id = rep(c(1, 2), each = 3, times = 3),
  individual_id = rep(1:18),
  treatment = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1),
  y_bin = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
) %>%
  dplyr::arrange(cluster_id)

sw_data <- load_sw_data(
  period = "period",
  cluster_id = "cluster_id",
  individual_id = "individual_id",
  treatment = "treatment",
  outcome = "y_bin",
  data = sample_data
)

# Invalid input

test_that("Input validation works", {
  expect_error(analyze_sw_data(sample_data, "binary", "mixed", "TATE", "IT"), 
               "`dat` must be of class `sw_dat`.")
})

# Valid input



