

# Sample data for testing
sample_data <- data.frame(
  period = rep(c(1, 2, 3), each = 6),
  cluster_id = rep(c(1, 2), each = 3, times = 3),
  individual_id = rep(1:18),
  treatment = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1),
  y_bin = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
) %>%
  dplyr::arrange(cluster_id)


# Test when everything is correct
test_that("load_data works correctly", {
  result <- load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  )

  # Check class of the result
  expect_s3_class(result, "sw_dat")

  # Check attributes
  expect_equal(attr(result, "n_clusters"), 2)
  expect_equal(attr(result, "n_times"), 3)
  expect_equal(attr(result, "n_seq"), 2)

  # Check data content
  expect_equal(nrow(result), 18)
  expect_equal(result$time, sample_data$period)
  expect_equal(result$treatment, sample_data$treatment)
  expect_equal(result$outcome, sample_data$y_bin)
})

# Unit test for load_data function with incorrect inputs
test_that("load_data handles incorrect inputs correctly", {
  # Incorrect data type for `data`
  expect_error(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = list()
  ), "`data` must be a data frame.")

  # Incorrect `time_type`
  expect_error(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data,
    time_type = "invalid"
  ), "`time_type` must be a character string specifying `discrete` or `continuous`.")

  # Non-existent column names
  expect_error(load_data(
    time = "nonexistent",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`time` must be a character string specifying a single variable in `data`.")

  # Incorrect type for `treatment`
  sample_data_invalid_treatment <- sample_data
  sample_data_invalid_treatment$treatment <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R")
  expect_error(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_invalid_treatment
  ), "`treatment` must only contain binary values \\(either T/F or 1/0\\).")

  # Incorrect type for `outcome`
  sample_data_invalid_outcome <- sample_data
  sample_data_invalid_outcome$y_bin <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R")
  expect_error(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_invalid_outcome
  ), "`outcome` must only contain numeric or binary values \\(either T/F or 1/0\\).")

  # Empty string for `time`
  expect_error(load_data(
    time = "",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`time` must be a character string specifying a single variable in `data`.")

  # NULL for `time`
  expect_error(load_data(
    time = NULL,
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`time` must be a character string specifying a single variable in `data`.")

  # More than one value of treatment within a given combination of cluster_id and time
  sample_data_multiple_treatment_values <- sample_data
  sample_data_multiple_treatment_values[1, "treatment"] <- 1
  expect_error(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_multiple_treatment_values
  ), "Value of `treatment` variable must be the same for all observations in a given cluster-period.")
})

