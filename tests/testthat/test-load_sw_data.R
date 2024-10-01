

# Sample data for testing
sample_data <- data.frame(
  period = c(1, 2, 3, 1, 2, 3),
  id = c(2, 2, 2, 1, 1, 1),
  treatment = c(0, 1, 1, 0, 0, 1),
  y_bin = c(0, 1, 0, 1, 0, 1)
)

# Test when everything is correct
test_that("load_sw_data works correctly", {
  result <- load_sw_data(
    period = "period",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  )

  # Check class of the result
  expect_s3_class(result, "sw_dat")

  # Check attributes
  expect_equal(attr(result, "n_clusters"), 2)
  expect_equal(attr(result, "n_periods"), 3)
  expect_equal(attr(result, "n_sequences"), 2)

  # Check data content
  expect_equal(nrow(result), 6)
  expect_equal(result$period, sample_data$period)
  expect_equal(result$treatment, sample_data$treatment)
  expect_equal(result$outcome, sample_data$y_bin)
})


# Unit test for load_sw_data function with incorrect inputs
test_that("load_sw_data handles incorrect inputs correctly", {
  # Incorrect data type for `data`
  expect_error(load_sw_data(
    period = "period",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = list()
  ), "`data` must be a data frame.")

  # Incorrect `time_type`
  expect_error(load_sw_data(
    period = "period",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data,
    time_type = "invalid"
  ), "`time_type` must be a character string specifying `discrete` or `continuous`.")

  # Non-existent column names
  expect_error(load_sw_data(
    period = "nonexistent",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`period` must be a character string specifying a single variable in `data`.")

  # Incorrect type for `treatment`
  sample_data_invalid_treatment <- sample_data
  sample_data_invalid_treatment$treatment <- c("A", "B", "C", "D", "E", "F")
  expect_error(load_sw_data(
    period = "period",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_invalid_treatment
  ), "`treatment` must only contain binary values \\(either T/F or 1/0\\).")

  # Incorrect type for `outcome`
  sample_data_invalid_outcome <- sample_data
  sample_data_invalid_outcome$y_bin <- c("A", "B", "C", "D", "E", "F")
  expect_error(load_sw_data(
    period = "period",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_invalid_outcome
  ), "`outcome` must only contain numeric or binary values \\(either T/F or 1/0\\).")

  # Edge case: Empty string for `period`
  expect_error(load_sw_data(
    period = "",
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`period` must be a character string specifying a single variable in `data`.")

  # Edge case: NULL for `period`
  expect_error(load_sw_data(
    period = NULL,
    cluster_id = "id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data
  ), "`period` must be a character string specifying a single variable in `data`.")

})
