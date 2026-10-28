

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
  expect_warning(load_data(
    time = "period",
    cluster_id = "cluster_id",
    individual_id = "individual_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = sample_data_multiple_treatment_values
  ), "The value of the `treatment` variable is not the same for all observations in each cluster-period.")
})



# Covariates ------------------------------------------------------------------

sample_data_cov <- sample_data
sample_data_cov$age <- seq(30, 64, by = 2)
sample_data_cov$sex <- factor(rep(c("F", "M"), 9), levels = c("F", "M", "X"))
sample_data_cov$site <- paste0("site_", sample_data_cov$cluster_id)
sample_data_cov$period_label <- paste0("p", sample_data_cov$period)
sample_data_cov$enrolled <- as.Date("2024-01-01") + seq_len(18)
sample_data_cov$exp_1 <- 1

load_cov <- function(covariates, data = sample_data_cov) {
  load_data(
    time = "period",
    cluster_id = "cluster_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = data,
    covariates = covariates
  )
}

test_that("load_data carries covariates through", {
  result <- suppressMessages(load_cov(c("age", "sex")))

  expect_equal(attr(result, "covariates"), c("age", "sex"))
  expect_equal(result$age, sample_data_cov$age)
  expect_s3_class(result$sex, "factor")
  expect_equal(levels(result$sex), c("F", "M"))
})

test_that("load_data without covariates sets no covariate attribute", {
  result <- suppressMessages(load_cov(NULL))
  expect_null(attr(result, "covariates"))
  expect_false("age" %in% names(result))
})

test_that("load_data drops rows with missing covariates", {
  data_missing <- sample_data_cov
  data_missing$age[c(1, 5)] <- NA

  expect_snapshot(result <- load_cov("age", data = data_missing))
  expect_equal(nrow(result), 16)
  expect_equal(sum(is.na(result$age)), 0)
})

test_that("load_data validates covariates", {
  expect_snapshot(load_cov(1), error = TRUE)
  expect_snapshot(load_cov(c("age", "age")), error = TRUE)
  expect_snapshot(load_cov(c("age", "bmi")), error = TRUE)
  expect_snapshot(load_cov("period"), error = TRUE)
  expect_snapshot(load_cov("exp_1"), error = TRUE)
  expect_snapshot(load_cov("enrolled"), error = TRUE)
})

test_that("load_data rejects categorical relabellings of cluster or time", {
  expect_snapshot(load_cov("site"), error = TRUE)
  expect_snapshot(load_cov("period_label"), error = TRUE)
})

test_that("load_data rejects reserved covariate names", {
  data_reserved <- sample_data_cov
  data_reserved$time <- 1
  expect_snapshot(load_cov("time", data = data_reserved), error = TRUE)
})

# Offset ----------------------------------------------------------------------

test_that("load_data stores the offset and drops rows with a missing offset", {
  data_offset <- sample_data
  data_offset$log_n <- log(seq_len(18))
  data_offset$log_n[3] <- NA

  expect_snapshot(
    result <- load_data(
      time = "period", cluster_id = "cluster_id", treatment = "treatment",
      outcome = "y_bin", offset = "log_n", data = data_offset
    )
  )
  expect_equal(nrow(result), 17)
  expect_equal(result$offset, data_offset$log_n[-3])
})

test_that("load_data requires a numeric offset", {
  data_offset <- sample_data
  data_offset$log_n <- "a"

  expect_snapshot(
    load_data(
      time = "period", cluster_id = "cluster_id", treatment = "treatment",
      outcome = "y_bin", offset = "log_n", data = data_offset
    ),
    error = TRUE
  )
})


# Validation branches ---------------------------------------------------------

sample_data_val <- sample_data
sample_data_val$period_chr <- as.character(sample_data_val$period)
sample_data_val$expo_chr <- as.character(sample_data_val$period)
sample_data_val$expo_num <- as.numeric(sample_data_val$treatment) * 2
sample_data_val$y_lgl <- as.logical(sample_data_val$y_bin)
sample_data_val$trt_lgl <- as.logical(sample_data_val$treatment)
sample_data_val$succ <- sample_data_val$y_bin
sample_data_val$succ_chr <- as.character(sample_data_val$y_bin)
sample_data_val$succ_big <- 2
sample_data_val$trials <- 1

load_val <- function(data = sample_data_val, ...) {
  suppressMessages(load_data(
    time = "period",
    cluster_id = "cluster_id",
    treatment = "treatment",
    outcome = "y_bin",
    data = data,
    ...
  ))
}

test_that("load_data rejects tibbles and empty data frames", {
  expect_snapshot(load_val(data = tibble::as_tibble(sample_data_val)), error = TRUE)
  expect_snapshot(load_val(data = sample_data_val[0, ]), error = TRUE)
})

test_that("load_data validates individual_id, offset, and exposure_time names", {
  expect_snapshot(load_val(individual_id = "nope"), error = TRUE)
  expect_snapshot(load_val(offset = "nope"), error = TRUE)
  expect_snapshot(load_val(exposure_time = "nope"), error = TRUE)
})

test_that("load_data validates outcome, successes, and trials names", {
  expect_snapshot(
    load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
              outcome = "nope", data = sample_data_val),
    error = TRUE
  )
  expect_snapshot(
    load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
              outcome = c("succ", "nope"), data = sample_data_val),
    error = TRUE
  )
})

test_that("load_data checks types of time, exposure_time, and binomial columns", {
  expect_snapshot(
    load_data(time = "period_chr", cluster_id = "cluster_id", treatment = "treatment",
              outcome = "y_bin", data = sample_data_val),
    error = TRUE
  )
  expect_snapshot(load_val(exposure_time = "expo_chr"), error = TRUE)
  expect_snapshot(
    load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
              outcome = c("succ_chr", "trials"), data = sample_data_val),
    error = TRUE
  )
  expect_snapshot(
    load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
              outcome = c("succ_big", "trials"), data = sample_data_val),
    error = TRUE
  )
})

test_that("load_data converts logical outcome and treatment to integers", {
  res <- suppressMessages(load_data(
    time = "period", cluster_id = "cluster_id", treatment = "trt_lgl",
    outcome = "y_lgl", data = sample_data_val
  ))
  expect_type(res$outcome, "integer")
  expect_type(res$treatment, "integer")
})

test_that("load_data uses a supplied exposure_time column", {
  res <- load_val(exposure_time = "expo_num")
  expect_equal(res$exposure_time, sample_data_val$expo_num)
})
