# Create table of estimates

Format estimates returned by
[`analyze`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
as a table

## Usage

``` r
as_table(..., labels = NA)
```

## Arguments

- ...:

  One or more objects of class `"sw_analysis"` returned by
  [`analyze`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md).

- labels:

  A character vector of length equal to length(list(...)) representing
  curve labels

## Value

A table of effect estimate values

## Examples

``` r
# Load data
test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", data = sw_data_example)

# \donttest{
IT_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "IT")
ETI_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "ETI")
NCS_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "NCS")
ests_table <- as_table(IT_model, IT_model, NCS_model)
head(ests_table)
# }
```
