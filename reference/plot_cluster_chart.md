# Plot cluster chart

Plot cluster chart

## Usage

``` r
plot_cluster_chart(analysis_object)
```

## Arguments

- analysis_object:

  A list of class \`sw_analysis\`.

## Value

A list with a plot of the actual and predicted outcomes by cluster.

## Examples

``` r
# Load data
test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", data = sw_data_example)

# Analyze using TATE estimand for exposure times 1 through 4
results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "ETI")

# Plot by cluster
plot_cluster_chart(results_tate)
```
