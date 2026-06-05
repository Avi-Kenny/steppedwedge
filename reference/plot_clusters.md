# Plot observed and predicted outcomes by cluster over time

Plot observed and predicted outcomes by cluster over time

## Usage

``` r
plot_clusters(object, ncol = 3)
```

## Arguments

- object:

  An analysis object of class \`sw_analysis\` or a dataset of class
  \`sw_data\`/\`sw_dat\`.

- ncol:

  Integer; number of columns in the faceted plot. Defaults to 3.

## Value

A list with a \`ggplot2\` object of the actual (and predicted, if
applicable) outcomes by cluster.

## Examples

``` r
# Load data
test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", data = sw_data_example)

# Plot just the dataset
plot_clusters(test_data)

# Analyze using TATE estimand for exposure times 1 through 4
results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "ETI")

# Plot analysis results (with predicted lines) by cluster
plot_clusters(results_tate)
```
