# Plot stepped wedge design

Plot stepped wedge design

## Usage

``` r
plot_design(dat)
```

## Arguments

- dat:

  A dataframe containing the stepped wedge trial data.

## Value

A list with a plot of the stepped wedge design.

## Examples

``` r
# Load data
example_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
# Plot design
plot_design(example_data)
```
