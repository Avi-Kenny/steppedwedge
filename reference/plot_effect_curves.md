# Plot effect estimates by exposure time for one or more models.

Plot effect estimates by exposure time for one or more models.

## Usage

``` r
plot_effect_curves(
  ...,
  labels = NA,
  facet_nrow = 1,
  continuous = FALSE,
  resolution = 0.1
)
```

## Arguments

- ...:

  One or more objects of class `"sw_analysis"` returned by
  [`analyze`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md).

- labels:

  A character vector of length equal to the length of list(...),
  representing plot labels. Only used if length(list(...))\>1.

- facet_nrow:

  Number of rows for displaying plots using ggplot2::facet_wrap().

- continuous:

  Logical; if TRUE, models fit with Natural Cubic Splines (exp_time =
  "NCS") will be evaluated and plotted as smooth, continuous curves.
  Models using discrete exposure times (e.g., ETI, DCT) will remain
  discrete.

- resolution:

  Numeric; the step size for generating the continuous curve. Defaults
  to 0.1. Only relevant if continuous = TRUE.

## Value

A plot of the effect curve for each `"sw_analysis"` object passed to the
function.

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
NCS_4_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "NCS", advanced = params(n_knots_exp = 4))

# Plot discrete models
plot_effect_curves(IT_model, NCS_4_model, ETI_model, facet_nrow = 1)

# Plot continuous NCS model alongside discrete models
plot_effect_curves(IT_model, NCS_4_model, ETI_model, facet_nrow = 1, continuous = TRUE)
# }
```
