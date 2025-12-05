# Analyze a stepped wedge dataset

Analyze a stepped wedge dataset

## Usage

``` r
analyze(
  dat,
  method = "mixed",
  estimand_type = "TATE",
  estimand_time = c(1, max(dat$exposure_time)),
  exp_time = "IT",
  cal_time = "categorical",
  family = stats::gaussian,
  exponentiate = FALSE,
  re = c("clust", "time"),
  corstr = "exchangeable",
  advanced = params()
)
```

## Arguments

- dat:

  A dataframe containing the stepped wedge trial data.

- method:

  A character string; either "mixed", for a mixed-effects model, or
  "GEE", for generalized estimating equations.

- estimand_type:

  One of c("TATE", "PTE"); "TATE" represents the time-averaged treatment
  effect and "PTE" represents the point treatment effect.

- estimand_time:

  An integer vector of length 1 or 2. When estimand_type="TATE",
  \`estimand_time\` must be a numeric vector of length 2, representing
  the start and end times of the exposure time period to average over.
  When estimand_type="PTE", \`estimand_time\` must be a numeric vector
  of length 1, representing the time period of interest. See examples.

- exp_time:

  One of c("IT", "ETI", "NCS", "TEH"); model for exposure time. "IT"
  encodes an immediate treatment model with a single treatment effect
  parameter. "ETI" is an exposure time indicator model, including one
  indicator variable for each exposure time point. "NCS" uses a natural
  cubic spline model for the exposure time trend. "TEH" includes a
  random slope term in the model, allowing the treatment effect to vary
  by timepoint.

- cal_time:

  One of c("categorical", "NCS", "linear", "none"); model for calendar
  time. "categorical" uses indicator variables for discrete time points,
  as in the Hussey and Hughes model. "NCS" uses a natural cubic spline,
  useful for datasets with continuous time. "linear" uses a single slope
  parameter. "none" assumes that there is no underlying calendar time
  trend.

- family:

  A family object; see documentation for \`glm\`.

- exponentiate:

  Logical; if TRUE, return exponentiated treatment effect estimates and
  confidence intervals (including in the \`effect_curve\` object).
  Defaults to FALSE.

- re:

  A character vector of random effects to include; only relevant if
  method="mixed" is used. Possible random effects include "clust"
  (random intercept for cluster), "time" (random intercept for
  cluster-time interaction), "ind" (random intercept for individuals;
  appropriate when a cohort design is used), "tx" (random treatment
  effect)

- corstr:

  One of c("independence", "exchangeable", "ar1"); only relevant if
  method="GEE" is used. Defines the GEE working correlation structure;
  see the documentation for \`geepack::geeglm\`.

- advanced:

  A list of options returned by
  [`params`](https://avi-kenny.github.io/steppedwedge/reference/params.md).

## Value

A list with the model object, model type as a string, estimand type as a
string, numeric treatment effect estimate, numeric treatment effect
standard error, treatment effect 95 a list with treatment effect
estimates (and standard errors and 95 at each exposure timepoint, the
original dataframe passed to \`analyze()\`, and an indicator whether the
effect estimates and CI are exponentiated.

## Examples

``` r
# Load data
test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", data = sw_data_example)

# Analysis example 1: TATE estimand for exposure times 1 through 4
results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "ETI")

results_tate

# Analysis example 2: PTE estimand for exposure time 3
results_pte <- analyze(dat = test_data, method = "mixed", estimand_type = "PTE",
estimand_time = 3, exp_time = "ETI")

results_pte

# Analysis example 3: TATE estimand for exposure times 1 through 4, Natural Cubic Splines model
results_tate_ncs <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
estimand_time = c(1, 4), exp_time = "NCS", advanced = params(n_knots_exp = 4))

results_tate_ncs

# Analysis example 4: TATE estimand for exposure times 1 through 4 with binomial outcome data
# Load data
test_data_bin <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = c("numerator", "denominator"), data = sw_data_example)

results_pte_bin <- analyze(dat = test_data_bin, family = binomial, method = "mixed", 
estimand_type = "TATE", estimand_time = c(1, 4), exp_time = "ETI")

results_pte_bin
```
