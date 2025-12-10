# Set advanced parameters for analysis of data from stepped wedge trials

This should be used in conjunction with
[`analyze`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
to set parameters controlling analysis; see examples.

## Usage

``` r
params(
  offset = NULL,
  n_knots_exp = 4,
  n_knots_cal = 4,
  var_est = "model",
  var_est_type = "classic",
  return_ncs = F
)
```

## Arguments

- offset:

  A linear predictor offset term; see docs for \`lme4::lmer\`.

- n_knots_exp:

  An integer; only relevant when exp_time="NCS". Specifies the number of
  knots to use for exposure time, including boundary knots. The spline
  basis includes an intercept, and the degree of the basis is equal to
  the number of knots.

- n_knots_cal:

  An integer; only relevant when cal_time="NCS". Specifies the number of
  knots to use for calendar time, including boundary knots. The spline
  basis includes an intercept, and the degree of the basis is equal to
  the number of knots.

- var_est:

  A character string; either "model", for model-based variance, or
  "robust", to use the robust variance estimator.

- var_est_type:

  A character string; one of c("classic","DF","KC","MD","FG"); only
  relevant when var_est="robust".

- return_ncs:

  Logical; only relevant when exp_time="NCS". Specifies whether the full
  covariance matrix for the calendar time parameters and the transformed
  treatment effect parameters are returned.

## Value

A list of options

## Examples

``` r
dat <- load_data(time = "period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_bin", data = sw_data_example)

analyze(dat = dat, method = "mixed", estimand_type = "TATE",  exp_time = "NCS",
family = binomial)
```
