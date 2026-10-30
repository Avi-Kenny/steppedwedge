# Changelog

## steppedwedge 1.1.0

#### Major changes

- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports the Delayed Constant Treatment (DCT) model.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports cluster-level random treatment effects via `re = "tx"`.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  can adjusts for covariates passed via `load_data(covariates = )`.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports robust variance estimation via `params(var_est = "robust")`.
- [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  accepts an `exposure_time` variable (in addition to automatically
  calculating exposure time when only a binary `treatment` variable is
  passed).
- [`plot_effect_curves()`](https://avi-kenny.github.io/steppedwedge/reference/plot_effect_curves.md)
  visualizes estimated treatment effect curves.
- [`plot_clusters()`](https://avi-kenny.github.io/steppedwedge/reference/plot_clusters.md)
  visualizes cluster-level data or fitted analysis objects.

#### Minor changes

- The `offset` argument of
  [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  has moved to
  [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md).
  Passing `offset` through
  [`params()`](https://avi-kenny.github.io/steppedwedge/reference/params.md)
  is deprecated.
- [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  now stores `time_type` as an attribute rather than a dataframe column.
  Access it with `attr(data, "time_type")`.
- `params(return_ncs=TRUE)` returns the natural cubic spline
  transformation matrix and the transformed coefficients and covariance
  matrices.
- Documentation and vignettes include examples for binary outcomes, DCT
  models, and cluster plots.
- The binomial outcome dataset is now `sw_data_example_binom`.
- For spline-based model components, the number of knots can be
  configured via
  [`params()`](https://avi-kenny.github.io/steppedwedge/reference/params.md),
  including the exposure time spline of the NCS model (`n_knots_exp`)
  and the calendar time trend spline (`n_knots_cal`).
- Fix to how `estimand_time` is coded in the TEH model.
- An informative error is thrown if the user tries to use a robust
  variance estimator with a TEH model.
- [`print()`](https://rdrr.io/r/base/print.html) formats and displays
  estimates, standard errors, and p-values for analysis objects.

## steppedwedge 1.0.0

CRAN release: 2025-02-21

- Initial CRAN submission.
