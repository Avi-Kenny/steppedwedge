# Changelog

## steppedwedge 1.1.0

### Breaking changes

- The `offset` argument of
  [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  has moved to
  [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md).
  Passing `offset` through
  [`params()`](https://avi-kenny.github.io/steppedwedge/reference/params.md)
  is deprecated.
- [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  stores `time_type` as an attribute rather than a data frame column.
  Access it with `attr(data, "time_type")`.

### New features

- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports Delayed Constant Treatment (DCT) models.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports random treatment effects across clusters via `re = "tx"`.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  adjusts for covariates passed from `load_data(covariates = )`.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  supports robust variance estimation via `params(var_est = "robust")`.
- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  reports p-values and, with `exponentiate = TRUE`, exponentiated
  estimates.
- [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  accepts an `exposure_time` variable.
- [`plot_effect_curves()`](https://avi-kenny.github.io/steppedwedge/reference/plot_effect_curves.md)
  visualizes estimated treatment effect curves.
- [`plot_clusters()`](https://avi-kenny.github.io/steppedwedge/reference/plot_clusters.md)
  visualizes cluster-level data or fitted analysis objects.
- `params(return_ncs = TRUE)` returns the natural cubic spline
  transformation matrix and the transformed coefficients and covariance
  matrices.

### Minor improvements and bug fixes

- Documentation and vignettes include examples for binary outcomes, DCT
  models, and cluster plots.
- The binomial outcome dataset is now `sw_data_example_binom`.
- [`params()`](https://avi-kenny.github.io/steppedwedge/reference/params.md)
  gains `n_knots_exp` and `n_knots_cal` to configure spline knots.
- TEH models correctly code `estimand_time`.
- Combining robust variance estimation with TEH models gives an
  informative error.
- [`print()`](https://rdrr.io/r/base/print.html) formats and displays
  estimates, standard errors, and p-values for analysis objects.
