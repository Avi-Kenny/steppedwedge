# steppedwedge 1.1.0

## Breaking changes

* The `offset` argument of `analyze()` has moved to `load_data()`. Passing
  `offset` through `params()` is deprecated.
* `load_data()` stores `time_type` as an attribute rather than a data frame
  column. Access it with `attr(data, "time_type")`.

## New features

* `analyze()` supports Delayed Constant Treatment (DCT) models.
* `analyze()` supports random treatment effects across clusters via `re = "tx"`.
* `analyze()` adjusts for covariates passed from `load_data(covariates = )`.
* `analyze()` supports robust variance estimation via
  `params(var_est = "robust")`.
* `analyze()` reports p-values and, with `exponentiate = TRUE`, exponentiated
  estimates.
* `load_data()` accepts an `exposure_time` variable.
* `plot_effect_curves()` visualizes estimated treatment effect curves.
* `plot_clusters()` visualizes cluster-level data or fitted analysis objects.
* `params(return_ncs = TRUE)` returns the natural cubic spline transformation
  matrix and the transformed coefficients and covariance matrices.

## Minor improvements and bug fixes

* Documentation and vignettes include examples for binary outcomes, DCT models,
  and cluster plots.
* The binomial outcome dataset is now `sw_data_example_binom`.
* `params()` gains `n_knots_exp` and `n_knots_cal` to configure spline knots.
* TEH models correctly code `estimand_time`.
* Combining robust variance estimation with TEH models gives an informative
  error.
* `print()` formats and displays estimates, standard errors, and p-values for
  analysis objects.
