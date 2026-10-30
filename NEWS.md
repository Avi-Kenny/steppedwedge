# steppedwedge 1.1.0

### Major changes

- `analyze()` supports the Delayed Constant Treatment (DCT) model.
- `analyze()` supports cluster-level random treatment effects via `re = "tx"`.
- `analyze()` can adjusts for covariates passed via `load_data(covariates = )`.
- `analyze()` supports robust variance estimation via `params(var_est = "robust")`.
- `load_data()` accepts an `exposure_time` variable (in addition to automatically calculating exposure time when only a binary `treatment` variable is passed).
- `plot_effect_curves()` visualizes estimated treatment effect curves.
- `plot_clusters()` visualizes cluster-level data or fitted analysis objects.

### Minor changes

- The `offset` argument of `analyze()` has moved to `load_data()`. Passing `offset` through `params()` is deprecated.
- `load_data()` now stores `time_type` as an attribute rather than a dataframe column. Access it with `attr(data, "time_type")`.
- `params(return_ncs=TRUE)` returns the natural cubic spline transformation matrix and the transformed coefficients and covariance matrices.
- Documentation and vignettes include examples for binary outcomes, DCT models, and cluster plots.
- The binomial outcome dataset is now `sw_data_example_binom`.
- For spline-based model components, the number of knots can be configured via `params()`, including the exposure time spline of the NCS model (`n_knots_exp`) and the calendar time trend spline (`n_knots_cal`).
- Fix to how `estimand_time` is coded in the TEH model.
- An informative error is thrown if the user tries to use a robust variance estimator with a TEH model.
- `print()` formats and displays estimates, standard errors, and p-values for analysis objects.

# steppedwedge 1.0.0

* Initial CRAN submission.
