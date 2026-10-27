# Changelog

## steppedwedge (development version)

- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  now uses the offset supplied via `load_data(offset = )`, which was
  previously stored but ignored. Because the offset is stored as a
  column, it stays aligned with the rows after
  [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  drops incomplete rows and sorts clusters by rollout order.
  `params(offset = )` is deprecated: it now warns, and it errors if its
  length does not match the data or if an offset was also given to
  [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md).

- [`analyze()`](https://avi-kenny.github.io/steppedwedge/reference/analyze.md)
  now includes covariates supplied via `load_data(covariates = )` as
  additive main effects in the mean model for all mixed and GEE models.
  Exposure time indicator coefficients are now matched exactly, so
  covariates whose names contain `exp_` are no longer mistaken for
  treatment effects, and the `return_ncs = TRUE` transformation matrix
  is built by coefficient index rather than block position.

- [`load_data()`](https://avi-kenny.github.io/steppedwedge/reference/load_data.md)
  gains a `covariates` argument for individual- or cluster-level
  variables. Rows with missing covariate values are dropped and
  reported, and covariates that duplicate design variables, use reserved
  internal names, or are categorical relabellings of the cluster or time
  variable are rejected.

## steppedwedge 1.0.0

CRAN release: 2025-02-21

## steppedwedge 0.1.0

- Initial CRAN submission.
