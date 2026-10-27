# load_data drops rows with missing covariates

    Code
      result <- load_cov("age", data = data_missing)
    Message
      Stepped wedge dataset loaded. Discrete time design with 2 clusters, 2 sequences, and 3 time points. Covariates: age. Note: 2/18 rows were dropped due to missing values for `cluster_id`, `time`, `treatment`, `outcome`, or covariates.

# load_data validates covariates

    Code
      load_cov(1)
    Condition
      Error in `validate_covariates()`:
      ! `covariates` must be a character vector of variable names in `data`.

---

    Code
      load_cov(c("age", "age"))
    Condition
      Error in `validate_covariates()`:
      ! `covariates` must not contain duplicate names.

---

    Code
      load_cov(c("age", "bmi"))
    Condition
      Error in `validate_covariates()`:
      ! The following `covariates` are not variables in `data`: `bmi`.

---

    Code
      load_cov("period")
    Condition
      Error in `validate_covariates()`:
      ! The following `covariates` are already used as design variables (time, cluster, individual, treatment, outcome, exposure time, or offset) and cannot also be covariates: `period`.

---

    Code
      load_cov("exp_1")
    Condition
      Error in `validate_covariates()`:
      ! The following `covariates` names are reserved for internal use; please rename them in `data`: `exp_1`.

---

    Code
      load_cov("enrolled")
    Condition
      Error in `validate_covariates()`:
      ! `covariates` must be numeric, logical, factor, or character variables. Invalid: `enrolled`.

# load_data rejects categorical relabellings of cluster or time

    Code
      load_cov("site")
    Condition
      Error in `check_covariate_aliasing()`:
      ! Covariate `site` is a one-to-one relabelling of the cluster identifier. Including it would add cluster fixed effects to the mean model, which is not supported; between-cluster variation is handled by random effects (method = "mixed") or the working correlation structure (method = "GEE").

---

    Code
      load_cov("period_label")
    Condition
      Error in `check_covariate_aliasing()`:
      ! Covariate `period_label` is a one-to-one relabelling of the time variable. Calendar time is modelled via the `cal_time` argument of analyze().

# load_data rejects reserved covariate names

    Code
      load_cov("time", data = data_reserved)
    Condition
      Error in `validate_covariates()`:
      ! The following `covariates` names are reserved for internal use; please rename them in `data`: `time`.

# load_data stores the offset and drops rows with a missing offset

    Code
      result <- load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
        outcome = "y_bin", offset = "log_n", data = data_offset)
    Message
      Stepped wedge dataset loaded. Discrete time design with 2 clusters, 2 sequences, and 3 time points. Note: 1/18 rows were dropped due to missing values for `cluster_id`, `time`, `treatment`, `outcome`, or `offset`.

# load_data requires a numeric offset

    Code
      load_data(time = "period", cluster_id = "cluster_id", treatment = "treatment",
        outcome = "y_bin", offset = "log_n", data = data_offset)
    Condition
      Error in `load_data()`:
      ! `offset` must be numeric.

