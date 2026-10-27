# params(offset = ) is deprecated but still supported

    Code
      res_params <- analyze(sw_no_offset, method = "mixed", estimand_type = "TATE",
        estimand_time = c(1, 4), exp_time = "IT", family = poisson, re = "clust",
        advanced = params(offset = sw_offset$offset))
    Condition
      Warning:
      `params(offset = )` is deprecated; use `load_data(offset = )` instead. A vector passed to `params()` must already match the row order of the `load_data()` output, which sorts rows by cluster rollout order.

# analyze() rejects conflicting or misaligned offsets

    Code
      analyze(sw_offset, method = "mixed", exp_time = "IT", family = poisson, re = "clust",
        advanced = params(offset = sw_offset$offset))
    Condition
      Error in `analyze()`:
      ! An offset was specified in both `load_data()` and `params()`. Specify it only via `load_data(offset = )`.

---

    Code
      analyze(sw_no_offset, method = "mixed", exp_time = "IT", family = poisson, re = "clust",
        advanced = params(offset = 1:10))
    Condition
      Error in `analyze()`:
      ! `params(offset = )` has length 10 but the data has 2011 rows. Specify the offset via `load_data(offset = )` instead.

