
simulate_sw_mixed <- function(n_clusters = 20,
                              n_times = 6,
                              n_ind = 10,       # Individuals per cluster-period
                              beta_0 = 10,      # Global intercept
                              beta_t = 0.5,     # Fixed time trend
                              beta_tx = 2.0,    # Average Treatment Effect
                              sigma_b_int = 3,  # SD of Cluster Random Intercept
                              sigma_b_slp = 1.5,# SD of Cluster Random Slope (Tx effect)
                              rho = 0.5,        # Correlation between Int and Slope
                              sigma_e = 1.0) {  # Residual error
  
  # 1. Generate Correlated Random Effects
  # Covariance Matrix: [[Var_int, Cov], [Cov, Var_slp]]
  cov_int_slp <- rho * sigma_b_int * sigma_b_slp
  Sigma <- matrix(c(sigma_b_int^2, cov_int_slp,
                    cov_int_slp,   sigma_b_slp^2), nrow = 2)
  
  # Draw random effects for each cluster
  # Column 1 = Random Intercept (b_0j), Column 2 = Random Slope (b_1j)
  re <- mvrnorm(n = n_clusters, mu = c(0, 0), Sigma = Sigma)
  
  clusters <- data.frame(
    cluster_id = 1:n_clusters,
    rand_int = re[, 1],
    rand_slope = re[, 2],
    # Create a simple stepped wedge design: stagger start times
    # (Clusters switch to Tx roughly evenly across timepoints)
    start_time = sort(rep(2:n_times, length.out = n_clusters))
  )
  
  # 2. Expand Grid (Long Format)
  dat <- expand.grid(
    cluster_id = 1:n_clusters,
    time = 1:n_times,
    individual_id_sub = 1:n_ind
  )
  
  # 3. Merge Cluster Info & Calculate Outcome
  dat <- merge(dat, clusters, by = "cluster_id")
  
  # Create unique individual IDs
  dat$individual_id <- paste0(dat$cluster_id, "-", dat$time, "-", dat$individual_id_sub)
  
  # Assign Treatment Status (1 if time >= start_time, else 0)
  dat$treatment <- ifelse(dat$time >= dat$start_time, 1, 0)
  dat$exposure_time <- dat$time # Required for your analyze function defaults
  
  # Simulate Outcome
  # Y = (Fixed Int + RE Int) + (Fixed Time * Time) + (Fixed Tx + RE Tx)*Treatment + Error
  dat$outcome <- (beta_0 + dat$rand_int) + 
    (beta_t * dat$time) + 
    ((beta_tx + dat$rand_slope) * dat$treatment) + 
    rnorm(nrow(dat), 0, sigma_e)
  
  # 4. Add attributes to mimic 'sw_dat' class required by your analyze() function
  class(dat) <- c("sw_dat", "data.frame")
  attr(dat, "binomial") <- FALSE
  
  return(dat)
}


test_that("params() correctly passes re_correlated argument", {
  p_def <- params()
  expect_false(p_def$re_correlated)
  
  p_true <- params(re_correlated = TRUE)
  expect_true(p_true$re_correlated)
})

test_that("analyze() constructs correct formula for Correlated vs Uncorrelated REs", {
  # 1. Setup minimal data
  dat <- simulate_sw_mixed(n_clusters = 6, n_times = 3, n_ind = 2)
  
  # 2. Test Uncorrelated (Default) behavior: (1 + treatment || cluster_id)
  res_uncorr <- analyze(dat, re = c("clust", "tx"), 
                        advanced = params(re_correlated = FALSE))
  
  # Extract the model object from the list
  model_uncorr <- res_uncorr$model 
  
  f_uncorr_str <- as.character(formula(model_uncorr))[3]
  
  # Check for expanded terms or explicit double bar
  has_split_terms <- grepl("\\(1 \\| cluster_id\\)", f_uncorr_str) & 
    grepl("\\(0 \\+ treatment \\| cluster_id\\)", f_uncorr_str)
  has_double_bar <- grepl("\\|\\|", f_uncorr_str)
  
  expect_true(has_split_terms || has_double_bar, 
              info = "Formula should reflect uncorrelated random effects")
  
  
  # 3. Test Correlated behavior: (1 + treatment | cluster_id)
  res_corr <- analyze(dat, re = c("clust", "tx"), 
                      advanced = params(re_correlated = TRUE))
  
  model_corr <- res_corr$model # Extract model
  
  f_corr_str <- as.character(formula(model_corr))[3]
  
  # Expect the combined single term
  expect_true(grepl("\\(1 \\+ treatment \\| cluster_id\\)", f_corr_str),
              info = "Formula should contain correlated random intercept and slope")
})

test_that("analyze() handles partial random effects specifications", {
  dat <- simulate_sw_mixed(n_clusters = 6, n_times = 3, n_ind = 2)
  
  # Case A: Random Cluster Intercept Only
  res_clust <- analyze(dat, re = c("clust"))
  f_clust <- as.character(formula(res_clust$model))[3] # Extract model
  
  expect_true(grepl("\\(1 \\| cluster_id\\)", f_clust))
  expect_false(grepl("treatment \\|", f_clust))
  
  # Case B: Random Treatment Effect Only
  res_tx <- analyze(dat, re = c("tx"))
  f_tx <- as.character(formula(res_tx$model))[3] # Extract model
  
  expect_true(grepl("\\(0 \\+ treatment \\| cluster_id\\)", f_tx))
  expect_false(grepl("\\(1 \\| cluster_id\\)", f_tx))
})

test_that("analyze() throws error for invalid inputs", {
  dat <- simulate_sw_mixed(n_clusters = 6, n_times = 3, n_ind = 2)
  
  expect_error(analyze(dat, re = c("invalid_re")), 
               "Random effects must be a subset of")
  
  expect_error(analyze(data.frame(x=1), re = c("clust"), estimand_time = c(1,2)), 
               "`dat` must be of class `sw_dat`")
})
