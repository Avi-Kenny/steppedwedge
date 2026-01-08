# TATE using objects returned with return_ncs = TRUE, model-based variance

test_that("TATE consistency: Direct output vs. results calculated from using 
          `return_ncs = TRUE`", {
  
  # --- 1. SETUP ---
  data(sw_data_example)
  dat <- load_data(
    time = "period", cluster_id = "cluster", treatment = "trt", 
    outcome = "outcome_cont", data = sw_data_example
  )
  
  # Define TATE window
  t_start <- 1
  t_end <- 3
  
  # Run analysis
  res <- analyze(
    dat = dat, method = "mixed", estimand_type = "TATE",
    estimand_time = c(t_start, t_end), exp_time = "NCS",
    advanced = params(n_knots_exp = 3, return_ncs = TRUE)
  )
  
  # --- 2. METHOD A: Direct Output ---
  tate_est_direct <- res$te_est
  tate_se_direct  <- res$te_se
  
  # --- 3. METHOD B: Option 1 ---
  # Strategy B Names: T_mat, V_orig
  T_mat <- res$T_mat
  beta_orig <- lme4::fixef(res$model)
  V_orig <- res$V_orig
  
  # Transform
  beta_curve_1 <- as.numeric(T_mat %*% beta_orig)
  V_curve_1 <- T_mat %*% V_orig %*% t(T_mat)
  
  # Apply names
  names(beta_curve_1) <- rownames(T_mat)
  rownames(V_curve_1) <- rownames(T_mat)
  colnames(V_curve_1) <- rownames(T_mat)
  
  # Extract window (ExpTime_1 to ExpTime_3)
  target_names <- paste0("ExpTime_", t_start:t_end)
  est_window_1 <- beta_curve_1[target_names]
  var_window_1 <- V_curve_1[target_names, target_names]
  
  # Calculate TATE
  k <- length(target_names)
  tate_est_opt1 <- mean(est_window_1)
  tate_se_opt1 <- sqrt(sum(var_window_1) / k^2)
  
  # --- 4. METHOD C: Option 2 ---
  # Strategy B Names: beta_new, V_new
  beta_curve_2 <- res$beta_new
  V_curve_2 <- res$V_new
  
  # Extract window
  est_window_2 <- beta_curve_2[target_names]
  var_window_2 <- V_curve_2[target_names, target_names]
  
  # Calculate TATE
  tate_est_opt2 <- mean(est_window_2)
  tate_se_opt2 <- sqrt(sum(var_window_2) / k^2)
  
  # --- 5. COMPARISON ---
  # Use as.numeric to ignore name attributes during comparison
  expect_equal(as.numeric(tate_est_direct), as.numeric(tate_est_opt1), tolerance = 1e-8)
  expect_equal(as.numeric(tate_se_direct),  as.numeric(tate_se_opt1),  tolerance = 1e-8)
  expect_equal(as.numeric(tate_est_direct), as.numeric(tate_est_opt2), tolerance = 1e-8)
  expect_equal(as.numeric(tate_se_direct),  as.numeric(tate_se_opt2),  tolerance = 1e-8)
})


# PTE using objects returned with return_ncs = TRUE, robust variance

test_that("PTE consistency with robust variance: Direct output vs. results 
          calculated from using `return_ncs = TRUE`", {
  
  # --- 1. SETUP ---
  data(sw_data_example)
  dat <- load_data(
    time = "period", cluster_id = "cluster", treatment = "trt", 
    outcome = "outcome_cont", data = sw_data_example
  )
  
  t_pte <- 3
  
  # Run analysis with ROBUST variances
  res <- analyze(
    dat = dat, method = "mixed", estimand_type = "PTE",
    estimand_time = t_pte, exp_time = "NCS",
    advanced = params(
      n_knots_exp = 3, 
      return_ncs = TRUE,
      var_est = "robust" 
    )
  )
  
  # Verify robust variance is actually different from naive
  vcov_naive  <- as.matrix(stats::vcov(res$model))
  vcov_robust <- as.matrix(res$V_orig) # Strategy B name
  expect_false(isTRUE(all.equal(vcov_naive, vcov_robust)))
  
  # --- 2. METHOD A: Direct Output ---
  pte_est_direct <- res$te_est
  pte_se_direct  <- res$te_se
  
  # --- 3. METHOD B: Option 1 ---
  T_mat <- res$T_mat
  beta_orig <- lme4::fixef(res$model)
  V_orig <- res$V_orig # Use the robust source matrix!
  
  # Transform
  beta_curve_1 <- as.numeric(T_mat %*% beta_orig)
  V_curve_1 <- T_mat %*% V_orig %*% t(T_mat)
  
  # Apply names
  names(beta_curve_1) <- rownames(T_mat)
  rownames(V_curve_1) <- rownames(T_mat)
  colnames(V_curve_1) <- rownames(T_mat)
  
  target_name <- paste0("ExpTime_", t_pte)
  pte_est_opt1 <- beta_curve_1[target_name]
  pte_se_opt1 <- sqrt(V_curve_1[target_name, target_name])
  
  # --- 4. METHOD C: Option 2 ---
  beta_curve_2 <- res$beta_new
  V_curve_2 <- res$V_new
  
  pte_est_opt2 <- beta_curve_2[target_name]
  pte_se_opt2 <- sqrt(V_curve_2[target_name, target_name])
  
  # --- 5. COMPARISON ---
  expect_equal(as.numeric(pte_est_direct), as.numeric(pte_est_opt1), tolerance = 1e-8)
  expect_equal(as.numeric(pte_se_direct),  as.numeric(pte_se_opt1),  tolerance = 1e-8)
  expect_equal(as.numeric(pte_est_direct), as.numeric(pte_est_opt2), tolerance = 1e-8)
  expect_equal(as.numeric(pte_se_direct),  as.numeric(pte_se_opt2),  tolerance = 1e-8)
})
