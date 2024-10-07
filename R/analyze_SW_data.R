analyze_SW_data <- function(dat, outcome_type, method) {
  
  df_results <- data.frame()
  
  ##########################################.
  ##### Immediate Treatment (IT) model #####
  ##########################################.
  
  # Fit mixed model
  model_it_mixed <- lmer(
    outcome ~ factor(period) + treatment + (1|cluster_id),
    data = dat
  )
  summary(model_it_mixed)
  
  # Extract an estimate and confidence interval for the estimated treatment
  #     effect; recall that the TATE estimator for any interval and the PTE/LTE
  #     estimators are all equivalent when using the immediate treatment model
  te_est <- summary(model_it_mixed)$coefficients["treatment",1]
  te_se <- summary(model_it_mixed)$coefficients["treatment",2]
  te_ci_lower <- te_est + c(-1.96) * te_se
  te_ci_upper <- te_est + c(1.96) * te_se
  
  new_row <- data.frame(
    model = "it_mixed",
    estimand = "TATE/LTE",
    te_est = te_est,
    te_se = te_se,
    te_ci_lower = te_ci_lower,
    te_ci_upper = te_ci_upper
  )
  
  ###############################################.
  ##### Exposure Time Indicator (ETI) model #####
  ###############################################.
  
  # Fit mixed model
  model_eti_mixed <- lmer(
    outcome ~ factor(period) + factor(exposure_time) + (1|cluster_id),
    data = dat
  )
  summary(model)
  
  ####### Resume here ############
  
  # Specify the indices corresponding to the exposure time variables
  indices <- c(8:13)
  
  # Extract coefficient estimates and covariance matrix corresponding to exposure
  #     time variables
  coeffs <- summary(model)$coefficients[,1][indices]
  cov_mtx <- vcov(model)[indices,indices]
  
  # Estimate the TATE over the interval [0,6]
  tate_est <- mean(coeffs)
  tate_se <- sqrt(mean(cov_mtx))
  tate_ci <- tate_est + c(-1.96,1.96) * tate_se
  
  # Estimate the TATE over the interval [0,6] (equivalent calculation using
  #     matrix multiplication)
  M <- matrix(rep(1/6, 6), nrow=1)
  tate_est <- (M %*% coeffs)[1]
  tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
  tate_ci <- tate_est + c(-1.96,1.96) * tate_se
  
  # Estimate the LTE
  lte_est <- as.numeric(coeffs[6])
  lte_se <- sqrt(cov_mtx[6,6])
  lte_ci <- lte_est + c(-1.96,1.96) * lte_se
  
  # Estimate the effect curve
  curve_eti <- as.numeric(c(0, coeffs))
  
  return(new_row)
  
  # # Display results
  # display_results("TATE/LTE", te_est, te_ci)

}
