#' Title
#'
#' @param dat A dataframe containing the stepped wedge trial data.
#' @param method A character string; either "mixed", for a mixed-effects model, or "GEE", for generalized estimating equations.
#' @param estimand A character string; either "TATE", for time-averaged treatment effect, or "LTE", for long-term treatment effect.
#' @param time_varying_assumption A character string; either "IT" for immediate treatment effect, "ETI", for Exposure time indicator, or "NCS", for Natural cubic spline.
#' @param family A character string; see documentation for `glm()`.
#' @param link A character string; see documentation for `glm()`.
#' @param corstr A character string; see documentation for `geepack::geeglm()`.
#'
#' @return A list with ___
#' @export
#'
#' @examples
#' # TO DO
analyze_sw_data <- function(dat, method, estimand, time_varying_assumption,
                            family, link, corstr = "exchangeable") {
  
  cluster_id <- NULL
  rm(cluster_id)
  
  ### Add input validation
  
  if (!methods::is(dat,"sw_dat")) { stop("`dat` must be of class `sw_dat`.") }
  
  
  
  # call appropriate family function with chosen link to create family object
  family_obj <- get(family)(link = link)
  

  if(method == "mixed" & estimand %in% c("TATE", "LTE") & time_varying_assumption == "IT") {
    
    ################################################.
    ##### Immediate Treatment (IT) mixed model #####
    ################################################.
    
    # Fit mixed model
    if(family == "gaussian" & link == "identity") {
      model_it_mixed <- lme4::lmer(
        outcome ~ factor(period) + treatment + (1|cluster_id),
        data = dat
      )
    } else {
      model_it_mixed <- lme4::glmer(
        outcome ~ factor(period) + treatment + (1|cluster_id),
        family = family_obj,
        data = dat
      )
    }
    
    summary_it <- summary(model_it_mixed)
    
    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE/LTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    te_se <- summary_it$coefficients["treatment",2]
    te_ci <- te_est + c(-1.96,1.96) * te_se
    # te_ci_lower <- te_est + c(-1.96) * te_se
    # te_ci_upper <- te_est + c(1.96) * te_se
    
    results <- list(
      model = model_it_mixed,
      model_type = "it_mixed",
      estimand = "TATE/LTE",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci
      # te_ci_lower = te_ci_lower,
      # te_ci_upper = te_ci_upper
    )
  } else if(method == "mixed" & time_varying_assumption == "ETI") {
  
    
    #####################################################.
    ##### Exposure Time Indicator (ETI) mixed model #####
    #####################################################.
    
    # Fit mixed model
    if(family == "gaussian" & link == "identity") {
      model_eti_mixed <- lme4::lmer(
        outcome ~ factor(period) + factor(exposure_time) + (1|cluster_id),
        data = dat
      )
    } else {
      model_eti_mixed <- lme4::glmer(
        outcome ~ factor(period) + factor(exposure_time) + (1|cluster_id),
        family = family_obj,
        data = dat
      )
    }
    
    summary_eti <- summary(model_eti_mixed)
    

    # Specify the indices of summary_eti corresponding to the exposure time variables
    indices <- grep("exposure_time", rownames(summary_eti$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to exposure
    #     time variables
    coeffs <- summary_eti$coefficients[,1][indices] # column 1 contains the estimates
    cov_mtx <- stats::vcov(model_eti_mixed)[indices,indices]
    
    if(estimand == "TATE") {

      # Estimate the TATE (using
      #     matrix multiplication)
      M <- matrix(rep(1/index_max), index_max, nrow=1)
      tate_est <- (M %*% coeffs)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      results <- list(
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci
      )
      
    } else if(estimand == "LTE") {
  
      # Estimate the LTE
      lte_est <- as.numeric(coeffs[index_max])
      lte_se <- sqrt(cov_mtx[index_max,index_max])
      lte_ci <- lte_est + c(-1.96,1.96) * lte_se
      
      results <- list(
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand = "LTE",
        te_est = lte_est,
        te_se = lte_se,
        te_ci = lte_ci
      )
      # 
      # # Estimate the effect curve
      # curve_eti <- as.numeric(c(0, coeffs))
      
    } 
    
  } else if(method == "mixed" & time_varying_assumption == "NCS") {
    
    
    ############################################.
    ##### Natural Cubic Spline (NCS) model #####
    ############################################.
    
    # Create the spline basis (4 degrees of freedom)
    J <- length(unique(dat$period))
    ns_basis <- splines::ns(c(0:(J-1)), knots=c((J-1)/4,(2*(J-1))/4,(3*(J-1))/4))
    dat$b1 <- ns_basis[dat$exposure_time+1,1]
    dat$b2 <- ns_basis[dat$exposure_time+1,2]
    dat$b3 <- ns_basis[dat$exposure_time+1,3]
    dat$b4 <- ns_basis[dat$exposure_time+1,4]
    
    # Fit mixed model
    if(family == "gaussian" & link == "identity") {
      model_ncs_mixed <- lme4::lmer(
        outcome ~ factor(period) + b1+b2+b3+b4 + (1|cluster_id),
        data = dat
      )
      # summary(model_ncs_mixed)
    }
    
    summary_ncs <- summary(model_ncs_mixed)
    
    # Specify the indices corresponding to the spline terms
    # indices <- c(8:11)
    indices <- grep("^b[0-9]+$", rownames(summary_ncs$coefficients))
    index_max <- length(indices)
    
    # Extract coefficient estimates and covariance matrix corresponding to spline
    #     terms
    coeffs_spl <- summary_ncs$coefficients[,1][indices]
    cov_mtx_spl <- stats::vcov(model_ncs_mixed)[indices,indices]
    
    # Transform the spline terms into effect curve estimates (+ covariance matrix)
    B <- matrix(NA, nrow=(J-1), ncol=4)
    for (i in 1:(J-1)) {
      for (j in 1:4) {
        B[i,j] <- ns_basis[i+1,j]
      }
    }
    coeffs_trans <- as.numeric(B %*% coeffs_spl)
    cov_mtx <- B %*% cov_mtx_spl %*% t(B)
    
    if(estimand == "TATE") {
      # Estimate the TATE over the interval [0,6]
      M <- matrix(rep(1/index_max, index_max), nrow=1)
      tate_est <- (M %*% coeffs_trans)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci
      )
    } else if(estimand == "LTE") {
    
      # Estimate the LTE
      lte_est <- as.numeric(coeffs_trans[index_max])
      lte_se <- sqrt(cov_mtx[index_max, index_max])
      lte_ci <- lte_est + c(-1.96,1.96) * lte_se
    }
    
    results <- list(
      model = model_ncs_mixed,
      model_type = "ncs_mixed",
      estimand = "LTE",
      te_est = lte_est,
      te_se = lte_se,
      te_ci = lte_ci
    )
    
    # # Estimate the effect curve
    # curve_ncs <- c(0, coeffs_trans)
    
  } else if(method == "GEE" & estimand %in% c("TATE", "LTE") & time_varying_assumption == "IT") {
    
    
    ##############################################.
    ##### Immediate Treatment (IT) GEE model #####
    ##############################################.
    
    
    # Fit GEE model
    model_it_GEE <- geepack::geeglm(
      outcome ~ factor(period) + treatment,
      data = dat,
      family = family_obj,
      id = cluster_id,
      corstr = corstr
    )
    summary_it <- summary(model_it_GEE)
    
    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE/LTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    te_se <- summary_it$coefficients["treatment",2]
    te_ci <- te_est + c(-1.96,1.96) * te_se
    # te_ci_lower <- te_est + c(-1.96) * te_se
    # te_ci_upper <- te_est + c(1.96) * te_se
    
    results <- list(
      model = model_it_GEE,
      model_type = "it_GEE",
      estimand = "TATE/LTE",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci
      # te_ci_lower = te_ci_lower,
      # te_ci_upper = te_ci_upper
    )
  } else if(method == "GEE" & time_varying_assumption == "ETI") {
    
    
    ###################################################.
    ##### Exposure Time Indicator (ETI) GEE model #####
    ###################################################.
    
    # Fit GEE model
    model_eti_GEE <- geepack::geeglm(
      outcome ~ factor(period) + factor(exposure_time),
      data = dat,
      family = family_obj,
      id = cluster_id,
      corstr = corstr
    )
    summary_eti <- summary(model_eti_GEE)
    
    
    # Specify the indices of summary_eti corresponding to the exposure time variables
    indices <- grep("exposure_time", rownames(summary_eti$coefficients))
    index_max <- length(indices)
    
    # Extract coefficient estimates and covariance matrix corresponding to exposure
    #     time variables
    coeffs <- summary_eti$coefficients[,1][indices] # column 1 contains the estimates
    cov_mtx <- stats::vcov(model_eti_GEE)[indices,indices]
    
    if(estimand == "TATE") {
      
      # Estimate the TATE (equivalent calculation using
      #     matrix multiplication)
      M <- matrix(rep(1/index_max), index_max, nrow=1)
      tate_est <- (M %*% coeffs)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      results <- list(
        model = model_eti_GEE,
        model_type = "eti_GEE",
        estimand = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci
      )
      
    } else if(estimand == "LTE") {
      
      # Estimate the LTE
      lte_est <- as.numeric(coeffs[index_max])
      lte_se <- sqrt(cov_mtx[index_max,index_max])
      lte_ci <- lte_est + c(-1.96,1.96) * lte_se
      
      results <- list(
        model = model_eti_GEE,
        model_type = "eti_GEE",
        estimand = "LTE",
        te_est = lte_est,
        te_se = lte_se,
        te_ci = lte_ci
      )
      # 
      # # Estimate the effect curve
      # curve_eti <- as.numeric(c(0, coeffs))
      
    } 
    
  }
  
  return(results)
  
  # # Display results
  # display_results("TATE/LTE", te_est, te_ci)

}
