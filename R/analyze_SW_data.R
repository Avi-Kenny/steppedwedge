#' Title
#'
#' @param dat A dataframe containing the stepped wedge trial data.
#' @param method A character string; either "mixed", for a mixed-effects model, or "GEE", for generalized estimating equations.
#' @param estimand A character string; either "TATE", for time-averaged treatment effect, or "LTE", for long-term treatment effect.
#' @param time_varying_assumption A character string; either "IT" for immediate treatment effect, or "ETI", for Exposure time indicator.
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
