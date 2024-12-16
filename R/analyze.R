#' Title
#'
#' @param dat A dataframe containing the stepped wedge trial data.
#' @param method A character string; either "mixed", for a mixed-effects model, or "GEE", for generalized estimating equations.
#' @param estimand A character string; either "TATE", for time-averaged treatment effect, or "LTE", for long-term treatment effect.
#' @param exp_time One of c("IT", "ETI", "NCS"); model for exposure time. "IT" encodes an immediate treatment model with a single treatment effect parameter. "ETI" is an exposure time indicator model, including one indicator variable for each exposure time point. "NCS" uses a natural cubic spline model for the exposure time trend.
#' @param cal_time One of c("categorical", "NCS", "linear", "none"); model for calendar time. "categorical" uses indicator variables for discrete time points, as in the Hussey and Hughes model. "NCS" uses a natural cubic spline, useful for datasets with continuous time. "linear" uses a single slope parameter. "none" assumes that there is no underlying calendar time trend.
#' @param family A family object; see documentation for `glm()`.
#' @param re A character vector of random effects to include; only relevant if method="mixed" is used. Possible random effects include "clust" (random intercept for cluster), "time" (random intercept for cluster-time interaction), "ind" (random intercept for individuals; appropriate when a cohort design is used), "tx" (random treatment effect)
#' @param corstr One of c("independence", "exchangeable", "ar1"); only relevant if method="mixed" is used. Defines the GEE working correlation structure; see the documentation for `geepack::geeglm()`.
#'
#' @return A list with ___
#' @export
#'
#' @examples
#' # TO DO
analyze <- function(dat, method="mixed", estimand, exp_time="IT",
                    cal_time="categorical", family=stats::gaussian,
                    re=c("clust", "time"), corstr="exchangeable") {

  cluster_id <- NULL
  rm(cluster_id)

  if (!(cal_time %in% c("categorical", "NCS", "linear", "none"))) {
    stop("`cal_time` misspecified.")
  }
  if (!(exp_time %in% c("IT", "ETI", "NCS"))) {
    stop("`exp_time` misspecified.")
  }
  if (!all(re %in% c("clust", "time", "ind", "tx"))) {
    stop('Random effects must be a subset of the vector c("clust", "time", "ind", "tx")')
  }

  if (!methods::is(dat,"sw_dat")) { stop("`dat` must be of class `sw_dat`.") }

  # call appropriate family function with chosen link to create family object
  if (is.character(family)) {
    family <- get(family, mode="function", envir=parent.frame())
  }
  if (is.function(family)) { family <- family() }

  # Parse formula terms for calendar time
  if (cal_time=="categorical") {
    f_cal <- "factor(time) - 1 + "
  } else if (cal_time=="linear") {
    f_cal <- "time + "
  } else if (cal_time=="none") {
    f_cal <- ""
  } else if (cal_time=="NCS") {

    knots_cal <- seq(min(dat$time), max(dat$time), length.out=5) # Make this configurable
    basis_cal <- splines::ns(
      x = dat$time,
      knots = knots_cal[2:4],
      intercept = FALSE,
      Boundary.knots = knots_cal[c(1,5)]
    )
    dat$j_1 <- basis_cal[,1]
    dat$j_2 <- basis_cal[,2]
    dat$j_3 <- basis_cal[,3]
    dat$j_4 <- basis_cal[,4]
    rm(knots_cal,basis_cal)

    f_cal <- "j_1 + j_2 + j_3 + j_4 + "

  }

  # Parse formula terms for random effects
  f_re <- ""
  if ("clust" %in% re) {
    f_re <- paste0(f_re, "(1|cluster_id) + ")
  }
  if ("time" %in% re) {
    dat$ij <- as.integer(factor(paste0(dat$cluster_id,"-",dat$time)))
    f_re <- paste0(f_re, "(1|ij) + ")
  }
  if ("ind" %in% re) {
    f_re <- paste0(f_re, "(1|individual_id) + ")
  }
  if ("tx" %in% re) {
    stop("Random treatment effects not yet implemented")
  }

  if(method == "mixed" & estimand %in% c("TATE", "LTE") & exp_time == "IT") {

    ################################################.
    ##### Immediate Treatment (IT) mixed model #####
    ################################################.

    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0("outcome ~ ", f_cal, "treatment + (1|cluster_id)")
      model_it_mixed <- lme4::lmer(formula, data=dat)
    } else {
      formula <- paste0("outcome ~ ", f_cal, "treatment + (1|cluster_id)")
      model_it_mixed <- lme4::glmer(formula, family=family, data=dat)
    }

    summary_it <- summary(model_it_mixed)

    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE/LTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    te_se <- summary_it$coefficients["treatment",2]
    te_ci <- te_est + c(-1.96,1.96) * te_se

    results <- list(
      model = model_it_mixed,
      model_type = "it_mixed",
      estimand = "TATE/LTE",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci,
      converged = performance::check_convergence(model_it_mixed)[1]
    )
  } else if(method == "mixed" & exp_time == "ETI") {


    #####################################################.
    ##### Exposure Time Indicator (ETI) mixed model #####
    #####################################################.

    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0("outcome ~ ", f_cal, "factor(exposure_time) + (1|cluster_id)")
      model_eti_mixed <- lme4::lmer(formula, data=dat)
    } else {
      formula <- paste0("outcome ~ ", f_cal, "factor(exposure_time) + (1|cluster_id)")
      model_eti_mixed <- lme4::glmer(formula, family=family, data=dat)
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

      # Estimate the TATE
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
        te_ci = tate_ci,
        converged = performance::check_convergence(model_eti_mixed)[1]
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
        te_ci = lte_ci,
        converged = performance::check_convergence(model_eti_mixed)[1]
      )
      #
      # # Estimate the effect curve
      # curve_eti <- as.numeric(c(0, coeffs))

    }

  } else if(method == "mixed" & exp_time == "NCS") {


    ############################################.
    ##### Natural Cubic Spline (NCS) mixed model #####
    ############################################.

    # Create the spline basis (4 degrees of freedom)
    J <- length(unique(dat$time))
    ns_basis <- splines::ns(c(0:(J-1)), knots=c((J-1)/4,(2*(J-1))/4,(3*(J-1))/4))
    dat$b1 <- ns_basis[dat$exposure_time+1,1]
    dat$b2 <- ns_basis[dat$exposure_time+1,2]
    dat$b3 <- ns_basis[dat$exposure_time+1,3]
    dat$b4 <- ns_basis[dat$exposure_time+1,4]

    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0("outcome ~ ", f_cal, "b1 + b2 + b3 + b4 + (1|cluster_id)")
      model_ncs_mixed <- lme4::lmer(formula, data=dat)
    } else {
      formula <- paste0("outcome ~ ", f_cal, "b1 + b2 + b3 + b4 + (1|cluster_id)")
      model_ncs_mixed <- lme4::glmer(formula, family=family, data=dat)
    }

    summary_ncs <- summary(model_ncs_mixed)

    # Specify the indices corresponding to the spline terms
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
        te_ci = tate_ci,
        converged = performance::check_convergence(model_ncs_mixed)[1]
      )
    } else if(estimand == "LTE") {

      # Estimate the LTE
      lte_est <- as.numeric(coeffs_trans[index_max])
      lte_se <- sqrt(cov_mtx[index_max, index_max])
      lte_ci <- lte_est + c(-1.96,1.96) * lte_se

      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand = "LTE",
        te_est = lte_est,
        te_se = lte_se,
        te_ci = lte_ci,
        converged = performance::check_convergence(model_ncs_mixed)[1]
      )
    }



    # # Estimate the effect curve
    # curve_ncs <- c(0, coeffs_trans)

  } else if(method == "GEE" & estimand %in% c("TATE", "LTE") & exp_time == "IT") {


    ##############################################.
    ##### Immediate Treatment (IT) GEE model #####
    ##############################################.


    # Fit GEE model
    formula <- paste0("outcome ~ ", f_cal, "treatment")
    model_it_GEE <- geepack::geeglm(
      stats::as.formula(formula),
      data = dat,
      family = family,
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

    results <- list(
      model = model_it_GEE,
      model_type = "it_GEE",
      estimand = "TATE/LTE",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci
    )
  } else if(method == "GEE" & exp_time == "ETI") {


    ###################################################.
    ##### Exposure Time Indicator (ETI) GEE model #####
    ###################################################.

    # Fit GEE model
    formula <- paste0("outcome ~ ", f_cal, "factor(exposure_time)")
    model_eti_GEE <- geepack::geeglm(
      stats::as.formula(formula),
      data = dat,
      family = family,
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

      # Estimate the TATE
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
