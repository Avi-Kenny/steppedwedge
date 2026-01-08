#' Analyze a stepped wedge dataset
#'
#' @param dat A dataframe containing the stepped wedge trial data.
#' @param method A character string; either "mixed", for a mixed-effects model,
#'     or "GEE", for generalized estimating equations.
#' @param estimand_type One of c("TATE", "PTE"); "TATE" represents the
#'     time-averaged treatment effect and "PTE" represents the point treatment
#'     effect.
#' @param estimand_time An integer vector of length 1 or 2. When
#'     estimand_type="TATE", `estimand_time` must be a numeric vector of length
#'     2, representing the start and end times of the exposure time period to
#'     average over. When estimand_type="PTE", `estimand_time` must be a numeric
#'     vector of length 1, representing the time period of interest. See
#'     examples.
#' @param exp_time One of c("IT", "ETI", "NCS", "TEH"); model for exposure time.
#'     "IT" encodes an immediate treatment model with a single treatment effect
#'     parameter. "ETI" is an exposure time indicator model, including one
#'     indicator variable for each exposure time point. "NCS" uses a natural
#'     cubic spline model for the exposure time trend. "TEH" includes a random
#'     slope term in the model, allowing the treatment effect to vary by
#'     timepoint.
#' @param cal_time One of c("categorical", "NCS", "linear", "none"); model for
#'     calendar time. "categorical" uses indicator variables for discrete time
#'     points, as in the Hussey and Hughes model. "NCS" uses a natural cubic
#'     spline, useful for datasets with continuous time. "linear" uses a single
#'     slope parameter. "none" assumes that there is no underlying calendar time
#'     trend.
#' @param family A family object; see documentation for `glm`.
#' @param exponentiate Logical; if TRUE, return exponentiated treatment effect 
#'     estimates and confidence intervals (including in the `effect_curve`
#'     object). Defaults to FALSE.
#' @param re A character vector of random effects to include; only relevant if
#'     method="mixed" is used. Possible random effects include "clust" (random
#'     intercept for cluster), "time" (random intercept for cluster-time
#'     interaction), "ind" (random intercept for individuals; appropriate when a
#'     cohort design is used), "tx" (random treatment effect)
#' @param corstr One of c("independence", "exchangeable", "ar1"); only relevant
#'     if method="GEE" is used. Defines the GEE working correlation structure;
#'     see the documentation for `geepack::geeglm`.
#' @param advanced A list of options returned by \code{\link{params}}.
#'
#' @return A list with the model object, model type as a string, estimand type
#' as a string, numeric treatment effect estimate, numeric treatment effect standard error,
#' treatment effect 95% confidence interval as a numeric vector of length 2,
#' a list with treatment effect estimates (and standard errors and 95% confidence intervals)
#' at each exposure timepoint, the original dataframe passed to `analyze()`, and an
#' indicator whether the effect estimates and CI are exponentiated.
#' @export
#'
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#'
#' # Analysis example 1: TATE estimand for exposure times 1 through 4
#' results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI")
#'
#' results_tate
#'
#' # Analysis example 2: PTE estimand for exposure time 3
#' results_pte <- analyze(dat = test_data, method = "mixed", estimand_type = "PTE",
#' estimand_time = 3, exp_time = "ETI")
#'
#' results_pte
#' 
#' # Analysis example 3: TATE estimand for exposure times 1 through 4, Natural Cubic Splines model
#' results_tate_ncs <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "NCS", advanced = params(n_knots_exp = 4))
#' 
#' results_tate_ncs
#' 
#' # Analysis example 4: TATE estimand for exposure times 1 through 4 with binomial outcome data
#' # Load data
#' test_data_bin <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = c("numerator", "denominator"), data = sw_data_example_binom)
#' 
#' results_pte_bin <- analyze(dat = test_data_bin, family = binomial, method = "mixed", 
#' estimand_type = "TATE", estimand_time = c(1, 4), exp_time = "ETI")
#'
#' results_pte_bin
#' 


analyze <- function(dat, method="mixed", estimand_type="TATE",
                    estimand_time=c(1,max(dat$exposure_time)), exp_time="IT",
                    cal_time="categorical", family=stats::gaussian,
                    exponentiate = FALSE,
                    re=c("clust", "time"), corstr="exchangeable", 
                    advanced = params()) {

  dat_orig <- dat
  cluster_id <- NULL
  rm(cluster_id)

  if (!(cal_time %in% c("categorical", "NCS", "linear", "none"))) {
    stop("`cal_time` misspecified.")
  }
  if (!(exp_time %in% c("IT", "ETI", "NCS", "TEH"))) {
    stop("`exp_time` misspecified.")
  }
  if (!all(re %in% c("clust", "time", "ind", "tx"))) {
    stop('Random effects must be a subset of the vector c("clust", "time", "ind", "tx")')
  }
  if (estimand_type == "TATE" &
      !(length(estimand_time) == 2 & is.numeric(estimand_time))) {
    stop('When estimand_type=="TATE", `estimand_time` must be a numeric vector of length 2')
  }
  if (estimand_type == "PTE" &
      !(length(estimand_time) == 1 & is.numeric(estimand_time))) {
    stop('When estimand_type=="PTE", `estimand_time` must be a numeric vector of length 1')
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

    knots_cal <- seq(min(dat$time), max(dat$time), length.out=advanced$n_knots_cal)
    basis_cal <- splines::ns(
      x = dat$time,
      knots = knots_cal[2:(advanced$n_knots_cal-1)],
      intercept = TRUE,
      Boundary.knots = knots_cal[c(1,advanced$n_knots_cal)]
    )
    dat$j <- rep(NA, nrow(dat))
    for (i in 1:advanced$n_knots_cal) {
      dat[[paste0("j_", i)]] <- basis_cal[,i]
    }
    f_cal_terms <- paste0("j_", 1:ncol(basis_cal))
    f_cal <- paste(f_cal_terms, collapse = " + ")
    f_cal <- paste0(f_cal, " - 1 + ")

    rm(knots_cal,basis_cal)
  }

  # Parse formula terms for random effects
  f_re <- ""
  if ("clust" %in% re) {
    f_re <- paste0(f_re, " + (1|cluster_id)")
  }
  if ("time" %in% re) {
    dat$ij <- as.integer(factor(paste0(dat$cluster_id,"-",dat$time)))
    f_re <- paste0(f_re, " + (1|ij)")
  }
  if ("ind" %in% re) {
    f_re <- paste0(f_re, " + (1|individual_id)")
  }
  if ("tx" %in% re) {
    stop("Random treatment effects not yet implemented")
  }

  # Parse formula terms for outcome
  f_out <- ifelse(attr(dat, "binomial") == TRUE,
                  "cbind(successes, trials - successes) ~ ",
                  "outcome ~ ")

  # Save vector of (non-zero) exposure times
  exp_times <- sort(unique(dat$exposure_time))
  exp_times <- exp_times[exp_times!=0]
  max_exp_time <- max(exp_times)

  if(method == "mixed" & estimand_type %in% c("TATE", "PTE") & exp_time == "IT") {

    ################################################.
    ##### Immediate Treatment (IT) mixed model #####
    ################################################.

    # Fit mixed model
    if(is.null(advanced$offset)) {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(f_out, f_cal, "treatment", f_re))
        model_it_mixed <- lme4::lmer(formula, data=dat)
      } else {
        formula <- stats::as.formula(paste0(f_out, f_cal, "treatment", f_re))
        model_it_mixed <- lme4::glmer(formula, family=family, data=dat)
      }
    } else {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(f_out, f_cal, "treatment", f_re))
        model_it_mixed <- lme4::lmer(formula, data=dat, offset=advanced$offset)
      } else {
        formula <- stats::as.formula(paste0(f_out, f_cal, "treatment", f_re))
        model_it_mixed <- lme4::glmer(formula, family=family, data=dat,
                                      offset=advanced$offset)
      }
    }


    summary_it <- summary(model_it_mixed)

    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    if(advanced$var_est == "model") {
      te_se <- summary_it$coefficients["treatment",2]
    } else if(advanced$var_est == "robust") {
      cov_cr  <- vcovCR.glmerMod(model_it_mixed, cluster = dat$cluster_id, 
                                 type = advanced$var_est_type)
      cov_mtx <- cov_cr["treatment", "treatment"]
      te_se  <- sqrt(cov_mtx)
    }
    te_ci <- te_est + c(-1.96,1.96) * te_se
    
    if(exponentiate) {
      te_est <- exp(te_est)
      te_ci <- exp(te_ci)
      zero_value <- 1
    } else {
      zero_value <- 0
    }

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = c(0, exp_times),
      est = c(zero_value, rep(te_est, length(exp_times))),
      se = c(0, rep(te_se, length(exp_times))),
      vcov = NA,
      ci_upper = c(zero_value, rep(te_ci[1], length(exp_times))),
      ci_lower = c(zero_value, rep(te_ci[2], length(exp_times)))
    )
    
    if(lme4::isSingular(model_it_mixed)) {
      is_converged <- FALSE
    } else {
      is_converged <- performance::check_convergence(model_it_mixed)[1]
    }

    results <- list(
      model = model_it_mixed,
      model_type = "it_mixed",
      estimand_type = "TATE (IT)",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci,
      converged = is_converged,
      messages = model_it_mixed@optinfo$conv$lme4$messages,
      effect_curve = effect_curve,
      dat = dat_orig
    )
  } else if(method == "mixed" & exp_time == "ETI") {


    #####################################################.
    ##### Exposure Time Indicator (ETI) mixed model #####
    #####################################################.

    for (i in c(1:length(exp_times))) {
      dat[[paste0("exp_",exp_times[i])]] <- as.integer(dat$exposure_time==exp_times[i])
    }

    f_exp <- paste0("exp_", exp_times, collapse = " + ")

    # Fit mixed model
    if(is.null(advanced$offset)) {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(f_out, f_cal, f_exp, f_re))
        model_eti_mixed <- lme4::lmer(formula, data=dat)
      } else {
        formula <- stats::as.formula(paste0(f_out, f_cal, f_exp, f_re))
        model_eti_mixed <- lme4::glmer(formula, family=family, data=dat)
      }
    } else {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(f_out, f_cal, f_exp, f_re))
        model_eti_mixed <- lme4::lmer(formula, data=dat, offset=advanced$offset)
      } else {
        formula <- stats::as.formula(paste0(f_out, f_cal, f_exp, f_re))
        model_eti_mixed <- lme4::glmer(formula, family=family, data=dat,
                                       offset=advanced$offset)
      }
    }


    summary_eti <- summary(model_eti_mixed)


    # Specify the indices of summary_eti corresponding to the exposure time variables
    indices <- grep("exp_", rownames(summary_eti$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to exposure
    #     time variables
    coeffs <- summary_eti$coefficients[,1][indices] # column 1 contains the estimates
    if(advanced$var_est == "model") {
      se_eti <- summary_eti$coefficients[,2][indices] # column 2 contains the standard errors
      cov_mtx <- stats::vcov(model_eti_mixed)[indices,indices]
    } else if(advanced$var_est == "robust") {
      cov_cr  <- vcovCR.glmerMod(model_eti_mixed, cluster = dat$cluster_id, 
                                 type = advanced$var_est_type)
      cov_mtx <- cov_cr[indices, indices, drop = FALSE]
      se_eti  <- sqrt(Matrix::diag(cov_mtx))
    }

    # Calculate the CI for treatment effect at each exposure time
    ci_lower_eti <- coeffs - 1.96 * se_eti
    ci_upper_eti <- coeffs + 1.96 * se_eti
    
    if(exponentiate) {
      coeffs_return <- exp(coeffs)
      ci_lower_eti_return <- exp(ci_lower_eti)
      ci_upper_eti_return <- exp(ci_upper_eti)
      zero_value <- 1
    } else {
      coeffs_return <- coeffs
      ci_lower_eti_return <- ci_lower_eti
      ci_upper_eti_return <- ci_upper_eti
      zero_value <- 0
    }

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = c(0, exp_times),
      est = c(zero_value, as.numeric(coeffs_return)),
      se = c(0, se_eti),
      vcov = cov_mtx,
      ci_lower = c(zero_value, as.numeric(ci_lower_eti_return)),
      ci_upper = c(zero_value, as.numeric(ci_upper_eti_return))
    )
    
    if(lme4::isSingular(model_eti_mixed)) {
      is_converged <- FALSE
    } else {
      is_converged <- performance::check_convergence(model_eti_mixed)[1]
    }

    if(estimand_type == "TATE") {

      # Estimate the TATE
      num_estimand_timepoints <- estimand_time[2] - estimand_time[1] + 1
      M <- matrix(c(rep(0, estimand_time[1] - 1),
                    rep(1 / num_estimand_timepoints, num_estimand_timepoints),
                    rep(0, index_max - estimand_time[2])
                  ),
                  nrow = 1)
      tate_est <- (M %*% coeffs)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      if(exponentiate) {
        tate_est_return <- exp(tate_est)
        tate_ci_return <- exp(tate_ci)
      } else {
        tate_est_return <- tate_est
        tate_ci_return <- tate_ci
      }

      results <- list(
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand_type = "TATE",
        te_est = tate_est_return,
        te_se = tate_se,
        te_ci = tate_ci_return,
        converged = is_converged,
        messages = model_eti_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )

    } else if(estimand_type == "PTE") {

      # Estimate the PTE
      pte_est <- as.numeric(coeffs[estimand_time])
      pte_se <- sqrt(cov_mtx[estimand_time,estimand_time])
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se
      
      if(exponentiate) {
        pte_est_return <- exp(pte_est)
        pte_ci_return <- exp(pte_ci)
      } else {
        pte_est_return <- pte_est
        pte_ci_return <- pte_ci
      }
      
        results <- list(
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand_type = "PTE",
        te_est = pte_est_return,
        te_se = pte_se,
        te_ci = pte_ci_return,
        converged = is_converged,
        messages = model_eti_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )

    }

  } else if(method == "mixed" & exp_time == "TEH") {


    #####################################################.
    ##### Treatment Effect Heterogeneity (TEH) mixed model #####
    #####################################################.


    # Fit mixed model
    if(is.null(advanced$offset)) {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(
          f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re
        ))
        model_teh_mixed <- lme4::lmer(formula, data=dat)
      } else {
        formula <- stats::as.formula(paste0(
          f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re
        ))
        model_teh_mixed <- lme4::glmer(formula, family=family, data=dat)
      }
    } else {
      if(family$family == "gaussian" & family$link == "identity") {
        formula <- stats::as.formula(paste0(
          f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re
        ))
        model_teh_mixed <- lme4::lmer(formula, data=dat, offset=advanced$offset)
      } else {
        formula <- stats::as.formula(paste0(
          f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re
        ))
        model_teh_mixed <- lme4::glmer(formula, family=family, data=dat,
                                       offset=advanced$offset)
      }
    }


    summary_teh <- summary(model_teh_mixed)

    # Extract random slopes for treatment and their variances from mixed model
    re_model <- lme4::ranef(model_teh_mixed, condVar = TRUE)
    re_treatment <- re_model$exposure_time

    re_var <- attr(re_treatment, "postVar")[1,1,]
    re_se <- sqrt(re_var)

    # Extract fixed treatment effect from mixed model
    fe_treatment <- summary_teh$coefficients["treatment",1]

    # Calculate the CI for treatment effect at each exposure time
    ####### RESUME HERE #######
    est_teh <- rep(fe_treatment, length(exp_times)) + re_treatment[rownames(re_treatment) != "0", "treatment"]
    se_teh <- sqrt(summary_teh$coefficients["treatment",2]^2 + re_se[-1]^2)
    ci_lower_teh <- est_teh - 1.96 * se_teh
    ci_upper_teh <- est_teh + 1.96 * se_teh
    
    if(exponentiate) {
      est_teh_return <- exp(est_teh)
      ci_lower_teh_return <- exp(ci_lower_teh)
      ci_upper_teh_return <- exp(ci_upper_teh)
      zero_value <- 1
    } else {
      est_teh_return <- est_teh
      ci_lower_teh_return <- ci_lower_teh
      ci_upper_teh_return <- ci_upper_teh
      zero_value <- 0
    }

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = c(0, exp_times),
      est = c(zero_value, est_teh_return),
      se = c(0, se_teh),
      vcov = NA,
      ci_upper = c(zero_value, ci_upper_teh_return),
      ci_lower = c(zero_value, ci_lower_teh_return)
    )

    if(lme4::isSingular(model_teh_mixed)) {
      is_converged <- FALSE
    } else {
      is_converged <- performance::check_convergence(model_teh_mixed)[1]
    }
    
    if(estimand_type == "TATE") {

      # Estimate the TATE
      tate_est <- summary_teh$coefficients["treatment",1]
      tate_se <- summary_teh$coefficients["treatment",2]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      if(exponentiate) {
        tate_est_return <- exp(tate_est)
        tate_ci_return <- exp(tate_ci)
      } else {
        tate_est_return <- tate_est
        tate_ci_return <- tate_ci
      }
      
      results <- list(
        model = model_teh_mixed,
        model_type = "teh_mixed",
        estimand_type = "TATE",
        te_est = tate_est_return,
        te_se = tate_se,
        te_ci = tate_ci_return,
        converged = is_converged,
        messages = model_teh_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )

    } else if(estimand_type == "PTE") {

      # Estimate the PTE by combining the estimates from the fixed effect component and the random effect for the final timepoint
      re_treatment_pte <- re_treatment[rownames(re_treatment) == as.character(estimand_time), "treatment"]
      pte_est <- lme4::fixef(model_teh_mixed)["treatment"] + re_treatment_pte

      # Estimate the SE of the PTE by combining the variances from the fixed effect component and the random effect for the final timepoint
      re_se_pte <- re_se[estimand_time]

      pte_se <- sqrt(summary_teh$coefficients["treatment",2]^2 + re_se_pte^2)
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se
      
      if(exponentiate) {
        pte_est_return <- exp(pte_est)
        pte_ci_return <- exp(pte_ci)
      } else {
        pte_est_return <- pte_est
        pte_ci_return <- pte_ci
      }

      results <- list(
        model = model_teh_mixed,
        model_type = "teh_mixed",
        estimand_type = "PTE",
        te_est = pte_est_return,
        te_se = pte_se,
        te_ci = pte_ci_return,
        converged = is_converged,
        messages = model_teh_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )

    }

  } else if(method == "mixed" & exp_time == "NCS") {


    ############################################.
    ##### Natural Cubic Spline (NCS) mixed model #####
    ############################################.

    # Create the spline basis
    S <- max(dat$exposure_time)
    knots_exp <- seq(0, S, length.out=advanced$n_knots_exp)
    ns_basis <- splines::ns(
      x = dat$exposure_time,
      knots = knots_exp[2:(advanced$n_knots_exp-1)],
      intercept = TRUE,
      Boundary.knots = knots_exp[c(1,advanced$n_knots_exp)]
    )

    dat$b <- rep(NA, nrow(dat))
    for (i in 1:advanced$n_knots_exp) {
      dat[[paste0("b", i)]] <- ns_basis[,i] * dat$treatment
    }

    # Fit mixed model
    formula <- stats::as.formula(paste0(
      f_out, f_cal, paste0("b", 1:advanced$n_knots_exp, collapse = " + "), f_re
    ))

    if(is.null(advanced$offset)) {
      if(family$family == "gaussian" & family$link == "identity") {
        model_ncs_mixed <- lme4::lmer(formula, data=dat)
      } else {
        model_ncs_mixed <- lme4::glmer(formula, family=family, data=dat)
      }
    } else {
      if(family$family == "gaussian" & family$link == "identity") {
        model_ncs_mixed <- lme4::lmer(formula, data=dat, offset=advanced$offset)
      } else {
        model_ncs_mixed <- lme4::glmer(formula, family=family, data=dat,
                                       offset=advanced$offset)
      }
    }

    summary_ncs <- summary(model_ncs_mixed)

    # Specify the indices corresponding to the spline terms
    indices <- grep("^b[0-9]+$", rownames(summary_ncs$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to spline
    # terms
    coeffs_orig <- summary_ncs$coefficients[,1]
    coeffs_spl <- coeffs_orig[indices]
    if(advanced$var_est == "model") {
      cov_orig_full <- stats::vcov(model_ncs_mixed)
      cov_mtx_spl <- cov_orig_full[indices,indices]
      # te_se <- summary_it$coefficients["treatment",2]
    } else if(advanced$var_est == "robust") {
      cov_orig_full <- vcovCR.glmerMod(model_ncs_mixed, cluster = dat$cluster_id,
                                      type = advanced$var_est_type)
      indices <- grep("^b[0-9]+$", rownames(cov_orig_full))
      cov_mtx_spl <- cov_orig_full[indices,indices]
    }
    
    # Transform the spline terms into effect curve estimates (+ covariance matrix)
    B <- as.matrix(splines::ns(
      x = c(1:S),
      knots = knots_exp[2:(advanced$n_knots_exp-1)],
      intercept = TRUE,
      Boundary.knots = knots_exp[c(1,advanced$n_knots_exp)]
    ))

    class(B) <- "matrix"

    coeffs_trans <- as.numeric(B %*% coeffs_spl)
    cov_mtx <- B %*% cov_mtx_spl %*% t(B)
    se_ncs <- sqrt(diag(matrix(cov_mtx, nrow = nrow(cov_mtx))))
    
    
    # Calculate the CI for treatment effect at each exposure time
    ci_lower_ncs <- coeffs_trans - 1.96 * se_ncs
    ci_upper_ncs <- coeffs_trans + 1.96 * se_ncs
    
    if(exponentiate) {
      coeffs_trans_return <- exp(coeffs_trans)
      ci_lower_ncs_return <- exp(ci_lower_ncs)
      ci_upper_ncs_return <- exp(ci_upper_ncs)
      zero_value <- 1
    } else {
      coeffs_trans_return <- coeffs_trans
      ci_lower_ncs_return <- ci_lower_ncs
      ci_upper_ncs_return <- ci_upper_ncs
      zero_value <- 0
    }

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = c(0, 1:S),
      est = c(zero_value, coeffs_trans),
      se = c(0, se_ncs),
      vcov = cov_mtx,
      ci_lower = c(zero_value, ci_lower_ncs),
      ci_upper = c(zero_value, ci_upper_ncs)
    )
    
    # Create A* block diagonal transformation matrix to return
    
    if(advanced$return_ncs == TRUE) {
      # Dimensions for the identity block (non-spline parameters)
      total_fixed_params <- nrow(summary_ncs$coefficients)
      n_spline_params <- length(indices)
      n_other_params <- total_fixed_params - n_spline_params
      
      # Create the identity block (I) for non-spline terms
      I_block <- diag(n_other_params)
      
      # Create zero blocks to fill the off-diagonals
      # Top-right: 0s connecting non-spline params to exposure time rows
      Zero_top_right <- matrix(0, nrow = n_other_params, ncol = n_spline_params)
      
      # Bottom-left: 0s connecting spline params to non-spline rows
      Zero_bottom_left <- matrix(0, nrow = nrow(B), ncol = n_other_params)
      
      # Bind them together: [ I   0 ]
      #                     [ 0   B ]
      Top_Row <- cbind(I_block, Zero_top_right)
      Bottom_Row <- cbind(Zero_bottom_left, B)
      
      T_mat <- rbind(Top_Row, Bottom_Row)
      
      rownames(T_mat) <- c(rownames(summary_ncs$coefficients)[-indices], 
                           paste0("ExpTime_", 1:nrow(B)))
      colnames(T_mat) <- names(coeffs_orig)
      
      coeffs_trans_full <- as.numeric(T_mat %*% coeffs_orig)
      cov_trans_full <- T_mat %*% cov_orig_full %*% t(T_mat)
      
      name_prefix <- rownames(summary_ncs$coefficients)[-indices]
      name_suffix <- paste0("ExpTime_", 1:nrow(B))
      new_names <- c(name_prefix, name_suffix)
      
      names(coeffs_trans_full) <- new_names
      rownames(cov_trans_full) <- new_names
      colnames(cov_trans_full) <- new_names
    }
    
    if(lme4::isSingular(model_ncs_mixed)) {
      is_converged <- FALSE
    } else {
      is_converged <- performance::check_convergence(model_ncs_mixed)[1]
    }
    
    if(estimand_type == "TATE") {
      # Estimate the TATE
      num_estimand_timepoints <- estimand_time[2] - estimand_time[1] + 1
      M <- matrix(c(rep(0, estimand_time[1] - 1),
                    rep(1 / num_estimand_timepoints, num_estimand_timepoints),
                    rep(0, max_exp_time - estimand_time[2])
                    ),
                  nrow = 1)
      tate_est <- (M %*% coeffs_trans)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se
      
      if(exponentiate) {
        tate_est_return <- exp(tate_est)
        tate_ci_return <- exp(tate_ci)
      } else {
        tate_est_return <- tate_est
        tate_ci_return <- tate_ci
      }

      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand_type = "TATE",
        te_est = tate_est_return,
        te_se = tate_se,
        te_ci = tate_ci_return,
        converged = is_converged,
        messages = model_ncs_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )
    } else if(estimand_type == "PTE") {

      # Estimate the PTE
      pte_est <- as.numeric(coeffs_trans[estimand_time])
      pte_se <- sqrt(cov_mtx[estimand_time, estimand_time])
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se
      
      if(exponentiate) {
        pte_est_return <- exp(pte_est)
        pte_ci_return <- exp(pte_ci)
      } else {
        pte_est_return <- pte_est
        pte_ci_return <- pte_ci
      }
      
      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand_type = "PTE",
        te_est = pte_est_return,
        te_se = pte_se,
        te_ci = pte_ci_return,
        converged = is_converged,
        messages = model_ncs_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve,
        dat = dat_orig
      )
    }
    
    if(advanced$return_ncs == TRUE) {
      results$T_mat    = T_mat
      results$V_orig   = cov_orig_full
      results$beta_new = coeffs_trans_full
      results$V_new    = cov_trans_full
    }

  } else if(method == "GEE" & estimand_type %in% c("TATE", "PTE") & exp_time == "IT") {


    ##############################################.
    ##### Immediate Treatment (IT) GEE model #####
    ##############################################.


    # Fit GEE model
    formula <- paste0(f_out, f_cal, "treatment")
    model_it_GEE <- geepack::geeglm(
      stats::as.formula(formula),
      data = dat,
      family = family,
      id = cluster_id,
      corstr = corstr
    )
    summary_it <- summary(model_it_GEE)

    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    te_se <- summary_it$coefficients["treatment",2]
    te_ci <- te_est + c(-1.96,1.96) * te_se

    results <- list(
      model = model_it_GEE,
      model_type = "it_GEE",
      estimand_type = "TATE (IT)",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci,
      dat = dat_orig
    )
  } else if(method == "GEE" & exp_time == "ETI") {


    ###################################################.
    ##### Exposure Time Indicator (ETI) GEE model #####
    ###################################################.

    for (i in c(1:length(exp_times))) {
      dat[[paste0("exp_",exp_times[i])]] <- as.integer(dat$exposure_time==exp_times[i])
    }

    f_exp <- paste0("exp_", exp_times, collapse = " + ")

    # Fit GEE model
    formula <- paste0(f_out, f_cal, f_exp)
    model_eti_GEE <- geepack::geeglm(
      stats::as.formula(formula),
      data = dat,
      family = family,
      id = cluster_id,
      corstr = corstr
    )
    summary_eti <- summary(model_eti_GEE)


    # Specify the indices of summary_eti corresponding to the exposure time variables
    indices <- grep("exp_", rownames(summary_eti$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to exposure
    #     time variables
    coeffs <- summary_eti$coefficients[,1][indices] # column 1 contains the estimates
    cov_mtx <- stats::vcov(model_eti_GEE)[indices,indices]

    if(estimand_type == "TATE") {

      # Estimate the TATE
      num_estimand_timepoints <- estimand_time[2] - estimand_time[1] + 1
      M <- matrix(c(rep(0, estimand_time[1] - 1),
                    rep(1 / num_estimand_timepoints, num_estimand_timepoints),
                    rep(0, index_max - estimand_time[2])
      ),
      nrow = 1)
      tate_est <- (M %*% coeffs)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se

      results <- list(
        model = model_eti_GEE,
        model_type = "eti_GEE",
        estimand_type = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci,
        dat = dat_orig
      )

    } else if(estimand_type == "PTE") {

      # Estimate the PTE
      pte_est <- as.numeric(coeffs[estimand_time])
      pte_se <- sqrt(cov_mtx[estimand_time,estimand_time])
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se

      results <- list(
        model = model_eti_GEE,
        model_type = "eti_GEE",
        estimand_type = "PTE",
        te_est = pte_est,
        te_se = pte_se,
        te_ci = pte_ci,
        dat = dat_orig
      )


    }

  }
  
  results$exponentiated <- exponentiate

  class(results) <- c("list", "sw_analysis")

  return(results)

}
