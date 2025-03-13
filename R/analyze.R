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
#' @param re A character vector of random effects to include; only relevant if
#'     method="mixed" is used. Possible random effects include "clust" (random
#'     intercept for cluster), "time" (random intercept for cluster-time
#'     interaction), "ind" (random intercept for individuals; appropriate when a
#'     cohort design is used), "tx" (random treatment effect)
#' @param corstr One of c("independence", "exchangeable", "ar1"); only relevant
#'     if method="GEE" is used. Defines the GEE working correlation structure;
#'     see the documentation for `geepack::geeglm`.
#' @param offset A linear predictor offset term; see docs for `lme4::lmer`.
#' @param n_knots_exp An integer; only relevant when exp_time="NCS". Specifies
#'     the number of knots to use for exposure time, including boundary knots.
#'     The spline basis includes an intercept, and the degree of the basis is
#'     equal to the number of knots.
#' @param n_knots_cal An integer; only relevant when exp_time="NCS". Specifies
#'     the number of knots to use for calendar time, including boundary knots.
#'     The spline basis includes an intercept, and the degree of the basis is
#'     equal to the number of knots.
#'
#' @return A list with the model object, model type as a string, estimand type
#' as a string, numeric treatment effect estimate, numeric treatment effect standard error,
#' treatment effect 95% confidence interval as a numeric vector of length 2, and
#' dataframe with treatment effect at each exposure timepoint.
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
analyze <- function(dat, method="mixed", estimand_type="TATE",
                    estimand_time=c(1,attr(dat,"n_seq")), exp_time="IT",
                    cal_time="categorical", family=stats::gaussian,
                    re=c("clust", "time"), corstr="exchangeable", offset=NULL,
                    n_knots_exp=4, n_knots_cal=4) {

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

    knots_cal <- seq(min(dat$time), max(dat$time), length.out=n_knots_cal)
    basis_cal <- splines::ns(
      x = dat$time,
      knots = knots_cal[2:(n_knots_cal-1)],
      intercept = TRUE,
      Boundary.knots = knots_cal[c(1,n_knots_cal)]
    )
    dat$j <- rep(NA, nrow(dat))
    for (i in 1:n_knots_cal) {
      dat[[paste0("j_", i)]] <- basis_cal[,i]
    }
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

  if(method == "mixed" & estimand_type %in% c("TATE", "PTE") & exp_time == "IT") {

    ################################################.
    ##### Immediate Treatment (IT) mixed model #####
    ################################################.

    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0(f_out, f_cal, "treatment", f_re)
      model_it_mixed <- lme4::lmer(formula, data=dat, offset=offset)
    } else {
      formula <- paste0(f_out, f_cal, "treatment", f_re)
      model_it_mixed <- lme4::glmer(formula, family=family, data=dat,
                                    offset=offset)
    }

    summary_it <- summary(model_it_mixed)

    # Extract an estimate and confidence interval for the estimated treatment
    #     effect; recall that the TATE estimator for any interval and the PTE
    #     estimators are all equivalent when using the immediate treatment model
    te_est <- summary_it$coefficients["treatment",1]
    te_se <- summary_it$coefficients["treatment",2]
    te_ci <- te_est + c(-1.96,1.96) * te_se

    # Estimate the effect curve
    exp_times <- sort(unique(dat$exposure_time))
    exp_times <- exp_times[exp_times!=0]
    effect_curve <- list(
      exp_time = exp_times,
      est = rep(te_est, length(exp_times)),
      se = rep(te_se, length(exp_times)),
      vcov = NA,
      ci_upper = rep(te_ci[1], length(exp_times)),
      ci_lower = rep(te_ci[2], length(exp_times))
    )

    results <- list(
      model = model_it_mixed,
      model_type = "it_mixed",
      estimand_type = "TATE (IT)",
      te_est = te_est,
      te_se = te_se,
      te_ci = te_ci,
      converged = performance::check_convergence(model_it_mixed)[1],
      messages = model_it_mixed@optinfo$conv$lme4$messages,
      effect_curve = effect_curve
    )
  } else if(method == "mixed" & exp_time == "ETI") {


    #####################################################.
    ##### Exposure Time Indicator (ETI) mixed model #####
    #####################################################.

    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0(f_out, f_cal, "factor(exposure_time)", f_re)
      model_eti_mixed <- lme4::lmer(formula, data=dat, offset=offset)
    } else {
      formula <- paste0(f_out, f_cal, "factor(exposure_time)", f_re)
      model_eti_mixed <- lme4::glmer(formula, family=family, data=dat,
                                     offset=offset)
    }

    summary_eti <- summary(model_eti_mixed)


    # Specify the indices of summary_eti corresponding to the exposure time variables
    indices <- grep("exposure_time", rownames(summary_eti$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to exposure
    #     time variables
    coeffs <- summary_eti$coefficients[,1][indices] # column 1 contains the estimates
    se_eti <- summary_eti$coefficients[,2][indices] # column 2 contains the standard errors
    cov_mtx <- stats::vcov(model_eti_mixed)[indices,indices]

    # Calculate the CI for treatment effect at each exposure time
    ci_lower_eti <- coeffs - 1.96 * se_eti
    ci_upper_eti <- coeffs + 1.96 * se_eti

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = unique(dat$exposure_time)[unique(dat$exposure_time)!=0],
      est = as.numeric(coeffs),
      se = se_eti,
      vcov = cov_mtx,
      ci_lower = as.numeric(ci_lower_eti),
      ci_upper = as.numeric(ci_upper_eti)
    )

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
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand_type = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci,
        converged = performance::check_convergence(model_eti_mixed)[1],
        messages = model_eti_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )

    } else if(estimand_type == "PTE") {

      # Estimate the PTE
      pte_est <- as.numeric(coeffs[estimand_time])
      pte_se <- sqrt(cov_mtx[estimand_time,estimand_time])
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se

      results <- list(
        model = model_eti_mixed,
        model_type = "eti_mixed",
        estimand_type = "PTE",
        te_est = pte_est,
        te_se = pte_se,
        te_ci = pte_ci,
        converged = performance::check_convergence(model_eti_mixed)[1],
        messages = model_eti_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )

    }

  } else if(method == "mixed" & exp_time == "TEH") {


    #####################################################.
    ##### Treatment Effect Heterogeneity (TEH) mixed model #####
    #####################################################.


    # Fit mixed model
    if(family$family == "gaussian" & family$link == "identity") {
      formula <- paste0(f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re)
      model_teh_mixed <- lme4::lmer(formula, data=dat, offset=offset)
    } else {
      formula <- paste0(f_out, f_cal, "treatment + (0 + treatment|exposure_time)", f_re)
      model_teh_mixed <- lme4::glmer(formula, family=family, data=dat,
                                     offset=offset)
    }

    summary_teh <- summary(model_teh_mixed)

    # Extract random slopes for treatment and their variances from mixed model
    exp_timepoints <- unique(dat$exposure_time[dat$exposure_time != 0])
    max_exp_timepoint <- max(exp_timepoints)
    re_treatment <- lme4::ranef(model_teh_mixed)$exposure_time

    re_var <- attr(lme4::ranef(model_teh_mixed, condVar = TRUE)$exposure_time, "postVar")[1,1,]
    re_se <- sqrt(re_var)

    # Extract fixed treatment effect from mixed model
    fe_treatment <- summary_teh$coefficients["treatment",1]

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = unique(dat$exposure_time)[unique(dat$exposure_time)!=0],
      est = rep(fe_treatment, length(exp_timepoints)) + re_treatment$treatment, # Let's double-check this; this may have broken
      se = NA,
      vcov = NA,
      ci_upper = NA,
      ci_lower = NA
    )

    if(estimand_type == "TATE") {

      # Estimate the TATE
      tate_est <- summary_teh$coefficients["treatment",1]
      tate_se <- summary_teh$coefficients["treatment",2]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se

      results <- list(
        model = model_teh_mixed,
        model_type = "teh_mixed",
        estimand_type = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci,
        converged = performance::check_convergence(model_teh_mixed)[1],
        messages = model_teh_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )

    } else if(estimand_type == "PTE") {

      # Estimate the PTE by combining the estimates from the fixed effect component and the random effect for the final timepoint
      re_treatment_pte <- re_treatment[rownames(re_treatment) == as.character(estimand_time), "treatment"]
      pte_est <- lme4::fixef(model_teh_mixed)["treatment"] + re_treatment_pte

      # Estimate the SE of the PTE by combining the variances from the fixed effect component and the random effect for the final timepoint
      re_se_pte <- re_se[estimand_time]

      pte_se <- sqrt(summary_teh$coefficients["treatment",2]^2 + re_se_pte^2)
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se

      results <- list(
        model = model_teh_mixed,
        model_type = "teh_mixed",
        estimand_type = "PTE",
        te_est = pte_est,
        te_se = pte_se,
        te_ci = pte_ci,
        converged = performance::check_convergence(model_teh_mixed)[1],
        messages = model_teh_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )

    }

  } else if(method == "mixed" & exp_time == "NCS") {


    ############################################.
    ##### Natural Cubic Spline (NCS) mixed model #####
    ############################################.

    # Create the spline basis
    S <- max(dat$exposure_time)
    knots_exp <- seq(0, S, length.out=n_knots_exp)
    ns_basis <- splines::ns(
      x = dat$exposure_time,
      knots = knots_exp[2:(n_knots_exp-1)],
      intercept = TRUE,
      Boundary.knots = knots_exp[c(1,n_knots_exp)]
    )

    dat$b <- rep(NA, nrow(dat))
    for (i in 1:n_knots_exp) {
      dat[[paste0("b", i)]] <- ns_basis[,i] * dat$treatment
    }

    # Fit mixed model
    formula <- paste0(f_out, f_cal, paste0("b", 1:n_knots_exp, collapse = " + "), f_re)

    if(family$family == "gaussian" & family$link == "identity") {
      model_ncs_mixed <- lme4::lmer(formula, data=dat, offset=offset)
    } else {
      model_ncs_mixed <- lme4::glmer(formula, family=family, data=dat,
                                     offset=offset)
    }

    summary_ncs <- summary(model_ncs_mixed)

    # Specify the indices corresponding to the spline terms
    indices <- grep("^b[0-9]+$", rownames(summary_ncs$coefficients))
    index_max <- length(indices)

    # Extract coefficient estimates and covariance matrix corresponding to spline
    # terms
    coeffs_spl <- summary_ncs$coefficients[,1][indices]
    cov_mtx_spl <- stats::vcov(model_ncs_mixed)[indices,indices]

    # Get number of unique (non-zero) exposure times
    exp_timepoints <- unique(dat$exposure_time[dat$exposure_time != 0])
    num_exp_timepoints <- length(exp_timepoints)
    max_exp_timepoint <- max(exp_timepoints)

    # Transform the spline terms into effect curve estimates (+ covariance matrix)
    B <- as.matrix(splines::ns(
      x = c(1:S),
      knots = knots_exp[2:(n_knots_exp-1)],
      intercept = TRUE,
      Boundary.knots = knots_exp[c(1,n_knots_exp)]
    ))

    class(B) <- "matrix"

    coeffs_trans <- as.numeric(B %*% coeffs_spl)
    cov_mtx <- B %*% cov_mtx_spl %*% t(B)
    se_ncs <- sqrt(diag(matrix(cov_mtx, nrow = nrow(cov_mtx))))


    # Calculate the CI for treatment effect at each exposure time
    ci_lower_ncs <- coeffs_trans - 1.96 * se_ncs
    ci_upper_ncs <- coeffs_trans + 1.96 * se_ncs

    # Estimate the effect curve
    effect_curve <- list(
      exp_time = unique(dat$exposure_time)[unique(dat$exposure_time)!=0],
      est = coeffs_trans,
      se = se_ncs,
      vcov = cov_mtx,
      ci_lower = ci_lower_ncs,
      ci_upper = ci_upper_ncs
    )

    if(estimand_type == "TATE") {
      # Estimate the TATE
      num_estimand_timepoints <- estimand_time[2] - estimand_time[1] + 1
      M <- matrix(c(rep(0, estimand_time[1] - 1),
                    rep(1 / num_estimand_timepoints, num_estimand_timepoints),
                    rep(0, max_exp_timepoint - estimand_time[2])
                    ),
                  nrow = 1)
      tate_est <- (M %*% coeffs_trans)[1]
      tate_se <- (sqrt(M %*% cov_mtx %*% t(M)))[1,1]
      tate_ci <- tate_est + c(-1.96,1.96) * tate_se

      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand_type = "TATE",
        te_est = tate_est,
        te_se = tate_se,
        te_ci = tate_ci,
        converged = performance::check_convergence(model_ncs_mixed)[1],
        messages = model_ncs_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )
    } else if(estimand_type == "PTE") {

      # Estimate the PTE
      pte_est <- as.numeric(coeffs_trans[estimand_time])
      pte_se <- sqrt(cov_mtx[estimand_time, estimand_time])
      pte_ci <- pte_est + c(-1.96,1.96) * pte_se

      results <- list(
        model = model_ncs_mixed,
        model_type = "ncs_mixed",
        estimand_type = "PTE",
        te_est = pte_est,
        te_se = pte_se,
        te_ci = pte_ci,
        converged = performance::check_convergence(model_ncs_mixed)[1],
        messages = model_ncs_mixed@optinfo$conv$lme4$messages,
        effect_curve = effect_curve
      )
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
      te_ci = te_ci
    )
  } else if(method == "GEE" & exp_time == "ETI") {


    ###################################################.
    ##### Exposure Time Indicator (ETI) GEE model #####
    ###################################################.

    # Fit GEE model
    formula <- paste0(f_out, f_cal, "factor(exposure_time)")
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
        te_ci = tate_ci
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
        te_ci = pte_ci
      )
      #
      # # Estimate the effect curve
      # curve_eti <- as.numeric(c(0, coeffs))

    }

  }

  class(results) <- c("list", "sw_analysis")

  return(results)

}
