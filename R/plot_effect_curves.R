#' Plot effect estimates by exposure time for one or more models.
#'
#' @param ... One or more objects of class \code{"sw_analysis"} returned by
#'     \code{\link{analyze}}.
#' @param labels A character vector of length equal to the length of list(...),
#'     representing plot labels. Only used if length(list(...))>1.
#' @param facet_nrow Number of rows for displaying plots using 
#'     ggplot2::facet_wrap().
#' @param continuous Logical; if TRUE, models fit with Natural Cubic Splines 
#'     (exp_time = "NCS") will be evaluated and plotted as smooth, continuous curves.
#'     Models using discrete exposure times (e.g., ETI, DCT) will remain discrete.
#' @param resolution Numeric; the step size for generating the continuous curve. 
#'     Defaults to 0.1. Only relevant if continuous = TRUE.
#' @return A plot of the effect curve for each \code{"sw_analysis"} object 
#'     passed to the function.
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#'
#' \donttest{
#' IT_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "IT")
#' ETI_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI")
#' NCS_4_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "NCS", advanced = params(n_knots_exp = 4))
#' 
#' # Plot discrete models
#' plot_effect_curves(IT_model, NCS_4_model, ETI_model, facet_nrow = 1)
#' 
#' # Plot continuous NCS model alongside discrete models
#' plot_effect_curves(IT_model, NCS_4_model, ETI_model, facet_nrow = 1, continuous = TRUE)
#' }
#' @export
plot_effect_curves <- function(..., labels = NA, facet_nrow = 1, continuous = FALSE, resolution = 0.1) {
  
  # Errors
  objs <- list(...)
  if (length(objs) == 0) {
    stop(paste0("One or more objects of class 'sw_analysis' must be passed int",
                "o `plot_effect_curves`."))
  }
  
  # To prevent R CMD CHECK notes
  x <- y <- ci_lower <- ci_upper <- curve <- ymin <- ymax <- y_ref <- NULL
  rm(x, y, ci_lower, ci_upper, curve, ymin, ymax, y_ref)
  
  if (missing(labels) || is.na(labels[1])) { 
    labels <- sapply(substitute(list(...))[-1], deparse)
  }
  
  # Build the plotting dataframe directly
  df_plot <- data.frame()
  
  for (i in seq_along(objs)) {
    obj <- objs[[i]]
    lbl <- labels[i]
    
    if (!methods::is(obj, "sw_analysis")) {
      stop("One or more of the objects passed is not of class 'sw_analysis'.")
    }
    
    # --- NEW LOGIC: Generate high-resolution data for NCS models if requested ---
    if (continuous == TRUE && obj$exp_time == "NCS") {
      
      S <- max(obj$dat$exposure_time)
      
      # Safely extract coefficients depending on model type
      if (inherits(obj$model, "merMod")) {
        coeffs <- lme4::fixef(obj$model)
      } else {
        coeffs <- stats::coef(obj$model)
      }
      
      spline_vars <- grep("^b[0-9]+$", names(coeffs), value = TRUE)
      
      # Use stored knots if available; fall back to reverse-engineering for
      # objects created before knots_exp was stored in results
      if (!is.null(obj$knots_exp)) {
        knots_exp <- obj$knots_exp
      } else {
        knots_exp <- seq(0, S, length.out = length(spline_vars))
      }
      x_continuous <- seq(1, S, by = resolution) # Start at 1 to enforce structural zero
      
      B_cont <- as.matrix(splines::ns(
        x = x_continuous,
        knots = knots_exp[2:(length(knots_exp) - 1)],
        intercept = TRUE,
        Boundary.knots = knots_exp[c(1, length(knots_exp))]
      ))
      
      coeffs_spl <- coeffs[spline_vars]
      
      # Safely extract covariance matrix, converting S4 to base matrix
      if ("V_orig" %in% names(obj)) {
        cov_mtx_spl <- as.matrix(obj$V_orig[spline_vars, spline_vars])
      } else {
        cov_mtx_spl <- as.matrix(stats::vcov(obj$model)[spline_vars, spline_vars])
      }
      
      # Calculate continuous estimates and SEs
      est_cont <- as.numeric(B_cont %*% coeffs_spl)
      se_cont <- sqrt(rowSums((B_cont %*% cov_mtx_spl) * B_cont))
      
      ci_lower_cont <- est_cont - 1.96 * se_cont
      ci_upper_cont <- est_cont + 1.96 * se_cont
      
      # Handle exponentiation
      if (obj$exponentiated) {
        est_cont <- exp(est_cont)
        ci_lower_cont <- exp(ci_lower_cont)
        ci_upper_cont <- exp(ci_upper_cont)
        zero_value <- 1
      } else {
        zero_value <- 0
      }
      
      df_add <- data.frame(
        x = c(0, x_continuous),         # Manually prepend structural zero at t=0
        y = c(zero_value, est_cont),
        ci_lower = c(zero_value, ci_lower_cont),
        ci_upper = c(zero_value, ci_upper_cont),
        curve = lbl,
        exponentiated = obj$exponentiated,
        y_ref = ifelse(obj$exponentiated, 1, 0)
      )
      
    } else {
      # --- ORIGINAL LOGIC: Fallback for discrete models (ETI, DCT, IT) ---
      df_add <- data.frame(
        x = obj$effect_curve$exp_time,
        y = obj$effect_curve$est,
        ci_lower = obj$effect_curve$ci_lower,
        ci_upper = obj$effect_curve$ci_upper,
        curve = lbl,
        exponentiated = obj$exponentiated,
        y_ref = ifelse(obj$exponentiated, 1, 0)
      )
    }
    
    df_plot <- rbind(df_plot, df_add)
  }
  
  # Color scheme
  curve_colors <- c("deepskyblue3", "darkorchid3", "darkgreen", "darkorange",
                    "firebrick3", "darkgrey")
  
  df_ref <- unique(df_plot[c("curve", "y_ref")])
  
  # Set up ggplot2 object
  plot <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = x, y = y, color = curve, fill = curve)
  ) +
    ggplot2::geom_hline(
      data = df_ref,
      ggplot2::aes(yintercept = y_ref),
      linetype = "dashed",
      color = "grey60"
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=ci_lower, ymax=ci_upper), 
                         alpha = 0.05, linetype = "dotted") +
    ggplot2::facet_wrap(~ curve, nrow = facet_nrow) +
    ggplot2::labs(x="Exposure time", y="Effect estimate", color="",
                  fill="") +
    ggplot2::scale_color_manual(values=curve_colors) +
    ggplot2::scale_fill_manual(values=curve_colors) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color="#bbbbbb", fill=NA)
    )
  
  return(plot)
  
}


#' Create table of estimates
#'
#' @description Format estimates returned by \code{\link{analyze}} as a table
#' @param ... One or more objects of class \code{"sw_analysis"} returned by
#'     \code{\link{analyze}}.
#' @param labels A character vector of length equal to length(list(...))
#'     representing curve labels
#' @return A table of effect estimate values
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#'
#' \donttest{
#' IT_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "IT")
#' ETI_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI")
#' NCS_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "NCS")
#' ests_table <- as_table(IT_model, IT_model, NCS_model)
#' head(ests_table)
#' }
#' @export
as_table <- function(..., labels = NA) {
  
  df_ests <- data.frame(
    x = double(),
    y = double(),
    ci_lower = double(),
    ci_upper = double(),
    curve = character(),
    exponentiated = logical(),
    y_ref = double()
  )
  
  counter <- 1
  for (obj in list(...)) {
    if (!methods::is(obj, "sw_analysis")) {
      stop(paste0("One or more of the objects passed into `as_table` is not of ",
                  "class 'sw_analysis'."))
    }
    
    df_add <- data.frame(
      x = obj$effect_curve$exp_time,
      y = obj$effect_curve$est,
      ci_lower = obj$effect_curve$ci_lower,
      ci_upper = obj$effect_curve$ci_upper,
      curve = labels[counter],
      exponentiated = obj$exponentiated,
      y_ref = ifelse(obj$exponentiated, 1, 0)
    )
    
    df_ests <- rbind(df_ests,df_add)
    counter <- counter + 1
    
  }
  
  return(df_ests)
  
}




