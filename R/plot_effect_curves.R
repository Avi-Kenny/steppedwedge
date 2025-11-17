#' Plot effect estimates by exposure time for one or more models.
#'
#' @param ... One or more objects of class \code{"sw_analysis"} returned by
#'     \code{\link{analyze}}.
#' @param labels A character vector of length equal to the length of list(...),
#'     representing plot labels. Only used if length(list(...))>1.
#' @param facet_nrow Number of rows for displaying plots using 
#'     ggplot2::facet_wrap().
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
#' TEH_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "TEH")
#' plot_effect_curves(IT_model, NCS_4_model, ETI_model, TEH_model, facet_nrow = 1)
#' }
#' @export
plot_effect_curves <- function(..., labels = NA, facet_nrow = 1) {
  # Errors
  if (length(list(...)) == 0) {
    stop(paste0("One or more objects of class 'sw_analysis' must be passed int",
                "o `plot_effect_curves`."))
  }
  
  # To prevent R CMD CHECK notes
  x <- y <- ci_lower <- ci_upper <- curve <- ymin <- ymax <- y_ref <- NULL
  rm(x, y, ci_lower, ci_upper, curve, ymin, ymax, y_ref)
  
  if (missing(labels)) { labels <- sapply(substitute(list(...))[-1], deparse)
  
  }
  
  df_plot <- as_table(..., labels=labels)
  
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
#' TEH_model <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "TEH")
#' ests_table <- as_table(IT_model, IT_model, NCS_model, TEH_model)
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




