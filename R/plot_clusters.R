#' Plot observed and predicted outcomes by cluster over time
#'
#' @param object An analysis object of class `sw_analysis` or a dataset of class `sw_data`/`sw_dat`.
#' @param ncol Integer; number of columns in the faceted plot. Defaults to 3.
#'
#' @returns A list with a `ggplot2` object of the actual (and predicted, if applicable) outcomes by cluster.
#' @export
#'
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#'
#' # Plot just the dataset
#' plot_clusters(test_data)
#' 
#' # Analyze using TATE estimand for exposure times 1 through 4
#' results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI")
#' 
#' # Plot analysis results (with predicted lines) by cluster
#' plot_clusters(results_tate)
#' 
plot_clusters <- function(object, ncol=3)
{
  # Prevent R CMD CHECK note
  outcome <- preds <- time <- treatment <- cluster <- dx <- n_in_group <- y_ref <-
    prop <- successes <- trials <- NULL
  rm(outcome,preds,time,treatment, cluster, dx, n_in_group, y_ref, prop, successes, 
     trials)
  
  # Input validation & Type Check
  is_analysis <- methods::is(object, "sw_analysis")
  is_data <- methods::is(object, "sw_dat") || methods::is(object, "sw_data")
  
  if (!is_analysis && !is_data) { 
    stop("`object` must be of class `sw_analysis` or `sw_dat`.") 
  }
  
  # Extract data and predictions if analysis object
  if (is_analysis) {
    dat <- object$dat
    dat$preds <- as.numeric(stats::predict(object$model, type = "response"))
  } else {
    dat <- object
  }
  
  # Record whether the plot will use proportions — applies to both binary (0/1) and
  # binomial (successes/trials) outcomes — before columns are dropped by distinct()
  is_binomial_outcome <- isTRUE(attr(dat, "binomial"))
  is_proportion_outcome <- is_binomial_outcome ||
    ("outcome" %in% names(dat) &&
       all(is.na(dat$outcome) | dat$outcome %in% c(0, 1, TRUE, FALSE)))
  
  dat$cluster <- factor(paste("Cluster", dat$cluster_id))
  
  cluster_order <- order(as.numeric(unique(gsub("Cluster ", "", dat$cluster))))
  dat$cluster <- factor(dat$cluster, levels = paste("Cluster", cluster_order))
  
  # Prep for line segment for clusters with a single timepoint at a particular treatment level
  # Only needed if we are drawing prediction lines
  if (is_analysis) {
    dat_seg <- dat %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(dx = 0.2) %>%  
      dplyr::group_by(cluster, treatment) %>%
      dplyr::mutate(n_in_group = dplyr::n_distinct(time)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_in_group == 1)   # keep only singleton groups for the segment layer
  }
  
  # Base Plots
  if("outcome" %in% names(dat)) {
    if(all(is.na(dat$outcome) | dat$outcome %in% c(0, 1, TRUE, FALSE))) {
      dat <- dat %>%
        dplyr::group_by(cluster, time) %>%
        dplyr::mutate(prop = mean(outcome))
      
      # Filter distinct safely depending on whether predictions exist
      if (is_analysis) {
        dat <- dat %>% dplyr::distinct(cluster, time, prop, preds, treatment)
      } else {
        dat <- dat %>% dplyr::distinct(cluster, time, prop, treatment)
      }
      
      cluster_chart <- ggplot2::ggplot(dat, ggplot2::aes(x=time, y=prop, color=factor(treatment))) +
        ggplot2::geom_jitter(alpha=0.5, width = 0, height = 0) +
        ggplot2::labs(color="Treatment", x="Time", y="Proportion with outcome") +
        ggplot2::scale_color_manual(values=c("#E69F00", "#009E73")) +
        ggplot2::facet_wrap(~cluster, ncol=ncol) +
        ggplot2::theme(legend.position = "bottom")
      
    } else {
      cluster_chart <- ggplot2::ggplot(dat, ggplot2::aes(x=time, y=outcome, color=factor(treatment))) +
        ggplot2::geom_jitter(alpha=0.5, width = 0.1, height = 0) +
        ggplot2::labs(color="Treatment", x="Time", y="Outcome") +
        ggplot2::scale_color_manual(values=c("#E69F00", "#009E73")) +
        ggplot2::facet_wrap(~cluster, ncol=ncol) +
        ggplot2::theme(legend.position = "bottom")
    }
    
  } else if("successes" %in% names(dat)) {
    dat <- dat %>%
      data.frame() %>%
      dplyr::group_by(cluster, time) %>%
      dplyr::mutate(prop = sum(successes) / sum(trials))
    
    if (is_analysis) {
      dat <- dat %>% dplyr::distinct(cluster, time, prop, preds, treatment)
    } else {
      dat <- dat %>% dplyr::distinct(cluster, time, prop, treatment)
    }
    
    cluster_chart <- ggplot2::ggplot(dat, ggplot2::aes(x=time, y=prop, color=factor(treatment))) +
      ggplot2::geom_jitter(alpha=0.5, width = 0.1, height = 0) +
      ggplot2::labs(color="Treatment", x="Time", y="Proportion with outcome") +
      ggplot2::scale_color_manual(values=c("#E69F00", "#009E73")) +
      ggplot2::facet_wrap(~cluster, ncol=ncol) +
      ggplot2::theme(legend.position = "bottom")
  }
  
  # Conditionally add prediction lines, segments, and context-specific captions
  if (is_analysis) {
    cluster_chart <- cluster_chart + 
      ggplot2::geom_line(ggplot2::aes(y=preds), linewidth=1) +
      ggplot2::geom_segment(
        data = dat_seg,
        ggplot2::aes(x = time - dx, xend = time + dx, y = preds, yend = preds,
                     group = factor(treatment), color = factor(treatment)),
        linewidth = 1,
        lineend = "round"
      )
    
    if (is_proportion_outcome) {
      cluster_chart <- cluster_chart + 
        ggplot2::labs(caption="Lines represent predicted probabilities; points represent actual proportions per cluster-period")
    }
  }
  
  return(list(cluster_chart = cluster_chart))
}