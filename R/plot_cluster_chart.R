#' Plot cluster chart
#'
#' @param analysis_object A list of class `sw_analysis`.
#'
#' @returns A list with a plot of the actual and predicted outcomes by cluster.
#' @export
#'
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#'
#' # Analyze using TATE estimand for exposure times 1 through 4
#' results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI")
#' 
#' # Plot by cluster
#' plot_cluster_chart(results_tate)
#' 
plot_cluster_chart <- function(analysis_object)
{
  # Prevent R CMD CHECK note
  outcome <- preds <- time <- treatment <- cluster <- dx <- n_in_group <- y_ref <- NULL
  rm(outcome,preds,time,treatment, cluster, dx, n_in_group, y_ref)
  
  # Input validation
  if (!methods::is(analysis_object,"sw_analysis")) { stop("`analysis_object` must be of class `sw_analysis`.") }
  
  dat <- analysis_object$dat
  
  dat$preds <- as.numeric(stats::predict(analysis_object$model))

  dat$cluster <- factor(paste("Cluster", dat$cluster_id))

  cluster_order <- order(as.numeric(unique(gsub("Cluster ", "", dat$cluster))))
  dat$cluster <- factor(dat$cluster, levels = paste("Cluster", cluster_order))
  
  
  # Prep for line segment for clusters with a single timepoint at a particular treatment level
  dat_seg <- dat %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(dx = 0.2) %>%  
    dplyr::group_by(cluster, treatment) %>%
    dplyr::mutate(n_in_group = dplyr::n_distinct(time)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_in_group == 1)   # keep only singleton groups for the segment layer
  
  cluster_chart <- ggplot2::ggplot(dat, ggplot2::aes(x=time, y=outcome, color=factor(treatment))) +
    ggplot2::geom_jitter(alpha=0.5, width = 0.1) +
    ggplot2::geom_line(ggplot2::aes(y=preds), linewidth=1) +
    # short horizontal segment for singleton groups
    ggplot2::geom_segment(
      data = dat_seg,
      ggplot2::aes(x = time - dx, xend = time + dx, y = preds, yend = preds,
                   group = factor(treatment), color = factor(treatment)),
      linewidth = 1,
      lineend = "round"
    ) +
    ggplot2::labs(color="Treatment", x="Time", y="Outcome") +
    ggplot2::scale_color_manual(values=c("#E69F00", "#009E73")) +
    ggplot2::facet_wrap(~cluster, ncol=4) +
    # move legend to bottom
    ggplot2::theme(legend.position = "bottom")
  
  return(list(cluster_chart = cluster_chart))

}