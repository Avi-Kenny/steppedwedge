#' Plot stepped wedge design
#'
#' @param data A dataframe containing the stepped wedge trial data.
#'
#' @return A list with the number of clusters, number of periods, a summary of the data, and a plot of the stepped wedge design.
#' @export
#'
#' @examples
#' # TO DO
plot_sw_design <- function(data)
{

  # Prevent R CMD CHECK note
  aes <- cluster_id <- first_exposure <- n <- period <- treatment <- NULL
  x <- geeCRT::sampleSWCRTLarge
  rm(aes,cluster_id,first_exposure,n,period,treatment,x)

  num_clusters <- attr(data, "n_clusters")
  num_periods <- attr(data, "n_periods")
  num_sequences <- attr(data, "n_sequences")
  data <- data.frame(data)
  # num_clusters <- length(unique(data$cluster_id))

  num_periods <- length(unique(data$period))
  # Create a data frame with positions and shading status
  sw_data <- data %>%
    dplyr::select(cluster_id, period, treatment) %>%
    dplyr::group_by(cluster_id, period) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::distinct()
  summary <- summary(sw_data)

  # Plot the grid using ggplot2
  design_plot <-  ggplot2::ggplot(sw_data, ggplot2::aes(x = period, y = forcats::fct_rev(cluster_id),
                               fill = as.character(treatment))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("1" = "grey", "0" = "white")) +
    # reverse order of y axis and label every integer on y axis
    ggplot2::scale_x_continuous(breaks = seq(min(sw_data$period), max(sw_data$period), by = 1)) +
    ggplot2::geom_text(aes(label = n), vjust = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "SW design",
         fill = "Treatment",
         caption = "*Number in each cell represents sample size for the cluster-period.",
         x = "Period",
         y = "Cluster ID") +
    # move legend to bottom
    ggplot2::theme(legend.position = "bottom")

  return(list(num_clusters = num_clusters,
              num_periods = num_periods,
              num_sequences = num_sequences,
              summary = summary,
              design_plot = design_plot))
}
