#' Title
#'
#' @param data A dataframe containing the stepped wedge trial data.
#'
#' @return A list with the number of clusters, number of periods, a summary of the data, and a plot of the stepped wedge design.
#' @export
#'
#' @examples TO DO
plot_sw_design <- function(data)
{
  num_clusters <- attr(data, "n_clusters")
  num_periods <- attr(data, "n_periods")
  num_sequences <- attr(data, "n_sequences")
  data <- data.frame(data)
  # num_clusters <- length(unique(data$cluster_id))

  num_periods <- length(unique(data$period))
  # Create a data frame with positions and shading status
  sw_data <- data %>%
    select(cluster_id, period, treatment) %>%
    group_by(cluster_id, period) %>%
    mutate(n = n()) %>%
    distinct()
  summary <- summary(sw_data)

  # Plot the grid using ggplot2
  design_plot <-  ggplot2::ggplot(sw_data, aes(x = period, y = fct_rev(cluster_id),
                               fill = as.character(treatment))) +
    geom_tile(color = "black") +
    scale_fill_manual(values = c("1" = "grey", "0" = "white")) +
    # reverse order of y axis and label every integer on y axis
    scale_x_continuous(breaks = seq(min(sw_data$period), max(sw_data$period), by = 1)) +
    geom_text(aes(label = n), vjust = 1) +
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = "SW design",
         fill = "Treatment",
         caption = "*Number in each cell represents sample size for the cluster-period.",
         x = "Period",
         y = "Cluster ID") +
    # move legend to bottom
    theme(legend.position = "bottom")

  return(list(num_clusters = num_clusters,
              num_periods = num_periods,
              num_sequences = num_sequences,
              summary = summary,
              design_plot = design_plot))
}
