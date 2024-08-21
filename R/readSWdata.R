readSWdata <- function(df, outcome_type)
{
  summary <- summary(df)
  num_clusters <- length(unique(df$id))
  num_periods <- length(unique(df$period))
  # Create a data frame with positions and shading status
  sw_data <- df %>%
    select(id, period, treatment) %>%
    distinct()

  # Plot the grid using ggplot2
  design_plot <- ggplot2::ggplot(sw_data, aes(x = period, y = id, fill = as.character(treatment))) +
    geom_tile(color = "black") +
    scale_fill_manual(values = c("1" = "grey", "0" = "white")) +
    # reverse order of y axis and label every integer on y axis
    scale_y_reverse(breaks = 1:max(sw_data$id)) +
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = "SW design",
         fill = "Treatment")
  return(list(num_clusters = num_clusters,
              num_periods = num_periods,
              summary = summary,
              design_plot = design_plot))
}
