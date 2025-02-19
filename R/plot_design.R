#' Plot stepped wedge design
#'
#' @param dat A dataframe containing the stepped wedge trial data.
#'
#' @return A list with a plot of the stepped wedge design.
#' @export
#'
#' @examples
#' # Load data
#' example_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_example)
#' # Plot design
#' plot_design(example_data)
plot_design <- function(dat)
{

  # Prevent R CMD CHECK note
  aes <- cluster_id <- first_exposure <- n <- time <- treatment <- x <- NULL
  rm(aes,cluster_id,first_exposure,n,time,treatment,x)

  # Input validation
  if (!methods::is(dat,"sw_dat")) { stop("`dat` must be of class `sw_dat`.") }

  num_clusters <- attr(dat, "n_clusters")
  num_times <- attr(dat, "n_times")
  num_seq <- attr(dat, "n_seq")
  dat <- data.frame(dat)
  
  num_times <- length(unique(dat$time))
  # Create a data frame with positions and shading status
  sw_data <- dat %>%
    dplyr::select(cluster_id, time, treatment) %>%
    dplyr::group_by(cluster_id, time) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::distinct()
  summary <- summary(sw_data)

  # Plot the grid using ggplot2
  design_plot <-  ggplot2::ggplot(sw_data, ggplot2::aes(x = time, y = forcats::fct_rev(cluster_id),
                               fill = as.character(treatment))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("1" = "grey", "0" = "white")) +
    # reverse order of y axis and label every integer on y axis
    ggplot2::scale_x_continuous(breaks = seq(min(sw_data$time), max(sw_data$time), by = 1)) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "SW design",
         fill = "Treatment",
         caption = "*Number in each cell represents sample size for the cluster-time.",
         x = "Time",
         y = "Cluster ID") +
    # move legend to bottom
    ggplot2::theme(legend.position = "bottom")

  return(list(design_plot = design_plot))
}
