readSWdata <- function(df, outcome_type)
{
  summary <- summary(df)
  num_clusters <- length(unique(df$id))
  num_periods <- length(unique(df$period))
  return(list(num_clusters = num_clusters, num_periods = num_periods, summary = summary))
}
