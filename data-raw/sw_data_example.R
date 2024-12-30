set.seed(2278)
expit <- function(x) { 1 / (1+exp(-x))}

sw_data_example <- data.frame(cluster = 1:15) %>%
  # expand data_sw with period 1 through 6 for each cluster
  tidyr::expand(cluster, period = 1:6) %>%
  # generate cluster period sizes
  dplyr::mutate(cp_size = sample(15:30, dplyr::n(), replace = TRUE))

sw_data_example %<>%
  # create individual observations for each cluster period
  tidyr::uncount(weights = cp_size) %>%
  # assign sequences
  dplyr::mutate(seq = ceiling(cluster / 3)) %>%
  # assign treatment indicator
  dplyr::mutate(trt = ifelse(period > seq, 1, 0)) %>%
  # generate cluster random effect
  dplyr::group_by(cluster) %>%
  dplyr::mutate(cluster_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  # generate cluster-time random effect
  dplyr::group_by(cluster, period) %>%
  dplyr::mutate(cluster_time_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  dplyr::ungroup() %>%
  # generate binary outcomes
  dplyr::rowwise() %>%
  dplyr::mutate(prob = expit(0.2 + cluster_re + cluster_time_re)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(outcome_bin = rbinom(n = dplyr::n(), size = 1, prob = prob)) %>%
  # generate continuous outcomes
  dplyr::mutate(outcome_cont = rnorm(n = dplyr::n(), mean = (cluster_re+cluster_time_re), sd = 1)) %>%
  dplyr::select(-c(seq, dplyr::ends_with("_re"))) %>%
  # convert to non-tibble dataframe
  data.frame()

usethis::use_data(sw_data_example, overwrite = TRUE)
