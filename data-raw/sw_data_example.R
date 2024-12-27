set.seed(2278)

sw_data_example <- data.frame(cluster = 1:15) %>%
  # expand data_sw with period 1 through 6 for each cluster
  tidyr::expand(cluster, period = 1:6) %>%
  # generate cluster period sizes
  dplyr::mutate(cp_size = sample(15:30, n(), replace = TRUE))

sw_data_example %<>%
  # create individual observations for each cluster period
  tidyr::uncount(weights = cp_size) %>%
  # assign sequences
  dplyr::mutate(seq = ceiling(cluster / 3)) %>%
  # assign treatment indicator
  dplyr::mutate(trt = ifelse(period > seq, 1, 0)) %>%
  # generate cluster random effect
  group_by(cluster) %>%
  dplyr::mutate(cluster_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  # generate cluster-time random effect
  dplyr::group_by(cluster, period) %>%
  dplyr::mutate(cluster_time_re = rnorm(n = 1, mean = 0, sd = 0.1)) %>%
  dplyr::ungroup() %>%
  # generate binary outcomes
  dplyr::rowwise() %>%
  dplyr::mutate(prob = max(c(0, min(c(1, 0.2 + cluster_re + cluster_time_re))))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(outcome_bin = rbinom(n = n(), size = 1, prob = prob)) %>%
  # generate continuous outcomes
  dplyr::mutate(outcome_cont = rnorm(n = n(), mean = 0, sd = 1) + cluster_re + cluster_time_re) %>%
  select(-c(seq, dplyr::ends_with("_re"))) %>%
  # convert to non-tibble dataframe
  data.frame()

usethis::use_data(sw_data_example, overwrite = TRUE)
