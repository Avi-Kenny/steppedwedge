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
  # generate binary outcomes
  dplyr::mutate(outcome_bin = rbinom(n = n(), size = 1, prob = 0.2)) %>%
  # generate continuous outcomes
  dplyr::mutate(outcome_cont = rnorm(n = n(), mean = 0, sd = 1)) %>%
  select(-seq) %>%
  # convert to non-tibble dataframe
  data.frame()

usethis::use_data(sw_data_example, overwrite = TRUE)
