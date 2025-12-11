set.seed(2278)

sw_data_example_binom <- data.frame(cluster = 1:15) %>%
  # expand data_sw with period 1 through 6 for each cluster
  tidyr::expand(cluster, period = 1:6) %>%
  # generate cluster period sizes
  dplyr::mutate(cp_size = sample(15:30, dplyr::n(), replace = TRUE))

# treatment effects
b_bin <- log(2)
b_cont <- 0.5

sw_data_example_binom %<>%
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
  # generate probability for each cluster period
  dplyr::mutate(prob = plogis(0.2 + b_bin * trt + cluster_re + cluster_time_re)) %>%
  # generate binomial outcomes
  dplyr::mutate(denominator = cp_size) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(numerator = rbinom(1, denominator, prob)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(seq, cp_size, prob, dplyr::ends_with("_re"))) %>%
  # convert to non-tibble dataframe
  data.frame()


usethis::use_data(sw_data_example_binom, overwrite = TRUE)
