set.seed(2278)

sw_data_example <- data.frame(cluster = 1:15) %>%
  # expand data_sw with period 1 through 6 for each cluster
  tidyr::expand(cluster, period = 1:6) %>%
  # generate cluster period sizes
  dplyr::mutate(cp_size = sample(15:30, dplyr::n(), replace = TRUE))

# treatment effects
b_bin <- log(2)
b_cont <- 0.5

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
  dplyr::mutate(prob = plogis(0.2 + b_bin * trt + cluster_re + cluster_time_re)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(outcome_bin = rbinom(n = dplyr::n(), size = 1, prob = prob)) %>%
  # generate continuous outcomes
  dplyr::mutate(outcome_cont = rnorm(n = dplyr::n(), 
                                     mean = (b_cont * trt + cluster_re+cluster_time_re),
                                     sd = 1)) %>%
  # generate binomial outcomes
  mutate(denominator = floor(runif(nrow(.), min = 5, max = 15))) %>%
  rowwise() %>%
  mutate(numerator = rbinom(1, denominator, prob)) %>%
  ungroup() %>%
  dplyr::select(-c(seq, dplyr::ends_with("_re"))) %>%
  # convert to non-tibble dataframe
  data.frame()

usethis::use_data(sw_data_example, overwrite = TRUE)
