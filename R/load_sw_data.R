#' Load and format data object
#'
#' @param period A character string; the name of the numeric variable representing the discrete time period of the observation.
#' @param cluster_id A character string; the name of the numeric variable identifying the cluster.
#' @param individual_id A character string (optional); the name of the numeric variable identifying the individual.
#' @param treatment A character string; the name of the binary variable indicating treatment. Accepts either integer (0/1) or Boolean (T/F) values.
#' @param covariates A character vector; the names of the covariate columns. Columns values should be either numeric, binary, or factors. Character columns will be converted into factors.
#' @param outcome A character string; the name of the numeric or binary variable indicating outcome. Accepts either numeric or Boolean (T/F) values.
#' @param data A dataframe containing the stepped wedge trial data.
#' @param time_type A character string describing how time is accounted for: 'discrete' (default) or 'continuous'
#'
#' @return An object of class \code{sw_data}
#' @export
#'
#' @examples
#' load_sw_data(period = "period", cluster_id = "id", individual_id = NULL,
#' treatment = "treatment", covariates = NULL, outcome = "y_bin",
#' data = geeCRT::sampleSWCRTLarge)
load_sw_data <- function(
    period, cluster_id, individual_id = NULL, treatment, covariates = NULL,
    outcome, time_type = "discrete", data
) {

  ########## David - work on individual_id and covariates as optional inputs ######

  # To prevent R CMD CHECK notes # David question
  .covariates <- .period <- .cluster_id <- .individual_id <- first_exposure <- NULL
  rm(.covariates,.period,.cluster_id,.individual_id,first_exposure)

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
    # David question - added this error message for tibble dataframe
    if (methods::is(data,"tbl_df")) { stop("`data` must be a non-tibble data frame.") }
    if (nrow(data)==0) { stop("`data` is an empty data frame.") }

    # Validate: `time_type`
    if (!(time_type %in% c("discrete", "continuous"))) {
      stop(paste0(
        "`time_type` must be a character string specifying `discrete` or `continuous`."
      ))
    }

    for (arg in c("period", "cluster_id", "treatment",
                  "covariates", "individual_id", "outcome")) {

      var <- get(arg)


      # Variable is a character string specifying variable(s) in `data`
      is_string <- methods::is(var, "character")
      length_one <- as.logical(length(var) == 1)
      in_df <- all(as.logical(var %in% names(data)))
      if (arg == "covariates") {
        if (!is.null(covariates) && !(is_string && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a vector of character strings spe",
              "cifying one or more variables in `data`."
            )
          )
        }
      } else if (arg == "individual_id") {
        if (!is.null(individual_id) && !(is_string && length_one && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a character string specifying a s",
              "ingle variable in `data`."
            )
          )
        }
      } else {
        if (!(is_string && length_one && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a character string specifying a s",
              "ingle variable in `data`."
            )
          )
        }
      }

      # Assign column(s) to val
      if (arg == "covariates") {
        val <- data[, var, drop = F]
        val2 <- list()
      } else {
        val <- data[, var]
      }

      # Validate: `period`
      if (arg %in% c("period")) {
        if (!is.numeric(val)) {
          stop(paste0("`", arg, "` must be numeric."))
        }
      }

      # David question - what types do we want to allow for treatment and outcome?
      # Separate treatment (binary) and outcome (and detect whether binary/count/continuous)
      # Validate: `treatment`
      if (arg %in% c("treatment")) {
        if (any(!(val %in% c(0, 1, F, T, NA)))) {
          stop(paste0(
            "`",
            arg,
            "` must only contain binary values (either T",
            "/F or 1/0)."
          ))
        }
      }

      # Validate: `outcome`
      if (arg %in% c("outcome")) {
        if (any(!(val %in% c(0, 1, F, T) | is.numeric(val)))) {
          stop(paste0(
            "`",
            arg,
            "` must only contain numeric or binary values (either T",
            "/F or 1/0)."
          ))
        }
      }

      assign(x = paste0(".", arg), value = val)

    }

  }


  # Convert binary variables to integers (if specified as boolean)
  .outcome <- as.integer(.outcome)
  .treatment <- as.integer(.treatment)


  .dim_x <- length(.covariates)


  # David question - do we want to rename covariates?
  # Rename covariate dataframe to c("x1", "x2", ...)
  # names(.covariates) <- paste0("x", c(1:.dim_x))


  # Create data object
  dat <- cbind(
    .covariates,
    "outcome" = .outcome,
    "period" = .period,
    "cluster_id" = .cluster_id,
    "individual_id" = .individual_id,
    "treatment" = .treatment,
    "time_type" = time_type
  )

  # Test whether, for all observations with a particular value of cluster_id and period, the value of treatment is the same
  # If not, throw an error
  # if (any(duplicated(dat[, c("cluster_id", "period")]) & duplicated(dat[, c("cluster_id", "period", "treatment")]))) {
  #   stop("For each unique combination of `cluster_id` and `period`, the value of `treatment` must be the same.")
  # }

  dat2 <- dat %>%
    dplyr::distinct(cluster_id, period, treatment) %>%
    dplyr::group_by(cluster_id, period) %>%
    dplyr::filter(dplyr::n() > 1)

  if (nrow(dat2) > 0) {
    stop("Value of `treatment` variable must be the same for all observations in a given cluster-period.")
  }

  # Handle missing values
  # Ignore everything related to covariates for now, only check necessary columns for missingness
  # Give warning if anything necessary missing (x/y records contain missing data and are being dropped)
  dat_no_missing <- dat[stats::complete.cases(dat$outcome, dat$period, dat$cluster_id, dat$treatment), ]

  # Count number of rows dropped due to missing values
  num_dropped <- nrow(dat) - nrow(dat_no_missing)
  num_total <- nrow(dat)

  # Order cluster id factor levels by roll-out sequence of intervention,
  # and calculate exposure time for each cluster-period
  dat_return <- dat_no_missing %>%
    dplyr::group_by(cluster_id) %>%
    # create variable for first sw_step where each index_ward has no_we_exposure == 1
    dplyr::mutate(first_exposure = min(period[treatment == 1])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_id = forcats::fct_reorder(factor(cluster_id), first_exposure)) %>%
    dplyr::mutate(exposure_time = ifelse(treatment == 1,
                                  period - first_exposure + 1,
                                  0)) %>%
    dplyr::arrange(cluster_id) # necessary for some GEE analysis functions

  # Add attributes and class to data object, return data object
  n_clusters <- length(unique(dat_return$cluster_id))
  n_periods <- length(unique(dat_return$period))
  n_sequences <- length(unique(dat_return$first_exposure))
  attr(dat_return, "n_clusters") <- n_clusters
  attr(dat_return, "n_periods") <- n_periods
  attr(dat_return, "n_sequences") <- n_sequences

  class(dat_return) <- c("data.frame", "sw_dat")

  message(
    paste0(
      "Stepped wedge dataset loaded. ",
      stringr::str_to_title(time_type),
      " time design with ",
      n_clusters,
      " clusters, ",
      n_sequences,
      " sequences, and ",
      n_periods,
      " time points. ",
      num_dropped,
      "/",
      num_total,
      " rows were dropped due to missing values for `cluster_id`, `period`, `treatment`, or `outcome`."
    )
  )

  return(dat_return)

}


#### usethis::use_package(package="sticky", type="Imports", min_version=T)
#### Unit testing
#### usethis::use_test("test-core-functions")
#### usethis::use_vignette(name="basic", title="Basic package workflow")
#### usethis::use_testthat()
### Immediate need: design plot, discrete/continuous time, missing data
### Print method-# of clusters, # of time points, etc.


