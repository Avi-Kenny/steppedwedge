#' Load and format data object
#'
#' @param time A character string; the name of the numeric variable representing
#'     time. Time can be either discrete or continuous.
#' @param cluster_id A character string; the name of the numeric variable
#'     identifying the cluster.
#' @param individual_id A character string (optional); the name of the numeric
#'     variable identifying the individual.
#' @param treatment A character string; the name of the binary variable
#'     indicating treatment. Values must be either integers (0/1) or Boolean
#'     (T/F).
#' @param outcome Either a character string or a vector of two character strings;
#'     for a numeric or binary outcome, the single character string indicates
#'     the name of the numeric or binary outcome variable; for binomial outcome
#'     data, the vector of two character strings indicates the "# of successes"
#'     variable and the "# of trials" variable, respectively. Values in the
#'     outcome variable(s) must be either numeric or Boolean (T/F).
#' @param exposure_time A character string (optional); the name of the numeric
#'     variable identifying the exposure time variable. If this is not provided,
#'     the package will calculate exposure time automatically based on
#'     \code{treatment} and \code{time}.
#' @param offset A character string (optional); the name of the numeric
#'     variable specifying the offset. The offset is added to the linear
#'     predictor of every model fit by \code{\link{analyze}}. Rows with a
#'     missing offset are dropped.
#' @param data A dataframe containing the stepped wedge trial data.
#' @param time_type One of c("discrete", "continuous"); whether the model treats
#'     time as discrete or continuous.
#' @param covariates A character vector (optional); the names of variables in
#'     `data` to include as additive covariates in the mean model fit by
#'     \code{\link{analyze}}. Covariates can be individual-level or
#'     cluster-level and must be numeric, binary, or categorical. Typically,
#'     covariates are chosen because of presumed association with the outcome,
#'     which may increase the precision of treatment effect estimation.
#'
#' @return An object of class \code{sw_data}
#' @export
#'
#' @examples
#' example_data <- load_data(time = "period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_cont", offset = NULL, data = sw_data_example)
#' base::summary(example_data)
#'
#' example_data_binomial <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = c("numerator", "denominator"), data = sw_data_example_binom)
#' base::summary(example_data_binomial)
#'
#' # Including covariates in the mean model
#' sw_data_cov <- sw_data_example
#' sw_data_cov$x <- stats::rnorm(nrow(sw_data_cov))
#' example_data_cov <- load_data(time = "period", cluster_id = "cluster",
#' treatment = "trt", outcome = "outcome_cont", data = sw_data_cov,
#' covariates = "x")
#'
load_data <- function(
    time, cluster_id, individual_id=NULL, treatment, outcome,
    exposure_time=NULL, offset=NULL, time_type="discrete", data,
    covariates=NULL
) {

  # To prevent R CMD CHECK notes
  .time <- .cluster_id <- .individual_id <- .successes <- .trials <- first_exposure <- first_exposure_sort <- .exposure_time <- .offset <- NULL
  rm(.time,.cluster_id,.individual_id,.successes,.trials,first_exposure,.offset)

  outcome_length <- length(outcome)
  outcome_binomial <- dplyr::case_when(
    outcome_length == 1 ~ FALSE,
    outcome_length == 2 ~ TRUE
  )
  if (outcome_binomial == TRUE) {
    successes <- outcome[1]
    trials <- outcome[2]
    outcome <- NULL
  } else {
    successes <- NULL
    trials <- NULL
  }

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
    if (methods::is(data,"tbl_df")) { stop("`data` must be a non-tibble data frame.") }
    if (nrow(data)==0) { stop("`data` is an empty data frame.") }

    # Validate: `time_type`
    if (!(time_type %in% c("discrete", "continuous"))) {
      stop(paste0(
        "`time_type` must be a character string specifying `discrete` or `continuous`."
      ))
    }

    for (arg in c("time", "cluster_id", "treatment", "offset", "exposure_time",
                  "individual_id", "outcome", "successes", "trials")) {

      var <- get(arg)

      # Variable is a character string specifying variable(s) in `data`
      is_string <- methods::is(var, "character")
      length_one <- as.logical(length(var) == 1)
      in_df <- all(as.logical(var %in% names(data)))
      if (arg == "individual_id") {
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
      } else if (arg == "offset") {
        if (!is.null(offset) && !(is_string && length_one && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a character string specifying a s",
              "ingle variable in `data`."
            )
          )
        }
      } else if (arg == "exposure_time") {
        if (!is.null(exposure_time) && !(is_string && length_one && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a character string specifying a s",
              "ingle variable in `data`."
            )
          )
        }
      } else if (arg %in% c("outcome", "successes", "trials")) {
        if (!is.null(var) && !(is_string && length_one && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a character string specifying a s",
              "ingle variable in `data`."
            )
          )
        }
      } else if (arg %in% c("time", "cluster_id", "treatment")) {
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
      val <- data[, var]

      # Validate: `time`
      if (arg %in% c("time")) {
        if (!is.numeric(val)) {
          stop(paste0("`", arg, "` must be numeric."))
        }
      }

      # Validate: `exposure_time`
      if (!is.null(exposure_time) && arg %in% c("exposure_time")) {
        if (!is.numeric(val)) {
          stop(paste0("`", arg, "` must be numeric."))
        }
      }

      # Validate: `offset`
      if (!is.null(offset) && arg %in% c("offset")) {
        if (!is.numeric(val)) {
          stop(paste0("`", arg, "` must be numeric."))
        }
      }

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

      # Validate: `outcome` for non-binomial data
      if (arg %in% c("outcome") & outcome_binomial == FALSE) {
        if (any(!(val %in% c(0, 1, F, T) | is.numeric(val)))) {
          stop(paste0(
            "`",
            arg,
            "` must only contain numeric or binary values (either T",
            "/F or 1/0)."
          ))
        }
      }

      # Validate: `successes` and `trials` for binomial data
      if (arg %in% c("successes", "trials") & outcome_binomial == TRUE) {
        if (any(!is.numeric(val))) {
          stop(paste0(
            "`",
            arg,
            "` must only contain numeric values."
          ))
        }
      }

      assign(x = paste0(".", arg), value = val)

    }
  }

  # Validate: `successes` <= `trials` for each observation
  if (outcome_binomial == TRUE) {
    if (any(.successes > .trials)) {
      stop(paste0(
        "`successes` must be less than or equal to `trials` for all observations."
      ))
    }
  }

  if (!is.null(covariates)) {
    validate_covariates(
      covariates = covariates,
      data = data,
      design_vars = c(time, cluster_id, individual_id, treatment, outcome,
                      successes, trials, exposure_time, offset)
    )
  }


  # Convert binary variables to integers (if specified as boolean)
  if(typeof(.outcome) == "logical") {.outcome <- as.integer(.outcome)}
  if(typeof(.treatment) == "logical") {.treatment <- as.integer(.treatment)}

  # Create data object
  dat <- data.frame(
    "outcome" = .outcome,
    "successes" = .successes,
    "trials" = .trials,
    "time" = .time,
    "cluster_id" = .cluster_id,
    "individual_id" = .individual_id,
    "offset" = .offset,
    "treatment" = .treatment,
    "exposure_time" = .exposure_time
  )

  if (!is.null(covariates)) {
    dat[covariates] <- data[covariates]
  }

  attr(dat, "time_type") <- time_type

  # Test whether, for all observations with a particular value of cluster_id and time, the value of treatment is the same
  # If not, throw an error
  # if (any(duplicated(dat[, c("cluster_id", "time")]) & duplicated(dat[, c("cluster_id", "time", "treatment")]))) {
  #   stop("For each unique combination of `cluster_id` and `time`, the value of `treatment` must be the same.")
  # }
  dat2 <- dat %>%
    dplyr::group_by(cluster_id, time) %>%
    dplyr::mutate(avg_tx=mean(treatment))

  if (any(!(dat2$avg_tx %in% c(0,1)))) {
    warning("The value of the `treatment` variable is not the same for all observations in each cluster-period.")
  }

  # Handle missing values; rows missing any design variable or covariate are
  # dropped and reported in the load message
  complete <- stats::complete.cases(dat$outcome, dat$time, dat$cluster_id,
                                    dat$treatment)
  if (!is.null(offset)) {
    complete <- complete & !is.na(dat$offset)
  }
  if (!is.null(covariates)) {
    complete <- complete & stats::complete.cases(dat[covariates])
  }
  dat_no_missing <- dat[complete, ]

  if (!is.null(covariates)) {
    # Unused factor levels would produce all-zero columns in the GEE design
    dat_no_missing[covariates] <- lapply(
      dat_no_missing[covariates],
      function(x) if (is.factor(x)) droplevels(x) else x
    )
    check_covariate_aliasing(dat_no_missing, covariates)
  }

  # Count number of rows dropped due to missing values
  num_dropped <- nrow(dat) - nrow(dat_no_missing)
  num_total <- nrow(dat)

  # Order cluster id factor levels by roll-out sequence of intervention,
  # and calculate exposure time for each cluster-period
  dat_return <- dat_no_missing %>%
    dplyr::group_by(cluster_id) %>%
    # create variable for first period where cluster has treatment == 1
    dplyr::mutate(first_exposure = ifelse(max(treatment) > 0,
                                          min(time[treatment == 1]),
                                          NA)) %>%
    dplyr::mutate(first_exposure_sort = ifelse(is.na(first_exposure),
                                               0,
                                               first_exposure)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cluster_id = forcats::fct_reorder(factor(cluster_id), first_exposure_sort)) %>%
    dplyr::select(-first_exposure_sort) %>%
    dplyr::arrange(cluster_id) # necessary for some GEE analysis functions

  # If exposure_time is not provided, calculate it
  if (is.null(exposure_time)) {
    dat_return %<>% dplyr::mutate(
      exposure_time = ifelse(treatment==1, time-first_exposure+1, 0)
    )
  }

  # Add attributes and class to data object, return data object
  n_clusters <- length(unique(dat_return$cluster_id))
  n_times <- length(unique(dat_return$time))
  n_seq <- length(unique(dat_return$first_exposure))
  attr(dat_return, "n_clusters") <- n_clusters
  attr(dat_return, "n_times") <- n_times
  attr(dat_return, "n_seq") <- n_seq
  attr(dat_return, "binomial") <- outcome_binomial
  attr(dat_return, "covariates") <- covariates

  class(dat_return) <- c("data.frame", "sw_dat")

  msg <- paste0(
    "Stepped wedge dataset loaded. ",
    stringr::str_to_title(time_type),
    " time design with ",
    n_clusters,
    " clusters, ",
    n_seq,
    " sequences, and ",
    n_times,
    " time points."
  )
  if (!is.null(covariates)) {
    msg <- paste0(msg, " Covariates: ", paste(covariates, collapse = ", "), ".")
  }
  if (num_dropped>0) {
    vars_checked <- c("`cluster_id`", "`time`", "`treatment`", "`outcome`",
                      if (!is.null(offset)) "`offset`",
                      if (!is.null(covariates)) "covariates")
    n_vars <- length(vars_checked)
    vars_checked <- paste0(
      paste(vars_checked[-n_vars], collapse = ", "), ", or ",
      vars_checked[n_vars], "."
    )
    msg <- paste0(
      msg, " Note: ", num_dropped, "/", num_total,
      " rows were dropped due to missing values for ", vars_checked
    )
  }

  message(msg)

  return(dat_return)

}

validate_covariates <- function(covariates, data, design_vars) {

  if (!is.character(covariates) || length(covariates) == 0 ||
      anyNA(covariates)) {
    stop("`covariates` must be a character vector of variable names in `data`.")
  }
  if (anyDuplicated(covariates)) {
    stop("`covariates` must not contain duplicate names.")
  }

  fmt <- function(x) paste0("`", x, "`", collapse = ", ")

  not_in_data <- setdiff(covariates, names(data))
  if (length(not_in_data) > 0) {
    stop("The following `covariates` are not variables in `data`: ",
         fmt(not_in_data), ".")
  }

  design_overlap <- intersect(covariates, design_vars)
  if (length(design_overlap) > 0) {
    stop("The following `covariates` are already used as design variables ",
         "(time, cluster, individual, treatment, outcome, exposure time, or ",
         "offset) and cannot also be covariates: ", fmt(design_overlap), ".")
  }

  # Column names that load_data() and analyze() create internally
  reserved <- c("outcome", "successes", "trials", "time", "cluster_id",
                "individual_id", "offset", "treatment", "exposure_time",
                "first_exposure", "avg_tx", "ij", "j", "b",
                "delayed_treatment")
  reserved_pattern <- "^(j_[0-9]+|b[0-9]+|exp_[0-9]+|washout_[0-9]+)$"
  is_reserved <- covariates %in% reserved | grepl(reserved_pattern, covariates)
  if (any(is_reserved)) {
    stop("The following `covariates` names are reserved for internal use; ",
         "please rename them in `data`: ", fmt(covariates[is_reserved]), ".")
  }

  valid_type <- vapply(
    data[covariates],
    function(x) is.numeric(x) || is.logical(x) || is.factor(x) || is.character(x),
    logical(1)
  )
  if (!all(valid_type)) {
    stop("`covariates` must be numeric, logical, factor, or character ",
         "variables. Invalid: ", fmt(covariates[!valid_type]), ".")
  }

  invisible(TRUE)
}

check_covariate_aliasing <- function(dat, covariates) {

  is_relabelling <- function(x, design) {
    n_design <- length(unique(design))
    length(unique(x)) == n_design &&
      nrow(unique(data.frame(x, design))) == n_design
  }

  for (cov in covariates) {
    x <- dat[[cov]]
    if (is.numeric(x)) next
    if (is_relabelling(x, dat$cluster_id)) {
      stop("Covariate `", cov, "` is a one-to-one relabelling of the cluster ",
           "identifier. Including it would add cluster fixed effects to the ",
           "mean model, which is not supported; between-cluster variation is ",
           "handled by random effects (method = \"mixed\") or the working ",
           "correlation structure (method = \"GEE\").")
    }
    if (is_relabelling(x, dat$time)) {
      stop("Covariate `", cov, "` is a one-to-one relabelling of the time ",
           "variable. Calendar time is modelled via the `cal_time` argument ",
           "of analyze().")
    }
  }

  invisible(TRUE)
}





