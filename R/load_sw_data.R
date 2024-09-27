#' Load and format data object
#'
#' @param period A character string; the name of the numeric variable representing the discrete time period of the observation.
#' @param cluster_id A character string; the name of the numeric variable identifying the cluster.
#' @param individual_id A character string (optional); the name of the numeric variable identifying the individual.
#' @param treatment A character string; the name of the binary variable indicating treatment. Accepts either integer (0/1) or Boolean (T/F) values.
#' @param covariates A character vector; the names of the covariate columns. Columns values should be either numeric, binary, or factors. Character columns will be converted into factors.
#' @param outcome A character string; the name of the numeric or binary variable indicating outcome. Accepts either numeric or Boolean (T/F) values.
#' @param data A dataframe containing the stepped wedge trial data.
#'
#' @return An object of class \code{sw_data}
#' @export
#'
#' @examples
#' load_sw_data(period = "period", cluster_id = "id", individual_id = NULL,
#' treatment = "treatment", covariates = NULL, outcome = "y_bin",
#' data = geeCRT::sampleSWCRTLarge)
load_sw_data <- function(
    period, cluster_id, individual_id = NULL, treatment, covariates = NULL, outcome, data
) {

  ########## David - work on individual_id and covariates as optional inputs ######

  # To prevent R CMD CHECK notes # David question
  .covariates <- .period <- .cluster_id <- .individual_id <- NULL; rm(.covariates, .period, .cluster_id, .individual_id);

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
    # David question - added this error message for tibble dataframe
    if (methods::is(data,"tbl_df")) { stop("`data` must be a non-tibble data frame.") }
    if (nrow(data)==0) { stop("`data` is an empty data frame.") }

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
    "treatment" = .treatment
  )

  # Handle missing values
  # Ignore everything related to covariates for now, only check necessary columns for missingness
  # Give warning if anything necessary missing (x/y records contain missing data and are being dropped)
  dat_no_missing <- dat[stats::complete.cases(dat$outcome, dat$period, dat$cluster_id, dat$treatment), ]

  # Count number of rows dropped due to missing values
  num_dropped <- nrow(dat) - nrow(dat_no_missing)
  num_total <- nrow(dat)

  message(paste0(num_dropped, "/", num_total, " rows were dropped due to missing values for `cluster_id`, `period`, `treatment`, or `outcome`."))


  # Create and return data object

  # David question - Do we want to assign a class and/or attributes?
  # maybe add attributes later
  # class(dat) <- c("data.frame", "steppedwedge_dat")
  # attr(dat, "groups") <- .groups
  # attr(dat, "covariate_names") <- x_names
  # attr(dat, "dim_x") <- .dim_x
  # attr(dat, "n") <- .n_v+.n_p
  # attr(dat, "n_vacc") <- .n_v
  # attr(dat, "n_vacc2") <- sum(df_v$z)
  # attr(dat, "n_plac") <- .n_p
  attr(dat, "n_clusters") <- length(unique(dat$cluster_id))
  attr(dat, "n_periods") <- length(unique(dat$period))

  dat <- dat %>%
    group_by(cluster_id) %>%
    # create variable for first sw_step where each index_ward has no_we_exposure == 1
    mutate(first_exposure = min(period[treatment == 1])) %>%
    ungroup() %>%
    mutate(cluster_id = fct_reorder(factor(cluster_id), first_exposure))

  class(dat) <- c("data.frame", "sw_dat")
  return(dat)

}


#### usethis::use_package(package="sticky", type="Imports", min_version=T)
#### Unit testing
#### usethis::use_test("test-core-functions")
#### usethis::use_vignette(name="basic", title="Basic package workflow")
#### usethis::use_testthat()
### Immediate need: design plot, discrete/continuous time, missing data
### Print method-# of clusters, # of time points, etc.


