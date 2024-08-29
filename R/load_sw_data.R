#' Load and format data object
#'
#' @param period
#' @param cluster_id
#' @param individual_id
#' @param treatment
#' @param covariates
#' @param outcome
#' @param data
#'
#' @return
#' @export
#'
#' @examples
load_sw_data <- function(
    period, cluster_id, individual_id = NULL, treatment, covariates = NULL, outcome, data
) {

  ########## David - work on individual_id and covariates as optional inputs ######

  # To prevent R CMD CHECK notes # David question
  # .time <- .marker <- .covariates <- NULL; rm(.time,.marker,.covariates);

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
    # David question - added this error message for tibble dataframe
    if (methods::is(data,"tbl_df")) { stop("`data` must be a non-tibble data frame.") }
    if (nrow(data)==0) { stop("`data` is an empty data frame.") }

    for (arg in c("period", "cluster_id", "individual_id", "treatment",
                  "covariates", "outcome")) {

      var <- get(arg)


      # Variable is a character string specifying variable(s) in `data`
      is_string <- methods::is(var, "character")
      length_one <- as.logical(length(var) == 1)
      in_df <- all(as.logical(var %in% names(data)))
      if (arg == "covariates") {
        if (!(is_string && in_df)) {
          stop(
            paste0(
              "`",
              arg,
              "` must be a vector of character strings spe",
              "cifying one or more variables in `data`."
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

        # David question - Do we want to convert character/factor columns to
        # indicator columns as in the code below?
        # # Convert factor/character columns
        # for (col in names(val)) {
        #   if (is.numeric(val[, col])) {
        #     val2[[col]] <- val[, col]
        #   } else if (is.logical(val[, col])) {
        #     val2[[col]] <- as.integer(val[, col])
        #   } else if (is.factor(val[, col]) ||
        #              is.character(val[, col])) {
        #     tmp_col <- as.integer(as.factor(val[, col]))
        #     tmp_unique <- unique(tmp_col)
        #     tmp_size <- length(tmp_unique)
        #     for (i in c(1:(tmp_size - 1))) {
        #       col_new <- paste0(col, "_", i)
        #       val2[[col_new]] <- as.integer(tmp_col == tmp_unique[i])
        #     }
        #   } else {
        #     stop(paste0("The data type of column `", col,
        #                 "is ", class(val[, col])
        #                 # "` is not supported."
        #                 ))
        #   }
        # }
        # val <- as.data.frame(val2)
        # x_names <- names(val)
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
      # Currently allowing numeric, 0, 1, F, T for both
      # Validate: `treatment`, `outcome`
      if (arg %in% c("treatment", "outcome")) {
        if (any(!(val %in% c(0, 1, F, T) | is.numeric(val)))) {
          stop(paste0(
            "`",
            arg,
            "` must only contain numeric or binary values (either T",
            "/F or 1/0)."
          ))
        }
      }

      # David question - we want to allow missing values, right?
      # # No missing values allowed (except marker)
      # if (!(arg %in% c("marker", "weights", "covariates"))) {
      #   if (any(is.na(val))) {
      #     stop("NA values not allowed in `", arg, "`.")
      #   }
      # } else if (arg == "covariates") {
      #   cond_1 <- any(is.na(val))
      #   cond_2 <- any(is.na(val[as.logical(data[, ph2]), ]))
      #   msg_1 <- "NA values not allowed in `covariates` (if covariates_ph2==F)."
      #   msg_2 <- paste0("NA values only allowed in `covariates` for which ph2==F (if covariates_ph2==T).")
      #   if (cond_1 && !covariates_ph2) {
      #     stop(msg_1)
      #   }
      #   if (cond_2 && covariates_ph2) {
      #     stop(msg_2)
      #   }
      # }

      assign(x = paste0(".", arg), value = val)


    }

    }


  # Convert binary variables to integers (if specified as boolean)
  .outcome <- as.integer(.outcome)
  .treatment <- as.integer(.treatment)




  .dim_x <- length(.covariates)

  # David question - needed?
  # .n_v <- sum(.vacc)
  # .n_p <- sum(1-.vacc)

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




  # Create and return data object

  # David question - Do we want to assign a class and/or attributes?
  # class(dat) <- c("data.frame", "steppedwedge_dat")
  # attr(dat, "groups") <- .groups
  # attr(dat, "covariate_names") <- x_names
  # attr(dat, "dim_x") <- .dim_x
  # attr(dat, "n") <- .n_v+.n_p
  # attr(dat, "n_vacc") <- .n_v
  # attr(dat, "n_vacc2") <- sum(df_v$z)
  # attr(dat, "n_plac") <- .n_p
  return(dat)

}





