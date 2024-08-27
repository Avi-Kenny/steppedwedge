############################ BEGIN REVISED VERSION ############################
############## STILL NEED TO REVISE PARAMETER LIST
##' Load and format data object
#'
#' @description This function takes in user-supplied data and returns a data
#'     object that can be read in by \code{\link{summary_stats}},
#'     \code{\link{est_ce}}, \code{\link{est_med}}, and other estimation
#'     functions. Data is expected to come from a vaccine clinical trial,
#'     possibly involving two-phase sampling and possibly including a biomarker
#'     of interest.
#' @param time A character string; the name of the numeric variable representing
#'     observed event or censoring times.
#' @param event A character string; the name of the binary variable
#'     corresponding to whether the observed time represents an event time (1)
#'     or a censoring time (0). Either integer (0/1) or Boolean (T/F) values are
#'     allowed.
#' @param vacc A character string; the name of the binary variable denoting
#'     whether the individual is in the vaccine group (1) or the placebo group
#'     (0). Accepts either integer (0/1) or Boolean (T/F) values.
#' @param marker A character string; the name of the numeric variable of
#'     biomarker values.
#' @param covariates A character vector; the names of the covariate columns.
#'     Columns values should be either numeric, binary, or factors. Character
#'     columns will be converted into factors.
#' @param weights A character string; the name of the numeric variable
#'     containing inverse-probability-of-sampling (IPS) weights.
#' @param ph2 A character string; the name of the binary variable representing
#'     whether the individual is in the phase-two cohort (1) or not (0). Accepts
#'     either integer (0/1) or Boolean (T/F) values.
#' @param strata A character string; the name of the variable containing strata
#'     identifiers (for two-phase sampling strata).
#' @param data A dataframe containing the vaccine trial data.
#' @param covariates_ph2 A boolean; if at least one of the covariates is
#'     measured only in the phase-two cohort, set this to TRUE.
#' @return An object of class \code{vaccine_dat}.
#' @examples
#' data(hvtn505)
#' dat <- load_data(time="HIVwk28preunblfu", event="HIVwk28preunbl", vacc="trt",
#'                  marker="IgG_V2", covariates=c("age","BMI","bhvrisk"),
#'                  weights="wt", ph2="casecontrol", data=hvtn505)
#' @export
load_sw_data <- function(
    period, cluster_id, individual_id = NULL, treatment, covariates = NULL, outcome, data
) {

  ########## David - work on individual_id and covariates as optional inputs ######

  # To prevent R CMD CHECK notes # David question
  # .time <- .marker <- .covariates <- NULL; rm(.time,.marker,.covariates);

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
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

  # !!!!! Warning/error if there is only one unique level or too many (>15) unique levels
  # !!!!! Warning/error if there are numbers passed in as character strings
  # !!!!! Check what processing we have to do to strata (e.g. convert to integers)
  # !!!!! Convert booleans to integers (0 or 1) for all coolumns (incl covariates)
  # !!!!! Store two copies of covariates; one for Cox model and one for NPCVE etc.
  # !!!!! Also maybe store a combined version of the dataset (or have a helper function to combine)?


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

############################ END REVISED VERSION ############################

########################### Test revised version

# load data
library(geeCRT)
library(tidyverse)

sampleSWCRTLarge_renamed <- load_sw_data(
    period = "period_orig",
    cluster_id = "id",
    individual_id = "record_id",
    treatment = "treatment",
    covariates = c("age", "sex"),
    outcome = "y_bin",
    # outcome = "y_con",
    data = sampleSWCRTLarge %>%
      slice_head(n = 5) %>%
      # test validation of treatment variable
      # mutate(treatment = "RED") %>%
      rename("period_orig" = "period") %>%
      mutate(record_id = row_number()) %>%
      rowwise() %>%
      mutate(age = runif(1, min = 6, max = 100)) %>%
      mutate(sex = sample(
        x = c("male", "female"),
        size = 1
      )) %>%
      ungroup() %>%
      relocate(age) %>%
      as.data.frame()
)

str(sampleSWCRTLarge_renamed)

sampleSWCRTLarge %>%
  rename("period_orig" = "period") %>%
  mutate(record_id = row_number()) %>%
  rowwise() %>%
  mutate(age = runif(1, min = 6, max = 100)) %>%
  ungroup() %>%
  relocate(age) %>%
  glimpse()


############################ BEGIN ORIGINAL VERSION ############################

##' Load and format data object
#'
#' @description This function takes in user-supplied data and returns a data
#'     object that can be read in by \code{\link{summary_stats}},
#'     \code{\link{est_ce}}, \code{\link{est_med}}, and other estimation
#'     functions. Data is expected to come from a vaccine clinical trial,
#'     possibly involving two-phase sampling and possibly including a biomarker
#'     of interest.
#' @param time A character string; the name of the numeric variable representing
#'     observed event or censoring times.
#' @param event A character string; the name of the binary variable
#'     corresponding to whether the observed time represents an event time (1)
#'     or a censoring time (0). Either integer (0/1) or Boolean (T/F) values are
#'     allowed.
#' @param vacc A character string; the name of the binary variable denoting
#'     whether the individual is in the vaccine group (1) or the placebo group
#'     (0). Accepts either integer (0/1) or Boolean (T/F) values.
#' @param marker A character string; the name of the numeric variable of
#'     biomarker values.
#' @param covariates A character vector; the names of the covariate columns.
#'     Columns values should be either numeric, binary, or factors. Character
#'     columns will be converted into factors.
#' @param weights A character string; the name of the numeric variable
#'     containing inverse-probability-of-sampling (IPS) weights.
#' @param ph2 A character string; the name of the binary variable representing
#'     whether the individual is in the phase-two cohort (1) or not (0). Accepts
#'     either integer (0/1) or Boolean (T/F) values.
#' @param strata A character string; the name of the variable containing strata
#'     identifiers (for two-phase sampling strata).
#' @param data A dataframe containing the vaccine trial data.
#' @param covariates_ph2 A boolean; if at least one of the covariates is
#'     measured only in the phase-two cohort, set this to TRUE.
#' @return An object of class \code{vaccine_dat}.
#' @examples
#' data(hvtn505)
#' dat <- load_data(time="HIVwk28preunblfu", event="HIVwk28preunbl", vacc="trt",
#'                  marker="IgG_V2", covariates=c("age","BMI","bhvrisk"),
#'                  weights="wt", ph2="casecontrol", data=hvtn505)
#' @export
load_data <- function(
    time, event, vacc, marker, covariates, weights, ph2, strata=NA, data,
    covariates_ph2=FALSE
) {

  # To prevent R CMD CHECK notes
  .time <- .marker <- .covariates <- NULL; rm(.time,.marker,.covariates);

  # Input validation
  {

    if (!methods::is(data,"data.frame")) { stop("`data` must be a data frame.") }
    if (nrow(data)==0) { stop("`data` is an empty data frame.") }

    for (arg in c("time", "event", "vacc", "marker", "covariates", "weights",
                  "ph2", "strata")) {

      var <- get(arg)
      if (!(arg=="strata" && missing(strata))) {
        # if (!(arg=="strata")) { # For testing

        # Variable is a character string specifying variable(s) in `data`
        is_string <- methods::is(var,"character")
        length_one <- as.logical(length(var)==1)
        in_df <- all(as.logical(var %in% names(data)))
        if (arg=="covariates") {
          if (!(is_string&&in_df)) {
            stop(paste0("`", arg, "` must be a vector of character strings spe",
                        "cifying one or more variables in `data`."))
          }
        } else {
          if (!(is_string&&length_one&&in_df)) {
            stop(paste0("`", arg, "` must be a character string specifying a s",
                        "ingle variable in `data`."))
          }
        }

        # Assign column(s) to val
        if (arg=="covariates") {

          val <- data[,var, drop=F]
          val2 <- list()

          # Convert factor/character columns
          for (col in names(val)) {
            if (is.numeric(val[,col])) {
              val2[[col]] <- val[,col]
            } else if (is.logical(val[,col])) {
              val2[[col]] <- as.integer(val[,col])
            } else if (is.factor(val[,col]) || is.character(val[,col])) {
              tmp_col <- as.integer(as.factor(val[,col]))
              tmp_unique <- unique(tmp_col)
              tmp_size <- length(tmp_unique)
              for (i in c(1:(tmp_size-1))) {
                col_new <- paste0(col, "_", i)
                val2[[col_new]] <- as.integer(tmp_col==tmp_unique[i])
              }
            } else {
              stop(paste0("The data type of column `", col, "` is not supported."))
            }
          }
          val <- as.data.frame(val2)
          x_names <- names(val)
        } else {
          val <- data[,var]
        }

        # Validate: `time`, `marker`, `weights`
        if (arg %in% c("time", "marker", "weights")) {
          if (!is.numeric(val)) { stop(paste0("`", arg, "` must be numeric.")) }
        }

        # Validate: `event`, `vacc`, `ph2`
        if (arg %in% c("event", "vacc", "ph2")) {
          if (any(!(val %in% c(0,1,F,T)))) {
            stop(paste0("`", arg, "` must only contain binary values (either T",
                        "/F or 1/0)."))
          }

        }

        # No missing values allowed (except marker)
        if (!(arg %in% c("marker", "weights", "covariates"))) {
          if (any(is.na(val))) { stop("NA values not allowed in `", arg, "`.") }
        } else if (arg=="covariates") {
          cond_1 <- any(is.na(val))
          cond_2 <- any(is.na(val[as.logical(data[,ph2]),]))
          msg_1 <- "NA values not allowed in `covariates` (if covariates_ph2==F)."
          msg_2 <- paste0("NA values only allowed in `covariates` for which ph2==F (if covariates_ph2==T).")
          if (cond_1 && !covariates_ph2) { stop(msg_1) }
          if (cond_2 && covariates_ph2) { stop(msg_2) }
        }

        assign(x=paste0(".",arg), value=val)

      } else {

        .strata <- NA

      }

    }

  }

  # Convert binary variables to integers (if specified as boolean)
  .event <- as.integer(.event)
  .vacc <- as.integer(.vacc)
  .ph2 <- as.integer(.ph2)

  # Create additional variables
  .groups <- ifelse(any(.vacc==0) && any(.vacc==1), "both",
                    ifelse(any(.vacc==1), "vaccine", "placebo"))

  .weights <- ifelse(is.na(.weights), 0, .weights)
  .dim_x <- length(.covariates)
  .n_v <- sum(.vacc)
  .n_p <- sum(1-.vacc)

  # Rename covariate dataframe to c("x1", "x2", ...)
  names(.covariates) <- paste0("x", c(1:.dim_x))

  # !!!!! Warning/error if there is only one unique level or too many (>15) unique levels
  # !!!!! Warning/error if there are numbers passed in as character strings
  # !!!!! Check what processing we have to do to strata (e.g. convert to integers)
  # !!!!! Convert booleans to integers (0 or 1) for all coolumns (incl covariates)
  # !!!!! Store two copies of covariates; one for Cox model and one for NPCVE etc.
  # !!!!! Also maybe store a combined version of the dataset (or have a helper function to combine)?

  strata_na <- is.na(.strata[[1]])

  if (.groups %in% c("vaccine", "both")) {

    # Create strata (if not given)
    .ind_v <- which(.vacc==1)
    if(strata_na) {
      .strata <- as.integer(factor(.weights[.ind_v]))
    } else {
      .strata <- as.integer(factor(.strata[.ind_v]))
    }

    # Create data object
    df_v <- cbind(
      .covariates[.ind_v,, drop=F], "y"=.time[.ind_v], "delta"=.event[.ind_v],
      "s"=.marker[.ind_v], "weights"=.ph2[.ind_v]*.weights[.ind_v],
      "strata"=.strata, "z"=.ph2[.ind_v], "a"=1
    )

    # Stabilize weights (rescale to sum to sample size)
    .stb_v <- sum(df_v$weights) / .n_v
    df_v$weights <- df_v$weights / .stb_v

  }

  if (.groups %in% c("placebo", "both")) {

    # Create strata (if not given)
    .ind_p <- which(.vacc==0)
    if(strata_na) {
      .strata <- as.integer(factor(.weights[.ind_p]))
    } else {
      .strata <- as.integer(factor(.strata[.ind_p]))
    }

    # Create data object
    df_p <- cbind(
      .covariates[.ind_p,, drop=F], "y"=.time[.ind_p], "delta"=.event[.ind_p],
      "s"=.marker[.ind_p], "weights"=.ph2[.ind_p]*.weights[.ind_p],
      "strata"=.strata, "z"=.ph2[.ind_p], "a"=0
    )

    # Stabilize weights (rescale to sum to sample size)
    .stb_p <- sum(df_p$weights) / .n_p
    df_p$weights <- df_p$weights / .stb_p

  }

  if (.groups=="vaccine") {
    dat <- df_v
  } else if (.groups=="placebo") {
    dat <- df_p
  } else if (.groups=="both") {
    dat <- rbind(df_v, df_p)
  }

  # Create and return data object
  class(dat) <- c("data.frame", "vaccine_dat")
  attr(dat, "groups") <- .groups
  attr(dat, "covariate_names") <- x_names
  attr(dat, "covariates_ph2") <- covariates_ph2
  attr(dat, "dim_x") <- .dim_x
  attr(dat, "n") <- .n_v+.n_p
  attr(dat, "n_vacc") <- .n_v
  attr(dat, "n_vacc2") <- sum(df_v$z)
  attr(dat, "n_plac") <- .n_p
  return(dat)

}

############################ END ORIGINAL VERSION ############################

##### Experiment with original version #######

data(hvtn505)
dat <- load_data(time="HIVwk28preunblfu", event="HIVwk28preunbl", vacc="trt",
                 marker="IgG_V2", covariates=c("age","BMI","bhvrisk"),
                 weights="wt", ph2="casecontrol", data=hvtn505)

hvtn505$HIVwk28preunblfu <- "test"
dat <- load_data(time=4, event="HIVwk28preunbl", vacc="trt",
                 marker="IgG_V2", covariates=c("age","BMI","bhvrisk"),
                 weights="wt", ph2="casecontrol", data=hvtn505)

hvtn505 %>% glimpse()

##### End experiment with original version #######

readSWdata <- function(df, outcome_type)
{
  summary <- summary(df)
  num_clusters <- length(unique(df$id))
  num_periods <- length(unique(df$period))
  # Create a data frame with positions and shading status
  sw_data <- df %>%
    select(id, period, treatment) %>%
    distinct()

  # Plot the grid using ggplot2
  design_plot <- ggplot2::ggplot(sw_data, aes(x = period, y = id, fill = as.character(treatment))) +
    geom_tile(color = "black") +
    scale_fill_manual(values = c("1" = "grey", "0" = "white")) +
    # reverse order of y axis and label every integer on y axis
    scale_y_reverse(breaks = 1:max(sw_data$id)) +
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = "SW design",
         fill = "Treatment")
  return(list(num_clusters = num_clusters,
              num_periods = num_periods,
              summary = summary,
              design_plot = design_plot))
}
