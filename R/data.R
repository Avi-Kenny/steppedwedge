#' Example stepped wedge data
#'
#' Data generated for the purpose of demonstrating the steppedwedge package
#'
#' @format ## `sw_data_example`
#' A data frame with 2,063 rows and 5 columns:
#' \describe{
#'   \item{cluster}{Cluster id}
#'   \item{period}{Time period}
#'   \item{trt}{Treatment indicator}
#'   \item{outcome_bin}{Binary outcome}
#'   \item{outcome_cont}{Continuous outcome}
#'   ...
#' }
"sw_data_example"

#' Example stepped wedge data with aggregrated outcomes
#'
#' A simulated stepped wedge dataset formatted with separate columns for
#' the number of successes (numerator) and number of trials (denominator).
#'
#' @format ## `sw_data_example_binom`
#' A data frame with 90 rows and 5 columns:
#' \describe{
#'   \item{cluster}{Cluster id}
#'   \item{period}{Time period}
#'   \item{trt}{Treatment indicator}
#'   \item{denominator}{Denominator (# trials)}
#'   \item{numerator}{Numerator (# successes)}
#'   ...
#' }
"sw_data_example_binom"