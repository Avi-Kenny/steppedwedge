#' Title
#'
#' @param x A list returned by steppedwedge::analysis()
#' @param ... Additional arguments
#'
#' @return A summary of the analysis object, including an estimate and 95% CI for the treatment effect, and messages on convergence.
#' @export
#'
#' @examples
#' # Load data
#' test_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
#' treatment = "trt", outcome = "outcome_bin", data = sw_data_example)
#' 
#' # Analysis example: TATE estimand for exposure times 1 through 4
#' results_tate <- analyze(dat = test_data, method = "mixed", estimand_type = "TATE",
#' estimand_time = c(1, 4), exp_time = "ETI", family = poisson)
#' 
#' results_tate
print.sw_analysis <- function(x, ...) {
  cat("Treatment effect estimate: ", round(x$te_est, 3), "\n", sep = "")
  cat("Treatment effect 95% confidence interval: ", round(x$te_ci[1], 3), ", ", round(x$te_ci[2], 3), "\n", sep = "")
  cat("Converged: ", x$converged, "\n", sep = "")
}
