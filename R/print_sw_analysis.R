#' Title
#'
#' @param x A list returned by steppedwedge::analysis()
#' @param ... Additional arguments
#'
#' @return A summary of the analysis object, including an estimate and 95% CI for the treatment effect, and messages on convergence.
#' @export
#'
#' @examples
#' # TO DO
print.sw_analysis <- function(x, ...) {
  cat("Treatment effect estimate: ", round(x$te_est, 3), "\n", sep = "")
  cat("Treatment effect 95% confidence interval: ", round(x$te_ci[1], 3), ", ", round(x$te_ci[2], 3), "\n", sep = "")
  cat("Converged: ", x$converged, "\n", sep = "")
}
