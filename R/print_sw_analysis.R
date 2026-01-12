#' Summarize a list returned by steppedwedge::analyze()
#'
#' @param x A list returned by steppedwedge::analyze()
#' @param ... Additional arguments
#'
#' @return A summary of the analysis object, including an estimate and 95% CI for the treatment effect, and messages on convergence.
#' @noRd
#' @export
print.sw_analysis <- function(x, ...) {
  
  # 1. Model Summary
  if (x$method == "GEE") {
    cat("GEE model with working ", x$corstr, " correlation structure\n", sep = "")
  } else if (x$method == "mixed") {
    # Map random effects codes to readable strings
    re_map <- c(clust = "cluster", time = "time", ind = "individual", tx = "treatment")
    re_readable <- re_map[x$re]
    
    # Handle case where mapping fails (fallback to raw code)
    re_readable[is.na(re_readable)] <- x$re[is.na(re_readable)]
    
    # Combine into a string (e.g., "cluster and time")
    re_string <- paste(re_readable, collapse = " and ")
    
    cat("Mixed model with random effects for ", re_string, "\n", sep = "")
  }
  
  # 2. Time Assumptions
  cat('Exp time: "', x$exp_time, '", cal time: "', x$cal_time, '"\n', sep = "")
  
  # Determine suffix for labels if exponentiated
  suffix <- if (isTRUE(x$exponentiated)) " (exponentiated)" else ""
  
  # 3. Estimand & 4. CI
  cat(x$estimand_type, " estimate", suffix, ": ", round(x$te_est, 3), "\n", sep = "")
  cat(x$estimand_type, " 95% confidence interval", suffix, ": ", 
      round(x$te_ci[1], 3), ", ", round(x$te_ci[2], 3), "\n", sep = "")
  
  # 5. P-value
  cat(x$estimand_type, " p-value: ", format.pval(x$te_p, eps = 0.001), "\n", sep = "")
  
  # 6. Convergence
  # For GEE models, x$converged is NA, so we print "NA" explicitly
  conv_output <- ifelse(is.na(x$converged), "NA", x$converged)
  cat("Converged: ", conv_output, "\n", sep = "")
}
