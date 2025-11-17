#' Ensure coefficient consistency between original and recreated models
#'
#' This function addresses coefficient discrepancies by ensuring proper
#' hyperparameter scaling and coefficient aggregation consistency.
#' @param original_json Original JSON from robyn_write
#' @param recreated_output Output from robyn_recreate
#' @param tolerance Numeric tolerance for coefficient comparison
#' @return List containing consistency status and detailed comparison
#' @rdname check_recreated_coef
#' @aliases check_recreated_coef
#' @export
check_recreated_coef <- function(original_json, recreated_output, tolerance = 1e-10) {
  
  # Extract coefficients
  original_coefs <- original_json$ExportedModel$summary$coef
  recreated_coefs <- recreated_output$OutputCollect$xDecompAgg$coef
  
  # Extract variable names from separate columns
  original_vars <- original_json$ExportedModel$summary$variable
  recreated_vars <- recreated_output$OutputCollect$xDecompAgg$rn
  
  cat("Original coefficients:", length(original_coefs), "\n")
  cat("Recreated coefficients:", length(recreated_coefs), "\n")
  cat("Original variables:", length(original_vars), "\n")
  cat("Recreated variables:", length(recreated_vars), "\n")
  
  # Assign variable names to coefficient vectors
  names(original_coefs) <- original_vars
  names(recreated_coefs) <- recreated_vars
  
  # Find common variables
  common_names <- intersect(original_vars, recreated_vars)
  
  if (length(common_names) == 0) {
    stop("No common variable names found between original and recreated models")
  }
  
  cat("Common variables:", length(common_names), "\n")
  cat("Common variable names (first 10):", paste(head(common_names, 10), collapse = ", "), "\n")
  
  # Subset to common variables
  original_common <- original_coefs[common_names]
  recreated_common <- recreated_coefs[common_names]
  
  # Create comparison dataframe
  coef_comparison <- data.frame(
    variable = common_names,
    original = as.numeric(original_common),
    recreated = as.numeric(recreated_common),
    stringsAsFactors = FALSE
  )
  
  # Calculate differences
  coef_comparison$difference <- abs(coef_comparison$original - coef_comparison$recreated)
  coef_comparison$percent_diff <- abs((coef_comparison$original - coef_comparison$recreated) / coef_comparison$original) * 100
  
  # Calculate statistics
  max_abs_diff <- max(coef_comparison$difference, na.rm = TRUE)
  max_percent_diff <- max(coef_comparison$percent_diff, na.rm = TRUE)
  mean_abs_diff <- mean(coef_comparison$difference, na.rm = TRUE)
  mean_percent_diff <- mean(coef_comparison$percent_diff, na.rm = TRUE)
  
  # Determine consistency
  is_consistent <- max_abs_diff < tolerance
  
  # Print results
  cat("\n=== COEFFICIENT CONSISTENCY RESULTS ===\n")
  cat("Variables compared:", length(common_names), "\n")
  cat("Maximum absolute difference:", sprintf("%.12f", max_abs_diff), "\n")
  cat("Mean absolute difference:", sprintf("%.12f", mean_abs_diff), "\n")
  cat("Maximum percent difference:", sprintf("%.6f", max_percent_diff), "%\n")
  cat("Mean percent difference:", sprintf("%.6f", mean_percent_diff), "%\n")
  cat("Tolerance level:", tolerance, "\n")
  cat("Consistency status:", if(is_consistent) "✓ PASS" else "✗ FAIL", "\n\n")
  
  # Show top differences
  top_diff <- coef_comparison[order(coef_comparison$difference, decreasing = TRUE), ]
  cat("variable differences check:\n")
  print(top_diff)
  
  # Show variables with differences > tolerance
  problematic_vars <- coef_comparison[coef_comparison$difference > tolerance, ]
  if (nrow(problematic_vars) > 0) {
    cat("\nVariables with differences exceeding tolerance (", tolerance, "):\n")
    print(problematic_vars[order(problematic_vars$difference, decreasing = TRUE), ])
  } else {
    cat("\n✓ All variables within tolerance\n")
  }
  
  return(list(
    is_consistent = is_consistent,
    max_abs_diff = max_abs_diff,
    max_percent_diff = max_percent_diff,
    mean_abs_diff = mean_abs_diff,
    mean_percent_diff = mean_percent_diff,
    comparison_df = coef_comparison,
    common_variables = common_names,
    original_only = setdiff(original_vars, recreated_vars),
    recreated_only = setdiff(recreated_vars, original_vars),
    problematic_vars = problematic_vars
  ))
}
