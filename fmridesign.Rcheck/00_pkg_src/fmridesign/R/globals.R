# Declare global variables to satisfy R CMD check for non-standard evaluation
# and pivoted/long-form data frames used in ggplot2

utils::globalVariables(c(
  "scan_number", "Regressor", "Value",
  "Var1", "Var2", "Correlation",
  "Time", "Response", "ContrastName", "Weight",
  ".time", ".block", "condition", "value"
))
