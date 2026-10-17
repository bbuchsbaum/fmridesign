# Declare global variables to satisfy R CMD check for non-standard evaluation
# and pivoted/long-form data frames used in ggplot2

utils::globalVariables(c(
  "Regressor", "Time", "Response",
  ".block", ".group", "condition", "value",
  "run", "start", "end", "xintercept"
))
