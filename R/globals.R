# Column names used unquoted in ggplot2 aes() and data-masking calls, declared
# so R CMD check does not report them as undefined globals.

utils::globalVariables(c(
  # event-model plots (plot_event_model.R)
  "Time", "Response", "Regressor", "onset", "duration", "xintercept",
  ".group", ".block", ".colour", "x", "r", "lab",
  # heatmaps (plot_heatmaps.R)
  "y", "value", "w", "tcol", "xend", "yend"
))
