#!/usr/bin/env Rscript
# Combine fmridesign + nilearn raw CSVs into a comparison table + markdown summary.

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
r_csv <- if ("--r" %in% args) args[which(args == "--r") + 1L] else "bench/results/fmridesign.csv"
py_csv <- if ("--py" %in% args) args[which(args == "--py") + 1L] else "bench/results/nilearn.csv"
out_csv <- if ("--out" %in% args) args[which(args == "--out") + 1L] else "bench/results/comparison.csv"
out_md <- if ("--md" %in% args) args[which(args == "--md") + 1L] else "bench/RESULTS.md"

read_keep <- function(path) {
  d <- read.csv(path, stringsAsFactors = FALSE)
  d <- d[!as.logical(d$warmup), , drop = FALSE]
  d
}

r <- read_keep(r_csv)
p <- read_keep(py_csv)
all <- rbind(
  r[, intersect(names(r), names(p))],
  p[, intersect(names(r), names(p))]
)

agg <- aggregate(
  elapsed_sec ~ library + workload + label + style + hrf + n_events + n_rows + n_cols,
  data = all,
  FUN = function(x) c(median = median(x), mean = mean(x), sd = sd(x), n = length(x))
)
# unpack matrix columns from aggregate
med <- agg$elapsed_sec[, "median"]
mn <- agg$elapsed_sec[, "mean"]
sdev <- agg$elapsed_sec[, "sd"]
n <- agg$elapsed_sec[, "n"]
summary_df <- data.frame(
  library = agg$library,
  workload = agg$workload,
  label = agg$label,
  style = agg$style,
  hrf = agg$hrf,
  n_events = agg$n_events,
  n_rows = agg$n_rows,
  n_cols = agg$n_cols,
  elapsed_sec_median = med,
  elapsed_sec_mean = mn,
  elapsed_sec_sd = sdev,
  n_reps = n,
  stringsAsFactors = FALSE
)

# Wide comparison
wide_r <- summary_df[summary_df$library == "fmridesign", ]
wide_p <- summary_df[summary_df$library == "nilearn", ]
comp <- merge(
  wide_r[, c("workload", "label", "style", "hrf", "n_events", "n_rows", "n_cols",
             "elapsed_sec_median", "elapsed_sec_mean", "elapsed_sec_sd")],
  wide_p[, c("workload", "n_cols", "elapsed_sec_median", "elapsed_sec_mean", "elapsed_sec_sd")],
  by = "workload",
  suffixes = c("_fmridesign", "_nilearn")
)
# >1 means fmridesign is faster; <1 means nilearn is faster.
comp$fmridesign_vs_nilearn <- comp$elapsed_sec_median_nilearn / comp$elapsed_sec_median_fmridesign
comp <- comp[order(comp$workload), ]

dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write.csv(comp, out_csv, row.names = FALSE)

# Markdown
lines <- c(
  "# Design-matrix benchmark: fmridesign vs nilearn (FitLins hot path)",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "## Scope",
  "",
  "- **fmridesign**: `event_model()` + `design_matrix()` (plus `baseline_model()` for the multi-term workload).",
  "- **nilearn / FitLins**: `nilearn.glm.first_level.make_first_level_design_matrix`.",
  "  FitLins uses this (or `FirstLevelModel`) for first-level design construction, so these",
  "  numbers represent the FitLins design-matrix hot path for equivalent event/FIR/modulated/trialwise models.",
  "- Drift/baseline disabled on the nilearn side for event-only isolation (`drift_model=None`).",
  "- The multi-term workload uses two polynomial drift terms per run plus an equivalent runwise-intercept span in both libraries.",
  "  Its categorical/interaction terms use SPM and its separate modulator uses SPMG3 on both sides.",
  "- Multi-run designs use a concatenated global onset axis in both libraries.",
  "- Times are median wall-clock seconds over non-warmup reps (see `bench/workloads.json`).",
  "",
  "## Results",
  "",
  "| Workload | Events | fmridesign cols | nilearn cols | fmridesign (s) | nilearn (s) | fmridesign / nilearn |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(comp))) {
  lines <- c(lines, sprintf(
    "| %s | %d | %d | %d | %.4f | %.4f | **%.2fx** |",
    comp$label[i],
    comp$n_events[i],
    comp$n_cols_fmridesign[i],
    comp$n_cols_nilearn[i],
    comp$elapsed_sec_median_fmridesign[i],
    comp$elapsed_sec_median_nilearn[i],
    comp$fmridesign_vs_nilearn[i]
  ))
}

lines <- c(
  lines,
  "",
  "Ratio column: `nilearn_time / fmridesign_time`. Values **> 1** mean fmridesign is faster.",
  "",
  "## Interpretation notes",
  "",
  "- Column counts are not always identical across libraries (naming, constant/intercept columns,",
  "  interaction encoding, SPMG3 vs nilearn's three SPM bases). Compare timings within each workload,",
  "  not across mismatched column counts.",
  "- Dense categorical / FIR / modulated designs: fmridesign is ahead after the shared-HRF C++",
  "  eval + metadata opts (see table; FIR/SPMG3/multi-term show the largest dense wins).",
  "- Trialwise/LSS: fmridesign's largest advantage. Per-block zero-column skip + shared-HRF",
  "  evaluation avoid empty regressors and per-column `Reg`/`prep_reg_inputs` overhead; nilearn",
  "  evaluates every trial column over the full concatenated series.",
  "- Remaining fmridesign time is dominated by `fmrihrf`'s C++ evaluate kernel",
  "  (see `OPTIMIZATION_NOTES.md` for next cross-repo batch-evaluate targets).",
  "- Numeric equivalence is intentionally out of scope for this harness (different HRF discretizations",
  "  / oversampling). This suite is for **hot-path wall-clock** comparison of equivalent operations.",
  "",
  "## Re-run",
  "",
  "```bash",
  "bash bench/run_compare.sh",
  "```",
  ""
)

writeLines(lines, out_md)
cat("Wrote ", out_csv, "\n", sep = "")
cat("Wrote ", out_md, "\n", sep = "")
print(comp[, c("workload", "elapsed_sec_median_fmridesign", "elapsed_sec_median_nilearn", "fmridesign_vs_nilearn")])
