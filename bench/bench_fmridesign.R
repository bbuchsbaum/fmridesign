#!/usr/bin/env Rscript
# Benchmark fmridesign design-matrix construction against shared workloads.
#
# Usage:
#   Rscript bench/bench_fmridesign.R [--out bench/results/fmridesign.csv]
#
# Emits one CSV row per (workload, rep) with wall-clock seconds for:
#   event_model() + design_matrix()  (and baseline_model for multi_term)

suppressPackageStartupMessages({
  library(jsonlite)
  library(pkgload)
})

args <- commandArgs(trailingOnly = TRUE)
out_path <- "bench/results/fmridesign.csv"
if ("--out" %in% args) {
  out_path <- args[which(args == "--out") + 1L]
}

root <- if (file.exists("DESCRIPTION")) "." else normalizePath("..")
setwd(root)
pkgload::load_all(".", quiet = TRUE)

wl <- jsonlite::fromJSON("bench/workloads.json", simplifyVector = FALSE)
defaults <- wl$defaults
n_reps <- as.integer(defaults$n_reps)
warmup <- as.integer(defaults$warmup)
seed0 <- as.integer(defaults$seed)
TR <- as.numeric(defaults$TR)

hrf_basis <- function(hrf) {
  switch(hrf,
    spm = "spmg1",
    spm_deriv_dispersion = "spmg3",
    fir12 = "fir",
    stop("unknown hrf: ", hrf)
  )
}

make_events <- function(w, seed) {
  set.seed(seed)
  n_runs <- as.integer(w$n_runs)
  run_len <- as.integer(w$run_len)
  n_ev <- as.integer(w$n_events_per_run)
  n_cond <- as.integer(w$n_conditions)
  duration <- as.numeric(w$duration)

  max_onset <- run_len * TR - 24
  onsets <- unlist(lapply(seq_len(n_runs), function(r) {
    sort(runif(n_ev, 4, max_onset))
  }))
  run <- rep(seq_len(n_runs), each = n_ev)
  condition <- factor(rep_len(LETTERS[seq_len(n_cond)], length(onsets)))
  task <- factor(ifelse(as.integer(condition) %% 2L == 1L, "X", "Y"))
  rt <- rnorm(length(onsets), mean = 0.8, sd = 0.15)

  data.frame(
    onset = onsets,
    duration = duration,
    run = run,
    condition = condition,
    task = task,
    rt = rt,
    stringsAsFactors = FALSE
  )
}

build_model <- function(w, events) {
  n_runs <- as.integer(w$n_runs)
  run_len <- as.integer(w$run_len)
  sf <- fmrihrf::sampling_frame(rep(run_len, n_runs), TR = TR)
  basis <- hrf_basis(w$hrf)
  style <- w$style

  if (identical(style, "categorical")) {
    if (identical(w$hrf, "fir12")) {
      form <- onset ~ hrf(condition, basis = "fir", nbasis = 12, durations = duration)
    } else if (identical(basis, "spmg1")) {
      form <- onset ~ hrf(condition, basis = "spmg1", durations = duration)
    } else if (identical(basis, "spmg3")) {
      form <- onset ~ hrf(condition, basis = "spmg3", durations = duration)
    } else {
      stop("unsupported categorical basis: ", basis)
    }
    event_model(form, data = events, block = ~run, sampling_frame = sf)
  } else if (identical(style, "modulated")) {
    form <- onset ~ hrf(condition, basis = "spmg1", durations = duration) +
      hrf(rt, basis = "spmg1", durations = duration)
    event_model(form, data = events, block = ~run, sampling_frame = sf)
  } else if (identical(style, "trialwise")) {
    form <- onset ~ trialwise(basis = "spmg1", durations = duration)
    event_model(form, data = events, block = ~run, sampling_frame = sf)
  } else if (identical(style, "multi_term")) {
    form <- onset ~ hrf(condition, durations = duration) +
      hrf(condition, task, durations = duration) +
      hrf(rt, basis = "spmg3", durations = duration)
    em <- event_model(form, data = events, block = ~run, sampling_frame = sf)
    bm <- baseline_model(basis = "poly", degree = 2, sframe = sf)
    list(event = em, baseline = bm)
  } else {
    stop("unknown style: ", style)
  }
}

time_one <- function(w, seed) {
  events <- make_events(w, seed)
  gc(FALSE)
  tm <- system.time({
    obj <- build_model(w, events)
    if (identical(w$style, "multi_term")) {
      dm_e <- design_matrix(obj$event)
      dm_b <- design_matrix(obj$baseline)
      n_cols <- ncol(dm_e) + ncol(dm_b)
      n_rows <- nrow(dm_e)
    } else {
      dm <- design_matrix(obj)
      n_cols <- ncol(dm)
      n_rows <- nrow(dm)
    }
  })
  list(
    elapsed = unname(tm[["elapsed"]]),
    user = unname(tm[["user.self"]]),
    system = unname(tm[["sys.self"]]),
    n_rows = n_rows,
    n_cols = n_cols
  )
}

rows <- list()
for (w in wl$workloads) {
  cat(sprintf("[fmridesign] %s ...\n", w$id))
  for (rep_i in seq_len(n_reps + warmup)) {
    res <- time_one(w, seed = seed0 + rep_i)
    rows[[length(rows) + 1L]] <- data.frame(
      library = "fmridesign",
      workload = w$id,
      label = w$label,
      rep = rep_i,
      warmup = rep_i <= warmup,
      elapsed_sec = res$elapsed,
      user_sec = res$user,
      system_sec = res$system,
      n_rows = res$n_rows,
      n_cols = res$n_cols,
      n_runs = as.integer(w$n_runs),
      run_len = as.integer(w$run_len),
      n_events = as.integer(w$n_runs) * as.integer(w$n_events_per_run),
      hrf = w$hrf,
      style = w$style,
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(out, out_path, row.names = FALSE)
cat("Wrote ", out_path, " (", nrow(out), " rows)\n", sep = "")
