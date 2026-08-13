#!/usr/bin/env python3
"""Benchmark nilearn (FitLins design-matrix hot path) on shared workloads.

FitLins constructs first-level design matrices through
``nilearn.glm.first_level.make_first_level_design_matrix`` (or
``FirstLevelModel``). Timing this API therefore measures the design-matrix
work FitLins performs for equivalent event-related / FIR / modulated /
trialwise (one column per trial) models.

Usage:
  python3 bench/bench_nilearn.py [--out bench/results/nilearn.csv]
"""

from __future__ import annotations

import argparse
import json
import time
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from nilearn.glm.first_level import make_first_level_design_matrix

ROOT = Path(__file__).resolve().parents[1]


def hrf_kwargs(hrf: str) -> dict:
    if hrf == "spm":
        return {"hrf_model": "spm"}
    if hrf == "spm_deriv_dispersion":
        return {"hrf_model": "spm + derivative + dispersion"}
    if hrf == "fir12":
        return {"hrf_model": "fir", "fir_delays": list(range(12))}
    raise ValueError(f"unknown hrf: {hrf}")


def make_events(w: dict, seed: int, tr: float) -> pd.DataFrame:
    rng = np.random.default_rng(seed)
    n_runs = int(w["n_runs"])
    run_len = int(w["run_len"])
    n_ev = int(w["n_events_per_run"])
    n_cond = int(w["n_conditions"])
    duration = float(w["duration"])
    max_onset = run_len * tr - 24.0

    rows = []
    letters = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    for r in range(n_runs):
        onsets = np.sort(rng.uniform(4.0, max_onset, size=n_ev))
        # Global onsets (concatenated multi-run axis), matching fmridesign.
        onsets_global = onsets + r * run_len * tr
        for i, onset in enumerate(onsets_global):
            cond = letters[i % n_cond]
            rows.append(
                {
                    "onset": float(onset),
                    "duration": duration,
                    "trial_type": cond,
                    "modulation": float(rng.normal(0.8, 0.15)),
                    "run": r + 1,
                    "local_onset": float(onsets[i]),
                }
            )
    return pd.DataFrame(rows)


def frame_times(w: dict, tr: float) -> np.ndarray:
    n = int(w["n_runs"]) * int(w["run_len"])
    return np.arange(n, dtype=float) * tr


def block_diagonal_polynomial_regs(
    w: dict, order: int = 2
) -> tuple[np.ndarray, list[str]]:
    """Build the nuisance span used by fmridesign's runwise baseline.

    fmridesign constructs ``order`` orthogonal-polynomial columns separately
    for each run and uses a runwise intercept. Nilearn always supplies one
    global constant, so the additional intercept columns here are indicators
    for runs 2..N; together with Nilearn's constant they span the same runwise
    intercept space without introducing a rank-deficient design.
    """
    n_runs = int(w["n_runs"])
    run_len = int(w["run_len"])
    n_rows = n_runs * run_len
    columns: list[np.ndarray] = []
    names: list[str] = []

    # QR of [1, x, ..., x^order] gives polynomial columns orthogonal to the
    # within-run intercept, matching the blockwise span of stats::poly().
    x = np.linspace(-1.0, 1.0, run_len)
    vandermonde = np.column_stack([x**degree for degree in range(order + 1)])
    run_poly = np.linalg.qr(vandermonde, mode="reduced")[0][:, 1 : order + 1]

    for run in range(n_runs):
        rows = slice(run * run_len, (run + 1) * run_len)
        for degree in range(order):
            column = np.zeros(n_rows)
            column[rows] = run_poly[:, degree]
            columns.append(column)
            names.append(f"poly_{degree + 1}_run_{run + 1}")

    # Treatment-coded run indicators plus Nilearn's global constant span the
    # same space as fmridesign's N one-hot runwise intercept columns.
    for run in range(1, n_runs):
        column = np.zeros(n_rows)
        column[run * run_len : (run + 1) * run_len] = 1.0
        columns.append(column)
        names.append(f"run_{run + 1}")

    return np.column_stack(columns), names


def build_events_for_style(events: pd.DataFrame, style: str) -> list[pd.DataFrame]:
    """Return one or more nilearn event tables for the workload style."""
    if style == "categorical":
        return [events[["onset", "duration", "trial_type"]].copy()]
    if style == "modulated":
        # Nilearn folds modulation into the same trial_type columns.
        # To approximate fmridesign's condition + separate modulator term,
        # build condition columns (unmodulated) and a single modulator column.
        base = events[["onset", "duration", "trial_type"]].copy()
        mod = events[["onset", "duration", "modulation"]].copy()
        mod = mod.rename(columns={"modulation": "modulation"})
        mod["trial_type"] = "rt"
        # Nilearn uses column name 'modulation'
        mod_ev = mod.rename(columns={"modulation": "modulation"})
        # Actually make_first_level_design_matrix expects 'modulation' column
        mod_ev = events[["onset", "duration", "modulation"]].copy()
        mod_ev["trial_type"] = "rt"
        return [pd.concat([base, mod_ev], ignore_index=True)]
    if style == "trialwise":
        tw = events[["onset", "duration"]].copy()
        tw["trial_type"] = [f"t{i}" for i in range(len(tw))]
        return [tw]
    if style == "multi_term":
        # Match the two HRF bases used by fmridesign: SPM for the categorical
        # and interaction terms, SPMG3 for the separate modulator term.
        base = events[["onset", "duration", "trial_type"]].copy()
        inter = events[["onset", "duration"]].copy()
        task = np.where(events["trial_type"].isin(list("ACEG")), "X", "Y")
        inter["trial_type"] = events["trial_type"].astype(str) + "_" + task
        mod = events[["onset", "duration", "modulation"]].copy()
        mod["trial_type"] = "rt"
        return [pd.concat([base, inter], ignore_index=True), mod]
    raise ValueError(f"unknown style: {style}")


def time_one(w: dict, seed: int, tr: float) -> dict:
    events = make_events(w, seed, tr)
    ft = frame_times(w, tr)
    kwargs = hrf_kwargs(w["hrf"])
    kwargs.update(drift_model=None)

    t0 = time.perf_counter()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        event_tables = build_events_for_style(events, w["style"])
        if w["style"] == "multi_term":
            nuisance, nuisance_names = block_diagonal_polynomial_regs(w, order=2)
            X_main = make_first_level_design_matrix(
                ft,
                event_tables[0],
                hrf_model="spm",
                drift_model=None,
                add_regs=nuisance,
                add_reg_names=nuisance_names,
            )
            X_mod = make_first_level_design_matrix(
                ft,
                event_tables[1],
                hrf_model="spm + derivative + dispersion",
                drift_model=None,
            )
            # The main design already contains the intercept space. Remove the
            # extra constant that Nilearn adds to every standalone design.
            X_mod = X_mod.drop(columns="constant")
            n_rows = X_main.shape[0]
            n_cols = X_main.shape[1] + X_mod.shape[1]
        else:
            X = make_first_level_design_matrix(ft, event_tables[0], **kwargs)
            n_rows, n_cols = X.shape
    elapsed = time.perf_counter() - t0
    return {
        "elapsed_sec": elapsed,
        "n_rows": int(n_rows),
        "n_cols": int(n_cols),
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", default="bench/results/nilearn.csv")
    args = parser.parse_args()

    with open(ROOT / "bench" / "workloads.json") as f:
        wl = json.load(f)

    defaults = wl["defaults"]
    n_reps = int(defaults["n_reps"])
    warmup = int(defaults["warmup"])
    seed0 = int(defaults["seed"])
    tr = float(defaults["TR"])

    rows = []
    for w in wl["workloads"]:
        print(f"[nilearn] {w['id']} ...", flush=True)
        for rep_i in range(1, n_reps + warmup + 1):
            res = time_one(w, seed=seed0 + rep_i, tr=tr)
            rows.append(
                {
                    "library": "nilearn",
                    "workload": w["id"],
                    "label": w["label"],
                    "rep": rep_i,
                    "warmup": rep_i <= warmup,
                    "elapsed_sec": res["elapsed_sec"],
                    "user_sec": np.nan,
                    "system_sec": np.nan,
                    "n_rows": res["n_rows"],
                    "n_cols": res["n_cols"],
                    "n_runs": int(w["n_runs"]),
                    "run_len": int(w["run_len"]),
                    "n_events": int(w["n_runs"]) * int(w["n_events_per_run"]),
                    "hrf": w["hrf"],
                    "style": w["style"],
                }
            )

    out = pd.DataFrame(rows)
    out_path = Path(args.out)
    if not out_path.is_absolute():
        out_path = ROOT / out_path
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(out_path, index=False)
    print(f"Wrote {out_path} ({len(out)} rows)")


if __name__ == "__main__":
    main()
