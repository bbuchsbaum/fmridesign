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
        # Approximate: conditions + interaction-like labels + modulator.
        base = events[["onset", "duration", "trial_type"]].copy()
        inter = events[["onset", "duration"]].copy()
        task = np.where(events["trial_type"].isin(list("ACEG")), "X", "Y")
        inter["trial_type"] = events["trial_type"].astype(str) + "_" + task
        mod = events[["onset", "duration", "modulation"]].copy()
        mod["trial_type"] = "rt"
        return [pd.concat([base, inter, mod], ignore_index=True)]
    raise ValueError(f"unknown style: {style}")


def time_one(w: dict, seed: int, tr: float) -> dict:
    events = make_events(w, seed, tr)
    ft = frame_times(w, tr)
    kwargs = hrf_kwargs(w["hrf"])
    # Isolate event/HRF path for most workloads. The multi-term workload also
    # includes a polynomial baseline on the fmridesign side, so match that.
    if w["style"] == "multi_term":
        kwargs.update(drift_model="polynomial", drift_order=2)
        # Modulator uses SPMG3 on the R side; approximate with SPM + deriv + disp.
        kwargs["hrf_model"] = "spm + derivative + dispersion"
    else:
        kwargs.update(drift_model=None)

    event_tables = build_events_for_style(events, w["style"])

    t0 = time.perf_counter()
    n_cols = 0
    n_rows = 0
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        for et in event_tables:
            X = make_first_level_design_matrix(ft, et, **kwargs)
            n_rows = X.shape[0]
            n_cols += X.shape[1]
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
