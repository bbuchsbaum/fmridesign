#!/usr/bin/env python3
"""Focused structural tests for the Nilearn comparison harness."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path

import numpy as np


MODULE_PATH = Path(__file__).with_name("bench_nilearn.py")
SPEC = importlib.util.spec_from_file_location("bench_nilearn", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
BENCH = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BENCH)


class BlockDiagonalPolynomialRegsTest(unittest.TestCase):
    def test_matches_runwise_polynomial_and_intercept_span(self) -> None:
        workload = {"n_runs": 3, "run_len": 10}
        regs, names = BENCH.block_diagonal_polynomial_regs(workload, order=2)

        self.assertEqual(regs.shape, (30, 8))  # 3*2 polynomials + 2 run dummies
        self.assertEqual(len(names), regs.shape[1])
        self.assertEqual(
            np.linalg.matrix_rank(np.column_stack([np.ones(30), regs])), 9
        )  # 3 runs * (2 polynomial terms + 1 intercept)

        for run in range(3):
            outside = np.ones(30, dtype=bool)
            outside[run * 10 : (run + 1) * 10] = False
            run_columns = regs[:, run * 2 : (run + 1) * 2]
            self.assertTrue(np.all(run_columns[outside, :] == 0))

    def test_single_run_needs_no_extra_intercept_indicator(self) -> None:
        regs, names = BENCH.block_diagonal_polynomial_regs(
            {"n_runs": 1, "run_len": 12}, order=2
        )
        self.assertEqual(regs.shape, (12, 2))
        self.assertEqual(names, ["poly_1_run_1", "poly_2_run_1"])


class MultiTermDesignTest(unittest.TestCase):
    def test_counts_each_event_and_runwise_nuisance_column_once(self) -> None:
        workload = {
            "n_runs": 2,
            "run_len": 30,
            "n_events_per_run": 4,
            "n_conditions": 2,
            "duration": 0.0,
            "hrf": "spm",
            "style": "multi_term",
        }
        result = BENCH.time_one(workload, seed=43, tr=2.0)

        self.assertEqual(result["n_rows"], 60)
        # 2 conditions + 2 interaction labels + 3 modulator bases, plus
        # 2 runs * (2 polynomial terms + 1 intercept).
        self.assertEqual(result["n_cols"], 13)


if __name__ == "__main__":
    unittest.main()
