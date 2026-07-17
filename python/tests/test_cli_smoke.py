import json
import shutil
import uuid
from pathlib import Path

import pandas as pd
from click.testing import CliRunner

from infer_auto.cli import main


def _make_work_dir() -> Path:
    base = Path(__file__).resolve().parent / "_work"
    work_dir = base / str(uuid.uuid4())
    work_dir.mkdir(parents=True, exist_ok=True)
    return work_dir


def test_cli_dry_run_linear_model() -> None:
    work_dir = _make_work_dir()
    try:
        csv_path = work_dir / "linear.csv"
        out_dir = work_dir / "out"
        pd.DataFrame(
            {
                "mpg": [21.0, 22.0, 18.0, 30.0],
                "hp": [110, 95, 175, 65],
                "wt": [2.62, 2.32, 3.44, 1.9],
            }
        ).to_csv(csv_path, index=False)

        result = CliRunner().invoke(
            main,
            [
                "run",
                "--input",
                str(csv_path),
                "--dv",
                "mpg",
                "--iv",
                "hp,wt",
                "--dry-run",
                "--output-dir",
                str(out_dir),
            ],
            catch_exceptions=False,
        )

        assert result.exit_code == 0
        assert "Resolved model family: lm" in result.output
        assert (out_dir / "analysis_spec.json").exists()
        assert (out_dir / "data_for_r.csv").exists()

        spec_payload = json.loads((out_dir / "analysis_spec.json").read_text(encoding="utf-8"))
        # By default no factors are force-tested; post-hoc is purely
        # auto-triggered by ANOVA significance in R.
        assert spec_payload["posthoc"]["factors"] == []
        assert spec_payload["posthoc"]["auto"] is True
        assert spec_payload["compare_terms"] is True
    finally:
        shutil.rmtree(work_dir, ignore_errors=True)


def test_cli_posthoc_and_term_comparison_flags() -> None:
    work_dir = _make_work_dir()
    try:
        csv_path = work_dir / "linear.csv"
        out_dir = work_dir / "out"
        pd.DataFrame(
            {
                "mpg": [21.0, 22.0, 18.0, 30.0],
                "hp": [110, 95, 175, 65],
                "wt": [2.62, 2.32, 3.44, 1.9],
            }
        ).to_csv(csv_path, index=False)

        result = CliRunner().invoke(
            main,
            [
                "run",
                "--input",
                str(csv_path),
                "--dv",
                "mpg",
                "--iv",
                "hp,wt",
                "--posthoc-factors",
                "hp,hp:wt",
                "--no-auto-posthoc",
                "--no-term-comparison",
                "--dry-run",
                "--output-dir",
                str(out_dir),
            ],
            catch_exceptions=False,
        )

        assert result.exit_code == 0
        spec_payload = json.loads((out_dir / "analysis_spec.json").read_text(encoding="utf-8"))
        assert spec_payload["posthoc"]["factors"] == ["hp", "hp:wt"]
        assert spec_payload["posthoc"]["auto"] is False
        assert spec_payload["compare_terms"] is False
    finally:
        shutil.rmtree(work_dir, ignore_errors=True)


def test_cli_dry_run_repeated_measures_resolves_lmm() -> None:
    work_dir = _make_work_dir()
    try:
        csv_path = work_dir / "repeated.csv"
        out_dir = work_dir / "out"
        pd.DataFrame(
            {
                "subject": [1, 1, 2, 2, 3, 3],
                "time": ["t1", "t2", "t1", "t2", "t1", "t2"],
                "group": ["A", "A", "B", "B", "A", "A"],
                "score": [10.0, 11.5, 9.5, 12.0, 10.2, 11.9],
            }
        ).to_csv(csv_path, index=False)

        result = CliRunner().invoke(
            main,
            [
                "run",
                "--input",
                str(csv_path),
                "--dv",
                "score",
                "--iv",
                "group,time",
                "--id-col",
                "subject",
                "--time-col",
                "time",
                "--dry-run",
                "--output-dir",
                str(out_dir),
            ],
            catch_exceptions=False,
        )

        assert result.exit_code == 0
        assert "Resolved model family: lmm" in result.output
        assert "(1 | subject)" in result.output
    finally:
        shutil.rmtree(work_dir, ignore_errors=True)
