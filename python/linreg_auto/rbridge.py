from __future__ import annotations

import json
import shutil
import subprocess
from pathlib import Path
from typing import Any, Dict, Optional, Union

from .spec import AnalysisSpec, Results


def locate_rscript(r_path: Optional[str] = None) -> str:
    if r_path:
        candidate = Path(r_path)
        if candidate.exists():
            return str(candidate)
        discovered = shutil.which(r_path)
        if discovered:
            return discovered
        raise FileNotFoundError(f"Rscript executable was not found at --r-path: {r_path}")

    discovered = shutil.which("Rscript")
    if discovered:
        return discovered
    raise FileNotFoundError("Rscript executable was not found on PATH. Provide --r-path.")


def _error_results(spec: AnalysisSpec, message: str, *, stdout: str = "", stderr: str = "") -> Dict[str, Any]:
    errors = [message]
    if stderr.strip():
        errors.append(f"stderr: {stderr.strip()}")
    warnings = [f"stdout: {stdout.strip()}"] if stdout.strip() else []
    return Results(
        model_family_used=spec.model_family,
        formula=spec.formula,
        warnings=warnings,
        errors=errors,
    ).to_dict()


def run_r_analysis(spec_path: Union[str, Path], *, r_path: Optional[str] = None, timeout: int = 300) -> Dict[str, Any]:
    spec_file = Path(spec_path)
    spec = AnalysisSpec.from_json(spec_file)
    output_dir = Path(spec.output_dir or spec_file.parent)
    output_dir.mkdir(parents=True, exist_ok=True)
    results_path = output_dir / "results.json"
    run_script = Path(__file__).resolve().parents[2] / "R" / "run_analysis.R"

    def persist(results_payload: Dict[str, Any]) -> Dict[str, Any]:
        results_path.write_text(json.dumps(results_payload, indent=2), encoding="utf-8")
        return results_payload

    if not run_script.exists():
        return persist(
            _error_results(spec, f"R analysis entrypoint does not exist yet: {run_script}")
        )

    try:
        rscript = locate_rscript(r_path)
    except FileNotFoundError as exc:
        return persist(_error_results(spec, str(exc)))

    command = [rscript, str(run_script), "--config", str(spec_file)]
    try:
        completed = subprocess.run(
            command,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as exc:
        return persist(
            _error_results(
                spec,
                f"R analysis timed out after {timeout} seconds.",
                stdout=exc.stdout or "",
                stderr=exc.stderr or "",
            )
        )
    except OSError as exc:
        return persist(_error_results(spec, f"Failed to invoke Rscript: {exc}"))

    if results_path.exists():
        payload = Results.from_json(results_path).to_dict()
        if completed.stdout.strip():
            payload.setdefault("warnings", [])
            payload["warnings"].append(f"R stdout: {completed.stdout.strip()}")
        if completed.stderr.strip():
            payload.setdefault("warnings", [])
            payload["warnings"].append(f"R stderr: {completed.stderr.strip()}")
        results_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
        return payload

    message = f"R analysis did not produce results.json (exit code {completed.returncode})."
    return persist(_error_results(spec, message, stdout=completed.stdout, stderr=completed.stderr))
