from __future__ import annotations

import re
from collections import Counter
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import pandas as pd

WIDE_SUFFIX_RE = re.compile(r"^(?P<stem>.+?)[_\-\.]?(?P<suffix>(?:t|time)?\d+)$", re.IGNORECASE)


def _normalize_list(values: Optional[Sequence[str]]) -> List[str]:
    return [value for value in (values or []) if value]


def _suffix_groups(columns: Iterable[str]) -> Dict[str, List[Tuple[str, str]]]:
    groups: Dict[str, List[Tuple[str, str]]] = {}
    for column in columns:
        match = WIDE_SUFFIX_RE.match(str(column))
        if not match:
            continue
        groups.setdefault(match.group("stem"), []).append((column, match.group("suffix")))
    return {stem: members for stem, members in groups.items() if len(members) >= 2}


def detect_shape_details(
    df: pd.DataFrame,
    *,
    id_col: Optional[str] = None,
    time_col: Optional[str] = None,
    dependent_vars: Optional[Sequence[str]] = None,
) -> Dict[str, str]:
    dv_list = _normalize_list(dependent_vars)
    if id_col and id_col in df.columns:
        duplicate_count = int(df[id_col].duplicated().sum())
        if duplicate_count > 0:
            return {
                "shape": "long",
                "reason": f"id column '{id_col}' has repeated rows ({duplicate_count}), suggesting long/repeated data.",
            }

    if time_col and time_col in df.columns and id_col and id_col in df.columns:
        grouped = df.groupby(id_col, dropna=False)[time_col].nunique(dropna=True)
        if (grouped > 1).any():
            return {
                "shape": "long",
                "reason": f"time column '{time_col}' varies within '{id_col}', suggesting long/repeated data.",
            }

    groups = _suffix_groups(df.columns)
    if groups:
        most_common_stem, members = max(groups.items(), key=lambda item: len(item[1]))
        return {
            "shape": "wide",
            "reason": f"Detected similarly suffixed columns for stem '{most_common_stem}' ({len(members)} columns), suggesting wide repeated measures.",
        }

    hinted_dvs = [dv for dv in dv_list if dv in df.columns]
    if len(hinted_dvs) > 1:
        return {
            "shape": "wide",
            "reason": f"Multiple dependent-variable columns were provided ({', '.join(hinted_dvs)}), suggesting wide layout.",
        }

    return {
        "shape": "long",
        "reason": "No repeated-id or repeated-measure wide pattern detected; treating input as analysis-ready long/tabular data.",
    }


def detect_shape(
    df: pd.DataFrame,
    *,
    id_col: Optional[str] = None,
    time_col: Optional[str] = None,
    dependent_vars: Optional[Sequence[str]] = None,
) -> str:
    return detect_shape_details(df, id_col=id_col, time_col=time_col, dependent_vars=dependent_vars)["shape"]


def _infer_value_vars(
    df: pd.DataFrame,
    *,
    id_col: Optional[str] = None,
    time_col: Optional[str] = None,
    dependent_vars: Optional[Sequence[str]] = None,
) -> List[str]:
    dv_list = [dv for dv in _normalize_list(dependent_vars) if dv in df.columns]
    if len(dv_list) > 1:
        return dv_list

    protected = {id_col, time_col}
    groups = _suffix_groups(column for column in df.columns if column not in protected)
    if groups:
        stem = max(groups.items(), key=lambda item: len(item[1]))[0]
        return [column for column, _ in groups[stem]]

    raise ValueError(
        "Could not infer wide measurement columns. Provide repeated-measure columns via --dv or use --shape long."
    )


def to_long(
    df: pd.DataFrame,
    *,
    id_col: Optional[str] = None,
    time_col: Optional[str] = None,
    dependent_vars: Optional[Sequence[str]] = None,
    variable_col: str = "measure",
    value_col: str = "value",
) -> pd.DataFrame:
    value_vars = _infer_value_vars(df, id_col=id_col, time_col=time_col, dependent_vars=dependent_vars)
    id_vars = [column for column in df.columns if column not in value_vars]
    melted = pd.melt(df, id_vars=id_vars, value_vars=value_vars, var_name=variable_col, value_name=value_col)

    groups = _suffix_groups(value_vars)
    if groups:
        suffix_map = {}
        stem_counter: Counter[str] = Counter()
        for stem, members in groups.items():
            for column, suffix in members:
                suffix_map[column] = {"stem": stem, "suffix": suffix}
                stem_counter[stem] += 1
        if melted[variable_col].isin(suffix_map).all():
            parsed = melted[variable_col].map(suffix_map)
            inferred_time_col = time_col or "time"
            melted[inferred_time_col] = parsed.map(lambda item: item["suffix"])
            unique_stems = {item["stem"] for item in suffix_map.values()}
            if len(unique_stems) == 1:
                stem = next(iter(unique_stems))
                melted = melted.drop(columns=[variable_col]).rename(columns={value_col: stem})
            else:
                melted[variable_col] = parsed.map(lambda item: item["stem"])
    return melted


def to_wide(
    df: pd.DataFrame,
    *,
    id_col: str,
    time_col: str,
    value_col: str,
    variable_col: Optional[str] = None,
) -> pd.DataFrame:
    if id_col not in df.columns or time_col not in df.columns or value_col not in df.columns:
        missing = [column for column in [id_col, time_col, value_col] if column not in df.columns]
        raise ValueError(f"Cannot pivot to wide; missing columns: {', '.join(missing)}")

    index_cols = [id_col]
    other_id_cols = [column for column in df.columns if column not in {time_col, value_col, variable_col} and column != id_col]
    index_cols.extend(other_id_cols)

    if variable_col and variable_col in df.columns:
        pivoted = df.pivot_table(index=index_cols, columns=[variable_col, time_col], values=value_col, aggfunc="first")
        pivoted.columns = [f"{variable}_{time}" for variable, time in pivoted.columns]
    else:
        pivoted = df.pivot_table(index=index_cols, columns=time_col, values=value_col, aggfunc="first")
        pivoted.columns = [f"{value_col}_{column}" for column in pivoted.columns]
    return pivoted.reset_index()
