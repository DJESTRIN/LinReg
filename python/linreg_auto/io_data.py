from __future__ import annotations

from pathlib import Path
from typing import Optional

import pandas as pd
from sqlalchemy import create_engine


def load_dataframe(
    input_path: Optional[str] = None,
    *,
    sheet: Optional[str] = None,
    sql_uri: Optional[str] = None,
    sql_query: Optional[str] = None,
) -> pd.DataFrame:
    """Load a dataset from file or SQL with clear errors."""
    if input_path and sql_uri:
        raise ValueError("Use either --input or --sql-uri/--sql-query, not both.")

    if sql_uri or sql_query:
        if not sql_uri or not sql_query:
            raise ValueError("Both --sql-uri and --sql-query are required for SQL input.")
        try:
            engine = create_engine(sql_uri)
            with engine.connect() as connection:
                return pd.read_sql_query(sql_query, connection)
        except Exception as exc:  # pragma: no cover - exact DB failures vary
            raise ValueError(f"Failed to load SQL data: {exc}") from exc

    if not input_path:
        raise ValueError("Provide --input or --sql-uri/--sql-query.")

    path = Path(input_path)
    if not path.exists():
        raise FileNotFoundError(f"Input file does not exist: {path}")
    if not path.is_file():
        raise ValueError(f"Input path is not a file: {path}")

    suffix = path.suffix.lower()
    try:
        if suffix == ".csv":
            return pd.read_csv(path)
        if suffix in {".xlsx", ".xlsm", ".xls"}:
            return pd.read_excel(path, sheet_name=sheet or 0)
        if suffix == ".parquet":
            return pd.read_parquet(path)
    except Exception as exc:
        raise ValueError(f"Failed to parse {path.name} as {suffix or 'unknown'}: {exc}") from exc

    raise ValueError(
        f"Unsupported input format for {path.name}. Expected .csv, .xlsx/.xls/.xlsm, .parquet, or SQL."
    )
