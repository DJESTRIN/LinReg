from pathlib import Path

import pandas as pd

from infer_auto.reshape import detect_shape, to_long, to_wide


def test_detect_shape_long_when_id_repeats() -> None:
    df = pd.DataFrame(
        {
            "subject": [1, 1, 2, 2],
            "time": ["t1", "t2", "t1", "t2"],
            "score": [10, 12, 9, 11],
        }
    )
    assert detect_shape(df, id_col="subject", time_col="time", dependent_vars=["score"]) == "long"


def test_detect_shape_wide_when_suffix_columns_exist() -> None:
    df = pd.DataFrame({"subject": [1, 2], "score_t1": [10, 11], "score_t2": [12, 14]})
    assert detect_shape(df, id_col="subject", time_col="time", dependent_vars=["score_t1", "score_t2"]) == "wide"


def test_to_long_and_back_to_wide_round_trip() -> None:
    wide = pd.DataFrame({"subject": [1, 2], "score_t1": [10, 11], "score_t2": [12, 14]})
    long_df = to_long(wide, id_col="subject", time_col="time", dependent_vars=["score_t1", "score_t2"])

    assert sorted(long_df.columns) == ["score", "subject", "time"]
    assert len(long_df) == 4
    assert set(long_df["time"]) == {"t1", "t2"}

    wide_again = to_wide(long_df, id_col="subject", time_col="time", value_col="score")
    assert {"score_t1", "score_t2", "subject"} <= set(wide_again.columns)
