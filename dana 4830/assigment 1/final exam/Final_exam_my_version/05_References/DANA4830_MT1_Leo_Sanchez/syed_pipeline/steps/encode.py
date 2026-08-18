"""Label binary encoding."""
from __future__ import annotations

import pandas as pd


def binary_encode_labels(labels: pd.Series, case_label: str) -> pd.Series:
    return (labels == case_label).astype(int)


def encoding_audit_table(labels: pd.Series, case_label: str) -> pd.DataFrame:
    counts = labels.value_counts()
    return pd.DataFrame({
        "label_str": counts.index,
        "count": counts.values,
        "binary": [1 if x == case_label else 0 for x in counts.index],
    })
