"""Quasi-constant and IQR outlier filters."""
from __future__ import annotations

import pandas as pd


def drop_quasi_constant(
    df: pd.DataFrame,
    method: str = "frequency",
    frequency_threshold: float = 0.99,
    variance_threshold: float = 0.01,
):
    """Remove quasi-constant features.

    method:
      - frequency: drop if top value frequency >= frequency_threshold (default 0.99)
      - variance: drop if sample variance <= variance_threshold (paper Track A: 0.01)
    """
    if method == "variance":
        var = df.var(axis=0, skipna=True)
        keep = var[var > variance_threshold].index
        removed = [c for c in df.columns if c not in keep]
        return df[keep], removed

    top_freq = df.apply(lambda s: s.value_counts(normalize=True, dropna=False).iloc[0])
    keep = top_freq[top_freq < frequency_threshold].index
    removed = [c for c in df.columns if c not in keep]
    return df[keep], removed


def remove_iqr_outlier_samples(df, y_series, labels, k: float = 1.5):
    row_means = df.mean(axis=1)
    q1, q3 = row_means.quantile([0.25, 0.75])
    iqr = q3 - q1
    lower, upper = q1 - k * iqr, q3 + k * iqr
    mask = row_means.between(lower, upper)
    removed_samples = pd.DataFrame({
        "row_index": df.index[~mask],
        "label": labels.loc[~mask].values,
        "binary_class": y_series.loc[~mask].values,
        "row_mean_expression": row_means.loc[~mask].values,
        "iqr_lower": lower,
        "iqr_upper": upper,
    })
    return (
        df.loc[mask].reset_index(drop=True),
        y_series.loc[mask].reset_index(drop=True),
        labels.loc[mask].reset_index(drop=True),
        removed_samples,
    )
