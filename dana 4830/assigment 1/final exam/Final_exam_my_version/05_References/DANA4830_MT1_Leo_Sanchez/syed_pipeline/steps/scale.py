"""Scaling transforms."""
from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler


def minmax_fit_transform(X: pd.DataFrame) -> pd.DataFrame:
    return pd.DataFrame(
        MinMaxScaler().fit_transform(X),
        columns=X.columns,
        index=X.index,
    ).fillna(0.0)


def log1p_transform(X: pd.DataFrame) -> pd.DataFrame:
    return X.apply(lambda s: np.log1p(np.clip(s, a_min=0, a_max=None)))


def log2p1_transform(X: pd.DataFrame) -> pd.DataFrame:
    return X.apply(lambda s: np.log2(np.clip(s, a_min=0, a_max=None) + 1))
