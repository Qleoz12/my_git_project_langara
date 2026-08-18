"""Step execution log for modular pipelines."""
from __future__ import annotations

from dataclasses import dataclass, field

import pandas as pd


@dataclass
class StepLog:
    entries: list = field(default_factory=list)

    def record(self, stage: str, shape=None, note: str = "", **extra):
        row = {"stage": stage, "note": note}
        if shape is not None:
            row["n_samples"] = shape[0]
            row["n_features"] = shape[1]
        row.update(extra)
        self.entries.append(row)

    def to_dataframe(self) -> pd.DataFrame:
        return pd.DataFrame(self.entries)
