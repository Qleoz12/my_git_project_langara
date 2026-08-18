"""Typed results for each pipeline section."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Optional

import pandas as pd


@dataclass
class Section32Result:
    comparison: pd.DataFrame
    X_raw: pd.DataFrame
    X_pp: pd.DataFrame
    X_scaled: pd.DataFrame
    y: Any
    labels: Any
    prep_info: dict
    quasi_constant_removed_genes: list = field(default_factory=list)
    iqr_removed_samples: pd.DataFrame = None


@dataclass
class Section33Result:
    deg_table: pd.DataFrame
    deg_sig: pd.DataFrame
    summary: pd.DataFrame
    X_deg: pd.DataFrame
    y: Any
    labels: Any
    fc_threshold: float
    prep_info: dict


@dataclass
class Section34PrepareResult:
    X_fs: pd.DataFrame
    X_bal: pd.DataFrame
    y_bal: Any
    up_pool: list
    down_pool: list
    deg_genes: list
    deg_sig: pd.DataFrame


@dataclass
class FSSelectionResult:
    method: str
    fs_type: str
    up_gene: Optional[str]
    down_gene: Optional[str]
    up_list: list
    down_list: list
    extra: dict = field(default_factory=dict)


@dataclass
class Section344Result:
    table3: pd.DataFrame
    detail: dict
    master_biomarkers: list
    overlap_up: pd.DataFrame
    overlap_down: pd.DataFrame
    master_merged_up: pd.DataFrame = None
    master_merged_down: pd.DataFrame = None
    table3_master: list = None


@dataclass
class Section35Result:
    genes: list
    ttest_table: pd.DataFrame


@dataclass
class Section36Result:
    X_train: pd.DataFrame
    X_test: pd.DataFrame
    y_train: Any
    y_test: Any
    labels_train: Any
    labels_test: Any
    summary: pd.DataFrame


@dataclass
class Section37Result:
    X_train: pd.DataFrame
    y_train: Any
    method: str
    before: dict
    after: dict


@dataclass
class Section37DualResult:
    train_unbalanced: Section37Result
    train_balanced: Section37Result
    method: str
    before: dict
    after: dict


@dataclass
class Section38Result:
    loocv_table: pd.DataFrame


@dataclass
class Section39Result:
    models: dict
    genes: list


@dataclass
class Section310Result:
    performance: pd.DataFrame
    best_model: str


@dataclass
class Section310CompareResult:
    loocv_unbal: Section38Result
    loocv_smote: Section38Result
    perf_unbal: Section310Result
    perf_smote: Section310Result
    comparison_table: pd.DataFrame
    delta_table: pd.DataFrame


@dataclass
class Section311Result:
    message: str
    cohorts: list = field(default_factory=lambda: ["GSE10616", "GSE36807"])
    performance: pd.DataFrame = None
    cohort_meta: pd.DataFrame = None


@dataclass
class Section312Result:
    pathway_genes: list
    instructions: str


@dataclass
class PipelineResult:
    config: Any
    section33: Section33Result
    section34_prepare: Section34PrepareResult
    section341: list
    section342: list
    section343: list
    section344: Section344Result
    section35: Optional[Section35Result] = None
    section36: Optional[Section36Result] = None
    section37: Optional[Section37Result] = None
    section38: Optional[Section38Result] = None
    section39: Optional[Section39Result] = None
    section310: Optional[Section310Result] = None
    section310_compare: Optional[Any] = None
    section311: Optional[Section311Result] = None
    section312: Optional[Section312Result] = None

    def summary(self) -> pd.DataFrame:
        s33 = self.section33
        n_up = int((s33.deg_sig["direction"] == "upregulated").sum())
        n_down = int((s33.deg_sig["direction"] == "downregulated").sum())
        rows = [
            {"Step": "§3.3 DEG", "Metric": "Upregulated", "Value": n_up},
            {"Step": "§3.3 DEG", "Metric": "Downregulated", "Value": n_down},
            {"Step": "§3.3 DEG", "Metric": "Total", "Value": n_up + n_down},
            {"Step": "§3.4.4", "Metric": "Table3 rows", "Value": len(self.section344.table3)},
            {"Step": "§3.4.4", "Metric": "Master biomarkers", "Value": len(self.section344.master_biomarkers)},
        ]
        if self.section310 is not None:
            best = self.section310.performance.iloc[0]
            rows.append({"Step": "§3.10", "Metric": "Best test accuracy", "Value": best["Test Accuracy"]})
        return pd.DataFrame(rows)
