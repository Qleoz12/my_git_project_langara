"""§3.3 DEG analysis."""
from __future__ import annotations

import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.stats.multitest import multipletests

from .config import PipelineConfig
from .preprocess import preprocess_for_deg, run_section32_preprocess
from .results import Section33Result


def compute_deg_table(X_df, y_arr, config: PipelineConfig):
    case = X_df.values[y_arr == 1]
    ctrl = X_df.values[y_arr == 0]
    genes = X_df.columns.to_numpy()
    n_genes = len(genes)
    pvals = np.empty(n_genes)
    tstats = np.empty(n_genes)
    mean_case = case.mean(axis=0)
    mean_ctrl = ctrl.mean(axis=0)

    batch = 2000
    for start in range(0, n_genes, batch):
        end = min(start + batch, n_genes)
        tt = stats.ttest_ind(case[:, start:end], ctrl[:, start:end], equal_var=False, nan_policy="omit")
        pvals[start:end] = tt.pvalue
        tstats[start:end] = tt.statistic

    eps = 1e-8
    fc = (mean_case + eps) / (mean_ctrl + eps)
    log2fc = np.log2(fc)
    pvals_bh = np.nan_to_num(pvals, nan=1.0, posinf=1.0, neginf=1.0)
    pvals_bh = np.clip(pvals_bh, 0.0, 1.0)
    _, qvals, _, _ = multipletests(pvals_bh, method="fdr_bh")

    deg = pd.DataFrame({
        "gene": genes,
        "mean_case": mean_case,
        "mean_control": mean_ctrl,
        "fold_change": fc,
        "log2FC": log2fc,
        "t_stat": tstats,
        "pvalue": pvals,
        "qvalue": qvals,
    })

    if config.fc_mode == "paper_fixed":
        fc_threshold = config.fc_threshold
    else:
        nonsig = deg[deg["qvalue"] >= 0.05]
        fc_threshold = float(nonsig["fold_change"].quantile(0.95)) if len(nonsig) else config.fc_threshold
    inv_threshold = 1.0 / fc_threshold

    def categorize(row):
        if row["qvalue"] < 0.05 and row["fold_change"] > fc_threshold:
            return "upregulated"
        if row["qvalue"] < 0.05 and row["fold_change"] < inv_threshold:
            return "downregulated"
        return "non-significant"

    deg["direction"] = deg.apply(categorize, axis=1)
    deg["fc_threshold"] = fc_threshold
    return deg.sort_values("pvalue"), fc_threshold


def run_section33_deg(X, y, labels, config: PipelineConfig, display_fn=print, preprocessed=None) -> Section33Result:
    if preprocessed is not None:
        X_deg = preprocessed.X_pp
        y = preprocessed.y
        labels = preprocessed.labels
        prep = preprocessed.prep_info
    else:
        X_deg, y, labels, prep = preprocess_for_deg(X, y, labels, config)
    deg, fc = compute_deg_table(X_deg, y.values, config)
    sig = deg[deg["direction"].isin(["upregulated", "downregulated"])]
    n_up = int((sig["direction"] == "upregulated").sum())
    n_down = int((sig["direction"] == "downregulated").sum())

    summary = pd.DataFrame({
        "Category": ["Upregulated", "Downregulated", "Total DEG"],
        "Our_count": [n_up, n_down, n_up + n_down],
    })
    if config.fc_mode == "paper_fixed":
        summary["Paper_Syed2024"] = [
            config.paper_deg_counts["up"],
            config.paper_deg_counts["down"],
            config.paper_deg_counts["total"],
        ]
        summary["Difference"] = summary["Our_count"] - summary["Paper_Syed2024"]
        summary["Difference_pct"] = (summary["Difference"] / summary["Paper_Syed2024"] * 100).round(2)

    if display_fn:
        display_fn(summary)

    return Section33Result(
        deg_table=deg, deg_sig=sig, summary=summary,
        X_deg=X_deg, y=y, labels=labels, fc_threshold=fc, prep_info=prep,
    )
