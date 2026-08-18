"""Shared helpers for Task 2.1 / limma-style DGE on microarray data."""
from __future__ import annotations

import pathlib

import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.stats.multitest import multipletests


def save_and_show(df, path, title=None, index=True, round_digits=4):
    """Save a table to CSV and show it in the notebook for professor review."""
    from IPython.display import display

    path = pathlib.Path(path)
    if title:
        print(title)
    view = df.copy()
    num_cols = view.select_dtypes(include=[np.number]).columns
    if len(num_cols):
        view[num_cols] = view[num_cols].round(round_digits)
    display(view)
    df.to_csv(path, index=index)
    print(f"Saved: {path}")


def quick_expression_skim(
    X,
    y=None,
    name: str = "",
    class_names: dict | None = None,
) -> pd.DataFrame:
    """Compact skim of an expression matrix. Do not print thousands of gene rows."""
    if not isinstance(X, pd.DataFrame):
        X = pd.DataFrame(X)
    arr = X.to_numpy(dtype=float)
    finite = arr[np.isfinite(arr)]
    n_nan = int(np.isnan(arr).sum())
    n_inf = int(np.isinf(arr).sum())
    n_cells = int(arr.size)
    row = {
        "dataset": name or "(unnamed)",
        "orientation": "samples × features",
        "n_samples": int(arr.shape[0]),
        "n_features": int(arr.shape[1]),
        "missing_cells": n_nan,
        "missing_pct": round(100 * n_nan / max(n_cells, 1), 4),
        "inf_cells": n_inf,
        "min": float(np.min(finite)) if finite.size else np.nan,
        "q1": float(np.quantile(finite, 0.25)) if finite.size else np.nan,
        "median": float(np.median(finite)) if finite.size else np.nan,
        "mean": float(np.mean(finite)) if finite.size else np.nan,
        "q3": float(np.quantile(finite, 0.75)) if finite.size else np.nan,
        "max": float(np.max(finite)) if finite.size else np.nan,
        "sd": float(np.std(finite)) if finite.size else np.nan,
        "zero_var_features": int((np.nanstd(arr, axis=0) == 0).sum()) if arr.shape[0] > 1 else 0,
        "duplicate_feature_names": int(X.columns.duplicated().sum()),
        "duplicate_sample_index": int(pd.Index(X.index).duplicated().sum()),
    }
    if y is not None:
        y = np.asarray(y)
        names = class_names or {0: "class0", 1: "class1"}
        for code, lab in names.items():
            row[lab] = int((y == code).sum())
    return pd.DataFrame([row])


def skim_summary(
    df,
    title: str | None = None,
    name: str | None = None,
    y=None,
    class_names: dict | None = None,
    max_cols: int = 50,
):
    """Skimr-style summary (ropensci/skimr). Wide expression matrices use a compact fallback."""
    from IPython.display import display

    if title:
        print(title)
    if not isinstance(df, pd.DataFrame):
        df = pd.DataFrame(df)

    use_compact = df.shape[1] > max_cols or df.shape[0] > 5000
    if not use_compact:
        rows = []
        for col in df.columns:
            s = df[col]
            row = {
                "variable": col,
                "type": str(s.dtype),
                "n_missing": int(s.isna().sum()),
                "n": int(s.notna().sum()),
            }
            if pd.api.types.is_numeric_dtype(s):
                vals = s.dropna()
                if len(vals):
                    row.update(
                        min=float(vals.min()),
                        median=float(vals.median()),
                        mean=float(vals.mean()),
                        max=float(vals.max()),
                        sd=float(vals.std()),
                    )
            rows.append(row)
        display(pd.DataFrame(rows).round(4))
        print("skimr-style summary (see https://github.com/ropensci/skimr).")
        return pd.DataFrame(rows)

    skim_df = quick_expression_skim(
        df, y=y, name=name or title or "", class_names=class_names
    )
    display(skim_df)
    print(
        f"Wide matrix ({df.shape[0]} rows × {df.shape[1]} cols): "
        "compact skimr-style summary (full per-column skimr not shown)."
    )
    return skim_df


def collapse_probes_to_genes(expr: pd.DataFrame, probe_map: pd.DataFrame) -> pd.DataFrame:
    """Map probes (columns) to gene symbols; keep highest-mean probe per gene."""
    pm = probe_map.copy()
    pm["ID"] = pm["ID"].astype(str)
    sym_col = "symbol" if "symbol" in pm.columns else "Gene.Symbol"

    expr = expr.copy()
    expr.columns = expr.columns.astype(str)

    mapping_rows = []
    for _, row in pm.iterrows():
        pid = str(row["ID"])
        if pid not in expr.columns:
            continue
        for sym in str(row[sym_col]).split("///"):
            sym = sym.strip()
            if sym and not sym.startswith("---"):
                mapping_rows.append({"probe": pid, "gene": sym})
    if not mapping_rows:
        return pd.DataFrame(index=expr.index)

    mapping = pd.DataFrame(mapping_rows)
    gene_data: dict[str, pd.Series] = {}
    for gene, grp in mapping.groupby("gene"):
        probes = [p for p in grp["probe"].unique() if p in expr.columns]
        if not probes:
            continue
        if len(probes) == 1:
            gene_data[gene] = expr[probes[0]]
        else:
            means = expr[probes].mean(axis=0)
            best = means.idxmax()
            gene_data[gene] = expr[best]

    genes = pd.DataFrame(gene_data, index=expr.index)
    return genes


def _fit_moments(s2: np.ndarray, df: float) -> tuple[float, float]:
    """Estimate prior variance and df from residual variances (limma eBayes)."""
    s2 = np.asarray(s2, dtype=float)
    s2 = s2[np.isfinite(s2) & (s2 > 0)]
    if s2.size == 0:
        return 1.0, 0.0
    z = np.log(s2)
    m = z.mean()
    v = z.var(ddof=1) if z.size > 1 else 0.0
    if v <= 0:
        return float(np.exp(m)), 0.0
  # method-of-moments estimators from limma
    d0 = max(2.0 * (v - 1.0 / df) / (v + 1.0), 0.0)
    s0_2 = float(np.exp(m + 0.5 * (1.0 / df - 1.0 / (df + d0)) * (v + 1.0 / df)))
    return s0_2, d0


def limma_two_group(X: pd.DataFrame, y: np.ndarray) -> tuple[pd.DataFrame, dict]:
    """Two-group moderated t-test (Smyth 2004 empirical Bayes)."""
    y = np.asarray(y)
    if set(np.unique(y)) - {0, 1}:
        raise ValueError("y must be binary 0/1 labels")

    Xv = X.values.astype(float)
    n, p = Xv.shape
    g1 = y == 1
    g0 = y == 0
    n1, n0 = int(g1.sum()), int(g0.sum())
    if n1 < 2 or n0 < 2:
        raise ValueError("Each group needs at least 2 samples")

    m1 = Xv[g1].mean(axis=0)
    m0 = Xv[g0].mean(axis=0)
    log2fc = m1 - m0

    df = n1 + n0 - 2
    v1 = Xv[g1].var(axis=0, ddof=1)
    v0 = Xv[g0].var(axis=0, ddof=1)
    s2 = ((n1 - 1) * v1 + (n0 - 1) * v0) / df
    s2 = np.where(s2 <= 0, np.nanmin(s2[s2 > 0]) if np.any(s2 > 0) else 1e-8, s2)

    s0_2, d0 = _fit_moments(s2, df)
    s2_post = ((df * s2) + (d0 * s0_2)) / (df + d0)
    se = np.sqrt(s2_post * (1.0 / n1 + 1.0 / n0))
    t_mod = log2fc / se
    t_mod = np.where(se == 0, 0.0, t_mod)

    pval = 2 * stats.t.sf(np.abs(t_mod), df + d0)
    fdr = multipletests(pval, method="fdr_bh")[1]

    direction = np.where(log2fc > 0, "up", np.where(log2fc < 0, "down", "none"))
    out = pd.DataFrame(
        {
            "log2FC": log2fc,
            "AveExpr": Xv.mean(axis=0),
            "t_mod": t_mod,
            "pval": pval,
            "FDR": fdr,
            "direction": direction,
        },
        index=X.columns,
    )
    out.index.name = "gene"
    info = {"d0": d0, "s0_2": s0_2, "dg": df, "n1": n1, "n0": n0}
    return out, info


def call_degs(dge: pd.DataFrame, fdr_cut: float = 0.05, fc_cut: float = 1.0) -> pd.DataFrame:
    """Flag DEGs using FDR and absolute log2 fold-change cutoffs."""
    out = dge.copy()
    out["DEG"] = (out["FDR"] < fdr_cut) & (out["log2FC"].abs() >= fc_cut)
    return out
