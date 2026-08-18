"""§3.2 preprocessing — delegates to injectable steps/."""
from __future__ import annotations

import pandas as pd

from .results import Section32Result
from .steps.filters import drop_quasi_constant, remove_iqr_outlier_samples
from .steps.log import StepLog
from .steps.plot import plot_preprocessing_bars
from .steps.scale import log1p_transform, log2p1_transform, minmax_fit_transform
from .config import PipelineConfig


def _matrix_stats(X: pd.DataFrame) -> dict:
    import numpy as np
    vals = X.values.ravel()
    vals = vals[~np.isnan(vals)]
    if len(vals) == 0:
        return {"n_samples": X.shape[0], "n_genes": X.shape[1], "min": np.nan, "max": np.nan, "mean": np.nan, "std": np.nan}
    return {
        "n_samples": X.shape[0], "n_genes": X.shape[1],
        "min": float(np.min(vals)), "max": float(np.max(vals)),
        "mean": float(np.mean(vals)), "std": float(np.std(vals)),
    }


def preprocess_for_deg(X, y, labels, config: PipelineConfig | None = None):
    if config is None:
        from .config import PipelineConfig as PC
        config = PC()
    X_work = X.copy()
    if config.use_log2p1:
        X_work = log2p1_transform(X_work)
    elif config.use_log1p:
        X_work = log1p_transform(X_work)
    qc_removed = []
    if config.use_quasi_constant:
        X_work, qc_removed = drop_quasi_constant(
            X_work,
            method=config.quasi_constant_method,
            frequency_threshold=config.quasi_constant_frequency_threshold,
            variance_threshold=config.quasi_constant_variance_threshold,
        )
    iqr_removed = pd.DataFrame()
    if config.use_iqr:
        X_work, y, labels, iqr_removed = remove_iqr_outlier_samples(X_work, y, labels)
    info = {
        "quasi_constant_removed": len(qc_removed),
        "quasi_constant_removed_genes": qc_removed,
        "outlier_samples_removed": len(iqr_removed),
        "iqr_removed_samples": iqr_removed,
        "shape": X_work.shape,
    }
    return X_work, y, labels, info


def display_preprocessing_removals(result, display_fn=print, max_genes_show: int = 50):
    genes = getattr(result, "quasi_constant_removed_genes", None) or result.prep_info.get("quasi_constant_removed_genes", [])
    samples = getattr(result, "iqr_removed_samples", None)
    if samples is None:
        samples = result.prep_info.get("iqr_removed_samples", pd.DataFrame())
    if display_fn:
        display_fn(f"Quasi-constant genes removed: {len(genes)}")
        if genes:
            display_fn(genes[:max_genes_show])
            if len(genes) > max_genes_show:
                display_fn(f"... and {len(genes) - max_genes_show} more")
        display_fn(f"IQR outlier samples removed: {len(samples)}")
        if samples is not None and len(samples):
            display_fn(samples)


def run_section32_preprocess(X, y, labels, show_plots: bool = False, display_fn=None, config: PipelineConfig | None = None):
    from .config import PipelineConfig as PC
    if config is None:
        config = PC()
    log = StepLog()
    rows = []

    def _record(stage: str, Xdf: pd.DataFrame, note: str = ""):
        rows.append({"Stage": stage, "Note": note, **_matrix_stats(Xdf)})
        log.record(stage, Xdf.shape, note=note)

    X_work = X.copy()
    _record("Before preprocessing", X_work)
    if config.use_log2p1:
        X_work = log2p1_transform(X_work)
        _record("After log2(x+1)", X_work, "log2(x+1)")
    elif config.use_log1p:
        X_work = log1p_transform(X_work)
        _record("After log1p", X_work, "log(1+x)")
    qc_removed = []
    if config.use_quasi_constant:
        X_work, qc_removed = drop_quasi_constant(
            X_work,
            method=config.quasi_constant_method,
            frequency_threshold=config.quasi_constant_frequency_threshold,
            variance_threshold=config.quasi_constant_variance_threshold,
        )
        qc_note = (
            f"removed {len(qc_removed)} genes (variance <= {config.quasi_constant_variance_threshold})"
            if config.quasi_constant_method == "variance"
            else f"removed {len(qc_removed)} genes (frequency >= {config.quasi_constant_frequency_threshold})"
        )
        _record("After quasi-constant removal", X_work, qc_note)
    iqr_removed = pd.DataFrame()
    if config.use_iqr:
        X_work, y, labels, iqr_removed = remove_iqr_outlier_samples(X_work, y, labels)
        _record("After IQR outlier removal", X_work, f"removed {len(iqr_removed)} samples")
    X_scaled = minmax_fit_transform(X_work)
    _record("After Min–Max scaling", X_scaled, "values scaled to [0, 1]")

    comparison = pd.DataFrame(rows)
    prep_info = {
        "quasi_constant_removed": len(qc_removed),
        "quasi_constant_removed_genes": qc_removed,
        "outlier_samples_removed": len(iqr_removed),
        "iqr_removed_samples": iqr_removed,
        "shape_before": X.shape,
        "shape_after": X_work.shape,
        "step_log": log.to_dataframe(),
    }
    if show_plots:
        plot_preprocessing_bars(comparison, show=True)
    if display_fn:
        display_fn(comparison)
        display_preprocessing_removals(
            Section32Result(
                comparison=comparison, X_raw=X, X_pp=X_work, X_scaled=X_scaled,
                y=y, labels=labels, prep_info=prep_info,
                quasi_constant_removed_genes=qc_removed, iqr_removed_samples=iqr_removed,
            ),
            display_fn=display_fn,
        )
    return Section32Result(
        comparison=comparison, X_raw=X, X_pp=X_work, X_scaled=X_scaled,
        y=y, labels=labels, prep_info=prep_info,
        quasi_constant_removed_genes=qc_removed, iqr_removed_samples=iqr_removed,
    )


def minmax_deg_matrix(X_deg: pd.DataFrame, deg_genes: list) -> pd.DataFrame:
    cols = [g for g in deg_genes if g in X_deg.columns]
    return minmax_fit_transform(X_deg[cols])
