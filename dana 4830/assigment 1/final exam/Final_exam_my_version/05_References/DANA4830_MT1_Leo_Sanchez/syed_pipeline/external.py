"""§3.11 external validation on independent GEO cohorts."""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score, confusion_matrix, roc_auc_score
from sklearn.preprocessing import MinMaxScaler

from .config import PipelineConfig
from .loaders import load_geo_ibd
from .results import Section311Result


def _subsample_stratified(X, y, labels, max_per_class, random_state):
    if max_per_class is None or max_per_class <= 0:
        return X, y, labels
    y_s = pd.Series(y).reset_index(drop=True)
    lab_s = pd.Series(labels).reset_index(drop=True)
    idx = []
    for cls in y_s.unique():
        cls_idx = y_s[y_s == cls].index.tolist()
        n = min(len(cls_idx), max_per_class)
        rng = np.random.default_rng(random_state)
        idx.extend(rng.choice(cls_idx, size=n, replace=False).tolist())
    idx = sorted(idx)
    return X.iloc[idx].reset_index(drop=True), y_s.iloc[idx].reset_index(drop=True), lab_s.iloc[idx].reset_index(drop=True)


def _scale_biomarkers_cohort(X, genes):
    """MinMax fit on external cohort only (legacy / ablation)."""
    genes = [g for g in genes if g in X.columns]
    if not genes or len(X) == 0:
        return pd.DataFrame(), genes, "cohort_minmax"
    sub = X[genes].apply(pd.to_numeric, errors="coerce").fillna(0.0)
    scaled = MinMaxScaler().fit_transform(sub)
    return pd.DataFrame(scaled, columns=genes, index=sub.index), genes, "cohort_minmax"


def _scale_biomarkers_train_ref(X_ext, biomarkers, X_ref: pd.DataFrame):
    """MinMax each gene using min/max from GSE75214 reference (§3.4-style per gene)."""
    scaled = {}
    for g in biomarkers:
        if g not in X_ref.columns:
            continue
        ref_vals = pd.to_numeric(X_ref[g], errors="coerce").fillna(0.0)
        vmin, vmax = float(ref_vals.min()), float(ref_vals.max())
        if g not in X_ext.columns:
            continue
        ext_vals = pd.to_numeric(X_ext[g], errors="coerce").fillna(0.0)
        if vmax <= vmin:
            scaled[g] = pd.Series(0.0, index=X_ext.index)
        else:
            scaled[g] = (ext_vals - vmin) / (vmax - vmin)
    genes_out = list(scaled.keys())
    if not genes_out:
        return pd.DataFrame(), genes_out, "train_reference"
    return pd.DataFrame(scaled, index=X_ext.index), genes_out, "train_reference"


def _evaluate_model(model, X_scaled, y, train_genes):
    """Apply pre-trained model; missing biomarkers in external → 0.0."""
    X_m = pd.DataFrame(index=X_scaled.index)
    n_missing = 0
    for g in train_genes:
        if g in X_scaled.columns:
            X_m[g] = X_scaled[g]
        else:
            X_m[g] = 0.0
            n_missing += 1
    X_m = X_m[train_genes].fillna(0.0)
    y_pred = model.predict(X_m)
    cm = confusion_matrix(y, y_pred)
    row = {
        "Test Accuracy": round(accuracy_score(y, y_pred), 4),
        "TN": int(cm[0, 0]), "FP": int(cm[0, 1]),
        "FN": int(cm[1, 0]), "TP": int(cm[1, 1]),
        "genes_missing_filled_0": n_missing,
    }
    if hasattr(model, "predict_proba"):
        try:
            row["AUC-ROC"] = round(roc_auc_score(y, model.predict_proba(X_m)[:, 1]), 4)
        except Exception:
            row["AUC-ROC"] = np.nan
    else:
        row["AUC-ROC"] = np.nan
    return row


def _validate_cohort(
    geo_id, biomarkers, train_genes, models, data_dir, config,
    X_scaler_ref=None,
):
    if not config.external_inference_only:
        raise ValueError(
            "§3.11 is inference-only (external_inference_only=True). "
            "Do not retrain on external cohorts here; use run_pipeline per GEO instead."
        )

    X, y, labels = load_geo_ibd(geo_id, data_dir)
    full_shape = X.shape
    n_case = int(y.sum())
    n_ctrl = int((y == 0).sum())

    if config.external_use_subsample:
        X, y, labels = _subsample_stratified(
            X, y, labels, config.external_max_per_class, config.random_state,
        )

    genes_avail = [g for g in biomarkers if g in X.columns]
    missing = sorted(set(biomarkers) - set(genes_avail))

    use_train_scaler = config.external_use_train_scaler and X_scaler_ref is not None
    if use_train_scaler:
        X_mm, genes_scaled, scale_mode = _scale_biomarkers_train_ref(X, biomarkers, X_scaler_ref)
    else:
        X_mm, genes_scaled, scale_mode = _scale_biomarkers_cohort(X, biomarkers)

    if not genes_scaled and not train_genes:
        meta = {
            "geo_id": geo_id,
            "mode": "inference_only",
            "scaling": scale_mode,
            "full_shape": full_shape,
            "eval_shape": (len(y), 0),
            "n_case_full": n_case,
            "n_ctrl_full": n_ctrl,
            "n_eval": len(y),
            "n_genes_mapped": 0,
            "n_genes_missing": len(missing),
            "missing_genes": missing,
            "labels_eval": labels.value_counts().to_dict() if hasattr(labels, "value_counts") else {},
        }
        return meta, pd.DataFrame([
            {"Model": m, "Cohort": geo_id, "Status": "no biomarkers mapped"}
            for m in config.external_models
        ])

    rows = []
    for model_name in config.external_models:
        if model_name not in models:
            rows.append({"Model": model_name, "Cohort": geo_id, "Status": "model not trained"})
            continue
        row = _evaluate_model(models[model_name], X_mm, y, train_genes)
        row.update({"Model": model_name, "Cohort": geo_id, "Status": "ok", "scaling": scale_mode})
        rows.append(row)

    meta = {
        "geo_id": geo_id,
        "mode": "inference_only",
        "scaling": scale_mode,
        "full_shape": full_shape,
        "eval_shape": (len(y), len(genes_scaled)),
        "n_case_full": n_case,
        "n_ctrl_full": n_ctrl,
        "n_eval": len(y),
        "n_genes_mapped": len(genes_scaled),
        "n_genes_missing": len(missing),
        "missing_genes": missing,
        "labels_eval": labels.value_counts().to_dict() if hasattr(labels, "value_counts") else {},
    }
    return meta, pd.DataFrame(rows)


def run_section311_external(
    biomarkers,
    models,
    train_genes,
    config: PipelineConfig,
    data_dir=None,
    X_scaler_ref=None,
    display_fn=None,
) -> Section311Result:
    """
  §3.11 external validation (inference only by default).

    Flow: load GEO → optional subsample → scale biomarkers → predict with §3.9 models.
    No training on external data when config.external_inference_only is True.

    X_scaler_ref: GSE75214 expression before MinMax (e.g. deg_result.X_deg) so external
    genes use the same min/max as the training study when external_use_train_scaler=True.
    """
    if data_dir is None and getattr(config, "data_dir", None):
        data_dir = config.data_dir
    if data_dir is None:
        data_dir = Path(__file__).resolve().parent.parent / "01_Data"
    else:
        data_dir = Path(data_dir)

    if config.external_use_train_scaler and X_scaler_ref is None:
        import warnings
        warnings.warn(
            "§3.11: X_scaler_ref not passed — falling back to MinMax fit on each external cohort. "
            "Pass deg_result.X_deg for train-reference scaling.",
            stacklevel=2,
        )

    cohorts = config.external_cohorts
    all_meta = []
    all_rows = []
    messages = []

    mode_msg = (
        "inference_only"
        if config.external_inference_only
        else "train_allowed"
    )
    scale_msg = (
        "train_reference_minmax"
        if config.external_use_train_scaler and X_scaler_ref is not None
        else "cohort_minmax"
    )
    if display_fn:
        print(f"§3.11 mode: {mode_msg} | scaling: {scale_msg} | models: {list(config.external_models)}")

    for geo_id in cohorts:
        soft = data_dir / f"{geo_id}_family.soft.gz"
        tar = data_dir / f"{geo_id}_RAW.tar"
        if not soft.exists() and not tar.exists():
            msg = f"{geo_id}: not found — place {geo_id}_family.soft.gz or {geo_id}_RAW.tar in {data_dir}"
            messages.append(msg)
            if display_fn:
                display_fn(msg)
            continue
        try:
            meta, perf = _validate_cohort(
                geo_id, biomarkers, train_genes, models, data_dir, config,
                X_scaler_ref=X_scaler_ref,
            )
            all_meta.append(meta)
            all_rows.append(perf)
            dim_msg = (
                f"{geo_id} | full dim: {meta['full_shape']} "
                f"(IBD={meta['n_case_full']}, Healthy={meta['n_ctrl_full']}) | "
                f"eval dim: {meta['eval_shape']} | biomarkers found: {meta['n_genes_mapped']}/{len(biomarkers)} | "
                f"scaling={meta['scaling']}"
            )
            messages.append(dim_msg)
            if display_fn:
                print(dim_msg)
                if meta["missing_genes"]:
                    print(f"  missing genes: {meta['missing_genes']}")
                display_fn(perf)
        except Exception as exc:
            msg = f"{geo_id}: error — {exc}"
            messages.append(msg)
            if display_fn:
                display_fn(msg)

    perf_df = pd.concat(all_rows, ignore_index=True) if all_rows else pd.DataFrame()
    meta_df = pd.DataFrame(all_meta) if all_meta else pd.DataFrame()

    if display_fn and len(perf_df):
        print("\n--- External validation summary (compare with GSE75214 test) ---")
        print("GSE75214 test reference: XGBoost ~0.954 acc | DT ~0.954 acc")
        display_fn(perf_df)

    return Section311Result(
        message="\n".join(messages),
        cohorts=cohorts,
        performance=perf_df,
        cohort_meta=meta_df,
    )
