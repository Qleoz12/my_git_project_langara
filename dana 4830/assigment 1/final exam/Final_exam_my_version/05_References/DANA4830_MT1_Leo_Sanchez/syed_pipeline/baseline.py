"""A2-style baseline evaluation: RF + val-tuned threshold."""
from __future__ import annotations

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.ensemble import ExtraTreesClassifier, RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score, average_precision_score, confusion_matrix, f1_score,
    fbeta_score, precision_score, recall_score, roc_auc_score,
)
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier

from .config import PipelineConfig
from .fs_extras import (
    build_merged_features, pca_features, prune_correlated_features,
    run_disr, screened_consensus_genes, top_deg_features_by_qvalue,
)
from .univariate import select_overlap_top_k

METRIC_LABELS = {
    "accuracy": ("Test_Accuracy", "Accuracy (test)"),
    "recall": ("Test_Recall", "Recall (test)"),
    "balanced_acc": ("Test_Balanced_Acc", "Balanced accuracy (test)"),
    "f1": ("Test_F1", "F1 (test)"),
}


def metric_column(config: PipelineConfig) -> str:
    col, _ = METRIC_LABELS.get(config.optimize_metric, METRIC_LABELS["accuracy"])
    return col


def metric_plot_label(config: PipelineConfig) -> str:
    _, label = METRIC_LABELS.get(config.optimize_metric, METRIC_LABELS["accuracy"])
    return label


def evaluate_predictions(y_true, y_pred, y_proba=None):
    cm = confusion_matrix(y_true, y_pred)
    tn, fp, fn, tp = cm.ravel() if cm.size == 4 else (0, 0, 0, 0)
    spec = tn / (tn + fp) if (tn + fp) else 0.0
    rec = recall_score(y_true, y_pred, zero_division=0)
    out = {
        "Accuracy": round(accuracy_score(y_true, y_pred), 4),
        "Precision": round(precision_score(y_true, y_pred, zero_division=0), 4),
        "Recall": round(rec, 4),
        "Specificity": round(spec, 4),
        "F1": round(f1_score(y_true, y_pred, zero_division=0), 4),
        "F2": round(fbeta_score(y_true, y_pred, beta=2, zero_division=0), 4),
        "Balanced_Acc": round((rec + spec) / 2, 4),
        "TN": int(tn), "FP": int(fp), "FN": int(fn), "TP": int(tp),
    }
    if y_proba is not None:
        try:
            auc = roc_auc_score(y_true, y_proba)
            out["AUC"] = round(auc, 4)
            out["AUC-ROC"] = round(auc, 4)
        except Exception:
            out["AUC"] = np.nan
            out["AUC-ROC"] = np.nan
        try:
            out["AUC_PR"] = round(average_precision_score(y_true, y_proba), 4)
        except Exception:
            out["AUC_PR"] = np.nan
    return out


def to_metrics_row(best, source, n_feats=None):
    row = best.to_dict() if isinstance(best, pd.Series) else dict(best)
    row["source"] = source
    if n_feats is not None:
        row["N Feats"] = n_feats
    return row


def _policy_status(optimize_metric: str, target_recall: float, min_policy_accuracy: float, best: dict) -> str:
    if optimize_metric == "recall":
        if best.get("recall", 0) >= target_recall and best.get("accuracy", 0) >= min_policy_accuracy:
            return f"Recall >= {target_recall:.2f}, accuracy >= {min_policy_accuracy:.2f}"
        if best.get("accuracy", 0) >= min_policy_accuracy:
            return f"Best recall with accuracy >= {min_policy_accuracy:.2f}"
        return f"Best F2; accuracy < {min_policy_accuracy:.2f}"
    if optimize_metric == "accuracy":
        return "Best accuracy (validation)"
    if optimize_metric == "balanced_acc":
        return "Best balanced accuracy (validation)"
    if optimize_metric == "f1":
        return "Best F1 (validation)"
    return f"Best {optimize_metric} (validation)"


def select_threshold_policy(
    y_val,
    proba,
    optimize_metric: str = "accuracy",
    target_recall: float = 0.80,
    min_policy_accuracy: float = 0.50,
):
    """Pick classification threshold on validation data.

    optimize_metric:
      - accuracy: maximize validation accuracy (midterm default)
      - recall: A2-style — prefer recall >= target_recall with min accuracy floor
      - balanced_acc / f1: maximize that metric
    """
    best = {
        "threshold": 0.5, "recall": 0.0, "accuracy": 0.0,
        "f1": 0.0, "balanced_acc": 0.0,
    }

    for thr in np.arange(0.01, 1.0, 0.01):
        pred = (proba >= thr).astype(int)
        rec = recall_score(y_val, pred, zero_division=0)
        acc = accuracy_score(y_val, pred)
        f1 = f1_score(y_val, pred, zero_division=0)
        cm = confusion_matrix(y_val, pred)
        tn, fp = (int(cm.ravel()[0]), int(cm.ravel()[1])) if cm.size == 4 else (0, 0)
        spec = tn / (tn + fp) if (tn + fp) else 0.0
        bal = (rec + spec) / 2
        row = {
            "threshold": round(float(thr), 2),
            "recall": rec, "accuracy": acc, "f1": f1, "balanced_acc": bal,
        }

        if optimize_metric == "recall":
            if rec >= target_recall and acc >= min_policy_accuracy:
                if f1 >= best["f1"]:
                    best = row
        elif optimize_metric == "accuracy":
            if acc > best["accuracy"]:
                best = row
        elif optimize_metric == "balanced_acc":
            if bal > best["balanced_acc"]:
                best = row
        elif optimize_metric == "f1":
            if f1 > best["f1"]:
                best = row
        else:
            if acc > best["accuracy"]:
                best = row

    if optimize_metric == "recall" and best["recall"] == 0.0:
        for thr in np.arange(0.01, 1.0, 0.01):
            pred = (proba >= thr).astype(int)
            f1 = f1_score(y_val, pred, zero_division=0)
            if f1 > best["f1"]:
                best = {
                    "threshold": round(float(thr), 2),
                    "recall": recall_score(y_val, pred, zero_division=0),
                    "accuracy": accuracy_score(y_val, pred),
                    "f1": f1,
                    "balanced_acc": best["balanced_acc"],
                }
    best["policy_status"] = _policy_status(
        optimize_metric, target_recall, min_policy_accuracy, best,
    )
    return best


def get_baseline_classifiers(config: PipelineConfig) -> dict:
    return {
        "Logistic Regression": LogisticRegression(
            fit_intercept=True, penalty="l2", dual=False, C=1.0, tol=1e-4,
            max_iter=200, solver="liblinear", random_state=config.random_state,
            class_weight="balanced",
        ),
        "Random Forest": RandomForestClassifier(
            n_estimators=200, max_depth=14, min_samples_leaf=5,
            class_weight="balanced_subsample", random_state=config.random_state, n_jobs=-1,
        ),
        "Extra Trees": ExtraTreesClassifier(
            n_estimators=200, max_depth=14, min_samples_leaf=5,
            class_weight="balanced_subsample", random_state=config.random_state, n_jobs=-1,
        ),
        "Decision Tree": DecisionTreeClassifier(
            max_depth=12, min_samples_leaf=5, random_state=config.random_state,
            class_weight="balanced",
        ),
    }


def fit_model_tune_val(X_train, y_train, features, label: str, config: PipelineConfig, estimator):
    feats = [f for f in features if f in X_train.columns]
    if not feats:
        return None, {"label": label, "n_features": 0}
    X = X_train[feats].fillna(0.0)
    X_fit, X_val, y_fit, y_val = train_test_split(
        X, y_train, test_size=0.2, stratify=y_train, random_state=config.random_state,
    )
    model = estimator
    model.fit(X_fit, y_fit)
    if not hasattr(model, "predict_proba"):
        return None, {"label": label, "n_features": len(feats)}
    proba_val = model.predict_proba(X_val)[:, 1]
    policy = select_threshold_policy(
        y_val, proba_val,
        optimize_metric=config.optimize_metric,
        target_recall=config.target_recall,
        min_policy_accuracy=config.min_policy_accuracy,
    )
    thr = policy["threshold"]
    pred_val = (proba_val >= thr).astype(int)
    metrics = evaluate_predictions(y_val, pred_val, proba_val)
    metrics.update({
        "label": label,
        "Model": label,
        "n_features": len(feats),
        "N Feats": len(feats),
        "Threshold": thr,
        "Val_Recall": round(policy["recall"], 4),
        "Val_Accuracy": round(policy["accuracy"], 4),
        "policy_status": policy["policy_status"],
        "optimize_metric": config.optimize_metric,
    })
    return model, metrics


def evaluate_model_on_test(model, X_test, y_test, features, threshold: float, label: str = ""):
    feats = [f for f in features if f in X_test.columns]
    X_te = X_test[feats].fillna(0.0)
    proba = model.predict_proba(X_te)[:, 1]
    pred = (proba >= threshold).astype(int)
    m = evaluate_predictions(y_test, pred, proba)
    m["Threshold"] = threshold
    m["n_features"] = len(feats)
    m["N Feats"] = len(feats)
    if label:
        m["Model"] = label
        m["label"] = label
    return m


def fit_rf_tune_val(X_train, y_train, features, label: str, config: PipelineConfig):
    rf = get_baseline_classifiers(config)["Random Forest"]
    model, metrics = fit_model_tune_val(X_train, y_train, features, label, config, rf)
    if model is None:
        return None, metrics
    return model, metrics


def evaluate_rf_on_test(rf, X_test, y_test, features, threshold: float):
    feats = [f for f in features if f in X_test.columns]
    X_te = X_test[feats].fillna(0.0)
    proba = rf.predict_proba(X_te)[:, 1]
    pred = (proba >= threshold).astype(int)
    m = evaluate_predictions(y_test, pred, proba)
    m["Threshold"] = threshold
    m["n_features"] = len(feats)
    return m


def build_feature_sets(fs_344, deg_sig, config: PipelineConfig, X_train: pd.DataFrame, y_train):
    detail = fs_344.detail
    merged = build_merged_features(detail, cap=config.merged_cap)
    merged_pruned = prune_correlated_features(merged, X_train, config.correlation_prune_threshold)
    disr_cands = merged[: min(80, len(merged))]
    disr_feats, _ = run_disr(X_train, y_train, disr_cands, max_features=config.disr_max_features)

    screened = screened_consensus_genes(fs_344.overlap_up, fs_344.overlap_down, min_votes=2)
    baseline_degs = top_deg_features_by_qvalue(deg_sig, config.baseline_max_features)

    table3_key = f"Table3_8genes ({config.case_label})"
    return {
        "Baseline_top_DEGs": baseline_degs,
        table3_key: fs_344.table3_master or [],
        "Screened_consensus": screened,
        "Merged": merged,
        "Merged_pruned": merged_pruned,
        "DISR": disr_feats,
    }


def run_baseline_suite(
    X_train, y_train, X_test, y_test, feature_sets: dict,
    config: PipelineConfig, display_fn=None, show_plot: bool = True,
):
    rows = []
    models = {}
    sort_col = metric_column(config)

    for label, feats in feature_sets.items():
        if label == "PCA_n50" or not feats:
            continue
        rf, train_m = fit_rf_tune_val(X_train, y_train, feats, label, config)
        if rf is None:
            continue
        test_m = evaluate_rf_on_test(rf, X_test, y_test, feats, train_m["Threshold"])
        row = {"Feature_Set": label, **{f"Test_{k}": v for k, v in test_m.items() if k != "label"}}
        row["Train_Recall"] = train_m.get("Recall")
        row["optimize_metric"] = config.optimize_metric
        rows.append(row)
        models[label] = (rf, feats, train_m["Threshold"])

    deg_cols = [c for c in X_train.columns if c in X_test.columns]
    if len(deg_cols) >= config.pca_components:
        X_tr_p, X_te_p, pc_cols = pca_features(
            X_train[deg_cols], X_test[deg_cols], n_components=config.pca_components,
        )
        rf_pca, train_m = fit_rf_tune_val(X_tr_p, y_train, pc_cols, "PCA_n50", config)
        if rf_pca is not None:
            proba = rf_pca.predict_proba(X_te_p[pc_cols].fillna(0.0))[:, 1]
            pred = (proba >= train_m["Threshold"]).astype(int)
            test_m = evaluate_predictions(y_test, pred, proba)
            rows.append({
                "Feature_Set": "PCA_n50",
                "Test_Recall": test_m["Recall"],
                "Test_Specificity": test_m["Specificity"],
                "Test_F1": test_m["F1"],
                "Test_Balanced_Acc": test_m["Balanced_Acc"],
                "Test_Accuracy": test_m["Accuracy"],
                "Test_AUC-ROC": test_m.get("AUC-ROC"),
                "n_features": config.pca_components,
                "Threshold": train_m["Threshold"],
                "optimize_metric": config.optimize_metric,
            })
            models["PCA_n50"] = (rf_pca, pc_cols, train_m["Threshold"])

    df = pd.DataFrame(rows)
    if len(df) and sort_col in df.columns:
        df = df.sort_values(sort_col, ascending=False)
    if display_fn and len(df):
        display_fn(df)
    if show_plot and config.show_plots and len(df) and sort_col in df.columns:
        fig, ax = plt.subplots(figsize=(10, max(4, len(df) * 0.35)))
        ax.barh(df["Feature_Set"], df[sort_col], color="teal")
        ax.set_xlabel(metric_plot_label(config))
        ax.set_title(f"Selection comparison: RF + val-tuned ({config.optimize_metric})")
        ax.invert_yaxis()
        plt.tight_layout()
        plt.show()
    return df, models


def run_all_models_baseline(
    X_train, y_train, X_test, y_test, features: list,
    config: PipelineConfig, display_fn=None, show_plot: bool = True,
):
    """A2-style: LR, RF, Extra Trees, DT on full feature pool with val-tuned threshold."""
    feats = [f for f in features if f in X_train.columns and f in X_test.columns]
    rows = []
    sort_col = "Accuracy"

    for name, clf in get_baseline_classifiers(config).items():
        label = f"{name} (All {len(feats)})"
        model, val_m = fit_model_tune_val(X_train, y_train, feats, label, config, clf)
        if model is None:
            continue
        test_m = evaluate_model_on_test(
            model, X_test, y_test, feats, val_m["Threshold"], label=label,
        )
        row = {
            "Model": label,
            "N Feats": len(feats),
            "policy_status": val_m.get("policy_status", ""),
            **{k: test_m[k] for k in [
                "Threshold", "Accuracy", "Precision", "Recall", "F1", "F2",
                "AUC", "AUC_PR", "Specificity", "TN", "FP", "FN", "TP",
            ] if k in test_m},
        }
        rows.append(row)

    df = pd.DataFrame(rows)
    if len(df) and sort_col in df.columns:
        df = df.sort_values(sort_col, ascending=False)

    display_cols = [
        "Model", "N Feats", "Accuracy", "Precision", "Recall", "F1", "F2",
        "AUC", "AUC_PR", "Specificity", "TN", "FP", "FN", "TP",
        "Threshold", "policy_status",
    ]
    display_cols = [c for c in display_cols if c in df.columns]
    if display_fn and len(df):
        display_fn(df[display_cols])
    elif len(df):
        print(df[display_cols].to_string(index=False))

    if show_plot and config.show_plots and len(df) and sort_col in df.columns:
        fig, ax = plt.subplots(figsize=(10, max(4, len(df) * 0.5)))
        plot_df = df.sort_values(sort_col, ascending=True)
        ax.barh(plot_df["Model"], plot_df[sort_col], color="steelblue", alpha=0.85)
        ax.set_xlabel(f"{sort_col} (test)")
        ax.set_title(f"Baseline models — all top-DEGs ({config.optimize_metric} threshold on val)")
        plt.tight_layout()
        plt.show()
    return df


def run_univariate_quick_compare(
    X_train, y_train, X_test, y_test, score_tables: dict,
    config: PipelineConfig, k: int | None = None, display_fn=None, show_plot: bool = True,
    gene_pool: list | None = None,
):
    """RF + val-tuned threshold for top-K per univariate method + overlap panel."""
    k = k or config.univariate_quick_k
    sort_col = "Accuracy"
    gene_pool = gene_pool or list(next(iter(score_tables.values())).index)
    performance_rows = []

    rf_clf = get_baseline_classifiers(config)["Random Forest"]
    baseline_model, baseline_val = fit_model_tune_val(
        X_train, y_train, gene_pool,
        f"Baseline RF ({len(gene_pool)})", config, rf_clf,
    )
    baseline_test = evaluate_model_on_test(
        baseline_model, X_test, y_test, gene_pool,
        baseline_val["Threshold"], label=f"Baseline RF ({len(gene_pool)})",
    )
    baseline_test["policy_status"] = baseline_val.get("policy_status", "")
    performance_rows.append(to_metrics_row(baseline_test, f"Baseline RF ({len(gene_pool)})", len(gene_pool)))

    for method, scores in score_tables.items():
        feats = scores.head(k).index.tolist()
        model, val_m = fit_model_tune_val(
            X_train, y_train, feats, f"{method} (top {k})", config, rf_clf,
        )
        if model is None:
            continue
        test_m = evaluate_model_on_test(
            model, X_test, y_test, feats, val_m["Threshold"],
            label=f"{method} (top {k})",
        )
        test_m["policy_status"] = val_m.get("policy_status", "")
        performance_rows.append(to_metrics_row(test_m, method, len(feats)))

    overlap_feats = select_overlap_top_k(score_tables, k=k, min_votes=2)
    ov_model, ov_val = fit_model_tune_val(
        X_train, y_train, overlap_feats, f"Overlap top-{k}", config, rf_clf,
    )
    ov_test = evaluate_model_on_test(
        ov_model, X_test, y_test, overlap_feats, ov_val["Threshold"],
        label=f"Overlap top-{k}",
    )
    ov_test["policy_status"] = ov_val.get("policy_status", "")
    performance_rows.append(to_metrics_row(ov_test, f"Overlap top-{k}", len(overlap_feats)))

    df = pd.DataFrame(performance_rows)
    if len(df) and sort_col in df.columns:
        df = df.sort_values(sort_col, ascending=False)

    display_cols = [
        "source", "N Feats", "Accuracy", "Recall", "F2", "AUC_PR",
        "Precision", "Specificity", "Threshold", "policy_status",
    ]
    display_cols = [c for c in display_cols if c in df.columns]
    if display_fn and len(df):
        display_fn(df[display_cols])
    elif len(df):
        print(df[display_cols].to_string(index=False))

    if show_plot and config.show_plots and len(df) and sort_col in df.columns:
        fig, ax = plt.subplots(figsize=(10, max(4, len(df) * 0.4)))
        plot_df = df.sort_values(sort_col, ascending=True)
        ax.barh(plot_df["source"], plot_df[sort_col], color="steelblue", alpha=0.85)
        ax.set_xlabel(f"{sort_col} (test)")
        ax.set_title(f"RF performance by univariate feature set (top {k} per method)")
        plt.tight_layout()
        plt.show()

    return df, baseline_model, score_tables
