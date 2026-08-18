"""§3.5–§3.10 machine learning steps."""
from __future__ import annotations

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from imblearn.over_sampling import SMOTE
from imblearn.under_sampling import RandomUnderSampler
from sklearn.base import clone
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score, balanced_accuracy_score, confusion_matrix, f1_score,
    precision_score, recall_score, roc_auc_score, roc_curve,
)
from sklearn.model_selection import LeaveOneOut, train_test_split
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier

try:
    from xgboost import XGBClassifier
except ImportError:
    XGBClassifier = None

from .config import PipelineConfig
from .results import (
    Section35Result, Section36Result, Section37Result, Section37DualResult,
    Section38Result, Section39Result, Section310Result, Section310CompareResult,
)
from .visualization import biomarker_ttest_table, plot_confusion_matrix, plot_hfcp


def get_paper_classifiers(config: PipelineConfig):
    n_trees = 100 if config.loocv_fast else 1000
    clfs = {
        "Logistic Regression": LogisticRegression(
            fit_intercept=True, penalty="l2", dual=False, C=1.0, tol=1e-4,
            max_iter=200, solver="liblinear", random_state=123, class_weight="balanced",
        ),
        "KNN": KNeighborsClassifier(n_neighbors=5),
        "Gaussian NB": GaussianNB(var_smoothing=1e-9),
        "SVC": SVC(probability=True, random_state=123, class_weight="balanced"),
        "Decision Tree": DecisionTreeClassifier(
            max_depth=7, criterion="gini", random_state=1,
            min_samples_split=3, min_samples_leaf=1, splitter="best", class_weight="balanced",
        ),
        "Random Forest": RandomForestClassifier(
            n_estimators=n_trees, max_depth=5, random_state=123, n_jobs=-1,
            class_weight="balanced_subsample",
        ),
        "MLP": MLPClassifier(
            hidden_layer_sizes=(100,), activation="relu", solver="adam", alpha=1e-4,
            learning_rate_init=0.001, max_iter=200, random_state=123,
        ),
    }
    if XGBClassifier is not None:
        clfs["XGBoost"] = XGBClassifier(
            n_estimators=100, max_depth=3, learning_rate=0.1,
            eval_metric="logloss", n_jobs=-1,
        )
    return clfs


def run_section35_hfcp(X_fs, y, biomarkers, deg_table, config: PipelineConfig, display_fn=None) -> Section35Result:
    genes = [g for g in biomarkers if g in X_fs.columns][: config.hfcp_top_n]
    ttest = biomarker_ttest_table(X_fs, y, genes, deg_table)
    plot_hfcp(X_fs, y, genes, config.case_label, config.ctrl_label,
              "§3.5 HFCP", show=config.show_plots)
    if display_fn:
        display_fn(ttest)
    return Section35Result(genes=genes, ttest_table=ttest)


def run_section36_split(X, y, labels, config: PipelineConfig, display_fn=None) -> Section36Result:
    test_size = 1.0 - config.train_frac
    X_tr, X_te, y_tr, y_te, lab_tr, lab_te = train_test_split(
        X, y, labels, test_size=test_size, stratify=y, random_state=config.random_state,
    )
    summary = pd.DataFrame({
        "Set": ["Train", "Test", "Total"],
        "Samples": [len(y_tr), len(y_te), len(y)],
        "Case": [int(y_tr.sum()), int(y_te.sum()), int(y.sum())],
        "Control": [int((y_tr == 0).sum()), int((y_te == 0).sum()), int((y == 0).sum())],
    })
    if display_fn:
        display_fn(summary)
    return Section36Result(
        X_train=X_tr, X_test=X_te, y_train=y_tr, y_test=y_te,
        labels_train=lab_tr, labels_test=lab_te, summary=summary,
    )


def _balance_train_arrays(X_train, y_train, config: PipelineConfig):
    before = pd.Series(y_train).value_counts().to_dict()
    method = config.balance_train

    if method == "smote":
        k = min(config.smote_k_neighbors, int((y_train == 0).sum()) - 1)
        if k < 1:
            X_out, y_out = X_train.copy(), y_train.copy()
            method = "none"
        else:
            smote = SMOTE(sampling_strategy="auto", random_state=config.smote_random_state, k_neighbors=k)
            X_out, y_out = smote.fit_resample(X_train, y_train)
            X_out = pd.DataFrame(X_out, columns=X_train.columns, index=range(len(y_out)))
    elif method == "rus":
        rus = RandomUnderSampler(random_state=config.random_state)
        X_out, y_out = rus.fit_resample(X_train, y_train)
        X_out = pd.DataFrame(X_out, columns=X_train.columns, index=range(len(y_out)))
    else:
        X_out, y_out = X_train.copy(), y_train.copy()

    after = pd.Series(y_out).value_counts().to_dict()
    return X_out, y_out, method, before, after


def run_section37_balance(
    X_train, y_train, config: PipelineConfig, display_fn=None, return_both: bool = False,
):
    X_unbal = X_train.copy()
    y_unbal = y_train.copy() if hasattr(y_train, "copy") else pd.Series(y_train).copy()

    X_out, y_out, method, before, after = _balance_train_arrays(X_train, y_train, config)
    if display_fn:
        display_fn(pd.DataFrame({"before": [before], "after": [after], "method": [method]}))

    balanced = Section37Result(X_train=X_out, y_train=y_out, method=method, before=before, after=after)
    if not return_both:
        return balanced

    unbalanced = Section37Result(
        X_train=X_unbal, y_train=y_unbal, method="none", before=before, after=before,
    )
    return Section37DualResult(
        train_unbalanced=unbalanced,
        train_balanced=balanced,
        method=method,
        before=before,
        after=after,
    )


def run_section38_loocv(X_train, y_train, genes, config: PipelineConfig, display_fn=None, train_label: str | None = None) -> Section38Result:
    X = X_train[genes].fillna(0.0)
    y = pd.Series(y_train).reset_index(drop=True)
    n_ctrl = int((y == 0).sum())
    n_case = int((y == 1).sum())
    classifiers = get_paper_classifiers(config)
    loo = LeaveOneOut()
    rows = []
    for name, clf in classifiers.items():
        y_true, y_pred = [], []
        for tr_idx, te_idx in loo.split(X):
            m = clone(clf)
            m.fit(X.iloc[tr_idx], y.iloc[tr_idx])
            y_true.append(int(y.iloc[te_idx].values[0]))
            y_pred.append(int(m.predict(X.iloc[te_idx])[0]))
        cm = confusion_matrix(y_true, y_pred)
        rows.append({
            "Model": name, "LOOCV Accuracy": round(accuracy_score(y_true, y_pred), 4),
            "TN": cm[0, 0], "FP": cm[0, 1], "FN": cm[1, 0], "TP": cm[1, 1],
        })
    loocv_df = pd.DataFrame(rows).sort_values("LOOCV Accuracy", ascending=False)
    if display_fn:
        tag = f" ({train_label})" if train_label else ""
        display_fn(pd.DataFrame([{
            "LOOCV train": f"{config.ctrl_label}={n_ctrl}, {config.case_label}={n_case}, total={len(y)}{tag}",
            "CM check": f"TN+FP={n_ctrl}, FN+TP={n_case} (sumas por fila = train, no test)",
        }]))
        display_fn(loocv_df)
    return Section38Result(loocv_table=loocv_df)


def run_section39_train(X_train, y_train, genes, config: PipelineConfig) -> Section39Result:
    X_tr = X_train[genes].fillna(0.0)
    models = {}
    for name, clf in get_paper_classifiers(config).items():
        m = clone(clf)
        m.fit(X_tr, y_train)
        models[name] = m
    return Section39Result(models=models, genes=genes)


def _rank_column(config: PipelineConfig) -> str:
    return {
        "accuracy": "Test Accuracy",
        "recall": "Recall",
        "balanced_acc": "Balanced Acc",
        "f1": "F1",
    }.get(config.optimize_metric, "Test Accuracy")


def slice_section36_genes(s36: Section36Result, genes: list) -> Section36Result:
    g = [x for x in genes if x in s36.X_train.columns]
    return Section36Result(
        X_train=s36.X_train[g], X_test=s36.X_test[g],
        y_train=s36.y_train, y_test=s36.y_test,
        labels_train=s36.labels_train, labels_test=s36.labels_test,
        summary=s36.summary,
    )


def _metric_row(y_test, y_pred, name, y_proba=None):
    cm = confusion_matrix(y_test, y_pred)
    tn, fp, fn, tp = (cm.ravel() if cm.size == 4 else (0, 0, 0, 0))
    spec = tn / (tn + fp) if (tn + fp) else 0.0
    row = {
        "Model": name,
        "Test Accuracy": round(accuracy_score(y_test, y_pred), 4),
        "Balanced Acc": round(balanced_accuracy_score(y_test, y_pred), 4),
        "Precision": round(precision_score(y_test, y_pred, zero_division=0), 4),
        "Recall": round(recall_score(y_test, y_pred, zero_division=0), 4),
        "Specificity": round(spec, 4),
        "F1": round(f1_score(y_test, y_pred, zero_division=0), 4),
        "TN": int(tn), "FP": int(fp), "FN": int(fn), "TP": int(tp),
        "AUC-ROC": np.nan,
    }
    if y_proba is not None:
        try:
            row["AUC-ROC"] = round(roc_auc_score(y_test, y_proba), 4)
        except Exception:
            pass
    return row


def run_section310_evaluate(
    X_train, y_train, X_test, y_test, genes, config: PipelineConfig,
    display_fn=None, balance_label: str | None = None,
) -> Section310Result:
    X_tr = X_train[genes].fillna(0.0)
    X_te = X_test[genes].fillna(0.0)
    rows = []
    classifiers = get_paper_classifiers(config)
    for name, clf in classifiers.items():
        m = clone(clf)
        m.fit(X_tr, y_train)
        y_pred = m.predict(X_te)
        proba = m.predict_proba(X_te)[:, 1] if hasattr(m, "predict_proba") else None
        rows.append(_metric_row(y_test, y_pred, name, proba))

    rank_col = _rank_column(config)
    perf = pd.DataFrame(rows).sort_values(rank_col, ascending=False)
    best = perf.iloc[0]["Model"]

    if config.show_plots and hasattr(clone(classifiers[best]), "predict_proba"):
        m = clone(classifiers[best])
        m.fit(X_tr, y_train)
        proba = m.predict_proba(X_te)[:, 1]
        fpr, tpr, _ = roc_curve(y_test, proba)
        cm = confusion_matrix(y_test, m.predict(X_te))
        fig, axes = plt.subplots(1, 2, figsize=(13, 5))
        train_tag = f", train={balance_label}" if balance_label else ""
        plot_confusion_matrix(
            cm, axes[0], config.ctrl_label, config.case_label,
            title=f"§3.10 — {best} (test set{train_tag})",
        )
        axes[1].plot(fpr, tpr, label=f"AUC={roc_auc_score(y_test, proba):.3f}")
        axes[1].plot([0, 1], [0, 1], "k--", alpha=0.4)
        axes[1].legend()
        axes[1].set_title(f"§3.10 ROC{train_tag}")
        plt.tight_layout()
        plt.show()

    if display_fn:
        display_fn(perf)
    return Section310Result(performance=perf, best_model=best)


def _class_count_summary(y, config: PipelineConfig, set_name: str) -> dict:
    y = pd.Series(y).reset_index(drop=True)
    n_ctrl = int((y == 0).sum())
    n_case = int((y == 1).sum())
    return {
        "Set": set_name,
        config.ctrl_label: n_ctrl,
        config.case_label: n_case,
        "Total": len(y),
    }

def run_section310_compare(
    X_train_a, y_train_a, label_a,
    X_train_b, y_train_b, label_b,
    X_test, y_test, genes, config: PipelineConfig, display_fn=None,
    compare_model: str = "XGBoost",
) -> Section310CompareResult:
    loocv_unbal = run_section38_loocv(X_train_a, y_train_a, genes, config, display_fn=None)
    loocv_smote = run_section38_loocv(X_train_b, y_train_b, genes, config, display_fn=None)

    perf_unbal = run_section310_evaluate(
        X_train_a, y_train_a, X_test, y_test, genes, config,
        display_fn=None, balance_label=label_a,
    )
    perf_smote = run_section310_evaluate(
        X_train_b, y_train_b, X_test, y_test, genes, config,
        display_fn=None, balance_label=label_b,
    )

    def _tag(df, tag):
        out = df.copy()
        out.insert(0, "Train balance", tag)
        return out

    comparison_table = pd.concat(
        [_tag(perf_unbal.performance, label_a), _tag(perf_smote.performance, label_b)],
        ignore_index=True,
    )

    metrics = ["Test Accuracy", "TN", "FP", "FN", "TP", "AUC-ROC"]
    delta_rows = []
    for model in perf_unbal.performance["Model"]:
        row_a = perf_unbal.performance[perf_unbal.performance["Model"] == model].iloc[0]
        row_b = perf_smote.performance[perf_smote.performance["Model"] == model]
        if row_b.empty:
            continue
        row_b = row_b.iloc[0]
        delta = {"Model": model}
        for m in metrics:
            va, vb = row_a.get(m), row_b.get(m)
            if pd.notna(va) and pd.notna(vb):
                delta[f"Δ {m} ({label_b}−{label_a})"] = round(float(vb) - float(va), 4)
        delta_rows.append(delta)
    delta_table = pd.DataFrame(delta_rows)

    if config.show_plots:
        classifiers = get_paper_classifiers(config)
        model_name = compare_model if compare_model in classifiers else perf_unbal.performance.iloc[0]["Model"]
        fig, axes = plt.subplots(1, 2, figsize=(13, 5))
        X_te_g = X_test[genes].fillna(0.0)
        n_ctrl = int((pd.Series(y_test) == 0).sum())
        n_case = int((pd.Series(y_test) == 1).sum())
        fig.suptitle(
            f"§3.10 — {model_name} | test: {n_ctrl} {config.ctrl_label} + {n_case} {config.case_label} "
            f"(test NUNCA balanceado; 105/105 es solo train SMOTE)",
            fontsize=10,
        )
        for ax, label, X_tr, y_tr in [
            (axes[0], label_a, X_train_a, y_train_a),
            (axes[1], label_b, X_train_b, y_train_b),
        ]:
            m = clone(classifiers[model_name])
            X_tr_g = X_tr[genes].fillna(0.0)
            m.fit(X_tr_g, y_tr)
            cm = confusion_matrix(y_test, m.predict(X_te_g))
            plot_confusion_matrix(
                cm, ax, config.ctrl_label, config.case_label,
                title=f"train={label}",
            )
        plt.tight_layout()
        plt.show()

    if display_fn:
        display_fn(pd.DataFrame([
            _class_count_summary(y_train_a, config, f"Train ({label_a})"),
            _class_count_summary(y_train_b, config, f"Train ({label_b})"),
            _class_count_summary(y_test, config, "Test (§3.10 — sin balancear)"),
        ]))
        display_fn(pd.DataFrame({
            "LOOCV": [label_a, label_b],
            "Best LOOCV model": [
                loocv_unbal.loocv_table.iloc[0]["Model"],
                loocv_smote.loocv_table.iloc[0]["Model"],
            ],
            "Best LOOCV Accuracy": [
                loocv_unbal.loocv_table.iloc[0]["LOOCV Accuracy"],
                loocv_smote.loocv_table.iloc[0]["LOOCV Accuracy"],
            ],
        }))
        display_fn(comparison_table)
        display_fn(delta_table)

    return Section310CompareResult(
        loocv_unbal=loocv_unbal,
        loocv_smote=loocv_smote,
        perf_unbal=perf_unbal,
        perf_smote=perf_smote,
        comparison_table=comparison_table,
        delta_table=delta_table,
    )
