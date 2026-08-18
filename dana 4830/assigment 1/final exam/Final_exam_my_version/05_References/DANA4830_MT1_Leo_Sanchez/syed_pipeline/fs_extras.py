"""A2-style extra feature selection: Merged, DISR, PCA, correlation prune."""
from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.feature_selection import mutual_info_classif
from sklearn.preprocessing import MinMaxScaler


def prune_correlated_features(features, X_ref: pd.DataFrame, threshold: float = 0.82):
    """Drop redundant features by |Pearson r| on train reference matrix."""
    feats = [f for f in features if f in X_ref.columns]
    if len(feats) <= 1:
        return feats
    corr = X_ref[feats].corr().abs()
    keep = []
    for f in feats:
        if not keep:
            keep.append(f)
            continue
        redundant = any(corr.loc[f, k] > threshold for k in keep if f != k)
        if not redundant:
            keep.append(f)
    return keep


def build_merged_features(fs_detail: dict, cap: int = 160) -> list:
    """Union of top overlap lists from Syed FS methods."""
    merged = []
    seen = set()
    for picks in fs_detail.values():
        for direction in ("upregulated", "downregulated"):
            for g in picks.get(direction, []):
                if g and g not in seen:
                    seen.add(g)
                    merged.append(g)
                if len(merged) >= cap:
                    return merged
    return merged


def discretize_for_mi(series: pd.Series, n_bins: int = 6):
    try:
        return pd.qcut(series.rank(method="first"), q=n_bins, labels=False, duplicates="drop")
    except Exception:
        return pd.cut(series, bins=n_bins, labels=False)


def run_disr(X_train: pd.DataFrame, y_train, candidate_features: list, max_features: int = 10):
    """Double Input Symmetrical Relevance — greedy MI / redundancy."""
    cands = [f for f in candidate_features if f in X_train.columns]
    if not cands:
        return [], []
    X_sub = X_train[cands].fillna(0.0)
    mi_scores = mutual_info_classif(X_sub, y_train, random_state=42)
    mi_map = dict(zip(cands, mi_scores))
    anchor = max(mi_map, key=mi_map.get)
    selected = [anchor]
    history = [{"gene": anchor, "score": mi_map[anchor]}]
    disc = {f: discretize_for_mi(X_sub[f]) for f in cands}

    while len(selected) < max_features:
        best_g, best_score = None, -1.0
        for g in cands:
            if g in selected:
                continue
            redundancy = 0.0
            for s in selected:
                try:
                    redundancy += mutual_info_classif(
                        pd.DataFrame({g: disc[g], s: disc[s]}).fillna(0),
                        disc[g], discrete_features=True,
                    )[0]
                except Exception:
                    redundancy += abs(X_sub[[g, s]].corr().iloc[0, 1])
            score = mi_map[g] / (1.0 + redundancy)
            if score > best_score:
                best_g, best_score = g, score
        if best_g is None:
            break
        selected.append(best_g)
        history.append({"gene": best_g, "score": best_score})
    return selected, history


def pca_features(X_train: pd.DataFrame, X_test: pd.DataFrame, n_components: int = 50):
    """Fit PCA on train; return component column names for train/test arrays."""
    scaler = MinMaxScaler()
    X_tr = scaler.fit_transform(X_train.fillna(0.0))
    X_te = scaler.transform(X_test.fillna(0.0))
    n_components = min(n_components, X_tr.shape[1], X_tr.shape[0] - 1)
    pca = PCA(n_components=n_components, random_state=42)
    X_tr_p = pca.fit_transform(X_tr)
    X_te_p = pca.transform(X_te)
    cols = [f"PC{i+1}" for i in range(n_components)]
    return (
        pd.DataFrame(X_tr_p, columns=cols, index=range(len(X_tr_p))),
        pd.DataFrame(X_te_p, columns=cols, index=range(len(X_te_p))),
        cols,
    )


def top_deg_features_by_qvalue(deg_sig: pd.DataFrame, max_features: int = 500) -> list:
    sub = deg_sig.sort_values("qvalue")
    return sub["gene"].head(max_features).tolist()


def screened_consensus_genes(overlap_up: pd.DataFrame, overlap_down: pd.DataFrame, min_votes: int = 2) -> list:
    genes = []
    for overlap in (overlap_up, overlap_down):
        if overlap is None or overlap.empty:
            continue
        vote_col = "votes" if "votes" in overlap.columns else "Total"
        if vote_col not in overlap.columns:
            continue
        genes.extend(overlap[overlap[vote_col] >= min_votes].index.tolist())
    return sorted(set(genes))
