"""A2-style univariate feature scoring for gene expression (numeric only)."""
from __future__ import annotations

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.feature_selection import f_classif, mutual_info_classif
from sklearn.neighbors import NearestNeighbors
from sklearn.preprocessing import MinMaxScaler

from .config import PipelineConfig

METHOD_SCOPE = {
    "Pearson |r| with Target": "numeric genes, |Pearson r| with binary label",
    "ANOVA F-Statistic": "numeric genes, sklearn f_classif",
    "ReliefF Weight": "numeric genes, ReliefF-style weights on MinMax-scaled matrix",
    "Mutual Information": "numeric genes, sklearn mutual_info_classif",
}


def _gene_pool(X_train: pd.DataFrame, gene_pool: list | None) -> list:
    if gene_pool is not None:
        return [g for g in gene_pool if g in X_train.columns]
    return list(X_train.columns)


def relief_style_scores(
    X_df: pd.DataFrame,
    y_series: pd.Series,
    sample_size: int = 3000,
    n_neighbors: int = 1,
    random_state: int = 42,
) -> pd.Series:
    rng = np.random.default_rng(random_state)
    if len(X_df) > sample_size:
        idx = rng.choice(len(X_df), size=sample_size, replace=False)
        X_relief, y_relief = X_df.iloc[idx], y_series.iloc[idx]
    else:
        X_relief, y_relief = X_df, y_series
    scaler = MinMaxScaler()
    X_scaled = pd.DataFrame(
        scaler.fit_transform(X_relief.fillna(0.0)),
        columns=X_relief.columns,
        index=X_relief.index,
    )
    class0 = X_scaled[y_relief.values == 0]
    class1 = X_scaled[y_relief.values == 1]
    if len(class0) < 2 or len(class1) < 2:
        return pd.Series(0.0, index=X_df.columns, dtype=float)
    nn0 = NearestNeighbors(n_neighbors=min(n_neighbors + 1, len(class0))).fit(class0)
    nn1 = NearestNeighbors(n_neighbors=min(n_neighbors + 1, len(class1))).fit(class1)
    weights = np.zeros(X_scaled.shape[1])
    for row_idx, row in X_scaled.iterrows():
        row_arr = row.to_frame().T
        if y_relief.loc[row_idx] == 0:
            same_pool, other_pool, same_nn, other_nn = class0, class1, nn0, nn1
        else:
            same_pool, other_pool, same_nn, other_nn = class1, class0, nn1, nn0
        same_indices = same_nn.kneighbors(row_arr, return_distance=False)[0]
        same_match = same_pool.iloc[same_indices[-1]].values
        other_index = other_nn.kneighbors(row_arr, return_distance=False)[0][0]
        other_match = other_pool.iloc[other_index].values
        weights += np.abs(row.values - other_match) - np.abs(row.values - same_match)
    return pd.Series(weights / len(X_scaled), index=X_df.columns).clip(lower=0)


def compute_univariate_scores(
    X_train: pd.DataFrame,
    y_train,
    config: PipelineConfig,
    gene_pool: list | None = None,
) -> dict[str, pd.Series]:
    """Return score_tables dict keyed by method name."""
    genes = _gene_pool(X_train, gene_pool)
    X_num = X_train[genes].replace([np.inf, -np.inf], np.nan).fillna(0.0)
    y = pd.Series(y_train).reset_index(drop=True)

    pearson = X_num.corrwith(y).abs().replace([np.inf, -np.inf], np.nan).fillna(0.0)

    anova_f, _ = f_classif(X_num, y)
    anova = pd.Series(anova_f, index=genes).replace([np.inf, -np.inf], np.nan).fillna(0.0)

    mi = mutual_info_classif(X_num, y, random_state=config.random_state)
    mi_scores = pd.Series(mi, index=genes).replace([np.inf, -np.inf], np.nan).fillna(0.0)

    relief = relief_style_scores(
        X_num, y,
        sample_size=getattr(config, "relief_sample_size", 3000),
        random_state=config.random_state,
    )

    return {
        "Pearson |r| with Target": pearson.sort_values(ascending=False),
        "ANOVA F-Statistic": anova.sort_values(ascending=False),
        "ReliefF Weight": relief.sort_values(ascending=False),
        "Mutual Information": mi_scores.sort_values(ascending=False),
    }


def build_univariate_rank_table(score_tables: dict, top_n: int = 100) -> pd.DataFrame:
    rows = []
    for method, scores in score_tables.items():
        for rank, (feature, score) in enumerate(scores.head(top_n).items(), start=1):
            rows.append({
                "method": method,
                "feature_scope": METHOD_SCOPE.get(method, "numeric"),
                "rank": rank,
                "feature": feature,
                "score": float(score),
            })
    return pd.DataFrame(rows)


def build_overlap_matrix(score_tables: dict, top_n: int = 25) -> pd.DataFrame:
    overlap_top = {m: set(s.head(top_n).index) for m, s in score_tables.items()}
    all_top = sorted(set().union(*overlap_top.values()))
    matrix = pd.DataFrame({"feature": all_top})
    for m in score_tables:
        matrix[m] = [f in overlap_top[m] for f in all_top]
    matrix["n_methods"] = matrix[list(score_tables)].sum(axis=1)
    return matrix.sort_values("n_methods", ascending=False)


def select_overlap_top_k(score_tables: dict, k: int = 25, min_votes: int = 2) -> list:
    overlap = build_overlap_matrix(score_tables, top_n=k)
    voted = overlap[overlap["n_methods"] >= min_votes].sort_values(
        ["n_methods", "feature"], ascending=[False, True],
    )
    if len(voted) >= k:
        return voted["feature"].head(k).tolist()
    # pad with highest-vote singles if needed
    remaining = k - len(voted)
    extras = []
    for method, scores in score_tables.items():
        for g in scores.index:
            if g not in voted["feature"].tolist() and g not in extras:
                extras.append(g)
            if len(voted) + len(extras) >= k:
                break
        if len(voted) + len(extras) >= k:
            break
    return voted["feature"].tolist() + extras[:remaining]


def plot_univariate_summary(
    score_tables: dict,
    config: PipelineConfig,
    overlap_top_n: int | None = None,
    display_fn=None,
    csv_path: str | None = "bc_univariate_rank_top100.csv",
):
    """Bar charts (top-15 per method), overlap table, optional CSV."""
    top_n = overlap_top_n or getattr(config, "univariate_overlap_top_n", 25)
    rank_top = getattr(config, "univariate_rank_top", 100)

    print(f"Univariate methods: {len(score_tables)} | rank table top={rank_top} | overlap top={top_n}")
    rank_df = build_univariate_rank_table(score_tables, rank_top)
    if csv_path:
        rank_df.to_csv(csv_path, index=False)
        print(f"Saved {csv_path}")

    overlap_df = build_overlap_matrix(score_tables, top_n)
    if display_fn:
        display_fn(overlap_df.head(25))
    else:
        print(overlap_df.head(25).to_string(index=False))

    if config.show_plots:
        n_methods = len(score_tables)
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))
        axes = axes.ravel()
        for ax, (method, scores) in zip(axes, score_tables.items()):
            top = scores.head(15)
            ax.barh(top.index[::-1], top.values[::-1], color="steelblue", alpha=0.85)
            ax.set_title(method, fontsize=10)
            ax.set_xlabel("score")
        for j in range(len(score_tables), len(axes)):
            axes[j].set_visible(False)
        plt.suptitle("Top-15 genes per univariate method (breast cancer train)")
        plt.tight_layout()
        plt.show()

        if len(overlap_df):
            plot_df = overlap_df.head(20)
            bool_cols = list(score_tables.keys())
            heat = plot_df.set_index("feature")[bool_cols].astype(int)
            fig2, ax2 = plt.subplots(figsize=(8, max(4, len(heat) * 0.35)))
            sns.heatmap(heat, cmap="Blues", cbar=False, ax=ax2, linewidths=0.5)
            ax2.set_title(f"Top-{top_n} overlap across univariate methods")
            plt.tight_layout()
            plt.show()

    return rank_df, overlap_df
