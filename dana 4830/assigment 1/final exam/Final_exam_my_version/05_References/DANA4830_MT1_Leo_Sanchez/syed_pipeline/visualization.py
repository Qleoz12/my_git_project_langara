"""Plots: volcano, heatmap, Venn, HFCP, overlap."""
from __future__ import annotations

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

try:
    from matplotlib_venn import venn2
except ImportError:
    from matplotlib_venn import venn2


def plot_confusion_matrix(cm, ax, ctrl_label="Control", case_label="Case", title="Confusion matrix"):
    """Rows = true label, columns = predicted. Class 0 = control, class 1 = case."""
    cell_names = {(0, 0): "TN", (0, 1): "FP", (1, 0): "FN", (1, 1): "TP"}
    annot = np.array([[f"{cell_names[i, j]}\n{cm[i, j]}" for j in range(cm.shape[1])] for i in range(cm.shape[0])])
    sns.heatmap(
        cm, annot=annot, fmt="", cmap="Blues", ax=ax, cbar=False,
        xticklabels=[f"Pred {ctrl_label}\n(0)", f"Pred {case_label}\n(1)"],
        yticklabels=[f"True {ctrl_label}\n(0)", f"True {case_label}\n(1)"],
    )
    ax.set_xlabel("Predicted label →")
    ax.set_ylabel("True label →")
    ax.set_title(title)


def plot_volcano(deg_table, fc_threshold, case_name, ctrl_name, title, show=True):
    fig, ax = plt.subplots(figsize=(10, 6))
    colors = deg_table["direction"].map({
        "upregulated": "#d62728", "downregulated": "#1f77b4", "non-significant": "#bdbdbd",
    })
    ax.scatter(
        deg_table["log2FC"],
        -np.log10(deg_table["pvalue"].clip(lower=1e-300)),
        c=colors, s=8, alpha=0.6, linewidths=0,
    )
    ax.axhline(-np.log10(0.05), color="gray", ls="--", lw=0.8)
    ax.set_xlabel("log2 Fold Change")
    ax.set_ylabel("-log10(p-value)")
    ax.set_title(title)
    plt.tight_layout()
    if show:
        plt.show()
    return fig


def plot_deg_heatmap(X, y, deg_sig, case_name, ctrl_name, title, top_n=15, show=True):
    top_up = deg_sig[deg_sig["direction"] == "upregulated"].head(top_n)["gene"]
    top_down = deg_sig[deg_sig["direction"] == "downregulated"].head(top_n)["gene"]
    genes = list(top_up) + list(top_down)
    hm = X[genes].T
    hm_z = hm.sub(hm.mean(axis=1), axis=0).div(hm.std(axis=1).replace(0, np.nan), axis=0)
    col_colors = pd.Series(np.where(y == 1, case_name, ctrl_name)).map({case_name: "#d62728", ctrl_name: "#1f77b4"})
    g = sns.clustermap(hm_z, col_colors=col_colors, cmap="RdBu_r", center=0,
                       figsize=(14, 10), xticklabels=False, yticklabels=True,
                       dendrogram_ratio=(0.08, 0.12), cbar_kws={"label": "Z-score"})
    g.fig.suptitle(title, y=1.02)
    if show:
        plt.show()
    return g


def plot_venn(upreg_genes, downreg_genes, title, show=True):
    fig, ax = plt.subplots(figsize=(6, 5))
    venn2([set(upreg_genes), set(downreg_genes)], set_labels=("Upregulated", "Downregulated"), ax=ax)
    ax.set_title(title)
    plt.tight_layout()
    if show:
        plt.show()
    return fig


def plot_hfcp(X_scaled, y, genes, case_name, ctrl_name, title, show=True):
    n = len(genes)
    if n == 0:
        return None
    ncols = 3
    nrows = int(np.ceil(n / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=(4 * ncols, 3 * nrows))
    axes = np.atleast_1d(axes).flatten()
    for i, gene in enumerate(genes):
        ax = axes[i]
        sns.kdeplot(X_scaled.loc[y == 1, gene], label=case_name, fill=True, alpha=0.4, ax=ax, color="#d62728")
        sns.kdeplot(X_scaled.loc[y == 0, gene], label=ctrl_name, fill=True, alpha=0.4, ax=ax, color="#1f77b4")
        ax.set_title(gene)
        ax.set_xlabel("Min-Max Expression")
        ax.legend(fontsize=8)
    for j in range(i + 1, len(axes)):
        axes[j].axis("off")
    fig.suptitle(title, y=1.02)
    plt.tight_layout()
    if show:
        plt.show()
    return fig


def biomarker_ttest_table(X_scaled, y, master_biomarkers, deg_table):
    rows = []
    for gene in master_biomarkers:
        t_vals = X_scaled.loc[y == 1, gene]
        n_vals = X_scaled.loc[y == 0, gene]
        tt = stats.ttest_ind(t_vals, n_vals, equal_var=False)
        direction = deg_table.loc[deg_table["gene"] == gene, "direction"].values[0]
        rows.append({
            "Gene Biomarker": gene, "Direction": direction,
            "Case Mean": t_vals.mean(), "Control Mean": n_vals.mean(),
            "t-Statistic": tt.statistic, "p-Value": tt.pvalue,
        })
    return pd.DataFrame(rows)


def _genes_from_direction(picks, direction):
    val = picks.get(direction)
    if val is None:
        return set()
    if isinstance(val, (list, tuple, set)):
        return set(val)
    return {val}


def _master_genes_for_direction(selection_detail, pool_direction):
    genes = set()
    for picks in selection_detail.values():
        genes.update(_genes_from_direction(picks, pool_direction))
    return sorted(genes)


def plot_overlap_heatmap(selection_detail, title, pool_direction, show=True):
    candidates = _master_genes_for_direction(selection_detail, pool_direction)
    if not candidates:
        return pd.DataFrame()

    presence = pd.DataFrame(index=candidates)
    for method_name, picks in selection_detail.items():
        short = method_name.replace(" Score", "").replace(" Classifier", "").replace(" with Target", "")
        selected = _genes_from_direction(picks, pool_direction)
        presence[short] = [1 if g in selected else 0 for g in candidates]

    method_cols = list(presence.columns)
    presence["Total"] = presence[method_cols].sum(axis=1)
    presence = presence.sort_values("Total", ascending=False)

    if show:
        n = len(presence)
        fig_h = min(28, max(4, n * 0.28))
        fig, axes = plt.subplots(1, 2, figsize=(14, fig_h), gridspec_kw={"width_ratios": [8, 1.4], "wspace": 0.08})
        sns.heatmap(presence[method_cols], ax=axes[0], cmap="Blues", vmin=0, vmax=1, cbar_kws={"shrink": 0.35})
        axes[0].set_title(f"Per method — {pool_direction}")
        vmax = max(int(presence["Total"].max()), 1)
        sns.heatmap(presence[["Total"]], ax=axes[1], annot=True, fmt="d", cmap="YlOrRd", vmin=0, vmax=vmax)
        axes[1].set_yticklabels([])
        fig.suptitle(title, y=1.002)
        plt.tight_layout()
        plt.show()

    return presence.rename(columns={"Total": "votes"})
