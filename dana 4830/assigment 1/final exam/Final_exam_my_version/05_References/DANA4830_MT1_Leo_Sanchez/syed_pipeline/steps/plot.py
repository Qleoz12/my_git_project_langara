"""Diagnostic plots for preprocessing and correlations."""
from __future__ import annotations

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns


def plot_preprocessing_bars(comparison: pd.DataFrame, show: bool = True):
    fig, axes = plt.subplots(1, 2, figsize=(12, 4))
    n = len(comparison)
    if "Stage" in comparison.columns:
        labels = comparison["Stage"].tolist()
    else:
        labels = [f"Step {i}" for i in range(n)]
    axes[0].bar(range(n), comparison["n_samples"], color="steelblue")
    axes[0].set_xticks(range(n))
    axes[0].set_xticklabels(labels, rotation=25, ha="right", fontsize=8)
    axes[0].set_title("Samples — before vs after")
    axes[1].bar(range(n), comparison["n_genes"], color="coral")
    axes[1].set_xticks(range(n))
    axes[1].set_xticklabels(labels, rotation=25, ha="right", fontsize=8)
    axes[1].set_title("Genes — before vs after")
    plt.tight_layout()
    if show:
        plt.show()
    return fig


def plot_correlation_matrix(X: pd.DataFrame, features: list, title: str = "Correlation", show: bool = True):
    cols = [f for f in features if f in X.columns]
    if len(cols) < 2:
        return None
    sub = X[cols].corr()
    fig, ax = plt.subplots(figsize=(max(8, len(cols) * 0.25), max(6, len(cols) * 0.25)))
    sns.heatmap(sub, cmap="coolwarm", center=0, vmin=-1, vmax=1, ax=ax, square=len(cols) <= 30)
    ax.set_title(title)
    plt.tight_layout()
    if show:
        plt.show()
    return fig
