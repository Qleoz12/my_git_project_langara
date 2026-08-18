"""§3.4 feature selection — paper Table 3 selectors."""
from __future__ import annotations

import numpy as np
import pandas as pd
from imblearn.under_sampling import RandomUnderSampler
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier
from sklearn.feature_selection import SelectKBest, f_classif, mutual_info_classif, RFECV
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold
from sklearn.neighbors import NearestNeighbors
from sklearn.preprocessing import MinMaxScaler

from .config import PipelineConfig
from .preprocess import minmax_deg_matrix
from .results import FSSelectionResult, Section34PrepareResult, Section344Result
from .visualization import plot_overlap_heatmap


def top_genes_by_direction(gene_list, deg_df, n=300):
    sub = deg_df[deg_df["gene"].isin(gene_list)].copy()
    sub["abs_log2fc"] = sub["log2FC"].abs()
    return sub.sort_values(["qvalue", "abs_log2fc"], ascending=[True, False]).head(n)["gene"].tolist()


def _pick_top_n(scores, genes, n):
    if not genes:
        return []
    ranked = scores.reindex(genes).dropna().sort_values(ascending=False)
    return ranked.head(min(n, len(ranked))).index.tolist()


def select_mi(X_in, y_in, genes, config: PipelineConfig):
    if not genes:
        return None, []
    k_use = min(config.mi_k, len(genes))

    def mi_score(X, y):
        return mutual_info_classif(X, y, n_neighbors=3, random_state=config.random_state)

    selector = SelectKBest(score_func=mi_score, k=k_use)
    X_sub = X_in[genes].replace([np.inf, -np.inf], np.nan).fillna(0.0)
    selector.fit(X_sub, y_in)
    scores = pd.Series(selector.scores_, index=genes).sort_values(ascending=False)
    top_overlap = _pick_top_n(scores, genes, config.fs_overlap_n)
    top_table = top_overlap[: config.fs_top_n]
    return (top_table[0] if top_table else None), top_overlap


def select_rfecv(X_in, y_in, genes, config: PipelineConfig):
    if not genes:
        return None, [], {}
    genes = genes[: config.rfecv_gene_cap]
    X_sub = X_in[genes].replace([np.inf, -np.inf], np.nan).fillna(0.0)
    if X_sub.shape[1] < 11:
        top_overlap = genes[: config.fs_overlap_n]
        top_table = top_overlap[: config.fs_top_n]
        return (top_table[0] if top_table else None), top_overlap, {}
    rfecv = RFECV(
        estimator=RandomForestClassifier(n_estimators=100, random_state=config.random_state, n_jobs=-1),
        step=1,
        min_features_to_select=min(10, X_sub.shape[1] - 1),
        cv=StratifiedKFold(5, shuffle=True, random_state=config.random_state),
        scoring="roc_auc",
        n_jobs=-1,
    )
    rfecv.fit(X_sub, y_in)
    ranking = pd.Series(rfecv.ranking_, index=genes).sort_values()
    top_overlap = ranking.head(config.fs_overlap_n).index.tolist()
    top_table = top_overlap[: config.fs_top_n]
    extra = {"n_features_": getattr(rfecv, "n_features_", None), "ranking": ranking}
    return (top_table[0] if top_table else None), top_overlap, extra


def select_elastic_net(X_in, y_in, genes, config: PipelineConfig, max_features=2):
    if not genes:
        return None, []
    model = LogisticRegression(
        penalty="elasticnet", solver="saga", l1_ratio=1.0, C=1.0,
        max_iter=1000, random_state=config.random_state,
    )
    X_sub = X_in[genes].fillna(0.0)
    model.fit(X_sub, y_in)
    coef = pd.Series(np.abs(model.coef_.ravel()), index=genes).sort_values(ascending=False)
    nonzero = coef[coef > 0]
    ranked = (nonzero if len(nonzero) else coef)
    top_overlap = ranked.head(config.fs_overlap_n).index.tolist()
    top_table = top_overlap[: config.fs_top_n]
    return (top_table[0] if top_table else None), top_overlap


def select_gbc(X_in, y_in, genes, config: PipelineConfig, max_features=2):
    if not genes:
        return None, []
    X_sub = X_in[genes].fillna(0.0)
    model = GradientBoostingClassifier(
        n_estimators=100, learning_rate=0.1, max_depth=3,
        min_samples_split=2, min_samples_leaf=1, subsample=1.0,
        max_features=min(max_features, X_sub.shape[1]), random_state=config.random_state,
    )
    model.fit(X_sub, y_in)
    imp = pd.Series(model.feature_importances_, index=genes).sort_values(ascending=False)
    top_overlap = imp.head(config.fs_overlap_n).index.tolist()
    top_table = top_overlap[: config.fs_top_n]
    return (top_table[0] if top_table else None), top_overlap


def run_section34_prepare_on_train(deg_result, s36, config: PipelineConfig, display_fn=None) -> Section34PrepareResult:
    """FS on train split only (anti-leakage). Full X_fs kept for HFCP on all samples."""
    deg_sig = deg_result.deg_sig
    upreg = deg_sig[deg_sig["direction"] == "upregulated"]["gene"].tolist()
    downreg = deg_sig[deg_sig["direction"] == "downregulated"]["gene"].tolist()
    deg_genes = deg_sig["gene"].tolist()

    X_fs = minmax_deg_matrix(deg_result.X_deg, deg_genes)
    X_train_fs = s36.X_train.copy()
    y_train = s36.y_train

    if config.fs_balance == "rus":
        rus = RandomUnderSampler(random_state=config.random_state)
        X_bal, y_bal = rus.fit_resample(X_train_fs, y_train)
        X_bal = pd.DataFrame(X_bal, columns=X_train_fs.columns, index=range(len(y_bal))).fillna(0.0)
    else:
        X_bal, y_bal = X_train_fs.copy(), y_train

    up_pool = top_genes_by_direction(upreg, deg_sig, config.rfecv_pool_size)
    down_pool = top_genes_by_direction(downreg, deg_sig, config.rfecv_pool_size)

    if display_fn:
        display_fn(pd.DataFrame([{
            "FS mode": "train_only",
            "train_samples": len(y_train),
            "test_samples": len(s36.y_test),
            "deg_genes": len(deg_genes),
        }]))

    return Section34PrepareResult(
        X_fs=X_fs, X_bal=X_bal, y_bal=y_bal,
        up_pool=up_pool, down_pool=down_pool,
        deg_genes=deg_genes, deg_sig=deg_sig,
    )


def run_section34_prepare(deg_result, config: PipelineConfig, display_fn=None) -> Section34PrepareResult:
    X_deg = deg_result.X_deg
    y = deg_result.y
    deg_sig = deg_result.deg_sig
    upreg = deg_sig[deg_sig["direction"] == "upregulated"]["gene"].tolist()
    downreg = deg_sig[deg_sig["direction"] == "downregulated"]["gene"].tolist()
    deg_genes = deg_sig["gene"].tolist()

    X_fs = minmax_deg_matrix(X_deg, deg_genes)
    if config.fs_balance == "rus":
        rus = RandomUnderSampler(random_state=config.random_state)
        X_bal, y_bal = rus.fit_resample(X_fs, y)
        X_bal = pd.DataFrame(X_bal, columns=deg_genes, index=range(len(y_bal))).fillna(0.0)
    else:
        X_bal, y_bal = X_fs.copy(), y

    up_pool = top_genes_by_direction(upreg, deg_sig, config.rfecv_pool_size)
    down_pool = top_genes_by_direction(downreg, deg_sig, config.rfecv_pool_size)

    return Section34PrepareResult(
        X_fs=X_fs, X_bal=X_bal, y_bal=y_bal,
        up_pool=up_pool, down_pool=down_pool,
        deg_genes=deg_genes, deg_sig=deg_sig,
    )


def _fs_row(name, fs_type, up_g, down_g, up_list, down_list, extra=None):
    return FSSelectionResult(
        method=name, fs_type=fs_type,
        up_gene=up_g, down_gene=down_g,
        up_list=up_list, down_list=down_list,
        extra=extra or {},
    )


def run_section341_mi(fs_ctx: Section34PrepareResult, config: PipelineConfig) -> FSSelectionResult:
    up_g, up_l = select_mi(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.up_pool, config)
    down_g, down_l = select_mi(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.down_pool, config)
    return _fs_row("Mutual Information Score", "Filter", up_g, down_g, up_l, down_l)


def run_section342_rfecv(fs_ctx: Section34PrepareResult, config: PipelineConfig) -> FSSelectionResult:
    up_g, up_l, ex_up = select_rfecv(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.up_pool, config)
    down_g, down_l, ex_down = select_rfecv(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.down_pool, config)
    extra = {"up": ex_up, "down": ex_down}
    return _fs_row("RFECV", "Wrapper", up_g, down_g, up_l, down_l, extra)


def run_section343_embedded(fs_ctx: Section34PrepareResult, config: PipelineConfig) -> list:
    up_en, up_en_l = select_elastic_net(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.up_pool, config)
    down_en, down_en_l = select_elastic_net(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.down_pool, config)
    up_gb, up_gb_l = select_gbc(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.up_pool, config)
    down_gb, down_gb_l = select_gbc(fs_ctx.X_bal, fs_ctx.y_bal, fs_ctx.down_pool, config)
    return [
        _fs_row("Elastic Net", "Embedded", up_en, down_en, up_en_l, down_en_l),
        _fs_row("Gradient Boosting Classifier", "Embedded", up_gb, down_gb, up_gb_l, down_gb_l),
    ]


def _results_to_table3(results: list) -> pd.DataFrame:
    rows = []
    for r in results:
        rows.append({
            "FS Methods": r.method,
            "Type of FS Method": r.fs_type,
            "Selected Upregulated": r.up_gene,
            "Selected Downregulated": r.down_gene,
            "N_up": len(r.up_list),
            "N_down": len(r.down_list),
            "Top_up": r.up_list,
            "Top_down": r.down_list,
        })
    return pd.DataFrame(rows)


def _count_method_votes(detail, gene, pool_direction):
    return sum(
        1 for picks in detail.values()
        if gene in _genes_from_direction(picks, pool_direction)
    )


def _genes_from_direction(picks, direction):
    val = picks.get(direction)
    if val is None:
        return set()
    if isinstance(val, (list, tuple, set)):
        return set(val)
    return {val}


def build_master_merged(detail, deg_sig, pool_direction):
    gene_methods = {}
    for method_name, picks in detail.items():
        for gene in _genes_from_direction(picks, pool_direction):
            gene_methods.setdefault(gene, set()).add(method_name)

    rows = []
    for gene, methods in gene_methods.items():
        meta = deg_sig[deg_sig["gene"] == gene]
        row = {
            "gene": gene,
            "method_votes": len(methods),
            "methods": ", ".join(sorted(methods)),
        }
        if len(meta):
            r = meta.iloc[0]
            row.update({"log2FC": r["log2FC"], "qvalue": r["qvalue"]})
        rows.append(row)

    if not rows:
        return pd.DataFrame()
    return pd.DataFrame(rows).sort_values(
        ["method_votes", "qvalue"], ascending=[False, True], na_position="last",
    ).reset_index(drop=True)


def run_section344_complement(fs_ctx, fs_results: list, config: PipelineConfig, display_fn=None) -> Section344Result:
    table3 = _results_to_table3(fs_results)
    detail = {}
    for r in fs_results:
        detail[r.method] = {"upregulated": r.up_list, "downregulated": r.down_list}

    table3_master = sorted({
        g for r in fs_results for g in (r.up_gene, r.down_gene) if g
    })
    master = sorted({
        g for r in fs_results for g in (r.up_list + r.down_list) if g
    })

    merged_up = build_master_merged(detail, fs_ctx.deg_sig, "upregulated")
    merged_down = build_master_merged(detail, fs_ctx.deg_sig, "downregulated")

    overlap_up = plot_overlap_heatmap(
        detail, f"FS overlap — UPREGULATED (top-{config.fs_overlap_n} per method)", "upregulated",
        show=config.show_plots,
    )
    overlap_down = plot_overlap_heatmap(
        detail, f"FS overlap — DOWNREGULATED (top-{config.fs_overlap_n} per method)", "downregulated",
        show=config.show_plots,
    )

    if display_fn:
        display_fn(table3[["FS Methods", "Type of FS Method", "Selected Upregulated",
                           "Selected Downregulated", "N_up", "N_down"]])
        display_fn(pd.DataFrame({"table3_master (#1 each method)": table3_master}))

        print(f"\nUnion of all top-{config.fs_overlap_n} lists: {len(master)} genes "
              f"(UP={len(merged_up)}, DOWN={len(merged_down)})")

        for direction, merged, overlap in [
            ("UPREGULATED", merged_up, overlap_up),
            ("DOWNREGULATED", merged_down, overlap_down),
        ]:
            if overlap.empty:
                continue
            vote_col = "votes" if "votes" in overlap.columns else "Total"
            summary = overlap[vote_col].value_counts().sort_index(ascending=False)
            print(f"\n--- {direction}: vote distribution (methods agreeing) ---")
            display_fn(pd.DataFrame({"votes": summary.index, "n_genes": summary.values}))
            consensus = overlap[overlap[vote_col] >= 2].head(15)
            if len(consensus):
                print(f"Top consensus genes ({direction}, votes >= 2):")
                display_fn(consensus)

        print("\n--- Per-method selected lists (top overlap pool) ---")
        for method_name, picks in detail.items():
            display_fn(pd.DataFrame({
                "method": [method_name],
                "n_up": [len(picks["upregulated"])],
                "n_down": [len(picks["downregulated"])],
                "top5_up": [", ".join(list(picks["upregulated"])[:5])],
                "top5_down": [", ".join(list(picks["downregulated"])[:5])],
            }))

        if len(merged_up):
            print("\nMaster merged — UPREGULATED (sorted by method_votes, log2FC, qvalue):")
            display_fn(merged_up)
        if len(merged_down):
            print("\nMaster merged — DOWNREGULATED (sorted by method_votes, log2FC, qvalue):")
            display_fn(merged_down)

    return Section344Result(
        table3=table3, detail=detail, master_biomarkers=master,
        overlap_up=overlap_up, overlap_down=overlap_down,
        master_merged_up=merged_up, master_merged_down=merged_down,
        table3_master=table3_master,
    )
