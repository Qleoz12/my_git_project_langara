"""Orchestrator — run full §3.3–§3.12 pipeline."""
from __future__ import annotations

from .config import ExamConfig, PaperConfig, PipelineConfig
from .deg import run_section33_deg
from .external import run_section311_external
from .feature_selection import (
    run_section34_prepare,
    run_section34_prepare_on_train,
    run_section341_mi,
    run_section342_rfecv,
    run_section343_embedded,
    run_section344_complement,
)
from .ml import (
    run_section35_hfcp,
    run_section36_split,
    run_section37_balance,
    run_section38_loocv,
    run_section39_train,
    run_section310_evaluate,
    run_section310_compare,
    slice_section36_genes,
)
from .pathway import run_section312_pathway
from .preprocess import minmax_deg_matrix
from .results import PipelineResult
from .visualization import plot_deg_heatmap, plot_venn, plot_volcano


def _run_fs_block(s33, config, s36=None, display_fn=None):
    if config.fs_on_train_only and s36 is not None:
        s34prep = run_section34_prepare_on_train(s33, s36, config, display_fn=display_fn)
    else:
        s34prep = run_section34_prepare(s33, config, display_fn=display_fn)
    s341 = [run_section341_mi(s34prep, config)]
    s342 = [run_section342_rfecv(s34prep, config)]
    s343 = run_section343_embedded(s34prep, config)
    s344 = run_section344_complement(s34prep, s341 + s342 + s343, config, display_fn=display_fn)
    return s34prep, s341, s342, s343, s344


def run_pipeline(
    X, y, labels, config: PipelineConfig | None = None, display_fn=None,
    run_ml: bool = True, compare_balance: bool = False, preprocessed=None,
) -> PipelineResult:
    if config is None:
        config = PaperConfig()
    disp = display_fn

    s33 = run_section33_deg(X, y, labels, config, display_fn=disp, preprocessed=preprocessed)

    if disp:
        up = s33.deg_sig[s33.deg_sig["direction"] == "upregulated"]["gene"].tolist()
        down = s33.deg_sig[s33.deg_sig["direction"] == "downregulated"]["gene"].tolist()
        plot_volcano(s33.deg_table, s33.fc_threshold, config.case_label, config.ctrl_label,
                     "Volcano — DEG", show=config.show_plots)
        plot_deg_heatmap(s33.X_deg, s33.y, s33.deg_sig, config.case_label, config.ctrl_label,
                         "Heatmap — top DEGs", show=config.show_plots)
        plot_venn(up, down, "Venn — DEG directions", show=config.show_plots)

    s36 = None
    if config.fs_on_train_only and run_ml:
        deg_genes = s33.deg_sig["gene"].tolist()
        X_fs_full = minmax_deg_matrix(s33.X_deg, deg_genes)
        y_al = s33.y.reset_index(drop=True)
        labels_al = s33.labels.reset_index(drop=True)
        s36 = run_section36_split(X_fs_full, y_al, labels_al, config, display_fn=disp)

    s34prep, s341, s342, s343, s344 = _run_fs_block(s33, config, s36=s36, display_fn=disp)

    result = PipelineResult(
        config=config, section33=s33, section34_prepare=s34prep,
        section341=s341, section342=s342, section343=s343, section344=s344,
    )

    if not run_ml:
        s312 = run_section312_pathway(s344.table3_master or s344.master_biomarkers, config, display_fn=disp)
        result.section312 = s312
        return result

    ml_genes = s344.table3_master or s344.master_biomarkers
    if s36 is None:
        y_al = s33.y.reset_index(drop=True)
        labels_al = s33.labels.reset_index(drop=True)
        s36 = run_section36_split(s34prep.X_fs[ml_genes], y_al, labels_al, config, display_fn=disp)
    s36_ml = slice_section36_genes(s36, ml_genes)

    s35 = run_section35_hfcp(s34prep.X_fs, s33.y, ml_genes, s33.deg_table, config, display_fn=disp)
    if compare_balance:
        s37_dual = run_section37_balance(s36_ml.X_train, s36_ml.y_train, config, display_fn=disp, return_both=True)
        s37 = s37_dual.train_balanced
        train_unbal = (s37_dual.train_unbalanced.X_train, s37_dual.train_unbalanced.y_train)
        train_smote = (s37_dual.train_balanced.X_train, s37_dual.train_balanced.y_train)
        s38 = run_section38_loocv(train_smote[0], train_smote[1], ml_genes, config, display_fn=disp)
        s39 = run_section39_train(train_smote[0], train_smote[1], ml_genes, config)
        s310_cmp = run_section310_compare(
            train_unbal[0], train_unbal[1], "unbalanced",
            train_smote[0], train_smote[1], "smote",
            s36_ml.X_test, s36_ml.y_test, ml_genes, config, display_fn=disp,
        )
        s310 = s310_cmp.perf_smote
    else:
        s37 = run_section37_balance(s36_ml.X_train, s36_ml.y_train, config, display_fn=disp)
        s38 = run_section38_loocv(s37.X_train, s37.y_train, ml_genes, config, display_fn=disp)
        s39 = run_section39_train(s37.X_train, s37.y_train, ml_genes, config)
        s310 = run_section310_evaluate(
            s37.X_train, s37.y_train, s36_ml.X_test, s36_ml.y_test, ml_genes, config, display_fn=disp,
        )
        s310_cmp = None
    s311 = run_section311_external(
        ml_genes, s39.models, s39.genes, config, display_fn=disp,
        X_scaler_ref=result.section33.X_deg,
    )
    s312 = run_section312_pathway(ml_genes, config, display_fn=disp)

    result.section35 = s35
    result.section36 = s36_ml
    result.section37 = s37
    result.section38 = s38
    result.section39 = s39
    result.section310 = s310
    result.section310_compare = s310_cmp
    result.section311 = s311
    result.section312 = s312
    return result
