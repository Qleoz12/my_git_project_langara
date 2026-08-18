"""Syed et al. (2024) Figure 1(A) pipeline — exportable module."""
from .config import ExamConfig, PaperConfig, PipelineConfig
from .deg import run_section33_deg, compute_deg_table
from .external import run_section311_external
from .feature_selection import (
    run_section34_prepare,
    run_section34_prepare_on_train,
    run_section341_mi,
    run_section342_rfecv,
    run_section343_embedded,
    run_section344_complement,
)
from .baseline import (
    build_feature_sets, evaluate_predictions, fit_rf_tune_val,
    metric_column, run_all_models_baseline, run_baseline_suite,
    run_univariate_quick_compare, select_threshold_policy, to_metrics_row,
)
from .univariate import (
    build_overlap_matrix, build_univariate_rank_table, compute_univariate_scores,
    plot_univariate_summary, select_overlap_top_k,
)
from .fs_extras import (
    build_merged_features, prune_correlated_features, run_disr,
    screened_consensus_genes, top_deg_features_by_qvalue, pca_features,
)
from .loaders import load_breast_cancer, load_geo_ibd, load_gse75214
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
from .pathway import run_section312_pathway, PAPER_PATHWAY_GENES
from .pipeline import run_pipeline
from .preprocess import run_section32_preprocess, display_preprocessing_removals, minmax_deg_matrix
from .results import PipelineResult
from .visualization import plot_volcano, plot_deg_heatmap, plot_venn, plot_hfcp, plot_overlap_heatmap, plot_confusion_matrix

__all__ = [
    "ExamConfig", "PaperConfig", "PipelineConfig", "PipelineResult",
    "run_pipeline", "run_section32_preprocess", "display_preprocessing_removals",
    "run_section33_deg", "run_section34_prepare", "run_section34_prepare_on_train",
    "run_section341_mi", "run_section342_rfecv", "run_section343_embedded",
    "run_section344_complement", "run_section35_hfcp", "run_section36_split",
    "run_section37_balance", "run_section38_loocv", "run_section39_train",
    "run_section310_evaluate", "run_section310_compare", "run_section311_external",
    "run_section312_pathway", "slice_section36_genes", "minmax_deg_matrix",
    "build_feature_sets", "run_baseline_suite", "run_all_models_baseline",
    "run_univariate_quick_compare", "evaluate_predictions", "metric_column", "to_metrics_row",
    "compute_univariate_scores", "build_univariate_rank_table", "plot_univariate_summary",
    "build_overlap_matrix", "select_overlap_top_k",
    "screened_consensus_genes", "build_merged_features", "run_disr",
    "top_deg_features_by_qvalue",
    "load_gse75214", "load_geo_ibd", "load_breast_cancer",
    "plot_volcano", "plot_deg_heatmap", "plot_venn", "plot_hfcp",
    "plot_overlap_heatmap", "plot_confusion_matrix", "PAPER_PATHWAY_GENES",
]
