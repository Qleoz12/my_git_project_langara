"""Pipeline configuration — explicit Exam vs Paper profiles."""
from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class PipelineConfig:
    fc_mode: str = "paper_fixed"
    fc_threshold: float = 1.06712
    fs_top_n: int = 1
    fs_overlap_n: int = 25
    fs_methods: list = field(default_factory=lambda: ["mi", "rfecv", "elastic_net", "gbc"])
    include_a2_filters: bool = False
    rfecv_pool_size: int = 300
    rfecv_gene_cap: int = 300
    fs_balance: str = "rus"
    balance_train: str = "none"
    train_frac: float = 0.65
    smote_random_state: int = 3
    smote_k_neighbors: int = 3
    loocv_fast: bool = True
    random_state: int = 42
    mi_k: int = 2
    case_label: str = "Case"
    ctrl_label: str = "Control"
    hfcp_top_n: int = 9
    show_plots: bool = True
    data_dir: str = None
    external_cohorts: list = field(default_factory=lambda: ["GSE10616", "GSE36807"])
    external_models: list = field(default_factory=lambda: ["XGBoost", "Decision Tree"])
    external_max_per_class: int = 15
    external_use_subsample: bool = True
    # §3.11: inference only — preprocess external cohorts + predict with §3.9 models (no retrain)
    external_inference_only: bool = True
    # MinMax per biomarker using GSE75214 reference (X_scaler_ref), not fit on each external cohort
    external_use_train_scaler: bool = True

    # Injectable preprocessing flags
    use_iqr: bool = True
    use_quasi_constant: bool = True
    quasi_constant_method: str = "frequency"  # "frequency" | "variance"
    quasi_constant_frequency_threshold: float = 0.99
    quasi_constant_variance_threshold: float = 0.01
    use_log1p: bool = False
    use_log2p1: bool = False
    fs_on_train_only: bool = True
    baseline_max_features: int = 500
    # Threshold tuning + model ranking: "accuracy" | "recall" | "balanced_acc" | "f1"
    optimize_metric: str = "accuracy"
    target_recall: float = 0.80
    min_policy_accuracy: float = 0.50
    correlation_prune_threshold: float = 0.82
    merged_cap: int = 160
    pca_components: int = 50
    disr_max_features: int = 10
    univariate_rank_top: int = 100
    univariate_quick_k: int = 25
    univariate_overlap_top_n: int = 25
    relief_sample_size: int = 3000

    paper_deg_counts: dict = field(
        default_factory=lambda: {"up": 1422, "down": 817, "total": 2239}
    )


@dataclass
class ExamConfig(PipelineConfig):
    balance_train: str = "rus"
    fs_balance: str = "rus"
    include_a2_filters: bool = False
    fc_mode: str = "adaptive"
    case_label: str = "Tumor"
    ctrl_label: str = "Normal"
    external_cohorts: list = field(default_factory=list)
    optimize_metric: str = "accuracy"
    quasi_constant_method: str = "variance"
    quasi_constant_variance_threshold: float = 0.01
    use_log2p1: bool = True


@dataclass
class PaperConfig(PipelineConfig):
    balance_train: str = "smote"
    fs_balance: str = "rus"
    include_a2_filters: bool = False
    fc_mode: str = "paper_fixed"
    case_label: str = "IBD"
    ctrl_label: str = "Healthy"
    optimize_metric: str = "accuracy"
    quasi_constant_method: str = "variance"
    quasi_constant_variance_threshold: float = 0.01
