"""Injectable preprocessing / encoding steps with logging."""
from .encode import binary_encode_labels, encoding_audit_table
from .filters import drop_quasi_constant, remove_iqr_outlier_samples
from .log import StepLog
from .plot import plot_correlation_matrix, plot_preprocessing_bars
from .scale import log1p_transform, log2p1_transform, minmax_fit_transform

__all__ = [
    "StepLog",
    "binary_encode_labels",
    "encoding_audit_table",
    "drop_quasi_constant",
    "remove_iqr_outlier_samples",
    "minmax_fit_transform",
    "log1p_transform",
    "log2p1_transform",
    "plot_preprocessing_bars",
    "plot_correlation_matrix",
]
