"""GEO download, parsing, and multi-dataset merge for Task 2.2."""
from __future__ import annotations

import gzip
import io
import re
import urllib.request
from pathlib import Path

import numpy as np
import pandas as pd

from task2_1_lib import collapse_probes_to_genes

GEO_FTP = "https://ftp.ncbi.nlm.nih.gov/geo/series"
GSE_IDS = ["GSE6791", "GSE9750", "GSE63514", "GSE67522"]


def _gse_prefix(gse: str) -> str:
    n = gse.replace("GSE", "")
    return f"GSE{n[:-3]}nnn"


def download_series_matrix(gse: str, dest: Path) -> Path:
    """Download a GEO series matrix file if not already cached."""
    dest = Path(dest)
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.exists() and dest.stat().st_size > 1000:
        return dest
    url = f"{GEO_FTP}/{_gse_prefix(gse)}/{gse}/matrix/{gse}_series_matrix.txt.gz"
    print(f"Downloading {gse} from GEO...")
    with urllib.request.urlopen(url, timeout=120) as resp:
        dest.write_bytes(resp.read())
    return dest


def read_series_matrix(path: Path | str) -> tuple[pd.DataFrame, dict[str, list[str]]]:
    """Parse a GEO series_matrix.txt(.gz) into expression matrix + sample metadata."""
    path = Path(path)
    opener = gzip.open if str(path).endswith(".gz") else open
    meta: dict[str, list[str]] = {}
    rows: list[list[str]] = []
    headers: list[str] | None = None
    in_table = False

    with opener(path, "rt", encoding="utf-8", errors="replace") as fh:
        for line in fh:
            line = line.rstrip("\n")
            low = line.lower()
            if low.startswith("!series_matrix_table_begin"):
                in_table = True
                continue
            if low.startswith("!series_matrix_table_end"):
                break
            if not in_table:
                if line.startswith("!Sample_"):
                    parts = line.split("\t")
                    key = parts[0]
                    vals = [p.strip().strip('"') for p in parts[1:]]
                    if key not in meta:
                        meta[key] = vals
                    else:
                        for i, val in enumerate(vals):
                            if i < len(meta[key]):
                                meta[key][i] = f"{meta[key][i]} | {val}"
                            else:
                                meta[key].append(val)
                continue
            parts = line.split("\t")
            if headers is None:
                headers = [p.strip('"') for p in parts]
            else:
                rows.append([p.strip('"') for p in parts])

    if headers is None:
        raise ValueError(f"No data table found in {path}")

    df = pd.DataFrame(rows, columns=headers)
    df = df.set_index("ID_REF")
    for c in df.columns:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    return df, meta


def _meta_field(meta: dict[str, list[str]], *keys: str) -> list[str]:
    for k in keys:
        if k in meta:
            return [str(x).lower() for x in meta[k]]
    return []


def _label_samples(gse: str, meta: dict[str, list[str]], n_samples: int) -> np.ndarray:
    """Return binary labels: 1=cancer/tumor, 0=normal; -1=exclude."""
    titles = _meta_field(meta, "!Sample_title")
    sources = _meta_field(meta, "!Sample_source_name_ch1")
    chars = _meta_field(meta, "!Sample_characteristics_ch1")

    combined = []
    for i in range(n_samples):
        parts = []
        for arr in (titles, sources, chars):
            if i < len(arr):
                parts.append(arr[i])
        combined.append(" | ".join(parts))

    labels = np.full(n_samples, -1, dtype=int)
    for i, text in enumerate(combined):
        title = titles[i] if i < len(titles) else ""
        source = sources[i] if i < len(sources) else ""

        if "cell line" in text or "cell-line" in text or "hela" in text:
            continue
        if gse == "GSE6791":
            if "head and neck" in source or "head & neck" in source:
                continue
            if "cervical normal" in source or title.startswith("cn"):
                labels[i] = 0
            elif "cervical cancer" in source or title.startswith("cc"):
                labels[i] = 1
            continue
        if any(x in text for x in ["normal cervix", "normal cervical", "cervical normal", "healthy cervix"]):
            labels[i] = 0
        elif any(x in text for x in ["cervical cancer", "cervical carcinoma", "cervical adenocarcinoma",
                                      "cervical squamous", "cervical tumor", "squamous cell cancer",
                                      "squamous cell carcinoma"]) and "normal" not in text:
            labels[i] = 1
        elif "normal" in text and "cancer" not in text and "tumor" not in text and "carcinoma" not in text:
            labels[i] = 0
        elif any(x in text for x in ["cancer", "tumor", "carcinoma", "malignant"]):
            labels[i] = 1

    if (labels >= 0).sum() == 0:
        raise ValueError(f"No valid samples could be labeled in {gse}")
    unknown = int((labels == -1).sum())
    if unknown:
        print(f"  {gse}: excluding {unknown} samples (non-cervical / cell line)")
    return labels


def _to_log2(expr: pd.DataFrame) -> pd.DataFrame:
    """Put expression on log2 scale if values look like raw intensities."""
    vals = expr.values.astype(float)
    if np.nanmax(vals) > 100:
        return np.log2(expr.clip(lower=0) + 1.0)
    return expr.astype(float)


def load_gse_gene_matrix(
    gse: str, path: Path | str, probe_map: pd.DataFrame
) -> tuple[pd.DataFrame, np.ndarray]:
    """Load one GEO study as a sample-by-gene matrix with cancer/normal labels."""
    genes, labels, _audit = load_gse_gene_matrix_with_audit(gse, path, probe_map)
    return genes, labels


def load_gse_gene_matrix_with_audit(
    gse: str, path: Path | str, probe_map: pd.DataFrame
) -> tuple[pd.DataFrame, np.ndarray, dict]:
    """Same load as before, plus a provenance/EDA audit dict. Merge logic is unchanged."""
    path = Path(path)
    if not path.exists():
        path = download_series_matrix(gse, path)

    expr, meta = read_series_matrix(path)
    raw_probes, raw_samples = expr.shape  # GEO table: probes × samples
    raw_vals = expr.to_numpy(dtype=float)
    n_mapped = int(probe_map["ID"].astype(str).isin(expr.index.astype(str)).sum()) if "ID" in probe_map.columns else np.nan

    expr_log = _to_log2(expr)
    labels_all = _label_samples(gse, meta, expr_log.shape[1])
    n_excluded = int((labels_all < 0).sum())
    keep = labels_all >= 0
    expr_keep = expr_log.loc[:, keep]
    labels = labels_all[keep]

    genes = collapse_probes_to_genes(expr_keep.T, probe_map)
    finite = raw_vals[np.isfinite(raw_vals)]
    audit = {
        "Dataset": gse,
        "Raw orientation": "probes × samples",
        "Raw samples": int(raw_samples),
        "Raw probes": int(raw_probes),
        "Excluded samples": n_excluded,
        "Retained samples": int(keep.sum()),
        "Cancer": int((labels == 1).sum()),
        "Normal": int((labels == 0).sum()),
        "Probes mapped to a symbol": n_mapped,
        "Pct probes with a map": round(100 * n_mapped / max(raw_probes, 1), 2) if pd.notna(n_mapped) else np.nan,
        "Genes after mapping": int(genes.shape[1]),
        "Missing cells (raw)": int(np.isnan(raw_vals).sum()),
        "Min (raw finite)": float(np.min(finite)) if finite.size else np.nan,
        "Median (raw finite)": float(np.median(finite)) if finite.size else np.nan,
        "Max (raw finite)": float(np.max(finite)) if finite.size else np.nan,
        "log2 applied": bool(np.nanmax(raw_vals) > 100) if np.isfinite(raw_vals).any() else False,
    }
    return genes, labels.astype(int), audit


def merge_datasets(
    matrices: list[pd.DataFrame],
    labels: list[np.ndarray],
    batch_names: list[str],
    center: bool = True,
) -> tuple[pd.DataFrame, np.ndarray, np.ndarray, list[str]]:
    """Intersect genes, optionally center each dataset, and stack samples.

    center=True is the exam merge (study-wise centering). center=False is only
    for a before-adjustment PCA check and is not saved as the master matrix.
    """
    common = set(matrices[0].columns)
    for m in matrices[1:]:
        common &= set(m.columns)
    common_genes = sorted(common)
    if not common_genes:
        raise ValueError("No shared genes across datasets")

    stacked = []
    y_all = []
    batch = []
    for mat, y, name in zip(matrices, labels, batch_names):
        sub = mat[common_genes].copy()
        if center:
            sub = sub - sub.mean(axis=0)
        stacked.append(sub)
        y_all.append(np.asarray(y))
        batch.extend([name] * len(y))

    master = pd.concat(stacked, axis=0, ignore_index=True)
    y_all = np.concatenate(y_all)
    batch = np.array(batch)
    return master, y_all, batch, common_genes


def ensure_geo_cache(data_dir: Path | str) -> None:
    """Download all four cervical GEO matrices into data_dir/geo_raw."""
    data_dir = Path(data_dir)
    raw = data_dir / "geo_raw"
    for gse in GSE_IDS:
        download_series_matrix(gse, raw / f"{gse}_series_matrix.txt.gz")
