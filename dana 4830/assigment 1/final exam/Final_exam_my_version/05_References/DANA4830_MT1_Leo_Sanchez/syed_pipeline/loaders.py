"""Data loaders for GSE75214 and breast cancer exam cohort."""
from __future__ import annotations

import tarfile
from pathlib import Path

import numpy as np
import pandas as pd

try:
    import GEOparse
except ImportError:
    GEOparse = None


def _parse_gene_assignment(val):
    if pd.isna(val) or not str(val).strip():
        return np.nan
    block = str(val).split(" /// ")[0]
    parts = [p.strip() for p in block.split(" // ")]
    return parts[1] if len(parts) >= 2 else np.nan


def _refseq_to_symbols(acc_series):
    """Map RefSeq accessions (NM_*) to gene symbols via mygene when available."""
    acc = acc_series.astype(str).str.strip()
    acc = acc[acc.str.match(r"NM_\d+", na=False)].unique().tolist()
    if not acc:
        return {}
    try:
        import mygene
        mg = mygene.MyGeneInfo()
        hits = mg.querymany(acc, scopes="refseq.rna", fields="symbol", species="human", verbose=False)
        out = {}
        for h in hits:
            if h.get("notfound"):
                continue
            sym = h.get("symbol")
            q = h.get("query")
            if sym and q:
                out[q] = str(sym).upper()
        return out
    except Exception:
        return {}


def _probe_to_gene_map(gpl):
    annot = gpl.table.copy()
    id_col = "ID" if "ID" in annot.columns else annot.columns[0]
    if "gene_assignment" in annot.columns:
        return annot.set_index(id_col)["gene_assignment"].map(_parse_gene_assignment)
    for col in annot.columns:
        cl = col.lower()
        if "gene symbol" in cl or cl == "symbol" or cl.endswith("_symbol"):
            return annot.set_index(id_col)[col].astype(str).replace("", np.nan)
    if "GB_ACC" in annot.columns:
        ref_map = _refseq_to_symbols(annot["GB_ACC"])
        if ref_map:
            sym = annot["GB_ACC"].astype(str).str.strip().map(ref_map)
            sym.index = annot[id_col].astype(str)
            return sym
    gene_col = next((c for c in annot.columns if "symbol" in c.lower()), annot.columns[-1])
    return annot.set_index(id_col)[gene_col].astype(str).replace("", np.nan)


def _label_from_gsm(gsm):
    chars = gsm.metadata.get("characteristics_ch1", [])
    source = " ".join(gsm.metadata.get("source_name_ch1", []))
    title = " ".join(gsm.metadata.get("title", []))
    text = " | ".join([source, title] + list(chars)).lower()
    if any(k in text for k in ["control", "healthy", "normal"]):
        return "Healthy"
    if any(k in text for k in ["colitis", "crohn", "ibd", "uc", " cd", "cd ", "disease", "inflam"]):
        return "IBD"
    return "Unknown"


def _load_geo_series(geo_id, data_dir):
    if GEOparse is None:
        raise ImportError("GEOparse required for GEO loaders")

    data_dir = Path(data_dir)
    tar_path = data_dir / f"{geo_id}_RAW.tar"
    extract_dir = data_dir / f"{geo_id}_RAW"
    if tar_path.exists():
        extract_dir.mkdir(parents=True, exist_ok=True)
        with tarfile.open(tar_path, "r") as tar:
            tar.extractall(path=extract_dir)

    soft_path = data_dir / f"{geo_id}_family.soft.gz"
    if soft_path.exists():
        gse = GEOparse.get_GEO(filepath=str(soft_path), silent=True)
    else:
        gse = GEOparse.get_GEO(geo=geo_id, destdir=str(data_dir), silent=True)
    return gse


def _gse_to_matrix(gse):
    rows = []
    for gsm_name, gsm in gse.gsms.items():
        rows.append({"sample": gsm_name, "label": _label_from_gsm(gsm)})
    meta = pd.DataFrame(rows)
    meta = meta[meta["label"].isin(["IBD", "Healthy"])].copy()

    expr = gse.pivot_samples("VALUE")
    expr.index = expr.index.astype(str).str.strip('"')
    meta = meta[meta["sample"].isin(expr.columns)].copy()
    expr = expr[meta["sample"].tolist()]

    gpl = list(gse.gpls.values())[0]
    probe_to_gene = _probe_to_gene_map(gpl)
    probe_to_gene.index = probe_to_gene.index.astype(str)
    expr.index = expr.index.astype(str).map(probe_to_gene)
    expr = expr[~expr.index.isna()]
    expr.index = expr.index.astype(str)
    expr = expr.groupby(expr.index).mean().T
    expr = expr.apply(pd.to_numeric, errors="coerce").astype(np.float32)

    sample_order = expr.index.tolist()
    labels = meta.set_index("sample").loc[sample_order, "label"].reset_index(drop=True)
    y = (labels == "IBD").astype(int)
    X = expr.reset_index(drop=True)
    return X, y, labels


def load_geo_ibd(geo_id, data_dir=None):
    """Load IBD vs Healthy matrix from GEO (GSE10616, GSE36807, GSE75214, …)."""
    if data_dir is None:
        data_dir = Path(__file__).resolve().parent.parent / "01_Data"
    return _gse_to_matrix(_load_geo_series(geo_id, data_dir))


def load_gse75214(data_dir=None):
    if data_dir is None:
        data_dir = Path(__file__).resolve().parent.parent / "01_Data"
    else:
        data_dir = Path(data_dir)

    if GEOparse is None:
        raise ImportError("GEOparse required for load_gse75214")

    gse = _load_geo_series("GSE75214", data_dir)
    return _gse_to_matrix(gse)


def load_breast_cancer(data_dir=None):
    if data_dir is None:
        data_dir = Path(__file__).resolve().parent.parent / "01_Data"
    else:
        data_dir = Path(data_dir)

    y_raw = pd.read_csv(data_dir / "y_data_full.csv", header=None).squeeze("columns").astype(str).str.strip()
    X = pd.read_csv(data_dir / "X_data_full.csv", index_col=0)
    X = X.apply(pd.to_numeric, errors="coerce").astype(np.float32)
    valid = y_raw.isin(["Tumor", "Normal"])
    labels = y_raw[valid].reset_index(drop=True)
    X = X.reset_index(drop=True)
    if len(labels) != len(X):
        n = min(len(labels), len(X))
        labels, X = labels.iloc[:n].reset_index(drop=True), X.iloc[:n].reset_index(drop=True)
    y = (labels == "Tumor").astype(int)
    return X, y, labels
