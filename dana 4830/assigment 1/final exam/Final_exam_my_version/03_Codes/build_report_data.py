"""Collect headline metrics from executed notebooks into report_data.json."""
from __future__ import annotations

import json
import pathlib

import pandas as pd

ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT = ROOT / "04_Analysis"
DATA = ROOT / "01_Data"
WANG = ["CDK1", "CDC20", "AURKA", "TOP2A", "ASPM", "NCAPG", "KIF23", "CENPF", "KIF20A", "PRC1"]


def main() -> None:
    dge = pd.read_csv(OUT / "dge_table_2_1.csv")
    if "gene" in dge.columns:
        dge = dge.set_index("gene")
    deg = dge[dge["DEG"]]
    n_15 = int(((dge["FDR"] < 0.05) & (dge["log2FC"].abs() >= 1.5)).sum())
    top = dge.nlargest(8, "log2FC")[["log2FC"]].reset_index()
    top_list = [[row["gene"], round(row["log2FC"], 2), "up"] for _, row in top.iterrows()]

    ml = pd.read_csv(OUT / "ml_biomarkers_2_1.csv")
    cmp = pd.read_csv(OUT / "panel_comparison_2_1.csv")

    cdge = pd.read_csv(OUT / "cervical_dge_table.csv")
    if "gene" in cdge.columns:
        cdge = cdge.set_index("gene")
    strict = (cdge["FDR"] < 0.05) & (cdge["log2FC"].abs() >= 1.5)
    cdeg = cdge[strict]
    wang_rows = []
    for gene in WANG:
        if gene not in cdge.index:
            continue
        is_deg = bool(strict.loc[gene])
        wang_rows.append([gene, round(float(cdge.loc[gene, "log2FC"]), 2), is_deg])

    master = pd.read_csv(DATA / "cervical_master.csv")
    cml = pd.read_csv(OUT / "cervical_ml_panel.csv")

    report = {
        "t21_deg": int(deg.shape[0]),
        "t21_up": int((deg["log2FC"] > 0).sum()),
        "t21_down": int((deg["log2FC"] < 0).sum()),
        "t21_deg15": n_15,
        "t21_top": top_list,
        "t21_ml_n": int(ml.shape[0]),
        "t21_cmp": cmp.values.tolist(),
        "t22_deg": int(cdeg.shape[0]),
        "t22_up": int((cdeg["log2FC"] > 0).sum()),
        "t22_down": int((cdeg["log2FC"] < 0).sum()),
        "t22_topup": cdeg.nlargest(8, "log2FC").index.tolist(),
        "t22_topdn": cdeg.nsmallest(6, "log2FC").index.tolist(),
        "t22_wang": wang_rows,
        "t22_wang_rec": sum(1 for row in wang_rows if row[2]),
        "t22_ml_n": int(cml.shape[0]),
        "t22_aurka": "AURKA" in set(cml["gene"]) if "gene" in cml.columns else False,
        "t22_samples": int(master.shape[0]),
        "t22_genes": int(sum(c not in ("target", "batch") for c in master.columns)),
        "t22_batch": [
            ["GSE6791", 20, 8],
            ["GSE9750", 33, 24],
            ["GSE63514", 28, 24],
            ["GSE67522", 20, 22],
        ],
    }
    (OUT / "report_data.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(f"Wrote {OUT / 'report_data.json'}")


if __name__ == "__main__":
    main()
