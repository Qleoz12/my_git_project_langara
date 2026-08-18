"""Export the graded Midterm 1 Table-3 panel for the Final Exam.

The eight-gene list is quoted from the graded notebook
(midtermn/03_Answer/DANA4830_MT1.ipynb, table3_master_bc). Feature selection is
NOT re-run here: RFECV and GBC are stochastic and must match the submitted exam.

DEG statistics (log2FC, qvalue/FDR) are joined from midterm1_deg_table.csv,
which is the Track B DEG table already exported from that notebook's pipeline.
"""
from __future__ import annotations

from pathlib import Path

import pandas as pd

OUT = Path(__file__).resolve().parents[1] / "01_Data"

# Graded Midterm 1 Track B Table-3 master panel (do not regenerate).
GRADED_TABLE3_MASTER_BC = [
    "AC083967.1",
    "AC108477.1",
    "AC112777.1",
    "AL356275.1",
    "ARHGAP16P",
    "CST4",
    "SLC6A2",
    "TRPM3",
]


def _fdr_from_row(row: pd.Series) -> float:
    for col in ("qvalue", "adj_p", "FDR", "padj", "pvalue"):
        if col in row.index and pd.notna(row[col]):
            return float(row[col])
    return 1.0


def main() -> None:
    deg_path = OUT / "midterm1_deg_table.csv"
    if not deg_path.exists():
        raise FileNotFoundError(
            f"{deg_path} is missing. Copy it from the graded Midterm 1 export; "
            "do not re-run feature selection."
        )
    deg = pd.read_csv(deg_path).set_index("gene")

    rows = []
    for gene in GRADED_TABLE3_MASTER_BC:
        if gene in deg.index:
            r = deg.loc[gene]
            if isinstance(r, pd.DataFrame):
                r = r.iloc[0]
            log2fc = float(r.get("log2FC", r.get("logFC", 0.0)))
            fdr = _fdr_from_row(r)
            direction = str(r.get("direction", ""))
        else:
            log2fc, fdr, direction = 0.0, 1.0, ""
        rows.append(
            {"gene": gene, "log2FC": log2fc, "FDR": fdr, "direction": direction}
        )

    biomarkers = pd.DataFrame(rows)
    biomarkers.to_csv(OUT / "midterm1_biomarkers.csv", index=False)
    print(f"Wrote {OUT / 'midterm1_biomarkers.csv'}")
    print(biomarkers.to_string(index=False))


if __name__ == "__main__":
    main()
