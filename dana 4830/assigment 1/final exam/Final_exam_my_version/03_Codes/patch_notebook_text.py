"""Patch notebook markdown to use Leo's actual metrics instead of Chris's template text."""
from __future__ import annotations

import json
import pathlib

CODES = pathlib.Path(__file__).resolve().parent

REPLACEMENTS = [
    ("502 DEGs, 303 higher in disease and 199 lower", "535 DEGs, 323 higher in disease and 212 lower"),
    ("502 DEGs led by named inflammation genes", "535 DEGs led by named inflammation genes"),
    ("The cutoffs give 502 DEGs", "The cutoffs give 535 DEGs"),
    ("The pipeline finds 502 DEGs", "The pipeline finds 535 DEGs"),
    ("502 DEG genes", "535 DEG genes"),
    ("502 DEGs", "535 DEGs"),
    ("45 gene machine learning panel", "36 gene machine learning panel"),
    ("45 gene panel", "36 gene panel"),
    ("45 genes from Notebook 2", "36 genes from Notebook 2"),
    ("the 45 genes", "the 36 genes"),
    ("of the 45 genes", "of the 36 genes"),
    ("a 45 gene", "a 36 gene"),
    ("154 gene statistics panel", "158 gene statistics panel"),
    ("The two panels built on GSE75214 agree on a core of 18 genes", "The two panels built on GSE75214 agree on a core of 15 genes"),
    ("The two GSE75214 panels share a core of 18 genes", "The two GSE75214 panels share a core of 15 genes"),
    ("statistics panel and the machine learning panel share 18 genes", "statistics panel and the machine learning panel share 15 genes"),
    ("core of 18 genes and both reach an AUC near 0.99", "core of 15 genes and both reach an AUC near 0.99"),
    ("Three of its 8 genes exist in this data, yet none\nANXA1 is a strong IBD DEG, and none of them enter the machine learning panel.", "Three of its 8 genes exist on this platform, yet none are strong IBD DEGs, and none enter the machine learning panel."),
    ("only ANXA1 of its 21 genes is an IBD DEG", "none of its 8 genes is an IBD DEG"),
    ("only ANXA1 of its 21 genes is", "none of its 8 genes is"),
    ("Twenty of its 21 genes exist in this data, yet only", "Three of its 8 genes exist on this platform, yet none"),
    ("365 DEGs, 170 up and 195 down", "381 DEGs, 180 up and 201 down"),
    ("365 DEGs to a compact 41 gene panel", "381 DEGs to a compact 47 gene panel"),
    ("found 365 DEGs led by CDKN2A", "found 381 DEGs led by CDKN2A"),
    ("cuts the 365 DEGs to a 41 gene panel", "cuts the 381 DEGs to a 47 gene panel"),
    ("The merge gives 365 DEGs", "The merge gives 381 DEGs"),
    ("cuts the 365 DEGs to a 41 gene panel that reaches", "cuts the 381 DEGs to a 47 gene panel that reaches"),
]

NOTEBOOKS = [
    "2_1_01_data_and_dge.ipynb",
    "2_1_02_feature_selection.ipynb",
    "2_1_03_evaluation_and_comparison.ipynb",
    "2_2_02_validate_pipelines.ipynb",
    "2_0_final_overview.ipynb",
]


def patch_notebook(path: pathlib.Path) -> int:
    nb = json.loads(path.read_text(encoding="utf-8"))
    n = 0
    for cell in nb.get("cells", []):
        if cell.get("cell_type") != "markdown":
            continue
        src = cell.get("source", [])
        text = "".join(src)
        new_text = text
        for old, new in REPLACEMENTS:
            new_text = new_text.replace(old, new)
        if new_text != text:
            cell["source"] = [new_text]
            n += 1
    if n:
        path.write_text(json.dumps(nb, indent=1, ensure_ascii=False), encoding="utf-8")
    return n


def main() -> None:
    for name in NOTEBOOKS:
        changed = patch_notebook(CODES / name)
        print(f"{name}: {changed} markdown cells updated")


if __name__ == "__main__":
    main()
