"""Verify notebooks contain no obsolete Chris-template numbers."""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

CODES = Path(__file__).resolve().parent
ROOT = CODES.parent
REPORT = ROOT / "04_Analysis" / "report_data.json"

NOTEBOOKS = [
    "2_1_01_data_and_dge.ipynb",
    "2_1_02_feature_selection.ipynb",
    "2_1_03_evaluation_and_comparison.ipynb",
    "2_2_01_merge_datasets.ipynb",
    "2_2_02_validate_pipelines.ipynb",
    "2_0_final_overview.ipynb",
]

# Patterns that should NOT appear in source/markdown (not in base64 image data)
OBSOLETE_PATTERNS = [
    (r"\b502 DEG", "502 DEGs (use 535)"),
    (r"Statistics panel \(154\)", "hard-coded Statistics panel (154)"),
    (r"Machine learning panel \(45\)", "hard-coded ML panel (45)"),
    (r"36 gene machine learning panel", "old 36-gene ML panel (use report_data.json t21_ml_n)"),
    (r"a 36 gene panel", "old 36-gene panel (use t21_ml_n)"),
    (r"\b154 strong DEGs\b", "154 strong DEGs (use 158)"),
    (r"\b154 genes\b", "154 genes (use 158)"),
    (r"21 breast cancer genes", "21 breast cancer genes (use 8)"),
    (r"11,575", "11,575 genes (use 12,193)"),
    (r"\b41 gene panel\b", "41 gene panel (use 47)"),
    (r"one of the 41 genes\b", "41 genes in ML panel (use 47)"),
    (r"only one of its genes is an IBD DEG", "only one MT1 gene (use none of 8)"),
    (r"At the stricter 1\.5 cutoff used in Task 2\.2 the list tightens to 154 genes", "154 at 1.5 cutoff (use 158)"),
]


def cell_text(cell: dict) -> str:
  if cell.get("cell_type") == "markdown":
    return "".join(cell.get("source", []))
  if cell.get("cell_type") == "code":
    return "".join(cell.get("source", []))
  return ""


def scan_notebook(path: Path) -> list[str]:
    issues = []
    nb = json.loads(path.read_text(encoding="utf-8"))
    for i, cell in enumerate(nb.get("cells", [])):
        text = cell_text(cell)
        if not text or "image/png" in text:
            continue
        for pattern, label in OBSOLETE_PATTERNS:
            if re.search(pattern, text):
                issues.append(f"{path.name} cell {i}: {label}")
    return issues


def main() -> int:
    all_issues: list[str] = []
    for name in NOTEBOOKS:
        all_issues.extend(scan_notebook(CODES / name))

    if all_issues:
        print("OBSOLETE NUMBER CHECK FAILED:")
        for issue in all_issues:
            print(f"  - {issue}")
        return 1

    if REPORT.exists():
        print(f"report_data.json present: {REPORT}")
    print("All notebook source/markdown checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
