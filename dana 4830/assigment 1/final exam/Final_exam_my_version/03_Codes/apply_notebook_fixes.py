"""Apply planned consistency fixes to all exam notebooks."""
from __future__ import annotations

import json
from pathlib import Path

CODES = Path(__file__).resolve().parent

REPLACEMENTS = [
    ('for pname, genes in [("Statistics panel (154)", strong_deg), ("Machine learning panel (45)", ml_genes)]:',
     'for pname, genes in [(f"Statistics panel ({len(strong_deg)})", strong_deg), (f"Machine learning panel ({len(ml_genes)})", ml_genes)]:'),
    ("The statistics panel is the 154 strong DEGs from\nthe Rosati pipeline. The machine learning panel is the 36 genes from Notebook 2. The\nMidterm 1 panel is the 21 breast cancer genes.",
     "The statistics panel is the strong DEGs at the 1.5 fold cutoff from\nthe Rosati pipeline. The machine learning panel is the genes from Notebook 2. The\nMidterm 1 panel is the eight breast cancer genes from table3_master_bc."),
    ("At the stricter 1.5 cutoff used in Task 2.2 the list tightens to 154 genes.",
     "At the stricter 1.5 cutoff used in Task 2.2 the list tightens to 158 genes."),
    ("11,575", "12,193"),
    ("one of the 41 genes the panel keeps", "one of the 47 genes the panel keeps"),
    ("cut the DEGs to a 41 gene panel", "cut the DEGs to a 47 gene panel"),
    ("only one of its genes is an IBD DEG", "none of its eight genes is an IBD DEG"),
    ("- Task 2.2 Four cervical cancer datasets merged and validated against Wang et al. (2022).al cancer datasets merged into one master set, both pipelines run and\n  compared with Wang et al. (2022).",
     "- Task 2.2 Four cervical cancer datasets merged into one master set, both pipelines run and\n  compared with Wang et al. (2022)."),
]

WANG_SECTION = """## Note on Wang et al. vs this merge

Wang et al. (2022) ran differential expression on each of the four GEO series separately and
then took the intersection of DEGs across studies before PPI and hub-gene analysis.

The final exam instead asks us to harmonize and merge the four datasets into one master set and
run the pipelines on that unified cohort. This is not an exact reproduction of Wang's workflow;
we use Wang's ten reported hub genes as an external literature benchmark, not as a target to match.
"""

SURVIVAL_OVERVIEW = """**Survival analysis (Rosati §4.2).** GSE75214 is a case-control IBD cohort without
follow-up time, so Kaplan-Meier or Cox analysis was not performed. ROC and pathway enrichment
were used instead to assess biomarker utility, as the dataset allows.
"""


def patch_file(name: str, extra_markdown: dict[str, str] | None = None) -> int:
    path = CODES / name
    nb = json.loads(path.read_text(encoding="utf-8"))
    changed = 0
    for cell in nb.get("cells", []):
        if cell.get("cell_type") not in ("markdown", "code"):
            continue
        src = cell.get("source", [])
        text = "".join(src)
        new_text = text
        for old, new in REPLACEMENTS:
            new_text = new_text.replace(old, new)
        if extra_markdown:
            for key, addition in extra_markdown.items():
                if key in new_text and addition.strip() not in new_text:
                    new_text = new_text.rstrip() + "\n\n" + addition
        if new_text != text:
            cell["source"] = [new_text]
            changed += 1
    if changed:
        path.write_text(json.dumps(nb, indent=1, ensure_ascii=False), encoding="utf-8")
    print(f"{name}: {changed} cells updated")
    return changed


def add_overview_report_cell() -> None:
    path = CODES / "2_0_final_overview.ipynb"
    nb = json.loads(path.read_text(encoding="utf-8"))
    code = '''# Authoritative headline metrics from executed analyses
import json
import pandas as pd
from pathlib import Path

report = json.loads((OUT / "report_data.json").read_text(encoding="utf-8"))
rows = [
    ("GSE75214 DEGs (|log2FC|>=1.0)", report["t21_deg"]),
    ("GSE75214 DEGs (|log2FC|>=1.5)", report["t21_deg15"]),
    ("GSE75214 ML panel genes", report["t21_ml_n"]),
    ("Master samples", report["t22_samples"]),
    ("Master common genes", report["t22_genes"]),
    ("Master DEGs (|log2FC|>=1.5)", report["t22_deg"]),
    ("Master ML panel genes", report["t22_ml_n"]),
    ("Wang hub genes recovered", f"{report['t22_wang_rec']}/10"),
]
summary = pd.DataFrame(rows, columns=["Metric", "Value"])
display(summary)
'''
    # Insert after setup cell (index 2) if not already present
    for cell in nb["cells"]:
        if cell.get("cell_type") == "code" and "report_data.json" in "".join(cell.get("source", [])):
            print("2_0_final_overview.ipynb: report cell already present")
            return
    nb["cells"].insert(3, {
        "cell_type": "code",
        "execution_count": None,
        "metadata": {},
        "outputs": [],
        "source": [code],
    })
    path.write_text(json.dumps(nb, indent=1, ensure_ascii=False), encoding="utf-8")
    print("2_0_final_overview.ipynb: added report_data summary cell")


def main() -> None:
    patch_file("2_1_01_data_and_dge.ipynb")
    patch_file("2_1_03_evaluation_and_comparison.ipynb")
    patch_file("2_2_01_merge_datasets.ipynb", {"## 4. Check that the batch centering worked": WANG_SECTION})
    patch_file("2_2_02_validate_pipelines.ipynb")
    patch_file("2_0_final_overview.ipynb", {"# DANA 4830 Final Exam, Consolidated Overview": SURVIVAL_OVERVIEW})
    add_overview_report_cell()


if __name__ == "__main__":
    main()
