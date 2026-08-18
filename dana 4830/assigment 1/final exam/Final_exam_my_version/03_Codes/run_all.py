"""Run the full final exam pipeline in order."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

CODES = Path(__file__).resolve().parent
ROOT = CODES.parent
ANALYSIS = ROOT / "04_Analysis"
FIGURES = ANALYSIS / "figures"

NOTEBOOKS = [
    "2_1_01_data_and_dge.ipynb",
    "2_1_02_feature_selection.ipynb",
    "2_1_03_evaluation_and_comparison.ipynb",
    "2_2_01_merge_datasets.ipynb",
    "2_2_02_validate_pipelines.ipynb",
    "2_0_final_overview.ipynb",
]

SCRIPTS = [
    "make_diagrams.py",
    "build_report_data.py",
    "sync_notebook_numbers.py",
]

OPTIONAL_SCRIPTS = [
    "build_final_report.py",
]


def run(cmd: list[str]) -> None:
    print(">>", " ".join(cmd))
    subprocess.run(cmd, cwd=CODES, check=True)


def clean_outputs() -> None:
    for pattern in ["*.csv", "*.html"]:
        for path in ANALYSIS.glob(pattern):
            path.unlink()
    if FIGURES.exists():
        for path in FIGURES.glob("*.png"):
            path.unlink()
    master = ROOT / "01_Data" / "cervical_master.csv"
    if master.exists():
        master.unlink()
    print("Cleaned generated outputs.")


def export_overview_pdf() -> None:
    """Export master overview to PDF (webpdf). HTML print-to-PDF is the fallback."""
    overview = "2_0_final_overview.ipynb"
    pdf_path = ANALYSIS / "2_0_final_overview.pdf"
    try:
        run([
            sys.executable, "-m", "jupyter", "nbconvert",
            "--to", "webpdf", overview,
            "--output-dir", str(ANALYSIS),
        ])
        print(f"PDF exported: {pdf_path}")
    except subprocess.CalledProcessError:
        html_path = ANALYSIS / "2_0_final_overview.html"
        print(
            "webpdf export failed (Chrome/Chromium may be missing). "
            f"Open {html_path} in a browser and use Print → Save as PDF."
        )


def main() -> None:
    if "--clean" in sys.argv:
        clean_outputs()

    if "--overview-pdf" in sys.argv:
        run([sys.executable, "fix_notebook_outputs.py", "2_0_final_overview.ipynb"])
        run([
            sys.executable, "-m", "jupyter", "nbconvert",
            "--execute", "--inplace", "2_0_final_overview.ipynb",
        ])
        run([sys.executable, "fix_notebook_outputs.py", "2_0_final_overview.ipynb"])
        run([
            sys.executable, "-m", "jupyter", "nbconvert",
            "--to", "html", "2_0_final_overview.ipynb",
            "--output-dir", str(ANALYSIS),
        ])
        export_overview_pdf()
        print("Overview PDF ready:", ANALYSIS / "2_0_final_overview.pdf")
        return

    with_docx = "--with-docx" in sys.argv

    for nb in NOTEBOOKS:
        run([sys.executable, "fix_notebook_outputs.py", nb])
        run([sys.executable, "-m", "jupyter", "nbconvert", "--execute", "--inplace", nb])

    run([sys.executable, "fix_notebook_outputs.py"])
    for script in SCRIPTS:
        run([sys.executable, script])
    if with_docx:
        for script in OPTIONAL_SCRIPTS:
            run([sys.executable, script])

    for nb in NOTEBOOKS:
        run([
            sys.executable, "-m", "jupyter", "nbconvert",
            "--to", "html", nb,
            "--output-dir", str(ANALYSIS),
        ])

    export_overview_pdf()
    print("Pipeline complete.")


if __name__ == "__main__":
    main()
