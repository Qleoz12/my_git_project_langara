"""Task 1.2 diagrams. Scientific steps stay the same; this file only improves layout."""
from __future__ import annotations

import pathlib
import shutil

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch, Rectangle

ROOT = pathlib.Path(__file__).resolve().parent.parent
FIG = ROOT / "04_Analysis" / "figures"
FIG.mkdir(parents=True, exist_ok=True)


def _box(ax, x, y, w, h, text, fc="#E8F4FD", ec="#1F4E79", fontsize=8.2, fw="normal"):
    ax.add_patch(
        FancyBboxPatch(
            (x, y), w, h, boxstyle="round,pad=0.012,rounding_size=0.06",
            linewidth=1.15, edgecolor=ec, facecolor=fc,
        )
    )
    ax.text(x + w / 2, y + h / 2, text, ha="center", va="center", fontsize=fontsize, fontweight=fw, color="#1a1a1a")


def _phase(ax, x, y, w, h, title, fc):
    ax.add_patch(Rectangle((x, y), w, h, facecolor=fc, edgecolor="none", alpha=0.35, zorder=0))
    ax.text(x + 0.12, y + h - 0.22, title, fontsize=8, fontweight="bold", color="#333333", va="top")


def _arrow(ax, x1, y1, x2, y2):
    ax.add_patch(
        FancyArrowPatch((x1, y1), (x2, y2), arrowstyle="-|>", mutation_scale=11, linewidth=1.05, color="#444444")
    )


def diagram_rosati(suffix: str = ""):
    fig, ax = plt.subplots(figsize=(12.5, 16.2))
    ax.set_xlim(0, 12.5)
    ax.set_ylim(0, 16.2)
    ax.axis("off")
    ax.set_title(
        "Diagram 1 — Rosati et al. (2024) bioinformatics / DGE pipeline",
        fontsize=13.5, fontweight="bold", pad=10,
    )

    _phase(ax, 0.3, 14.55, 11.9, 1.45, "DATA", "#D6EAF8")
    _box(ax, 3.6, 14.7, 5.3, 1.05, "Omics / gene-expression data", fc="#D6EAF8", fontsize=10, fw="bold")

    _phase(ax, 0.3, 12.55, 11.9, 1.85, "1. DATA PREPARATION", "#E8F8F5")
    _box(ax, 0.7, 12.7, 3.4, 1.35, "Quality control\nFiltering", fc="#D5F5E3")
    _box(ax, 4.55, 12.7, 3.4, 1.35, "Normalization\n(method depends on data type)", fc="#D5F5E3")
    _box(ax, 8.4, 12.7, 3.4, 1.35, "Comparable samples\nfor DGE", fc="#D5F5E3")
    _arrow(ax, 4.1, 13.37, 4.55, 13.37)
    _arrow(ax, 7.95, 13.37, 8.4, 13.37)

    _phase(ax, 0.3, 8.15, 11.9, 4.2, "2. DIFFERENTIAL GENE EXPRESSION  (alternatives, not all used together)", "#FCF3CF")
    _box(ax, 0.55, 9.55, 5.5, 2.55, "", fc="#FDEBD0", ec="#B9770E")
    ax.text(3.3, 11.85, "RNA-seq counts", ha="center", fontsize=9, fontweight="bold")
    _box(ax, 0.75, 10.55, 2.45, 1.05, "edgeR\nTMM + Neg. Binomial", fc="#F6DDCC", ec="#D35400", fontsize=7.6)
    _box(ax, 3.35, 10.55, 2.45, 1.05, "DESeq2\nsize factors + Neg. Binomial", fc="#F6DDCC", ec="#D35400", fontsize=7.4)
    _box(ax, 0.75, 9.7, 2.45, 0.7, "NOISeq  (noise)", fc="#F6DDCC", ec="#D35400", fontsize=7.6)
    _box(ax, 3.35, 9.7, 2.45, 0.7, "SAMseq  (rank)", fc="#F6DDCC", ec="#D35400", fontsize=7.6)
    _box(ax, 6.55, 9.55, 5.3, 2.55, "Microarray / normalised expression\n\nlimma\nlinear models + empirical Bayes\n\nThis exam (GSE75214) uses limma only", fc="#D5F5E3", ec="#1E8449", fontsize=8.2)

    _phase(ax, 0.3, 6.35, 11.9, 1.65, "3. DEG SELECTION", "#FADBD8")
    _box(ax, 0.7, 6.5, 2.6, 1.2, "logFC\nUp / Down", fc="#F5B7B1")
    _box(ax, 3.7, 6.5, 2.6, 1.2, "p-value", fc="#F5B7B1")
    _box(ax, 6.7, 6.5, 2.6, 1.2, "FDR / adj. p", fc="#F5B7B1")
    _box(ax, 9.7, 6.5, 2.15, 1.2, "Candidate\nDEGs", fc="#F1948A", fontsize=8.5, fw="bold")
    _arrow(ax, 3.3, 7.1, 3.7, 7.1)
    _arrow(ax, 6.3, 7.1, 6.7, 7.1)
    _arrow(ax, 9.3, 7.1, 9.7, 7.1)

    _phase(ax, 0.3, 4.35, 11.9, 1.8, "4. FUNCTIONAL ANALYSIS", "#E8DAEF")
    _box(ax, 0.7, 4.5, 2.6, 1.3, "GO", fc="#D2B4DE")
    _box(ax, 3.55, 4.5, 2.6, 1.3, "KEGG / pathways", fc="#D2B4DE")
    _box(ax, 6.4, 4.5, 2.6, 1.3, "GSEA / ORA", fc="#D2B4DE")
    _box(ax, 9.25, 4.5, 2.6, 1.3, "PPI when relevant", fc="#D2B4DE")

    _phase(ax, 0.3, 2.35, 11.9, 1.8, "5. BIOMARKER EVALUATION", "#D6EAF8")
    _box(ax, 1.5, 2.5, 4.4, 1.3, "ROC curve\nSensitivity vs false-positive rate", fc="#AED6F1")
    _box(ax, 6.6, 2.5, 4.4, 1.3, "AUC\nDiscrimination in the evaluated data", fc="#AED6F1")
    _arrow(ax, 5.9, 3.15, 6.6, 3.15)

    _phase(ax, 0.3, 0.25, 11.9, 1.9, "6. CLINICAL VALIDATION  (needs follow-up time; not available in GSE75214)", "#F5CBA7")
    _box(ax, 0.7, 0.4, 3.5, 1.4, "Survival analysis\nKaplan–Meier", fc="#EDBB99")
    _box(ax, 4.5, 0.4, 3.5, 1.4, "Hazard ratio", fc="#EDBB99")
    _box(ax, 8.3, 0.4, 3.5, 1.4, "Candidate biomarkers\n(need independent data)", fc="#F9E79F", fw="bold")

    ax.text(
        6.25, 15.95,
        "RNA-seq tools are Rosati context. Negative Binomial is a count model, not a two-class outcome.",
        ha="center", fontsize=8, style="italic", color="#555555",
    )
    out = FIG / f"diagram1_rosati_dge_pipeline{suffix}.png"
    fig.tight_layout()
    fig.savefig(out, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {out}")


def diagram_midterm1(suffix: str = ""):
    fig, ax = plt.subplots(figsize=(12.5, 14.8))
    ax.set_xlim(0, 12.5)
    ax.set_ylim(0, 14.8)
    ax.axis("off")
    ax.set_title(
        "Diagram 2 — Midterm 1 pipeline (Leo Sanchez / syed_pipeline)",
        fontsize=13.5, fontweight="bold", pad=10,
    )

    _phase(ax, 0.3, 13.15, 11.9, 1.4, "DATA", "#D6EAF8")
    _box(ax, 3.3, 13.3, 5.9, 1.0, "Breast-cancer expression data  (graded Track B)", fc="#D6EAF8", fontsize=10, fw="bold")

    _phase(ax, 0.3, 11.35, 11.9, 1.6, "PREPROCESSING", "#E8F8F5")
    _box(ax, 1.2, 11.5, 4.7, 1.15, "Quality filters / scaling as in syed_pipeline", fc="#D5F5E3")
    _box(ax, 6.6, 11.5, 4.7, 1.15, "DGE / statistical reduction (FDR)", fc="#D5F5E3")
    _arrow(ax, 5.9, 12.07, 6.6, 12.07)

    _phase(ax, 0.3, 8.35, 11.9, 2.8, "FEATURE SELECTION  (four methods, then a vote)", "#F5EEF8")
    _box(ax, 0.6, 9.55, 2.7, 1.3, "MI\nMutual information", fc="#D7BDE2")
    _box(ax, 3.5, 9.55, 2.7, 1.3, "RFECV\nRFE + CV", fc="#D7BDE2")
    _box(ax, 6.4, 9.55, 2.7, 1.3, "Elastic Net\nL1 + L2", fc="#D7BDE2")
    _box(ax, 9.3, 9.55, 2.7, 1.3, "GBC\nTree importance", fc="#D7BDE2")
    _box(ax, 3.2, 8.5, 6.1, 0.85, "Consensus voting  →  8-gene Table-3 panel (official Midterm 1)", fc="#C39BD3", fw="bold", fontsize=8.4)

    _phase(ax, 0.3, 6.15, 11.9, 1.95, "TRAINING STRATEGY", "#FDEBD0")
    _box(ax, 0.8, 6.3, 5.2, 1.5, "Stratified train / test\nBalance on TRAIN only\n(MT1 breast cancer: RUS)", fc="#F5CBA7", fontsize=8.4)
    _box(ax, 6.5, 6.3, 5.2, 1.5, "Classifiers + CV / LOOCV\nas implemented in Midterm 1", fc="#F5CBA7", fontsize=8.4)
    _arrow(ax, 6.0, 7.05, 6.5, 7.05)

    _phase(ax, 0.3, 3.85, 11.9, 2.05, "EVALUATION", "#D5F5E3")
    _box(ax, 0.8, 4.05, 5.2, 1.55, "ROC / AUC\nFP, FN, sensitivity, specificity", fc="#ABEBC6", fontsize=8.6)
    _box(ax, 6.5, 4.05, 5.2, 1.55, "External comparison in Task 2.1\nIBD GSE75214  (0 / 8 overlap)", fc="#ABEBC6", fontsize=8.4)
    _arrow(ax, 6.0, 4.82, 6.5, 4.82)

    _phase(ax, 0.3, 1.55, 11.9, 2.05, "OUTPUT", "#F9E79F")
    _box(ax, 2.6, 1.75, 7.3, 1.55, "Official 8-gene breast-cancer panel\nAC083967.1, AC108477.1, AC112777.1, AL356275.1,\nARHGAP16P, CST4, SLC6A2, TRPM3", fc="#F7DC6F", fontsize=8.2, fw="bold")

    ax.text(
        6.25, 0.55,
        "DGE asks which genes change. ML asks which genes help predict. PCA is exploratory only. VAE not used.",
        ha="center", fontsize=8, style="italic", color="#555555",
    )
    ax.text(
        6.25, 0.22,
        "This diagram is the graded Midterm 1 pipeline. It is not Chris's pipeline.",
        ha="center", fontsize=8, style="italic", color="#555555",
    )
    out = FIG / f"diagram2_midterm1_ml_pipeline{suffix}.png"
    fig.tight_layout()
    fig.savefig(out, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {out}")


def diagram_tmm(suffix: str = ""):
    fig, ax = plt.subplots(figsize=(10.5, 9.2))
    ax.set_xlim(0, 10.5)
    ax.set_ylim(0, 9.2)
    ax.axis("off")
    ax.set_title("TMM (edgeR) — conceptual sketch, not used on GSE75214", fontsize=12.5, fontweight="bold")
    _box(ax, 0.5, 7.4, 4.2, 1.2, "Sample A\n10 million reads", fc="#D6EAF8", fontsize=10)
    _box(ax, 5.8, 7.4, 4.2, 1.2, "Sample B\n20 million reads", fc="#D6EAF8", fontsize=10)
    _arrow(ax, 2.6, 7.4, 2.6, 6.45)
    _arrow(ax, 7.9, 7.4, 7.9, 6.45)
    _box(ax, 2.0, 5.15, 6.5, 1.25, "Raw counts are not directly comparable\n(library size and composition differ)", fc="#FDEBD0", fontsize=9)
    _arrow(ax, 5.25, 5.15, 5.25, 4.3)
    _box(ax, 1.7, 2.85, 7.1, 1.4, "Remove extreme M/A genes, then estimate scaling\nTMM = Trimmed Mean of M-values", fc="#D5F5E3", fontsize=9)
    _arrow(ax, 5.25, 2.85, 5.25, 2.05)
    _box(ax, 1.7, 0.55, 7.1, 1.4, "TMM factors  →  more comparable library sizes  →  DGE", fc="#ABEBC6", fontsize=9, fw="bold")
    out = FIG / f"diagram_tmm_normalization{suffix}.png"
    fig.tight_layout()
    fig.savefig(out, dpi=180, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {out}")


if __name__ == "__main__":
    for src, bak in [
        ("diagram1_rosati_dge_pipeline.png", "diagram1_rosati_dge_pipeline_v1.png"),
        ("diagram2_midterm1_ml_pipeline.png", "diagram2_midterm1_ml_pipeline_v1.png"),
        ("diagram_tmm_normalization.png", "diagram_tmm_normalization_v1.png"),
    ]:
        a, b = FIG / src, FIG / bak
        if a.exists() and not b.exists():
            shutil.copy2(a, b)
            print(f"Kept original as {b.name}")
    diagram_rosati("_v2")
    diagram_midterm1("_v2")
    diagram_tmm("_v2")
    diagram_rosati("")
    diagram_midterm1("")
    diagram_tmm("")
    extra = FIG / "diagram_supplement_tmm.png"
    shutil.copy2(FIG / "diagram_tmm_normalization.png", extra)
    print(f"Saved {extra}")
