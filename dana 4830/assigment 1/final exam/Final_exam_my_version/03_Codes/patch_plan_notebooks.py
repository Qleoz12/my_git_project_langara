"""Insert plan-stage markdown and SMOTE train-only note into exam notebooks."""
from __future__ import annotations

import json
from pathlib import Path

CODES = Path(__file__).resolve().parent


def load(name: str) -> dict:
    return json.loads((CODES / name).read_text(encoding="utf-8"))


def save(name: str, nb: dict) -> None:
    (CODES / name).write_text(json.dumps(nb, indent=1, ensure_ascii=False) + "\n", encoding="utf-8")


def md_cell(text: str) -> dict:
    src = text.strip() + "\n"
    lines = [ln + "\n" for ln in src.split("\n")]
    if lines:
        lines[-1] = lines[-1].rstrip("\n")
        if not lines[-1].endswith("\n"):
            # jupyter often stores last line without newline or with
            pass
    # store as list of lines with newlines except possibly last
    parts = src.splitlines(keepends=True)
    if parts and not parts[-1].endswith("\n"):
        parts[-1] += "\n"
    return {"cell_type": "markdown", "metadata": {}, "source": parts}


def insert_after(nb: dict, index: int, cell: dict) -> None:
    nb["cells"].insert(index + 1, cell)


def replace_source_containing(nb: dict, needle: str, new_src: str) -> bool:
    for cell in nb["cells"]:
        if cell.get("cell_type") != "code":
            continue
        text = "".join(cell.get("source", []))
        if needle in text:
            parts = new_src.splitlines(keepends=True)
            if parts and not parts[-1].endswith("\n"):
                parts[-1] += "\n"
            cell["source"] = parts
            return True
    return False


def src_join(nb, i):
    return "".join(nb["cells"][i].get("source", []))


def main() -> None:
    # --- 2_1_01 ---
    nb = load("2_1_01_data_and_dge.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**DGE method choice (Rosati context, Task 2.1 execution).** Rosati et al. review several DGE strategies: edgeR (TMM + negative binomial), DESeq2 (median-of-ratios + negative binomial), NOISeq, SAMseq, and limma. Those RNA-seq tools are **not** run here. GSE75214 is a processed microarray of log2 intensities, so the executed method is **limma** (empirical Bayes moderated *t*, Smyth 2004) in `task2_1_lib.limma_two_group`. See `04_Analysis/dge_methods_notes.md` and the TMM sketch `diagram_tmm_normalization.png` (TMM is for RNA-seq library sizes, not this matrix)."""
        ),
    )
    save("2_1_01_data_and_dge.ipynb", nb)

    # --- 2_1_02 ---
    nb = load("2_1_02_feature_selection.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**DGE vs ML (exam message).** Differential expression reduces the *biological* search space under FDR. Machine learning (MI, RFECV, Elastic Net, GBC, then consensus) then reduces the *predictive* feature space. High-dimensional genes → DEG filter (535 genes from Notebook 1) → ML selection → compact panel.

**SMOTE rule.** SMOTE must be applied only to **training** data, never to the full matrix before a train/test split. The cell below first holds out a stratified 20% test set (never SMOTEd, never used for feature selection). SMOTE then balances the training fold only (172 vs 22 → balanced train). Class weights remain on the models. Midterm 1 Track B used RUS on breast cancer; here SMOTE is preferred because IBD is 172:22 and we do not want to discard disease samples.

**Limitation if SMOTE were fit on all 194 rows before selection:** the synthetic minority points would leak into feature ranking. Evaluation in Notebook 3 already places SMOTE *inside* each CV fold (`ImbPipeline`)."""
        ),
    )
    ok = replace_source_containing(
        nb,
        "Xb, yb = sm.fit_resample(X.values, y)",
        """# SMOTE on the training split only (never before the split, never on the held-out test).
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(
    X.values, y, test_size=0.20, stratify=y, random_state=RNG
)
sm = SMOTE(random_state=RNG, k_neighbors=5)
Xb, yb = sm.fit_resample(X_train, y_train)
print("Full data:", dict(Counter(y)))
print("Train before SMOTE:", dict(Counter(y_train)))
print("Train after SMOTE :", dict(Counter(yb)))
print("Held-out test (no SMOTE, not used for FS):", dict(Counter(y_test)))
Xs = StandardScaler().fit_transform(Xb)
TOPN = 50
""",
    )
    if not ok:
        raise SystemExit("failed to patch SMOTE cell in 2_1_02")
    save("2_1_02_feature_selection.ipynb", nb)

    # --- 2_1_03 ---
    nb = load("2_1_03_evaluation_and_comparison.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**Clinical evaluation (confusion matrix).** At a chosen threshold:

|  | Actual disease | Actual no disease |
|--|----------------|-------------------|
| Predicted + | TP | FP |
| Predicted − | FN | TN |

- **False positive (FP):** the model calls a patient (or gene panel call) positive when they are not. Cost: extra tests, anxiety, possible unnecessary treatment.
- **False negative (FN):** a true positive is missed. Cost: delayed diagnosis.

Sensitivity = TP / (TP + FN). Specificity = TN / (TN + FP). Lowering FN raises sensitivity; lowering FP raises specificity. You cannot freely minimize both: the threshold is a trade-off. That is why we use the **ROC curve and AUC**, which summarize performance across thresholds rather than a single cut.

**FDR vs FP/FN.** During DGE we test ~20,000 genes. Many nominal *p* < 0.05 arise by chance, so we control **FDR** (Benjamini–Hochberg adjusted *p*) to limit false *discoveries* in the gene list. During classification we report FP, FN, sensitivity, specificity, ROC, and AUC for a *predictor*. Related ideas, not the same quantity.

**Optimistic AUC.** The ML panel AUC from CV on the same GSE75214 matrix used for feature selection is an in-sample / nested-but-still-same-cohort estimate, not a clinical diagnostic claim.

**Midterm 1 panel.** None of the eight graded breast-cancer Table-3 genes were recovered as IBD DEGs. That is limited **transferability across diseases**, not evidence that Midterm 1 was wrong."""
        ),
    )
    save("2_1_03_evaluation_and_comparison.ipynb", nb)

    # --- 2_2_01 ---
    nb = load("2_2_01_merge_datasets.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**Batch handling.** Per-dataset gene centering reduces study-specific baseline shifts so PCA no longer separates platforms as strongly. This is **not** equivalent to a full batch-effect model such as **ComBat** (`sva`). Do not say “we ran ComBat” unless ComBat was actually fit.

**Wang vs this merge.** Wang et al. (2022) ran DGE **per study** and intersected DEG lists. This exam **merges samples first**, then runs DGE on the master matrix. Recovering 10/10 published hub genes is an **external benchmark / concordance**, not an exact replication of Wang’s workflow."""
        ),
    )
    save("2_2_01_merge_datasets.ipynb", nb)

    # --- 2_2_02 ---
    nb = load("2_2_02_validate_pipelines.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**Cutoff.** A gene is called DEG at |log2FC| ≥ 1.5 and BH FDR < 0.05, as stated in the exam.

**Wang 10/10.** All ten hub genes (CDK1, CDC20, AURKA, TOP2A, ASPM, NCAPG, KIF23, CENPF, KIF20A, PRC1) matching as DEGs is concordance with the literature, not a claim that we reproduced Wang’s per-study intersection pipeline.

**Centering ≠ ComBat.** The master matrix was study-centered in Notebook 1."""
        ),
    )
    save("2_2_02_validate_pipelines.ipynb", nb)

    # --- 2_0 ---
    nb = load("2_0_final_overview.ipynb")
    insert_after(
        nb,
        0,
        md_cell(
            """**Task 1.1 (written).** The full ~300-word comparison is in `04_Analysis/task1_1_comparison.md`. Short version: Rosati asks which genes are differentially expressed (limma here; edgeR/DESeq2/NOISeq/SAMseq are RNA-seq alternatives). Syed/Midterm 1 asks which subset predicts class (MI, RFECV, EN, GBC). They combine: DGE filters biology under FDR; ML filters predictive features. Evidence: 535 IBD DEGs; 0 of 8 Midterm 1 breast-cancer genes overlap that set (limited transferability).

**Dimensionality reduction.** PCA in Task 2.2 is **exploratory** (batch / class visualization), not the feature-selection engine. A VAE is **not** used: encoder/decoder latent axes are not gene names, so biomarker interpretability would drop (latent feature 17 ≠ AURKA). VAE remains optional extra work, not part of this submission.

**Glossary and defense.** See `04_Analysis/glossary.md` and `04_Analysis/DEFENSE_CHECKLIST.md`."""
        ),
    )
    save("2_0_final_overview.ipynb", nb)
    print("Patched six notebooks.")


if __name__ == "__main__":
    main()
