# Task 1.1 — Study Notes (not the final written answer)

Use these bullets to write your own ~300-word comparison in your own words.

## Rosati et al. (2024) — statistics-first pipeline

- Starts from normalized omics data (microarray or RNA-seq counts).
- Core step is differential expression with limma, DESeq2, or edgeR depending on data type.
- Genes are ranked by moderated t-statistics, fold change, and BH-FDR.
- Biology is interpreted through pathway enrichment (KEGG, GO, Reactome).
- Biomarker quality is checked with ROC/AUC and, when survival data exist, Kaplan–Meier / Cox models.
- Strength: transparent, literature-aligned gene lists; each DEG has a clear statistical story.
- Limitation: many significant genes; no built-in classifier or class-balance handling.

## Syed et al. / Midterm 1 ML pipeline (syed_pipeline)

- Preprocess (log2, filter, scale) then univariate DEG screening.
- Four feature-selection methods: MI, RFECV, Elastic Net, GBC.
- HFCP consensus (votes ≥ 2) yields a compact panel (8 genes for breast cancer).
- Class imbalance handled with SMOTE (IBD track) or RUS (breast cancer track) plus class weights.
- Classifiers evaluated with LOOCV / stratified CV; metrics include accuracy, recall, F2, AUC.
- External validation on an independent cohort when available.
- Strength: strong predictive performance; small panels suitable for a classifier.
- Limitation: harder to interpret; risk of overfitting if FS and evaluation leak information.

## Trade-offs to mention

| Aspect | Rosati / limma | Syed / ML |
|---|---|---|
| Goal | Find significant genes | Build a predictive panel |
| Feature count | Hundreds of DEGs | Few consensus genes |
| Interpretability | High (per-gene stats) | Lower (model-driven) |
| Class imbalance | Not addressed | SMOTE + class weights |
| Transfer across diseases | Biology may not transfer | Panel trained on one disease |

## Your Task 2.1 evidence (GSE75214, IBD)

Numbers from `panel_comparison_2_1.csv` and notebook outputs:

- ~535 DEGs at FDR < 0.05 and |log2FC| ≥ 1.0; top hits: SLC6A14, DUOX2, LCN2, MMP3, S100A8.
- Single-gene ROC AUC > 0.91 for top inflammation genes.
- ML panel (35 genes after train-only SMOTE) and the statistics panel are compared with ROC/AUC; evaluation SMOTE sits inside CV folds.
- Midterm 1 breast-cancer panel (8 genes: CST4, TRPM3, SLC6A2, etc.) does **not** transfer to IBD:
  - 0 genes overlap between ML panel and MT1 panel.
  - 3 of 8 MT1 genes are present on the GSE75214 platform; none are strong IBD DEGs.
- Conclusion: disease-specific biology matters more than classifier complexity for cross-disease transfer.

## Diagram references (Task 1.2)

- Diagram 1: `04_Analysis/figures/diagram1_rosati_dge_pipeline.png`
- Diagram 2: `04_Analysis/figures/diagram2_midterm1_ml_pipeline.png` (your syed_pipeline steps)
