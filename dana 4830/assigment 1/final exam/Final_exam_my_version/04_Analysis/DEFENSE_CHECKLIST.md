# Oral defense checklist

All oral-defense questions are consolidated in `03_Codes/2_0_final_overview.ipynb` (section **Oral defense**). Use this file as a quick index.

## DGE

- Why limma instead of DESeq2? Microarray log2 intensities, not raw counts.
- What is TMM? Robust library-size scaling for RNA-seq (not used on GSE75214).
- TMM vs DESeq2 normalization? Trimmed M-values vs median-of-ratios.
- Why control FDR? Thousands of simultaneous tests; nominal p < 0.05 is not enough.

## ML

- Why feature selection? DGE shrinks the biological list; ML shrinks the predictive set.
- Why SMOTE? 172 vs 22; otherwise the classifier favours the majority class.
- Where must SMOTE be applied? Training data only, never before the split / inside CV folds.
- Why can AUC ≈ 0.99 be optimistic? Feature selection and evaluation on the same cohort.

## Clinical / stats

- FP vs FN? False alarm vs missed disease.
- Sensitivity vs specificity? Catch true disease vs rule out non-disease.
- Why not minimize both freely? Threshold trade-off; ROC studies that trade-off.
- What does an ROC threshold change? Moves FP/FN (sensitivity vs specificity).

## Dimensionality

- Why no VAE? Optional; extra hyperparameters; latent axes are not genes.
- PCA vs VAE? PCA is linear, interpretable loadings; VAE is nonlinear latent space.
- Why can VAE hurt biomarker interpretability? “Latent 17” is not AURKA.

## Biology

- Why 0/8 Midterm genes overlap IBD DEGs? Disease-specific signature (breast ≠ IBD).
- Does 0 overlap mean Midterm was wrong? No: limited transferability.
- Why isn’t AUC ≈ 0.99 a clinical biomarker? In-sample / CV optimism; no independent clinical validation.

## Task 2.2

- What is a batch effect? Study/platform shift unrelated to biology.
- Did you run ComBat? No: per-study gene centering, which is weaker than ComBat.
- Why is 10/10 Wang not an exact replication? Wang intersected per-study DEGs; we merge samples first.
