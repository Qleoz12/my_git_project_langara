# Task 1.1 — Comparison of biomarker approaches (~300 words)

Rosati et al. (2024) review a statistics-first bioinformatics pipeline: quality
control, normalization, differential expression, multiple-testing control (FDR),
pathway enrichment, then ROC/AUC and, when follow-up exists, survival models. The
scientific question is *which genes are differentially expressed* and what biology
they point to. Depending on the assay, that DGE step may use edgeR (TMM + negative
binomial), DESeq2 (median-of-ratios + negative binomial), NOISeq, SAMseq, or limma
(linear models with empirical Bayes). GSE75214 is a microarray of log2 intensities,
not raw RNA-seq counts, so limma is the appropriate tool here; swapping in DESeq2
or edgeR would be methodologically wrong for this matrix.

Syed et al. (2024), which Midterm 1 replicated as `syed_pipeline`, ask a different
question: *which small subset best predicts class*? After DEG filtering, four
feature-selection methods (mutual information, RFECV, elastic net, gradient boosting)
are combined into a compact panel, class imbalance is treated (SMOTE or RUS plus
weights), and classifiers are scored with CV metrics. Linear PCA can visualize
samples; a VAE is a possible nonlinear reducer but is not used here, because latent
dimensions are no longer named genes and would weaken biomarker interpretability.

The two pipelines complement each other rather than compete. DGE reduces the
biological search space under FDR control and points to pathways; machine learning
then shrinks the predictive feature space to a compact panel. On GSE75214 this exam
found 535 IBD DEGs (FDR < 0.05, |log2FC| ≥ 1.0) and a **16-gene ML consensus panel**
after train-only SMOTE. Because expression values are numeric and the LD1 teaching
plot on that panel shows disease and control samples largely separated, LDA was a
natural linear baseline: shrinkage LDA on the ML panel reached **mean CV ROC-AUC
0.9948** (SMOTE inside each fold). That supports the idea that the selected genes
carry a strong linear signal in this cohort, while logistic regression and random
forest give similar high AUC as extra checks.

None of the eight Midterm 1 breast-cancer Table-3 biomarkers overlapped that IBD
DEG set (three of eight are present on the GSE75214 platform). That does not
invalidate the midterm model; it shows limited cross-disease transfer. Overall, DGE
is useful for statistically stable, biologically interpretable signals; ML is useful
for compact prediction panels. Using both gives a better balance between biological
meaning and predictive performance within the disease studied.
