# DGE methods — Task 1 / oral defense notes

These methods are **context** from Rosati et al. (2024). They are **not** all run
on GSE75214. Task 2.1 uses **limma** only.

| Method | Normalization / model | Data type |
|--------|------------------------|-----------|
| edgeR | TMM + Negative Binomial | RNA-seq counts |
| DESeq2 | Median-of-ratios + Negative Binomial | RNA-seq counts |
| NOISeq | noise / non-parametric | RNA-seq |
| SAMseq | rank / resampling | RNA-seq |
| limma | linear models / empirical Bayes (Smyth 2004) | microarray (and voom RNA-seq) |

**Defense sentence.** Rosati discusses multiple DGE strategies, but GSE75214 is
analyzed with limma because the available data are microarray expression values
rather than raw RNA-seq counts. Replacing limma with DESeq2 or edgeR “to use more
methods” would be methodologically incorrect on this matrix.

**TMM (edgeR), one sentence.** TMM (Trimmed Mean of M-values) does not simply
divide every gene by the total number of reads. It estimates a robust scaling
factor after excluding genes with extreme M/A values, then uses effective library
sizes so samples with different depths are comparable.

**DESeq2 vs TMM.** DESeq2 uses the median of gene-wise ratios to a geometric-mean
pseudo-reference sample. Both correct for library size; they are different
estimators. Neither applies to the already-normalized log2 microarray matrix used
here.

See `04_Analysis/figures/diagram_tmm_normalization.png` for a visual sketch.
