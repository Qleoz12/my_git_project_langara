# Midterm 1 biomarker panel — provenance

The eight genes in `midterm1_biomarkers.csv` are the **Table-3 master panel**
(`table3_master_bc`) from the **graded** Midterm 1 notebook:

`05_References/DANA4830_MT1_Leo_Sanchez/DANA4830_MT1.ipynb`

and the original submission in `midtermn/03_Answer/DANA4830_MT1.ipynb`.

They are **not** re-derived for the final exam. Mutual Information, RFECV, Elastic
Net, and Gradient Boosting contain stochastic steps; re-running feature selection
would change the list and would no longer match the graded work.

## Official 8-gene Table-3 panel (Track B, breast cancer)

| Gene | Role in Table 3 |
|------|-----------------|
| CST4 | Mutual Information, up |
| SLC6A2 | Mutual Information, down |
| ARHGAP16P | RFECV, up |
| AC108477.1 | RFECV, down |
| AC112777.1 | Elastic Net, up |
| AL356275.1 | Elastic Net, down |
| AC083967.1 | Gradient Boosting, up |
| TRPM3 | Gradient Boosting, down |

`log2FC` and `FDR` in the CSV are joined from `midterm1_deg_table.csv` (BH q-values
from the Midterm 1 Track B DEG table). FDR is **not** a placeholder of 1.0.

A larger 11-gene “votes ≥ 2” screened list exists in the Midterm 1 notebook. That
list is **not** the comparison panel for Task 2.1. Task 2.1 compares against these
eight Table-3 genes only.

Zero overlap with GSE75214 IBD DEGs does **not** mean the Midterm 1 model was wrong.
It means a breast-cancer signature is disease-specific and does not transfer to IBD.
