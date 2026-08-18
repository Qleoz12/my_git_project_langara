# DANA 4830 Final Exam — Leo Sanchez

**Student:** Leo Sanchez  
**Course:** DANA 4830 — Summer 2026  
**Seed:** `RNG=42` in all notebooks

Biomarker identification comparing the classical bioinformatics pipeline (Rosati et al.
2024) with the machine learning pipeline from Midterm 1 (`syed_pipeline`), on inflammatory
bowel disease (GSE75214) and on four merged cervical cancer datasets (Wang et al. 2022).

This submission is **Python only**. No R packages are required.

Every number in the notebooks is produced by running the code. No value is typed by hand.

## What to submit

**Main deliverable:** `04_Analysis/2_0_final_overview.pdf` — export of
`03_Codes/2_0_final_overview.ipynb` (master report). The other five notebooks are supporting detail.

Regenerate only the submission PDF (after CSVs exist):

```bash
cd 03_Codes
python run_all.py --overview-pdf
```

Fallback if `webpdf` fails: open `04_Analysis/2_0_final_overview.html` → Print → Save as PDF.

Word (`DANA4830_Final_Report.docx`) is optional: `python run_all.py --with-docx`.

## Folder structure

```
Final_exam_my_version/
  01_Data/            input data, GEO cache, Midterm 1 exports
  02_Question/        exam question PDF
  03_Codes/           notebooks and shared .py libraries
  04_Analysis/        figures, result tables, HTML, notes, glossary
  05_References/      Rosati 2024, Syed 2024, Wang 2022, packaged Midterm 1
  requirements.txt
  README.md
  Task_1.1.docx
  DANA4830_Final_Report.docx
```

## How to reproduce

1. `pip install -r requirements.txt`
2. From `03_Codes/`:

   ```
   python run_all.py
   ```

   Use `--clean` only when you intend to delete generated CSVs/HTML/figures and re-run everything.

   Use `--with-docx` to also build `DANA4830_Final_Report.docx` (optional).

   Primary deliverable: `04_Analysis/2_0_final_overview.pdf` (or HTML → Print to PDF if webpdf fails).

   Notebook order: `2_1_01` → `2_1_02` → `2_1_03` → `2_2_01` → `2_2_02` → `2_0_final_overview`.

3. `python sync_notebook_numbers.py` must exit 0 (no leftover Chris-template numbers).

## `01_Data/` inventory

| File / folder | Description |
|---------------|-------------|
| `gse75214_clean.csv` | GSE75214 expression (Task 2.1) |
| `gpl6244_probe_to_symbol.csv` | Probe map for GSE75214 |
| `midterm1_biomarkers.csv` | Graded 8-gene Table-3 panel (FDR = BH q-value) |
| `midterm1_biomarkers_PROVENANCE.md` | Do not re-run Midterm FS |
| `midterm1_deg_table.csv` | Midterm 1 Track B DEG table |
| `GPL570/96/10558_probe_to_symbol.csv` | Cervical platform maps |
| `geo_raw/` | Cached GEO series matrices |
| `enrichr_*.csv` | Enrichr cache |
| `cervical_master.csv` | Task 2.2 merge output |

## Papers (`05_References/`)

- `Rosati_etal_2024.pdf`
- `Syed_etal_2024.pdf`
- `Wang_etal_2022.pdf` (and `Wang_2021.pdf` if present from the shared pack)
- `DANA4830_MT1_Leo_Sanchez/` — graded Midterm 1 notebook + `syed_pipeline`

Exam PDF: `02_Question/Final_Exam.pdf`.

## Shared libraries

| File | Purpose |
|------|---------|
| `task2_1_lib.py` | probe collapse, limma-style DGE, skim_summary |
| `task2_2_lib.py` | GEO parse, merge, study-wise centering |
| `welm_models.py` | WELM / BWELM classifiers |
| `make_diagrams.py` | Task 1.2 + TMM sketch |
| `export_mt1_data.py` | join graded 8-gene panel to DEG FDR |
| `build_report_data.py` | `report_data.json` |
| `build_final_report.py` | Word report + Task 1.1 docx |
| `run_all.py` | end-to-end |
| `sync_notebook_numbers.py` | fail on obsolete Chris numbers |

## R → Python mapping

| Classical R tool | Python equivalent used here |
|------------------|-----------------------------|
| limma (eBayes) | `task2_1_lib.limma_two_group` |
| GEOquery | `urllib` + `task2_2_lib.read_series_matrix` |
| annotation packages | `*_probe_to_symbol.csv` |
| enrichR / clusterProfiler | Enrichr API + CSV cache |
| sva ComBat | **not used** — per-dataset gene centering only |
| pROC | `sklearn.metrics.roc_auc_score` |
| edgeR / DESeq2 / NOISeq / SAMseq | **not executed** (RNA-seq; see `dge_methods_notes.md`) |

limma (not DESeq2/edgeR) is correct because GSE75214 and the cervical series are **microarrays**.

## Official numbers (`04_Analysis/report_data.json`)

Re-run `build_report_data.py` after notebooks. At plan freeze these were:

| Analysis | Metric | Value |
|----------|--------|-------|
| GSE75214 | DEGs (FDR<0.05, \|log2FC\|≥1.0) | 535 (323 up, 212 down) |
| GSE75214 | DEGs at \|log2FC\|≥1.5 | 158 |
| GSE75214 | ML panel | 35 genes (train-only SMOTE for feature selection) |
| GSE75214 | MT1 panel | 8 genes, 0 overlap with ML, 3 present on platform |
| Cervical | 179 samples, 12,191 genes | 381 DEGs; Wang 10/10 |

## Methodological notes

- **Survival:** GSE75214 has no follow-up; Kaplan–Meier / Cox not run.
- **SMOTE:** training split / inside CV folds only; Midterm 1 breast cancer used RUS.
- **VAE:** not used (optional). PCA is exploratory visualization only.
- **Wang:** per-study DEG intersection in the paper vs sample merge here; 10/10 = benchmark.
- **Batch:** study-wise centering ≠ ComBat.
- **AUC:** ML panel CV AUC is optimistic (same cohort as feature selection).
- **0/8 overlap:** limited cross-disease transfer, not a failed Midterm 1.

## Written work and defense

- Task 1.1: `04_Analysis/task1_1_comparison.md` and `Task_1.1.docx`
- Study notes (not the answer): `04_Analysis/task1_1_comparison_STUDY_NOTES.md`
- DGE methods: `04_Analysis/dge_methods_notes.md`
- Glossary: `04_Analysis/glossary.md`
- Oral checklist: `04_Analysis/DEFENSE_CHECKLIST.md`

## Figures (`04_Analysis/figures/`)

- `diagram1_rosati_dge_pipeline.png`
- `diagram2_midterm1_ml_pipeline.png`
- `diagram_tmm_normalization.png`
- volcano / ROC / enrichment / merge PCA from the task notebooks
