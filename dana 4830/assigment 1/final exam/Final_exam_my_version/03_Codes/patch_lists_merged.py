"""Patch 2_1_01 and 2_1_02 with A2-style lists, clusters, merged/prune, membership map."""
from __future__ import annotations

import json
from pathlib import Path

CODES = Path(__file__).resolve().parent


def load(name: str) -> dict:
    return json.loads((CODES / name).read_text(encoding="utf-8"))


def save(name: str, nb: dict) -> None:
    (CODES / name).write_text(json.dumps(nb, indent=1, ensure_ascii=False) + "\n", encoding="utf-8")


def lines(text: str) -> list[str]:
    parts = text.strip("\n") + "\n"
    out = parts.splitlines(keepends=True)
    return out


def md(text: str) -> dict:
    return {"cell_type": "markdown", "metadata": {}, "source": lines(text)}


def code(text: str) -> dict:
    return {"cell_type": "code", "metadata": {}, "source": lines(text), "outputs": [], "execution_count": None}


def set_src(nb, i, text, kind="code"):
    nb["cells"][i]["source"] = lines(text)
    if kind == "code":
        nb["cells"][i]["outputs"] = []
        nb["cells"][i]["execution_count"] = None


def insert(nb, i, cell):
    nb["cells"].insert(i, cell)


def patch_01():
    nb = load("2_1_01_data_and_dge.ipynb")
    src3 = "".join(nb["cells"][3]["source"])
    if "from IPython.display import display" not in src3:
        src3 = src3.replace(
            "import seaborn as sns\n",
            "import seaborn as sns\nfrom IPython.display import display\n",
        )
        set_src(nb, 3, src3)

    set_src(
        nb,
        15,
        """# A gene is a DEG when its FDR is below 0.05 and its absolute log2 fold change is at
# least 1, which means it at least doubles or halves between the groups. We also count
# the stricter cutoff of 1.5 because Task 2.2 uses that value.
from IPython.display import display

dge = lib.call_degs(dge, fdr_cut=0.05, fc_cut=1.0)
n_deg = int(dge["DEG"].sum())
n_up = int((dge["DEG"] & (dge["direction"] == "up")).sum())
n_down = int((dge["DEG"] & (dge["direction"] == "down")).sum())
n_15 = int(((dge["FDR"] < 0.05) & (dge["log2FC"].abs() >= 1.5)).sum())

print(f"DEGs at FDR < 0.05 and |log2FC| >= 1.0 : {n_deg}  (up {n_up}, down {n_down})")
print(f"DEGs at FDR < 0.05 and |log2FC| >= 1.5 : {n_15}")

up = dge[dge["DEG"] & (dge["direction"] == "up")].sort_values("log2FC", ascending=False)
down = dge[dge["DEG"] & (dge["direction"] == "down")].sort_values("log2FC")
cols = [c for c in ["log2FC", "FDR", "t", "AveExpr"] if c in dge.columns]
if not cols:
    cols = ["log2FC", "FDR"]

print("\\nTop 25 up in disease (list, not only CSV):")
display(up[cols].head(25).round(4))
print("\\nTop 25 down in disease:")
display(down[cols].head(25).round(4))
""",
    )

    insert(
        nb,
        17,
        md(
            """**Lists.** The tables above are the gene names to read. CSV is the archive; the notebook
is the place to inspect who is up (SLC6A14, DUOX2, MMPs, LCN2, …) and who is down."""
        ),
    )
    # heatmap was cell 24, after insert at 17 it is 25
    set_src(
        nb,
        25,
        """# Clustered heatmap of the 40 strongest DEGs (samples as rows).
# After clustering we cut the *gene* dendrogram into modules so you can read which
# genes travel together (the red boxes you draw by hand in other assignments).
from scipy.cluster.hierarchy import fcluster

N_HEAT = 40
N_MODULES = 4
top_heat = (
    dge[dge["DEG"]]
    .reindex(dge[dge["DEG"]]["log2FC"].abs().sort_values(ascending=False).index)
    .head(N_HEAT)
)
top40 = top_heat.index.tolist()
heat = genes[top40]
row_colors = ["tomato" if v == 1 else "steelblue" for v in target]

cg = sns.clustermap(
    heat, cmap="vlag", z_score=1, figsize=(14, 10),
    row_colors=row_colors, col_cluster=True, row_cluster=True,
    xticklabels=True, yticklabels=False, dendrogram_ratio=(0.12, 0.18),
)
cg.ax_heatmap.set_xticklabels(cg.ax_heatmap.get_xticklabels(), rotation=90, fontsize=8)
cg.fig.suptitle("Top 40 DEGs — red rows = disease, blue rows = control", y=1.02)
cg.savefig(FIG / "task2_1_heatmap.png", dpi=150)
plt.show()

# Gene order after clustering + module id (1..N_MODULES)
order = cg.dendrogram_col.reordered_ind
gene_order = [heat.columns[i] for i in order]
modules = fcluster(cg.dendrogram_col.linkage, t=N_MODULES, criterion="maxclust")
mod_by_orig = {heat.columns[i]: int(modules[i]) for i in range(len(heat.columns))}
cluster_map = pd.DataFrame({
    "gene": gene_order,
    "cluster_position": range(1, len(gene_order) + 1),
    "module": [mod_by_orig[g] for g in gene_order],
    "log2FC": [float(dge.loc[g, "log2FC"]) for g in gene_order],
    "FDR": [float(dge.loc[g, "FDR"]) for g in gene_order],
    "direction": [dge.loc[g, "direction"] for g in gene_order],
})
print("Gene modules in the heatmap (read left → right on the x axis):")
display(cluster_map)
for m, sub in cluster_map.groupby("module"):
    print(f"\\nModule {m} ({len(sub)} genes): {', '.join(sub['gene'].tolist())}")
cluster_map.to_csv(OUT / "task2_1_heatmap_gene_modules.csv", index=False)
""",
    )

    set_src(
        nb,
        28,
        """# Save tables AND show the full DEG lists in the notebook.
dge_out = dge.copy()
dge_out.to_csv(OUT / "dge_table_2_1.csv")

deg_genes = dge[dge["DEG"]].index.tolist()
deg_matrix = genes[deg_genes].copy()
deg_matrix.insert(0, "target", target)
deg_matrix.to_csv(OUT / "deg_matrix_2_1.csv", index=False)

deg_table = dge[dge["DEG"]][["log2FC", "FDR", "direction"]].copy()
deg_table = deg_table.sort_values("log2FC", ascending=False)
deg_table.to_csv(OUT / "deg_list_2_1.csv")
up[cols].to_csv(OUT / "deg_up_2_1.csv")
down[cols].to_csv(OUT / "deg_down_2_1.csv")

print("Saved dge_table_2_1.csv", dge_out.shape)
print("Saved deg_matrix_2_1.csv", deg_matrix.shape)
print("Saved deg_list_2_1.csv / deg_up_2_1.csv / deg_down_2_1.csv")
print("\\nAll", len(deg_table), "DEGs (scroll the table):")
display(deg_table.round(4))
""",
    )
    save("2_1_01_data_and_dge.ipynb", nb)
    print("patched 2_1_01")


def patch_02():
    nb = load("2_1_02_feature_selection.ipynb")
    src3 = "".join(nb["cells"][3]["source"])
    src3 = src3.replace(
        "from collections import Counter\n",
        "from collections import Counter, OrderedDict\nfrom IPython.display import display\n",
    )
    if "DATA = " not in src3:
        src3 = src3.replace(
            "OUT = pathlib.Path(\"../04_Analysis\")\n",
            "OUT = pathlib.Path(\"../04_Analysis\")\nDATA = pathlib.Path(\"../01_Data\")\n",
        )
    src3 = src3.replace(
        "print(\"Disease (1):\", int((y == 1).sum()), \" Control (0):\", int((y == 0).sum()))",
        """print("Disease (1):", int((y == 1).sum()), " Control (0):", int((y == 0).sum()))
# Per-method list size for the vote / merged map (RFECV may keep more internally).
TOPN = 25
print("TOPN per method for merged/vote lists:", TOPN)""",
    )
    set_src(nb, 3, src3)

    # drop TOPN = 50 from SMOTE cell if present
    src6 = "".join(nb["cells"][6]["source"]).replace("TOPN = 50\n", "")
    set_src(nb, 6, src6)

    set_src(
        nb,
        9,
        """# Mutual information: keep TOPN genes and *display the whole list*.
mi = pd.Series(mutual_info_classif(Xb, yb, random_state=RNG), index=genes).sort_values(ascending=False)
mi_rank = mi.reset_index()
mi_rank.columns = ["gene", "MI"]
mi_rank["rank"] = range(1, len(mi_rank) + 1)
mi_sel = set(mi.head(TOPN).index)
print(f"MI top {TOPN} (read this list):")
display(mi_rank.head(TOPN).round(4))
""",
    )

    set_src(
        nb,
        12,
        """# RFECV chooses how many genes are jointly useful (n_features_). That number can be
# large (e.g. 160). For the vote we still take TOPN by ranking so methods are comparable,
# but we *display the full RFECV support set* so you can read who was kept.
rfe = RFECV(
    LogisticRegression(max_iter=2000, class_weight="balanced", random_state=RNG),
    step=25, cv=StratifiedKFold(5, shuffle=True, random_state=RNG),
    scoring="roc_auc", min_features_to_select=10, n_jobs=1,
)
rfe.fit(Xs, yb)
rfe_rank = pd.Series(rfe.ranking_, index=genes).sort_values()
rfe_support = [g for g, keep in zip(genes, rfe.support_) if keep]
rfe_sel = set(rfe_rank.head(TOPN).index)

rfe_tbl = pd.DataFrame({
    "gene": rfe_rank.index,
    "ranking": rfe_rank.values,
    "in_rfecv_support": [g in set(rfe_support) for g in rfe_rank.index],
}).reset_index(drop=True)
rfe_tbl["rank_for_vote"] = range(1, len(rfe_tbl) + 1)

print("RFECV best n_features_ (CV):", int(rfe.n_features_))
print("RFECV support genes (read this list, can be >> TOPN):", len(rfe_support))
display(pd.DataFrame({"rfecv_support_gene": rfe_support}))
print(f"\\nTop {TOPN} by RFECV ranking (used in merged / votes):")
display(rfe_tbl.head(TOPN))
rfe_tbl.to_csv(OUT / "fs_rfecv_ranking_2_1.csv", index=False)
""",
    )
    set_src(
        nb,
        13,
        """**Reading.** RFECV often keeps a large joint set. The support table is the one to read.
The vote still uses the top TOPN ranks so MI / EN / GBC / RFECV contribute similar list sizes.""",
        kind="md",
    )

    set_src(
        nb,
        15,
        """# Elastic Net: rank by |coefficient|, show TOPN and count of non-zero genes.
en = LogisticRegression(
    penalty="elasticnet", solver="saga", l1_ratio=0.5, C=0.1,
    max_iter=5000, class_weight="balanced", random_state=RNG,
)
en.fit(Xs, yb)
en_abs = pd.Series(np.abs(en.coef_[0]), index=genes).sort_values(ascending=False)
en_tbl = en_abs.reset_index()
en_tbl.columns = ["gene", "abs_coef"]
en_tbl["nonzero"] = en_tbl["abs_coef"] > 1e-6
en_tbl["rank"] = range(1, len(en_tbl) + 1)
en_sel = set(en_abs.head(TOPN).index)
print("Non-zero Elastic Net genes:", int(en_tbl["nonzero"].sum()))
print(f"EN top {TOPN}:")
display(en_tbl.head(TOPN).round(6))
""",
    )

    set_src(
        nb,
        18,
        """# Gradient Boosting importance — full top list, not only 10 names in a print.
gb = GradientBoostingClassifier(random_state=RNG)
gb.fit(Xb, yb)
gb_imp = pd.Series(gb.feature_importances_, index=genes).sort_values(ascending=False)
gb_tbl = gb_imp.reset_index()
gb_tbl.columns = ["gene", "importance"]
gb_tbl["rank"] = range(1, len(gb_tbl) + 1)
gb_sel = set(gb_imp.head(TOPN).index)
print(f"GBC top {TOPN}:")
display(gb_tbl.head(TOPN).round(6))
""",
    )

    set_src(
        nb,
        21,
        """# Consensus (votes >= 2) stays the exam panel. Then A2-style merged + merged prune.
votes = Counter()
for s in [mi_sel, rfe_sel, en_sel, gb_sel]:
    votes.update(s)
panel = sorted([g for g, ct in votes.items() if ct >= 2], key=lambda g: -votes[g])

dge = pd.read_csv(OUT / "dge_table_2_1.csv", index_col=0)
ml = dge.reindex(panel)[["log2FC", "FDR", "direction"]].copy()
ml["n_methods"] = [votes[g] for g in panel]
ml["MI"] = [g in mi_sel for g in panel]
ml["RFECV"] = [g in rfe_sel for g in panel]
ml["ElasticNet"] = [g in en_sel for g in panel]
ml["GBC"] = [g in gb_sel for g in panel]
ml.to_csv(OUT / "ml_biomarkers_2_1.csv")
print("Consensus panel (votes >= 2):", len(panel))
print("Genes picked by 3+ methods:", int((ml["n_methods"] >= 3).sum()))
display(ml.round(4))

# --- A2 pattern: merged union of the four TOPN lists ---
method_sets = OrderedDict([
    ("MI", list(mi.head(TOPN).index)),
    ("RFECV", list(rfe_rank.head(TOPN).index)),
    ("ElasticNet", list(en_abs.head(TOPN).index)),
    ("GBC", list(gb_imp.head(TOPN).index)),
])
merged_unique_features = list(dict.fromkeys([g for lst in method_sets.values() for g in lst]))
print(f"\\nMerged unique (union of top {TOPN} per method): {len(merged_unique_features)}")

# Membership map: where each DEG comes from
all_genes = merged_unique_features
member = pd.DataFrame(0, index=all_genes, columns=list(method_sets) + ["Consensus", "MT1_ref"])
for name, lst in method_sets.items():
    member.loc[lst, name] = 1
member.loc[[g for g in panel if g in member.index], "Consensus"] = 1

mt1_path = DATA / "midterm1_biomarkers.csv"
if mt1_path.exists():
    mt1_genes = pd.read_csv(mt1_path)["gene"].astype(str).tolist()
else:
    mt1_genes = []
for g in mt1_genes:
    if g not in member.index:
        member.loc[g] = 0
    member.loc[g, "MT1_ref"] = 1
member = member.fillna(0).astype(int)
member["n_fs_methods"] = member[["MI", "RFECV", "ElasticNet", "GBC"]].sum(axis=1)
member = member.sort_values(["n_fs_methods", "Consensus"], ascending=False)
member.to_csv(OUT / "fs_membership_map_2_1.csv")
print("\\nMembership map (1 = selected). Midterm 1 is a *reference* column, not the last word.")
display(member)

fig, ax = plt.subplots(figsize=(7, max(6, 0.22 * min(len(member), 80))))
plot_m = member.drop(columns=["n_fs_methods"]).head(80)
sns.heatmap(plot_m, cmap="Blues", cbar=False, ax=ax, linewidths=0.2)
ax.set_title(f"Where DEGs come from (top {TOPN}/method; max 80 rows shown)")
ax.set_ylabel("gene")
fig.tight_layout()
fig.savefig(FIG / "task2_1_fs_membership.png", dpi=150)
plt.show()

# --- merged prune: drop one of each |r| > 0.82 pair, keep stronger |log2FC| ---
def prune_correlated_features(features, X_ref, threshold=0.82):
    feats = [f for f in features if f in X_ref.columns]
    if len(feats) < 2:
        return feats
    corr = X_ref[feats].corr().abs()
    drop = set()
    strength = dge.reindex(feats)["log2FC"].abs().fillna(0)
    for i, a in enumerate(feats):
        if a in drop:
            continue
        for b in feats[i + 1:]:
            if b in drop:
                continue
            if corr.loc[a, b] < threshold:
                continue
            sa = float(strength[a]) if a in strength.index else 0.0
            sb = float(strength[b]) if b in strength.index else 0.0
            drop.add(b if sa >= sb else a)
    return [f for f in feats if f not in drop]

X_ref = pd.DataFrame(X_train, columns=genes)
merged_pruned_features = prune_correlated_features(merged_unique_features, X_ref, 0.82)
dropped = sorted(set(merged_unique_features) - set(merged_pruned_features))
print(f"\\nMerged pruned (|r|>0.82): {len(merged_unique_features)} -> {len(merged_pruned_features)} (dropped {len(dropped)})")
print("Dropped (first 25):", dropped[:25])
pd.Series(merged_unique_features, name="gene").to_csv(OUT / "fs_merged_2_1.csv", index=False)
pd.Series(merged_pruned_features, name="gene").to_csv(OUT / "fs_merged_pruned_2_1.csv", index=False)

merged_view = dge.reindex(merged_unique_features)[["log2FC", "FDR", "direction"]].copy()
merged_view["pruned_kept"] = [g in set(merged_pruned_features) for g in merged_unique_features]
print("\\nMerged list:")
display(merged_view.round(4))

# Midterm 1 as reference comparison only
print("\\nMidterm 1 reference overlap (not a replacement of the IBD panel):")
print("MT1 genes:", mt1_genes)
print("MT1 ∩ merged:", sorted(set(mt1_genes) & set(merged_unique_features)))
print("MT1 ∩ consensus:", sorted(set(mt1_genes) & set(panel)))
print("MT1 ∩ GSE75214 DEG matrix:", sorted(set(mt1_genes) & set(genes)))
""",
    )

    set_src(
        nb,
        22,
        """**Reading.** Consensus (votes ≥ 2) is the compact exam panel. **Merged** is the union of
the four TOPN lists (A2 style). **Merged pruned** drops redundant co-expressed genes
(|r| > 0.82). The membership heatmap is the map of *where each DEG comes from*.
Midterm 1 genes are a reference column only: zero overlap is limited transfer, not a
failed midterm.""",
        kind="md",
    )
    save("2_1_02_feature_selection.ipynb", nb)
    print("patched 2_1_02")


if __name__ == "__main__":
    patch_01()
    patch_02()
