"""§3.12 pathway analysis reference."""
from __future__ import annotations

import pandas as pd

from .results import Section312Result

PAPER_PATHWAY_GENES = ["VWF", "IL1RL1", "DENND2B", "MMP14", "NAAA", "PANK1"]


def run_section312_pathway(biomarkers, config, display_fn=None) -> Section312Result:
    table = pd.DataFrame({
        "gene": PAPER_PATHWAY_GENES,
        "in_master_set": [g in biomarkers for g in PAPER_PATHWAY_GENES],
    })
    instructions = (
        "Run WebGestalt 2024 ORA (GO + KEGG) on paper final six genes or your master biomarker set. "
        "Significance cutoff p < 0.05."
    )
    if display_fn:
        display_fn(table)
        display_fn(instructions)
    return Section312Result(pathway_genes=PAPER_PATHWAY_GENES, instructions=instructions)
