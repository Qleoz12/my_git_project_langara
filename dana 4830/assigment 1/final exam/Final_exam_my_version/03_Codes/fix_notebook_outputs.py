"""Fix invalid notebook stream outputs (missing 'name' field)."""
from __future__ import annotations

import json
import pathlib
import sys


def fix_notebook(path: pathlib.Path) -> None:
    nb = json.loads(path.read_text(encoding="utf-8"))
    changed = False
    for cell in nb.get("cells", []):
        if cell.get("cell_type") == "code" and "outputs" not in cell:
            cell["outputs"] = []
            changed = True
        if cell.get("cell_type") == "code" and "execution_count" not in cell:
            cell["execution_count"] = None
            changed = True
        for out in cell.get("outputs", []):
            if out.get("output_type") == "stream" and "name" not in out:
                out["name"] = "stdout"
                changed = True
            if out.get("output_type") == "display_data" and "metadata" not in out:
                out["metadata"] = {}
                changed = True
            if out.get("output_type") == "execute_result" and "metadata" not in out:
                out["metadata"] = {}
                changed = True
    if changed:
        path.write_text(json.dumps(nb, indent=1, ensure_ascii=False), encoding="utf-8")
        print(f"Fixed {path}")
    else:
        print(f"No changes needed for {path}")


if __name__ == "__main__":
    codes = pathlib.Path(__file__).resolve().parent
    default = [
        "2_1_01_data_and_dge.ipynb",
        "2_1_02_feature_selection.ipynb",
        "2_1_03_evaluation_and_comparison.ipynb",
        "2_2_01_merge_datasets.ipynb",
        "2_2_02_validate_pipelines.ipynb",
        "2_0_final_overview.ipynb",
    ]
    for name in sys.argv[1:] or default:
        fix_notebook(codes / name)
