# -*- coding: utf-8 -*-
"""Genera `A1_Leonardo_LS_ES.ipynb` desde el inglés intacto usando fuentes españolas locales."""
from __future__ import annotations

import json
from pathlib import Path

from a1_spanish_cell_sources_part1 import PART1
from a1_spanish_cell_sources_part2 import PART2


def text_to_nb_source(text: str) -> list[str]:
    if text == "":
        return []
    return text.splitlines(keepends=True)


def main() -> None:
    root = Path(__file__).resolve().parent
    src_nb = root / "A1_Leonardo_LS.ipynb"
    dst_nb = root / "A1_Leonardo_LS_ES.ipynb"

    cells_es = PART1 + PART2
    nb = json.loads(src_nb.read_text(encoding="utf-8"))

    if len(nb["cells"]) != len(cells_es):
        raise SystemExit(
            f"Cantidad de celdas discordante: notebook {len(nb['cells'])} vs traducciones {len(cells_es)}"
        )

    for cell, spanish_txt in zip(nb["cells"], cells_es):
        cell["source"] = text_to_nb_source(spanish_txt)
        if cell["cell_type"] == "code":
            cell["outputs"] = []
            cell["execution_count"] = None

    dst_nb.write_text(
        json.dumps(nb, ensure_ascii=False, indent=1) + "\n",
        encoding="utf-8",
    )
    print(f"Creado: {dst_nb}")


if __name__ == "__main__":
    main()
