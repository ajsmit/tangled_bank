from __future__ import annotations

import shutil

from utils import OUT_ROOT


def main() -> None:
    if OUT_ROOT.exists():
        shutil.rmtree(OUT_ROOT)


if __name__ == "__main__":
    main()
