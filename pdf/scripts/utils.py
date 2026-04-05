from __future__ import annotations

from pathlib import Path
import ast
import shutil
import subprocess
import sys
import re
from typing import Iterable


REPO_ROOT = Path(__file__).resolve().parents[2]
PDF_ROOT = REPO_ROOT / "pdf"
CONFIG_ROOT = PDF_ROOT / "config"
OUT_ROOT = PDF_ROOT / "out"
TEX_OUT = OUT_ROOT / "tex"
PDF_OUT = OUT_ROOT / "pdf"
AUX_OUT = OUT_ROOT / "aux"
LOG_OUT = OUT_ROOT / "logs"
TMP_OUT = OUT_ROOT / "tmp"
BOOK_OUT = OUT_ROOT / "book"
BOOK_TEX_OUT = BOOK_OUT / "tex"
BOOK_PDF_OUT = BOOK_OUT / "pdf"
BOOK_AUX_OUT = BOOK_OUT / "aux"


class CommandFailure(RuntimeError):
    def __init__(self, cmd: list[str], returncode: int, output: str = "") -> None:
        self.cmd = cmd
        self.returncode = returncode
        self.output = output
        super().__init__(f"Command failed with exit code {returncode}: {' '.join(cmd)}")


def ensure_dirs() -> None:
    for path in (TEX_OUT, PDF_OUT, AUX_OUT, LOG_OUT, TMP_OUT, BOOK_OUT, BOOK_TEX_OUT, BOOK_PDF_OUT, BOOK_AUX_OUT):
        path.mkdir(parents=True, exist_ok=True)
    (REPO_ROOT / "site_libs").mkdir(exist_ok=True)
    support_links = {
        PDF_ROOT / "images": REPO_ROOT / "images",
        PDF_ROOT / "styles": REPO_ROOT / "styles",
    }
    for link_path, target in support_links.items():
        if link_path.is_symlink() or link_path.exists():
            continue
        link_path.symlink_to(target, target_is_directory=True)
    tex_images = TEX_OUT / "images"
    tex_images.mkdir(parents=True, exist_ok=True)
    source_images = REPO_ROOT / "images"
    if source_images.exists():
        for item in source_images.iterdir():
            target = tex_images / item.name
            if target.exists():
                continue
            target.symlink_to(item.resolve(), target_is_directory=item.is_dir())


def run(cmd: list[str], cwd: Path | None = None, quiet: bool = False) -> str:
    if quiet:
        proc = subprocess.run(
            cmd,
            cwd=cwd or REPO_ROOT,
            capture_output=True,
            text=True,
        )
        output = (proc.stdout or "") + (proc.stderr or "")
        if proc.returncode != 0:
            raise CommandFailure(cmd, proc.returncode, output)
        return output
    proc = subprocess.run(cmd, cwd=cwd or REPO_ROOT)
    if proc.returncode != 0:
        raise SystemExit(proc.returncode)
    return ""


def validate_pdf(path: Path) -> None:
    data = path.read_bytes()
    if not data.startswith(b"%PDF-"):
        raise SystemExit(f"Generated invalid PDF for {path.stem}: missing PDF header")
    startxref_count = data.count(b"startxref")
    eof_count = data.count(b"%%EOF")
    if startxref_count != 1 or eof_count != 1:
        raise SystemExit(
            f"Generated invalid PDF for {path.stem}: malformed trailer structure "
            f"(startxref={startxref_count}, eof={eof_count})"
        )
    tail = data[-2048:]
    if b"startxref" not in tail or b"%%EOF" not in tail:
        raise SystemExit(f"Generated invalid PDF for {path.stem}: missing final trailer")
    if re.search(br"/Root\b", data) is None:
        raise SystemExit(f"Generated invalid PDF for {path.stem}: missing /Root entry")

    try:
        from pypdf import PdfReader  # type: ignore

        reader = PdfReader(str(path))
        _ = len(reader.pages)
    except ImportError:
        return
    except Exception as exc:
        raise SystemExit(f"Generated invalid PDF for {path.stem}: {exc}") from exc


def _parse_scalar(value: str):
    value = value.strip()
    if value == "":
        return ""
    if value in {"true", "false"}:
        return value == "true"
    if value in {"null", "None"}:
        return None
    if (value.startswith('"') and value.endswith('"')) or (
        value.startswith("'") and value.endswith("'")
    ):
        return ast.literal_eval(value)
    try:
        if "." in value:
            return float(value)
        return int(value)
    except ValueError:
        return value


def _parse_block(lines: list[tuple[int, str]], start: int, indent: int):
    if start >= len(lines):
        return None, start

    current_indent, current_text = lines[start]
    if current_indent < indent:
        return None, start

    if current_text.startswith("- "):
        items = []
        index = start
        while index < len(lines):
            line_indent, text = lines[index]
            if line_indent < indent or not text.startswith("- "):
                break
            if line_indent != indent:
                raise SystemExit(f"Unsupported YAML indentation in PDF pipeline config near: {text}")
            payload = text[2:].strip()
            if payload == "":
                item, index = _parse_block(lines, index + 1, indent + 2)
                items.append(item)
                continue
            if ":" in payload:
                key, raw = payload.split(":", 1)
                mapping = {key.strip(): _parse_scalar(raw)}
                index += 1
                while index < len(lines):
                    next_indent, next_text = lines[index]
                    if next_indent < indent + 2:
                        break
                    if next_indent == indent + 2 and ":" in next_text and not next_text.startswith("- "):
                        sub_key, sub_raw = next_text.split(":", 1)
                        sub_key = sub_key.strip()
                        sub_raw = sub_raw.strip()
                        if sub_raw == "":
                            sub_value, index = _parse_block(lines, index + 1, indent + 4)
                            mapping[sub_key] = sub_value
                        else:
                            mapping[sub_key] = _parse_scalar(sub_raw)
                            index += 1
                    else:
                        break
                items.append(mapping)
            else:
                items.append(_parse_scalar(payload))
                index += 1
        return items, index

    mapping = {}
    index = start
    while index < len(lines):
        line_indent, text = lines[index]
        if line_indent < indent:
            break
        if line_indent != indent:
            raise SystemExit(f"Unsupported YAML indentation in PDF pipeline config near: {text}")
        if ":" not in text:
            raise SystemExit(f"Unsupported YAML syntax in PDF pipeline config near: {text}")
        key, raw = text.split(":", 1)
        key = key.strip()
        raw = raw.strip()
        if raw == "":
            value, index = _parse_block(lines, index + 1, indent + 2)
            mapping[key] = value
        else:
            mapping[key] = _parse_scalar(raw)
            index += 1
    return mapping, index


def load_yaml(path: Path) -> dict:
    try:
        import yaml  # type: ignore
    except ImportError:
        yaml = None
    if yaml is not None:
        with path.open("r", encoding="utf-8") as handle:
            data = yaml.safe_load(handle)
        return data or {}

    raw_lines = path.read_text(encoding="utf-8").splitlines()
    lines: list[tuple[int, str]] = []
    for raw in raw_lines:
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        indent = len(raw) - len(raw.lstrip(" "))
        lines.append((indent, raw.strip()))
    if not lines:
        return {}
    data, _ = _parse_block(lines, 0, lines[0][0])
    if not isinstance(data, dict):
        raise SystemExit(f"Expected mapping at top level of {path}")
    return data or {}


def _format_scalar(value) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if value is None:
        return "null"
    if isinstance(value, (int, float)):
        return str(value)
    text = str(value)
    plain_safe = all(ch.isalnum() or ch in "/._-+" for ch in text)
    return text if plain_safe and text else f'"{text}"'


def _dump_yaml_obj(data, indent: int = 0) -> list[str]:
    prefix = " " * indent
    lines: list[str] = []
    if isinstance(data, dict):
        for key, value in data.items():
            if isinstance(value, (dict, list)):
                lines.append(f"{prefix}{key}:")
                lines.extend(_dump_yaml_obj(value, indent + 2))
            else:
                lines.append(f"{prefix}{key}: {_format_scalar(value)}")
        return lines
    if isinstance(data, list):
        for item in data:
            if isinstance(item, dict):
                first = True
                for key, value in item.items():
                    if first:
                        if isinstance(value, (dict, list)):
                            lines.append(f"{prefix}- {key}:")
                            lines.extend(_dump_yaml_obj(value, indent + 4))
                        else:
                            lines.append(f"{prefix}- {key}: {_format_scalar(value)}")
                        first = False
                    else:
                        if isinstance(value, (dict, list)):
                            lines.append(f"{prefix}  {key}:")
                            lines.extend(_dump_yaml_obj(value, indent + 4))
                        else:
                            lines.append(f"{prefix}  {key}: {_format_scalar(value)}")
            elif isinstance(item, (dict, list)):
                lines.append(f"{prefix}-")
                lines.extend(_dump_yaml_obj(item, indent + 2))
            else:
                lines.append(f"{prefix}- {_format_scalar(item)}")
        return lines
    raise SystemExit("Unsupported data type while writing PDF pipeline YAML.")


def dump_yaml(path: Path, data: dict) -> None:
    try:
        import yaml  # type: ignore
    except ImportError:
        yaml = None
    if yaml is not None:
        with path.open("w", encoding="utf-8") as handle:
            yaml.safe_dump(data, handle, sort_keys=False)
        return
    path.write_text("\n".join(_dump_yaml_obj(data)) + "\n", encoding="utf-8")


def chapter_manifest() -> list[dict]:
    data = load_yaml(CONFIG_ROOT / "chapters.yml")
    chapters = data.get("chapters", [])
    if not isinstance(chapters, list):
        raise SystemExit("pdf/config/chapters.yml must define a 'chapters' list.")
    return chapters


def find_chapter(name: str) -> dict:
    chapters = chapter_manifest()
    for chapter in chapters:
        if chapter["slug"] == name or chapter["source"] == name:
            return chapter
    raise SystemExit(f"Unknown chapter '{name}'.")


def chapter_tex_path(slug: str) -> Path:
    return TEX_OUT / f"{slug}.tex"


def chapter_pdf_path(slug: str) -> Path:
    return PDF_OUT / f"{slug}.pdf"


def python_executable() -> str:
    return sys.executable or "python3"


def backup_files(paths: Iterable[Path]) -> dict[Path, bytes | None]:
    state: dict[Path, bytes | None] = {}
    for path in paths:
        state[path] = path.read_bytes() if path.exists() else None
    return state


def restore_files(state: dict[Path, bytes | None]) -> None:
    for path, content in state.items():
        if content is None:
            if path.exists():
                path.unlink()
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(content)


def reset_dir(path: Path) -> None:
    if path.exists():
        shutil.rmtree(path)
    path.mkdir(parents=True, exist_ok=True)
