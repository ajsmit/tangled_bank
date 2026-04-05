from __future__ import annotations

import argparse
import shutil
import re
import tempfile
from pathlib import Path

from utils import (
    AUX_OUT,
    CommandFailure,
    CONFIG_ROOT,
    LOG_OUT,
    PDF_OUT,
    REPO_ROOT,
    TEX_OUT,
    chapter_manifest,
    chapter_pdf_path,
    chapter_tex_path,
    dump_yaml,
    ensure_dirs,
    find_chapter,
    load_yaml,
    reset_dir,
    run,
    validate_pdf,
    TMP_OUT,
)

PDF_EXECUTE_SETUP = """```{r pdf-print-width, include=FALSE}
options(
  width = 75,
  pillar.width = 75
)
```

"""


def inject_pdf_execute_setup(content: str) -> str:
    if "pdf-print-width" in content:
        return content
    if content.startswith("---\n"):
        end = content.find("\n---", 4)
        if end != -1:
            end += 4
            return content[:end] + "\n\n" + PDF_EXECUTE_SETUP + content[end:].lstrip("\n")
    return PDF_EXECUTE_SETUP + content


def resolved_meta_file(chapter: dict | None = None) -> Path:
    data = load_yaml(CONFIG_ROOT / "pdf-meta.yml")
    for key in ("bibliography", "csl"):
        value = data.get(key)
        if isinstance(value, str):
            data[key] = str((REPO_ROOT / value).resolve())

    include_values = data.get("include-in-header", [])
    if isinstance(include_values, str):
        include_values = [include_values]
    resolved_includes: list[str] = []
    for value in include_values:
        src = (REPO_ROOT / value).resolve()
        if src.name == "preamble.tex":
            resolved_preamble = TMP_OUT / "preamble.resolved.tex"
            content = src.read_text(encoding="utf-8")
            content = content.replace(
                r"\input{pdf/templates/macros.tex}",
                rf"\input{{{(REPO_ROOT / 'pdf' / 'templates' / 'macros.tex').resolve()}}}",
            )
            resolved_preamble.write_text(content, encoding="utf-8")
            resolved_includes.append(str(resolved_preamble))
        else:
            resolved_includes.append(str(src))
    data["include-in-header"] = resolved_includes

    for key in ("template-partials",):
        values = data.get(key, [])
        if isinstance(values, str):
            values = [values]
        if isinstance(values, list):
            data[key] = [str((REPO_ROOT / value).resolve()) for value in values]

    metadata = data.get("metadata", {})
    if not isinstance(metadata, dict):
        metadata = {}
    if chapter is not None:
        slug = str(chapter.get("slug", ""))
        match = re.match(r"(\d+)", slug)
        if match:
            metadata["pdf_chapter_number"] = int(match.group(1))
    data["metadata"] = metadata

    out = TMP_OUT / "pdf-meta.resolved.yml"
    dump_yaml(out, data)
    return out


def classify_callout_kind(kind: str, title: str) -> str:
    normalized = title.strip().lower()
    if normalized == "mathematical detail":
        return "math"
    if normalized.startswith("do it now"):
        return "action"
    if normalized.startswith("self-assessment task"):
        return "assessment"
    return kind


def latex_callout(kind: str, title: str, body: str) -> str:
    resolved_kind = classify_callout_kind(kind, title)
    cleaned_body = body.strip()
    return (
        f"\\begin{{bcbcallout}}{{{resolved_kind}}}{{{title.strip()}}}\n\n"
        f"{cleaned_body}\n\n"
        "\\end{bcbcallout}"
    )


def postprocess_tex(tex_path: Path) -> None:
    text = tex_path.read_text(encoding="utf-8")

    math_detail_callout = re.compile(
        r"""
        \\begin\{tcolorbox\}\[[^\]]*?colframe=quarto-callout-note-color-frame[^\]]*?\]
        \s*\\begin\{minipage\}\[t\]\{5\.5mm\}
        \s*\\textcolor\{quarto-callout-note-color\}\{\\faInfo\}
        \s*\\end\{minipage\}%
        \s*\\begin\{minipage\}\[t\]\{\s*\\textwidth\s*-\s*5\.5mm\s*\}
        \s*\\vspace\{-3mm\}\\textbf\{Mathematical Detail\}\\vspace\{3mm\}
        \s*(?P<body>.*?)
        \s*\\end\{minipage\}%
        \s*\\end\{tcolorbox\}
        """,
        re.DOTALL | re.VERBOSE,
    )

    titled_callout = re.compile(
        r"""
        \\begin\{tcolorbox\}\[
        (?P<options>[^\]]*?colframe=quarto-callout-(?P<kind>[a-z]+)-color-frame[^\]]*?)
        title=\\textcolor\{quarto-callout-[^}]+\}\{\\fa[^}]+\}\\hspace\{0\.5em\}\{(?P<title>.*?)\}
        [^\]]*?\]
        \n\n
        (?P<body>.*?)
        \n\n\\end\{tcolorbox\}
        """,
        re.DOTALL | re.VERBOSE,
    )

    untitled_callout = re.compile(
        r"""
        \\begin\{tcolorbox\}\[
        [^\]]*?colframe=quarto-callout-(?P<kind>[a-z]+)-color-frame[^\]]*?
        \]
        \s*\\begin\{minipage\}\[t\]\{5\.5mm\}
        \s*\\textcolor\{quarto-callout-[^}]+\}\{\\fa[^}]+\}
        \s*\\end\{minipage\}%
        \s*\\begin\{minipage\}\[t\]\{\\textwidth\s*-\s*5\.5mm\}
        \s*\\vspace\{-3mm\}\\textbf\{(?P<title>.*?)\}\\vspace\{3mm\}
        \s*(?P<body>.*?)
        \s*\\end\{minipage\}%
        \s*\\end\{tcolorbox\}
        """,
        re.DOTALL | re.VERBOSE,
    )

    text = math_detail_callout.sub(
        lambda match: latex_callout("math", "Mathematical Detail", match.group("body")),
        text,
    )
    text = titled_callout.sub(
        lambda match: latex_callout(
            match.group("kind"), match.group("title"), match.group("body")
        ),
        text,
    )
    text = untitled_callout.sub(
        lambda match: latex_callout(
            match.group("kind"), match.group("title"), match.group("body")
        ),
        text,
    )

    def resize_figure_block(match: re.Match[str]) -> str:
        block = match.group(0)
        lowered = block.lower()
        panelled = (
            "panel" in lowered
            or "panels" in lowered
            or "side by side" in lowered
            or "clockwise from top left" in lowered
        )
        target = "0.8\\\\textwidth" if panelled else "0.5\\\\textwidth"
        return re.sub(
            r"\\includegraphics\[[^\]]*\]\{",
            rf"\\includegraphics[width={target},keepaspectratio]{{",
            block,
            count=1,
        )

    text = re.sub(
        r"\\begin\{figure\}.*?\\end\{figure\}",
        resize_figure_block,
        text,
        flags=re.DOTALL,
    )

    tex_path.write_text(text, encoding="utf-8")


def render_tex(chapter: dict, quiet: bool = False) -> Path:
    source = chapter["source"]
    slug = chapter["slug"]
    tex_path = chapter_tex_path(slug)
    meta_path = resolved_meta_file(chapter)
    source_path = REPO_ROOT / source
    stage_root = Path(tempfile.mkdtemp(prefix=f"stage-{slug}-", dir=TMP_OUT))
    stage_project = stage_root / "project"
    stage_project.mkdir(parents=True, exist_ok=True)
    try:
        for rel in (
            Path("data"),
            Path("images"),
            Path("styles"),
            Path("includes"),
            Path("references.bib"),
            Path("_extensions"),
        ):
            src = REPO_ROOT / rel
            if not src.exists():
                continue
            dst = stage_project / rel
            dst.parent.mkdir(parents=True, exist_ok=True)
            if dst.exists() or dst.is_symlink():
                continue
            if src.is_dir():
                dst.symlink_to(src.resolve(), target_is_directory=True)
            else:
                dst.symlink_to(src.resolve())

        project_config = {
            "project": {
                "type": "default",
                "output-dir": str(TEX_OUT.resolve()),
            }
        }
        dump_yaml(stage_project / "_quarto.yml", project_config)

        staged_source = stage_project / source
        staged_source.parent.mkdir(parents=True, exist_ok=True)
        source_text = source_path.read_text(encoding="utf-8")
        staged_source.write_text(inject_pdf_execute_setup(source_text), encoding="utf-8")

        parent_metadata = source_path.parent / "_metadata.yml"
        if parent_metadata.exists():
            staged_parent_metadata = staged_source.parent / "_metadata.yml"
            shutil.copy2(parent_metadata, staged_parent_metadata)

        cmd = [
            "quarto",
            "render",
            source_path.name,
            "--to",
            "latex",
            "--metadata-file",
            str(meta_path),
            "--lua-filter",
            str(REPO_ROOT / "pdf" / "filters" / "print-cleanup.lua"),
        ]
        run(cmd, cwd=stage_project / source_path.parent.relative_to(REPO_ROOT), quiet=quiet)

        rendered_tex = TEX_OUT / source_path.parent.relative_to(REPO_ROOT) / tex_path.name
        if rendered_tex.exists():
            shutil.copy2(rendered_tex, tex_path)
        elif not tex_path.exists():
            staged_tex = stage_project / source_path.parent.relative_to(REPO_ROOT) / tex_path.name
            if not staged_tex.exists():
                raise SystemExit(f"Expected rendered TeX not found: {tex_path}, {rendered_tex}, or {staged_tex}")
            shutil.copy2(staged_tex, tex_path)

        support_dir = TEX_OUT / source_path.parent.relative_to(REPO_ROOT) / f"{source_path.stem}_files"

        staged_support_dir = stage_project / source_path.parent.relative_to(REPO_ROOT) / f"{source_path.stem}_files"
        if staged_support_dir.exists() and not support_dir.exists():
            support_dir.parent.mkdir(parents=True, exist_ok=True)
            shutil.copytree(staged_support_dir, support_dir)
        local_support_link = TEX_OUT / f"{source_path.stem}_files"
        if support_dir.exists() and not local_support_link.exists():
            local_support_link.symlink_to(support_dir.relative_to(TEX_OUT), target_is_directory=True)
        if support_dir.exists():
            mediabag_dir = support_dir / "mediabag"
            mediabag_dir.mkdir(parents=True, exist_ok=True)
            support_images = support_dir / "images"
            if not support_images.exists():
                support_images.symlink_to((TEX_OUT / "images").resolve(), target_is_directory=True)
            mirrored_images = TEX_OUT / source_path.parent.relative_to(REPO_ROOT) / "images"
            mirrored_images.parent.mkdir(parents=True, exist_ok=True)
            if not mirrored_images.exists():
                mirrored_images.symlink_to((TEX_OUT / "images").resolve(), target_is_directory=True)
        postprocess_tex(tex_path)
        return tex_path
    finally:
        shutil.rmtree(stage_root, ignore_errors=True)


def compile_pdf(tex_path: Path, slug: str, quiet: bool = False) -> Path:
    pdf_path = chapter_pdf_path(slug)
    for suffix in (".pdf", ".aux", ".fdb_latexmk", ".fls", ".lof", ".log", ".lot", ".out", ".toc"):
        candidate = AUX_OUT / f"{slug}{suffix}"
        if candidate.exists():
            candidate.unlink()
    cmd = [
        "latexmk",
        "-r",
        str(CONFIG_ROOT / "latexmkrc"),
        "-f",
        "-g",
        f"-outdir={AUX_OUT}",
        f"-auxdir={AUX_OUT}",
        f"-jobname={slug}",
        tex_path.name,
    ]
    aux_pdf = AUX_OUT / f"{slug}.pdf"
    log_path = AUX_OUT / f"{slug}.log"
    try:
        run(cmd, cwd=TEX_OUT, quiet=quiet)
    except CommandFailure as exc:
        log_text = log_path.read_text(encoding="utf-8", errors="ignore") if log_path.exists() else exc.output
        fatal_markers = (
            "! LaTeX Error:",
            "Undefined control sequence",
            "Emergency stop",
            "Fatal error occurred",
            "Missing character:",
        )
        has_fatal_marker = any(marker in log_text for marker in fatal_markers)
        if not aux_pdf.exists() or has_fatal_marker:
            raise
    if not aux_pdf.exists():
        raise SystemExit(f"Expected compiled PDF not found: {aux_pdf}")
    validate_pdf(aux_pdf)
    pdf_path.write_bytes(aux_pdf.read_bytes())
    return pdf_path


def publish_pdf(chapter: dict, pdf_path: Path) -> None:
    source = chapter["source"]
    if not source.startswith("BCB744/basic_stats/"):
        return
    site_dir = REPO_ROOT / "_site" / "BCB744" / "basic_stats"
    site_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(pdf_path, site_dir / pdf_path.name)


def build_one(chapter: dict, quiet: bool = False) -> Path:
    slug = chapter["slug"]
    tex_path = render_tex(chapter, quiet=quiet)
    pdf_path = compile_pdf(tex_path, slug, quiet=quiet)
    publish_pdf(chapter, pdf_path)
    return pdf_path


def main() -> None:
    parser = argparse.ArgumentParser(description="Render BCB744 standalone PDFs.")
    parser.add_argument("target", help="'all' or a chapter slug/source path")
    args = parser.parse_args()

    ensure_dirs()

    if args.target == "all":
        for chapter in chapter_manifest():
            build_one(chapter)
        return

    chapter = find_chapter(args.target)
    build_one(chapter)


if __name__ == "__main__":
    main()
