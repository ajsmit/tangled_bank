from __future__ import annotations

import re
import shutil
import tempfile
from pathlib import Path

from render_pdf import PDF_EXECUTE_SETUP, inject_pdf_execute_setup, postprocess_tex
from utils import (
    CommandFailure,
    BOOK_AUX_OUT,
    BOOK_OUT,
    BOOK_PDF_OUT,
    BOOK_TEX_OUT,
    CONFIG_ROOT,
    REPO_ROOT,
    TMP_OUT,
    chapter_manifest,
    dump_yaml,
    ensure_dirs,
    load_yaml,
    run,
    validate_pdf,
)


BOOK_SLUG = "bcb744-biostatistics-book"
FRONTMATTER_FILES = {
    "@@TITLE_PAGE_FILE@@": "title-page.tex",
    "@@HALF_TITLE_FILE@@": "half-title.tex",
    "@@COPYRIGHT_FILE@@": "copyright.tex",
    "@@DEDICATION_FILE@@": "dedication.tex",
    "@@PREFACE_FILE@@": "preface.tex",
}


def source_chapter_title(chapter: dict) -> str:
    source_path = REPO_ROOT / chapter["source"]
    text = source_path.read_text(encoding="utf-8")
    match = re.search(r"(?ms)^---\n(.*?)\n---", text)
    if not match:
        return str(chapter["title"])
    frontmatter = match.group(1)
    title_match = re.search(r'(?m)^title:\s*["\']?(.*?)["\']?\s*$', frontmatter)
    if not title_match:
        return str(chapter["title"])
    raw_title = title_match.group(1).strip()
    clean_title = re.sub(r"^\d+\.\s*", "", raw_title)
    return clean_title or str(chapter["title"])


def build_book_project_config(chapters: list[dict]) -> dict:
    foundations = [chapter["source"] for chapter in chapters if chapter["slug"].startswith(("01-", "02-", "03-", "04-"))]
    inference = [chapter["source"] for chapter in chapters if chapter["slug"].startswith(("05-", "06-", "07-", "08-", "09-", "10-"))]
    modelling = [chapter["source"] for chapter in chapters if chapter["slug"].startswith(("11-", "12-", "13-", "14-", "15-", "16-", "17-"))]
    extensions = [chapter["source"] for chapter in chapters if chapter["slug"].startswith(("18-", "19-", "20-", "21-", "22-", "23-", "24-", "25-"))]
    reproducibility = [chapter["source"] for chapter in chapters if chapter["slug"].startswith(("26-",))]
    return {
        "project": {
            "type": "book",
            "output-dir": "_book",
        },
        "book": {
            "title": "Biostatistics: The Book",
            "author": "A. J. Smit",
            "output-file": BOOK_SLUG,
            "chapters": [
                "index.qmd",
                {"part": "pdf/book/part-foundations.qmd", "chapters": foundations},
                {"part": "pdf/book/part-inference.qmd", "chapters": inference},
                {"part": "pdf/book/part-modelling.qmd", "chapters": modelling},
                {"part": "pdf/book/part-extensions.qmd", "chapters": extensions},
                {"part": "pdf/book/part-reproducibility.qmd", "chapters": reproducibility},
            ],
        },
        "execute": {
            "freeze": False,
            "cache": False,
        },
    }


def resolved_book_meta_file() -> Path:
    data = load_yaml(CONFIG_ROOT / "pdf-meta.yml")
    for key in ("bibliography", "csl"):
        value = data.get(key)
        if isinstance(value, str):
            data[key] = str((REPO_ROOT / value).resolve())

    data["documentclass"] = "book"
    data["classoption"] = ["letterpaper", "twoside", "openright"]
    data["top-level-division"] = "section"
    data["number-sections"] = True
    data["toc"] = False
    data["lof"] = False
    data["lot"] = False
    data["pdf-engine"] = "lualatex"
    data["keep-tex"] = True

    include_values = data.get("include-in-header", [])
    if isinstance(include_values, str):
        include_values = [include_values]
    resolved_includes: list[str] = []
    for value in include_values:
        src = (REPO_ROOT / value).resolve()
        if src.name == "preamble.tex":
            resolved_preamble = TMP_OUT / "book-preamble.resolved.tex"
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

    data.pop("template-partials", None)
    frontmatter_src = (REPO_ROOT / "pdf" / "templates" / "book-frontmatter.tex").resolve()
    frontmatter_out = TMP_OUT / "book-frontmatter.resolved.tex"
    frontmatter_text = frontmatter_src.read_text(encoding="utf-8")
    for token, filename in FRONTMATTER_FILES.items():
        frontmatter_text = frontmatter_text.replace(
            token,
            str((REPO_ROOT / "pdf" / "book" / filename).resolve()),
        )
    frontmatter_text = (
        rf"\renewcommand{{\BCBBookTitle}}{{Biostatistics: The Book}}" "\n"
        rf"\renewcommand{{\BCBBookSubtitle}}{{BCB744 Biostatistics}}" "\n"
        rf"\renewcommand{{\BCBBookAuthor}}{{A. J. Smit}}" "\n\n"
        + frontmatter_text
    )
    frontmatter_out.write_text(frontmatter_text, encoding="utf-8")
    data["include-before-body"] = [str(frontmatter_out)]

    metadata = data.get("metadata", {})
    if not isinstance(metadata, dict):
        metadata = {}
    metadata["pdf_book_title"] = "Biostatistics: The Book"
    metadata["pdf_book_subtitle"] = "BCB744 Biostatistics"
    metadata["pdf_book_author"] = "A. J. Smit"
    data["metadata"] = metadata

    out = TMP_OUT / "pdf-book-meta.resolved.yml"
    dump_yaml(out, data)
    return out


def hydrate_book_assets(stage_project: Path, book_dir: Path) -> None:
    asset_sources = {
        "index_files": stage_project / "index_files",
        "BCB744": stage_project / "BCB744",
        "images": stage_project / "images",
    }
    for name, src in asset_sources.items():
        if not src.exists():
            continue
        dst = book_dir / name
        if dst.exists() or dst.is_symlink():
            continue
        dst.symlink_to(src.resolve(), target_is_directory=True)


def stage_book_project(stage_project: Path, chapters: list[dict]) -> None:
    for rel in (
        Path("data"),
        Path("images"),
        Path("styles"),
        Path("includes"),
        Path("references.bib"),
        Path("_extensions"),
        Path("pdf"),
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

    dump_yaml(stage_project / "_quarto.yml", build_book_project_config(chapters))
    (stage_project / "index.qmd").write_text("", encoding="utf-8")

    for chapter in chapters:
        source_path = REPO_ROOT / chapter["source"]
        staged_source = stage_project / chapter["source"]
        staged_source.parent.mkdir(parents=True, exist_ok=True)
        source_text = source_path.read_text(encoding="utf-8")
        staged_source.write_text(inject_pdf_execute_setup(source_text), encoding="utf-8")


def copy_book_outputs(book_dir: Path) -> Path:
    pdf_path = book_dir / f"{BOOK_SLUG}.pdf"
    if not pdf_path.exists():
        raise SystemExit(f"Expected compiled book PDF not found: {pdf_path}")
    validate_pdf(pdf_path)
    BOOK_PDF_OUT.mkdir(parents=True, exist_ok=True)
    BOOK_TEX_OUT.mkdir(parents=True, exist_ok=True)
    shutil.copy2(pdf_path, BOOK_PDF_OUT / pdf_path.name)
    if book_dir.exists():
        aux_target = BOOK_AUX_OUT / BOOK_SLUG
        if aux_target.exists():
            shutil.rmtree(aux_target)
        shutil.copytree(book_dir, aux_target)
    return BOOK_PDF_OUT / pdf_path.name


def compile_book(book_dir: Path, tex_path: Path) -> Path:
    pdf_path = book_dir / f"{BOOK_SLUG}.pdf"
    log_path = book_dir / f"{BOOK_SLUG}.log"
    for suffix in (".pdf", ".aux", ".fdb_latexmk", ".fls", ".lof", ".log", ".lot", ".out", ".toc"):
        candidate = book_dir / f"{BOOK_SLUG}{suffix}"
        if candidate.exists():
            candidate.unlink()
    cmd = [
        "latexmk",
        "-r",
        str(CONFIG_ROOT / "latexmkrc"),
        "-f",
        "-g",
        f"-outdir={book_dir}",
        f"-auxdir={book_dir}",
        f"-jobname={BOOK_SLUG}",
        tex_path.name,
    ]
    try:
        run(cmd, cwd=book_dir, quiet=True)
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
        if not pdf_path.exists() or has_fatal_marker:
            raise
    return pdf_path


def postprocess_book_tex(tex_path: Path, chapters: list[dict]) -> None:
    postprocess_tex(tex_path)
    text = tex_path.read_text(encoding="utf-8")
    text = re.sub(
        r"(?:\\mainmatter\s*)?(?:\\bookmarksetup\{startatroot\}\s*)?\\section\{\}\\label\{section\}\s*(?:\\bookmarksetup\{startatroot\}\s*)?",
        lambda m: "\\mainmatter\n" if "\\mainmatter" in m.group(0) else "",
        text,
        count=1,
    )

    for chapter in chapters:
        slug = str(chapter["slug"])
        match = re.match(r"(\d+)", slug)
        if not match:
            continue
        number = int(match.group(1))
        display_title = source_chapter_title(chapter)
        label_slug = re.sub(r"[^a-z0-9]+", "-", display_title.strip().lower()).strip("-")
        label = rf"\label{{{label_slug}}}"
        label_index = text.find(label)
        if label_index == -1:
            continue
        section_index = text.rfind(r"\section{", 0, label_index)
        if section_index == -1:
            continue
        replacement = (
            rf"\setcounter{{chapter}}{{{number - 1}}}" "\n"
            rf"\chapter{{{display_title}}}{label}"
        )
        text = text[:section_index] + replacement + text[label_index + len(label):]

    text = re.sub(
        r"\\includegraphics\[[^\]]*\]\{",
        r"\\includegraphics[width=0.8\\textwidth,keepaspectratio]{",
        text,
    )

    tex_path.write_text(text, encoding="utf-8")


def build_book() -> Path:
    ensure_dirs()
    chapters = chapter_manifest()
    meta_path = resolved_book_meta_file()
    stage_root = Path(tempfile.mkdtemp(prefix="stage-book-", dir=TMP_OUT))
    stage_project = stage_root / "project"
    stage_project.mkdir(parents=True, exist_ok=True)
    try:
        stage_book_project(stage_project, chapters)
        cmd = [
            "quarto",
            "render",
            "--to",
            "latex",
            "--metadata-file",
            str(meta_path),
            "--lua-filter",
            str((REPO_ROOT / "pdf" / "filters" / "print-cleanup.lua").resolve()),
        ]
        run(cmd, cwd=stage_project)
        stage_book_dir = stage_project / "_book" / "book-latex"
        candidate_tex = [stage_book_dir / f"{BOOK_SLUG}.tex", stage_book_dir / "index.tex"]
        stage_tex = next((path for path in candidate_tex if path.exists()), None)
        if stage_tex is None:
            raise SystemExit(f"Expected rendered book TeX not found in {stage_book_dir}")
        hydrate_book_assets(stage_project, stage_book_dir)
        postprocess_book_tex(stage_tex, chapters)
        BOOK_TEX_OUT.mkdir(parents=True, exist_ok=True)
        final_tex = BOOK_TEX_OUT / f"{BOOK_SLUG}.tex"
        shutil.copy2(stage_tex, final_tex)
        compile_book(stage_book_dir, stage_tex)
        return copy_book_outputs(stage_book_dir)
    finally:
        shutil.rmtree(stage_root, ignore_errors=True)


def main() -> None:
    build_book()


if __name__ == "__main__":
    main()
