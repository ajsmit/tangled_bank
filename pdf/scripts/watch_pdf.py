from __future__ import annotations

import atexit
import fcntl
import os
from pathlib import Path
import select
import subprocess
import time
import traceback

from utils import CONFIG_ROOT, PDF_ROOT, TMP_OUT, chapter_manifest, ensure_dirs


WATCH_PATHS = [
    CONFIG_ROOT / "chapters.yml",
    CONFIG_ROOT / "pdf-meta.yml",
    CONFIG_ROOT / "latexmkrc",
    PDF_ROOT / "filters" / "print-cleanup.lua",
    PDF_ROOT / "templates" / "preamble.tex",
    PDF_ROOT / "templates" / "macros.tex",
    PDF_ROOT / "templates" / "before-body.tex",
]

DEBOUNCE_SECONDS = 0.8
LOCK_PATH = TMP_OUT / "watch_pdf.lock"


def log(message: str) -> None:
    print(message, flush=True)


def acquire_lock() -> object:
    LOCK_PATH.parent.mkdir(parents=True, exist_ok=True)
    handle = open(LOCK_PATH, "w", encoding="utf-8")
    try:
        fcntl.flock(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError as exc:
        handle.close()
        raise SystemExit("watch_pdf.py is already running. Stop the existing watcher first.") from exc
    handle.write(f"{os.getpid()}\n")
    handle.flush()

    def _release() -> None:
        try:
            fcntl.flock(handle.fileno(), fcntl.LOCK_UN)
        except OSError:
            pass
        try:
            handle.close()
        except OSError:
            pass

    atexit.register(_release)
    return handle


def watched_files() -> list[Path]:
    return WATCH_PATHS + [Path(ch["source"]) for ch in chapter_manifest()]


def snapshot() -> dict[str, float]:
    files = watched_files()
    return {str(f): f.stat().st_mtime for f in files if f.exists()}


def watch_dirs() -> list[Path]:
    dirs = {path.parent.resolve() for path in watched_files()}
    return sorted(dirs)


def changed_paths(state: dict[str, float], new_state: dict[str, float]) -> list[str]:
    changed = [path for path, mtime in new_state.items() if mtime != state.get(path)]
    removed = [path for path in state if path not in new_state]
    return sorted(set(changed + removed))


def rebuild_for_changes(changed: list[str]) -> None:
    qmd_changes = [path for path in changed if path.endswith(".qmd")]
    if qmd_changes:
        rebuild_set = []
        for source in qmd_changes:
            chapter = next(ch for ch in chapter_manifest() if ch["source"] == source)
            rebuild_set.append(chapter)
    else:
        rebuild_set = chapter_manifest()

    for chapter in rebuild_set:
        try:
            log(f"Rebuilding {chapter['slug']}...")
            script = (PDF_ROOT / "scripts" / "bcb744_pdf").resolve()
            proc = subprocess.run(
                [str(script), "build", chapter["slug"]],
                cwd=PDF_ROOT.parent,
                capture_output=True,
                text=True,
            )
            if proc.returncode != 0:
                output = "\n".join(
                    part for part in (proc.stdout.strip(), proc.stderr.strip()) if part
                )
                raise RuntimeError(
                    f"Command failed with exit code {proc.returncode}: {script} build {chapter['slug']}"
                    + (f"\n{output}" if output else "")
                )
            log(f"Finished {chapter['slug']}.")
        except Exception as exc:
            log(f"Build failed for {chapter['slug']}: {exc}")
            traceback.print_exc()


def poll_loop() -> None:
    log("watch_pdf.py: falling back to polling mode.")
    state = snapshot()
    while True:
        time.sleep(1.0)
        new_state = snapshot()
        changed = changed_paths(state, new_state)
        if changed:
            log(f"Detected changes in: {', '.join(changed)}")
            rebuild_for_changes(changed)
            state = new_state


def register_file_events(kq: select.kqueue) -> list[int]:
    fds: list[int] = []
    events = []
    for path in watched_files():
        resolved = path.resolve()
        if not resolved.exists():
            continue
        fd = os.open(resolved, os.O_EVTONLY)
        fds.append(fd)
        events.append(
            select.kevent(
                fd,
                filter=select.KQ_FILTER_VNODE,
                flags=select.KQ_EV_ADD | select.KQ_EV_ENABLE | select.KQ_EV_CLEAR,
                fflags=(
                    select.KQ_NOTE_WRITE
                    | select.KQ_NOTE_EXTEND
                    | select.KQ_NOTE_ATTRIB
                    | select.KQ_NOTE_LINK
                    | select.KQ_NOTE_RENAME
                    | select.KQ_NOTE_DELETE
                    | select.KQ_NOTE_REVOKE
                ),
            )
        )
    for event in events:
        kq.control([event], 0, 0)
    return fds


def kqueue_loop() -> None:
    kq = select.kqueue()
    state = snapshot()
    log("Watching BCB744 standalone PDF sources with filesystem events...")
    file_handles = register_file_events(kq)
    try:
        while True:
            ready = kq.control(None, 1, None)
            if not ready:
                continue
            time.sleep(DEBOUNCE_SECONDS)
            while kq.control(None, 32, 0):
                time.sleep(0.1)
            new_state = snapshot()
            changed = changed_paths(state, new_state)
            if changed:
                log(f"Detected changes in: {', '.join(changed)}")
                rebuild_for_changes(changed)
                state = snapshot()
            for fd in file_handles:
                os.close(fd)
            kq.close()
            kq = select.kqueue()
            file_handles = register_file_events(kq)
    finally:
        for fd in file_handles:
            os.close(fd)
        kq.close()


def main() -> None:
    ensure_dirs()
    _lock = acquire_lock()
    if hasattr(select, "kqueue"):
        kqueue_loop()
    else:
        poll_loop()


if __name__ == "__main__":
    main()
