#!/usr/bin/env python3
"""Check that CMake and Meson compile every library Fortran source."""

from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = ROOT / "src"


def relative_sources() -> set[str]:
    return {
        path.relative_to(ROOT).as_posix()
        for path in SOURCE_ROOT.rglob("*.f90")
    }


def cmake_sources() -> set[str]:
    found: set[str] = set()
    pattern = re.compile(r'"\$\{dir\}/([^"\n]+\.f90)"')
    for manifest in SOURCE_ROOT.rglob("CMakeLists.txt"):
        for match in pattern.findall(manifest.read_text(encoding="utf-8")):
            found.add((manifest.parent / match).relative_to(ROOT).as_posix())
    return found


def meson_sources() -> set[str]:
    found: set[str] = set()
    pattern = re.compile(r"['\"]([^'\"\n]+\.f90)['\"]")
    for manifest in SOURCE_ROOT.rglob("meson.build"):
        for match in pattern.findall(manifest.read_text(encoding="utf-8")):
            found.add((manifest.parent / match).relative_to(ROOT).as_posix())
    return found


def report(name: str, actual: set[str], listed: set[str]) -> bool:
    missing = sorted(actual - listed)
    stale = sorted(listed - actual)
    if not missing and not stale:
        print(f"{name}: {len(actual)} sources accounted for")
        return True
    for path in missing:
        print(f"{name}: missing {path}", file=sys.stderr)
    for path in stale:
        print(f"{name}: stale {path}", file=sys.stderr)
    return False


def main() -> int:
    actual = relative_sources()
    valid = report("CMake", actual, cmake_sources())
    valid = report("Meson", actual, meson_sources()) and valid
    return 0 if valid else 1


if __name__ == "__main__":
    raise SystemExit(main())
