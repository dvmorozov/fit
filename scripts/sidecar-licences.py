# SPDX-License-Identifier: GPL-3.0-or-later
"""Collects the licence texts of everything the frozen sidecar bundles.

Run with the interpreter the sidecar was frozen with (scripts/build-app.ps1
does, after the freeze). What it bundles is Python itself and the closure of
Worker/py/requirements.txt - the distributions those requirements pull in,
walked through their own metadata - so that closure is what is collected, and
the build tools in the same environment (pip, PyInstaller) are not: they are
not in the package.

Each distribution's licence files go to <out>/<name>/, Python's to
<out>/python/, and <out>/README.txt lists name, version and licence. A
distribution that declares no licence file stops the build: a package that
ships a library without its licence is not one to publish.
"""

import argparse
import os
import re
import shutil
import sys
import sysconfig
from importlib import metadata

_NAME = re.compile(r"^\s*([A-Za-z0-9][A-Za-z0-9._-]*)")


def _normalise(name: str) -> str:
    return re.sub(r"[-_.]+", "-", name).lower()


def _requirement_names(lines) -> list[str]:
    names = []
    for line in lines:
        line = line.split("#", 1)[0].strip()
        if not line or line.startswith("-"):
            continue
        m = _NAME.match(line)
        if m:
            names.append(m.group(1))
    return names


def _closure(roots: list[str]) -> list[metadata.Distribution]:
    seen: dict[str, metadata.Distribution] = {}
    todo = list(roots)
    while todo:
        name = _normalise(todo.pop())
        if name in seen:
            continue
        dist = metadata.distribution(name)
        seen[name] = dist
        for req in dist.requires or []:
            #  An extra's requirement is not installed by the plain one, and a
            #  marker that does not hold here names nothing bundled.
            spec, _, marker = req.partition(";")
            if "extra" in marker:
                continue
            m = _NAME.match(spec)
            if m:
                todo.append(m.group(1))
    return sorted(seen.values(), key=lambda d: _normalise(d.metadata["Name"]))


def _licence_files(dist: metadata.Distribution) -> list[str]:
    found = []
    for f in dist.files or []:
        parts = [p.lower() for p in f.parts]
        leaf = parts[-1]
        in_info = any(p.endswith(".dist-info") for p in parts)
        if in_info and ("licenses" in parts or leaf.startswith(("license", "licence", "copying", "notice", "authors"))):
            found.append(str(dist.locate_file(f)))
    return found


def _declared_licence(dist: metadata.Distribution) -> str:
    md = dist.metadata
    for key in ("License-Expression", "License"):
        value = (md.get(key) or "").strip()
        if value and "\n" not in value and len(value) < 80:
            return value
    classifiers = [c.split("::")[-1].strip() for c in md.get_all("Classifier") or []
                   if c.startswith("License ::")]
    return ", ".join(classifiers) or "see the files in this folder"


def _python_licence() -> str:
    for d in (sysconfig.get_paths()["stdlib"], sys.base_prefix):
        for leaf in ("LICENSE.txt", "LICENSE"):
            path = os.path.join(d, leaf)
            if os.path.isfile(path):
                return path
    raise SystemExit("Python's own LICENSE file was not found beside its standard library.")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--requirements", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args(argv)

    with open(args.requirements, encoding="utf-8") as f:
        roots = _requirement_names(f)
    os.makedirs(args.out, exist_ok=True)

    index = ["The frozen compute sidecar bundles the following. Each folder here",
             "holds the licence texts its distribution ships.", ""]
    py = os.path.join(args.out, "python")
    os.makedirs(py, exist_ok=True)
    shutil.copy(_python_licence(), os.path.join(py, "LICENSE.txt"))
    index.append("python %s  Python Software Foundation License" % sys.version.split()[0])

    for dist in _closure(roots):
        name = _normalise(dist.metadata["Name"])
        files = _licence_files(dist)
        if not files:
            raise SystemExit("%s %s declares no licence file; it cannot be shipped without one."
                             % (name, dist.version))
        dest = os.path.join(args.out, name)
        os.makedirs(dest, exist_ok=True)
        for path in files:
            shutil.copy(path, os.path.join(dest, os.path.basename(path)))
        index.append("%s %s  %s" % (name, dist.version, _declared_licence(dist)))

    with open(os.path.join(args.out, "README.txt"), "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(index) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
