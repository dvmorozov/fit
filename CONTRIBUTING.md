# Contributing to Fit

Thanks for your interest in improving Fit. This document covers how contributions are licensed and the
basics of getting set up. For architecture and extension points, see `docs/contributing/`.

## Licensing of contributions

- **Code** is licensed under **GPL-3.0-or-later** (see `LICENSE`).
- **Documentation** (including the site/Pages content under `docs/`) is licensed under
  **CC BY 4.0** (see `docs/LICENSE`).

By contributing, you agree to three things:

1. **Everyone gets your contribution under these same terms.** It is published
   under GPL-3.0-or-later (code) or CC BY 4.0 (documentation), like the rest.
2. **The maintainer may also license it under other terms.** You grant Dmitry
   Morozov, the maintainer and copyright holder of this project, a perpetual,
   worldwide, non-exclusive, royalty-free and irrevocable licence to use, modify,
   sublicense and distribute your contribution under terms of his choosing,
   including terms that are not free. You keep the copyright in what you wrote,
   and everyone else's rights to it under the GPL are unaffected.
3. **It is yours to give.** You wrote it, or otherwise have the right to submit it
   on these terms - including your employer's permission where your employer has
   rights in what you create.

**Why the second.** Software built on this framework is also offered under other
terms, which only the holder of every part of it can do. Without the grant, a
single contribution would make that impossible for the whole framework - while
with it, nothing about the GPL version changes for anyone.

**How to say it.** Contributions arrive as issues with a patch attached (see
below), so say it there: *"I agree to the contribution terms in
CONTRIBUTING.md."* This replaces the Developer Certificate of Origin sign-off the
project used to ask for; its third point is what the sign-off certified.

## How this repository is published

**Read this before opening a pull request.** This repository is a **snapshot**, not
a mirror: each publication force-pushes a single orphan commit to `main` from a
development tree that is kept privately. The history you see here is regenerated
every time.

The consequence is blunt and worth stating plainly rather than letting you find
out: **a pull request cannot be merged directly.** The next publication would
overwrite it. Nothing about your contribution is unwelcome - the mechanism simply
cannot accept it as a merge.

What works instead:

- **Open an issue.** Describe the change, or attach a patch (`git format-patch`).
  It is applied in the development tree and appears in the next snapshot, with
  authorship preserved in the commit trailers.
- **For a new curve type, loader, backend or whole vertical, you may not need us
  at all.** Extension is by registration: a directory plus one search-path entry,
  in a repository of your own. See
  [writing a module](docs/contributing/writing-a-module.md).

## Getting set up

- Building from source: [docs/user-guide/building-from-source.md](docs/user-guide/building-from-source.md)
  - the script and the Lazarus IDE walkthrough, with exact versions.
- `./scripts/build-app.ps1` is the whole build. CI runs the same script, so what
  fails for you fails there.
- Use the GitHub **noreply** email and an **SSH** remote for this and the component
  repos (`fitgrids`, `fitminimizers`) to avoid email-privacy push rejections.

## Scope

Fit is a framework for fitting arbitrary data to arbitrary classes of models, for research and for
teaching. The framework itself carries no field: each class of fitting tasks - diffraction, technical
analysis, the next one - is a module that owns its interface, its models, its rules and its
computations, and explains them. Diffraction still lives in this tree while it is moved out
([the plan](docs/internal/diffraction-extraction.md)). A new field belongs in a module of your own -
which is precisely what [the module contract](docs/contributing/module-architecture.md) is for.
