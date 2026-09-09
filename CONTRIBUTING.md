# Contributing to Fit

Thanks for your interest in improving Fit. This document covers how contributions are licensed and the
basics of getting set up. For architecture and extension points, see `docs/contributing/`.

## Licensing of contributions

- **Code** is licensed under **GPL-3.0-or-later** (see `LICENSE`).
- **Documentation** (including the site/Pages content under `docs/`) is licensed under
  **CC BY 4.0** (see `docs/LICENSE`).

By contributing, you agree your contributions are licensed under these same terms.

## Developer Certificate of Origin (DCO)

This project uses the **DCO** instead of a CLA. Every commit must be signed off, certifying you wrote the
change or have the right to submit it under the project license (see https://developercertificate.org/).

Add a sign-off line to each commit:

```
Signed-off-by: Your Name <your-email@example.com>
```

You can do this automatically with `git commit -s`.

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

Fit is a focused, interactive multi-peak curve-fitting tool (general + XRD/neutron). It deliberately does
**not** aim to be a Rietveld/structure-refinement suite. Anything beyond that scope belongs in a
module of your own - which is precisely what the extension contract is for. See
[the architecture](docs/contributing/architecture.md) for the full list of extension points.
