# Fit

An interactive curve-fitting application. Load a data set, place curves on it, and
fit them — one peak or a hundred, by hand or automatically. Save the whole session
as a project and reopen it later to carry on from where you stopped.

**Automatic decomposition** (*Fit → Automatically*) needs no curve count from you.
It seeds a curve on every point of every peak, then removes curves for as long as
the fit stays within the accuracy you set, so what remains is the fewest curves
that accuracy allows. See [the Fitting chapter](https://dvmorozov.github.io/fit/guide-fitting.html)
of the user guide and [how it decides](https://dvmorozov.github.io/fit/how-many-curves.html).

**Data from where it is published** (*File → New Project from Data Source*)
finds a data set, downloads it and starts a project from it: the samples
installed with Fit, any web address, or a DOI resolved to a Zenodo or figshare
record. [`Modules/open-data-spectra/`](Modules/open-data-spectra/README.md) adds
the NIST Chemistry WebBook and a JCAMP-DX reader as its own build variant, with
one `RegisterDataSource` line and no framework file changed. A project embeds
its data, so fitting and reopening never need the network.

Free Pascal / Lazarus. Runs on Linux, Windows and macOS: installers for Linux and
Windows, one script for macOS.

**[dvmorozov.github.io/fit](https://dvmorozov.github.io/fit/)** — downloads, the
[user guide](https://dvmorozov.github.io/fit/guide.html), and the
[developers' pages](https://dvmorozov.github.io/fit/developers.html) for anyone
extending it.

## Two programs

Fit is a **client and a compute server**. The desktop client has no fitting engine
of its own; it talks to `fit_server` over HTTP+JSON — an API the server documents
itself, at `/docs`, which `./scripts/build-app.ps1 -Task api-docs` opens for you.
The installers hide all of it: what they put in the menu is a launcher, which
starts the server when nothing is answering and reuses one that is. Started by hand, the server goes first. The two
can run on the same machine or on different ones.

An optional Python sidecar adds the lmfit engine. Without it, the native Downhill
Simplex engine is used and nothing else changes.

## Get it

Prebuilt downloads for Linux (a portable archive, a `.deb` and an `.rpm`) and
Windows (an installer) are on the
[Releases](https://github.com/dvmorozov/fit/releases/latest) page. The packages
install the client, the compute server and a launcher that starts both. macOS is
built from source with one script.

To build it yourself:

```
./scripts/build-app.ps1
```

That checks the toolchain, builds both programs, runs the tests and writes an
archive into `dist/`. It needs Lazarus 3.0+, PowerShell 7 and the two sibling
repositories — see [building from source](docs/user-guide/building-from-source.md),
which also covers the Lazarus IDE route.

## Documentation

**Using it** — the user guide is inside the application, under
*Help → Explain Everything*, and the same text is published as the
[user guide](https://dvmorozov.github.io/fit/guide.html): every menu command, pane,
file and curve type. Its source is the explanation registry
(`Desktop/guide_*.pas`), so the program and the site cannot disagree. Setting up
the optional Python engine:
[Setting up the Python engine](https://dvmorozov.github.io/fit/guide-fitting.html#fitting-python-setup) ·
[building from source](docs/user-guide/building-from-source.md)

**Extending it** — [architecture](docs/contributing/architecture.md) ·
[module architecture](docs/contributing/module-architecture.md) ·
[writing a module](docs/contributing/writing-a-module.md) ·
[adding a curve model](docs/contributing/adding-a-curve-model.md) ·
[adding an axis mode](docs/contributing/adding-an-axis-mode.md) ·
[client and server](docs/contributing/client-server.md) ·
[loss functions](docs/contributing/loss-functions.md) ·
[findings](docs/contributing/findings.md)

Fit is becoming a framework for fitting any data to any class of models, for
research and for teaching: each field is a module that owns its interface, its
models, its rules and its computations, and everything it adds explains itself -
select a curve type or a row of the Model panel and the Explain pane says what it
is, what it rests on and what it does not cover.

A new curve type, data loader, optimiser, objective or whole analysis vertical is
added by **registration**: a directory plus one entry on a project's unit search
path. No framework file changes.
[`Modules/example-linear/`](Modules/example-linear/README.md) is a complete
working example in six files.

**What is registered, and every extension seam, is on the
[project site](https://dvmorozov.github.io/fit/architecture.html)** — generated
from the code on each publication, so it is never a description of an older
version. Nothing here restates it.

**[AGENTS.md](AGENTS.md)** states the invariants and the traps, for AI agents
working on this code.

## Contributing

This repository is published as a snapshot, so a pull request cannot be merged
directly. See [CONTRIBUTING.md](CONTRIBUTING.md) for what works instead.

Built on the sibling packages [fitgrids](https://github.com/dvmorozov/fitgrids)
and [fitminimizers](https://github.com/dvmorozov/fitminimizers).

GPLv3-or-later; see [LICENSE](LICENSE) and [THIRD-PARTY.md](THIRD-PARTY.md).
Documentation is CC BY 4.0.
