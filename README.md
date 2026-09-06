# Fit

An interactive curve-fitting application. Load a data set, place curves on it, and
fit them — one peak or a hundred, by hand or automatically. Save the whole session
as a project and reopen it later to carry on from where you stopped.

Free Pascal / Lazarus. Runs on Linux, Windows and macOS: installers for Linux and
Windows, one script for macOS.

**[dvmorozov.github.io/fit](https://dvmorozov.github.io/fit/)** — downloads, what
the application can do today, and the architecture for anyone extending it.

## Two programs

Fit is a **client and a compute server**. The desktop client has no fitting engine
of its own; it talks to `fit_server` over HTTP+JSON. The installers hide that: what
they put in the menu is a launcher, which starts the server when nothing is
answering and reuses one that is. Started by hand, the server goes first. The two
can run on the same machine or on different ones.

An optional Python sidecar adds the lmfit engine. Without it, the native Downhill
Simplex engine is used and nothing else changes.

## Get it

Prebuilt binaries for all three platforms are on the
[Releases](https://github.com/dvmorozov/fit/releases/latest) page — portable
archives, plus a `.deb` and an `.rpm` for Linux that install the client, the
compute server and a launcher that starts both.

To build it yourself:

```
./scripts/build-app.ps1
```

That checks the toolchain, builds both programs, runs the tests and writes an
archive into `dist/`. It needs Lazarus 3.0+, PowerShell 7 and the two sibling
repositories — see [building from source](docs/user-guide/building-from-source.md),
which also covers the Lazarus IDE route.

## Documentation

**Using it** — [building from source](docs/user-guide/building-from-source.md) ·
[the fitting workflow](docs/user-guide/fitting-workflow.md) ·
[project files](docs/user-guide/project-files.md) ·
[curve types](docs/user-guide/curve-types.md) ·
[user-defined curves](docs/user-guide/user-defined-curves.md) ·
[argument axes](docs/user-guide/argument-axes.md) ·
[compute backends](docs/user-guide/compute-backends.md) ·
[loss functions](docs/user-guide/loss-functions.md)

**Extending it** — [architecture](docs/contributing/architecture.md) ·
[writing a module](docs/contributing/writing-a-module.md) ·
[adding a curve model](docs/contributing/adding-a-curve-model.md) ·
[adding an argument axis](docs/contributing/adding-an-argument-axis.md) ·
[client and server](docs/contributing/client-server.md) ·
[loss functions](docs/contributing/loss-functions.md) ·
[findings](docs/contributing/findings.md)

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
