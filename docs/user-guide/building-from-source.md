<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Building from source

Linux and Windows have prebuilt installers in every release. **macOS is built
from source** - the command is below, and it produces `Fit.app`. Build from
source also to modify Fit or to write a module.

There are two documented routes and they produce the same thing. The script is
what the project's own CI runs, so if it fails for you that is a bug rather than a
missing step.

## What you need

| Tool | Version | Notes |
|---|---|---|
| Lazarus | 3.0 or later | brings Free Pascal with it |
| Free Pascal | 3.2.2 or later | included in the Lazarus installers |
| PowerShell 7 | 7.4 or later | for the build script; only that |
| Python | 3.10 – 3.13 | **optional** — the lmfit compute sidecar |

Fit fits perfectly well without Python. The sidecar is an extra engine, not a
requirement.

### Installing them

```bash
# Debian / Ubuntu
sudo apt-get install lazarus
sudo snap install powershell --classic

# macOS
brew install powershell/tap/powershell
#  Lazarus itself: see the note below - it is not a brew install any more.

# Windows
choco install lazarus powershell-core
```

On **macOS** Lazarus is no longer installable with Homebrew: the cask was
deprecated in January 2025 and disabled a year later, and there has never been a
formula. Install the official release instead - which is what this repository's
`scripts/install-lazarus-macos.sh` does, picking the right archive for Apple
Silicon or Intel and installing the compiler package beside it. It lives in the
sources, so run it after the clone below:

```bash
./scripts/install-lazarus-macos.sh          # then add /Applications/lazarus to PATH
```

## Getting the sources

Fit needs **two sibling repositories checked out beside it**. Its projects
reference them by relative path, so the three directories must sit side by side:

```bash
mkdir fit-everything && cd fit-everything
git clone https://github.com/dvmorozov/fit
git clone https://github.com/dvmorozov/fitgrids
git clone https://github.com/dvmorozov/fitminimizers
```

```
fit-everything/
  fit/              this repository
  fitgrids/         grid components
  fitminimizers/    optimisation algorithms
```

## Route 1: the script

```bash
cd fit
./scripts/build-app.ps1
```

That is the whole build: it checks the toolchain, registers the three Lazarus
packages, builds the client and the compute server, runs the test suite and
writes what this operating system installs from into `dist/`.

Individual steps, when you want them:

```bash
./scripts/build-app.ps1 -Task check     # report the toolchain and stop
./scripts/build-app.ps1 -Task build     # client and server
./scripts/build-app.ps1 -Task test      # the test suite
./scripts/build-app.ps1 -Task test -Suite unit   # ...the fast half alone, seconds
./scripts/build-app.ps1 -Task package   # an installer for this OS
./scripts/build-app.ps1 -Task package -Install   # ...and install it here
```

`-Install` is a parameter, not a task: the tasks are the phases of the build,
and installing is what to do with what they produced. `-Task all -Install`
builds everything and installs it here.

Every build raises the build number in `Desktop/Fit.lpi` first, which is what the
Lazarus IDE does when it builds the same project - the binary then reports the
build it came from, in the window title and in Help > About. `lazbuild` does not
honour that setting, so the script does it. Two exceptions: an automated build
(`$env:CI`) never raises it, and `-NoVersionBump` builds the version the project
file already carries.

```bash
./scripts/build-app.ps1 -Task build -NoVersionBump
```

On Linux, `package` also builds a **`.deb` and an `.rpm`** when `dpkg-deb` and
`rpmbuild` are installed (`apt install rpm fakeroot`); without them it says so and
produces the archive alone.

Those packages install the client and the compute server into `/usr/lib/fit` and a
launcher as `/usr/bin/fit`. The launcher starts the server if nothing is answering
on port 8787 and then opens the client, so the desktop entry works on a fresh
install with no second step. The server stays running afterwards, so later starts
are immediate; its log is `~/.local/share/fit/server.log`.

**All three platforms follow one rule.** The Linux wrapper, `fit_launcher.exe`
and `Contents/MacOS/fit_launcher` inside `Fit.app` each probe `http://127.0.0.1:8787/health`, start a
server only when nothing answers, wait up to ten seconds for it to bind, and
leave a running one alone. `FIT_PORT` moves all of it to another port.

### macOS: the application bundle

```bash
./scripts/install-lazarus-macos.sh          # once; then add /Applications/lazarus to PATH
./scripts/build-app.ps1 -Task all -Install
```

`dist/Fit.app` holds the client (`Contents/MacOS/Fit`), the compute server and the
sample data, started by `Contents/MacOS/fit_launcher`; the install copies it to
`/Applications`, or `~/Applications` when that is not writable. Double-click it - the bundle starts the compute server itself and
reuses one already running. Its log is `~/Library/Logs/Fit/server.log`.

The bundle is unsigned, so the install strips the quarantine flag; without that
macOS calls a locally built app damaged. A `.dmg` is written beside it when
`hdiutil` is present.

Apple Silicon and Intel both build natively. Set `MACOS_SIGN_IDENTITY`, and
`MACOS_NOTARY_PROFILE` beside it, to sign and notarise; unset, both steps are
skipped.

**A plain `-Task build` also leaves something you can double-click**, at
`Desktop/o/<target>/Fit.app` - a bundle around the binary that was just built. On
macOS an application has to be in a bundle to get a menu bar and a Dock tile at all,
so this is how the client is started rather than an extra convenience; running the
bare executable gives a window with neither.

### Windows: the installer

`-Task package` writes `dist/Fit-windows-x86_64-setup.exe` when Inno Setup is
installed (`choco install innosetup` or `winget install JRSoftware.InnoSetup`);
without it the script says so and skips that step. `ISCC.exe` is looked for in
`$env:ISCC`, on `PATH`, under Program Files and under
`%LOCALAPPDATA%\Programs`, where winget puts a per-user install. The install is
per-user and needs no elevation. Its Start-menu entry runs `fit_launcher.exe`,
which starts the compute server and then the client. The desktop shortcut and
the `.fitproj` association are checkboxes, both ticked by default.
`WINDOWS_SIGN_CMD` signs the installer when set.

`setup.exe` shows the application's own icon: the installer reads
`Desktop/Fit.ico`, the file the client's icon is compiled from, so a new icon
reaches the installer on the next build with nothing else to change.

**Packaging other binaries under another name.** `-ClientBinary` and
`-ServerBinary` package binaries built elsewhere - an application with a module
in it. `-PackageName` is the package identity: the `.deb`/`.rpm` package name and
the Windows upgrade key, so keep it stable or an install lands beside the
previous one instead of over it. It also names everything a Linux package
installs (`/usr/lib/<name>`, `/usr/bin/<name>`, `<name>.desktop`, the icon,
`/usr/share/<name>`, `~/.local/share/<name>/server.log`) and the macOS bundle
identifier (`io.github.dvmorozov.<name>`), so two products never share a file.
`-PackageLabel` is the name people read - the installer title, the Start-menu
and Linux menu entries, the macOS bundle and its log folder - and, with its
spaces removed, the stem of the file names. Both default to Fit's own, `fit` and
`Fit`:

```powershell
./scripts/build-app.ps1 -Task package -ClientBinary <client> -ServerBinary <server> `
    -PackageName fit-pro -PackageLabel 'Fit Pro'
# dist/FitPro-windows-x86_64-setup.exe, titled "Fit Pro"
```

**If it says `lazbuild` or `fpc` was not found**, the tools are installed but not
on `PATH` — the Windows and macOS installers do not put them there. The script
looks in the places those installers use, including the compiler that ships
inside the Lazarus directory; if yours is somewhere else, point at it directly
with `$env:LAZBUILD` or `$env:FPC`.

**If it says a package was not found**, the sibling repositories are missing or in
the wrong place — see the layout above. `lazbuild` resolves `FitMinimizers`,
`FitGrids` and `ta` through its *package links*, not through a project's search
paths, which is why the script registers them on every build rather than in a
setup step you could skip.

## Route 2: the Lazarus IDE

Same result, more clicks, and the way to get a debugger.

1. **Register the three packages** — `Package ▸ Open Package File (.lpk)`, then
   *Use ▸ Install* is **not** needed; *Compile* is enough for all three:
   - `../fitminimizers/package/FitMinimizers.lpk`
   - `../fitgrids/package/FitGrids.lpk`
   - `Packages/TAGraph/Package/ta.lpk` — Fit's own locally modified TAChart fork.
     Register **this** one, not the TAChart that ships with Lazarus.
2. **Build the client** — `Project ▸ Open Project`, `Desktop/Fit.lpi`,
   then `Run ▸ Build`.
3. **Build the compute server** — open `Worker/fit_server.lpi` and build it too.
   It is a separate program: **the client has no engine of its own and needs the
   server running.**
4. **Run** — start `Worker/o/fit_server` first, then the client. Or open
   `fit.lpg` (a project group) to keep both projects in one IDE session.

The IDE builds the server with its own widgetset; the script passes
`--widgetset=nogui`, which is what a headless machine needs. Either works on a
desktop.

## Running the tests

```bash
./scripts/build-app.ps1 -Task test               # the whole suite
./scripts/build-app.ps1 -Task test -Suite unit   # the fast half, seconds
```

The suite has two halves, and which half a test is in depends on what it needs
rather than on how fast it is.

**Unit** tests need nothing outside their own process. The unit run therefore
builds no compute server and starts none - there is nothing for one to answer - so
it takes seconds. That is the one to run while you are changing something.

**Integration** tests start a compute server, speak HTTP, read data files and run
fits to convergence. They take a couple of minutes, and they are what tells you
the build actually works. `-Task test` and `-Task all` run both.

Some integration tests are **skipped rather than failed** when the Python sidecar
is not installed - they report as ignored and say why. That is expected on a
machine without it, and the unit half never touches the sidecar at all.

## The optional Python sidecar

Set it up once as [compute backends](compute-backends.md#one-time-setup)
describes: a virtual environment built from the pinned requirements. It is built
once **per machine** - in
`%LOCALAPPDATA%\Fit\py` or `~/.local/share/fit/py`, not in the checkout, so
every clone on the machine shares it and none of it is ever synced or copied.
`FIT_PY_HOME` overrides the location. See
[compute backends](compute-backends.md) for what goes in it and why it is not
installed system-wide.

Then choose **Fit → Minimizer → Levenberg-Marquardt (Python/lmfit)** in the client. With no sidecar the native
Downhill Simplex engine is used and nothing else changes.

## Adding a module

Extension is **static, at compile time**: a module is a directory plus one search
path entry. See [writing a module](../contributing/writing-a-module.md) and the
worked example in [`Modules/example-linear/`](../../Modules/example-linear/README.md),
which builds with:

```bash
lazbuild Modules/example-linear/Fit_example.lpi
```
