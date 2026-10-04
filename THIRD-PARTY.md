# Third-party components and licenses

This project bundles or builds upon the following third-party components. Each remains under its own
license; this file is provided for attribution and redistribution compliance (notably for installers and
any frozen binaries).

## Build / runtime (linked into the desktop app)

| Component | Role | License |
|-----------|------|---------|
| Free Pascal RTL / FCL | language runtime | LGPL with static-linking exception |
| Lazarus LCL (incl. TAChart) | GUI / charting | modified LGPL (LGPL + linking exception) |
| [fitminimizers](https://github.com/dvmorozov/fitminimizers) | the downhill-simplex minimizer and its numerical helpers | MPL-2.0 |
| [fitgrids](https://github.com/dvmorozov/fitgrids) | the editable grids of the tables | MPL-2.0 |

The LGPL-with-linking-exception terms of FPC/Lazarus permit distributing the application under the
project's GPLv3 license. fitminimizers and fitgrids are the same author's sibling repositories, under
the Mozilla Public License 2.0, which is file-level: their files keep MPL-2.0 inside the application,
whatever its own licence. Every package carries the MPL-2.0 text beside the program as
`LICENSE-MPL-2.0`, and the About box says where their source is.

A product built on Fit with a licence of its own ships that licence as `LICENSE`, and Fit's GPLv3
text beside it as `LICENSE-Fit`.

**The chart is Lazarus's own TAChart** (`TAChartLazarusPkg`, part of the Lazarus distribution),
which the desktop client uses unmodified; what Fit adds to it is Fit's own code in
`Desktop/fit_chart.pas`, written against TAChart's extension points. Until 2026-09 the client drew on
`Packages/TAGraph`, a locally modified fork of Philippe Martinole's 2005 TAChart (LGPL v2-or-later
per its header, `GPL` per its package file); it was deleted, and each release that shipped it keeps
its source in the tag that release was built from. Modifying TAChart itself is not covered by the
linking exception: changes to the component would have to be published under its own licence, which
is why Fit's additions live in its own unit instead.

## Compute sidecar (separate process — invoked at arm's length, not linked)

The Python compute sidecar is built from `Worker/py/requirements.txt`, whose pins and
their dependencies are below. Every installer carries it **frozen** (PyInstaller, one folder,
`sidecar/` beside `fit_server`), so an installed copy needs no Python; a copy built from source
uses a Python environment instead.

| Library | License |
|---------|---------|
| Python (bundled in the frozen sidecar) | Python Software Foundation License |
| PyInstaller bootloader (the frozen sidecar's executable) | GPL-2.0 with the PyInstaller bootloader exception, which permits distributing it with any program |
| NumPy, SciPy | BSD-3-Clause |
| OpenBLAS, and the GCC Fortran and quadmath runtimes (bundled in the NumPy and SciPy wheels) | BSD-3-Clause; GPL-3.0 with the GCC Runtime Library Exception |
| lmfit | BSD-3-Clause |
| uncertainties, dill (pulled in by lmfit) | BSD-3-Clause |
| asteval (pulled in by lmfit) | MIT |

The test tools installed beside them (pytest and its dependencies, coverage) are not part of any
package.

Because the sidecar runs as a **separate process** communicating over a defined protocol (not linked into
the GPLv3 application), these libraries are used under their own permissive licenses. Each
installer ships their licence texts in `sidecar/licenses/`, collected from the frozen environment
by `scripts/sidecar-licences.py`, with an index in `sidecar/licenses/README.txt`. The freeze
leaves out the standard-library modules that would bring GNU readline, gdbm, Berkeley DB or Tk,
none of which the sidecar uses.

## Programs run, not shipped

| Program | Role | License |
|---------|------|---------|
| [curl](https://curl.se/) | every download from the internet (`Common/curl_client.pas`) | curl licence (MIT-style) |

The system's own curl is started as a separate program; a Linux package declares it as a dependency,
and macOS and Windows 10 1803 or later include it.

## Icons and cursors

`Accessories/` holds the source images the toolbar icons in `Desktop/Forms/form_main.lfm`
were built from - they are embedded in the form's `TImageList`, so the icons ship in
every binary whether or not the directory does.

| Component | Role | License |
|-----------|------|---------|
| [16x16 Free Toolbar Icons](http://www.small-icons.com/stock-icons/16x16-free-toolbar-icons.htm), Aha-Soft | toolbar icons | [CC BY 3.0 US](http://creativecommons.org/licenses/by/3.0/us/) |
| [16x16 Free Application Icons](http://www.small-icons.com/stock-icons/16x16-free-application-icons.htm), Aha-Soft | application icons | [CC BY 3.0 US](http://creativecommons.org/licenses/by/3.0/us/) |

Attribution: icons by [Aha-Soft](http://www.aha-soft.com/). Each directory keeps the
license text it shipped with.

## Retired

| Component | Status |
|-----------|--------|
| wst-0.5 (Web Service Toolkit) | removed; the XML-RPC transport it carried was replaced by HTTP+JSON |
| Ararat Synapse | removed with that transport; the bundled `Packages/synapse40` tree is gone |
| MathExpr (Windows-only shared library) | replaced by `Common/native_math_expr.pas`, which is cross-platform |

> Keep this file current as dependencies are added or removed (it is checked at each stage's checkpoint).
