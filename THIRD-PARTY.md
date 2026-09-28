# Third-party components and licenses

This project bundles or builds upon the following third-party components. Each remains under its own
license; this file is provided for attribution and redistribution compliance (notably for installers and
any frozen binaries).

## Build / runtime (linked into the desktop app)

| Component | Role | License |
|-----------|------|---------|
| Free Pascal RTL / FCL | language runtime | LGPL with static-linking exception |
| Lazarus LCL (incl. TAChart) | GUI / charting | modified LGPL (LGPL + linking exception) |
| `Packages/TAGraph` — Philippe Martinole's 2005 TAChart, **forked and modified here** | charting (the `TTAChart` the desktop client draws on) | LGPL v2-or-later per the unit header; see the note below |

The LGPL-with-linking-exception terms of FPC/Lazarus permit distributing the application under the
project's GPLv3 license.

**About the bundled `Packages/TAGraph`.** It is a local fork, not a copy: the vertical fit-interval
bounds (`psVertLineBT` / `psVertLineTB`) and `InitShowLines` / `InitShowPoints` were added here.
The licensing inside it is inconsistent — `Package/TAGraph.pas` carries the author's LGPL v2-or-later
header ("GNU Library General Public License ... version 2 ... or any later version"), while
`Package/ta.lpk` records `<License Value="GPL"/>`. Under GPLv3 that mismatch is harmless, because
either reading permits distribution here; it matters only for a distribution under any other licence,
where LGPL's relinking obligations (v2 §6) or the `ta.lpk` reading would apply. Migrating to upstream
Lazarus TAChart, which is modified LGPL with the linking exception, removes the ambiguity — see the
charting-component entry in the internal roadmap. Note that modifying upstream TAChart itself is not
covered by the linking exception: changes to the component would have to be published under its own
licence.

## Compute sidecar (separate process — invoked at arm's length, not linked)

The optional Python compute sidecar uses, among others:

| Library | License |
|---------|---------|
| NumPy, SciPy, pandas | BSD-3-Clause |
| lmfit | BSD-3-Clause |
| pybaselines | BSD-3-Clause / MIT |

Because the sidecar runs as a **separate process** communicating over a defined protocol (not linked into
the GPLv3 application), these libraries are used under their own permissive licenses. When shipping a
frozen sidecar binary in an installer, include the corresponding license texts.

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
