<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Architecture, and how to extend it

Fit began as a diffraction peak-fitting application. It is deliberately becoming
a **framework for fitting arbitrary data to arbitrary classes of models**, for
research and for teaching — every field is a module that owns its interface,
models, rules and computations and explains them, and the framework only hosts.
The technical-analysis pack is the second field, built almost entirely from parts
that already existed; diffraction is being moved out to become a module like it.
The contract a module is written against is
[module-architecture.md](module-architecture.md).

That is the measure this architecture is judged by. **The cost that matters is
not adding one more type today; it is what adding the *next* one costs someone
who was not part of the conversation that added the last one.** Everything below
exists to keep that cost low.

> Keeping this page current is part of the work, not a tidy-up afterwards. If
> you add or change an extension point, say so here in the same commit.

## Where the diagrams live

**The published diagrams are generated, and there is nothing to redraw.** Every
picture on <https://dvmorozov.github.io/fit/> is produced from this repository at
the moment the site is published, by the two generators below:

| Step | What it does |
|---|---|
| `scripts/dump-registries/` | An FPC console program. It links the registration front doors — `RegisterAppModules`, `RegisterAllCurveTypes`, `RegisterAllDataLoaders`, `RegisterAllMinimizers`, `RegisterBuiltInLosses`, `RegisterBuiltInActions` — and reads every registry back through the same public functions the application uses, printing the lot as JSON. |
| `scripts/gen-diagrams/` | Turns that JSON into Mermaid pages. Class hierarchies, which no registry can report, are parsed from the units. Standard-library Python only. |

`Invoke-Publish` runs both before the site snapshot and **fails the publish** if
either fails, so the diagrams can never be older than the code beside them.

**Why a program and not a script over the sources.** A registry is the only thing
that knows what is registered. Text matching gets it wrong in ways that matter:
the REST verbs go in through a local `Add` helper, so a naive pattern reports one
verb instead of fourteen; the registry tests register fakes, inflating every
count; and *which module directory was on the unit search path* — the entire
point of the module mechanism — leaves no trace a line match could find.

**Do not hand-edit a generated page** — the next publish overwrites it. To change
what a page says, change `scripts/gen-diagrams/`: the prose lives in
`content/*.html`, the look in `assets/style.css`, and the facts come from the
dump. A generated section is substituted into the prose at a `{{MARKER}}`
placeholder, and a missing placeholder is an error rather than a silently
dropped section.

**To look at the sites before publishing them**, the maintainers' publish
tooling has a preview step (*Preview sites*). It regenerates, assembles each site
the way a publish would, into `dist/preview/`, serves the lot on localhost and
opens them all. Ctrl+C stops it.

**Every file a site is made of is kept in a project, never only on `gh-pages`.**
A site is built (`New-SiteTree`) from three things: its repository's `site/`
folder - the static files its pages show and its host reads (pictures, the
picture its front page shows beside its name, `hero.svg`, the search-console
verification stub, the deploy stub) - the one icon every site
carries, `fit/site/favicon.ico`, the one design every site shares,
`fit/site/theme/` (Bootstrap 5 under MIT, its licence beside it, and `site.css`
with the palette - copied into each site as `theme/`), and the pages generated
from the code. The
`gh-pages` branches are what publishing *writes*, never what it reads. Built from
the branch, as it once was, every favicon and three screenshots existed only
there - one force-push from gone, with nothing in any project saying they
existed - and `dist/`, where everything is assembled, is swept by every clean.
So any site can be rebuilt from the projects at any time. A module product's site is
assembled the same way, from the module's own `site/` folder and the pages its
own generator writes - which the module's identity file names
(`build.siteBuilder`), so the framework names no module.

It is served over HTTP rather than opened as `file://` on purpose: the pages
import Mermaid as an ES module, and a module import from `file://` is blocked as
cross-origin, so every diagram would silently be missing.

**The sites are plain HTML and there is no site generator.** They ran
`jekyll-theme-dinky` with a copied-and-edited layout on top; the theme is gone and
so is Jekyll. Each page is a complete document, and a `.nojekyll` file tells
GitHub Pages to serve the tree untouched. That is worth the loss of
README-as-a-page: what is opened in a browser locally is byte-for-byte what gets
served, so checking the site needs nothing installed.

The generated pages are committed to `gh-pages`, which looks redundant and is not.
`pages.yml` deploys that branch as committed, without running the generator, so
what is committed is what is served. It is reached by a stub workflow on the
`gh-pages` branch (kept in `site/.github/workflows/pages.yml`), because a push
runs the workflows of the branch pushed and the real one lives on `main`.

**Every site is published with its code, and read back as the commit pushed.**
Publish builds all three sites before anything is pushed (`Get-SitePublications`),
then force-pushes each one as a `gh-pages` snapshot to its own repository
(`Publish-Sites`). A site whose project carries no deploy stub gets fit's, the
way it gets the icon. Its Pages are switched to the deploy workflow and allowed to
deploy `gh-pages` where they are not already (`Confirm-PagesFromWorkflow`). The
stub calls fit's workflow, which deploys whichever repository called it. Only
fit's site used to be pushed, and the check afterwards asked only whether a
`gh-pages` existed: fitgrids' and fitminimizers' code went out on every publish
while their 2020 Jekyll sites stayed up, and that check passed.

**Every published repository is a snapshot, not fit's alone.** The packages were
pushed by a plain `git push` of the developer's branch, so they published their
whole history, plus a `dev` branch and tags, while fit published one commit.
`Publish-TreeSnapshot` now pushes each package as one orphan commit of its
committed tree (`commit-tree` from `HEAD^{tree}`, so it is exactly what is
committed). A package counts as published only while its remote branch is one
commit of that tree (`Test-TreeSnapshotCurrent`), so a remote that holds the same
tree *with* its history is replaced on the next publish. `Get-RemoteRefCleanup`
and `Remove-RemoteRefs` clear every published repository of everything but its
snapshot branch, `gh-pages` and the tags published releases hang off. Forks made
before that keep their own copies of the history, which no push can reach.

Nothing on the page is rewritten at deploy time. The download links go through
GitHub's `/releases/latest/download/` redirect, so the site can be published
before, during or after a release and is never left pointing at a version that
does not exist. What that costs is a name the release must keep, and a release
that is complete before it is a release: `public-release.yml` uploads into a
DRAFT, and `verify-assets` publishes it only once every archive the download
table links is attached. `/releases/latest/` ignores drafts, so a release with a
failed platform stays invisible and the site goes on serving the last complete
one — rather than becoming "latest" with three dead links, which is what a
release published asset-by-asset does the moment its first job finishes.

The dumper asserts the seam count, the sibling generator asserts that every
component it documents still exists, and the fit generator asserts two more
things: that every class or interface named in a hand-composed figure is still
declared in the sources, and that each parsed hierarchy still contains its
anchors. Add or remove a seam, or rename a class out from under a picture, and
generation stops with a message naming it — rather than quietly publishing a
picture with one fewer box.

**The hand-drawn UMLet diagrams are gone.** `Design/*.uxf` and the exported PNGs
beside them were deleted once the generator covered what they said; git history
keeps them. Each topic has a generated equivalent, and none of them is a copy:

| What the UMLet diagrams showed | Where it is now |
|---|---|
| communication classes, call chain, server classes | the client-to-server call chain on `architecture.html` — the live REST path only, since the wst/SOAP proxies and the CGI client they drew are in no project file any more |
| the threaded-subclass notification sequences | *Watching a fit run* on `architecture.html` — the inline path only: `TFitServiceWithThread`, `TFitTaskWithThread`, `TFitServiceMultithreaded` and `TFitServerApp` are deleted, having had no caller since the engine moved behind REST |
| `FitViewer` and its extension | *The view seam* on `architecture.html` |
| the `TPointsSet` hierarchy and the curve-type registry | *The curve classes* on `how-to-extend-curve-types.html`, parsed from the units |
| configurable and user-defined curve types | *Curve types the user defines* on the same page, plus the two-dialog sequence |
| the data loaders | `how-to-extend-data-loaders.html` |

The PasDoc API reference that used to sit in `gh-pages:doc/` is gone too — 217
files, last regenerated in 2020, that nothing ever linked to.

Mermaid blocks in this file and the other `docs/` pages stay hand-written and are
rendered by GitHub. They describe decision structure and process flow, which is
reasoning rather than something a registry can report.

## The shape of the system

Three processes. The client holds **no fitting engine at all**.

```mermaid
flowchart TB
    UI["<b>Fit</b> — desktop client<br/><i>Desktop/Fit.lpi</i><br/>UI only, no engine"]
    SRV["<b>fit_server</b> — compute server<br/><i>Worker/fit_server.lpi</i><br/>the engine lives here"]
    PY["<b>Python sidecar</b><br/><i>Worker/py/fit_backend.py</i><br/>lmfit / scipy / numpy"]

    UI -- "HTTP + JSON<br/>the ONLY client-facing API" --> SRV
    SRV -- "child process it owns<br/>starts on demand" --> PY

    classDef client fill:#e8f0fe,stroke:#4285f4,color:#111
    classDef server fill:#e6f4ea,stroke:#34a853,color:#111
    classDef side fill:#fef7e0,stroke:#fbbc04,color:#111
    class UI client
    class SRV server
    class PY side
```

Two rules that are easy to break and expensive to unbreak:

- **`fit_server` is the only client-facing endpoint.** Every backend lives behind
  it. The desktop never talks to the sidecar.
- **The sidecar is owned by `fit_server`**, started as a child process when
  needed — not a service the client discovers. Its start is waited for inside the fit
  that needs it, shown and stoppable like the fit (see "The Python sidecar" in
  `client-server.md`).

### The network boundary: two transports, each where it belongs

Non-negotiable 11 in `AGENTS.md`.

```mermaid
flowchart LR
    UI["Fit<br/>desktop client"]
    SRV["fit_server"]
    PY["Python sidecar"]
    WC["TWebClient<br/><i>Desktop/DataSources/web_client.pas</i>"]
    CC["TCurlClient<br/><i>Common/curl_client.pas</i>"]
    CURL(["system curl<br/>child process"])
    NET(["a public service"])

    UI -- "fphttpclient, plain HTTP" --> SRV
    SRV -- "fphttpclient, plain HTTP" --> PY
    UI --> WC --> CC --> CURL -- "https, the OS's TLS" --> NET

    classDef local fill:#e6f4ea,stroke:#34a853,color:#111
    classDef web fill:#e8f0fe,stroke:#4285f4,color:#111
    class UI,SRV,PY local
    class WC,CC,CURL web
```

- **The internet:** `TWebClient` (the size cap, the cancel, every message a user
  reads) over `TCurlClient`, which runs `curl`: `/usr/bin/curl` on macOS,
  `%SystemRoot%\System32\curl.exe` on Windows (shipped since Windows 10 1803),
  otherwise the first `curl` on `PATH`; the Linux packages depend on `curl`. It
  uses the operating system's TLS and certificate store and verifies
  certificates; it honours `http_proxy`, `https_proxy` and `no_proxy`, and
  ignores `~/.curlrc`.
- **Between this program's processes:** `fphttpclient`, plain HTTP - client to
  `fit_server` (loopback by default, or whatever host the user sets), `fit_server`
  to the sidecar or to another `fit_server`, and the launcher waiting for the
  server. These calls are frequent; a process per call would only add latency.
- **No unit uses `opensslsockets`.** `tools/build-tests/network_boundary.tests.ps1`
  fails on one that does, and on any production unit outside a short allow-list
  that uses `fphttpclient`.
- **A peer that has gone away is an error, not the end of the process.** Every
  program calls `broken_pipes.IgnoreBrokenPipes` first in its main, so a write to
  a reset peer fails with EPIPE - which `Fetch` recovers from by reconnecting -
  instead of raising SIGPIPE. One mechanism, chosen because it covers every
  case: a per-socket option cannot reach a server's socket before a gone client
  has reset it on macOS, where the signal also goes to the whole process. The
  same script test refuses a program, the test programs included, that does not
  call it.

*Why.* Free Pascal loads OpenSSL by name at the first https request, and 3.2.2
knows no OpenSSL 3 name. On macOS it loaded Apple's legacy 0.9.8, every public
service refused the handshake, and the socket layer reported that as a refused
connection (see [findings](findings.md)). Bundling OpenSSL 3 was rejected: a copy
per architecture, rewritten install names, re-signing, and Free Pascal 3.2.2
against OpenSSL 3 unproven.

### Updates: one request, verified before anything runs

```mermaid
sequenceDiagram
    participant W as Fit (Help > Check for Updates,<br/>or the start-up check)
    participant C as TUpdateChecker<br/><i>Desktop/update_check.pas</i>
    participant D as DecideUpdate<br/><i>Common/update_feed.pas</i>
    participant F as the feed<br/>(latest.json)
    participant H as IUpdateHost<br/>(the window's dialogs)
    participant S as fit_server
    W->>C: Run(Asked)
    C->>F: GET latest.json (TWebClient, over curl)
    C->>D: manifest, version, platform, install_channel stamp, skipped, gates
    D-->>C: up to date / install / managed elsewhere / no installer / held back
    C->>H: AskToInstall(version, notes)
    H-->>C: Later / Skip This Version / Install
    C->>F: download the installer by its exact name
    C->>C: SHA-256 against the feed's digest - a mismatch deletes it
    C->>H: Install(file)
    H->>S: POST /shutdown (loopback only)
    H->>H: hand the file to the system's installer, then quit
```

- **Everything is decided in `update_feed`**, a unit that touches nothing: the
  manifest, the four-part version comparison, the installer by exact name, the
  six-hour cooldown, the skipped version, and the stamp. The checker only fetches,
  verifies and asks; the host only shows dialogs and starts processes.
- **The stamp decides who updates a copy.** Packaging writes `install_channel`
  beside the program: `direct` for the program's own installers, `portable` for
  the archive, a store's name and update command for a store's build. A copy with
  no stamp was built from source and is only told that a version exists.
- **A module may hold a release back** through `RegisterUpdateGate`: a gate sees the
  new version and its date and answers with the reason it may not be installed.
  A module that makes the build a product of its own names that product and its
  feed with `UseUpdateSource`; an empty feed means the product has none yet.
- **The server is stopped by asking it**, `POST /shutdown`, which it honours only
  from loopback - the installer never has to kill it (the Windows uninstaller asks
  the same way before it falls back to `taskkill`).
- **No identifier is sent.** The request is the feed URL, nothing else.

## Inside the server: the fit path

```mermaid
flowchart LR
    REST["REST API<br/><i>Worker/fit_rest_api.pas</i>"]
    SVC["TFitService<br/><i>Server/fit_service.pas</i>"]
    TASK["TFitTask<br/><i>Server/fit_task.pas</i><br/>sums curves, evaluates the objective"]
    BE{{"IFitBackend<br/><i>Server/interfaces/int_fit_backend.pas</i><br/><b>the compute seam</b>"}}
    NAT["TNativeFitBackend<br/>Downhill Simplex, in-process"]
    PYB["TPythonFitBackend<br/>→ sidecar"]
    REM["TServerFitBackend<br/>→ another fit_server"]

    REST --> SVC --> TASK --> BE
    BE --> NAT
    BE --> PYB
    BE --> REM
```

**One task is one fit interval.** `TFitService.CreateTasks` builds a sub-task per
selected interval and hands it only that stretch of the profile, so the intervals
need no further machinery downstream: everything a task measures is, by
construction, measured over its interval. The service then pools the parts into
one figure. See [`loss-functions.md`](loss-functions.md) § Which points a figure
covers.

**Input and result are separate sets, and that is load-bearing.** The picked
curve positions are model input: unique x values that name real samples, each one
the seed its curve is rebuilt from, and each one carrying the **handle** that
says which instance it stands for - so that curve's fitted parameters can be
handed back to it after a model edit rebuilds everything. Where the curves
*ended up* is a different statement, so it is a different set -
`CreateResultedCurvePositions` derives it from the collected curves and nothing
reads it back.

```mermaid
flowchart LR
    PICKS["picked positions<br/><i>FCurvePositions</i><br/>unique x, on the sample grid"]
    BACK["background element<br/><i>FBackgroundCurveTypeId</i><br/>one curve type, or none"]
    TASKS["CreateTasks + RecreateCurves<br/>peaks first, the background last"]
    SLOT["background slot per interval<br/><i>curve_identity_registry</i>"]
    REST2["RestoreCurveValues<br/>hands back the previous fit<br/>by instance handle"]
    CURVES["built curves<br/><i>FCurves</i>"]
    OUT["fitted positions<br/><i>FResultedCurvePositions</i><br/>derived, read-only"]

    PICKS -->|seeds| TASKS
    BACK -->|one per fit interval,<br/>seeded from the baseline| TASKS
    SLOT -.->|its handle| TASKS
    TASKS --> REST2 --> CURVES
    CURVES -->|one per instance| OUT
    PICKS -.->|never written by a fit| PICKS
```

This is what makes fit → edit → fit work: the pick carries a handle issued once
and kept, so the previous round's values are found again whatever else changed.

**The background is model input too, of a second kind.** It has no pick: the
model names ONE background curve type, and every fit interval gets an instance
of it, built after the peaks and kept last in its task. Its handle is issued to
the interval's *background slot* - never the formula curve's slot of the same
interval, which is also positionless and one per interval - so its fitted values
come back to it on every rebuild, through a project file too. Nothing in the
engine names a background type: `TNamedPointsSet.IsBackground` is what every rule
asks (curve reduction passes it by, the shared-parameter write skips it, the
peak-type menu leaves it out). The profile is never rewritten for it; only the
user's own *Subtract* command does that. See the roadmap's "Background element".

The handle is ISSUED, not derived, and that is the whole point. It used to be a
hash of the instance's initial parameter values, which meant moving a pick
changed the key and orphaned everything stored under it - so the move was
refused. A move now rekeys instead: the curve keeps the shape the optimiser
found and is re-seeded where the user put it. See
[`curve_identity_registry.pas`](../../Server/curve_identity_registry.pas), and
`Server/fit_advice.pas` for the one move still refused - a module's markup
places every instance at once, so there is no correspondence to carry.

`IFitBackend` is coarse on purpose: **one call performs one whole fit**. That is
what lets a backend be in-process, a subprocess, or a machine across the network
without the fit path knowing which.

The wire contract (`Worker/fit_problem_json.pas`) is deliberately **engine-free
plain records**, so it can be tested in isolation and marshalled across a process
boundary. `Server/fit_task_marshalling.pas` maps both ways.

### Fit intervals side by side

The tasks are independent by construction, so `TFitService.RunTasks` fits them
at once (`Server/job_pool.pas`): at most one worker per processor - more only
take turns - and never more than the tasks, or what the `fitThreads` setting
allows. **One worker is the old loop exactly**, on the request thread, each task
ending through its own `Done`; the two must reach the same model bit for bit,
and `testcase_parallel_fit` and the module's golden fit compare them.

```mermaid
flowchart TB
    REQ["request thread<br/>holds Session.Lock<br/><b>the coordinator</b>"]
    PRE["before: each task's loss parts<br/>and frame, published"]
    POOL["RunJobs<br/>the caller is one worker<br/>longest task first"]
    W1["worker: task i<br/>reports through its own slot"]
    W2["worker: task j"]
    SLOT["FProgressLock<br/>slots summed into one R-factor<br/>frames assembled from copies"]
    DONE["after the join: DoneProc, once<br/>collect, remember, adopt removals"]

    REQ --> PRE --> POOL
    POOL --> W1 & W2
    W1 & W2 -->|on each improvement| SLOT
    POOL -->|every task finished| DONE
```

**An evaluation takes no shared lock.** A worker touches shared state only when
its task improves: it computes its own loss parts outside the lock, then under
`FProgressLock` replaces its slot, sums the slots into the same pooled R-factor
`GetTotalRFactor` computes, and - when a frame is due - copies *its own* curves
and slice into its slot and assembles the frame from the copies. No worker reads
another task's live objects, and `FCurves` / `FCalcProfile` are written only by
`DoneProc`.

**The step ends through `DoneProc` once**, after the join - what the last task's
`Done` did when they ran in a row. A step, not the run: the automatic run is
several steps, and the next one starts from what this one collected
(`TAutoDecompositionTest` fails without it). Stop is unchanged: every task's
flag is set, each worker ends the cycle it is in, and the join still collects
and measures the whole model.

**Lock order**, only ever taken downward:

`Session.Lock` (the coordinator) → `FTaskLock` → a task's `FMinimizerLock` →
`FProgressLock` → the progress log's lock → the identity registry's lock →
`LogCS`.

No worker reaches `Session.Lock`, which the coordinator holds while it waits; no
callback other than `WriteLog` is made under a lock; the registry and `LogCS`
call out to nothing. What else a worker shares is per thread or read-only: the
formula parser is a `threadvar` (`native_math_expr`, freed as each worker ends),
each worker starts with the caller's floating-point state, and the first
exception is raised by the coordinator after the join. Only an engine in this
process runs side by side - any minimizer kind that needs no sidecar, with no
compute server configured; a backend in another process gets one interval at a
time, as before (`ParallelFitPossible`).

### The module boundary

Every arrow crosses from the module to the framework. There is none the other way,
and that is the whole design: the framework can be built, tested and published
without any module existing.

```mermaid
flowchart LR
    subgraph FW["framework (published)"]
        direction TB
        AM["Common/app_modules.pas<br/><i>stub: registers nothing</i>"]
        MT["tests/no-modules/module_tests.pas<br/><i>stub: links nothing</i>"]
        REG["registries<br/>curve types · loaders · minimizers<br/>losses · actions · UI · modules"]
        ENG["engine + service + REST<br/><i>names no module</i>"]
        HOST["hosts: Fit.lpr · fit_server.lpr<br/>call RegisterAppModules"]
    end
    subgraph MOD["a module (its own directory, possibly its own repository)"]
        direction TB
        MAM["its app_modules.pas"]
        MMT["its module_tests.pas"]
        DOOR["its front door<br/>RegisterXModule"]
        SRC["its curve types, sessions,<br/>resources, UI, routes, tests"]
    end

    MAM -. "overrides, by search-path order" .-> AM
    MMT -. "overrides, by search-path order" .-> MT
    HOST --> AM
    MAM --> DOOR
    MMT --> SRC
    DOOR --> SRC
    DOOR --> REG
    SRC --> REG
    REG --> ENG

    classDef fw fill:#eef5ff,stroke:#4a76b8;
    classDef mod fill:#fff6e6,stroke:#c88a2a;
    class AM,MT,REG,ENG,HOST fw;
    class MAM,MMT,DOOR,SRC mod;
```

A build with no module links the two stubs, registers nothing extra, and every
module path becomes a no-op. Removing a module's directory from the search path
removes the module from that binary, and removes it completely.

**Everything registered explains itself.** A curve type answers
`TNamedPointsSet.Explanation`; a module registers an `IExplanationProvider` for its
own namespace of topics (rules, verdicts, the entries of its row menu). The
window's Explain pane, Help ▸ Explain Everything, the published explanations page
and the generated user guide all read the same registry
(`Common/explanation_registry.pas`), and `ExplanationFindings` over it is what a
completeness test asserts empty. The HTML view the pane draws with is reached
through `int_explain_view`, because the component cannot build against the
headless widget set the test suite links the main form with.

```mermaid
flowchart LR
    CT["<b>TNamedPointsSet.Explanation</b><br/><i>every curve type</i>"]
    CTP["CurveTypeExplanationProvider<br/><i>Desktop/ModelCurves/curve_type_explanations.pas</i>"]
    MP["a module's IExplanationProvider<br/><i>rules, verdicts, row menu entries</i>"]
    REG["<b>explanation registry</b><br/><i>Common/explanation_registry.pas</i><br/>RegisterExplanationProvider · FindExplanation"]
    FOC["FocusShownTopic<br/><i>Desktop/explanation_focus.pas</i><br/>row, curve type or hovered entry"]
    HTML["ExplanationHtml<br/><i>Desktop/explanation_html.pas</i>"]
    PANE["Explain pane<br/><i>CreateExplainView</i>"]
    ALL["Help ▸ Explain Everything"]
    DUMP["dump_registries → generated guide"]
    TEST["ExplanationFindings<br/>completeness tests"]

    CT --> CTP --> REG
    MP --> REG
    FOC --> REG
    REG --> HTML --> PANE
    REG --> ALL
    REG --> DUMP
    REG --> TEST

    classDef fw fill:#eef5ff,stroke:#4a76b8;
    classDef mod fill:#fff6e6,stroke:#c88a2a;
    class CTP,REG,FOC,HTML,PANE,ALL,DUMP,TEST fw;
    class CT,MP mod;
```

Every surface reads the registry and none reads a provider directly, so a topic
the pane can show is one the guide prints and the completeness test walks.

**Known gap: a curve's type is read back from its title.** The Model panel needs
each placed curve's type to explain its row, but no per-curve type crosses the
wire - `curveType` is the model's *selected* type, and a model may hold curves of
several. The engine titles every curve `<type name> [<n>]`, so the client reads
the name back (`model_outline.CurveTypeNameOfTitle`) and resolves it by name. A
curve renamed by the user explains nothing - or, if its new title begins with
another type's name, explains that type. A module
that owns its rows avoids this by carrying the type itself - the technical-analysis
pack's overlay sends each pattern's `curveTypeId` - and a per-curve type field on the framework's
curve list would retire the workaround.


## The extension points

| Extension point | Add one by | Guide |
|---|---|---|
| **Module** (a whole vertical) | A directory + one registration unit + one search-path entry | [writing a module](writing-a-module.md), [`Modules/example-linear/`](../../Modules/example-linear/README.md) |
| **Curve / lineshape model** | Subclassing `TNamedPointsSet`, self-registering in `initialization` | [adding a curve model](adding-a-curve-model.md) |
| **Axis mode** (a way of showing the argument or the value) | Subclassing `TAxisMode` and registering it; presentational only | [adding an axis mode](adding-an-axis-mode.md) |
| **Data loader** | Implementing `int_data_loader` and registering it in the loader registry | `Desktop/DataLoaders/dat_file_loader.pas` in the framework; `Modules/open-data-spectra/jcamp_dx_loader.pas` registered by a module |
| **Data source** (where a file is fetched from) | Subclassing `TDataSource`, declaring what it asks and what it produces, and registering it | `Desktop/DataSources/`, [module architecture](module-architecture.md) |
| **Compute backend / transport** | Implementing `IFitBackend` and registering it | [client/server](client-server.md) |
| **Minimizer** | Declaring a `TMinimizerInfo` that says what it needs (`Server/minimizer_declarations.pas`, which the client uses for its menu), then binding its backend by kind on the server (`BindMinimizerBackend`, `Server/minimizer_registration.pas`) - the client links no backend | `Server/minimizer_registration.pas` |
| **Loss function** | Registering a loss that declares its own compatibility facts | [loss functions](loss-functions.md) |
| **REST action** | Registering a `TActionInfo` — also the scripting surface | `Worker/action_registry.pas` |
| **UI menu, Tools-pane buttons, panel, pick mode** | Implementing `IUiModule`, declaring the menu as data | [writing a module](writing-a-module.md) |
| **Context menu over a Model row** | `IUiModule.RowMenuItems`, built as the menu opens, entries nested and greyed with a topic. Over a module's rows its owner is asked; over the framework's rows every module is, with the curve's handle as the row id, and the first answer owns the click (`module_menu.RowMenuModulesToAsk`) | [module architecture](module-architecture.md) |
| **Explanation** of anything a user meets | `TNamedPointsSet.Explanation` for a curve type; an `IExplanationProvider` registered with `RegisterExplanationProvider` for everything else | [module architecture](module-architecture.md) |
| **Sidecar route** | `@routes.get` / `@routes.post` in `<name>_routes.py`, in the module's own `Worker/py` | `Worker/py/routes.py`, `fit_backend.load_module_routes` |

Every one of these is a registration call. **Adding any of them requires no edit
to an existing file** — which is the property the whole arrangement exists to
have, and the one a reviewer should check first.

### Where data comes from, end to end

Fetching happens in the **client**. The server learns nothing about data sources:
what reaches it is a profile, through the verb that has always carried one.

```mermaid
flowchart LR
    subgraph Client["Desktop client"]
        W["data_source_wizard<br/>steps derived from what<br/>the source declared"]
        S["a registered TDataSource<br/>(framework's own, or a module's)"]
        WC["web_client<br/>one method reaches the network"]
        CU(["system curl<br/>(TCurlClient)"])
        C["download_cache<br/>per-user data directory"]
        L["data_loader_registry<br/>picks the reader by extension"]
        I["data_source_import<br/>ask, new project, import, record origin"]
        FC["TFitClient.LoadDataSet"]
    end
    subgraph Server["fit_server"]
        P["PUT /problems/{id}/profile"]
    end
    Net(["a public service"])

    W --> S --> WC --> CU --> Net
    WC --> C --> L --> W
    W --> I --> FC --> P
```

Two properties are worth stating because they are what the arrangement buys:

* **the preview is the import** — the file is downloaded once, read by the very
  loader the import will use, and Create Project fetches nothing again;
* **a project needs no network to reopen** — it stores the profile, and the
  cached file is what *Reload profile* and the recorded origin point at.

### The one idea that makes this cheap: capabilities, not enumeration

An extension states **facts about itself**; a single central rule derives what
those facts imply. It never enumerates its compatibility with every other
feature.

```mermaid
flowchart TB
    subgraph BAD ["✗ Enumeration — N edits per new feature"]
        direction TB
        B1["add a 5th loss function"] --> B2["revisit EVERY curve type"]
        B2 --> B3["any author who forgets<br/>silently claims support<br/>that does not work"]
    end

    subgraph GOOD ["✓ Capabilities — one edit, ever"]
        direction TB
        G1["curve declares:<br/><code>AmplitudeIsUnbounded</code><br/><code>IsAnalytic</code>"]
        G2["loss declares:<br/><code>LossIsSelfNormalising</code><br/><code>LossIsLeastSquares</code>"]
        G3["ONE central rule derives<br/>what is allowed"]
        G1 --> G3
        G2 --> G3
        G3 --> G4["every existing AND future<br/>type classified correctly,<br/>with no edits"]
    end
```

Worked examples in the codebase:

- `TNamedPointsSet.IsAnalytic` — "do I have a closed form?" → drives whether the
  formula-based backends can be used at all.
- `TNamedPointsSet.AmplitudeIsUnbounded` — "can my amplitude grow freely?" →
  drives which objectives are legitimate (`Server/loss_compatibility.pas`).
- `LossIsLeastSquares` — "am I a sum of squares?" → drives which engines can
  minimise me.

**The discipline that keeps it from bloating:** a capability describes a
*property of the model*, never a preference or a named special case; and a new
capability is introduced when a **second** real case needs it, not in
anticipation. Speculative vocabularies are how capability models turn into worse
enumerations.

### The corollary: a refusal must explain itself

Deriving compatibility means the app will sometimes override what the user chose.
Every such correction is sound — and invisible, which makes it
indistinguishable from a bug the moment someone notices the result does not match
their selection.

So **the decision and its explanation are the same code**:
`Server/fit_advice.pas` is called by the engine *and* by the UI. A separate UI
copy would drift, and a UI that confidently explains something the engine no
longer does is worse than silence, because it would be believed.

```mermaid
flowchart LR
    ADV["<b>AdviseFit</b><br/><i>Server/fit_advice.pas</i><br/>decides AND explains"]
    E1["TFitTask.EnforceLossCompatibility"]
    E2["TFitTask.Optimization<br/>(backend choice)"]
    U1["status bar — always"]
    U2["dialog — only when<br/>the reason changes"]
    U3["menu tooltips"]

    ADV --> E1
    ADV --> E2
    ADV --> U1
    ADV --> U2
    ADV --> U3
```

If you add a capability-derived refusal, route its user-facing explanation
through this unit rather than inventing your own.

## Extend, do not bypass

The architecture is only cheap to extend if extensions go *through* it. A parallel
channel is worse than a missing feature: it duplicates the truth, drifts from it,
and hides the real defect behind something that looks like progress.

**Before adding any unit, verb, wire contract or record, find how the app already
does the analogous thing.** For anything crossing the client/server boundary, read
`Desktop/http_fit_service.pas`'s implementation of the nearest existing verb — that
is where the truth about what actually reaches the client lives, and it is not
always what the server appears to send.

Two tells that you are building a bypass:

- **it needs a join key** back to an existing contract — then it is probably a
  bypass *of* that contract, and the original should be extended instead;
- **it works in tests but not in the app** — usually because the tests exercise
  in-process objects while the real path goes over HTTP and carries less.

Real example, kept because it cost the most: a module's per-curve metadata was thought to need
a new wire contract. `GET /curves` had always carried every curve's parameters,
and the client had always rebuilt them. The whole gap was that `value` is a JSON
number, so a GUID-valued parameter arrived as `0`. One field — `kind`, saying
what `value` holds, mirroring the `error` field beside it.

The same lesson decided where a curve's own handle went when instance identity
was reworked: **not** into a parameter. A parameter is a quantity of the model,
and `value` is a number; the handle is a handle to the object, so it is its own
field beside `params`.

### Worked example: the model history is a capture and a restore, kept

The History tab records every model a run reached and makes any of them current
again. It needed **no new server verb for the history itself**, because the
project file already had both halves: `CaptureProject` reads a live problem into
a `TProjectDocument` through the same `IFitService` verbs the user's gestures use,
and `ApplyProject` puts one back in the order `fit_project_restore` plans. A
history entry is that document's model half (`Common/model_history.pas`), stored as
parts of the same ZIP container (`Common/model_history_json.pas`), and making one
current *is* the restore Open Project does. A history-specific restore, written
verb by verb, would have been correct on the day it was written and wrong the
first time the restore order changed.

The one thing the engine lacked was **measuring a model it had not fitted**. Only a
fit measured anything, so a model put back by value - a reopened project then, a
history entry now - read "Not calculated". `POST /actions/evaluate-model`
(`IFitService.EvaluateModel`) has every interval measure what it holds, nothing
optimised, and the restore plan ends with it (`rsEvaluate`) for a model that was
measured when saved.

```mermaid
sequenceDiagram
    participant U as User
    participant W as TFormMain
    participant C as TFitClient
    participant D as TProjectWorkflow
    participant S as fit_server
    U->>W: Fit
    W->>C: MinimizeDifference
    C->>S: POST /actions/minimize-difference
    S-->>C: done (every stage)
    C->>C: Done: refresh the window
    C->>W: OnFitRunEnded(kind, stopped)
    W->>D: RecordRun
    D->>S: CaptureProject (GET ...)
    D->>D: TModelHistory.Add (parent = current)
    U->>W: double-click a History row
    W->>D: MakeHistoryEntryCurrent(id)
    D->>S: CaptureProject - keep unrecorded edits
    D->>S: ApplyProject (PUT profile ... PUT /curves)
    D->>S: POST /actions/evaluate-model
    D->>W: RefreshFromEngine
```

**Recorded on the client, once per run.** The engine's `DoneProc` ends every
*stage* - an automatic run has two - so a history kept there would hold a half-way
model for every press of Automatically. `TFitClient.Done` runs once per run the
user started, which is the unit the history is made of.

**Two models are the same model** when their inputs and fitted values are equal
(`ModelContentHash`): the R-factor and the statistics are measured *from* those and
may differ in the last bit after a restore, so they are left out - or a model made
current would compare unequal to its own entry and be recorded again as an edit
nobody made.

## Invariants worth knowing before you change anything

These are the ones that are expensive to rediscover. Each is pinned by a test
named after it.

| Invariant | Where | Why it matters |
|---|---|---|
| The client contains no engine | `Desktop/` | `strings Fit \| grep -x TFitTask` must find nothing |
| `fit_server` is the only client-facing endpoint | `Worker/` | Backends stay swappable |
| R-factor bounds define **independent** sub-problems | `Server/fit_task.pas` | Intervals may not overlap — this is what will allow parallel fitting |
| A curve position's `y` seeds the amplitude | `RecreateCurves` | Dropping it starts a fit from zero amplitude that never converges |
| Statistics use a fixed residual, whatever was minimised | `fit_statistics` | Otherwise χ² and AIC/BIC stop being comparable |
| A component of an additive model is exactly zero outside its support | `Modules/example-linear/linear_points_set.pas` | The additive sum is meaningless otherwise |
| Nothing reads pixels back off a canvas | `Desktop/fit_chart.pas` | One blocking X round trip per pixel; see below |
| A menu item is never freed by its own `OnClick` | `Desktop/Forms/form_main.pas` | The widgetset is still holding it — see below |
| Every process logs by default, with no switch | `Common/log.pas` | A fault has to be readable from the log the run already wrote |
| A run is recorded in the model history by the client, once per run | `TFitClient.Done` → `OnFitRunEnded` | The engine's `DoneProc` ends every stage; recorded there, an automatic run leaves a half-way model behind |
| A model history entry is restored by `ApplyProject`, never verb by verb | `Desktop/model_history_session.pas` | The restore order is data in `fit_project_restore`; a second restore drifts from it |

### Drawing: no read-back, and bands go first

The chart is Lazarus's TAChart, with what Fit adds to it in `Desktop/fit_chart.pas`
(the unit header lists each addition and why TAChart's own does not serve). Series
that paint an *area behind* the data — the fit-interval band and the
selected-points band — say so through `TFitSerie.IsBackgroundBand`, and each
series writes its layer into TAChart's `ZPosition`: bands, then data, then a
highlighted curve, each layer in the order its series were added (TAChart sorts by
`ZPosition` unstably, so the add order is written in too). Nothing enumerates which
series precedes which; each is asked.

That ordering exists to replace something worse. Both bands used to be painted a
pixel at a time, reading each pixel back (`Canvas.Pixels[x,y] = GraphBrush.Color`)
so the hatch would skip pixels a curve already occupied. On Windows a pixel read
is a local GDI call. On X11 it is `gdk_drawable_get_image(d, x, y, 1, 1)` — a
synchronous round trip to the X server, with the process blocked on the reply.
Over a band spanning the plot that is hundreds of thousands of round trips per
repaint, and since a fit interval defaults to the whole profile, it was every
repaint. It is why every operation in the application lagged for seconds, while
the server log showed nothing above 1 ms.

The hatch is now drawn as what it always was — the pixels where `(x±y) mod 16 = 0`
are a family of parallel diagonals sixteen pixels apart, anchored to the canvas so
the pattern does not crawl with the band — as lines clipped arithmetically, so the
result is identical on every widgetset. **Do not reintroduce a canvas read-back**, and be wary of any drawing
whose cost scales with the pixel count rather than the data.

### Drawing: a curve's line is its value plus what it rests on

A model is a sum of curves, and a curve whose contribution is a deviation from
others - a nested component - is drawn where it belongs, not where it sums. The
curve type answers `TNamedPointsSet.DrawnBaselineIn(AModel)`; the server asks it
in one place (`TFitService.CurveDrawnBaseline`) and sends it as the optional
`baseline` array of the points DTO (`TPointsData.Baseline`), on
`GET /curves/{cid}/points` and in every animated frame alike - both through the
one converter `wire_point_sets.PointsDataOf`. Like `ids` beside it, the field is
absent when there is none, so every existing reply is byte-identical, and the
reader refuses one out of step with the points. The client keeps it on the
curve (`TTitlePointsSet.FDrawnBaseline`, set by `NamedCurveOf`), and
`TFitViewer.PlotPointsSet` draws `DrawnYOf` - value + baseline. Nothing else
reads it: the fit, the residual, the tables and the statistics use the values.

### Colour: one palette in force, and every painter resolves against it

Everything the application paints itself takes its colours from the palette
`Desktop/app_theme.pas` makes current: a light and a dark `TThemePalette`, one
chosen by `TThemeController` from View > Theme and, under Follow System, the
system's appearance (`system_appearance`, behind one function). Nothing keeps a
colour it computed; each painter keeps where its colour comes from and resolves
it again when the controller says the palette changed.

```mermaid
flowchart LR
    Menu["View > Theme"] --> Ctl[TThemeController]
    Sys["system appearance<br/>(ThemeServices.OnThemeChange)"] --> Ctl
    Set["settings.json app.Theme"] <--> Ctl
    Ctl -->|UseDarkPalette| Pal[CurrentPalette]
    Ctl -->|OnChange| Win["TFormMain.ApplyTheme"]
    Win --> Chart["TFitChart.ApplyPalette<br/>each series' TSeriesColorSource"]
    Win --> Html["RepaintExplainViews<br/>every live HTML pane"]
    Win --> Rep["DrawReport<br/>report markup built again"]
    Win --> Grid["grid stripes and tints"]
    Pal -.read by.-> Chart
    Pal -.read by.-> Html
    Pal -.read by.-> Rep
```

Three rules hold it together. A series keeps a **source** (a role, a module's
colour, or fixed) rather than a colour, so a repaint resolves every source again
instead of rebuilding series or keeping a second mapping. An HTML pane is given
the palette as it is given a zoom - a colouring, not a page - so the
changed-only wrapper that stops the same page being re-sent still holds. And a
colour the framework does not choose goes through `ReadableOn`, never through a
table naming who chose it. `testcase_app_theme` walks both palettes against the
WCAG contrast floor, so the next colour added that cannot be read fails by name.

### Menus: rebuild from the main loop, never from a click

`CreateCurveTypeMenus` starts by clearing the menu, which destroys its items —
and those items' `OnClick` handlers are what ask for the rebuild. Calling it
directly from such a handler frees `Sender` while the widgetset is still
dispatching the click, and the fault lands inside the widgetset with no frame of
ours on the stack. Handlers therefore call `QueueCurveTypeMenuRebuild`, which
defers the work to the main loop via `Application.QueueAsyncCall` — the same
treatment, for the same reason, that `QueueError` gives a dialog raised from an
event handler or a menu.

### Nothing polls: events, and processes that say they are ready

The window does not re-derive its state on a timer. Everything that can change what it
offers raises an event that asks for one refresh (`deferred_ui`), and work that must wait
for an open menu is taken up when the application goes idle. The same rule holds between
processes: a started process says it is listening over a loopback connection its starter
listens on (`Common/readiness_channel.pas`), and the Python sidecar keeps that connection
as a lifeline, exiting when `fit_server` is gone. The architecture page draws the sequence
(`process_readiness_sequence` in the diagram generator).

### Log tiers: the line between value and volume

Both processes start at `Debug` and need no switch to be useful; a detailed build
(`FIT_DETAILED_LOG`, the `(Debug)` build modes and the development builds) starts
at `Trace`, and no release is one. One tier sits *below* the default — `Trace` — and it is
not a dumping ground for things thought unimportant. It is for **inner loops**:
output whose volume is set by an iteration count rather than by anything the
user did. Today that is the routes that only report progress - read every time
the window refreshes what it offers (`Common/rest_polling.pas`, single-sourced so
client and server cannot disagree) - and the minimizer's per-iteration progress.

The distinction is volume, not value. A three-second fit raises the minimizer's
progress over three hundred times; left at `Debug` it is 86 % of the file, and
because the log rotates it does not merely add noise — it evicts the events that
say what the user did. Those lines are still diagnostics and are still kept, one
switch away (`--log-level trace`, `/LOG_LEVEL=trace`). Anything bounded by user
actions belongs at `Debug`, where it is on by default.

### Seeing a slow repaint

`TFitChart.OnPaintTiming` reports the duration of every repaint, broken down by
**series**; `TFormMain` routes it to `client_log.LogClientTrace`, and the window
recorder takes its frames on it. Only
parts that took measurable time are named, so an ordinary repaint reports a bare
duration and a slow one names what was slow. This costs nothing to leave on and
is the only vantage point from which a slow chart is visible at all — a repaint
makes no server call and falls between two user actions, so neither the server
log nor the UI-action tier can see it.

It earned itself twice in the TAGraph fork the chart used to be (retired 2026-09
for upstream TAChart). The band's per-pixel read-back showed up as a series
costing seconds; and once that was gone the fork's phase breakdown pointed at `axis`,
which turned out to be a `Sleep(1)` left in `CalculateBounds` behind an
`if Maxi>59`. `DrawAxis` calls that routine three times a repaint — once on Y to
size the left margin, then once each for the X and Y mark loops — and every axis
in this application runs past 59, so every repaint slept three times doing
nothing. Removing it took the repaint median from 14 ms to 2 ms and the worst
case from 182 ms to 13 ms, measured over real sessions on the same machine.

Two lessons worth keeping: **`Sleep` in a paint path is not a small bug**, and a
cost that is invisible to every log tier will stay unattributed for as long as
something bigger is masking it.

## Testing, and why the suite is split in two

Every Pascal test class registers itself into one of two suites, and which one is
not a judgement about speed:

| Suite | Command | A test belongs here when |
|---|---|---|
| `unit` | `./scripts/build-app.ps1 -Task test -Suite unit` | it needs nothing outside its own process |
| `integration` | `./scripts/build-app.ps1 -Task test -Suite integration` | it starts a compute server, speaks HTTP, needs the Python sidecar, reads or writes a file, or runs the optimiser to convergence |

259 unit tests run in well under a second; all 385 take about two minutes. That
ratio is what makes the split worth having: the unit half is cheap enough to run
on every edit, and it is the half **line coverage is measured over** — an
integration test drives the same lines repeatedly to check behaviour, so it
inflates the number without reaching anything new. A unit run also builds no
compute server, because a unit test has nothing to ask one.
`tests/testcase_suite_split.pas` fails the suite when a class registers into
neither half, since an unclassified test drops out of `--suite=unit` without
failing anything.

How the suite is *built* is a separate axis from which half runs.
`-Task test` builds it with `lazbuild --widgetset=nogui`, linking the LCL
headlessly, and that binary carries everything. `tests/build.sh` builds a smaller
one with plain FPC and no LCL, for a machine without Lazarus; it is **not** the
unit suite — seven unit classes are missing from it and four integration classes
are in it. The Python sidecar has its own suite
(`Worker/py/.venv/bin/python -m pytest Worker/py`) at an enforced **100 %**
coverage gate.

Prefer a unit test. A decision table expressed over plain values can be tested
exhaustively in milliseconds; the same logic reached only through a live
`TFitTask` usually cannot be tested at all — and only the unit half is measured.

**Logic does not live in UI classes.** An LCL descendant cannot be instantiated
headlessly, so anything decided inside one is unreachable by any test: the
decision belongs in a counted module that a unit test can drive, leaving the UI
class to read controls and forward. `Desktop/int_ui_host.pas` and
`Desktop/int_fit_viewer.pas` exist as that seam, and `Desktop/pick_target.pas` is
the pattern already applied. [testing](testing.md) gives the rule and what
coverage counts.

The modules already lifted out of the window and the chart, each of which a unit
test drives directly:

| Module | What it decides | Lifted out of |
|---|---|---|
| `Desktop/action_state.pas` | which commands the window offers and which are ticked | `form_main` — four methods packing bit flags into widget `Tag`s |
| `Desktop/pick_guidance.pas` | what the user is told next while picking, and when a gesture ends | `form_main` — nested `case`s in a chart click handler |
| `Desktop/outline_layout.pas` | the tree a module's flattened outline describes | `form_main` — inside the method that fills a `TTreeView` |
| `Desktop/parameter_kinds.pas` | how a parameter is treated, in the terms the table shows | `form_main` — beside the colours that paint it |
| `Desktop/curve_type_menu.pas` | which group each curve type goes in, and the order the groups appear | `form_main` — a method creating `TMenuItem`s |
| `Desktop/module_menu.pas` | the menu a module's declarations describe | `form_main` — the same, for a module |
| `Desktop/grid_edit.pas` | what editing a cell of the profile table means | `form_main` — an editing-done handler |
| `Desktop/table_export.pas` | how a table leaves this program as text | `form_main` — a method that opens a save dialog |
| `Desktop/custom_axis.pas` | what a user-defined axis starts as, and when it is usable | `form_main` — between a dialog and a message box |
| `Desktop/typed_number.pas` | a number as a user typed it | `form_main` — a `StrToFloat` behind a swapped global separator |
| `Desktop/status_readout.pas` | the numbers along the bottom of the window | `form_main` — three handlers and a resize |
| `Desktop/legend_layout.pas` | where the pieces of a legend row sit | `form_main` — an owner-draw handler |
| `Desktop/points_tables.pas` | how many rows the small grids need, and what is in them | `fit_viewer` — expressions assigned to `RowCount` |
| `Desktop/summary_table.pas` | what the datasheet says about a fit | `fit_viewer` — written a grid cell at a time |
| `Desktop/series_palette.pas` | which colour a curve is drawn in | `fit_viewer` — a conditional inside a nested procedure |
| `Desktop/parameter_roles.pas` | which parameter of a user curve is the abscissa, position, amplitude, width | the properties dialog — the same rule in four handlers |
| `Desktop/formula_editing.pas` | what a formula keypad does to the text and the caret | the formula dialog — inside a `with EditExpression do` |
| `Desktop/ui_scaling.pas` | what pixel density the interface is laid out for | `ui_dpi` — behind `Forms` and gdk |
| `Server/curve_list.pas` | what the parameter table shows and accepts | an LCL grid presenter |
| `Desktop/pick_target.pas` | which sample a click means | a chart click handler |

Each landed with its tests in the same change, and each left its UI class
smaller. The rule that keeps it honest is in [testing](testing.md): the excluded
wrapper group's line count may only shrink.

**Self-enforcing tests** are the house speciality and the reason this scales: a
test that walks the registry and asserts every registered thing has fixtures will
fail when someone adds the next one without them. Line coverage is measured here,
but these do the job it cannot — they check the cases that matter rather than the
lines that ran, and this project's recurring failure is a green suite over a path
the user never takes.
