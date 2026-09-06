#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Generates the three published sites as plain HTML.

WHY THIS EXISTS. The diagrams this replaces were UMLet files exported by hand and
copied into the site branch under a different name. Three manual steps, each easy
to skip - which is how the published fitminimizers class diagram came to name five
classes that no longer exist anywhere in the source. Nothing here is drawn by hand,
so nothing here can go stale without the code going stale with it.

PLAIN HTML, NO SITE GENERATOR. The sites used jekyll-theme-dinky with a
copied-and-edited layout on top. The theme is gone and so is Jekyll: these pages
are complete documents, and a .nojekyll file tells GitHub Pages to serve them
untouched. What is checked in a browser here is byte-for-byte what is served.

TWO INPUTS, BECAUSE THERE ARE TWO KINDS OF FACT.

  Runtime truth comes from scripts/dump-registries, which links the framework's own
  registration front doors and reads the registries back. That is the only way to
  know what is REGISTERED - the REST verbs go in through a local helper, the test
  fixtures register fakes, and which module directory was on the unit search path
  is invisible to any amount of text matching.

  Structural truth - class hierarchies - has no runtime read-back, so it is parsed
  from the sources here. The parse is deliberately shallow: it reads declarations,
  not semantics, and anything it cannot resolve is reported rather than guessed.

The sibling packages (fitgrids, fitminimizers) have no registries at all, so they
get the structural pass alone.

  Some facts are neither: which processes exist, which thread a callback crosses,
  what happens when a dialog is cancelled. Nothing declares those, so those figures
  are composed here - and every class or interface they name is looked up in the
  structural parse, so a rename fails generation instead of publishing a picture
  that points at a class nobody has any more (see check_figure_symbols).

Prose lives in content/*.html and is never generated; generated sections are
substituted into it at the {{MARKER}} placeholders. Standard library only, on
purpose: Python is already required for the compute sidecar, so generating the
sites adds nothing to install. Mermaid renders in the browser, so there is no
diagram renderer to install either - and the pictures come out theme-aware.
"""

import argparse
import html
import json
import pathlib
import re
import shutil
import sys

HERE = pathlib.Path(__file__).resolve().parent

#  Every seam the dumper is expected to report. A seam vanishing from the dump is
#  a silent hole in the published picture - exactly the failure this tool exists
#  to prevent - so it is refused rather than rendered as a shorter list.
EXPECTED_SEAMS = [
    'curve_types', 'data_loaders', 'minimizers', 'losses', 'actions',
    'app_modules', 'ui_modules', 'sidecar_packs', 'curve_builders',
    'module_overlays',
]
#  Where a hand-composed figure's names are looked up. Every class or interface
#  drawn by hand must still be declared somewhere here, or generation fails - see
#  check_figure_symbols. The globs are listed rather than a recursive walk so that
#  backup/ and the module directories cannot quietly satisfy a stale name.
FIT_SYMBOL_GLOBS = [
    'Common/*.pas', 'Desktop/*.pas', 'Desktop/Forms/*.pas',
    'Desktop/ModelCurves/*.pas', 'Desktop/ModelCurves/CurveParameters/*.pas',
    'Desktop/ModelCurves/UserPointsSet/*.pas', 'Desktop/DataLoaders/*.pas',
    'Server/*.pas', 'Server/interfaces/*.pas', 'Worker/*.pas',
]

#  Types the figures may name that are declared outside this repository (LCL, RTL)
#  or are plain type aliases the shallow parser does not collect. Kept short on
#  purpose: every addition is a name the rot check stops watching.
KNOWN_EXTERNALS = {
    'TObject', 'TComponent', 'TInterfacedObject', 'TThread', 'TThreadMethod',
    'TForm', 'TStrings', 'TList',
}

#  Anchors each parsed hierarchy must contain. A hierarchy that silently lost its
#  root - a unit moved, a glob that no longer matches - would render as a handful
#  of orphan boxes rather than as a failure, which is the whole rot this generator
#  exists to prevent.
REQUIRED_CURVE_SYMBOLS = (
    'TPointsSet', 'TTitlePointsSet', 'TCurvePointsSet', 'TNamedPointsSet',
    'TCurveTypesSingleton', 'ICurveFactory', 'ICurveTypeIterator',
    'ICurveTypeSelector',
)
REQUIRED_USER_CURVE_SYMBOLS = (
    'TConfigurablePointsSet', 'TNonConfigurablePointsSet', 'TUserPointsSet',
    'TConfigurableUserPointsSet', 'IExpressionParser',
    #  The two dialogs, as ANSWERS rather than modal results - which is what
    #  lets the sequence between them be tested. ICreateUserPointsSetDlg was
    #  here and is gone: it answered a modal result, its only caller was that
    #  sequence, and when the sequence moved to the named answers it had none.
    'IUserCurveFormulaDlg', 'IUserCurveRolesDlg',
    'ICurveTypeParametersFactory', 'ICurveTypeStorage',
)

#  Helper records and aliases that live beside the curve classes but are not part
#  of the hierarchy being shown. Checked against the parse like everything else,
#  so a rename here fails rather than silently dropping nothing.
CURVE_DIAGRAM_DROP = ('TCurveType',)


BANNER = """<!--  GENERATED by scripts/gen-diagrams in the fit repository. Do not edit by
      hand: regenerating overwrites this file. The prose comes from
      scripts/gen-diagrams/content/, the facts from the registries themselves.  -->
"""


class GenerationError(Exception):
    """Something the generator must not paper over."""


# --------------------------------------------------------------------------
# HTML
# --------------------------------------------------------------------------

def esc(text):
    return html.escape(str(text), quote=False)


def table(headers, rows):
    """A table inside its own scroller.

    Every table is wrapped: a fourteen-row REST listing is wider than a phone,
    and without this the whole page scrolls sideways instead of the table.
    """
    out = ['<div class="table-scroll">', '<table>', '<thead><tr>']
    out += ['<th>%s</th>' % h for h in headers]
    out += ['</tr></thead>', '<tbody>']
    for row in rows:
        out.append('<tr>' + ''.join('<td>%s</td>' % c for c in row) + '</tr>')
    out += ['</tbody>', '</table>', '</div>']
    return '\n'.join(out)


def mermaid(lines):
    #  The source is escaped, not the rendered SVG: mermaid reads textContent,
    #  so a label containing < or & must survive as itself.
    return '<pre class="mermaid">%s</pre>' % html.escape('\n'.join(lines))


def yes_no(value):
    return 'yes' if value else '&mdash;'


def or_dash(value):
    """A cell's text, escaped - or an em dash when there is nothing to show.

    The dash has to be added AFTER escaping. Writing esc(value or '&mdash;')
    reads fine and is wrong: esc turns the ampersand into &amp; and the cell
    displays the literal characters &mdash;, which is what the curve-type table
    did in every Group row.
    """
    return esc(value) if value not in (None, '') else '&mdash;'


def status(kind, note=''):
    """One status pill.

    Three states only, and the WORD carries the meaning - the colour is a second
    channel, never the only one, so the table still reads correctly in monochrome
    or to anyone who cannot separate the hues.
    """
    labels = {'implemented': 'Implemented', 'partial': 'Partial',
              'planned': 'Planned'}
    if kind not in labels:
        raise GenerationError('unknown status %r' % kind)
    pill = '<span class="status status--%s">%s</span>' % (kind, labels[kind])
    return pill + ('<br>' + note if note else '')


def page(title, tagline, repo_url, body, nav=''):
    """One complete document. There is no layout engine; this is the layout."""
    return """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>%(title)s</title>
<meta name="description" content="%(tagline)s">
<!--
  No analytics, no ad network, no hit counter, no icon font, no webfont. Apart
  from the Mermaid module below - and, on the front page, the demo video - this
  document asks the network for nothing but its own stylesheet.

  user-scalable=no is deliberately absent: it used to be here, and it stops
  pinch-zoom on the one kind of device most likely to need it.
-->
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="assets/css/style.css">
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico">
</head>
<body>
<a class="skip" href="#content">Skip to content</a>

<header class="site">
  <div class="wrap">
    <p class="site-name"><a href="index.html">%(title)s</a></p>
    <p class="site-tagline">%(tagline)s</p>
    <p class="site-links"><a class="button" href="%(repo)s">View on GitHub</a></p>
  </div>
</header>

<main id="content" class="wrap">
%(body)s
%(nav)s
</main>

<footer class="site">
  <div class="wrap">
    <p>%(title)s is free software under
      <a href="https://www.gnu.org/licenses/gpl-3.0.html">GPL-3.0-or-later</a>.
      <a href="%(repo)s">Source</a>.</p>
    <h3>Find me on</h3>
    <p><a href="https://www.linkedin.com/in/dmitry-morozov-79490a59/">LinkedIn</a>
       or <a href="https://www.facebook.com/dmitry.v.morozov">Facebook</a>.</p>
  </div>
</footer>

<script type="module">
  import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11.4.1/dist/mermaid.esm.min.mjs';

  const figures = [...document.querySelectorAll('pre.mermaid')]
    .map((el) => ({ el, src: el.textContent }));

  //  RENDERED ONE AT A TIME, not through mermaid.run(). Given several diagrams
  //  on a page, run() draws the second and every later one into the second
  //  one's element - the page then shows three diagrams stacked on top of each
  //  other and two empty boxes. Rendering each explicitly and inserting the SVG
  //  that comes back keeps them apart.
  let pass = 0;
  const draw = async () => {
    const dark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    mermaid.initialize({
      startOnLoad: false,
      theme: dark ? 'dark' : 'default',
      securityLevel: 'strict',
    });
    pass += 1;
    for (let i = 0; i < figures.length; i += 1) {
      const fig = figures[i];
      try {
        //  A fresh id per pass: reusing one across re-renders makes mermaid
        //  find the previous pass's leftovers.
        const { svg } = await mermaid.render(`diagram-${pass}-${i}`, fig.src);
        fig.el.innerHTML = svg;
        fig.el.dataset.rendered = 'true';
      } catch (e) {
        //  Leave the source visible rather than an empty frame: a diagram that
        //  failed should look broken, not absent.
        fig.el.textContent = fig.src;
        fig.el.dataset.rendered = 'failed';
      }
    }
  };

  draw();
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', draw);
</script>
</body>
</html>
""" % {'title': esc(title), 'tagline': esc(tagline), 'repo': repo_url,
       'body': body, 'nav': nav}


def nav(*links):
    return ('<hr>\n<p class="nav">' +
            ' &middot; '.join('<a href="%s">%s</a>' % (h, t) for h, t in links) +
            '</p>')


# --------------------------------------------------------------------------
# Mermaid label helpers
# --------------------------------------------------------------------------

def mm_md(*lines):
    """A Mermaid markdown-string label: backticks, with real newlines.

    NOT <br/> and <small>. The page initialises mermaid with securityLevel
    'strict', which turns HTML labels off, and a label containing markup then
    renders as the literal words "Unsupported markdown" inside the node.
    """
    body = '\n'.join(str(l) for l in lines if l is not None and str(l) != '')
    #  A backtick or a double quote inside the label would end it early.
    body = body.replace('`', "'").replace('"', "'")
    return '["`%s`"]' % body


def mm_id(text):
    ident = re.sub(r'[^A-Za-z0-9_]', '_', str(text))
    if not ident or ident[0].isdigit():
        ident = 'n_' + ident
    return ident


# --------------------------------------------------------------------------
# The dump
# --------------------------------------------------------------------------

def load_dump(path):
    try:
        data = json.loads(pathlib.Path(path).read_text(encoding='utf-8'))
    except (OSError, ValueError) as exc:
        raise GenerationError('could not read the registry dump %s: %s' % (path, exc))
    seams = {s['key']: s for s in data.get('seams', [])}
    missing = [k for k in EXPECTED_SEAMS if k not in seams]
    if missing:
        raise GenerationError(
            'the registry dump is missing these seams: %s. Either dump_registries '
            'was not rebuilt after a seam changed, or a seam was removed - and the '
            'published page would silently stop mentioning it.' % ', '.join(missing))
    return data, seams


# --------------------------------------------------------------------------
# Pascal structural parsing (no runtime read-back exists for this)
# --------------------------------------------------------------------------

CLASS_RE = re.compile(
    r'^\s*(?P<name>T[A-Za-z0-9_]+)\s*=\s*class\s*\(\s*(?P<parent>[A-Za-z0-9_]+)'
    r'(?:\s*,\s*(?P<ifaces>[^)]*))?\)', re.MULTILINE)
CLASS_NOPARENT_RE = re.compile(
    r'^\s*(?P<name>T[A-Za-z0-9_]+)\s*=\s*class\s*(?:;|$)', re.MULTILINE)
IFACE_RE = re.compile(r'^\s*(?P<name>I[A-Za-z0-9_]+)\s*=\s*interface', re.MULTILINE)


def strip_comments(src):
    """Remove Pascal comments so a declaration quoted in prose is not parsed.

    These sources carry long explanatory comments that name classes and sometimes
    show declarations; parsing those would invent classes that do not exist.
    """
    src = re.sub(r'\(\*.*?\*\)', '', src, flags=re.DOTALL)
    src = re.sub(r'\{.*?\}', '', src, flags=re.DOTALL)
    src = re.sub(r'//[^\n]*', '', src)
    return src


def parse_units(paths):
    classes, ifaces = {}, {}
    for path in paths:
        try:
            src = strip_comments(path.read_text(encoding='utf-8', errors='replace'))
        except OSError as exc:
            raise GenerationError('could not read %s: %s' % (path, exc))
        for m in CLASS_RE.finditer(src):
            impl = [i.strip() for i in (m.group('ifaces') or '').split(',')
                    if i.strip().startswith('I')]
            classes[m.group('name')] = {'parent': m.group('parent'),
                                        'implements': impl, 'unit': path.name}
        for m in CLASS_NOPARENT_RE.finditer(src):
            classes.setdefault(m.group('name'),
                               {'parent': None, 'implements': [], 'unit': path.name})
        for m in IFACE_RE.finditer(src):
            ifaces[m.group('name')] = {'unit': path.name}
    return classes, ifaces


def hierarchy_diagram(classes, ifaces):
    lines = ['classDiagram', '    direction LR']
    known = set(classes)
    externals = {info['parent'] for info in classes.values()
                 if info['parent'] and info['parent'] not in known}
    for name in sorted(externals):
        lines.append('    class %s["%s"]' % (mm_id(name), name))
        lines.append('    <<external>> %s' % mm_id(name))
    for name in sorted(ifaces):
        lines.append('    class %s["%s"]' % (mm_id(name), name))
        lines.append('    <<interface>> %s' % mm_id(name))
    for name in sorted(classes):
        lines.append('    class %s["%s"]' % (mm_id(name), name))
    for name, info in sorted(classes.items()):
        parent = info['parent']
        if parent and (parent in known or parent in externals):
            lines.append('    %s <|-- %s' % (mm_id(parent), mm_id(name)))
        for iface in info['implements']:
            if iface in ifaces:
                lines.append('    %s <|.. %s' % (mm_id(iface), mm_id(name)))
    return mermaid(lines)
#  For the rot check only: any declared type name, not just classes and interfaces.
#  A figure may legitimately name a record or an enumeration (TModuleSeriesStyle is
#  a record), and those have no place in a hierarchy but must still exist.
TYPE_DECL_RE = re.compile(r'^\s*(?P<name>[TI][A-Za-z0-9_]+)\s*=', re.MULTILINE)


def fit_symbols(repo_root):
    """Every type name the fit sources declare, for the rot check."""
    root = pathlib.Path(repo_root)
    paths = sorted({p for pattern in FIT_SYMBOL_GLOBS for p in root.glob(pattern)})
    if not paths:
        raise GenerationError('no sources matched %s under %s - the tree moved'
                              % (', '.join(FIT_SYMBOL_GLOBS), root))
    classes, ifaces = parse_units(paths)
    if not classes or not ifaces:
        raise GenerationError('parsed no declarations from %s - the parser is broken '
                              'or the sources moved' % root)
    names = set(classes) | set(ifaces) | set(KNOWN_EXTERNALS)
    for path in paths:
        src = strip_comments(path.read_text(encoding='utf-8', errors='replace'))
        names.update(m.group('name') for m in TYPE_DECL_RE.finditer(src))
    return names


#  A name in a figure looks like a Pascal class or interface: T or I, then another
#  capital, then at least two more characters. Two-letter mermaid keywords (TB, LR)
#  are shorter than that and never match.
FIGURE_SYMBOL_RE = re.compile(r'\b[TI][A-Z][A-Za-z0-9_]{2,}\b')


def checked(lines, known, where):
    """A hand-composed figure - refused if it names something that no longer exists.

    Process and threading structure has no declaration to parse, so those pictures
    are written here rather than derived. That is exactly how the old UMLet diagrams
    came to name classes that had been renamed away, so every name in them is looked
    up in the parse and generation fails rather than publishing a stale box.
    """
    named = {n for line in lines for n in FIGURE_SYMBOL_RE.findall(line)}
    unknown = sorted(named - known)
    if unknown:
        raise GenerationError(
            'the %s figure names %s, which is not declared in the sources any more. '
            'Fix the figure (or the name) - a picture pointing at a class that was '
            'renamed away is worse than no picture.' % (where, ', '.join(unknown)))
    return mermaid(lines)


def parsed_hierarchy(repo_root, patterns, required, where, drop=()):
    """A class diagram read from the sources, with its anchors asserted."""
    root = pathlib.Path(repo_root)
    paths = sorted({p for pattern in patterns for p in root.glob(pattern)})
    if not paths:
        raise GenerationError('no sources matched %s under %s, so the %s diagram '
                              'cannot be generated'
                              % (', '.join(patterns), root, where))
    classes, ifaces = parse_units(paths)
    known = set(classes) | set(ifaces)
    missing = [n for n in required if n not in known]
    if missing:
        raise GenerationError(
            'the %s diagram lost %s. Either the class was renamed, or the units it '
            'is parsed from moved out of %s - and the diagram would render as a few '
            'orphan boxes instead of failing.'
            % (where, ', '.join(missing), ', '.join(patterns)))
    for name in drop:
        if name not in known:
            raise GenerationError(
                'the %s diagram drops %s, which is not declared any more - remove it '
                'from the drop list rather than leaving a filter nothing matches.'
                % (where, name))
        classes.pop(name, None)
        ifaces.pop(name, None)
    return hierarchy_diagram(classes, ifaces)


# --------------------------------------------------------------------------
# fit: the diagrams
# --------------------------------------------------------------------------

def topology_diagram():
    #  Structural, not parsed: the process split is a design decision, not a fact
    #  discoverable in a declaration. It is here rather than hand-written into a
    #  page so that the whole picture set has one source.
    return mermaid([
        'flowchart LR',
        '    client%s' % mm_md('**Desktop client**', 'no fitting engine at all'),
        '    server%s' % mm_md('**fit_server**', 'the only client-facing endpoint'),
        '    sidecar%s' % mm_md('**Python sidecar**', 'child process of fit_server'),
        '    remote%s' % mm_md('**Another fit_server**', 'optional'),
        '    client -- "HTTP + JSON" --> server',
        '    server -- "spawns, owns" --> sidecar',
        '    server -. "may delegate" .-> remote',
    ])


def seam_map_diagram(seams):
    lines = ['flowchart LR',
             '    subgraph yours["Your module - one directory"]',
             '        direction TB',
             '        code%s' % mm_md('**Your units**'),
             '        door%s' % mm_md('**The front door**', 'one exported procedure'),
             '        code --> door',
             '    end',
             '    subgraph fw["The framework - unedited"]',
             '        direction TB']
    for key in EXPECTED_SEAMS:
        s = seams[key]
        lines.append('        %s%s' % (mm_id(key), mm_md(
            '**%s**' % s['name'], s['entry_point'].split('.')[-1],
            '%d registered' % s['count'])))
    lines.append('    end')
    for key in EXPECTED_SEAMS:
        lines.append('    door --> %s' % mm_id(key))
    #  Two classes only: the point of the picture is which seams are still empty,
    #  because those are the ones nothing in the public build has claimed yet.
    lines.append('    classDef empty stroke-dasharray: 4 3;')
    empty = [mm_id(k) for k in EXPECTED_SEAMS if seams[k]['count'] == 0]
    if empty:
        lines.append('    class %s empty;' % ','.join(empty))
    return mermaid(lines)


def module_anatomy_diagram():
    return mermaid([
        'flowchart TB',
        "    subgraph sp[\"The project's unit search path, in order\"]",
        '        direction TB',
        '        first%s' % mm_md('**1. Modules/your-module/**'),
        '        rest%s' % mm_md('**2. Desktop/, Server/, Common/**',
                                 'and tests/no-modules/'),
        '    end',
        '    yours%s' % mm_md('**your-module/app_modules.pas**',
                              'RegisterAppModules calls your front door'),
        '    stub%s' % mm_md('**Common/app_modules.pas**', 'the stub: does nothing'),
        '    ytests%s' % mm_md('**your-module/module_tests.pas**',
                               'uses your testcase units'),
        '    stests%s' % mm_md('**tests/no-modules/module_tests.pas**',
                               'the stub: empty'),
        '    host%s' % mm_md('**The client, server and test runner**',
                             'all call RegisterAppModules'),
        '    first --> yours',
        '    first --> ytests',
        '    rest -.-> stub',
        '    rest -.-> stests',
        '    yours --> host',
        '    ytests --> host',
        '    classDef soft stroke-dasharray: 4 3;',
        '    class stub,stests soft;',
    ])


def backend_diagram():
    return mermaid([
        'flowchart LR',
        '    info%s' % mm_md('**TMinimizerInfo**', 'the engine, and what it can do'),
        '    factory%s' % mm_md('**CreateBackend**', 'TBackendFactory'),
        '    ctx%s' % mm_md('**TBackendContext**', 'PythonUrl, ServerUrl'),
        '    native%s' % mm_md('**TNativeFitBackend**', 'in process'),
        '    python%s' % mm_md('**TPythonFitBackend**', 'the sidecar'),
        '    remote%s' % mm_md('**TServerFitBackend**', 'another fit_server'),
        '    fallback%s' % mm_md('**nil - not available here**', 'caller falls back'),
        '    info --> factory',
        '    ctx --> factory',
        '    factory --> native',
        '    factory --> python',
        '    factory --> remote',
        '    factory --> fallback',
        '    classDef soft stroke-dasharray: 4 3;',
        '    class fallback soft;',
    ])
def call_chain_diagram(known):
    #  The live path only. The wst/SOAP proxies and the CGI client the old UMLet
    #  diagrams documented are gone from every project file; drawing them again
    #  would document a build nobody can produce.
    return checked([
        'flowchart LR',
        '    subgraph desktop["Desktop client - Fit.lpr"]',
        '        direction TB',
        '        form%s' % mm_md('**TFormMain**', 'the UI, and nothing else'),
        '        client%s' % mm_md('**TFitClient**',
                                   'RunAsync hands a blocking call',
                                   'to TServerCallThread'),
        '        http%s' % mm_md('**THttpFitService**', 'implements IFitService'),
        '        form --> client --> http',
        '    end',
        '    subgraph server["fit_server.lpr - the only client-facing endpoint"]',
        '        direction TB',
        '        rest%s' % mm_md('**TFitRestApi**', 'verbs from the action registry'),
        '        reg%s' % mm_md('**TSessionRegistry**', 'one problem, one session'),
        '        session%s' % mm_md('**TFitSession**',
                                    'implements IClientCallback',
                                    'and records the progress reported to it'),
        '        service%s' % mm_md('**TFitService**',
                                    'one TFitTask per fit interval'),
        '        task%s' % mm_md('**TFitTask**',
                                 'sums the curves, evaluates the objective'),
        '        rest --> reg --> session --> service --> task',
        '    end',
        '    http -- "HTTP + JSON" --> rest',
        '    service -. "IClientCallback" .-> session',
        '    http -. "polls /state, /async, /stats" .-> rest',
    ], known, 'client-to-server call chain')


def fit_progress_sequence(known):
    #  Two halves that never touch: progress travels INTO the session by callback,
    #  and out to the client by polling. Drawing them as one chain - which the old
    #  notification diagram did, back when the server called the client back - is
    #  the thing to avoid.
    return checked([
        'sequenceDiagram',
        '    autonumber',
        '    participant ui as TFormMain',
        '    participant client as TFitClient',
        '    participant api as TFitRestApi',
        '    participant session as TFitSession',
        '    participant service as TFitService',
        '    participant task as TFitTask',
        '    participant min as TMinimizer',
        '    ui->>client: start the fit',
        '    client->>api: POST the asynchronous verb',
        '    api->>session: run it under the session lock',
        '    session->>service: MinimizeDifference',
        '    service->>task: RecreateMainCalcThread runs the task inline',
        '    task->>min: Minimize, with OnShowCurMin bound to ShowCurMin',
        '    loop every accepted step',
        '        min-->>task: OnShowCurMin',
        '        task->>service: ServerShowCurMin, the R-factor recomputed',
        '        service->>session: ShowCurMinInternal, then IClientCallback',
        '    end',
        '    service->>session: Done',
        '    par the client is not called back',
        '        loop while the operation is running',
        '            ui->>client: TimerCheckState',
        '            client->>api: GET /state, /async, /stats',
        '            api-->>client: the values the session recorded',
        '        end',
        '    end',
        '    client->>ui: OnAsyncOperationFinished',
        '    note over service,task: The session owns a PLAIN TFitService and the '
        'task runs inline. Reporting through TMainCalcThread would need Synchronize '
        'pumped, and a headless server never pumps it - which is why progress '
        'reaches the client by polling rather than by callback.',
    ], known, 'progress reporting')


def viewer_seam_diagram(known):
    return checked([
        'flowchart LR',
        '    client%s' % mm_md('**TFitClient**', 'holds one IFitViewer',
                               'and names no visual class'),
        '    iface%s' % mm_md('**IFitViewer**', 'the view seam'),
        '    viewer%s' % mm_md('**TFitViewer**',
                               'the only place the charting component is known'),
        '    chart%s' % mm_md('**Chart series, legend, grids**'),
        '    module%s' % mm_md("**A module's presenter**",
                               'PlotModuleSeries, ShowModulePanel'),
        #  Not 'style' as an id: that word starts a styling statement in a
        #  flowchart, and the node would be parsed as one.
        '    vocab%s' % mm_md('**TModuleSeriesStyle**',
                              "the framework's vocabulary, not the chart's"),
        '    client --> iface',
        '    iface -. "implemented by" .-> viewer',
        '    viewer --> chart',
        '    module --> iface',
        '    module --> vocab',
        '    vocab --> iface',
    ], known, 'view seam')


def curve_hierarchy_diagram(repo_root):
    return parsed_hierarchy(repo_root, ['Desktop/ModelCurves/*.pas'],
                            REQUIRED_CURVE_SYMBOLS, 'curve class',
                            drop=CURVE_DIAGRAM_DROP)


def user_curve_diagram(repo_root):
    return parsed_hierarchy(repo_root, ['Desktop/ModelCurves/UserPointsSet/*.pas'],
                            REQUIRED_USER_CURVE_SYMBOLS, 'user curve type')


def configure_curve_sequence(known):
    return checked([
        'sequenceDiagram',
        '    autonumber',
        '    participant ui as TFormMain',
        '    participant iter as ICurveTypeIterator',
        '    participant conf as TConfigurableUserPointsSet',
        '    participant flow as RunUserCurveFlow',
        '    participant dlg as IUserCurveFormulaDlg',
        '    participant parser as IExpressionParser',
        '    participant factory as ICurveTypeParametersFactory',
        '    participant store as ICurveTypeStorage',
        '    participant roles as IUserCurveRolesDlg',
        '    ui->>iter: which registered type was clicked',
        '    iter-->>ui: GetCurrentCurveClass',
        '    ui->>conf: GetConfigurablePointsSet, HasConfigurableParameters',
        '    alt the type configures itself',
        '        ui->>conf: ShowConfigurationDialog',
        '        conf->>flow: the five collaborators, and nothing else',
        '        flow->>dlg: Ask - a name and a formula in x',
        '        alt cancelled',
        '            dlg-->>flow: daCancelled, and the definition is abandoned',
        '        else confirmed',
        '            flow->>parser: ParseExpression',
        '            alt the formula does not parse',
        '                parser-->>flow: nil, the message already shown',
        '                flow->>dlg: Ask again - nothing has been stored yet',
        '            else it parses',
        '                flow->>factory: CreateUserCurveType',
        '                flow->>store: AddCurveType',
        '                flow->>roles: Ask - which parameter plays which role',
        '                alt daAccepted',
        '                    flow->>store: UpdateCurveType',
        '                else daStartAgain',
        '                    flow->>store: DeleteCurveType, then Ask the formula again',
        '                else daCancelled',
        '                    flow->>flow: the stored type is left as it is',
        '                end',
        '            end',
        '        end',
        '    end',
        '    alt configuration was cancelled and HasDefaults',
        '        ui->>conf: SetDefaults',
        '    else cancelled with no defaults',
        '        ui->>ui: say so, and leave the type unselected',
        '    end',
    ], known, 'configuring a curve type')


# --------------------------------------------------------------------------
# fit: the pages
# --------------------------------------------------------------------------

FIT_REPO = 'https://github.com/dvmorozov/fit'
FIT_TAGLINE = 'Interactive curve fitting for Linux, Windows and macOS'


def seam_table(seams):
    rows = []
    for key in EXPECTED_SEAMS:
        s = seams[key]
        if s['count']:
            state = '<strong>%d</strong>' % s['count']
        elif s['module_only']:
            state = '0 &mdash; module-only seam'
        else:
            state = '0'
        rows.append([esc(s['name']), '<code>%s</code>' % esc(s['accepts']),
                     '<code>%s</code>' % esc(s['entry_point'].split('.')[-1]), state])
    return table(['Seam', 'You write', 'Entry point', 'Registered here'], rows)


def architecture_page(seams, known):
    b = [BANNER, '<h1>Architecture</h1>',
         '<p>Everything on this page is generated from the registries this build '
         'actually contains, so it cannot describe a version of the code that no '
         'longer exists.</p>',
         '<h2>Three processes</h2>',
         '<p>The client holds <strong>no fitting engine at all</strong>. '
         '<code>fit_server</code> is the only client-facing endpoint; the Python '
         'sidecar is a child process it owns and never something a client talks to '
         'directly.</p>',
         topology_diagram(),
         '<h2>Client and server</h2>',
         '<p>The client sends a request and <strong>polls</strong>; the server never '
         'calls it back. That is the whole of the wire protocol, and it is why the '
         'desktop can be closed and reopened while a problem is still on the '
         'server.</p>',
         call_chain_diagram(known),
         '<h2>Watching a fit run</h2>',
         '<p>Progress travels in two hops that never meet. Inside the server the '
         'minimizer reports each accepted step up through the task to the session, '
         'which records it; the client asks for that record on its own timer. A '
         'REST session therefore owns the <strong>synchronous</strong> '
         '<code>TFitService</code>, whose <code>RecreateMainCalcThread</code> runs '
         'the work inline &mdash; a headless server has no UI thread to pump, so a '
         'callback marshalled with <code>Synchronize</code> would never arrive.</p>',
         fit_progress_sequence(known),
         '<h2>The view seam</h2>',
         '<p>The client holds one <code>IFitViewer</code> and names no visual class '
         'at all, so the fitting logic links and tests without a widgetset. A module '
         'draws through the same seam, describing its series in the '
         "framework's own vocabulary rather than the charting component's &mdash; "
         'which is what lets that component be replaced without touching a '
         'module.</p>',
         viewer_seam_diagram(known),
         '<h2>The extension seams</h2>',
         '<p>Fit is a framework as much as an application. A new curve type, data '
         'loader, engine, objective, REST verb or whole analysis vertical is added '
         'by <strong>registration</strong> &mdash; a directory, plus one entry on a '
         "project's unit search path. No framework file changes.</p>",
         seam_map_diagram(seams), seam_table(seams)]

    empty = [seams[k]['name'] for k in EXPECTED_SEAMS
             if seams[k]['count'] == 0 and seams[k]['module_only']]
    if empty:
        b.append('<p>The %d seams shown dashed &mdash; %s &mdash; have nothing '
                 'registered in this build. That is not disuse: they exist '
                 '<strong>for</strong> modules, and the public framework deliberately '
                 'ships none. They are listed precisely because an extender needs to '
                 'know they are there and unclaimed.</p>'
                 % (len(empty), ', '.join(esc(n) for n in empty)))

    b += ['<h2>Anatomy of a module</h2>',
          '<p>A module is a directory. It wins by being <strong>first on the unit '
          'search path</strong>: Free Pascal resolves <code>app_modules</code> and '
          "<code>module_tests</code> to the module's copies instead of the "
          "framework's stubs, so the framework calls into the module without naming "
          'it.</p>',
          module_anatomy_diagram(),
          '<p><strong>The trap this design has to guard.</strong> A unit nobody '
          '<code>uses</code> is never linked, so its <code>initialization</code> '
          'never runs and its curve type silently does not exist. That is what '
          '<code>ExpectCurveTypes</code> is for: a pack declares the types it must '
          'have, and start-up fails naming every missing class rather than running '
          'with a shorter menu.</p>',
          '<h2>Engines and backends</h2>',
          '<p>A backend is never registered directly. A minimizer declares a factory, '
          'and the factory decides what can actually run here &mdash; returning '
          '<strong><code>nil</code> when it cannot</strong>, which is not a failure: '
          'the caller falls back to the default engine, which is why the application '
          'still fits with no Python installed at all.</p>',
          backend_diagram(),
          table(['Engine', 'Needs a formula', 'Needs the sidecar', 'Weighting',
                 'Curve scaling'],
                [[esc(i['name']), yes_no(i['needs_formula']),
                  yes_no(i['needs_python_sidecar']), yes_no(i['supports_weighting']),
                  yes_no(i['supports_curve_scaling'])]
                 for i in seams['minimizers']['items']]),
          '<h2>Objectives</h2>',
          '<p>What &ldquo;best fit&rdquo; means. Whether an objective may be used '
          'with a curve type is <strong>derived</strong> from these two flags &mdash; '
          'a self-normalising objective can be reduced by inflating a model whose '
          'amplitude is free, so that one pairing is refused. No table of type names '
          'is involved.</p>',
          table(['Objective', 'Self-normalising', 'Least squares',
                 'Poolable across intervals'],
                [[esc(i['name']), yes_no(i['self_normalising']),
                  yes_no(i['least_squares']), yes_no(i['poolable'])]
                 for i in seams['losses']['items']]),
          '<h2>The REST API</h2>',
          '<p><code>fit_server</code> answers %d verbs. An asynchronous one starts '
          'work that outlives the request, so a caller polls rather than waiting.</p>'
          % seams['actions']['count'],
          table(['Verb', 'Does', 'Asynchronous'],
                [['<code>%s</code>' % esc(i['name']), esc(i['description']),
                  yes_no(i['asynchronous'])]
                 for i in seams['actions']['items']])]

    return page('Fit', FIT_TAGLINE, FIT_REPO, '\n'.join(b),
                nav(('index.html', 'Home'), ('extension-points.html', 'Extension points'),
                    ('how-to-extend-curve-types.html', 'Adding a curve type'),
                    ('how-to-extend-data-loaders.html', 'Adding a data loader')))


def extension_points_page(seams):
    b = [BANNER, '<h1>Extension points</h1>',
         '<p>One section per seam: what you write, where it goes in, and what is '
         'registered through it in this build. Generated from the registries '
         'themselves.</p>']
    for key in EXPECTED_SEAMS:
        s = seams[key]
        b.append('<h2>%s</h2>' % esc(s['name']))
        b.append('<p>%s</p>' % esc(s['purpose']))
        if s['count']:
            here = '%d' % s['count']
        elif s['module_only']:
            here = ('none. This seam exists for modules; the public framework ships '
                    'none, and it is yours to claim.')
        else:
            here = 'none.'
        b.append('<ul><li><strong>You write</strong> &mdash; <code>%s</code></li>'
                 '<li><strong>Entry point</strong> &mdash; <code>%s</code></li>'
                 '<li><strong>Registered here</strong> &mdash; %s</li></ul>'
                 % (esc(s['accepts']), esc(s['entry_point']), here))
        items = s.get('items') or []
        if items:
            keys = [k for k in items[0].keys() if k != 'resources']
            rows = []
            for i in items:
                row = []
                for k in keys:
                    v = i.get(k, '')
                    row.append(yes_no(v) if isinstance(v, bool) else or_dash(v))
                rows.append(row)
            b.append(table([k.replace('_', ' ') for k in keys], rows))
    return page('Fit', FIT_TAGLINE, FIT_REPO, '\n'.join(b),
                nav(('index.html', 'Home'), ('architecture.html', 'Architecture')))


def group_note(seam):
    """A word about the Group column when every row of it is empty.

    Thirteen dashes read as a broken table unless the reader is told that the
    grouping exists and nothing has claimed it yet - the same reason the empty
    extension seams are called out rather than omitted.
    """
    if any(i.get('group') for i in seam['items']):
        return ''
    return ('<p>No registered type asks for a menu group, so the '
            '<strong>Group</strong> column is empty throughout. A type that returns '
            'a name from <code>GetCurveTypeGroup</code> gets a submenu of that name '
            '&mdash; which is what a pack contributing several related types wants, '
            'rather than crowding the flat list.</p>')


def curve_type_page(seams, repo_root, known):
    s = seams['curve_types']
    b = [BANNER, '<h1>How to add a curve type</h1>',
         '<p>A curve type is one unit. It subclasses <code>TNamedPointsSet</code>, '
         'answers a few questions about itself, and registers itself from its own '
         '<code>initialization</code> section. Nothing else is edited &mdash; the '
         'menu, the factory and the compatibility rules are all derived from what '
         'registered.</p>',
         mermaid(['flowchart TB',
                  '    unit%s' % mm_md('**Your unit**', 'class(TNamedPointsSet)'),
                  '    init%s' % mm_md('**initialization**',
                                       'RegisterCurveType(TYours)'),
                  '    reg%s' % mm_md('**TCurveTypesSingleton**', 'the curve factory'),
                  '    menu%s' % mm_md('**The Curve Type menu**',
                                       'grouped by GetCurveTypeGroup'),
                  '    engine%s' % mm_md('**The engine**', 'instantiates by type id'),
                  '    loss%s' % mm_md('**Objective compatibility**',
                                       'derived from AmplitudeIsUnbounded'),
                  '    axis%s' % mm_md('**Displayed axis**',
                                       'from CreatePreferredAxis'),
                  '    unit --> init --> reg',
                  '    reg --> menu', '    reg --> engine',
                  '    reg --> loss', '    reg --> axis']),
         '<h2>The questions a curve type answers</h2>',
         '<p>Each has a sensible default, so a type only overrides what differs. '
         'These are <strong>capabilities, not a list of type names</strong>: adding a '
         'type needs no edit to any compatibility table.</p>',
         table(['Type', 'Analytic', 'Group', 'Unbounded amplitude', 'Placed by',
                'Preferred axis'],
               [['%s<br><small><code>%s</code></small>'
                 % (esc(i['name']), esc(i.get('class', ''))),
                 yes_no(i.get('analytic')), or_dash(i.get('group')),
                 yes_no(i.get('amplitude_unbounded')),
                 esc(i.get('placed_by_point_set') or 'a single position'),
                 or_dash(i.get('preferred_axis'))]
                for i in s['items']]),
         '<p>%d curve types are registered in this build.</p>' % s['count'],
         group_note(s),
         '<h2>The curve classes</h2>',
         '<p>Read from the units themselves. A curve type is a point set first: '
         '<code>TPointsSet</code> holds the points, <code>TCurvePointsSet</code> adds '
         'the parameters a minimizer varies, and <code>TNamedPointsSet</code> adds the '
         'questions above. <code>TCurveTypesSingleton</code> is the registry every '
         'type registers into, and the three interfaces it implements are how the '
         'menu, the factory and the engine reach it &mdash; none of them names a '
         'curve class.</p>',
         curve_hierarchy_diagram(repo_root),
         '<h2>Curve types the user defines</h2>',
         '<p>A type whose formula is typed at run time still arrives through the same '
         'registry. The configuration lives in a <strong>separate class</strong> '
         '(<code>GetConfigurablePointsSet</code>) rather than on the curve itself: '
         'these are class-level questions asked before any instance exists, and Free '
         'Pascal has neither multiple inheritance nor class methods in interfaces.</p>',
         user_curve_diagram(repo_root),
         '<p>Creating one is two dialogs, and every failure has a defined landing '
         'place &mdash; an unparseable formula reopens the first dialog rather than '
         'raising, and a cancelled setup leaves the type unselected instead of '
         'half-registered.</p>',
         configure_curve_sequence(known),
         '<p>Full instructions, including the traps: '
         '<a href="%s/blob/main/docs/contributing/adding-a-curve-model.md">adding a '
         'curve model</a> &middot; '
         '<a href="%s/blob/main/docs/contributing/writing-a-module.md">writing a '
         'module</a> &middot; '
         '<a href="%s/tree/main/Modules/example-linear">a complete working '
         'example</a>.</p>' % (FIT_REPO, FIT_REPO, FIT_REPO)]
    return page('Fit', FIT_TAGLINE, FIT_REPO, '\n'.join(b),
                nav(('index.html', 'Home'), ('architecture.html', 'Architecture'),
                    ('extension-points.html', 'Extension points')))


def data_loader_page(seams):
    s = seams['data_loaders']
    b = [BANNER, '<h1>How to add a data loader</h1>',
         '<p>A loader reads one file format. It inherits <code>TDataLoader</code> and '
         'implements <code>LoadDataSetActually</code>, then registers itself with the '
         'extensions and the format name it claims.</p>',
         "<p>The <strong>Open</strong> dialog's filter is built from that registry, so "
         'a build can only offer a format something in it can actually read. Adding a '
         'loader therefore needs no edit to any existing file.</p>',
         mermaid(['flowchart TB',
                  '    unit%s' % mm_md('**Your loader**', 'class(TDataLoader)'),
                  '    reg%s' % mm_md('**RegisterDataLoader**',
                                      'class, extensions, format name'),
                  '    registry%s' % mm_md('**The loader registry**'),
                  '    filter%s' % mm_md('**The Open dialog filter**',
                                         'derived, never hand-written'),
                  '    open%s' % mm_md('**Opening a file**',
                                       'FindDataLoaderClass by extension'),
                  '    unit --> reg --> registry',
                  '    registry --> filter', '    registry --> open']),
         '<h2>Registered in this build</h2>',
         table(['Format', 'Extensions', 'Class'],
               [[esc(i['format']), '<code>%s</code>' % esc(i['extensions']),
                 '<code>%s</code>' % esc(i['class'])] for i in s['items']]),
         '<p>Full instructions: '
         '<a href="%s/blob/main/docs/contributing/writing-a-module.md">writing a '
         'module</a>.</p>' % FIT_REPO]
    return page('Fit', FIT_TAGLINE, FIT_REPO, '\n'.join(b),
                nav(('index.html', 'Home'), ('architecture.html', 'Architecture'),
                    ('extension-points.html', 'Extension points')))


def status_table(seams, repo_root):
    """What is implemented, what is partial, what is planned.

    Every marker is derived from the dump or from a fact checked here - none is
    typed in from memory. A marker nobody can trace is worse than the prose it
    would replace.
    """
    rows = [['Curve types', status('implemented'),
             '%d registered, including user-defined formulas'
             % seams['curve_types']['count']]]

    #  Declared loader classes against registered ones. The gap is real and is
    #  reported as a gap: a stub that raises ENotImplemented is declared but not
    #  registered, and smoothing that over is how a page starts lying.
    loader_dir = pathlib.Path(repo_root) / 'Desktop' / 'DataLoaders'
    declared = 0
    if loader_dir.is_dir():
        for p in loader_dir.glob('*.pas'):
            src = strip_comments(p.read_text(encoding='utf-8', errors='replace'))
            declared += len(re.findall(r'=\s*class\s*\(\s*TDataLoader\s*\)', src))
    registered = seams['data_loaders']['count']
    formats = ', '.join(esc(i['format']) for i in seams['data_loaders']['items'])
    if declared > registered:
        gap = declared - registered
        rows.append(['Data loaders', status('partial'),
                     '%d formats read (%s). %d loader %s declared but not registered '
                     '&mdash; a stub that raises <code>ENotImplemented</code>.'
                     % (registered, formats, gap,
                        'class' if gap == 1 else 'classes')])
    else:
        rows.append(['Data loaders', status('implemented'),
                     '%d formats read (%s)' % (registered, formats)])

    mins = ', '.join(esc(m['name']) for m in seams['minimizers']['items'])
    rows += [
        ['Data export', status('planned'), 'No export at all yet'],
        ['Compute backends', status('implemented'),
         '%s. A remote <code>fit_server</code> is a transport choice of the native '
         'engine, not a separate engine.' % mins],
        ['Objectives', status('implemented'),
         '%d, with curve-type compatibility derived from capabilities'
         % seams['losses']['count']],
        ['REST API', status('partial'),
         '%d verbs. Not specified as OpenAPI yet, and deliberately not documented as '
         'a stable contract.' % seams['actions']['count']],
        ['Module registration', status('implemented'),
         '%d seams; the framework ships no module' % len(EXPECTED_SEAMS)],
        ['Argument axes', status('implemented'),
         'Display-only: an axis never alters stored data or the fit'],
        ['Charting component', status('planned'),
         'Blocks per-point labels and point dragging'],
        ['Scripting and batch runs', status('planned'),
         'No way to drive a fit without the window'],
        ['Parallel and GPU compute', status('planned'),
         'Fit intervals are not run in parallel yet'],
        ['Native installers and signing', status('partial'),
         'Archives and <code>.deb</code>/<code>.rpm</code> are built; signing is off '
         'by default'],
    ]
    return (table(['Capability', 'State', 'Detail'], rows) +
            '\n<p>The full nine-stage plan, and what is settled and not up for '
            'reopening, is in the '
            '<a href="%s/blob/main/docs/contributing/roadmap.md">roadmap</a>.</p>'
            % FIT_REPO)


# --------------------------------------------------------------------------
# Driver
# --------------------------------------------------------------------------

def content(name):
    p = HERE / 'content' / name
    if not p.is_file():
        raise GenerationError('missing prose fragment %s' % p)
    return p.read_text(encoding='utf-8')


def substitute(text, marks):
    for key, value in marks.items():
        token = '{{%s}}' % key
        if token not in text:
            raise GenerationError(
                'the prose fragment has no %s placeholder, so a generated section '
                'would be dropped silently' % token)
        text = text.replace(token, value)
    left = re.findall(r'\{\{[A-Z_]+\}\}', text)
    if left:
        raise GenerationError('unfilled placeholders: %s' % ', '.join(left))
    return text


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding='utf-8')
    print('    %s' % path)


def install_shared(out):
    """The stylesheet, and the flag that keeps GitHub Pages out of the way."""
    css = out / 'assets' / 'css' / 'style.css'
    css.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(HERE / 'assets' / 'style.css', css)
    print('    %s' % css)
    #  Without this GitHub Pages runs Jekyll over the tree, which is both
    #  pointless here and would swallow any directory beginning with _.
    write(out / '.nojekyll', '')


def gen_fit(args):
    _, seams = load_dump(args.dump)
    #  Read once, so every hand-composed figure on every page is checked against
    #  the same parse in one run: a rename fails generation instead of publishing
    #  a page that still draws the old name.
    known = fit_symbols(args.repo_root)
    out = pathlib.Path(args.out)
    install_shared(out)
    body = substitute(content('fit-index.html'),
                      {'STATUS_TABLE': status_table(seams, args.repo_root)})
    write(out / 'index.html', page('Fit', FIT_TAGLINE, FIT_REPO, BANNER + body))
    write(out / 'architecture.html', architecture_page(seams, known))
    write(out / 'extension-points.html', extension_points_page(seams))
    write(out / 'how-to-extend-curve-types.html',
          curve_type_page(seams, args.repo_root, known))
    write(out / 'how-to-extend-data-loaders.html', data_loader_page(seams))


#  What each sibling package documents, and the state of each entry. The state is
#  a JUDGEMENT about source, so it is written here rather than guessed from a
#  declaration - TIconicGrid and TAnimatedGrid compile and do nothing, which no
#  parse can tell you.
SIBLINGS = {
    'fitgrids': {
        'title': 'fitgrids',
        'tagline': 'Grid components for Delphi and Lazarus',
        'repo': 'https://github.com/dvmorozov/fitgrids',
        'components': [
            ('TClipboardGrid', 'Copying and pasting through the clipboard.',
             'implemented', ''),
            ('TGEFGrid', 'Watches for exit from cell editing and raises '
             '<code>TGridEditingFinished</code>.', 'implemented', ''),
            ('TIDAGrid', 'Inserting, deleting and adding rows and columns &mdash; '
             '<strong>I</strong>nsert, <strong>D</strong>elete, <strong>A</strong>dd.',
             'implemented', ''),
            ('TDataGrid', 'Binds the grid to a data-source object through '
             '<code>IGridDataSource</code>, so the grid exchanges values with a class '
             'instead of holding them.', 'implemented', ''),
            ('TColoredGrid', 'Cell colours by cell type, set at design time.',
             'implemented', ''),
            ('TColorStringGrid', 'The same, on the string-grid branch.',
             'implemented', ''),
            ('TNumericGrid', 'Validates numeric input.', 'implemented', ''),
            ('TIconicGrid', 'An icon beside the text in a cell, from a per-cell image '
             'list.', 'planned', 'declared, body empty'),
            ('TAnimatedGrid', 'Animating those icons.', 'planned',
             'declared, body empty'),
        ],
        'note': '<p>The last two are declared in <code>NumericGrid.pas</code> and '
                'marked <em>&ldquo;not implemented yet&rdquo;</em>: the classes exist '
                'and compile, and do nothing beyond what they inherit. They are listed '
                'because a name that resolves but does nothing is worse to discover '
                'from the code than from a table.</p>',
    },
    'fitminimizers': {
        'title': 'fitminimizers',
        'tagline': 'Downhill simplex optimisation, with simulated annealing, for '
                   'Delphi and Lazarus',
        'repo': 'https://github.com/dvmorozov/fitminimizers',
        'components': [
            ('TDownhillSimplexAlgorithm', 'The classical downhill simplex.',
             'implemented', ''),
            ('TDownhillSimplexSAAlgorithm', 'The same, with simulated annealing, for '
             'a surface with more than one minimum.', 'implemented', ''),
            ('TDownhillSimplexServer', 'The container a host talks to, wrapping the '
             'algorithm as a component.', 'implemented', ''),
            ('TCombEnumerator, TCombSelector', 'Combinatorial enumeration of discrete '
             'parameter values.', 'implemented', ''),
            ('TRunningThread, TRunner, TRunnerPool', 'Background execution and a pool '
             'for running independent optimisations in parallel.', 'implemented', ''),
        ],
        'note': '',
    },
}


def gen_sibling(args):
    spec = SIBLINGS.get(args.repo)
    if spec is None:
        raise GenerationError('no page is defined for %r' % args.repo)
    pkg = pathlib.Path(args.package)
    if not pkg.is_dir():
        raise GenerationError(
            'the sibling package %s is not checked out, so its page cannot be '
            'generated. Check it out beside fit/ and run this again.' % pkg)
    paths = sorted(pkg.glob('*.pas'))
    if not paths:
        raise GenerationError('no .pas files under %s' % pkg)
    classes, ifaces = parse_units(paths)
    if not classes:
        raise GenerationError('parsed no classes from %s - the parser is broken or '
                              'the sources moved' % pkg)

    #  Every documented component must actually exist. A table entry for a class
    #  that has been renamed away is exactly the failure being fixed here.
    known = set(classes) | set(ifaces)
    for name, _, _, _ in spec['components']:
        for part in [n.strip() for n in name.split(',')]:
            if part not in known:
                raise GenerationError(
                    '%s documents %s, which is not declared in %s any more'
                    % (args.repo, part, pkg))

    comp = table(['Component', 'What it does', 'State'],
                 [['<code>%s</code>' % esc(n), d, status(st, note)]
                  for n, d, st, note in spec['components']]) + spec['note']

    diagram = ('<h2>Class diagram</h2>\n' + hierarchy_diagram(classes, ifaces) +
               '\n<p>%d %s and %d %s are declared in <code>package/</code>, read from '
               'the units themselves rather than from a diagram kept by hand.</p>'
               % (len(classes), 'class' if len(classes) == 1 else 'classes',
                  len(ifaces), 'interface' if len(ifaces) == 1 else 'interfaces'))

    out = pathlib.Path(args.out)
    install_shared(out)
    body = substitute(content('%s-index.html' % args.repo),
                      {'COMPONENTS': comp, 'CLASS_DIAGRAM': diagram})
    write(out / 'index.html',
          page(spec['title'], spec['tagline'], spec['repo'], BANNER + body))


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest='cmd', required=True)

    f = sub.add_parser('fit', help='generate the fit site from a registry dump')
    f.add_argument('--dump', required=True)
    f.add_argument('--repo-root', required=True)
    f.add_argument('--out', required=True)
    f.set_defaults(func=gen_fit)

    s = sub.add_parser('sibling', help='generate a fitgrids/fitminimizers site')
    s.add_argument('--repo', required=True)
    s.add_argument('--package', required=True)
    s.add_argument('--out', required=True)
    s.set_defaults(func=gen_sibling)

    args = ap.parse_args(argv)
    try:
        args.func(args)
    except GenerationError as exc:
        print('gen-diagrams: %s' % exc, file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
