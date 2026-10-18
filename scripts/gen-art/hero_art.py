# SPDX-License-Identifier: GPL-3.0-or-later
"""The pictures each site's front page shows beside its name, computed from real
curves and written as SVG.

WHY A SCRIPT AND NOT A DRAWING. Each picture is what its site is about - a
profile decomposed into peaks for Fit, a grid component for fitgrids, a
downhill simplex on an error landscape for fitminimizers - drawn from the same
mathematics the programs use, so it is exact, has no licence question, and can
be changed by editing numbers. The random noise is seeded, so a run writes the
committed files byte for byte; diagrams.tests.ps1 regenerates them and fails
when the script and a committed picture disagree.

Usage (from fit/):

    python3 scripts/gen-art/hero_art.py            # writes all three pictures
    python3 scripts/gen-art/hero_art.py fit OUT    # one picture, to OUT

The default destinations are each project's site/hero.svg - fit/site,
../fitgrids/site, ../fitminimizers/site - where the build menu's Preview sites
and Publish take them from (New-SiteTree). A module product draws its own
picture in its own repository with these helpers (svg, path, grid, the palette),
so every site's picture looks like its siblings.

THE LOOK, shared by every picture: a 640x400 navy panel with rounded corners,
drawn for the navy hero band of the shared theme (fit/site/theme), with the
theme's accents - teal #14b8a6, cyan #38bdf8, violet #a78bfa, amber #f59e0b -
system fonts only, and no script.
"""
import math
import os
import random
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
FIT = os.path.dirname(os.path.dirname(HERE))
UMBRELLA = os.path.dirname(FIT)

W, H = 640, 400
PANEL = ('<rect x="0.5" y="0.5" width="639" height="399" rx="18" fill="#0d2440" fill-opacity=".72" '
         'stroke="#5eead4" stroke-opacity=".22"/>')
FONT = 'font-family="system-ui,-apple-system,Segoe UI,Roboto,sans-serif"'


def svg(body, label):
    return ('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" role="img" aria-label="%s">\n'
            '<defs>\n'
            '<linearGradient id="g1" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#14b8a6" stop-opacity=".55"/><stop offset="1" stop-color="#14b8a6" stop-opacity=".04"/></linearGradient>\n'
            '<linearGradient id="g2" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#38bdf8" stop-opacity=".5"/><stop offset="1" stop-color="#38bdf8" stop-opacity=".04"/></linearGradient>\n'
            '<linearGradient id="g3" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#a78bfa" stop-opacity=".5"/><stop offset="1" stop-color="#a78bfa" stop-opacity=".04"/></linearGradient>\n'
            '<linearGradient id="g4" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#f59e0b" stop-opacity=".45"/><stop offset="1" stop-color="#f59e0b" stop-opacity=".04"/></linearGradient>\n'
            '<filter id="glow" x="-10%%" y="-10%%" width="120%%" height="120%%"><feGaussianBlur stdDeviation="3" result="b"/><feMerge><feMergeNode in="b"/><feMergeNode in="SourceGraphic"/></feMerge></filter>\n'
            '<clipPath id="panel"><rect x="1" y="1" width="638" height="398" rx="17"/></clipPath>\n'
            '</defs>\n%s\n<g clip-path="url(#panel)">\n%s\n</g>\n</svg>\n' % (W, H, label, PANEL, body))


def path(points):
    return 'M' + ' L'.join('%.1f %.1f' % p for p in points)


def grid(x0, y0, x1, y1, nx, ny):
    lines = []
    for i in range(nx + 1):
        x = x0 + (x1 - x0) * i / nx
        lines.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>' % (x, y0, x, y1))
    for j in range(ny + 1):
        y = y0 + (y1 - y0) * j / ny
        lines.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>' % (x0, y, x1, y))
    return '<g stroke="#94bde6" stroke-opacity=".12" stroke-width="1">%s</g>' % ''.join(lines)


# ---- Fit: a profile decomposed into peaks --------------------------------------
def fit_art():
    random.seed(7)
    x0, x1, base, top = 50, 600, 300, 50
    peaks = [(0.24, 0.55, 0.055), (0.42, 1.0, 0.045), (0.53, 0.62, 0.05), (0.76, 0.8, 0.06)]
    fills = ['url(#g1)', 'url(#g2)', 'url(#g3)', 'url(#g4)']
    strokes = ['#14b8a6', '#38bdf8', '#a78bfa', '#f59e0b']
    bg = lambda t: 0.06 + 0.04 * t

    def comp(t, p):
        c, a, w = p
        g = math.exp(-0.5 * ((t - c) / w) ** 2)
        l = 1 / (1 + ((t - c) / (w * 1.18)) ** 2)
        return a * (0.6 * g + 0.4 * l)

    n = 220
    ts = [i / n for i in range(n + 1)]
    sx = lambda t: x0 + (x1 - x0) * t
    sy = lambda v: base - (base - top) * v / 1.12
    out = [grid(x0, top - 10, x1, base, 11, 5)]
    for p, f, s in zip(peaks, fills, strokes):
        pts = [(sx(t), sy(bg(t) + comp(t, p))) for t in ts]
        area = path(pts) + ' L%.1f %.1f L%.1f %.1f Z' % (sx(1), sy(bg(1)), sx(0), sy(bg(0)))
        out.append('<path d="%s" fill="%s"/>' % (area, f))
        out.append('<path d="%s" fill="none" stroke="%s" stroke-width="1.6" stroke-opacity=".9"/>' % (path(pts), s))
    total = [(sx(t), sy(bg(t) + sum(comp(t, p) for p in peaks))) for t in ts]
    out.append('<path d="%s" fill="none" stroke="#5eead4" stroke-width="1.2" stroke-dasharray="4 4" stroke-opacity=".6"/>'
               % path([(sx(t), sy(bg(t))) for t in ts]))
    dots = []
    for i in range(0, n + 1, 3):
        t = ts[i]
        v = bg(t) + sum(comp(t, p) for p in peaks) + random.gauss(0, 0.018)
        dots.append('<circle cx="%.1f" cy="%.1f" r="2.1"/>' % (sx(t), sy(v)))
    out.append('<g fill="#e6eef7" fill-opacity=".85">%s</g>' % ''.join(dots))
    out.append('<path d="%s" fill="none" stroke="#ffffff" stroke-width="2.6" filter="url(#glow)"/>' % path(total))
    # residual strip
    out.append('<line x1="%d" y1="350" x2="%d" y2="350" stroke="#94bde6" stroke-opacity=".35"/>' % (x0, x1))
    res = [(sx(t), 350 + random.gauss(0, 4)) for t in ts[::2]]
    out.append('<path d="%s" fill="none" stroke="#5eead4" stroke-width="1.2" stroke-opacity=".8"/>' % path(res))
    out.append('<text x="%d" y="332" %s font-size="12" fill="#a9bbd0">residual</text>' % (x0, FONT))
    out.append('<text x="%d" y="34" %s font-size="12" fill="#a9bbd0">profile = background + peaks</text>' % (x0, FONT))
    return svg('\n'.join(out), 'A measured profile decomposed into peak curves, with their sum and the residual')


# ---- fitgrids: a grid component ----------------------------------------------------------
def grids_art():
    random.seed(3)
    x0, y0, cw, rh, cols, rows = 50, 60, 108, 34, 5, 8
    out = []
    heads = ['Position', 'Amplitude', 'Width', 'Shape', 'Area']
    out.append('<rect x="%d" y="%d" width="%d" height="%d" rx="8" fill="#0b1d33" stroke="#94bde6" stroke-opacity=".3"/>'
               % (x0, y0, cw * cols, rh * (rows + 1)))
    out.append('<rect x="%d" y="%d" width="%d" height="%d" rx="8" fill="#14b8a6" fill-opacity=".22"/>' % (x0, y0, cw * cols, rh))
    for c, h in enumerate(heads):
        out.append('<text x="%d" y="%d" %s font-size="13" font-weight="700" fill="#e6eef7">%s</text>'
                   % (x0 + c * cw + 12, y0 + 22, FONT, h))
    tints = {(2, 1): '#38bdf8', (4, 3): '#a78bfa', (6, 2): '#f59e0b', (1, 4): '#14b8a6'}
    for r in range(rows):
        y = y0 + rh * (r + 1)
        if r % 2:
            out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="#ffffff" fill-opacity=".03"/>' % (x0, y, cw * cols, rh))
        for c in range(cols):
            x = x0 + c * cw
            if (r, c) in tints:
                out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="%s" fill-opacity=".28"/>' % (x + 1, y + 1, cw - 2, rh - 2, tints[(r, c)]))
            val = ['%.3f' % (10 + r * 7.31 + random.random()), '%.1f' % (100 * random.random()),
                   '%.4f' % random.random(), '%.2f' % random.random(), '%.1f' % (50 * random.random())][c]
            out.append('<text x="%d" y="%d" %s font-size="13" fill="#cfe3f5" fill-opacity=".9">%s</text>'
                       % (x + 12, y + 22, 'font-family="ui-monospace,SFMono-Regular,Menlo,monospace"', val))
    for c in range(1, cols):
        out.append('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#94bde6" stroke-opacity=".18"/>'
                   % (x0 + c * cw, y0, x0 + c * cw, y0 + rh * (rows + 1)))
    for r in range(1, rows + 1):
        out.append('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#94bde6" stroke-opacity=".12"/>'
                   % (x0, y0 + r * rh, x0 + cw * cols, y0 + r * rh))
    sx, sy = x0 + 2 * cw, y0 + rh * 4
    out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="none" stroke="#5eead4" stroke-width="2.5" filter="url(#glow)"/>'
               % (sx, sy, cw, rh))
    out.append('<rect x="%d" y="%d" width="7" height="7" fill="#5eead4"/>' % (sx + cw - 4, sy + rh - 4))
    out.append('<text x="%d" y="40" %s font-size="12" fill="#a9bbd0">clipboard, editing, data binding, validation</text>' % (x0, FONT))
    return svg('\n'.join(out), 'A grid component with a header row, tinted cells and a selected cell')


# ---- fitminimizers: a landscape and a simplex closing in -------------------------------------
def min_art():
    cx, cy = 400, 215
    out = [grid(40, 40, 600, 370, 11, 6)]
    for k in range(1, 9):
        a, b = 24 * k + 5 * k * k / 4, 14 * k + 2.6 * k * k / 4
        op = 0.55 - k * 0.045
        out.append('<ellipse cx="%d" cy="%d" rx="%.1f" ry="%.1f" transform="rotate(-24 %d %d)" fill="none" '
                   'stroke="%s" stroke-opacity="%.2f" stroke-width="1.4"/>'
                   % (cx, cy, a, b, cx, cy, '#38bdf8' if k % 2 else '#14b8a6', op))
    # a downhill simplex: triangles shrinking towards the minimum
    tri = [(110, 320), (175, 300), (140, 255)]
    simplexes = []
    for step in range(9):
        simplexes.append(list(tri))
        cxm = sum(p[0] for p in tri) / 3
        cym = sum(p[1] for p in tri) / 3
        f = 0.78
        tri = [(cx + (x - cx) * f + (cxm - x) * 0.15 + 22 * f ** step, cy + (y - cy) * f + (cym - y) * 0.15 - 8 * f ** step) for x, y in tri]
    for i, t in enumerate(simplexes):
        op = 0.25 + 0.75 * i / len(simplexes)
        out.append('<polygon points="%s" fill="#f59e0b" fill-opacity="%.2f" stroke="#fbbf24" stroke-opacity="%.2f" stroke-width="2"/>'
                   % (' '.join('%.1f,%.1f' % p for p in t), 0.22 * op, 0.45 + 0.55 * op))
    cents = [(sum(p[0] for p in t) / 3, sum(p[1] for p in t) / 3) for t in simplexes]
    out.append('<path d="%s" fill="none" stroke="#ffffff" stroke-width="1.6" stroke-dasharray="3 4" stroke-opacity=".8"/>' % path(cents + [(cx, cy)]))
    out.append('<circle cx="%d" cy="%d" r="7" fill="#5eead4" filter="url(#glow)"/>' % (cx, cy))
    out.append('<text x="%d" y="%d" %s font-size="12" fill="#e6eef7">minimum</text>' % (cx + 12, cy - 10, FONT))
    out.append('<text x="40" y="30" %s font-size="12" fill="#a9bbd0">downhill simplex on an error landscape</text>' % FONT)
    return svg('\n'.join(out), 'Contour lines of an error landscape and a downhill simplex closing in on its minimum')


ART = {'fit': fit_art, 'grids': grids_art, 'min': min_art}

#  Where each picture lives: its project's site/ folder.
DESTINATIONS = {
    'fit': os.path.join(FIT, 'site', 'hero.svg'),
    'grids': os.path.join(UMBRELLA, 'fitgrids', 'site', 'hero.svg'),
    'min': os.path.join(UMBRELLA, 'fitminimizers', 'site', 'hero.svg'),
}


def main(argv):
    pairs = list(zip(argv[0::2], argv[1::2])) if argv else [
        (name, dest) for name, dest in DESTINATIONS.items() if os.path.isdir(os.path.dirname(dest))]
    for name, dest in pairs:
        with open(dest, 'w', encoding='utf-8', newline='\n') as f:
            f.write(ART[name]())
        print(dest)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
