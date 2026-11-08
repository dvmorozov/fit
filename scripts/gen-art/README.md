# The sites' front-page pictures

Every site's front page shows, beside its name, a picture of what the site is
about. Each is **computed** by `hero_art.py` from real curves and written as
SVG - not drawn by hand, not taken from elsewhere, and not made by an
image-generation model.

| Site | Picture | Function | Written to |
|---|---|---|---|
| Fit | a measured profile decomposed into peak curves, their sum and the residual | `fit_art` | `fit/site/hero.svg` |
| fitgrids | a grid component: header row, tinted cells, a selected cell | `grids_art` | `fitgrids/site/hero.svg` |
| fitminimizers | an error landscape's contours and a downhill simplex closing in on the minimum | `min_art` | `fitminimizers/site/hero.svg` |

A module product draws its own in its own repository, with these helpers, so
it matches its siblings; the framework names no module, so its picture is not
listed here.

```sh
python3 scripts/gen-art/hero_art.py            # all three, into each project's site/
python3 scripts/gen-art/hero_art.py fit OUT    # one, to OUT
```

The build menu's *Preview sites* and *Publish* take the committed
`site/hero.svg` (New-SiteTree); the page generator shows it
(`gen_diagrams.hero_art`) and refuses a front page whose project lacks one.

## For an agent changing or adding a picture

- **Edit the script, then regenerate; never edit an SVG.** The noise is seeded
  (`random.seed`), so a run writes the committed files byte for byte, and
  `tools/build-tests/diagrams.tests.ps1` ("is drawn by the script kept for it")
  regenerates them and fails when a committed picture differs from what the
  script draws. Commit the script change and the regenerated SVG together.
- **Draw the subject's own mathematics.** The pictures are honest diagrams of
  what the program does - peaks that are real Gaussian/Lorentzian blends, a
  simplex that really shrinks towards the minimum. A new picture follows the
  same rule: compute it, do not decorate it.
- **The look is shared.** A 640x400 panel (`PANEL`) with rounded corners, made
  for the navy hero band of the shared theme (`fit/site/theme`); the theme's
  accents - teal `#14b8a6`, cyan `#38bdf8`, violet `#a78bfa`, amber `#f59e0b`,
  with the gradients `g1`-`g4`; system fonts only (`FONT`); the `glow` filter
  for the line that matters; everything clipped to the panel. No script, no
  external reference, no embedded raster.
- **Every picture says what it shows**: `svg(body, label)` takes the
  description screen readers announce, and the page shows the same words as
  the image's `alt` (`SIBLINGS[...]['art']` for a sibling, the alt passed to
  `hero_art` for Fit).
- **A new site** gets a function here, an entry in `ART` and `DESTINATIONS`,
  its `site/hero.svg` committed in its own repository, and its alt text where
  its page is generated.
- **Keep labels short and factual.** The pictures are read at a glance beside
  a headline; a sentence belongs in the page.
