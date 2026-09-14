<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Project files

A **project** is your whole working context in one file: the data, everything you
have marked on it, the model, the parameters the last fit found, and the way the
window was set up. Save one, come back to it later, and **carry on fitting from
where you stopped** — not from the beginning.

That last part is the point of the format. A project does not merely re-draw what
you had; it restores the model's identity, so the values the optimiser found are
still attached to the curves they belong to and the next fit continues from them.

## Using them

| Menu | What it does |
|---|---|
| **File ▸ New Project** (Ctrl+N) | Closes the current project and starts empty |
| **File ▸ Open Project…** (Ctrl+O) | Opens a `.fitproj` |
| **File ▸ Open Recent ▸** | The last eight projects you opened, newest first |
| **File ▸ Save Project** (Ctrl+S) | Saves; asks for a name the first time |
| **File ▸ Save Project As…** | Saves under another name, asking first if that file already exists |
| **File ▸ Import Profile…** (F2) | Loads a data file into the current project, asking first if that would discard a model |
| **File ▸ Reload Profile** | Re-reads that data file, with the same question |
| **File ▸ Export ▸ …** | Writes a results table as text — see below |

**The project you had open last opens by itself next time.** Two command-line
switches override that: `/PROJECT=file.fitproj` opens that project, and
`/INFILE=file.dat` starts fresh with that data. If a remembered project has been
moved or deleted, Fit starts empty, says so in the log rather than failing to
start, and does not offer that one again - it also drops out of Open Recent.

## Save and Export are different things

**Save** writes the project. It can be opened again, and it is how you keep your
work.

**Export** writes a results table as tab-separated text for a spreadsheet or a
script. Nothing can open it again — it is a one-way copy of what is on screen.
There is one entry per table, so **Curve Parameters…** and **Summary Table…**
each write their own; neither depends on which tab happens to be in front, and
each is greyed out while its table is empty.

Exporting does **not** count as saving your work. If you export and then close,
Fit still asks about the project.

## What is in the file, and what is not

**Stored:** the profile as the program currently holds it (after any background
subtraction or smoothing), your background points, the fit intervals, the curve
positions you picked, every setting, the fitted parameter values with their
errors, the R-factor and statistics of the fit that was saved, and the working
context — argument axis, picking mode, active tab, the selected curve.

**Not stored, because it is recalculated when the project opens:** the calculated
profile, the difference curve, where the fitted curves sit, and the curves' own
points. Storing those would let a project disagree with its own model.

The data file's **path is recorded but not depended on**. The profile itself is
in the project, so it opens on a machine that has never seen the original file.

Window size and position are not in the project. They are yours, not the
document's, and a project opened on a different screen should not move your
window off it.

## The file itself

A `.fitproj` is a **ZIP archive of JSON parts** — the same arrangement `.xlsx`
and OpenDocument use — so you can open one with any archive tool and read what is
inside when you need to.

```
project.fitproj
├── manifest.json     which version wrote it
├── problem.json      the data and everything you marked on it
├── results.json      the fitted values, per curve
├── ui.json           how the window was set up
└── modules/…         anything an analysis pack keeps
```

**Old and new versions get along.** A part written by a newer version of Fit is
kept intact when an older one opens and saves the project, so passing a file
between versions does not quietly lose work. If a project genuinely needs a newer
Fit to be read correctly, it says so and refuses, rather than opening it
half-understood.
