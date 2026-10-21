<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# published-data — data deposited in public repositories

Finds data sets on Zenodo and figshare, lists only what Fit can read, and hands
the chosen file to the wizard of *File → New Project from Data Source*. Free Fit
and Fit Pro link it; a product with no use for published research data leaves
it out, which is why it is a module and not part of the framework.

| It contributes | Through | In |
|---|---|---|
| **Published data (DOI)**: a DOI, or words to search both repositories | `RegisterDataSource` | `doi_source.pas` |
| **Data catalog**: the same repositories, browsed by kind of file | `RegisterDataSource` | `catalog_source.pas` |
| what both ask the services, and how the answers are filtered | — | `published_repositories.pas`, `repository_filter.pas`, `repository_source.pas` |
| archives (`.zip`, `.gz`, `.tar`, `.tgz`) opened like folders, in memory | — | `archive_browser.pas`, `archive_members.pas` |
| an explanation of both sources | `RegisterExplanationProvider` | `published_data_explanations.pas` |

All of it is registered from one front door, `published_data_module.pas`.

## What it does, in examples

* `10.5281/zenodo.4892237` in the DOI box goes to that Zenodo record and lists
  its `.xrdml` scans; the record's papers and pictures are not listed.
* `calcite XRD` searches Zenodo and figshare for datasets holding a file Fit can
  read; each row says which repository, when, and how much readable data it holds.
* The Data catalog's row *PANalytical XRDML scan (.xrdml)* lists the newest
  datasets on both repositories holding such a file. The rows are the readers
  the build has, so a module that adds a format adds a row.

## What it does not do

* Only Zenodo and figshare are searched, a page at a time: 25 records from
  Zenodo (its limit without an account), 10 from figshare, each of whose
  records costs one more request because its search does not list files.
* A dataset is listed for the names of its files. Whether a `.csv` is a
  measurement is seen in the preview, before anything becomes a project.
* It finds and fetches; it does not identify, match or interpret.

## How the services are asked

Measured against the live services on 2026-10-20 and pinned by the tests'
recorded replies: Zenodo by `files.entries.key:(*.xrdml OR ...)` (its
`file_type` facet knows only the commonest types), figshare by a JSON search
with `:extension:` terms, posted from a file because Windows strips the quotes
from a command-line argument. The reasons are in the units' headers;
`docs/internal/roadmap.md` §7 records the decisions.
