<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# open-data-spectra — a public module for spectroscopy data

A small, real module: it adds **one file format** and **one place to fetch data
from**, and the framework knows the name of neither.

| It contributes | Through | In |
|---|---|---|
| the JCAMP-DX spectrum format | `data_loader_registry.RegisterDataLoader` | `jcamp_dx_loader.pas`, `jcamp_asdf.pas` |
| the NIST Chemistry WebBook as a data source | `data_source_registry.RegisterDataSource` | `nist_webbook_source.pas`, `html_scan.pas` |
| an explanation of both | `RegisterExplanationProvider` | `spectra_explanations.pas` |

All three are registered from one front door, `open_data_spectra_module.pas`.
Nothing else about this directory is special, and no file outside it changed to
make it work — which is the property the module contract exists to have.

## Why it is worth reading

`Modules/example-linear/` is the smallest module there can be: one curve type.
This one is the next size up, and it shows the two things a module usually
needs that an example cannot:

* **a format of its own.** JCAMP-DX is a spectroscopy interchange format, so it
  belongs to a spectroscopy module and not to the framework. The decoder is
  separate from the loader because the compressed forms are a rule over a string
  that a test can reach with a literal;
* **a source that reads a service written for people.** The WebBook publishes no
  programming interface, so its pages are scanned. That is a liability, and it is
  stated as a limitation of the source rather than hidden: when the site changes,
  the source refuses in words instead of quietly finding nothing.

## Building and testing it

From the framework's tree:

    ./scripts/build-app.ps1 -Task test        # the framework, without this module
    lazbuild --widgetset=nogui Modules/open-data-spectra/fit_tests_spectra.lpi
    tests/fit_tests_spectra --all

Its three project files mirror the example's: a client, a compute server and a
test suite, each reusing the framework's own main program with this directory
first on the unit search path.

**No test here touches the network.** The answers are recorded — see
`tests/fixtures/README.md`, which says when, and with what command.
