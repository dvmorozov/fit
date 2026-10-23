// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Everything this module contributes, in one call.)

WHAT THIS MODULE IS FOR. Data deposited in public repositories - Zenodo and
figshare - found by DOI or by words. It was the framework's own DOI source
until 2026-10-20; it became a module because a product must be able to leave a
feature out, and a feature leaves a product only by living in a module
(AGENTS.md, "Products"): a price-analysis product has no use for published
research data.

FIELD-NEUTRAL, AND THEREFORE NOT THE SCIENTIFIC MODULE. The repositories hold
every field's data, and a product for another field may want them without the
peak shapes. Formats a field reads stay in that field's module; this one only
finds and fetches.

The uses clause below is what LINKS these units: a source is registered by an
explicit call, so leaving one out fails the build rather than producing a
binary quietly short of it.
}
unit published_data_module;

{$mode objfpc}{$H+}

interface

{ Registers everything this module contributes. Idempotent, so a host that calls
  it twice - or a test that calls it after the application already has - is
  fine. }
procedure RegisterPublishedDataModule;

implementation

uses
    data_source_registry, doi_source, catalog_source,
    published_data_explanations;

procedure RegisterPublishedDataModule;
begin
    //  EVERYTHING A MODULE ADDS EXPLAINS ITSELF, and first: the registry walk
    //  over the sources asks whether their topics resolve.
    RegisterPublishedDataExplanations;
    RegisterDataSource(TDoiSource);
    //  The same repositories, browsed by subject rather than asked.
    RegisterDataSource(TCatalogSource);
end;

end.
