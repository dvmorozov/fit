// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Everything this module contributes, in one call.)

A MODULE'S FRONT DOOR, and a worked example of the two newest contribution
points: a FILE FORMAT and a DATA SOURCE. Both go through the framework's own
registries, called from here - the framework names neither this module nor
anything in it.

The uses clause below is what LINKS these units. Unlike a curve type, a loader
and a source are registered by an explicit call rather than from an
initialization section, so the call IS the link: leaving one out fails the
build rather than producing a binary quietly short of a format.

WHAT THIS MODULE IS FOR. Spectroscopy: the JCAMP-DX interchange format, and the
public collection that publishes spectra in it. A field's formats and a field's
sources belong to the module that covers that field, which is why neither is in
the framework.
}
unit open_data_spectra_module;

{$mode objfpc}{$H+}

interface

{ Registers everything this module contributes. Idempotent, so a host that calls
  it twice - or a test that calls it after the application already has - is
  fine. }
procedure RegisterOpenDataSpectraModule;

implementation

uses
    data_loader_registry, data_source_registry,
    jcamp_dx_loader, nist_webbook_source, spectra_explanations;

procedure RegisterOpenDataSpectraModule;
begin
    //  EVERYTHING A MODULE ADDS EXPLAINS ITSELF, and first: the registry walk
    //  over the sources asks whether their topics resolve.
    RegisterSpectraExplanations;
    //  The format, before the source that produces it: a source declares which
    //  kinds of file it delivers, and what can read one is the loader
    //  registry's answer.
    RegisterDataLoader(TJcampDxLoader, JcampExtensions, JcampFormatName);
    RegisterDataSource(TNistWebBookSource);
    //  NO ExpectCurveTypes: this module registers no curve type. What would be
    //  checked is that its units linked, and the calls above are what link
    //  them - a missing one is a compile error rather than a silent absence.
end;

end.
