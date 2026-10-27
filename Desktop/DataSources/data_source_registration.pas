// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which data sources this build ships, and what links them.)

THIS IS data_loader_registration'S SHAPE, for the same reasons: naming each
source here is what LINKS it, the call verifies the outcome in the binary that
is actually running, and it is deliberately not an initialization section -
that runs only when the unit is linked, which is the property being checked.

A MODULE'S SOURCES ARE NOT LISTED HERE. They arrive through the module's own
front door, which calls RegisterDataSource for each of them. This file is the
framework's own set: the two that name no field and need no service of
anyone's. The DOI source was the third until 2026-10-20; it is the
published-data module's now, because a product must be able to leave it out. A source for a
particular field - a mineral database, a price feed - belongs to the module that
owns that field, and the framework never learns its name.
}
unit data_source_registration;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source_registry;

{ Registers every source this build's framework ships and checks the result.
  Idempotent, so a second call - a test, a re-initialised client - is harmless.

  Called at start-up beside RegisterAllDataLoaders. AFTER it, and that ordering
  matters: a source declares which file extensions it produces and the
  completeness walk asks the loader registry about each one. }
procedure RegisterAllDataSources;

implementation

uses
    samples_source, url_source;

var
    Registered: boolean = False;

procedure RegisterAllDataSources;
begin
    if Registered then
        Exit;

    //  Offline first, and deliberately: it is the source that works in a build
    //  with no connection, and the one the end-to-end test drives.
    RegisterDataSource(TSamplesSource);
    RegisterDataSource(TUrlSource);

    Registered := True;
end;

end.
