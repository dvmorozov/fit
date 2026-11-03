// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Who builds the curves for a point set the user marked.)

The engine has one decision to make before it can fit: where the curves come
from. A type placed from a single curve position takes the built-in path. A type
placed by MARKING AN EXTENT cannot - a pair of picks may be one shape, a chain,
or a hierarchy, and only whoever defined that point set knows which.

WHY A REGISTRY AND NOT A METHOD ON THE CURVE CLASS. That was the first shape
tried, and it fails for a concrete reason worth keeping: building needs the
ENGINE, and a curve class is compiled by the light test suite, which builds
without the widget set on purpose so the model can be tested in seconds. Putting
the builder on the class dragged the whole LCL into that suite through
mscr_specimen_list.

Keying it by POINT SET rather than by class is also the truer statement: the
builder interprets the marks, and several curve types may share one way of being
marked.
}
unit curve_builder_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { Builds every curve the marked point set describes, into ATask.

      ATask is a TFitTask and AStoredValues a TMSCRCurveList - the values the
      last fit found, which the builder passes back to AddBuiltCurve so each
      curve is handed its own again. Both are passed as TObject so this unit
      names no engine type and can be reached from either side.

      Returns True when the builder has dealt with it, INCLUDING when it refused
      or deliberately built nothing. False sends the engine down the
      position-based path, which with nothing marked builds one curve per data
      point - what a user once saw as a hang. }
    TCurveBuilder = function(ATask, AStoredValues: TObject): boolean;

    ECurveBuilderRegistration = class(Exception);

{ Registers the builder for a point set. Raises on a duplicate: two builders for
  one set would be resolved by registration order, and the loser would be dead
  code that still looks installed. }
procedure RegisterCurveBuilder(const APointSetName: string;
    ABuilder: TCurveBuilder);

{ The builder for a point set, or False when nothing claims it - which is the
  ordinary case in a build that contains no module. }
function FindCurveBuilder(const APointSetName: string;
    out ABuilder: TCurveBuilder): boolean;

{ How many builders are registered. Zero in a build with no module, which is the
  ordinary case - so a caller reporting this must say "no module registered one"
  rather than treating it as a fault. }
function CurveBuilderCount: longint;

implementation

type
    TEntry = record
        PointSet: string;
        Builder:  TCurveBuilder;
    end;

var
    Entries: array of TEntry;

function CurveBuilderCount: longint;
begin
    Result := Length(Entries);
end;

function FindCurveBuilder(const APointSetName: string;
    out ABuilder: TCurveBuilder): boolean;
var
    i: longint;
begin
    Result := False;
    ABuilder := nil;
    for i := 0 to High(Entries) do
        if Entries[i].PointSet = APointSetName then
        begin
            ABuilder := Entries[i].Builder;
            Exit(True);
        end;
end;

procedure RegisterCurveBuilder(const APointSetName: string;
    ABuilder: TCurveBuilder);
var
    Existing: TCurveBuilder;
begin
    if APointSetName = '' then
        raise ECurveBuilderRegistration.Create(
            'a curve builder was registered without naming a point set');
    if not Assigned(ABuilder) then
        raise ECurveBuilderRegistration.Create(
            'no builder was given for "' + APointSetName + '"');
    if FindCurveBuilder(APointSetName, Existing) then
    begin
        //  Registered from every host, so a second identical registration is
        //  ordinary; two DIFFERENT builders for one set are not.
        if Existing = ABuilder then
            Exit;
        raise ECurveBuilderRegistration.Create(
            'two different builders claim the "' + APointSetName + '" point set');
    end;

    SetLength(Entries, Length(Entries) + 1);
    Entries[High(Entries)].PointSet := APointSetName;
    Entries[High(Entries)].Builder := ABuilder;
end;

end.
