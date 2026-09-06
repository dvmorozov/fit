// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place that says which curve types this application ships.)

THE DEFECT THIS EXISTS TO END. Every curve type registers itself from its unit's
`initialization` section. A unit's initialization only runs if the unit is
LINKED, and a unit is only linked if something `uses` it - so a curve type was
present in a binary exactly when some unrelated file happened to name it. Nothing
declared that dependency, nothing checked it, and nothing reported it.

It failed exactly that way. A pack's types were linked into the desktop
application and into the test binary, but NOT into `fit_server`, which is what
actually creates curves. Selecting one of its types there did nothing at all -
silently - and the fit fell through to auto-mode with one curve per data point,
101 of them, which presented to the user as a hang. Every test passed throughout,
because the test binary linked the units.

Two hand-maintained lists existed and neither was checked: one in
`fit_server.lpr` (which was the one missing that pack's units) and a later patch
in `fit_task.pas` with a comment asking the reader not to remove it. Both work
and both are undiscoverable: nothing connects such a line to what it protects, no
reader can tell which types are meant to be present, and the identical omission
for the next curve type added would be just as silent.

WHAT REPLACES THEM. Two things, and the second is the one that matters:

  1. This unit `uses` every curve unit, so linking is a stated dependency in a
     file whose whole purpose is to state it, rather than a side effect of an
     engine unit's import list.

  2. `RegisterAllCurveTypes` VERIFIES the outcome and RAISES, naming what is
     missing. Linking is a build-time property, and no test running INSIDE a
     binary can check it on another binary's behalf - a test asserting "the
     a pack's types are registered" passes trivially in a test binary that links
     them, which is exactly why the original defect survived a green suite. A
     check that runs at STARTUP, in whichever binary is actually running, is the
     only kind that can fail in the binary that has the problem.

So a missing curve type now stops the program with the name of what is missing,
instead of degrading into a silent misfit (D26).

ADDING A CURVE TYPE: add its unit to `uses` and one `Add` line to
`ExpectedTypes`. If either is forgotten, start-up says so by name.
}
unit curve_type_registration;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, curve_types_singleton, named_points_set,
    int_curve_factory,
    //  Every curve type this application ships. Naming them here is what links
    //  them; the list in ExpectedTypes is what proves it worked.
    gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
    asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
    voigt_points_set, doniach_sunjic_points_set, emg_points_set,
    skewed_gaussian_points_set, moffat_points_set, pearson7_points_set,
    step_points_set, user_points_set;

{ Makes sure every curve type this application ships is present, and raises when
  one is not.

  Call it once, early, in EVERY binary that creates curves - the client, the
  server and the test runner. It only checks, so calling it twice is harmless. }
procedure RegisterAllCurveTypes;

{ The class names of every type expected to be present. Exposed so a test can
  assert the list is not empty and a diagnostic can print it. }
function ExpectedCurveTypeNames: TStringList;

{ Declares that a PACK's curve types must also be present.

  A pack - an analysis vertical, a plug-in set - names its own types, because
  this file cannot: it ships with the framework and a pack may not be part of
  this build at all. Call it from the pack's own registration unit, whose uses
  clause is what links those units in the first place.

  Idempotent per pack name, so a pack registered from both the client and the
  compute server is checked once. Raises when a pack registers no types, which
  would make the call a no-op that reads like a guard. }
procedure ExpectCurveTypes(const APackName: string;
    const AClasses: array of TCurveClass);

implementation

type
    TExpectedType = record
        { What to call it when it is missing. A CLASS name rather than the
          display name: whoever reads the failure is looking for a unit. }
        Name: string;
        { The identity the factory is keyed by. }
        Id:   TCurveTypeId;
    end;
    TExpectedTypes = array of TExpectedType;

    { What a pack contributed: the class, and the name to report if it is
      missing. }
    TPackType = record
        Name: string;
        Cls:  TCurveClass;
    end;

var
    PackTypes: array of TPackType;
    PackNames: TStringList = nil;

{ Every curve type that must be present.

  A missing entry means a type can go absent without complaint; a stale entry
  means start-up fails until it is removed. That asymmetry is deliberate and the
  right way round - the second is noticed within seconds, the first is not
  noticed for months. }
function ExpectedTypes: TExpectedTypes;
var
    N, i: longint;

    procedure Add(const AName: string; AClass: TCurveClass);
    begin
        SetLength(Result, N + 1);
        Result[N].Name := AName;
        Result[N].Id := AClass.GetCurveTypeId;
        Inc(N);
    end;

begin
    Result := nil;
    N := 0;
    //  The framework's own types. A pack's are appended below, from whatever
    //  registered - this file must not name them, because a pack may not be
    //  part of the build.
    Add('TGaussPointsSet', TGaussPointsSet);
    Add('TLorentzPointsSet', TLorentzPointsSet);
    Add('TPseudoVoigtPointsSet', TPseudoVoigtPointsSet);
    Add('TAsymPseudoVoigtPointsSet', TAsymPseudoVoigtPointsSet);
    Add('T2BranchesPseudoVoigtPointsSet', T2BranchesPseudoVoigtPointsSet);
    Add('TVoigtPointsSet', TVoigtPointsSet);
    Add('TDoniachSunjicPointsSet', TDoniachSunjicPointsSet);
    Add('TEMGPointsSet', TEMGPointsSet);
    Add('TSkewedGaussianPointsSet', TSkewedGaussianPointsSet);
    Add('TMoffatPointsSet', TMoffatPointsSet);
    Add('TPearson7PointsSet', TPearson7PointsSet);
    Add('TStepPointsSet', TStepPointsSet);
    Add('TUserPointsSet', TUserPointsSet);

    for i := 0 to High(PackTypes) do
        Add(PackTypes[i].Name, PackTypes[i].Cls);
end;

procedure ExpectCurveTypes(const APackName: string;
    const AClasses: array of TCurveClass);
var
    i, N: longint;
begin
    if APackName = '' then
        raise Exception.Create('a curve-type pack was declared with no name');
    if Length(AClasses) = 0 then
        //  A pack that expects nothing is a check that cannot fail, which is
        //  worse than no check: it reads like a guard in review.
        raise Exception.CreateFmt(
            'pack "%s" declared no curve types, so nothing would be verified',
            [APackName]);

    if not Assigned(PackNames) then
        PackNames := TStringList.Create;
    //  Registered from every host that creates curves, so the second call must
    //  not double the list.
    if PackNames.IndexOf(APackName) >= 0 then
        Exit;
    PackNames.Add(APackName);

    N := Length(PackTypes);
    SetLength(PackTypes, N + Length(AClasses));
    for i := 0 to High(AClasses) do
    begin
        if not Assigned(AClasses[i]) then
            raise Exception.CreateFmt(
                'pack "%s" declared a curve type that is nil', [APackName]);
        PackTypes[N + i].Name := AClasses[i].ClassName;
        PackTypes[N + i].Cls := AClasses[i];
    end;
end;


function ExpectedCurveTypeNames: TStringList;
var
    E: TExpectedTypes;
    i: longint;
begin
    E := ExpectedTypes;
    Result := TStringList.Create;
    for i := 0 to High(E) do
        Result.Add(E[i].Name);
end;

procedure RegisterAllCurveTypes;
var
    E: TExpectedTypes;
    Missing: string;
    i: longint;
begin
    //  Registration itself has already happened, in each unit's initialization -
    //  naming those units above is what makes it happen. What remains is to
    //  confirm it, in THIS binary, which is the part that was never done.
    E := ExpectedTypes;
    Missing := '';
    for i := 0 to High(E) do
        if FindCurveClassById(E[i].Id) = nil then
        begin
            if Missing <> '' then
                Missing := Missing + ', ';
            Missing := Missing + E[i].Name;
        end;

    if Missing <> '' then
        //  Every missing type at once, not just the first: when a link line is
        //  dropped a whole pack goes together, and reporting one at a time turns
        //  a single diagnosis into several.
        raise Exception.CreateFmt(
            'These curve types are not present in this binary: %s. They register ' +
            'themselves from their unit initialization, so this means the unit ' +
            'was not linked - add it to the uses clause of ' +
            'curve_type_registration and rebuild.', [Missing]);
end;

end.
