// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the window asks of the selected curve type, answered outside it.)

THE CASE THAT MATTERS IS "NO TYPE SELECTED". The window asks two questions of
the selected curve type before it offers an engine or an objective - has it a
formula, and is its amplitude free to grow - and asked them inline, each with
its own nil check. The two nil answers are deliberately DIFFERENT: with nothing
selected a curve counts as analytic (every engine can fit one, and greying an
entry before a type is chosen would be a refusal nobody could explain), but its
amplitude does not count as free (that refuses an objective, and nothing has
been chosen that could earn the refusal). Folding both onto one default is the
mistake these pin.

THE PROBES ARE DECLARED HERE rather than looked up in the registry, so the
answers do not depend on which types a build happens to contain. They are
asked at CLASS level only and never instantiated.
}
unit testcase_curve_capability;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    named_points_set, int_curve_factory, curve_types_singleton,
    gauss_points_set;

type
    { A type with no formula: what a pattern fitted from a table looks like. }
    TNonAnalyticProbe = class(TNamedPointsSet)
    public
        class function IsAnalytic: boolean; override;
    end;

    { A type whose amplitude may grow over orders of magnitude. }
    TFreeAmplitudeProbe = class(TNamedPointsSet)
    public
        class function AmplitudeIsUnbounded: boolean; override;
    end;

    TCurveCapabilityTest = class(TTestCase)
    published
        procedure WithNoTypeSelectedACurveCountsAsAnalytic;
        procedure ATypeWithAFormulaIsAnalytic;
        procedure ATypeThatSaysItHasNoFormulaIsNot;

        procedure WithNoTypeSelectedTheAmplitudeIsNotFree;
        procedure APeakTypesAmplitudeIsNotFree;
        procedure ATypeThatSaysItsAmplitudeIsFreeIs;

        procedure ARegisteredTypeIsKnown;
        procedure TheNullIdIsNotARegisteredType;
        procedure AnIdNobodyRegisteredIsNotKnown;
    end;

implementation

class function TNonAnalyticProbe.IsAnalytic: boolean;
begin
    Result := False;
end;

class function TFreeAmplitudeProbe.AmplitudeIsUnbounded: boolean;
begin
    Result := True;
end;

{ ---- has it a formula ------------------------------------------------------ }

procedure TCurveCapabilityTest.WithNoTypeSelectedACurveCountsAsAnalytic;
begin
    AssertTrue(CurveIsAnalytic(nil));
end;

procedure TCurveCapabilityTest.ATypeWithAFormulaIsAnalytic;
begin
    AssertTrue(CurveIsAnalytic(TGaussPointsSet));
end;

procedure TCurveCapabilityTest.ATypeThatSaysItHasNoFormulaIsNot;
begin
    //  The class's own answer is what decides, not the nil default above.
    AssertFalse(CurveIsAnalytic(TNonAnalyticProbe));
end;

{ ---- is its amplitude free ------------------------------------------------- }

procedure TCurveCapabilityTest.WithNoTypeSelectedTheAmplitudeIsNotFree;
begin
    //  The opposite default to CurveIsAnalytic's, and on purpose: this one
    //  refuses an objective, and nothing chosen yet could earn that.
    AssertFalse(CurveAmplitudeIsFree(nil));
end;

procedure TCurveCapabilityTest.APeakTypesAmplitudeIsNotFree;
begin
    AssertFalse(CurveAmplitudeIsFree(TGaussPointsSet));
end;

procedure TCurveCapabilityTest.ATypeThatSaysItsAmplitudeIsFreeIs;
begin
    AssertTrue(CurveAmplitudeIsFree(TFreeAmplitudeProbe));
end;

{ ---- is it in this build --------------------------------------------------- }

procedure TCurveCapabilityTest.ARegisteredTypeIsKnown;
begin
    AssertTrue(CurveTypeIsRegistered(TGaussPointsSet.GetCurveTypeId));
end;

procedure TCurveCapabilityTest.TheNullIdIsNotARegisteredType;
begin
    AssertFalse(CurveTypeIsRegistered(GUID_NULL));
end;

procedure TCurveCapabilityTest.AnIdNobodyRegisteredIsNotKnown;
begin
    //  What a settings file written by a newer build, or with a pack since
    //  removed, holds.
    AssertFalse(CurveTypeIsRegistered(
        StringToGUID('{6B1D2E44-0F3A-4C55-9E21-7A0C3D5B8F19}')));
end;

initialization
    RegisterTest('unit', TCurveCapabilityTest);
end.
