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
    test_curve_types, special_curve_parameter, calculated_curve_parameter,
    noise_rule;

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

    { A curve with four parameters the fit may vary, of which the first Used
      are in play: what a series shape - terms added or dropped - looks like to
      the engine. }
    TSeriesProbe = class(TNamedPointsSet)
    private
        FUsed: longint;
    protected
        function VariableSetStamp: longint; override;
        function ParameterIsInUse(AParameter: TSpecialCurveParameter): boolean;
            override;
    public
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function NoiseRule: TNoiseRuleClass; override;
        property Used: longint read FUsed write FUsed;
    end;

    TCurveCapabilityTest = class(TTestCase)
    published
        procedure TheVariableParametersFollowTheTermsInUse;
        procedure ACurveWithNoTermsToChooseAnswersNone;
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

constructor TSeriesProbe.Create(AOwner: TComponent);
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    inherited Create(AOwner);
    FUsed := 4;
    for i := 0 to 3 do
    begin
        P := TCalculatedCurveParameter.Create;
        P.Name := 'p' + IntToStr(i);
        P.Type_ := Variable;
        AddParameter(P);
    end;
    InitListOfVariableParameters;
end;

function TSeriesProbe.VariableSetStamp: longint;
begin
    Result := FUsed;
end;

function TSeriesProbe.ParameterIsInUse(AParameter: TSpecialCurveParameter): boolean;
begin
    Result := StrToInt(Copy(AParameter.Name, 2, 1)) < FUsed;
end;

class function TSeriesProbe.GetCurveTypeName: string;
begin
    Result := 'Series (test)';
end;

class function TSeriesProbe.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{7A1D3C52-5B0E-4E57-9C61-2B5E8F40D1A7}');
end;

class function TSeriesProbe.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestGaussPointsSet.NoiseRule;
end;

class function TNonAnalyticProbe.IsAnalytic: boolean;
begin
    Result := False;
end;

class function TFreeAmplitudeProbe.AmplitudeIsUnbounded: boolean;
begin
    Result := True;
end;

{ ---- which of its parameters are in play ----------------------------------- }

procedure TCurveCapabilityTest.TheVariableParametersFollowTheTermsInUse;
var
    C: TSeriesProbe;
begin
    //  A SERIES SHAPE'S TERMS CHANGE AFTER THE CURVE IS BUILT: a fitted value
    //  restored into it, a copy of it, a step of the automatic run. The fit
    //  reads the parameters it may vary from the curve, so the answer must follow
    //  the change without anyone asking the curve to rebuild its list.
    C := TSeriesProbe.Create(nil);
    try
        AssertEquals('all four to begin with', 4, C.VariableCount);
        C.Used := 2;
        AssertEquals('two in play', 2, C.VariableCount);
        AssertEquals('the first of them', 'p0', C.VariableNames[0]);
        C.Used := 4;
        AssertEquals('four again', 4, C.VariableCount);
    finally
        C.Free;
    end;
end;

procedure TCurveCapabilityTest.ACurveWithNoTermsToChooseAnswersNone;
var
    C: TNamedPointsSet;
begin
    C := TTestGaussPointsSet.Create(nil);
    try
        AssertEquals(0, C.TermCount);
        AssertEquals(0, C.MaxTermCount);
    finally
        C.Free;
    end;
end;

{ ---- has it a formula ------------------------------------------------------ }

procedure TCurveCapabilityTest.WithNoTypeSelectedACurveCountsAsAnalytic;
begin
    AssertTrue(CurveIsAnalytic(nil));
end;

procedure TCurveCapabilityTest.ATypeWithAFormulaIsAnalytic;
begin
    AssertTrue(CurveIsAnalytic(TTestGaussPointsSet));
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
    AssertFalse(CurveAmplitudeIsFree(TTestGaussPointsSet));
end;

procedure TCurveCapabilityTest.ATypeThatSaysItsAmplitudeIsFreeIs;
begin
    AssertTrue(CurveAmplitudeIsFree(TFreeAmplitudeProbe));
end;

{ ---- is it in this build --------------------------------------------------- }

procedure TCurveCapabilityTest.ARegisteredTypeIsKnown;
begin
    AssertTrue(CurveTypeIsRegistered(TTestGaussPointsSet.GetCurveTypeId));
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
