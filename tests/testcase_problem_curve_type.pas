// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A problem's work follows its OWN curve type, never the process-wide selection.)

THE CURVE TYPE IS THE PROBLEM'S (TFitService.SetCurveType stores it there and
deliberately leaves the process-wide selector alone, so one problem's choice
cannot leak into another). One decision was still read from the global
selector: which extrema a curve-position search looks for. So finding curve
positions - and every automatic run, which starts with it - searched by
whatever type the process had selected last, anywhere: a peak type's problem
seeded curves at the dips once another problem, or the desktop menu in the same
process, had chosen a type that sits on both.

Red through REST, the way a client reaches it: the problem names a peak type,
the process selects one placed at maxima AND minima, and the positions found
must be the peaks only.
}
unit testcase_problem_curve_type;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson, jsonparser,
    fit_points_json,
    gauss_points_set, step_points_set, named_points_set, curve_types_singleton,
    int_curve_type_selector, testcase_rest_api;

type
    TProblemCurveTypeTest = class(TRestApiTestBase)
    private
        FSelectedBefore: TCurveTypeId;
    protected
        { This test MOVES the process-wide selection on purpose, and puts back
          what it found. }
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure CurvePositionsAreSearchedByTheProblemsOwnType;
        procedure ANewProblemStartsOnTheDefaultTypeWhateverIsSelected;
    end;

implementation

procedure TProblemCurveTypeTest.SetUp;
begin
    inherited SetUp;
    FSelectedBefore :=
        TCurveTypesSingleton.CreateCurveTypeSelector.GetSelectedCurveType;
end;

procedure TProblemCurveTypeTest.TearDown;
begin
    TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(FSelectedBefore);
    inherited TearDown;
end;

procedure TProblemCurveTypeTest.CurvePositionsAreSearchedByTheProblemsOwnType;
const
    Peak = 5.0;
    Dip = 15.0;
var
    Id, Code, i: longint;
    Prof, Got: TPointsData;
    Body: string;
begin
    //  Somebody else's choice: a step sits on maxima and minima alike.
    TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
        TStepPointsSet.GetCurveTypeId);

    Id := NewProblem;
    FApi.Handle('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code, Body);
    AssertEquals('the peak type is accepted', 200, Code);

    //  One peak and one dip on a flat zero.
    Prof := Default(TPointsData);
    SetLength(Prof.X, 41);
    SetLength(Prof.Y, 41);
    for i := 0 to 40 do
    begin
        Prof.X[i] := i * 0.5;
        Prof.Y[i] := 100 * Exp(-Sqr(Prof.X[i] - Peak) / 2) -
            100 * Exp(-Sqr(Prof.X[i] - Dip) / 2);
    end;
    FApi.Handle('PUT', Format('/problems/%d/profile', [Id]),
        PointsToJsonString(Prof), Code, Body);
    AssertEquals('the profile is accepted', 200, Code);

    FApi.Handle('POST', Format('/problems/%d/actions/compute-curve-positions',
        [Id]), '', Code, Body);
    AssertEquals('the positions are computed: ' + Body, 200, Code);
    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue('decoded', PointsFromJsonString(Body, Got));

    AssertTrue('the peak is found', Length(Got.X) > 0);
    for i := 0 to High(Got.X) do
        AssertTrue(Format('a Gaussian is not seeded at the dip (x = %g)',
            [Got.X[i]]), Abs(Got.X[i] - Dip) > 1.0);
end;

procedure TProblemCurveTypeTest.ANewProblemStartsOnTheDefaultTypeWhateverIsSelected;
var
    Id, Code: longint;
    Body: string;
    R: TJSONObject;
begin
    //  THE DEFECT, as it presented: Fit > Automatically on a new problem was
    //  refused with "No wave pattern has been marked yet" because a module's
    //  pattern type was selected elsewhere in the process, and a new problem
    //  took its type from that selection.
    TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
        TStepPointsSet.GetCurveTypeId);
    AssertFalse('the test needs a selection other than the default',
        IsEqualGUID(DefaultCurveTypeId, TStepPointsSet.GetCurveTypeId));

    Id := NewProblem;
    FApi.Handle('GET', Format('/problems/%d/settings', [Id]), '', Code, Body);
    AssertEquals('the settings are readable', 200, Code);
    R := TJSONObject(GetJSON(Body));
    try
        AssertEquals('a new problem starts on the registry''s default type',
            UpperCase(GUIDToString(DefaultCurveTypeId)),
            UpperCase(R.Get('curveType', '')));
    finally
        R.Free;
    end;
end;

initialization
    RegisterTest('unit', TProblemCurveTypeTest);
end.
