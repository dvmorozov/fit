// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The samples no curve covers, through REST, as the window gets them.)

FOUND IN USE: a model of compactly supported patterns whose first one began one
sample after the data scored 1.7E-5 over the whole profile and 2.5E-7 over two
intervals that left that sample out. The model was 0 there and its residual
was 68 times all the others together, and nothing the window read said so.

THE RED TEST ENTERS WHERE THE APPLICATION DOES: the window reads the fit's
statistics from GET /problems/<id>/stats, and that is what is asked here. The
curve type is the test's own (span_gauss_points_set), because the framework
ships no compactly supported type and a module's must not be named here.
}
unit testcase_uncovered_samples;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    gauss_points_set, span_gauss_points_set, rest_spec, testcase_rest_api;

type
    TUncoveredSamplesRestTest = class(TRestApiTestBase)
    private
        { A flat profile over x = 0..20, one interval over all of it, and one
          pick at 10 of ATypeId. }
        function ProblemWithOneCurveOf(const ATypeId: TGuid): longint;
        function StatisticsOf(AId: longint): TJSONObject;
    published
        procedure TheStatisticsNameTheSamplesNoCurveCovers;
        procedure AModelCoveringItsIntervalAddsNothingToTheReply;
        procedure EveryFieldItSendsIsDocumented;
    end;

implementation

function TUncoveredSamplesRestTest.ProblemWithOneCurveOf(
    const ATypeId: TGuid): longint;
var
    Code, i: longint;
    Profile: string;
begin
    Result := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(ATypeId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    //  FLAT, so the sample left out is as large as any other: what the
    //  window must name is a sample modelled as nothing, not a small one.
    Profile := '{"title":"profile","x":[';
    for i := 0 to 20 do
    begin
        if i > 0 then
            Profile := Profile + ',';
        Profile := Profile + IntToStr(i);
    end;
    Profile := Profile + '],"y":[';
    for i := 0 to 20 do
    begin
        if i > 0 then
            Profile := Profile + ',';
        Profile := Profile + '100';
    end;
    Profile := Profile + ']}';
    Call('PUT', Format('/problems/%d/profile', [Result]), Profile, Code).Free;
    AssertEquals('the profile is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        '{"x":[0,20],"y":[100,100]}', Code).Free;
    AssertEquals('the interval is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Result]),
        '{"x":[10],"y":[100]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
end;

function TUncoveredSamplesRestTest.StatisticsOf(AId: longint): TJSONObject;
var
    Code: longint;
    R: TJSONObject;
    D: TJSONData;
begin
    R := Call('GET', Format('/problems/%d/stats', [AId]), '', Code);
    try
        AssertEquals('the statistics are served', 200, Code);
        D := R.Find('statistics');
        AssertTrue('a statistics object: ' + R.AsJSON, D is TJSONObject);
        Result := TJSONObject(D.Clone);
    finally
        R.Free;
    end;
end;

procedure TUncoveredSamplesRestTest.TheStatisticsNameTheSamplesNoCurveCovers;
var
    S, Range: TJSONObject;
    Ranges: TJSONData;
    Share: double;
begin
    //  The curve exists on 1..20, so x = 0 - inside the interval - is no
    //  curve's.
    TSpanGaussPointsSet.SpanFrom := 1;
    TSpanGaussPointsSet.SpanTo := 20;
    S := StatisticsOf(ProblemWithOneCurveOf(
        TSpanGaussPointsSet.GetCurveTypeId));
    try
        AssertTrue('the model is measured: ' + S.AsJSON, S.Get('valid', False));
        AssertEquals('one sample no curve covers: ' + S.AsJSON, 1,
            S.Get('uncoveredSamples', 0));
        Ranges := S.Find('uncoveredRanges');
        AssertTrue('its stretch: ' + S.AsJSON,
            (Ranges is TJSONArray) and (TJSONArray(Ranges).Count = 1));
        Range := TJSONArray(Ranges).Objects[0];
        AssertEquals('from', 0, Range.Get('from', -1.0), 0);
        AssertEquals('to', 0, Range.Get('to', -1.0), 0);
        AssertEquals('count', 1, Range.Get('count', 0));
        Share := S.Get('uncoveredResidualShare', -1.0);
        AssertTrue('its part of the squared difference: ' + S.AsJSON,
            (Share > 0) and (Share <= 1));
    finally
        S.Free;
    end;
end;

procedure TUncoveredSamplesRestTest.AModelCoveringItsIntervalAddsNothingToTheReply;
var
    S: TJSONObject;
begin
    S := StatisticsOf(ProblemWithOneCurveOf(TGaussPointsSet.GetCurveTypeId));
    try
        AssertTrue('the model is measured: ' + S.AsJSON, S.Get('valid', False));
        AssertEquals('the reply is what it always was: ' + S.AsJSON, 0,
            Pos('uncovered', S.AsJSON));
    finally
        S.Free;
    end;
end;

procedure TUncoveredSamplesRestTest.EveryFieldItSendsIsDocumented;
var
    S, Doc, Props: TJSONObject;
    i: longint;
begin
    //  The optional fields are sent only here, so this is where the document
    //  is held to them; testcase_rest_spec holds it to the ones always sent.
    TSpanGaussPointsSet.SpanFrom := 1;
    TSpanGaussPointsSet.SpanTo := 20;
    S := StatisticsOf(ProblemWithOneCurveOf(
        TSpanGaussPointsSet.GetCurveTypeId));
    Doc := TJSONObject(GetJSON(OpenApiJson));
    try
        Props := Doc.Objects['components'].Objects['schemas'].
            Objects['Statistics'].Objects['properties'];
        AssertTrue('the reply names the samples', S.Count > 9);
        for i := 0 to S.Count - 1 do
            AssertTrue('documented: ' + S.Names[i],
                Props.Find(S.Names[i]) <> nil);
    finally
        Doc.Free;
        S.Free;
    end;
end;

initialization
    //  A UNIT test: the REST layer is driven in process, and placing a pick
    //  builds the model without running the optimiser.
    RegisterTest('unit', TUncoveredSamplesRestTest);
end.
