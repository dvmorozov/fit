// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A curve drawn on what it rests on: through REST, as the chart gets it.)

A COMPONENT OF A SUM MAY BE A DEVIATION - a nested wave pattern is its own
wiggle about its parent's leg, exactly zero at both ends - and drawn as it is
computed it sits at the bottom of the chart, far from the parent it belongs to.
TNamedPointsSet.DrawnBaselineIn lets a curve type say what each of its points is
drawn on, and the points route and the progress frames carry it beside the
values (TPointsData.Baseline) without changing them.

THE RED TESTS ENTER WHERE THE APPLICATION DOES: the chart's curves come from
GET /curves/<cid>/points while the model is still, and from the snapshot in
GET /progress?snapshot=1 while a fit runs. The second path is the one a field
added to the first alone would have missed, which is why both are here.

The curve type is the test's own - a Gaussian that says it rests on a constant -
because the framework ships no type that rests on anything, and a module's must
not be named here.
}
unit testcase_drawn_baseline;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, fpcunit, testregistry, fpjson,
    fit_points_json, fit_progress_json, gauss_points_set, named_points_set,
    self_copied_component, curve_types_singleton, int_curve_type_iterator,
    int_curve_factory,
    testcase_rest_api;

type
    { A Gaussian drawn on a constant, as a nested component is drawn on its
      parent. Registered for the whole test process, so it inherits a complete
      explanation from the Gaussian rather than writing one of its own. }
    TBaselinedGaussPointsSet = class(TGaussPointsSet)
    public
        { The one-argument constructor the engine builds a registered type
          with (TFitTask.CreatePatternInstance's generic path), building the
          Gaussian's parameters; the engine then places it at its pick. }
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        function DrawnBaselineIn(AModel: TSelfCopiedCompList): TDoubleDynArray;
            override;
    end;

    TDrawnBaselineBase = class(TRestApiTestBase)
    protected
        { A problem whose one curve is of ATypeId, with its handle. }
        function ProblemWithOneCurveOf(const ATypeId: TGuid;
            out AHandle: string): longint;
        function CurvePointsBody(AId: longint; const AHandle: string): string;
    end;

    TDrawnBaselineRestTest = class(TDrawnBaselineBase)
    published
        procedure ACurvesPointsCarryWhatItIsDrawnOn;
        procedure AnOrdinaryCurveSendsNoBaseline;
    end;

    { THE SEAM'S COMPLETENESS WALK: every registered curve type answers either
      nothing or one value per point, so a module's type that answers out of
      step fails here by name rather than being dropped in silence on the
      server (TFitService.CurveDrawnBaseline). }
    TDrawnBaselineRegistryTest = class(TTestCase)
    published
        procedure EveryCurveTypeAnswersInStepOrNotAtAll;
    end;

    { THE FRAMES OF A RUNNING FIT, which reach the chart by another route. An
      integration test: it runs the optimiser. }
    TDrawnBaselineFrameTest = class(TDrawnBaselineBase)
    published
        procedure AnAnimatedFrameCarriesWhatACurveIsDrawnOn;
    end;

const
    { What the test type rests on, far from anything its own values reach. }
    TestBaseline = 1000.0;

implementation

const
    BaselinedGaussId: TGuid = '{6C1E6B0A-3D6F-4C43-9B7E-2E1F0D5A7B11}';

constructor TBaselinedGaussPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner, 0);
end;

class function TBaselinedGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian on a baseline (test)';
end;

class function TBaselinedGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := BaselinedGaussId;
end;

function TBaselinedGaussPointsSet.DrawnBaselineIn(
    AModel: TSelfCopiedCompList): TDoubleDynArray;
var
    i: longint;
begin
    SetLength(Result, PointsCount);
    for i := 0 to PointsCount - 1 do
        Result[i] := TestBaseline;
end;

function TDrawnBaselineBase.ProblemWithOneCurveOf(const ATypeId: TGuid;
    out AHandle: string): longint;
var
    Code: longint;
    R: TJSONObject;
    Reply: string;
begin
    Result := ProblemReadyForPicks;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(ATypeId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    R := Call('PUT', Format('/problems/%d/positions', [Result]),
        '{"x":[10],"y":[100]}', Code);
    try
        if Assigned(R) then
            Reply := R.AsJSON
        else
            Reply := '';
    finally
        R.Free;
    end;
    AssertEquals('the pick is accepted: ' + Reply, 200, Code);
    AHandle := HandlesOf(Result);
    AssertTrue('one curve', (AHandle <> '') and (Pos(',', AHandle) = 0));
end;

function TDrawnBaselineBase.CurvePointsBody(AId: longint;
    const AHandle: string): string;
var
    Code: longint;
begin
    FApi.Handle('GET', Format('/problems/%d/curves/%s/points', [AId, AHandle]),
        '', Code, Result);
    AssertEquals('the curve''s points are readable', 200, Code);
end;

procedure TDrawnBaselineRestTest.ACurvesPointsCarryWhatItIsDrawnOn;
var
    Id: longint;
    Handle: string;
    P: TPointsData;
    i: longint;
begin
    Id := ProblemWithOneCurveOf(BaselinedGaussId, Handle);
    AssertTrue('decoded', PointsFromJsonString(CurvePointsBody(Id, Handle), P));
    AssertTrue('there are points', Length(P.X) > 0);
    AssertEquals('one per point', Length(P.X), Length(P.Baseline));
    for i := 0 to High(P.Baseline) do
        AssertEquals(Format('point %d', [i]), TestBaseline, P.Baseline[i], 1e-9);
    //  And the values stay the curve's own: a Gaussian of height ~100.
    for i := 0 to High(P.Y) do
        AssertTrue(Format('value %d is the curve''s', [i]), P.Y[i] < TestBaseline / 2);
end;

procedure TDrawnBaselineRestTest.AnOrdinaryCurveSendsNoBaseline;
var
    Id: longint;
    Handle: string;
begin
    //  Additive: every curve this framework has rests on nothing, and its reply
    //  is the one it always was.
    Id := ProblemWithOneCurveOf(TGaussPointsSet.GetCurveTypeId, Handle);
    AssertEquals(0, Pos('baseline', CurvePointsBody(Id, Handle)));
end;

procedure TDrawnBaselineFrameTest.AnAnimatedFrameCarriesWhatACurveIsDrawnOn;
var
    Id, Code: longint;
    Handle, Body: string;
    R: TFitProgressReport;
begin
    Id := ProblemWithOneCurveOf(BaselinedGaussId, Handle);
    //  Asked for before the fit, as a client animating it asks: a snapshot is
    //  taken only while somebody wants one.
    FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '',
        Code, Body);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code).Free;
    AssertEquals('the fit ran', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '',
        Code, Body);
    AssertEquals('the progress is readable', 200, Code);
    AssertTrue('decoded', FitProgressFromJsonString(Body, R));
    AssertTrue('a frame was taken', R.HasSnapshot);
    AssertEquals('of one curve', 1, Length(R.Snapshot.Curves));
    AssertEquals('drawn on its baseline',
        Length(R.Snapshot.Curves[0].Points.X),
        Length(R.Snapshot.Curves[0].Points.Baseline));
    AssertEquals(TestBaseline, R.Snapshot.Curves[0].Points.Baseline[0], 1e-9);
end;

procedure TDrawnBaselineRegistryTest.EveryCurveTypeAnswersInStepOrNotAtAll;
var
    Iter: ICurveTypeIterator;
    Cls: TCurveClass;
    Inst: TNamedPointsSet;
    Model: TSelfCopiedCompList;
    Answer: TDoubleDynArray;
    Asked: longint;
begin
    Asked := 0;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        Cls := Iter.GetCurrentCurveClass;
        Inst := Cls.Create(nil);
        Model := TSelfCopiedCompList.Create;
        try
            //  Asked as the server asks: inside the model it belongs to. The
            //  list does not own the curve, which is freed below.
            Model.OwnsObjects := False;
            Model.Add(Inst);
            Answer := Inst.DrawnBaselineIn(Model);
            AssertTrue(Iter.GetCurveTypeName + ' answers nothing or one value ' +
                'per point', (Length(Answer) = 0) or
                (Length(Answer) = Inst.PointsCount));
            Inc(Asked);
        finally
            Model.Free;
            Inst.Free;
        end;
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
    AssertTrue('every type was asked', Asked > 1);
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TBaselinedGaussPointsSet);
    RegisterTest('unit', TDrawnBaselineRestTest);
    RegisterTest('unit', TDrawnBaselineRegistryTest);
    RegisterTest('integration', TDrawnBaselineFrameTest);
end.
