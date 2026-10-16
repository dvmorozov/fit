// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A module settles its curves against each other before they are computed.)

WHY. Some curves are placed by others: a module's nested component sits on a
leg of its parent. The builder places it there when the model is built - but a
fit changes the parent on every evaluation and rebuilds nothing, so the
component stayed where it was first put, the fitted model broke the module's
own rule, and reopening the project (which rebuilds) measured a different
model (findings.md, "A fitted model did not reopen as it was saved"). A module
registers a settler; every task calls the settlers on its curves before it
computes them - in a fit, on every evaluation. The framework names no module.
}
unit testcase_model_settlers;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_rest_api, model_settlers, self_copied_component, gauss_points_set,
    SimpMath;

type
    TModelSettlersTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        { A problem holding one Gaussian peak and a pick on it; its id. }
        function APeakWithAPick: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFitSettlesTheModelBeforeEachEvaluation;
    end;

    { THE REGISTRY ITSELF, in memory: what a module's registration and its
      removal leave behind. Registering twice must not settle twice - a test
      registers beside the module that already did - and the order registered
      is the order settlers run in. }
    TModelSettlerRegistryTest = class(TTestCase)
    private
        FCurves: TSelfCopiedCompList;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure RegisteringTwiceSettlesOnce;
        procedure AnUnregisteredSettlerIsNotCalled;
        procedure UnregisteringOneKeepsTheOthersInTheirOrder;
        procedure UnregisteringOneNeverRegisteredChangesNothing;
    end;

implementation

var
    Settled, MostCurves: longint;

procedure CountSettling(ACurves: TSelfCopiedCompList);
begin
    Inc(Settled);
    if ACurves.Count > MostCurves then
        MostCurves := ACurves.Count;
end;

procedure TModelSettlersTest.SetUp;
begin
    FApi := TFitRestApi.Create;
    Settled := 0;
    MostCurves := 0;
    RegisterModelSettler(@CountSettling);
end;

procedure TModelSettlersTest.TearDown;
begin
    UnregisterModelSettler(@CountSettling);
    FreeAndNil(FApi);
end;

function TModelSettlersTest.Call(const M, P, B: string;
    out Code: longint): TJSONObject;
var
    Resp: string;
    D: TJSONData;
begin
    FApi.Handle(M, P, B, Code, Resp);
    D := GetJSON(Resp);
    if D is TJSONObject then
        Result := TJSONObject(D)
    else
    begin
        D.Free;
        Result := TJSONObject.Create;
    end;
end;

function TModelSettlersTest.APeakWithAPick: longint;
var
    Code, i: longint;
    R: TJSONObject;
    XS, YS: string;
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    R := Call('POST', '/problems', '', Code);
    Result := R.Get('id', 0);
    R.Free;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    XS := '';
    YS := '';
    for i := 0 to 40 do
    begin
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + FloatToStr(GaussPoint(100, 3, 20, i), FS);
    end;
    Call('PUT', Format('/problems/%d/profile', [Result]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code).Free;
    Call('POST', Format('/problems/%d/points/positions', [Result]),
        Format('{"x":21,"y":%s}', [FloatToStr(GaussPoint(100, 3, 20, 21), FS)]),
        Code).Free;
end;

procedure TModelSettlersTest.AFitSettlesTheModelBeforeEachEvaluation;
var
    Code, Id: longint;
begin
    Id := APeakWithAPick;
    Settled := 0;
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code).Free;
    AssertEquals('fitted', 200, Code);
    //  A fit evaluates many times; each evaluation settles first.
    AssertTrue(Format('settled %d times', [Settled]), Settled > 10);
    AssertEquals('with the task''s curves', 1, MostCurves);
end;

var
    { Which of the test's settlers ran, in order. Settlers a module registered
      in this process may run too; they write nothing here. }
    Ran: string;

procedure SettlerA(ACurves: TSelfCopiedCompList);
begin
    Ran := Ran + 'A';
end;

procedure SettlerB(ACurves: TSelfCopiedCompList);
begin
    Ran := Ran + 'B';
end;

procedure SettlerC(ACurves: TSelfCopiedCompList);
begin
    Ran := Ran + 'C';
end;

procedure TModelSettlerRegistryTest.SetUp;
begin
    FCurves := TSelfCopiedCompList.Create;
    Ran := '';
end;

procedure TModelSettlerRegistryTest.TearDown;
begin
    UnregisterModelSettler(@SettlerA);
    UnregisterModelSettler(@SettlerB);
    UnregisterModelSettler(@SettlerC);
    FreeAndNil(FCurves);
end;

procedure TModelSettlerRegistryTest.RegisteringTwiceSettlesOnce;
begin
    RegisterModelSettler(@SettlerA);
    RegisterModelSettler(@SettlerA);
    SettleModel(FCurves);
    AssertEquals('once per evaluation, however often registered', 'A', Ran);
end;

procedure TModelSettlerRegistryTest.AnUnregisteredSettlerIsNotCalled;
begin
    RegisterModelSettler(@SettlerA);
    UnregisterModelSettler(@SettlerA);
    SettleModel(FCurves);
    AssertEquals('', Ran);
end;

procedure TModelSettlerRegistryTest.UnregisteringOneKeepsTheOthersInTheirOrder;
begin
    RegisterModelSettler(@SettlerA);
    RegisterModelSettler(@SettlerB);
    RegisterModelSettler(@SettlerC);
    UnregisterModelSettler(@SettlerB);
    SettleModel(FCurves);
    AssertEquals('the rest, in the order they were registered', 'AC', Ran);
end;

procedure TModelSettlerRegistryTest.UnregisteringOneNeverRegisteredChangesNothing;
begin
    RegisterModelSettler(@SettlerA);
    UnregisterModelSettler(@SettlerB);
    SettleModel(FCurves);
    AssertEquals('A', Ran);
end;

initialization
    //  A UNIT test: the registry and an empty list, in memory.
    RegisterTest('unit', TModelSettlerRegistryTest);
    //  An INTEGRATION test: it runs the optimiser.
    RegisterTest('integration', TModelSettlersTest);
end.
