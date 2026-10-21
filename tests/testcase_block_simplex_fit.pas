// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The block-wise Downhill Simplex as the user selects it: a minimizer
kind, through REST.)

Fit > Minimizer offers it beside the Downhill Simplex (fit-performance.md, stage
9a); choosing it sends `minimizerKind`, and the next fit runs it. Over curves
that shape everything - the Gaussians here, whose support is unbounded - it is
the simplex over everything, value for value: such curves are one block
(TFitTask.ParamBlock). Where blocks differ - curves of bounded support - it is
tested in the module that has them.
}
unit testcase_block_simplex_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    fit_rest_api, int_fit_service, minimizer_registry, minimizer_registration,
    gauss_points_set;

type
    TBlockSimplexFitTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        function Num(const AValue: double): string;
        { Three peaks, a pick on each and one interval over all of them, fitted
          by AKind. Answers the problem. }
        function FitThreePeaks(AKind: longint;
            const AAction: string = 'minimize-difference'): longint;
        function CurveCount(AId: longint): longint;
        function Reached(AId: longint): double;
        function FittedValues(AId: longint): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheBlockWiseKindIsOfferedBesideTheSimplex;
        procedure OverCurvesThatShapeEverythingItIsTheSimplex;
        procedure RestartedItFitsAtLeastAsWellAsTheSimplex;
        procedure EveryKindRunsTheAutomaticDecomposition;
    end;

implementation

procedure TBlockSimplexFitTest.SetUp;
begin
    RegisterAllMinimizers;
    FApi := TFitRestApi.Create;
end;

procedure TBlockSimplexFitTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TBlockSimplexFitTest.Call(const M, P, B: string;
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

function TBlockSimplexFitTest.Num(const AValue: double): string;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStrF(AValue, ffGeneral, 17, 0, FS);
end;

function TBlockSimplexFitTest.FitThreePeaks(AKind: longint;
    const AAction: string): longint;
const
    CENTRES: array[0..2] of double = (15, 40, 65);
var
    Code, i, k: longint;
    R: TJSONObject;
    XS, YS: string;
    Y: double;
begin
    R := Call('POST', '/problems', '', Code);
    try
        Result := R.Get('id', 0);
    finally
        R.Free;
    end;
    XS := '';
    YS := '';
    for i := 0 to 80 do
    begin
        Y := 0;
        for k := 0 to 2 do
            Y := Y + (100 + 3 * Sin(i * 0.7)) * Exp(-Sqr((i - CENTRES[k]) / 3));
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + Num(Y);
    end;
    R := Call('PUT', Format('/problems/%d/profile', [Result]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code);
    R.Free;
    R := Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"minimizerKind":%d,"curveType":"%s"}',
        [AKind, GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code);
    try
        AssertEquals('the kind is accepted (' + R.Get('error', '') + ')', 200,
            Code);
    finally
        R.Free;
    end;
    //  Picked off centre, so every curve has somewhere to go - and at the
    //  profile's height there, which is what a pick seeds the amplitude from:
    //  picked at zero, every curve starts flat and no engine moves it.
    for k := 0 to 2 do
    begin
        R := Call('POST', Format('/problems/%d/points/positions', [Result]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 2),
            Num(100 * Exp(-Sqr(2 / 3)))]), Code);
        R.Free;
    end;
    R := Call('POST', Format('/problems/%d/actions/%s',
        [Result, AAction]), '', Code);
    try
        AssertEquals('fitted (' + R.Get('error', '') + ')', 200, Code);
    finally
        R.Free;
    end;
end;

function TBlockSimplexFitTest.Reached(AId: longint): double;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/rfactor', [AId]), '', Code);
    try
        Result := R.Get('curMin', NaN);
    finally
        R.Free;
    end;
end;

function TBlockSimplexFitTest.FittedValues(AId: longint): string;
var
    Code, i, j: longint;
    R: TJSONObject;
    Curves, Params: TJSONArray;
    P: TJSONObject;
begin
    //  The values alone: each problem issues its curves their own handles.
    Result := '';
    R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    try
        Curves := TJSONArray(R.Find('curves'));
        for i := 0 to Curves.Count - 1 do
        begin
            Params := TJSONArray(TJSONObject(Curves.Items[i]).Find('params'));
            for j := 0 to Params.Count - 1 do
            begin
                P := TJSONObject(Params.Items[j]);
                if P.Get('kind', '') <> 'text' then
                    Result := Result + Num(P.Get('value', NaN)) + ' ';
            end;
        end;
    finally
        R.Free;
    end;
end;

procedure TBlockSimplexFitTest.TheBlockWiseKindIsOfferedBesideTheSimplex;
begin
    AssertTrue('the block-wise kind is declared',
        IsKnownMinimizer(MIN_KIND_DHS_BLOCKS));
    AssertTrue('and the simplex still is', IsKnownMinimizer(MIN_KIND_DHS));
    AssertEquals('the simplex is still the default', MIN_KIND_DHS,
        DefaultMinimizerKind);
end;

{ ONE BLOCK IS THE SIMPLEX: a Gaussian is never zero, so three of them are
  fitted together, and the run is the simplex over everything through a view
  of the same parameters in the same order. }
procedure TBlockSimplexFitTest.OverCurvesThatShapeEverythingItIsTheSimplex;
var
    Simplex, Blocks: string;
begin
    Simplex := FittedValues(FitThreePeaks(MIN_KIND_DHS));
    Blocks := FittedValues(FitThreePeaks(MIN_KIND_DHS_BLOCKS));
    AssertTrue('a model was fitted', Simplex <> '');
    AssertEquals('value for value', Simplex, Blocks);
end;

{ THE RESTARTED SIMPLEX (stage 9c): the simplex over everything, started
  again from its own best while that pays. It begins with exactly the
  simplex's run, so it can only end at or below it - and on these peaks it
  ends below: the simplex left the third far too wide. }
procedure TBlockSimplexFitTest.RestartedItFitsAtLeastAsWellAsTheSimplex;
var
    Simplex, Restarted: double;
begin
    Simplex := Reached(FitThreePeaks(MIN_KIND_DHS));
    Restarted := Reached(FitThreePeaks(MIN_KIND_DHS_RESTARTED));
    WriteLn(StdErr, Format('[restarted] simplex %g, restarted %g',
        [Simplex, Restarted]));
    AssertTrue(Format('restarted %g against the simplex''s %g',
        [Restarted, Simplex]), Restarted < Simplex);
end;

function TBlockSimplexFitTest.CurveCount(AId: longint): longint;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    try
        Result := TJSONArray(R.Find('curves')).Count;
    finally
        R.Free;
    end;
end;

{ THE AUTOMATIC RUN UNDER EVERY KIND: its reduction stage runs the minimizer
  with the decomposition's loose convergence and a stop below the accepted
  R-factor (UseDecompositionConvergence), a path a plain fit never takes. Each
  kind must take the run through both stages to a fitted model. }
procedure TBlockSimplexFitTest.EveryKindRunsTheAutomaticDecomposition;
const
    KINDS: array[0..2] of longint = (MIN_KIND_DHS, MIN_KIND_DHS_BLOCKS,
        MIN_KIND_DHS_RESTARTED);
var
    k, Id: longint;
    Got: double;
begin
    for k := 0 to High(KINDS) do
    begin
        Id := FitThreePeaks(KINDS[k], 'do-all-automatically');
        Got := Reached(Id);
        WriteLn(StdErr, Format('[auto] kind %d: %d curves, R %g',
            [KINDS[k], CurveCount(Id), Got]));
        AssertTrue(Format('kind %d left curves', [KINDS[k]]),
            CurveCount(Id) > 0);
        AssertTrue(Format('kind %d reached a fit, R %g', [KINDS[k], Got]),
            (not IsNan(Got)) and (Got < 0.1));
    end;
end;

initialization
    RegisterTest('integration', TBlockSimplexFitTest);
end.
