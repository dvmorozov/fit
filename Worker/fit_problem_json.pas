// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Wire contract for one fit exchanged between the client and the compute
server.)

A fit is sent as one JSON object (the coarse, array-in/array-out request of
decision D3) and the result comes back as one JSON object. These plain records
are deliberately engine-free (no TFitTask / LCL types) so the contract can be
tested in isolation and marshalled across the process boundary; the client maps
its TFitTask onto TFitProblem, and the server rebuilds a TFitTask from it.

This first version covers a built-in curve type (selected by GUID). User-defined
(expression) curves extend TFitProblem in a later slice.
}
unit fit_problem_json;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, Math, fpjson, jsonparser;

type
    TDoubleArray = array of double;

    { A curve parameter. On input (problem seeds) Name/Value/Vary/Shared describe
      how the backend should treat it; on output (outcome) Name/Value/Error carry
      the fitted result. }
    TFitParamData = record
        Name:  string;
        Value: double;
        Error: double;   //  standard error; < 0 when not estimated
        { Input only: is this parameter optimized (False = held at Value)? Mirrors
          the native engine's variable-parameter classification. }
        Vary:  boolean;
        { Input only: is this a Shared parameter (one value tied across all curve
          instances, as the native FCommonVariableParameters does)? }
        Shared: boolean;
        { Input only: physical bounds the backend fit must honor (the native
          parameter clamps). -Inf/+Inf = unbounded; only finite bounds are sent. }
        Min, Max: double;
    end;
    TFitParamArray = array of TFitParamData;

    { One fitted curve = its parameter values after optimization. }
    TFitCurveData = record
        Params: TFitParamArray;
    end;
    TFitCurveArray = array of TFitCurveData;

    { The whole fit problem: the profile, where curves are placed, the curve type
      and the options the engine needs. }
    TFitProblem = record
        ProfileX, ProfileY:  TDoubleArray;
        { Curve positions as (x, y). The y carries the data height at that x - the
          engine seeds each curve's amplitude from it (RecreateCurves), so a y of 0
          would start the fit from a degenerate zero-amplitude curve. }
        PositionsX:          TDoubleArray;
        PositionsY:          TDoubleArray;
        CurveTypeId:         string;
        { The curve's analytic formula in x and its parameter names (numpy/asteval
          syntax). Carrying the formula is what keeps the Python backend
          model-agnostic - it evaluates this instead of re-implementing the curve.
          CurveTypeId remains for logging; Expression is what the fit uses. }
        Expression:          string;
        { Per placed curve, its seed parameter values (Name/Value; Error unused on
          input). The backend fits from these instead of re-deriving seeds, so it
          needs no per-curve-type knowledge. }
        Curves:              TFitCurveArray;
        { Residual weighting for the Python backend: 'poisson' (1/sqrt(counts),
          statistically correct for counting data) or 'none' (unweighted, matching
          the native engine's objective). Empty = the backend default (poisson).
          The native engine ignores this. }
        Weighting:           string;
        MaxRFactor:          double;
        WaveLength:          double;
        BackgroundVariation: boolean;
        CurveScaling:        boolean;
        MinimizerKind:       longint;
        { Objective to minimise (LOSS_KIND_* in fit_loss). A formula backend must
          minimise the SAME thing the native engine would, or the two engines
          silently answer different questions. }
        LossKind:            longint;
        BegIndex, EndIndex:  longint;
    end;

    { The outcome: status, R-factor and the fitted curves. }
    TFitOutcome = record
        ErrorCode: longint;
        RFactor:   double;
        Curves:    TFitCurveArray;
    end;

function FitProblemToJson(const P: TFitProblem): string;
function FitProblemFromJson(const S: string; out P: TFitProblem): boolean;
function FitOutcomeToJson(const O: TFitOutcome): string;
function FitOutcomeFromJson(const S: string; out O: TFitOutcome): boolean;

implementation

function DoublesToJson(const A: TDoubleArray): TJSONArray;
var
    i: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(A) do
        Result.Add(A[i]);
end;

{ Reads a numeric JSON array field into a dynamic array (empty when absent). }
function JsonToDoubles(O: TJSONObject; const Name: string): TDoubleArray;
var
    D: TJSONData;
    A: TJSONArray;
    i: integer;
begin
    Result := nil;
    D := O.Find(Name);
    if not (D is TJSONArray) then
        Exit;
    A := TJSONArray(D);
    SetLength(Result, A.Count);
    for i := 0 to A.Count - 1 do
        Result[i] := A.Items[i].AsFloat;
end;

{ Serializes an array of curves as [{params:[{name,value,error}]}]. Used by the
  problem to carry per-curve seed values to the backend. }
function CurvesToJson(const Curves: TFitCurveArray): TJSONArray;
var
    CurveObj, ParamObj: TJSONObject;
    Params: TJSONArray;
    i, j: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(Curves) do
    begin
        CurveObj := TJSONObject.Create;
        Params := TJSONArray.Create;
        for j := 0 to High(Curves[i].Params) do
        begin
            ParamObj := TJSONObject.Create;
            ParamObj.Add('name', Curves[i].Params[j].Name);
            ParamObj.Add('value', Curves[i].Params[j].Value);
            ParamObj.Add('error', Curves[i].Params[j].Error);
            ParamObj.Add('vary', Curves[i].Params[j].Vary);
            ParamObj.Add('shared', Curves[i].Params[j].Shared);
            //  Only finite bounds cross the wire (JSON has no Infinity); an absent
            //  bound means unbounded on the far side.
            if not IsInfinite(Curves[i].Params[j].Min) then
                ParamObj.Add('min', Curves[i].Params[j].Min);
            if not IsInfinite(Curves[i].Params[j].Max) then
                ParamObj.Add('max', Curves[i].Params[j].Max);
            Params.Add(ParamObj);
        end;
        CurveObj.Add('params', Params);
        Result.Add(CurveObj);
    end;
end;

{ Reads the [{params:[{name,value,error}]}] array back (empty when absent). }
function JsonToCurves(O: TJSONObject; const Name: string): TFitCurveArray;
var
    D, PD: TJSONData;
    Curves, Params: TJSONArray;
    CurveObj, ParamObj: TJSONObject;
    i, j: integer;
begin
    Result := nil;
    D := O.Find(Name);
    if not (D is TJSONArray) then
        Exit;
    Curves := TJSONArray(D);
    SetLength(Result, Curves.Count);
    for i := 0 to Curves.Count - 1 do
        if Curves.Items[i] is TJSONObject then
        begin
            CurveObj := TJSONObject(Curves.Items[i]);
            PD := CurveObj.Find('params');
            if PD is TJSONArray then
            begin
                Params := TJSONArray(PD);
                SetLength(Result[i].Params, Params.Count);
                for j := 0 to Params.Count - 1 do
                    if Params.Items[j] is TJSONObject then
                    begin
                        ParamObj := TJSONObject(Params.Items[j]);
                        Result[i].Params[j].Name  := ParamObj.Get('name', '');
                        Result[i].Params[j].Value := ParamObj.Get('value', 0.0);
                        Result[i].Params[j].Error := ParamObj.Get('error', -1.0);
                        //  Absent flags default to "varied, not shared" so an
                        //  older payload still fits.
                        Result[i].Params[j].Vary   := ParamObj.Get('vary', True);
                        Result[i].Params[j].Shared := ParamObj.Get('shared', False);
                        Result[i].Params[j].Min := ParamObj.Get('min', NegInfinity);
                        Result[i].Params[j].Max := ParamObj.Get('max', Infinity);
                    end;
            end;
        end;
end;

function FitProblemToJson(const P: TFitProblem): string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add('op', 'fit');
        O.Add('profileX', DoublesToJson(P.ProfileX));
        O.Add('profileY', DoublesToJson(P.ProfileY));
        O.Add('positionsX', DoublesToJson(P.PositionsX));
        O.Add('positionsY', DoublesToJson(P.PositionsY));
        O.Add('curveTypeId', P.CurveTypeId);
        O.Add('expression', P.Expression);
        O.Add('curves', CurvesToJson(P.Curves));
        O.Add('weighting', P.Weighting);
        O.Add('maxRFactor', P.MaxRFactor);
        O.Add('waveLength', P.WaveLength);
        O.Add('backgroundVariation', P.BackgroundVariation);
        O.Add('curveScaling', P.CurveScaling);
        O.Add('minimizerKind', P.MinimizerKind);
        O.Add('lossKind', P.LossKind);
        O.Add('begIndex', P.BegIndex);
        O.Add('endIndex', P.EndIndex);
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

function FitProblemFromJson(const S: string; out P: TFitProblem): boolean;
var
    D: TJSONData;
    O: TJSONObject;
begin
    Result := False;
    P := Default(TFitProblem);
    D := nil;
    try
        try
            D := GetJSON(S);
        except
            D := nil;
        end;
        if not (D is TJSONObject) then
            Exit;
        O := TJSONObject(D);
        P.ProfileX            := JsonToDoubles(O, 'profileX');
        P.ProfileY            := JsonToDoubles(O, 'profileY');
        P.PositionsX          := JsonToDoubles(O, 'positionsX');
        P.PositionsY          := JsonToDoubles(O, 'positionsY');
        P.CurveTypeId         := O.Get('curveTypeId', '');
        P.Expression          := O.Get('expression', '');
        P.Curves              := JsonToCurves(O, 'curves');
        P.Weighting           := O.Get('weighting', '');
        P.MaxRFactor          := O.Get('maxRFactor', 0.01);
        P.WaveLength          := O.Get('waveLength', 0.0);
        P.BackgroundVariation := O.Get('backgroundVariation', False);
        P.CurveScaling        := O.Get('curveScaling', False);
        P.MinimizerKind       := O.Get('minimizerKind', 0);
        //  0 is the corrected R-factor, so an older peer that omits the field
        //  lands on the right objective rather than on a historical one.
        P.LossKind            := O.Get('lossKind', 0);
        P.BegIndex            := O.Get('begIndex', 0);
        P.EndIndex            := O.Get('endIndex', 0);
        Result := True;
    finally
        D.Free;
    end;
end;

function FitOutcomeToJson(const O: TFitOutcome): string;
var
    Root, CurveObj, ParamObj: TJSONObject;
    Curves, Params: TJSONArray;
    i, j: integer;
begin
    Root := TJSONObject.Create;
    try
        Root.Add('ok', O.ErrorCode = 0);
        Root.Add('errorCode', O.ErrorCode);
        Root.Add('rFactor', O.RFactor);
        Curves := TJSONArray.Create;
        for i := 0 to High(O.Curves) do
        begin
            CurveObj := TJSONObject.Create;
            Params := TJSONArray.Create;
            for j := 0 to High(O.Curves[i].Params) do
            begin
                ParamObj := TJSONObject.Create;
                ParamObj.Add('name', O.Curves[i].Params[j].Name);
                ParamObj.Add('value', O.Curves[i].Params[j].Value);
                ParamObj.Add('error', O.Curves[i].Params[j].Error);
                Params.Add(ParamObj);
            end;
            CurveObj.Add('params', Params);
            Curves.Add(CurveObj);
        end;
        Root.Add('curves', Curves);
        Result := Root.AsJSON;
    finally
        Root.Free;
    end;
end;

function FitOutcomeFromJson(const S: string; out O: TFitOutcome): boolean;
var
    D, CD, PD: TJSONData;
    Root, CurveObj, ParamObj: TJSONObject;
    Curves, Params: TJSONArray;
    i, j: integer;
begin
    Result := False;
    O := Default(TFitOutcome);
    D := nil;
    try
        try
            D := GetJSON(S);
        except
            D := nil;
        end;
        if not (D is TJSONObject) then
            Exit;
        Root := TJSONObject(D);
        O.ErrorCode := Root.Get('errorCode', 0);
        O.RFactor   := Root.Get('rFactor', 0.0);
        CD := Root.Find('curves');
        if CD is TJSONArray then
        begin
            Curves := TJSONArray(CD);
            SetLength(O.Curves, Curves.Count);
            for i := 0 to Curves.Count - 1 do
                if Curves.Items[i] is TJSONObject then
                begin
                    CurveObj := TJSONObject(Curves.Items[i]);
                    PD := CurveObj.Find('params');
                    if PD is TJSONArray then
                    begin
                        Params := TJSONArray(PD);
                        SetLength(O.Curves[i].Params, Params.Count);
                        for j := 0 to Params.Count - 1 do
                            if Params.Items[j] is TJSONObject then
                            begin
                                ParamObj := TJSONObject(Params.Items[j]);
                                O.Curves[i].Params[j].Name  := ParamObj.Get('name', '');
                                O.Curves[i].Params[j].Value := ParamObj.Get('value', 0.0);
                                O.Curves[i].Params[j].Error := ParamObj.Get('error', -1.0);
                            end;
                    end;
                end;
        end;
        Result := True;
    finally
        D.Free;
    end;
end;

end.
