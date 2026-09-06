// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The REST surface of the compute server.)

This is the replacement for the retired XML-RPC/WST transport: the same
IFitService verbs, carried over HTTP+JSON. A problem is a resource
(/problems/<id>) - the stateful ProblemID model the original API used - so
several documents/clients can be served at once.

The whole API is a pure function of (method, path, body) -> (status, body), so
it can be unit-tested without opening a socket; fit_server.lpr only adapts an
HTTP server onto it.

Routes implemented so far:
  GET    /health
  POST   /problems                                   -> ok,id
  DELETE /problems/<id>
  GET    /problems/<id>/state                        -> ok,state
  PUT    /problems/<id>/profile                      body: points
  GET    /problems/<id>/profile                      -> points
  GET    /problems/<id>/calc-profile                 -> points
  GET    /problems/<id>/positions                    -> points  (the picks)
  GET    /problems/<id>/calc-positions               -> points  (what was built)
  GET    /problems/<id>/delta-profile                -> points
  POST   /problems/<id>/actions/minimize-difference  -> ok,message
  GET    /problems/<id>/rfactor                      -> ok,rFactor
}
unit fit_rest_api;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, DateUtils, fpjson, jsonparser,
    //  The single source of which routes are heartbeats; the client uses the
    //  same unit so the two sides cannot drift. See Common/rest_polling.pas.
    rest_polling,
    fit_worker_protocol, fit_points_json, fit_problem_json, Variants,
    fit_server_session,
    int_fit_service, minimizer_registry, minimizer_registration, action_registry,
    int_app_module, module_registry
    //  Which route a request names - the table, as a table.
    , rest_routes,
    points_set, title_points_set, named_points_set,
    self_copied_component, persistent_curve_parameters,
    persistent_curve_parameter_container, special_curve_parameter, log,
    fit_statistics, fit_service_statistics,
    //  EUserException: the engine's way of saying the REQUEST was inadmissible,
    //  as opposed to the engine being broken. The two get different status codes
    //  and different log tiers - see the handler at the bottom of Handle.
    MyExceptions;

type
    { Ensures the Python sidecar fit_server owns is running and returns its base
      URL (empty when unavailable). Wired by fit_server; nil when there is no
      sidecar. Keeps the REST layer testable without spawning Python. }
    TEnsurePythonSidecar = function(out AUrl: string): boolean of object;

    { Routes one request. Owns the problem registry. }
    TFitRestApi = class(TObject)
    private
        FSessions: TSessionRegistry;
        FEnsurePythonSidecar: TEnsurePythonSidecar;
        function ProblemOf(const AId: string; out ACode: longint;
            out AError: string): TFitSession;
        function SessionOfPath(const APath: string): TFitSession;
        procedure HandleRoute(const AMethod, APath, ABody: string;
            out ACode: longint; out AResponse: string);
    public
        constructor Create;
        destructor Destroy; override;
        { Set by fit_server so the API can start its Python sidecar on demand and
          tell the engine where to reach it (the single integration point). }
        property EnsurePythonSidecar: TEnsurePythonSidecar
            read FEnsurePythonSidecar write FEnsurePythonSidecar;
        { Handles one call. AResponse is always a JSON document. Logs the request,
          the outcome, and any engine exception (which becomes a 500 rather than a
          dead connection). }
        procedure Handle(const AMethod, APath, ABody: string;
            out ACode: longint; out AResponse: string);
        property Sessions: TSessionRegistry read FSessions;
    end;

{ True for a route that must NOT take the problem's lock.

  Exported for the same reason RunAction is: it is a rule rather than an
  internal branch, and getting it wrong is invisible from outside. Too narrow
  and a progress read waits behind the operation it is reporting on, so the
  client's poll freezes for the length of every fit; too wide and something
  that touches the engine runs unlocked in a threaded server. Neither shows up
  as a failed request. }
function IsUnlockedRoute(const AMethod, APath: string): boolean;

{ Registers the verbs this build offers, and runs one.

  Exported because the verb SET is now part of the engine's public surface, not
  an internal branch: a batch layer enumerates it, an assistant is given it to
  choose from, and a test can drive one verb without standing up a server. Both
  are idempotent and safe to call in any order. }
procedure RegisterBuiltInActions;
procedure RunAction(ASession: TFitSession; const AName, ABody: string;
    out ACode: longint; out AResult, AError: string);

implementation

{ Splits '/problems/12/profile' into ['problems','12','profile']. }
function SplitPath(const APath: string): TStringArray;
var
    Parts: TStringList;
    i: integer;
    S: string;
begin
    Parts := TStringList.Create;
    try
        Parts.Delimiter := '/';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := APath;
        SetLength(Result, 0);
        for i := 0 to Parts.Count - 1 do
        begin
            S := Trim(Parts[i]);
            if S <> '' then
            begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := S;
            end;
        end;
    finally
        Parts.Free;
    end;
end;

{ Copies a decoded point set into a fresh TTitlePointsSet (caller owns it). }
function ToTitlePointsSet(const P: TPointsData): TTitlePointsSet;
var
    i: integer;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.FTitle := P.Title;
    for i := 0 to High(P.X) do
        Result.AddNewPoint(P.X[i], P.Y[i]);
end;

{ Why a read failed: the status code it deserves and what to tell the caller.
  Carried as a record rather than a bare boolean because the two failures this
  reader can have want OPPOSITE responses - a body it cannot parse will fail
  again identically (400), a handle the model no longer holds will not (404). }
type
    TRouteFault = record
        Code: longint;
        Message: string;
    end;

{ Reads the whole model's restored values out of a PUT /curves body, resolving
  each handle to the index the ordinal service members take.

  RESOLUTION HAPPENS HERE, at the wire's boundary, exactly as it does for the
  two curve routes that address by handle in their path. IFitService keeps its
  ordinal members, and an index never outlives the request that made it.

  An unknown handle fails the WHOLE request rather than being skipped: a restore
  that silently dropped one curve would put a model on screen that is missing a
  peak, with nothing anywhere saying so. }
function ReadCurveValues(ASvc: IFitService; const ABody: string;
    out AEntries: TCurveValuesList; out AFault: TRouteFault): boolean;
var
    D: TJSONData;
    Root, CurveObj, ParamObj: TJSONObject;
    Curves, Params: TJSONArray;
    i, j, Index_: longint;
    Handle: string;
begin
    Result := False;
    AEntries := nil;
    AFault.Code := 400;
    AFault.Message := 'malformed curve values';

    D := nil;
    try
        try
            D := GetJSON(ABody);
        except
            D := nil;
        end;
        if not (D is TJSONObject) then
            Exit;
        Root := TJSONObject(D);
        if not (Root.Find('curves') is TJSONArray) then
            Exit;
        Curves := TJSONArray(Root.Find('curves'));

        SetLength(AEntries, Curves.Count);
        for i := 0 to Curves.Count - 1 do
        begin
            if not (Curves.Items[i] is TJSONObject) then
                Exit;
            CurveObj := TJSONObject(Curves.Items[i]);
            Handle := CurveObj.Get('id', '');
            Index_ := ASvc.IndexOfCurveInstance(Handle);
            if Index_ < 0 then
            begin
                AFault.Code := 404;
                AFault.Message := 'the model no longer holds a curve ' +
                    'identified by "' + Handle + '", so nothing was restored.';
                Exit;
            end;
            AEntries[i].CurveIndex := Index_;
            AEntries[i].Fitted := CurveObj.Get('fitted', False);

            if not (CurveObj.Find('params') is TJSONArray) then
                Exit;
            Params := TJSONArray(CurveObj.Find('params'));
            SetLength(AEntries[i].Params, Params.Count);
            for j := 0 to Params.Count - 1 do
            begin
                if not (Params.Items[j] is TJSONObject) then
                    Exit;
                ParamObj := TJSONObject(Params.Items[j]);
                AEntries[i].Params[j].Name := ParamObj.Get('name', '');
                AEntries[i].Params[j].Value := ParamObj.Get('value', 0.0);
                //  -1 is "the optimiser estimated none", which is what every
                //  parameter carries until one does.
                AEntries[i].Params[j].Error := ParamObj.Get('error', -1.0);
            end;
        end;
        Result := True;
    finally
        D.Free;
    end;
end;

{ Describes a point set as the wire record. }
function FromPointsSet(APoints: TPointsSet; const ATitle: string): TPointsData;
var
    i: integer;
begin
    Result := Default(TPointsData);
    Result.Title := ATitle;
    if not Assigned(APoints) then
        Exit;
    SetLength(Result.X, APoints.PointsCount);
    SetLength(Result.Y, APoints.PointsCount);
    for i := 0 to APoints.PointsCount - 1 do
    begin
        Result.X[i] := APoints.PointXCoord[i];
        Result.Y[i] := APoints.PointYCoord[i];
    end;
end;

{ A points response, taking ownership of the set the service handed back.

  AIds rides along for the picks and is empty everywhere else, exactly as it is
  on the write side: a curve's identity is issued to the pick it is seeded from,
  so only the picks have any. }
function PointsResponse(APoints: TTitlePointsSet;
    const AIds: TCurveInstanceIdList = nil): string;
var
    D: TPointsData;
    i: longint;
begin
    try
        if Assigned(APoints) then
            D := FromPointsSet(APoints, APoints.FTitle)
        else
            D := Default(TPointsData);
        //  Only when they line up. A mismatch here would be this server's own
        //  fault rather than the request's, and emitting a ragged list would
        //  make the reader refuse a reply it had no way to fix.
        if Length(AIds) = Length(D.X) then
        begin
            SetLength(D.Ids, Length(AIds));
            for i := 0 to High(AIds) do
                D.Ids[i] := AIds[i];
        end;
        Result := PointsToJsonString(D);
    finally
        APoints.Free;
    end;
end;

{ The statistics as a JSON object (always present; valid flags real numbers). }
function StatisticsJson(const S: TFitStatistics): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('valid', S.Valid);
    Result.Add('dataPoints', S.DataPoints);
    Result.Add('params', S.Params);
    Result.Add('degreesOfFreedom', S.DegreesOfFreedom);
    Result.Add('chiSquare', S.ChiSquare);
    Result.Add('reducedChiSquare', S.ReducedChiSquare);
    Result.Add('rSquared', S.RSquared);
    Result.Add('aic', S.AIC);
    Result.Add('bic', S.BIC);
end;

{ The problem's scalar settings as one resource. }
function SettingsOf(ASvc: IFitService): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('maxRFactor', ASvc.GetMaxRFactor);
    Result.Add('backFactor', ASvc.GetBackFactor);
    Result.Add('curveThresh', ASvc.GetCurveThresh);
    Result.Add('waveLength', ASvc.GetWaveLength);
    Result.Add('backgroundVariation', ASvc.GetBackgroundVariationEnabled);
    Result.Add('curveScaling', ASvc.GetCurveScalingEnabled);
    Result.Add('minimizerKind', ASvc.GetMinimizerKind);
    Result.Add('lossKind', ASvc.GetLossKind);
    Result.Add('weighting', ASvc.GetWeighting);
    Result.Add('curveType', GUIDToString(ASvc.GetCurveType));
end;

{ Applies whichever settings the body carries; absent fields are left alone. }
procedure ApplySettings(ASvc: IFitService; O: TJSONObject);
var
    S: string;
begin
    if O.Find('maxRFactor') <> nil then
        ASvc.SetMaxRFactor(O.Get('maxRFactor', ASvc.GetMaxRFactor));
    if O.Find('backFactor') <> nil then
        ASvc.SetBackFactor(O.Get('backFactor', ASvc.GetBackFactor));
    if O.Find('curveThresh') <> nil then
        ASvc.SetCurveThresh(O.Get('curveThresh', ASvc.GetCurveThresh));
    if O.Find('waveLength') <> nil then
        ASvc.SetWaveLength(O.Get('waveLength', ASvc.GetWaveLength));
    if O.Find('backgroundVariation') <> nil then
        ASvc.SetBackgroundVariationEnabled(
            O.Get('backgroundVariation', ASvc.GetBackgroundVariationEnabled));
    if O.Find('curveScaling') <> nil then
        ASvc.SetCurveScalingEnabled(
            O.Get('curveScaling', ASvc.GetCurveScalingEnabled));
    if O.Find('minimizerKind') <> nil then
        ASvc.SetMinimizerKind(O.Get('minimizerKind', ASvc.GetMinimizerKind));
    if O.Find('lossKind') <> nil then
        ASvc.SetLossKind(O.Get('lossKind', ASvc.GetLossKind));
    if O.Find('weighting') <> nil then
        ASvc.SetWeighting(O.Get('weighting', ASvc.GetWeighting));
    if O.Find('curveType') <> nil then
    begin
        S := O.Get('curveType', '');
        if S <> '' then
            ASvc.SetCurveType(StringToGUID(S));
    end;
end;

{ Every curve with its parameters (name, value, type). }
function CurvesOf(ASvc: IFitService): TJSONObject;
var
    Curves, Params: TJSONArray;
    CurveObj, ParamObj: TJSONObject;
    i, j, PCount, T: longint;
    Nm: string;
    Val: Variant;
    V: double;
begin
    Result := TJSONObject.Create;
    Curves := TJSONArray.Create;
    for i := 0 to ASvc.GetCurveCount - 1 do
    begin
        CurveObj := TJSONObject.Create;
        //  WHICH CURVE THIS IS, as its own field and not as a parameter. A
        //  parameter is a quantity of the model; this is a handle to the
        //  object. It is also what the other two curve routes address by, so it
        //  has to be readable here before either can be called.
        CurveObj.Add('id', ASvc.GetCurveInstanceId(i));
        //  Whether an optimiser produced these values. Emitted beside the
        //  handle rather than as a parameter, for the same reason the handle is:
        //  a parameter is a quantity of the model, and this is a fact about the
        //  instance. It is also the field the write side reads back.
        CurveObj.Add('fitted', ASvc.IsCurveFitted(i));
        Params := TJSONArray.Create;
        PCount := ASvc.GetCurveParameterCount(i);
        for j := 0 to PCount - 1 do
        begin
            ASvc.GetCurveParameter(i, j, Nm, V, T);
            ParamObj := TJSONObject.Create;
            ParamObj.Add('name', Nm);
            ParamObj.Add('value', V);
            ParamObj.Add('type', T);
            ParamObj.Add('error', ASvc.GetCurveParameterError(i, j));
            //  A non-numeric value replaces `value` with its own JSON type and
            //  says so in `kind`. JSON is self-describing, so nothing needs a
            //  second field: `value` simply IS a string when the parameter holds
            //  one. `kind` is emitted only then, so an all-numeric model
            //  serialises exactly as before (D1/D2).
            Val := ASvc.GetCurveParameterValue(i, j);
            if not VarIsNumeric(Val) then
            begin
                ParamObj.Delete(ParamObj.IndexOfName('value'));
                ParamObj.Add('value', VarToStr(Val));
                ParamObj.Add('kind', 'text');
            end;
            Params.Add(ParamObj);
        end;
        CurveObj.Add('params', Params);
        Curves.Add(CurveObj);
    end;
    Result.Add('curves', Curves);
end;

{ The user-defined curve's expression and its parameters. }
function SpecialParamsOf(ASvc: IFitService): TJSONObject;
var
    CP: Curve_parameters;
    Arr: TJSONArray;
    O: TJSONObject;
    i: longint;
begin
    Result := TJSONObject.Create;
    Arr := TJSONArray.Create;
    CP := ASvc.GetSpecialCurveParameters;
    try
        if Assigned(CP) then
            for i := 0 to CP.Count - 1 do
            begin
                O := TJSONObject.Create;
                O.Add('name', CP[i].Name);
                O.Add('value', CP[i].Value);
                O.Add('type', longint(CP[i].Type_));
                Arr.Add(O);
            end;
    finally
        CP.Free;
    end;
    Result.Add('params', Arr);
end;

{ Sets the user-curve expression (and, when given, its parameter values). }
procedure ApplySpecialParams(ASvc: IFitService; O: TJSONObject);
var
    Arr: TJSONArray;
    D: TJSONData;
    CP: Curve_parameters;
    P: TJSONObject;
    Container: TPersistentCurveParameterContainer;
    i: integer;
begin
    CP := nil;
    D := O.Find('params');
    if D is TJSONArray then
    begin
        Arr := TJSONArray(D);
        CP := Curve_parameters.Create(nil);
        //  Curve_parameters starts with one placeholder parameter.
        CP.Params.Clear;
        for i := 0 to Arr.Count - 1 do
            if Arr.Items[i] is TJSONObject then
            begin
                P := TJSONObject(Arr.Items[i]);
                Container := TPersistentCurveParameterContainer(CP.Params.Add);
                Container.Parameter.Name := P.Get('name', '');
                Container.Parameter.Value := P.Get('value', 0.0);
                Container.Parameter.Type_ :=
                    TParameterType(P.Get('type', 0));
            end;
    end;
    //  Nil means "initialize from the expression" - the service's own contract.
    ASvc.SetSpecialCurveParameters(O.Get('expression', ''), CP);
end;

{ One curve's points, so a thin client can plot it. Returns a fresh set the
  caller owns (nil when the index is out of range). }
function CurvePointsOf(ASvc: IFitService; AIndex: longint): TTitlePointsSet;
var
    Curves: TSelfCopiedCompList;
    Curve:  TNamedPointsSet;
    i: longint;
begin
    Result := nil;
    Curves := ASvc.GetCurves;
    try
        if (AIndex < 0) or (AIndex >= Curves.Count) then
            Exit;
        Curve := TNamedPointsSet(Curves.Items[AIndex]);
        Result := TTitlePointsSet.Create(nil);
        Result.FTitle := Curve.GetCurveTypeName;
        for i := 0 to Curve.PointsCount - 1 do
            Result.AddNewPoint(Curve.PointXCoord[i], Curve.PointYCoord[i]);
    finally
        Curves.Free;
    end;
end;

{ Appends a point to one of the named point sets. }
procedure AddPoint(ASvc: IFitService; const ASet: string; O: TJSONObject;
    out ACode: longint; out AError: string);
var
    X, Y: double;
begin
    ACode := 200;
    AError := '';
    X := O.Get('x', 0.0);
    Y := O.Get('y', 0.0);
    if ASet = 'profile' then
        ASvc.AddPointToProfile(X, Y)
    else if ASet = 'background' then
        ASvc.AddPointToBackground(X, Y)
    else if ASet = 'positions' then
        ASvc.AddPointToCurvePositions(X, Y)
    else if ASet = 'rfactor-bounds' then
        ASvc.AddPointToRFactorBounds(X, Y)
    else
        //  Anything else is a module's own set. The service refuses by name if
        //  no module claims it, so an unknown set is reported once, in the
        //  module's terms, rather than twice in different words.
        ASvc.AddPointToSet(ASet, X, Y);
end;

{ Moves an existing point in one of the named point sets. }
procedure ReplacePoint(ASvc: IFitService; const ASet: string; O: TJSONObject;
    out ACode: longint; out AError: string);
var
    PX, PY, X, Y: double;
begin
    ACode := 200;
    AError := '';
    PX := O.Get('prevX', 0.0);
    PY := O.Get('prevY', 0.0);
    X  := O.Get('x', 0.0);
    Y  := O.Get('y', 0.0);
    if ASet = 'profile' then
        ASvc.ReplacePointInProfile(PX, PY, X, Y)
    else if ASet = 'background' then
        ASvc.ReplacePointInBackground(PX, PY, X, Y)
    else if ASet = 'positions' then
        ASvc.ReplacePointInCurvePositions(PX, PY, X, Y)
    else if ASet = 'rfactor-bounds' then
        ASvc.ReplacePointInRFactorBounds(PX, PY, X, Y)
    else
        ASvc.ReplacePointInSet(ASet, PX, PY, X, Y);
end;

{ ---------------------------------------------------------------------------
  The built-in actions.

  One handler each, registered below, replacing a fourteen-branch if-chain. The
  bodies are unchanged: what changes is that the SET of verbs is now data, which
  is what a batch layer, an assistant driving the app, or a module adding a verb
  all need (see action_registry).
  --------------------------------------------------------------------------- }

{ The shape almost every action has: call the service, return what it says. }
procedure ActMinimizeDifference(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.MinimizeDifference;
end;

procedure ActMinimizeDifferenceAgain(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.MinimizeDifferenceAgain;
end;

procedure ActMinimizeNumberOfCurves(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.MinimizeNumberOfCurves;
end;

procedure ActDoAllAutomatically(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.DoAllAutomatically;
end;

procedure ActSmoothProfile(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.SmoothProfile;
end;

procedure ActComputeCurveBounds(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.ComputeCurveBounds;
end;

procedure ActComputeBackgroundPoints(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.ComputeBackgroundPoints;
end;

procedure ActComputeCurvePositions(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.ComputeCurvePositions;
end;

procedure ActSelectAllPointsAsCurvePositions(ASession: TFitSession;
    const ABody: string; out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.SelectAllPointsAsCurvePositions;
end;

procedure ActSelectEntireProfile(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.SelectEntireProfile;
end;

procedure ActCreateCurveList(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := ''; AResult := '';
    ASession.Service.CreateCurveList;
end;

procedure ActStop(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := ''; AResult := '';
    ASession.Service.StopAsyncOper;
end;

procedure ActSubtractBackground(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
var
    O: TJSONObject;
begin
    ACode := 200; AError := ''; AResult := '';
    O := ParseMessage(ABody);
    try
        ASession.Service.SubtractBackground((O <> nil) and O.Get('auto', False));
    finally
        O.Free;
    end;
end;

procedure ActSelectProfileInterval(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
var
    O: TJSONObject;
begin
    ACode := 200; AError := ''; AResult := '';
    O := ParseMessage(ABody);
    if O = nil then
    begin
        ACode := 400;
        AError := 'select-profile-interval needs start and stop';
        Exit;
    end;
    try
        AResult := ASession.Service.SelectProfileInterval(
            O.Get('start', 0), O.Get('stop', 0));
    finally
        O.Free;
    end;
end;

procedure Add(const AName, ADescription: string; AHandler: TActionHandler;
    AAsync: boolean = False);
var
    Info: TActionInfo;
begin
    Info := Default(TActionInfo);
    Info.Name := AName;
    Info.Description := ADescription;
    Info.IsAsynchronous := AAsync;
    Info.Handler := AHandler;
    RegisterAction(Info);
end;

var
    BuiltInActionsRegistered: boolean = False;

{ The verbs this build offers. Idempotent, and called from RunAction rather than
  from a start-up hook, so no host has to remember it and a test driving the
  router directly gets the same set. }
procedure RegisterBuiltInActions;
begin
    if BuiltInActionsRegistered then
        Exit;
    BuiltInActionsRegistered := True;

    Add('minimize-difference',
        'Fit the current model to the profile.',
        @ActMinimizeDifference, True);
    Add('minimize-difference-again',
        'Continue fitting from where the last fit stopped.',
        @ActMinimizeDifferenceAgain, True);
    Add('minimize-number-of-curves',
        'Fit, then drop curves that do not earn their place.',
        @ActMinimizeNumberOfCurves, True);
    Add('do-all-automatically',
        'Background, positions and fit, in one pass.',
        @ActDoAllAutomatically, True);
    Add('smooth-profile',
        'Smooth the experimental profile.',
        @ActSmoothProfile, True);
    Add('compute-curve-bounds',
        'Work out where each curve begins and ends.',
        @ActComputeCurveBounds, True);
    Add('compute-background-points',
        'Propose background points from the profile.',
        @ActComputeBackgroundPoints, True);
    Add('compute-curve-positions',
        'Propose a curve position for each peak found.',
        @ActComputeCurvePositions, True);
    Add('select-all-points-as-curve-positions',
        'Use every profile point as a curve position.',
        @ActSelectAllPointsAsCurvePositions, True);
    Add('select-entire-profile',
        'Fit over the whole profile rather than a marked interval.',
        @ActSelectEntireProfile, True);
    Add('create-curve-list',
        'Rebuild the curve list from the current model.',
        @ActCreateCurveList);
    Add('stop',
        'Stop the operation now, keeping what it has reached.',
        @ActStop);
    Add('subtract-background',
        'Subtract the background from the profile.',
        @ActSubtractBackground);
    Add('select-profile-interval',
        'Restrict the fit to the interval between start and stop.',
        @ActSelectProfileInterval);
end;

{ Runs one named action on the problem. }
procedure RunAction(ASession: TFitSession; const AName, ABody: string;
    out ACode: longint; out AResult, AError: string);
var
    Info: TActionInfo;
begin
    ACode := 200;
    AResult := '';
    AError := '';

    //  Looked up BEFORE the session is touched: a verb that does not exist
    //  should not reset the progress of an operation that does.
    RegisterBuiltInActions;
    if not FindAction(AName, Info) then
    begin
        ACode := 404;
        //  Names what could have been asked instead. The old message said only
        //  what was wrong, which for a typo in a script is half the answer.
        AError := 'unknown action: ' + AName + '. This build offers: ' +
            KnownActionNames;
        Exit;
    end;

    ASession.ResetProgress;
    Info.Handler(ASession, ABody, ACode, AResult, AError);
end;

{ TFitRestApi }

constructor TFitRestApi.Create;
begin
    inherited Create;
    FSessions := TSessionRegistry.Create;
end;

destructor TFitRestApi.Destroy;
begin
    FSessions.Free;
    inherited Destroy;
end;

function TFitRestApi.ProblemOf(const AId: string; out ACode: longint;
    out AError: string): TFitSession;
var
    Id: longint;
begin
    Result := nil;
    ACode := 200;
    AError := '';
    Id := StrToIntDef(AId, -1);
    if Id < 0 then
    begin
        ACode := 400;
        AError := 'bad problem id: "' + AId + '"';
        Exit;
    end;
    Result := FSessions.Find(Id);
    if Result = nil then
    begin
        ACode := 404;
        AError := Format('no such problem: %d', [Id]);
    end;
end;

{ Shortens a body for the log: enough to see what was sent, not a whole profile. }
function Brief(const S: string): string;
begin
    if Length(S) <= 200 then
        Result := S
    else
        Result := Copy(S, 1, 200) + Format('... (%d bytes)', [Length(S)]);
end;

{ Routes that must not take the problem's lock.

  TWO RULES, AND ONLY ONE OF THEM IS THIS UNIT'S.

  The first is the progress routes - state, async, rfactor - which are polled
  while an operation runs and must never wait behind the operation they report
  on, that being the whole point of polling them. Which routes those ARE is
  rest_polling's answer, and it is asked here rather than restated: this
  function used to carry its own copy of the same three names, matched by a
  different rule (exactly three segments, compared case-sensitively, where
  rest_polling reads the last segment of a path or a full URL). Two copies of
  one list is how a fourth polled route gets added to the documented home and
  missed here - and a polled route that takes the lock freezes the client's
  poll for the length of every fit, with nothing failing anywhere.

  The second is this unit's own: DELETE /problems/id destroys the problem, and
  with it the lock, so it cannot be the thing holding it. The registry's own
  lock guards that instead. It is not a polling question and does not belong in
  rest_polling. }
function IsUnlockedRoute(const AMethod, APath: string): boolean;
var
    Seg: TStringArray;
begin
    if IsPolledRoute(APath) then
        Exit(True);
    Seg := SplitPath(APath);
    Result := (Length(Seg) = 2) and (AMethod = 'DELETE');
end;

{ The problem a /problems/id/... path addresses, or nil. }
function TFitRestApi.SessionOfPath(const APath: string): TFitSession;
var
    Seg: TStringArray;
begin
    Result := nil;
    Seg := SplitPath(APath);
    if (Length(Seg) >= 2) and (Seg[0] = 'problems') then
        Result := FSessions.Find(StrToIntDef(Seg[1], -1));
end;

procedure TFitRestApi.Handle(const AMethod, APath, ABody: string;
    out ACode: longint; out AResponse: string);
var
    Started: TDateTime;
    Elapsed: int64;
    Session: TFitSession;
    Level: TMsgType;
begin
    //  The polled routes go to the Trace tier - the one tier off by default -
    //  because at two a second they would otherwise be the entire log. The rule
    //  lives in Common/rest_polling so the client cannot disagree about it.
    if IsPolledRoute(APath) then
        Level := TMsgType.Trace
    else
        Level := TMsgType.Notification;
    WriteLog(Format('--> %s %s  %s', [AMethod, APath, Brief(ABody)]), Level);
    Started := Now;
    //  The server is threaded: one problem may be reached by several connections
    //  at once. Serialize whatever touches the engine, but let the progress reads
    //  through unlocked - they exist to be polled while an operation runs.
    Session := SessionOfPath(APath);
    if Assigned(Session) and IsUnlockedRoute(AMethod, APath) then
        Session := nil;
    if Assigned(Session) then
        Session.Lock;
    try
    try
        HandleRoute(AMethod, APath, ABody, ACode, AResponse);
    except
        //  A REFUSAL IS NOT A FAULT, and the status code must not say it is.
        //
        //  EUserException is how the engine declines a request it cannot honour:
        //  a fit while another is running, a curve type this build does not have,
        //  moving a pick whose curve has been fitted. The request was wrong for
        //  the problem's state or content, which is a 400 - and 500 claimed the
        //  opposite, telling every consumer that the server had broken and the
        //  call was worth retrying unchanged. The desktop client never noticed
        //  because it reads the "ok" field and ignores the code, but the code is
        //  the part of this contract anything else reads first.
        //
        //  Logged at Warning, not Fatal, for the same reason: a user being told
        //  "no" is not an incident, and burying real faults among refusals in the
        //  log costs exactly when it matters. No stack trace either - the message
        //  is the whole story, and the trace is noise for a deliberate refusal.
        on E: EUserException do
        begin
            ACode := 400;
            AResponse := ErrorResponse(E.Message);
            WriteLog(Format('refused %s %s: %s', [AMethod, APath, E.Message]),
                TMsgType.Warning);
            Exit;
        end;
        //  Anything else IS a fault: the engine did something it did not intend.
        //  Never let it escape as a dead connection.
        on E: Exception do
        begin
            ACode := 500;
            AResponse := ErrorResponse(E.Message);
            WriteLog(Format('!!! %s %s -> %s: %s',
                [AMethod, APath, E.ClassName, E.Message]), TMsgType.Fatal);
            //  Where it came from - an engine assertion says little on its own.
            WriteLog(ExceptionTrace, TMsgType.Debug);
            Exit;
        end;
    end;
    Elapsed := MilliSecondsBetween(Now, Started);
    if ACode >= 400 then
        WriteLog(Format('<-- %d %s %s  %d ms  %s',
            [ACode, AMethod, APath, Elapsed, Brief(AResponse)]), TMsgType.Warning)
    else
        WriteLog(Format('<-- %d %s %s  %d ms  %s',
            [ACode, AMethod, APath, Elapsed, Brief(AResponse)]), Level);
    finally
        if Assigned(Session) then
            Session.Unlock;
    end;
end;

procedure TFitRestApi.HandleRoute(const AMethod, APath, ABody: string;
    out ACode: longint; out AResponse: string);
var
    Seg: TStringArray;
    Data: TJSONObject;
    Session: TFitSession;
    Points: TPointsData;
    Body: TJSONObject;
    Err, Res, Str: string;
    Resource: string;
    ResInfo: TModuleResource;
    SegIndex: longint;
    CurveIndex: longint;
    N: integer;
    Route: TRestRoute;
    Entries: TCurveValuesList;
    Fault: TRouteFault;
    States: TModuleStateArray;
    StateArr: TJSONArray;
begin
    ACode := 200;
    Seg := SplitPath(APath);
    N := Length(Seg);
    //  WHICH ROUTE THIS IS is rest_routes' answer; what it does is below. The
    //  three 404s that follow are NOT route questions - they guard the session
    //  lookup, and their order is why a request naming a problem that does not
    //  exist is answered "no such problem" even when the rest of its path is
    //  nonsense. Classifying first and refusing unknown routes first would
    //  change that answer.
    Route := RouteOf(AMethod, APath);

    //  GET /health
    if Route = rtHealth then
    begin
        Data := TJSONObject.Create;
        Data.Add('version', WORKER_PROTOCOL_VERSION);
        AResponse := OkResponse(Data);
        Exit;
    end;

    if (N = 0) or (Seg[0] <> 'problems') then
    begin
        ACode := 404;
        AResponse := ErrorResponse('unknown endpoint: ' + AMethod + ' ' + APath);
        Exit;
    end;

    //  POST /problems
    if Route = rtCreateProblem then
    begin
        Data := TJSONObject.Create;
        Data.Add('id', FSessions.CreateProblem);
        AResponse := OkResponse(Data);
        Exit;
    end;

    if N < 2 then
    begin
        ACode := 404;
        AResponse := ErrorResponse('unknown endpoint: ' + AMethod + ' ' + APath);
        Exit;
    end;

    Session := ProblemOf(Seg[1], ACode, Err);
    if Session = nil then
    begin
        AResponse := ErrorResponse(Err);
        Exit;
    end;

    //  DELETE /problems/{id}
    if Route = rtDiscardProblem then
    begin
        FSessions.Discard(Session.Id);
        AResponse := OkResponse(nil);
        Exit;
    end;

    if N < 3 then
    begin
        ACode := 404;
        AResponse := ErrorResponse('unknown endpoint: ' + AMethod + ' ' + APath);
        Exit;
    end;

    //  GET /problems/{id}/state
    if Route = rtState then
    begin
        Data := TJSONObject.Create;
        Data.Add('state', Ord(Session.Service.GetState));
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  PUT /problems/{id}/profile | background | positions | rfactor-bounds
    if Route = rtPutPointsSet then
    begin
        if not PointsFromJsonString(ABody, Points) then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed point set');
            Exit;
        end;
        //  HANDLES BELONG TO PICKS AND TO NOTHING ELSE. A curve's identity is
        //  issued to the pick it is seeded from, so a pick can be named and a
        //  profile sample cannot. Refused BY NAME rather than ignored: a field
        //  quietly dropped lets a client believe it restored an identity that
        //  was never established, which is exactly the silent degradation the
        //  DELETE member route refuses by name to avoid.
        if (Length(Points.Ids) > 0) and (Seg[2] <> 'positions') then
        begin
            ACode := 400;
            AResponse := ErrorResponse('curve identifiers may only be sent '
                + 'with the curve positions; the ' + Seg[2] + ' set has none. '
                + 'A curve''s identity is issued to the pick it is seeded '
                + 'from, so only a pick can carry one.');
            Exit;
        end;
        //  WHICH of the four, given that it IS one of the four - the route
        //  already established that, so this is a choice among known names
        //  rather than a second guard. The final else is rfactor-bounds.
        if Seg[2] = 'profile' then
            Res := Session.Service.SetProfilePointsSet(ToTitlePointsSet(Points))
        else if Seg[2] = 'background' then
            Res := Session.Service.SetBackgroundPointsSet(ToTitlePointsSet(Points))
        else if Seg[2] = 'positions' then
            Res := Session.Service.SetCurvePositions(ToTitlePointsSet(Points),
                Points.Ids)
        else
            Res := Session.Service.SetRFactorBounds(ToTitlePointsSet(Points));
        Data := TJSONObject.Create;
        Data.Add('message', Res);
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  GET /problems/{id}/module-states
    if Route = rtModuleStates then
    begin
        States := Session.Service.GetModuleProjectStates;
        StateArr := TJSONArray.Create;
        for SegIndex := 0 to High(States) do
        begin
            Body := TJSONObject.Create;
            Body.Add('module', States[SegIndex].Module);
            //  The document as TEXT, not parsed and re-emitted. The framework
            //  does not read what a module keeps, and re-encoding it here would
            //  be reading it.
            Body.Add('content', States[SegIndex].Content);
            StateArr.Add(Body);
        end;
        Data := TJSONObject.Create;
        Data.Add('states', StateArr);
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  PUT /problems/{id}/curves - the whole model's fitted values at once.
    if Route = rtPutCurves then
    begin
        if not ReadCurveValues(Session.Service, ABody, Entries, Fault) then
        begin
            //  404 for a handle the model does not hold, 400 for a body this
            //  route cannot read. The two call for opposite responses: a 400
            //  will fail again identically, a 404 says the model moved on.
            ACode := Fault.Code;
            AResponse := ErrorResponse(Fault.Message);
            Exit;
        end;
        Res := Session.Service.SetCurveValues(Entries);
        Data := TJSONObject.Create;
        Data.Add('message', Res);
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  GET /problems/{id}/profile | calc-profile | delta-profile
    if Route = rtGetPointsSet then
    begin
        if Seg[2] = 'profile' then
        begin
            AResponse := PointsResponse(Session.Service.GetProfilePointsSet);
            Exit;
        end;
        if Seg[2] = 'calc-profile' then
        begin
            AResponse := PointsResponse(Session.Service.GetCalcProfilePointsSet);
            Exit;
        end;
        if Seg[2] = 'delta-profile' then
        begin
            AResponse := PointsResponse(Session.Service.GetDeltaProfilePointsSet);
            Exit;
        end;
        if Seg[2] = 'background' then
        begin
            AResponse := PointsResponse(Session.Service.GetBackgroundPoints);
            Exit;
        end;
        if Seg[2] = 'positions' then
        begin
            //  WITH THE HANDLES, which is what makes the read the mirror of the
            //  write: a client can read the picks and hand exactly them back.
            AResponse := PointsResponse(Session.Service.GetCurvePositions,
                Session.Service.GetCurvePositionIds);
            Exit;
        end;
        //  The picks are 'positions'; what the model was built into is
        //  'calc-positions', on the same reading as profile/calc-profile.
        if Seg[2] = 'calc-positions' then
        begin
            AResponse := PointsResponse(
                Session.Service.GetResultedCurvePositions);
            Exit;
        end;
        if Seg[2] = 'rfactor-bounds' then
        begin
            AResponse := PointsResponse(Session.Service.GetRFactorBounds);
            Exit;
        end;
        if Seg[2] = 'rfactor' then
        begin
            Data := TJSONObject.Create;
            Data.Add('rFactor', Session.Service.GetRFactorStr);
            Data.Add('curMin', Session.CurMin);
            AResponse := OkResponse(Data);
            Exit;
        end;
    end;

    //  GET /problems/{id}/settings
    if Route = rtGetSettings then
    begin
        AResponse := OkResponse(SettingsOf(Session.Service));
        Exit;
    end;

    //  PUT /problems/{id}/settings - applies whichever fields are present
    if Route = rtPutSettings then
    begin
        Body := ParseMessage(ABody);
        if Body = nil then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed settings');
            Exit;
        end;
        try
            ApplySettings(Session.Service, Body);
        finally
            Body.Free;
        end;
        AResponse := OkResponse(SettingsOf(Session.Service));
        Exit;
    end;

    //  GET /problems/{id}/async - progress, for the client's polling loop
    if Route = rtAsync then
    begin
        Data := TJSONObject.Create;
        Data.Add('busy', Session.Service.AsyncOper);
        Data.Add('done', Session.IsDone);
        Data.Add('curMin', Session.CurMin);
        Data.Add('state', Ord(Session.Service.GetState));
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  GET /problems/{id}/stats
    if Route = rtStats then
    begin
        Data := TJSONObject.Create;
        Data.Add('calcTime', Session.Service.GetCalcTimeStr);
        Data.Add('rFactor', Session.Service.GetRFactorStr);
        Data.Add('absRFactor', Session.Service.GetAbsRFactorStr);
        Data.Add('sqrRFactor', Session.Service.GetSqrRFactorStr);
        //  The goodness-of-fit statistics the native engine does not itself keep.
        Data.Add('statistics', StatisticsJson(ServiceStatistics(Session.Service)));
        AResponse := OkResponse(Data);
        Exit;
    end;

    //  GET /problems/{id}/selected-interval
    if Route = rtSelectedInterval then
    begin
        AResponse := PointsResponse(Session.Service.GetSelectedProfileInterval);
        Exit;
    end;

    //  GET /problems/{id}/curves - every curve with its parameters
    if Route = rtCurves then
    begin
        AResponse := OkResponse(CurvesOf(Session.Service));
        Exit;
    end;

    //  GET /problems/{id}/special-params - the user-curve expression + parameters
    if Route = rtGetSpecialParams then
    begin
        AResponse := OkResponse(SpecialParamsOf(Session.Service));
        Exit;
    end;

    //  PUT /problems/{id}/special-params
    if Route = rtPutSpecialParams then
    begin
        Body := ParseMessage(ABody);
        if Body = nil then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed special parameters');
            Exit;
        end;
        try
            ApplySpecialParams(Session.Service, Body);
        finally
            Body.Free;
        end;
        AResponse := OkResponse(SpecialParamsOf(Session.Service));
        Exit;
    end;

    //  DELETE /problems/{id}/special-params - the user curve it describes is
    //  gone; the problem must not keep fitting its formula.
    if Route = rtDeleteSpecialParams then
    begin
        Session.Service.ClearSpecialCurve;
        AResponse := OkResponse(SpecialParamsOf(Session.Service));
        Exit;
    end;

    //  GET /problems/{id}/curves/{cid}/points - the curve's plotted points
    if Route = rtCurvePoints then
    begin
        CurveIndex := Session.Service.IndexOfCurveInstance(Seg[3]);
        if CurveIndex < 0 then
        begin
            //  404, NOT curve 0. StrToIntDef(Seg[3], 0) used to turn an
            //  unknown - or malformed - address into a silent read of the
            //  first curve.
            ACode := 404;
            AResponse := ErrorResponse(Format(
                'No curve %s exists in this model.', [Seg[3]]));
            Exit;
        end;
        AResponse := PointsResponse(
            CurvePointsOf(Session.Service, CurveIndex));
        Exit;
    end;

    //  PUT /problems/{id}/curves/{cid}/params/{j}  body: value
    if Route = rtCurveParam then
    begin
        CurveIndex := Session.Service.IndexOfCurveInstance(Seg[3]);
        if CurveIndex < 0 then
        begin
            //  404 rather than a write to curve 0 - see the points route.
            //  Writing to the wrong curve is worse than reading from it.
            ACode := 404;
            AResponse := ErrorResponse(Format(
                'No curve %s exists in this model.', [Seg[3]]));
            Exit;
        end;
        Body := ParseMessage(ABody);
        if Body = nil then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed parameter');
            Exit;
        end;
        try
            Session.Service.SetCurveParameter(CurveIndex,
                StrToIntDef(Seg[5], 0), Body.Get('value', 0.0));
        finally
            Body.Free;
        end;
        AResponse := OkResponse(nil);
        Exit;
    end;

    //  POST /problems/{id}/points/{set}   body: x, y   - append a point
    if Route = rtAddPoint then
    begin
        Body := ParseMessage(ABody);
        if Body = nil then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed point');
            Exit;
        end;
        try
            AddPoint(Session.Service, Seg[3], Body, ACode, Err);
        finally
            Body.Free;
        end;
        if ACode <> 200 then
            AResponse := ErrorResponse(Err)
        else
            AResponse := OkResponse(nil);
        Exit;
    end;

    //  PUT /problems/{id}/points/{set}  body: prevX, prevY, x, y  - move a point
    if Route = rtMovePoint then
    begin
        Body := ParseMessage(ABody);
        if Body = nil then
        begin
            ACode := 400;
            AResponse := ErrorResponse('malformed point');
            Exit;
        end;
        try
            ReplacePoint(Session.Service, Seg[3], Body, ACode, Err);
        finally
            Body.Free;
        end;
        if ACode <> 200 then
            AResponse := ErrorResponse(Err)
        else
            AResponse := OkResponse(nil);
        Exit;
    end;

    //  DELETE /problems/{id}/points/{set}/{pid} - remove one member by handle
    if Route = rtDeletePoint then
    begin
        //  ONE SET FOR NOW. The picks are the only members that carry a handle:
        //  a curve's identity is issued to the pick it is seeded from, so a
        //  pick can be named and a profile sample cannot. Refused by name
        //  rather than ignored, so a caller learns which sets this answers for.
        if Seg[3] <> 'positions' then
        begin
            ACode := 400;
            AResponse := ErrorResponse(Format(
                'The points of %s are not addressable one at a time; only ' +
                'positions are.', [Seg[3]]));
            Exit;
        end;

        CurveIndex := Session.Service.IndexOfCurveInstance(Seg[4]);
        if CurveIndex < 0 then
        begin
            //  404 rather than a guess. Deleting the wrong curve is the worst
            //  outcome available here - see the curve routes above.
            ACode := 404;
            AResponse := ErrorResponse(Format(
                'No curve %s exists in this model.', [Seg[4]]));
            Exit;
        end;

        //  The service removes the pick and the identity together; the reply is
        //  the refreshed collection, as rtDeleteSpecialParams answers with the
        //  parameters it just cleared.
        Session.Service.DeleteCurve(CurveIndex);
        AResponse := OkResponse(CurvesOf(Session.Service));
        Exit;
    end;

    //  GET | POST /problems/{id}/modules/{vendor}/{resource}
    //
    //  One route for everything modules contribute, replacing a route apiece.
    //  The reply is the resource itself rather than an ok-wrapped envelope,
    //  which is how these payloads already crossed the wire.
    //
    //  The policy each resource needs is read from its DECLARATION, not encoded
    //  here: a resource that says it needs the sidecar gets it started first,
    //  whatever the minimizer setting. Written per-route, that fact lived only
    //  in the router and the client had no way to know it.
    //  PUT is accepted alongside POST for a write: replacing a resource
    //  wholesale is what PUT means, and the bulk markup verb this replaced was a
    //  PUT. Refusing it would break every existing caller for no gain.
    if Route = rtModule then
    begin
        Resource := Seg[3];
        for SegIndex := 4 to N - 1 do
            Resource := Resource + '/' + Seg[SegIndex];

        if FindModuleResource(Resource, ResInfo) and
           ResInfo.NeedsPythonSidecar then
        begin
            if Assigned(FEnsurePythonSidecar) and FEnsurePythonSidecar(Str) then
                Session.Service.SetPythonSidecarUrl(Str)
            else
            begin
                //  Say which component is missing rather than returning an
                //  empty result - "not installed" and "found nothing" are
                //  different answers and must not look alike (D26).
                ACode := 503;
                AResponse := ErrorResponse(
                    'This feature needs the Python component, which could not ' +
                    'be started. Set it up in Worker/py: python3 -m venv .venv ' +
                    'then .venv/bin/pip install -r requirements.txt');
                Exit;
            end;
        end;

        try
            if AMethod = 'GET' then
                AResponse := Session.Service.ModuleGet(Resource)
            else
            begin
                AResponse := Session.Service.ModulePost(Resource, ABody);
                //  A write with nothing to report still answers like every other
                //  write route does. Returning an empty body instead leaves the
                //  caller parsing nothing, which is indistinguishable from a
                //  broken reply - and was, until a test dereferenced it.
                if AResponse = '' then
                    AResponse := OkResponse(nil);
            end;
        except
            //  Any refusal from the module - nothing marked, an unreadable
            //  reply, a resource no module owns - is the user's to see.
            on E: Exception do
            begin
                ACode := 400;
                AResponse := ErrorResponse(E.Message);
            end;
        end;
        Exit;
    end;

    if Route = rtAction then
    begin
        //  If this problem's minimizer is the Python sidecar, make sure it is
        //  running before a fit and tell the engine where to reach it. This is
        //  the single integration point: fit_server owns the sidecar; the engine
        //  just uses the URL through the IFitBackend seam. A native fit needs none
        //  of this.
        if MinimizerNeedsPythonSidecar(Session.Service.GetMinimizerKind) then
        begin
            if Assigned(FEnsurePythonSidecar) and FEnsurePythonSidecar(Str) then
                Session.Service.SetPythonSidecarUrl(Str)
            else
            begin
                ACode := 503;
                AResponse := ErrorResponse('The Python backend is not available. ' +
                    'Set it up in Worker/py: python3 -m venv .venv then ' +
                    '.venv/bin/pip install -r requirements.txt');
                Exit;
            end;
        end;
        RunAction(Session, Seg[3], ABody, ACode, Res, Err);
        if ACode <> 200 then
        begin
            AResponse := ErrorResponse(Err);
            Exit;
        end;
        Data := TJSONObject.Create;
        Data.Add('message', Res);
        AResponse := OkResponse(Data);
        Exit;
    end;

    ACode := 404;
    AResponse := ErrorResponse('unknown endpoint: ' + AMethod + ' ' + APath);
end;

end.
