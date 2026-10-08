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
HTTP server onto it. The list below is orientation: what the surface IS, in a
form something other than a person can read, is what GET /openapi.json answers,
and that one is assembled from the routing itself (see Worker/rest_spec.pas).

Routes implemented so far:
  GET    /health
  GET    /openapi.json                               -> this build's OpenAPI doc
  GET    /docs                                       -> Swagger UI, as HTML
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
    fit_worker_protocol, fit_points_json, fit_problem_json, fit_progress_json,
    //  A point set as the wire carries it - one conversion, shared with the
    //  progress frames (fit_service), so a field reaches both or neither.
    wire_point_sets,
    Variants,
    fit_server_session,
    //  TStopQuery: how the Python engine's start hears the run's Stop.
    fit_service,
    int_fit_service, minimizer_registry, minimizer_registration, action_registry,
    int_app_module, module_registry
    //  Which route a request names - the table, as a table.
    , rest_routes,
    //  What this build tells a caller about itself: the OpenAPI document and
    //  the page that renders it.
    rest_spec,
    points_set, title_points_set, named_points_set,
    self_copied_component, persistent_curve_parameters,
    persistent_curve_parameter_container, special_curve_parameter, log,
    fit_statistics, fit_service_statistics,
    //  EUserException: the engine's way of saying the REQUEST was inadmissible,
    //  as opposed to the engine being broken. The two get different status codes
    //  and different log tiers - see the handler at the bottom of Handle.
    MyExceptions,
    //  Why a written parameter is held at another value.
    fit_advice;

const
    { WHAT THE PROGRESS SAYS WHILE A FIT WAITS FOR THE PYTHON ENGINE. The first
      start after installing is the slow one - the frozen sidecar took 114 s on
      an Intel Mac while macOS looked over each of its libraries once - and a
      wait of that length that says nothing reads as a hung window. }
    PythonEngineStartingStage = 'Starting the Python engine. Its first start ' +
        'after installing can take a minute or two';
    { HOW LONG A MODULE'S FEATURE WAITS FOR THE PYTHON ENGINE: under the
      client's ordinary 30 s reply timeout, so the caller hears "not
      available" in words rather than a timeout. Such a request has no run to
      show its wait in and no Stop, and holds the problem while it waits - so
      it does not get the fit's five minutes. fit_server starts the engine at
      start-up when a module has Python routes, so it is usually ready. }
    ModuleSidecarWaitMsDefault = 25000;

type
    { Ensures the Python sidecar fit_server owns is running and returns its base
      URL (empty when unavailable). Waits while it starts, until AStop - which
      may be nil - says the caller stopped. Wired by fit_server; nil when there
      is no sidecar. Keeps the REST layer testable without spawning Python. }
    TEnsurePythonSidecar = function(AStop: TStopQuery; out AUrl: string): boolean of object;

    { A component the request needs is not there to be had - the Python
      engine, say. Still the user's to fix rather than a fault, but answered
      503 rather than 400: nothing was wrong with the request. }
    EComponentUnavailable = class(EUserException);

    { Routes one request. Owns the problem registry. }
    TFitRestApi = class(TObject)
    private
        FSessions: TSessionRegistry;
        FEnsurePythonSidecar: TEnsurePythonSidecar;
        FStartPythonSidecar: TThreadMethod;
        FModuleSidecarWaitMs: longint;
        { What a problem's fit asks as it begins, when it needs the Python
          engine: FEnsurePythonSidecar, with the run's own Stop. }
        function PythonUrlFor(AStop: TStopQuery): string;
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
        { Set by fit_server: starts the sidecar without waiting for it. }
        property StartPythonSidecar: TThreadMethod
            read FStartPythonSidecar write FStartPythonSidecar;
        { How long a module feature waits for the sidecar. A property rather
          than the constant alone so a test can wait milliseconds, not the
          25 s a real caller is given. }
        property ModuleSidecarWaitMs: longint
            read FModuleSidecarWaitMs write FModuleSidecarWaitMs;
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
{ A points response, taking ownership of the set the service handed back.

  AIds rides along for the picks and is empty everywhere else, exactly as it is
  on the write side: a curve's identity is issued to the pick it is seeded from,
  so only the picks have any. }
{ Writes AValue to parameter AParamIndex of curve ACurveIndex and answers what
  the model then holds: {"value": held}, and "note" saying why when it is not
  the value written (fit_advice.AdviseHeldParameterValue) - so any REST client
  hears what the window shows, rather than only the desktop comparing. }
function CurveParameterWriteReply(ASvc: IFitService;
    ACurveIndex, AParamIndex: longint; AValue: double): string;
var
    Name_, Why: string;
    Held: double;
    Kind: longint;
    Data: TJSONObject;
begin
    ASvc.SetCurveParameter(ACurveIndex, AParamIndex, AValue);
    Name_ := '';
    Held := AValue;
    Kind := 0;
    ASvc.GetCurveParameter(ACurveIndex, AParamIndex, Name_, Held, Kind);
    Data := TJSONObject.Create;
    Data.Add('value', Held);
    if AdviseHeldParameterValue(Name_, AValue, Held, Why) then
        Data.Add('note', Why);
    Result := OkResponse(Data);
end;

function PointsResponse(APoints: TTitlePointsSet;
    const AIds: TCurveInstanceIdList = nil): string;
var
    D: TPointsData;
    i: longint;
begin
    try
        if Assigned(APoints) then
            D := PointsDataOf(APoints, APoints.FTitle)
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
var
    Ids: TJSONArray;
    BackgroundIds: TCurveInstanceIdList;
    i: longint;
begin
    Result := TJSONObject.Create;
    Result.Add('maxRFactor', ASvc.GetMaxRFactor);
    Result.Add('backFactor', ASvc.GetBackFactor);
    Result.Add('curveThresh', ASvc.GetCurveThresh);
    Result.Add('waveLength', ASvc.GetWaveLength);
    Result.Add('backgroundVariation', ASvc.GetBackgroundVariationEnabled);
    Result.Add('curveScaling', ASvc.GetCurveScalingEnabled);
    Result.Add('minimizerKind', ASvc.GetMinimizerKind);
    Result.Add('fitThreads', ASvc.GetFitThreads);
    Result.Add('lossKind', ASvc.GetLossKind);
    Result.Add('weighting', ASvc.GetWeighting);
    Result.Add('curveType', GUIDToString(ASvc.GetCurveType));
    //  A project saved before one model held one module's curves may mix
    //  them; read-only, and what opening such a project warns about.
    Result.Add('modelMixesModules', ASvc.ModelMixesModules);
    //  THE MODEL'S BACKGROUND, beside the peak type: "" for none, which is
    //  also what a client that predates it will be read as having asked for.
    if IsEqualGUID(ASvc.GetBackgroundCurveType, GUID_NULL) then
        Result.Add('backgroundCurveType', '')
    else
        Result.Add('backgroundCurveType', GUIDToString(ASvc.GetBackgroundCurveType));
    Ids := TJSONArray.Create;
    BackgroundIds := ASvc.GetBackgroundCurveIds;
    for i := 0 to High(BackgroundIds) do
        Ids.Add(BackgroundIds[i]);
    Result.Add('backgroundCurveIds', Ids);
end;

{ Applies whichever settings the body carries; absent fields are left alone. }
procedure ApplySettings(ASvc: IFitService; O: TJSONObject);
var
    S: string;
    Background: TGuid;
    Ids: TCurveInstanceIdList;
    IdArray: TJSONArray;
    i: longint;
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
    if O.Find('fitThreads') <> nil then
        ASvc.SetFitThreads(O.Get('fitThreads', ASvc.GetFitThreads));
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
    //  LAST, after backgroundVariation: a body that switches the variation off
    //  and adds a background curve is applied in the order that makes it
    //  admissible. The type and its handles are ONE write - see
    //  IFitService.SetBackgroundCurveType.
    if (O.Find('backgroundCurveType') <> nil) or
       (O.Find('backgroundCurveIds') <> nil) then
    begin
        Background := ASvc.GetBackgroundCurveType;
        if O.Find('backgroundCurveType') <> nil then
        begin
            S := O.Get('backgroundCurveType', '');
            if S = '' then
                Background := GUID_NULL
            else if not TryStringToGUID(S, Background) then
                raise EUserException.Create('"' + S +
                    '" is not a curve type identifier.');
        end;
        Ids := nil;
        if O.Find('backgroundCurveIds') is TJSONArray then
        begin
            IdArray := TJSONArray(O.Find('backgroundCurveIds'));
            SetLength(Ids, IdArray.Count);
            for i := 0 to IdArray.Count - 1 do
                Ids[i] := IdArray.Items[i].AsString;
        end;
        ASvc.SetBackgroundCurveType(Background, Ids);
    end;
end;

{ Every curve with its parameters (name, value, type) - and, given the engine
  in APointsFrom, the points GET /curves/{cid}/points would answer for each,
  so a client drawing the model makes one request rather than one per curve. }
function CurvesOf(ASvc: IFitService; APointsFrom: TFitService = nil): TJSONObject;
var
    Points: TTitlePointsSet;
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
        //  WHICH TYPE, because a model may hold more than one - peaks and a
        //  background - and the client used to read it back from the title.
        CurveObj.Add('curveType', GUIDToString(ASvc.GetCurveTypeOf(i)));
        //  THE SAME POINTS THE CURVE'S OWN ROUTE SENDS, built the same way -
        //  CurvePointsCopy and PointsDataOf, baseline included - so the two
        //  can never draw a curve differently.
        if Assigned(APointsFrom) then
        begin
            Points := APointsFrom.CurvePointsCopy(i);
            try
                if Assigned(Points) then
                    CurveObj.Add('points', PointsToJson(
                        PointsDataOf(Points, Points.FTitle)));
            finally
                Points.Free;
            end;
        end;
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
                O.Add('held', CP[i].VariationDisabled);
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
                //  Held at its value; absent from an older client, varied.
                Container.Parameter.VariationDisabled := P.Get('held', False);
            end;
    end;
    //  Nil means "initialize from the expression" - the service's own contract.
    ASvc.SetSpecialCurveParameters(O.Get('expression', ''), CP);
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

procedure ActEvaluateModel(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200; AError := '';
    AResult := ASession.Service.EvaluateModel;
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
    //  SYNCHRONOUS: a sum over profiles the tasks already hold, never a fit.
    Add('evaluate-model',
        'Measure the model as it stands, without fitting it.',
        @ActEvaluateModel);
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

type
    { A wait with a deadline, as a stop question: "stop" once it has passed. }
    TStopAfter = class(TObject)
    private
        FDeadline: TDateTime;
    public
        constructor Create(AMs: longint);
        function Passed: boolean;
    end;

constructor TStopAfter.Create(AMs: longint);
begin
    inherited Create;
    FDeadline := IncMilliSecond(Now, AMs);
end;

function TStopAfter.Passed: boolean;
begin
    Result := Now >= FDeadline;
end;

{ TFitRestApi }

constructor TFitRestApi.Create;
begin
    inherited Create;
    FSessions := TSessionRegistry.Create;
    FModuleSidecarWaitMs := ModuleSidecarWaitMsDefault;
end;

destructor TFitRestApi.Destroy;
begin
    FSessions.Free;
    inherited Destroy;
end;

function TFitRestApi.PythonUrlFor(AStop: TStopQuery): string;
begin
    Result := '';
    if Assigned(FEnsurePythonSidecar) and FEnsurePythonSidecar(AStop, Result) then
        Exit;
    Result := '';
    //  STOPPED, NOT MISSING: the run ends as a stopped one does.
    if Assigned(AStop) and AStop() then
        Exit;
    raise EComponentUnavailable.Create('The Python backend is not available. ' +
        'Set it up as the user guide describes: Help > Explain Everything, ' +
        'Fitting, Setting up the Python engine.');
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

  THREE RULES, AND ONLY TWO OF THEM ARE THIS UNIT'S.

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
  rest_polling.

  The third is the stop action, and it is the only MUTATING route here. It is
  also the only one whose entire purpose is to reach a problem that is busy:
  everything else in this program waits for the fit because waiting is correct,
  while a stop that waits for the fit is a stop that never happens. It sets the
  engine's termination flag on the running tasks - under the service's own task
  lock - and reads nothing of the model, so it needs none of the protection the
  session lock gives. }
function IsUnlockedRoute(const AMethod, APath: string): boolean;
var
    Seg: TStringArray;
begin
    if IsPolledRoute(APath) then
        Exit(True);
    Seg := SplitPath(APath);
    if (Length(Seg) = 2) and (AMethod = 'DELETE') then
        Exit(True);
    //  AND STOP, which is the one ACTION that exists to reach a problem while
    //  it is busy. Served on the locked path it waited for the very fit it was
    //  meant to interrupt: the window froze for the rest of that fit and was
    //  then told the calculation had not started, because by the time the
    //  request was let in the operation was over.
    //
    //  Safe to let through because of what it does rather than because of what
    //  it reads: it sets the engine's own termination flag on the running tasks
    //  under the service's task lock, and touches nothing else of the model.
    Result := (Length(Seg) = 4) and (AMethod = 'POST') and
        (Seg[2] = 'actions') and (Seg[3] = 'stop');
end;

{ GET /problems/id/settings, which never waits for an action - see Handle. }
function IsSettingsRead(const AMethod, APath: string): boolean;
var
    Seg: TStringArray;
begin
    Seg := SplitPath(APath);
    Result := (AMethod = 'GET') and (Length(Seg) = 3) and
        (Seg[2] = 'settings');
end;

{ POST /problems/id/actions/name: an operation, which runs to its end on the
  request's thread holding the problem's lock. Stop is not one - it never
  takes the lock (IsUnlockedRoute). }
function IsActionCall(const AMethod, APath: string): boolean;
var
    Seg: TStringArray;
begin
    Seg := SplitPath(APath);
    Result := (AMethod = 'POST') and (Length(Seg) = 4) and
        (Seg[2] = 'actions');
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
    Published, Answered: boolean;
begin
    //  The polled routes go to the Trace tier - the one tier off by default -
    //  because at two a second they would otherwise be the entire log. The rule
    //  lives in Common/rest_polling so the client cannot disagree about it.
    if IsPolledRoute(APath) then
        Level := TMsgType.Trace
    else
        Level := TMsgType.Notification;
    //  Built only when it will be written: a polled route is asked ten times a
    //  second during a fit, and the Format and Brief ran on each whatever the
    //  tier (fit-performance.md, stage 3).
    if LogEnabled(Level) then
        WriteLog(Format('--> %s %s  %s', [AMethod, APath, Brief(ABody)]), Level);
    Started := Now;
    //  The server is threaded: one problem may be reached by several connections
    //  at once. Serialize whatever touches the engine, but let the progress reads
    //  through unlocked - they exist to be polled while an operation runs.
    Session := SessionOfPath(APath);
    if Assigned(Session) and IsUnlockedRoute(AMethod, APath) then
        Session := nil;
    Published := False;
    Answered := False;
    if Assigned(Session) and IsSettingsRead(AMethod, APath) then
        //  THE SETTINGS NEVER WAIT FOR AN ACTION: the lock if it is free, the
        //  reply published as the running action began if it is not
        //  (TFitSession.PublishSettings). Asked in turn rather than once each,
        //  because the action may take the lock between the two questions.
        while True do
        begin
            if Session.SettingsWhileBusy(AResponse) then
            begin
                ACode := 200;
                Answered := True;
                Session := nil;
                Break;
            end;
            if Session.TryLock then
                Break;
            Sleep(2);
        end
    else if Assigned(Session) then
    begin
        Session.Lock;
        //  An action runs to its end on this thread, holding the lock: the
        //  settings as it began are what they are until it ends.
        if IsActionCall(AMethod, APath) then
        begin
            Session.PublishSettings(OkResponse(SettingsOf(Session.Service)));
            Published := True;
        end;
    end;
    try
    try
        if not Answered then
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
        on E: EComponentUnavailable do
        begin
            ACode := 503;
            AResponse := ErrorResponse(E.Message);
            WriteLog(Format('unavailable %s %s: %s', [AMethod, APath, E.Message]),
                TMsgType.Warning);
            Exit;
        end;
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
    else if LogEnabled(Level) then
        WriteLog(Format('<-- %d %s %s  %d ms  %s',
            [ACode, AMethod, APath, Elapsed, Brief(AResponse)]), Level);
    finally
        if Published then
            Session.WithdrawSettings;
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
    Patience: TStopAfter;
    Available: boolean;
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
    Progress: TFitProgressReport;
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

    //  GET /openapi.json - what this build's surface is, in its own words.
    //  Answered before the guard below because it hangs off the root: it
    //  describes the server, and there is no problem to describe it against.
    if Route = rtOpenApi then
    begin
        //  The verbs have to be registered before they can be enumerated, and
        //  a document that listed none because nobody had run one yet would be
        //  wrong in the way that is hardest to notice. Idempotent, and the same
        //  call RunAction makes.
        RegisterBuiltInActions;
        AResponse := OpenApiJson;
        Exit;
    end;

    //  GET /docs - the page that reads it. The only reply this server sends
    //  that is not JSON; fit_server asks rest_spec.ContentTypeOf what to label
    //  it with.
    if Route = rtDocs then
    begin
        AResponse := SwaggerUiHtml;
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
            //  AN ENGINE THAT HAS TO START IS STARTED WHEN IT IS CHOSEN - or
            //  restored with a project - and not waited for: its first start
            //  can take minutes, better spent while the user loads data than
            //  in front of the first fit, which then usually finds it ready.
            if (Body.Find('minimizerKind') <> nil) and
               MinimizerNeedsPythonSidecar(Session.Service.GetMinimizerKind) and
               Assigned(FStartPythonSidecar) then
                FStartPythonSidecar();
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

    //  GET /problems/{id}/progress?since=<seq>&snapshot=0|1&snapshotSince=<rev>
    //  - how a running fit
    //  is going. Unlocked, like the other polled routes: GetFitProgress reads
    //  only the progress log, which guards itself.
    if Route = rtProgress then
    begin
        Progress := Session.Service.GetFitProgress(
            StrToIntDef(QueryParam(APath, 'since', '0'), 0),
            QueryParam(APath, 'snapshot', '0') = '1',
            //  The frame the caller drew: an unchanged one is not sent again.
            StrToIntDef(QueryParam(APath, 'snapshotSince', '0'), 0));
        AResponse := OkResponse(FitProgressToJson(Progress));
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
        //  How many workers the last fit's intervals ran on (stage 8).
        Data.Add('fitWorkers', Session.Service.LastFitWorkers);
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

    //  GET /problems/{id}/curves - every curve with its parameters, and with
    //  ?points=1 the points of each as well
    if Route = rtCurves then
    begin
        if QueryParam(APath, 'points', '0') = '1' then
            AResponse := OkResponse(CurvesOf(Session.Service, Session.Service))
        else
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
        //  THAT curve, not a copy of the model to pick it out of - see
        //  TFitService.CurvePointsCopy.
        AResponse := PointsResponse(
            Session.Service.CurvePointsCopy(CurveIndex));
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
            AResponse := CurveParameterWriteReply(Session.Service, CurveIndex,
                StrToIntDef(Seg[5], 0), Body.Get('value', 0.0));
        finally
            Body.Free;
        end;
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
            Patience := TStopAfter.Create(FModuleSidecarWaitMs);
            try
                Available := Assigned(FEnsurePythonSidecar) and
                    FEnsurePythonSidecar(@Patience.Passed, Str);
            finally
                Patience.Free;
            end;
            if Available then
                Session.Service.SetPythonSidecarUrl(Str)
            else
            begin
                //  Say which component is missing rather than returning an
                //  empty result - "not installed" and "found nothing" are
                //  different answers and must not look alike (D26).
                ACode := 503;
                AResponse := ErrorResponse(
                    'This feature needs the Python component, which could not ' +
                    'be started. Set it up as the user guide describes: Help > ' +
                    'Explain Everything, Fitting, Setting up the Python engine.');
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
        //  HOW A FIT REACHES THE PYTHON SIDECAR, should it need it. This is the
        //  single integration point: fit_server owns the sidecar; the engine
        //  just asks this source through the IFitBackend seam.
        //
        //  IT USED TO BE ASKED HERE, before every action of a problem set to
        //  Python: on the request thread, outside the run, for up to ten
        //  seconds. Stop could not end that wait, the progress did not say what
        //  it was, the Stop request itself passed through it - starting a
        //  second sidecar on another thread - and smoothing waited for an
        //  engine it never uses. Now the fit asks, as it begins and only if it
        //  needs it (TFitTask.Optimization), inside the run.
        Session.Service.SetPythonSidecarSource(@PythonUrlFor,
            PythonEngineStartingStage);
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
