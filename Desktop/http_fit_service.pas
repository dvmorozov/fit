// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(IFitService implemented over the compute server's REST API.)

This is the thin client's half of the transport that replaced XML-RPC/WST: it
implements exactly the same IFitService the in-process engine does, but every
verb is an HTTP+JSON call to an independently running fit_server (default
http://127.0.0.1:8787, possibly on another machine).

The client keeps a problem (session) on the server - the ProblemID model the
original API used - created lazily on first use.

Objects that cross the wire are rebuilt here, exactly as the retired
fit_client_proxy did: curves come back as TNamedPointsSet instances filled with
points and given their type name via SetCurveTypeName (the method whose whole
purpose is "deserializing objects received from server").
}
unit http_fit_service;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpjson, jsonparser, fphttpclient,
    int_fit_service, fit_statistics, mscr_specimen_list, named_points_set,
    points_set, self_copied_component, title_points_set,
    persistent_curve_parameters, Variants, client_log,
    int_app_module, module_registry,
    //  The two weighting names, and what an unrecognised one means.
    fit_weighting;

const
    { Where the compute server is expected unless configured otherwise. }
    DEFAULT_SERVER_URL = 'http://127.0.0.1:8787';
    { Connecting is either immediate or hopeless. }
    CONNECT_TIMEOUT_MS = 5000;
    { A reply to an ordinary call: generous, but never indefinite. }
    REPLY_TIMEOUT_MS = 30000;
    { Wave detection searches, so it may take as long as a fit. Giving up at the
      ordinary reply timeout would report a failure for something merely slow. }
    LONG_OPERATION_TIMEOUT_MS = 180000;
    { A fit has no useful upper bound, and it runs on a worker thread, so it is
      allowed to take as long as it takes. }
    NO_TIMEOUT = 0;

type
    { The compute server, seen through the IFitService contract. }
    THttpFitService = class(TObject, IFitService)
    private
        FBaseUrl:   string;
        FProblemId: longint;

        { Ensures a problem exists on the server; returns its id. }
        function ProblemId: longint;
        function Url(const ASuffix: string): string;

        { A client with timeouts, so a server that is down or wedged cannot make
          the application wait forever. }
        function NewClient(ATimeoutMs: integer): TFPHTTPClient;
        { Reports a transport failure in terms the user can act on. }
        procedure TransportFailed(E: Exception);
        function HttpGet(const APath: string): TJSONObject;
        function HttpSend(const AMethod, APath, ABody: string;
            ATimeoutMs: integer = REPLY_TIMEOUT_MS): TJSONObject;

    protected
        { THE ONE PLACE BYTES CROSS THE PROCESS BOUNDARY, and the only thing a test
          has to replace.

          Everything else in this unit is marshalling: building a URL, encoding a
          body, reading a reply, deciding what a missing field means. That is some
          seven hundred lines of decisions, and none of it was reachable by a test
          because each caller constructed its own TFPHTTPClient inline - so a
          double could only override the high-level verbs, which is precisely the
          code it wanted to exercise. The three call sites also repeated the same
          "a rejection is the server talking, anything else is the transport
          failing" handling three times over.

          Overriding Fetch and Send in a descendant runs every line of the real
          marshalling against a canned reply. See tests/mocks/mock_http_transport. }
        function Fetch(const AUrl: string; ATimeoutMs: integer): string; virtual;
        function Send(const AMethod, AUrl, ABody: string;
            ATimeoutMs: integer): string; virtual;

    private
        function GetPoints(const APath: string): TTitlePointsSet; overload;
        function GetPoints(const APath: string;
            out AIds: TCurveInstanceIdList): TTitlePointsSet; overload;
        function PutPoints(const APath: string; APoints: TPointsSet;
            const AIds: TCurveInstanceIdList = nil): string;
        function RunAction(const AName: string; const ABody: string = ''): string;
        function Settings: TJSONObject;
        procedure PutSetting(const AName: string; AValue: TJSONData);
    public
        constructor Create(const ABaseUrl: string);
        destructor Destroy; override;

        { True when the server answers /health. }
        function IsAvailable: boolean;

        { IFitService }
        function GetMaxRFactor: double;
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetBackFactor: double;
        procedure SetBackFactor(ABackFactor: double);
        function GetCurveThresh: double;
        procedure SetCurveThresh(ACurveThresh: double);
        { Virtual so a test can record the selection without a live server. }
        function GetCurveType: TCurveTypeId; virtual;
        procedure SetCurveType(ACurveTypeId: TCurveTypeId); virtual;
        function GetState: TFitServerState;
        function GetWaveLength: double;
        procedure SetWaveLength(AWaveLength: double);
        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);
        function GetMinimizerKind: longint;
        procedure SetMinimizerKind(AKind: longint);
        function GetLossKind: longint;
        procedure SetLossKind(AKind: longint);
        function GetWeighting: string;
        procedure SetWeighting(const AValue: string);
        function GetServerUrl: string;
        procedure SetServerUrl(const AUrl: string);
        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);

        function SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
        function GetProfilePointsSet: TTitlePointsSet; virtual;
        function GetSelectedProfileInterval: TTitlePointsSet;
        function SetBackgroundPointsSet(ABackgroundPoints: TTitlePointsSet): string;
        function GetBackgroundPoints: TTitlePointsSet;
        function SetCurvePositions(ACurvePositions: TPointsSet;
            const AIds: TCurveInstanceIdList = nil): string;
        function GetCurvePositions: TTitlePointsSet virtual;
        function GetCurvePositionIds: TCurveInstanceIdList;
        function IsCurveFitted(ACurveIndex: longint): boolean;
        function GetModuleProjectStates: TModuleStateArray;
        function GetResultedCurvePositions: TTitlePointsSet virtual;
        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet virtual;
        {  Virtual so a test can hold the bounds in memory instead of sending
           them, the way SetCurveType already allows. The refresh getters below
           are virtual for the same reason: a client action changes the model AND
           redraws, and a test of the model change must not need a live server to
           satisfy the redraw. }
        { Virtual so a test can hold a module's state in memory instead of
          sending it anywhere - the same reason the verbs these replaced were
          virtual. }
        function ModuleGet(const AResource: string): string; virtual;
        function ModulePost(const AResource, APayload: string): string; virtual;

        function GetSpecialCurveParameters: Curve_parameters;
        procedure ClearSpecialCurve;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
            CP: Curve_parameters);

        procedure AddPointToProfile(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        {  Virtual for the reason given above: a pick changes the model AND
           redraws, so a test of the redraw must not need a live server to
           satisfy the send. }
        procedure AddPointToRFactorBounds(XValue, YValue: double); virtual;
        procedure AddPointToSet(const AKind: string; XValue, YValue: double); virtual;
        procedure AddPointToCurvePositions(XValue, YValue: double); virtual;

        procedure ReplacePointInProfile(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInSet(const AKind: string;
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);

        function GetCurveAttributes: TMSCRCurveList virtual;
        function GetCurveCount: longint;
        function GetCurveInstanceId(ACurveIndex: longint): string;
        function IndexOfCurveInstance(const AInstanceId: string): longint;
        function GetCurveParameterCount(ACurveIndex: longint): longint;
        procedure GetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        function GetCurveParameterError(ACurveIndex: longint;
            ParamIndex: longint): double;
        function GetCurveParameterValue(ACurveIndex: longint;
            ParamIndex: longint): Variant;
        procedure SetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            Value: double);
        function SetCurveValues(const AEntries: TCurveValuesList): string;
        function DeleteCurve(ACurveIndex: longint): string;
        function GetCurves: TSelfCopiedCompList virtual;

        function GetCalcProfilePointsSet: TTitlePointsSet virtual;
        function GetDeltaProfilePointsSet: TTitlePointsSet virtual;

        function SmoothProfile: string;
        procedure SubtractBackground(Auto: boolean);
        function DoAllAutomatically: string;
        function MinimizeDifference: string;
        function MinimizeDifferenceAgain: string;
        function MinimizeNumberOfCurves: string;
        function ComputeCurveBounds: string;
        function ComputeBackgroundPoints: string;
        function ComputeCurvePositions: string;
        function SelectAllPointsAsCurvePositions: string;

        procedure StopAsyncOper;
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;
        function GetStatistics: TFitStatistics;

        function SelectProfileInterval(
            StartPointIndex, StopPointIndex: longint): string;
        function SelectEntireProfile: string;
        procedure CreateCurveList;
    end;

implementation

uses
    curve_instance_id, fit_points_json, persistent_curve_parameter_container,
    special_curve_parameter, MyExceptions;

constructor THttpFitService.Create(const ABaseUrl: string);
begin
    inherited Create;
    FBaseUrl := ABaseUrl;
    while (FBaseUrl <> '') and (FBaseUrl[Length(FBaseUrl)] = '/') do
        SetLength(FBaseUrl, Length(FBaseUrl) - 1);
    FProblemId := -1;
end;

destructor THttpFitService.Destroy;
begin
    if FProblemId > 0 then
        //  Release the server-side problem. A failure here must not break
        //  shutdown, so it is swallowed - but it goes through Send like every
        //  other request, and that is a fix rather than tidying.
        //
        //  This used to build a bare TFPHTTPClient itself, which meant it had NO
        //  TIMEOUTS: NewClient is where ConnectTimeout is set, precisely so that a
        //  server which is down or wedged cannot make the application wait
        //  forever, and the destructor was the one call that opted out. Against a
        //  dead port that cost three seconds of silent waiting on every teardown;
        //  against a wedged one it could hang the shutdown outright, with the
        //  exception handler above hiding that it was even trying.
        try
            Send('DELETE', Format('%s/problems/%d', [FBaseUrl, FProblemId]),
                '', CONNECT_TIMEOUT_MS);
        except
        end;
    inherited Destroy;
end;

{ Raises the server's error message as a user exception, so the UI shows it. }
function CheckedResponse(const ABody: string): TJSONObject;
var
    D: TJSONData;
begin
    Result := nil;
    D := nil;
    try
        D := GetJSON(ABody);
    except
        D := nil;
    end;
    if not (D is TJSONObject) then
    begin
        D.Free;
        LogClientWarning('unreadable reply from the compute server: ' +
            Copy(ABody, 1, 200));
        raise Exception.Create('The compute server returned an unreadable reply.');
    end;
    Result := TJSONObject(D);
    if not Result.Get('ok', True) then
    begin
        try
            LogClientWarning('the compute server rejected the request: ' +
                Result.Get('error', '(no message)'));
            raise EUserException.Create(Result.Get('error', 'The compute server ' +
                'rejected the request.'));
        finally
            Result.Free;
        end;
    end;
end;

function THttpFitService.NewClient(ATimeoutMs: integer): TFPHTTPClient;
begin
    Result := TFPHTTPClient.Create(nil);
    Result.ConnectTimeout := CONNECT_TIMEOUT_MS;
    Result.IOTimeout := ATimeoutMs;
end;

procedure THttpFitService.TransportFailed(E: Exception);
begin
    LogClientWarning(Format('server at %s unreachable: %s (%s)',
        [FBaseUrl, E.Message, E.ClassName]));
    raise EUserException.Create(Format(
        'The compute server at %s could not be reached (%s).' + LineEnding +
        'Start it, or point the client at another one with Fit -> Compute Server...',
        [FBaseUrl, E.Message]));
end;

function THttpFitService.Url(const ASuffix: string): string;
begin
    Result := Format('%s/problems/%d%s', [FBaseUrl, ProblemId, ASuffix]);
end;

{ The one place a rejection is separated from a transport failure: a rejection is
  the server talking and must reach the user as its own message, anything else is
  the connection and must say so. It used to be written out at each of the three
  call sites. }
function THttpFitService.Fetch(const AUrl: string; ATimeoutMs: integer): string;
var
    C: TFPHTTPClient;
begin
    Result := '';
    C := NewClient(ATimeoutMs);
    try
        try
            Result := C.Get(AUrl);
        except
            on E: EUserException do
                raise;
            on E: Exception do
                TransportFailed(E);
        end;
    finally
        C.Free;
    end;
end;

function THttpFitService.Send(const AMethod, AUrl, ABody: string;
    ATimeoutMs: integer): string;
var
    C: TFPHTTPClient;
    Req, Resp: TStringStream;
begin
    Result := '';
    C := NewClient(ATimeoutMs);
    Req := TStringStream.Create(ABody);
    Resp := TStringStream.Create('');
    try
        C.RequestBody := Req;
        C.AddHeader('Content-Type', 'application/json');
        try
            C.HTTPMethod(AMethod, AUrl, Resp, []);
        except
            on E: EUserException do
                raise;
            on E: Exception do
                TransportFailed(E);
        end;
        Result := Resp.DataString;
    finally
        Resp.Free;
        Req.Free;
        C.Free;
    end;
end;

function THttpFitService.HttpGet(const APath: string): TJSONObject;
var
    Body: string;
    Started: QWord;
begin
    Started := GetTickCount64;
    Body := Fetch(APath, REPLY_TIMEOUT_MS);
    LogServerCall('GET', APath, GetTickCount64 - Started,
        Format('%d bytes', [Length(Body)]));
    Result := CheckedResponse(Body);
end;

function THttpFitService.HttpSend(const AMethod, APath, ABody: string;
    ATimeoutMs: integer): TJSONObject;
var
    Body: string;
    Started: QWord;
begin
    Started := GetTickCount64;
    Body := Send(AMethod, APath, ABody, ATimeoutMs);
    LogServerCall(AMethod, APath, GetTickCount64 - Started,
        Format('sent %d bytes, got %d bytes', [Length(ABody), Length(Body)]));
    Result := CheckedResponse(Body);
end;

function THttpFitService.ProblemId: longint;
var
    R: TJSONObject;
begin
    if FProblemId <= 0 then
    begin
        R := HttpSend('POST', FBaseUrl + '/problems', '');
        try
            FProblemId := R.Get('id', -1);
        finally
            R.Free;
        end;
        if FProblemId <= 0 then
            raise Exception.Create('The compute server did not create a problem.');
    end;
    Result := FProblemId;
end;

function THttpFitService.IsAvailable: boolean;
var
    C: TFPHTTPClient;
begin
    Result := False;
    C := NewClient(CONNECT_TIMEOUT_MS);
    try
        try
            C.Get(FBaseUrl + '/health');
            Result := True;
        except
            Result := False;
        end;
    finally
        C.Free;
    end;
end;

{ ---------------- point sets ---------------- }

function THttpFitService.GetPoints(const APath: string): TTitlePointsSet;
var
    Ids: TCurveInstanceIdList;
begin
    Result := GetPoints(APath, Ids);
end;

{ The same read, also reporting the handles the reply carried.

  TWO ENTRY POINTS RATHER THAN A POINT SET THAT CARRIES HANDLES: TPointsSet is
  the engine's own type, and a handle is not a property of a point - it belongs
  to the CURVE the point seeds. Widening the point set to hold one would put an
  identity on every profile sample in the program. }
function THttpFitService.GetPoints(const APath: string;
    out AIds: TCurveInstanceIdList): TTitlePointsSet;
var
    P: TPointsData;
    i: integer;
begin
    Result := nil;
    AIds := nil;
    if not PointsFromJsonString(Fetch(Url(APath), REPLY_TIMEOUT_MS), P) then
        Exit;
    Result := TTitlePointsSet.Create(nil);
    Result.FTitle := P.Title;
    for i := 0 to High(P.X) do
        Result.AddNewPoint(P.X[i], P.Y[i]);
    SetLength(AIds, Length(P.Ids));
    for i := 0 to High(P.Ids) do
        AIds[i] := P.Ids[i];
end;

{ Whether the curve at ACurveIndex carries values an optimiser produced.

  Read from the curves reply, which is where the flag crosses - the same reply
  the handles and the parameters come from, so one request answers for the whole
  model rather than one per curve. }
function THttpFitService.IsCurveFitted(ACurveIndex: longint): boolean;
var
    R: TJSONObject;
    Curves: TJSONArray;
begin
    Result := False;
    R := HttpGet(Url('/curves'));
    try
        if not (R.Find('curves') is TJSONArray) then
            Exit;
        Curves := TJSONArray(R.Find('curves'));
        if (ACurveIndex < 0) or (ACurveIndex >= Curves.Count) then
            Exit;
        Result := TJSONObject(Curves.Items[ACurveIndex]).Get('fitted', False);
    finally
        R.Free;
    end;
end;

function THttpFitService.GetModuleProjectStates: TModuleStateArray;
var
    R: TJSONObject;
    Arr: TJSONArray;
    i: longint;
begin
    Result := nil;
    R := HttpGet(Url('/module-states'));
    try
        if not (R.Find('states') is TJSONArray) then
            Exit;
        Arr := TJSONArray(R.Find('states'));
        SetLength(Result, Arr.Count);
        for i := 0 to Arr.Count - 1 do
        begin
            Result[i].Module := TJSONObject(Arr.Items[i]).Get('module', '');
            Result[i].Content := TJSONObject(Arr.Items[i]).Get('content', '');
        end;
    finally
        R.Free;
    end;
end;

function THttpFitService.GetCurvePositionIds: TCurveInstanceIdList;
var
    Picks: TTitlePointsSet;
begin
    //  The handles come back WITH the picks, in one request, because that is
    //  the reply that carries them. Reading them separately would be two
    //  requests that an edit in between could make disagree.
    Picks := GetPoints('/positions', Result);
    Picks.Free;
end;

function THttpFitService.PutPoints(const APath: string;
    APoints: TPointsSet; const AIds: TCurveInstanceIdList): string;
var
    P: TPointsData;
    i: integer;
    R: TJSONObject;
begin
    P := Default(TPointsData);
    if Assigned(APoints) then
    begin
        if APoints is TTitlePointsSet then
            P.Title := TTitlePointsSet(APoints).FTitle;
        SetLength(P.X, APoints.PointsCount);
        SetLength(P.Y, APoints.PointsCount);
        for i := 0 to APoints.PointsCount - 1 do
        begin
            P.X[i] := APoints.PointXCoord[i];
            P.Y[i] := APoints.PointYCoord[i];
        end;
        //  Only when there are any: the codec omits the field entirely for an
        //  empty list, which is what keeps every message this client already
        //  sends byte-identical.
        if Length(AIds) = Length(P.X) then
        begin
            SetLength(P.Ids, Length(AIds));
            for i := 0 to High(AIds) do
                P.Ids[i] := AIds[i];
        end;
    end;
    R := HttpSend('PUT', Url(APath), PointsToJsonString(P));
    try
        Result := R.Get('message', '');
    finally
        R.Free;
    end;
end;

{ The profile and the background stay the caller's: TFitClient keeps plotting
  FExperimentalProfile after handing it over (it is one of the viewer's series),
  and frees FBackgroundPoints itself. Freeing them here left the chart drawing a
  dangling pointer - whatever was allocated next, typically the curve positions.
  The engine copies the profile for the same reason. }
function THttpFitService.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
begin
    Result := PutPoints('/profile', APointsSet);
end;

function THttpFitService.GetProfilePointsSet: TTitlePointsSet;
begin
    Result := GetPoints('/profile');
end;

function THttpFitService.GetSelectedProfileInterval: TTitlePointsSet;
begin
    Result := GetPoints('/selected-interval');
end;

function THttpFitService.SetBackgroundPointsSet(
    ABackgroundPoints: TTitlePointsSet): string;
begin
    Result := PutPoints('/background', ABackgroundPoints);
end;

function THttpFitService.GetBackgroundPoints: TTitlePointsSet;
begin
    Result := GetPoints('/background');
end;

{ These two do take ownership, as the engine's own setters do (they copy the
  points and free the argument). Callers hand over a set they built for the call. }
function THttpFitService.SetCurvePositions(ACurvePositions: TPointsSet;
    const AIds: TCurveInstanceIdList): string;
begin
    //  THE ONLY SET THAT CARRIES HANDLES. A curve's identity is issued to the
    //  pick it is seeded from, so a pick can be named and a profile sample
    //  cannot - the server refuses ids on any other set BY NAME, and this is
    //  the only caller here that passes any.
    Result := PutPoints('/positions', ACurvePositions, AIds);
    ACurvePositions.Free;
end;

function THttpFitService.GetCurvePositions: TTitlePointsSet;
begin
    Result := GetPoints('/positions');
end;

{ Named after the calc-profile/profile pair, which draws the same distinction:
  what was picked, and what the engine made of it. }
function THttpFitService.GetResultedCurvePositions: TTitlePointsSet;
begin
    Result := GetPoints('/calc-positions');
end;

function THttpFitService.SetRFactorBounds(ARFactorBounds: TPointsSet): string;
begin
    Result := PutPoints('/rfactor-bounds', ARFactorBounds);
    ARFactorBounds.Free;
end;

function THttpFitService.GetRFactorBounds: TTitlePointsSet;
begin
    Result := GetPoints('/rfactor-bounds');
end;

{ ------------------------- module resources -------------------------

  One pair of verbs for everything a module contributes. The policy each
  resource needs is read from its DECLARATION rather than hard-coded per verb:
  a resource that may run as long as a fit gets the long timeout, and one that
  is fetched on every redraw keeps the short one. Encoded on the module and
  applied here, so the two sides cannot disagree about a resource only one of
  them has to get right. }
function THttpFitService.ModuleGet(const AResource: string): string;
var
    Info: TModuleResource;
    Timeout: longint;
begin
    Timeout := REPLY_TIMEOUT_MS;
    if FindModuleResource(AResource, Info) and Info.LongRunning then
        Timeout := LONG_OPERATION_TIMEOUT_MS;
    //  The body IS the resource, not an ok-wrapped envelope - which is how these
    //  payloads already crossed the wire - so this does not go through
    //  CheckedResponse.
    Result := Fetch(Url('/modules/' + AResource), Timeout);
end;

function THttpFitService.ModulePost(const AResource, APayload: string): string;
var
    Info: TModuleResource;
    Timeout: longint;
begin
    Timeout := REPLY_TIMEOUT_MS;
    if FindModuleResource(AResource, Info) and Info.LongRunning then
        //  As long as a fit may take: a parser that searches would otherwise
        //  look like a failure when it is only slow, and on exactly the data it
        //  is most useful for.
        Timeout := LONG_OPERATION_TIMEOUT_MS;
    Result := Send('POST', Url('/modules/' + AResource), APayload, Timeout);
end;


function THttpFitService.GetCalcProfilePointsSet: TTitlePointsSet;
begin
    Result := GetPoints('/calc-profile');
end;

function THttpFitService.GetDeltaProfilePointsSet: TTitlePointsSet;
begin
    Result := GetPoints('/delta-profile');
end;

{ ---------------- individual points ---------------- }

procedure THttpFitService.AddPointToProfile(XValue, YValue: double);
begin
    HttpSend('POST', Url('/points/profile'),
        Format('{"x":%.17g,"y":%.17g}', [XValue, YValue])).Free;
end;

procedure THttpFitService.AddPointToBackground(XValue, YValue: double);
begin
    HttpSend('POST', Url('/points/background'),
        Format('{"x":%.17g,"y":%.17g}', [XValue, YValue])).Free;
end;

procedure THttpFitService.AddPointToRFactorBounds(XValue, YValue: double);
begin
    HttpSend('POST', Url('/points/rfactor-bounds'),
        Format('{"x":%.17g,"y":%.17g}', [XValue, YValue])).Free;
end;

{ A pick into a module's own set. The set name is part of the path exactly as
  the built-in ones are, so a module's picks travel the route that already
  exists rather than a channel of their own. }
procedure THttpFitService.AddPointToSet(const AKind: string;
    XValue, YValue: double);
begin
    HttpSend('POST', Url('/points/' + AKind),
        Format('{"x":%.17g,"y":%.17g}', [XValue, YValue])).Free;
end;

procedure THttpFitService.AddPointToCurvePositions(XValue, YValue: double);
begin
    HttpSend('POST', Url('/points/positions'),
        Format('{"x":%.17g,"y":%.17g}', [XValue, YValue])).Free;
end;

function MovedPoint(PX, PY, X, Y: double): string;
begin
    Result := Format('{"prevX":%.17g,"prevY":%.17g,"x":%.17g,"y":%.17g}',
        [PX, PY, X, Y]);
end;

procedure THttpFitService.ReplacePointInProfile(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    HttpSend('PUT', Url('/points/profile'),
        MovedPoint(PrevXValue, PrevYValue, NewXValue, NewYValue)).Free;
end;

procedure THttpFitService.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    HttpSend('PUT', Url('/points/background'),
        MovedPoint(PrevXValue, PrevYValue, NewXValue, NewYValue)).Free;
end;

procedure THttpFitService.ReplacePointInRFactorBounds(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    HttpSend('PUT', Url('/points/rfactor-bounds'),
        MovedPoint(PrevXValue, PrevYValue, NewXValue, NewYValue)).Free;
end;

procedure THttpFitService.ReplacePointInSet(const AKind: string;
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    HttpSend('PUT', Url('/points/' + AKind),
        MovedPoint(PrevXValue, PrevYValue, NewXValue, NewYValue)).Free;
end;

procedure THttpFitService.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    HttpSend('PUT', Url('/points/positions'),
        MovedPoint(PrevXValue, PrevYValue, NewXValue, NewYValue)).Free;
end;

{ ---------------- settings ---------------- }

function THttpFitService.Settings: TJSONObject;
begin
    Result := HttpGet(Url('/settings'));
end;

procedure THttpFitService.PutSetting(const AName: string; AValue: TJSONData);
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add(AName, AValue);   //  takes ownership of AValue
        HttpSend('PUT', Url('/settings'), O.AsJSON).Free;
    finally
        O.Free;
    end;
end;

function THttpFitService.GetMaxRFactor: double;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('maxRFactor', 0.0); finally R.Free; end;
end;

procedure THttpFitService.SetMaxRFactor(AMaxRFactor: double);
begin
    PutSetting('maxRFactor', TJSONFloatNumber.Create(AMaxRFactor));
end;

function THttpFitService.GetBackFactor: double;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('backFactor', 0.0); finally R.Free; end;
end;

procedure THttpFitService.SetBackFactor(ABackFactor: double);
begin
    PutSetting('backFactor', TJSONFloatNumber.Create(ABackFactor));
end;

function THttpFitService.GetCurveThresh: double;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('curveThresh', 0.0); finally R.Free; end;
end;

procedure THttpFitService.SetCurveThresh(ACurveThresh: double);
begin
    PutSetting('curveThresh', TJSONFloatNumber.Create(ACurveThresh));
end;

function THttpFitService.GetWaveLength: double;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('waveLength', 0.0); finally R.Free; end;
end;

procedure THttpFitService.SetWaveLength(AWaveLength: double);
begin
    PutSetting('waveLength', TJSONFloatNumber.Create(AWaveLength));
end;

function THttpFitService.GetBackgroundVariationEnabled: boolean;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('backgroundVariation', False); finally R.Free; end;
end;

procedure THttpFitService.SetBackgroundVariationEnabled(AEnable: boolean);
begin
    PutSetting('backgroundVariation', TJSONBoolean.Create(AEnable));
end;

function THttpFitService.GetCurveScalingEnabled: boolean;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('curveScaling', False); finally R.Free; end;
end;

procedure THttpFitService.SetCurveScalingEnabled(AEnabled: boolean);
begin
    PutSetting('curveScaling', TJSONBoolean.Create(AEnabled));
end;

function THttpFitService.GetMinimizerKind: longint;
var R: TJSONObject;
begin
    R := Settings;
    try Result := R.Get('minimizerKind', 0); finally R.Free; end;
end;

procedure THttpFitService.SetMinimizerKind(AKind: longint);
begin
    PutSetting('minimizerKind', TJSONIntegerNumber.Create(AKind));
end;

function THttpFitService.GetLossKind: longint;
var R: TJSONObject;
begin
    R := Settings;
    //  Defaults to the legacy R-factor, matching the server: a client talking to
    //  an older server that has no such setting must not appear to select
    //  something else.
    try Result := R.Get('lossKind', 0); finally R.Free; end;
end;

procedure THttpFitService.SetLossKind(AKind: longint);
begin
    PutSetting('lossKind', TJSONIntegerNumber.Create(AKind));
end;

function THttpFitService.GetWeighting: string;
var R: TJSONObject;
begin
    R := Settings;
    //  The default a server too old to carry the field implies.
    try Result := R.Get('weighting', WEIGHTING_POISSON); finally R.Free; end;
end;

procedure THttpFitService.SetWeighting(const AValue: string);
begin
    PutSetting('weighting', TJSONString.Create(AValue));
end;

function THttpFitService.GetCurveType: TCurveTypeId;
var R: TJSONObject; S: string;
begin
    R := Settings;
    try
        S := R.Get('curveType', '');
    finally
        R.Free;
    end;
    if S = '' then
        Result := Default(TCurveTypeId)
    else
        Result := StringToGUID(S);
end;

procedure THttpFitService.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
    PutSetting('curveType', TJSONString.Create(GUIDToString(ACurveTypeId)));
end;

{ The URL this service talks to (the client's own configuration). }
function THttpFitService.GetServerUrl: string;
begin
    Result := FBaseUrl;
end;

procedure THttpFitService.SetServerUrl(const AUrl: string);
begin
    //  Setting the same server again must change nothing: the form applies the
    //  configured URL during start-up, and discarding the problem there would
    //  strand whatever had already been sent to it (the loaded profile).
    if AUrl = FBaseUrl then
        Exit;
    //  Changing the server means abandoning the problem held on the old one.
    FBaseUrl := AUrl;
    FProblemId := -1;
end;

{ ---------------- state / status ---------------- }

function THttpFitService.GetState: TFitServerState;
var R: TJSONObject;
begin
    R := HttpGet(Url('/state'));
    try
        Result := TFitServerState(R.Get('state', 0));
    finally
        R.Free;
    end;
end;

function THttpFitService.AsyncOper: boolean;
var R: TJSONObject;
begin
    R := HttpGet(Url('/async'));
    try Result := R.Get('busy', False); finally R.Free; end;
end;

function THttpFitService.GetCalcTimeStr: string;
var R: TJSONObject;
begin
    R := HttpGet(Url('/stats'));
    try Result := R.Get('calcTime', ''); finally R.Free; end;
end;

function THttpFitService.GetRFactorStr: string;
var R: TJSONObject;
begin
    R := HttpGet(Url('/stats'));
    try Result := R.Get('rFactor', ''); finally R.Free; end;
end;

function THttpFitService.GetAbsRFactorStr: string;
var R: TJSONObject;
begin
    R := HttpGet(Url('/stats'));
    try Result := R.Get('absRFactor', ''); finally R.Free; end;
end;

function THttpFitService.GetSqrRFactorStr: string;
var R: TJSONObject;
begin
    R := HttpGet(Url('/stats'));
    try Result := R.Get('sqrRFactor', ''); finally R.Free; end;
end;

function THttpFitService.GetStatistics: TFitStatistics;
var
    R, S: TJSONObject;
    D: TJSONData;
begin
    Result := EmptyFitStatistics;
    R := HttpGet(Url('/stats'));
    try
        D := R.Find('statistics');
        if not (D is TJSONObject) then
            Exit;
        S := TJSONObject(D);
        Result.Valid := S.Get('valid', False);
        Result.DataPoints := S.Get('dataPoints', 0);
        Result.Params := S.Get('params', 0);
        Result.DegreesOfFreedom := S.Get('degreesOfFreedom', 0);
        Result.ChiSquare := S.Get('chiSquare', 0.0);
        Result.ReducedChiSquare := S.Get('reducedChiSquare', 0.0);
        Result.RSquared := S.Get('rSquared', 0.0);
        Result.AIC := S.Get('aic', 0.0);
        Result.BIC := S.Get('bic', 0.0);
    finally
        R.Free;
    end;
end;

{ ---------------- curves ---------------- }

{ The curves resource, fetched once per call. }
function THttpFitService.GetCurveCount: longint;
var R: TJSONObject;
begin
    R := HttpGet(Url('/curves'));
    try
        Result := R.Arrays['curves'].Count;
    finally
        R.Free;
    end;
end;

{ The handle of the curve at ACurveIndex, read from the model the server last
  reported. The client still addresses curves by index internally - the parameter
  grid is a list - so this is where an index becomes the handle the wire wants. }
function THttpFitService.GetCurveInstanceId(ACurveIndex: longint): string;
var R: TJSONObject; A: TJSONArray;
begin
    Result := '';
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        if (ACurveIndex >= 0) and (ACurveIndex < A.Count) then
            Result := TJSONObject(A.Items[ACurveIndex]).Get('id', '');
    finally
        R.Free;
    end;
end;

function THttpFitService.IndexOfCurveInstance(
    const AInstanceId: string): longint;
var R: TJSONObject; A: TJSONArray; i: longint;
begin
    Result := -1;
    if AInstanceId = '' then
        Exit;
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(TJSONObject(A.Items[i]).Get('id', ''), AInstanceId) then
            begin
                Result := i;
                Exit;
            end;
    finally
        R.Free;
    end;
end;

function THttpFitService.GetCurveParameterCount(ACurveIndex: longint): longint;
var R: TJSONObject; A: TJSONArray;
begin
    Result := 0;
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        if (ACurveIndex >= 0) and (ACurveIndex < A.Count) then
            Result := TJSONObject(A.Items[ACurveIndex]).Arrays['params'].Count;
    finally
        R.Free;
    end;
end;

function THttpFitService.GetCurveParameterValue(ACurveIndex: longint;
    ParamIndex: longint): Variant;
var R: TJSONObject; A, P: TJSONArray; O: TJSONObject;
begin
    Result := Null;
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        if (ACurveIndex < 0) or (ACurveIndex >= A.Count) then
            Exit;
        P := TJSONObject(A.Items[ACurveIndex]).Arrays['params'];
        if (ParamIndex < 0) or (ParamIndex >= P.Count) then
            Exit;
        O := TJSONObject(P.Items[ParamIndex]);
        if O.Get('kind', '') = 'text' then
            Result := O.Get('value', '')
        else
            Result := O.Get('value', 0.0);
    finally
        R.Free;
    end;
end;

procedure THttpFitService.GetCurveParameter(ACurveIndex: longint;
    ParamIndex: longint; var Name: string; var Value: double; var Type_: longint);
var R: TJSONObject; A, P: TJSONArray; O: TJSONObject;
begin
    Name := '';
    Value := 0;
    Type_ := 0;
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        if (ACurveIndex < 0) or (ACurveIndex >= A.Count) then
            Exit;
        P := TJSONObject(A.Items[ACurveIndex]).Arrays['params'];
        if (ParamIndex < 0) or (ParamIndex >= P.Count) then
            Exit;
        O := TJSONObject(P.Items[ParamIndex]);
        Name  := O.Get('name', '');
        Value := O.Get('value', 0.0);
        Type_ := O.Get('type', 0);
    finally
        R.Free;
    end;
end;

function THttpFitService.GetCurveParameterError(ACurveIndex: longint;
    ParamIndex: longint): double;
var R: TJSONObject; A, P: TJSONArray;
begin
    Result := -1;
    R := HttpGet(Url('/curves'));
    try
        A := R.Arrays['curves'];
        if (ACurveIndex < 0) or (ACurveIndex >= A.Count) then
            Exit;
        P := TJSONObject(A.Items[ACurveIndex]).Arrays['params'];
        if (ParamIndex >= 0) and (ParamIndex < P.Count) then
            Result := TJSONObject(P.Items[ParamIndex]).Get('error', -1.0);
    finally
        R.Free;
    end;
end;

procedure THttpFitService.SetCurveParameter(ACurveIndex: longint;
    ParamIndex: longint; Value: double);
var
    Id: string;
begin
    //  BY HANDLE, not by the index the caller holds. The order of the model is
    //  derived - it follows the intervals and the picks inside them - so an
    //  index the client is still holding after an edit names a different curve.
    //  Resolving here means a stale reference is refused with a 404 instead of
    //  writing a value into whichever curve happens to sit there now.
    Id := GetCurveInstanceId(ACurveIndex);
    if Id = '' then
        raise EUserException.CreateFmt(
            'There is no curve %d in the model any more, so its parameter ' +
            'was not changed.', [ACurveIndex]);
    HttpSend('PUT',
        Url(Format('/curves/%s/params/%d', [Id, ParamIndex])),
        Format('{"value":%.17g}', [Value])).Free;
end;

{ The whole model's fitted values in one request, addressed by handle.

  BY HANDLE for the reason SetCurveParameter is, and in ONE call for a reason of
  its own: the per-parameter route rebuilds the model on every write, so a
  ten-curve restore would be fifty requests and fifty rebuilds - and none of
  them could say that an optimiser, rather than a seed, is where the numbers
  came from.

  Values are written at 17 significant digits. A pick's abscissa and the value
  stored against its curve are compared elsewhere with tolerances as tight as
  1e-9, so a value that loses digits here comes back as a different one. }
function THttpFitService.SetCurveValues(const AEntries: TCurveValuesList): string;
var
    Body: TJSONObject;
    Curves, Params: TJSONArray;
    CurveObj, ParamObj, R: TJSONObject;
    i, j: longint;
    Id: string;
begin
    Body := TJSONObject.Create;
    try
        Curves := TJSONArray.Create;
        Body.Add('curves', Curves);
        for i := 0 to High(AEntries) do
        begin
            Id := GetCurveInstanceId(AEntries[i].CurveIndex);
            if Id = '' then
                raise EUserException.CreateFmt(
                    'There is no curve %d in the model any more, so its ' +
                    'values were not restored.', [AEntries[i].CurveIndex]);
            CurveObj := TJSONObject.Create;
            CurveObj.Add('id', Id);
            CurveObj.Add('fitted', AEntries[i].Fitted);
            Params := TJSONArray.Create;
            for j := 0 to High(AEntries[i].Params) do
            begin
                ParamObj := TJSONObject.Create;
                ParamObj.Add('name', AEntries[i].Params[j].Name);
                ParamObj.Add('value', AEntries[i].Params[j].Value);
                ParamObj.Add('error', AEntries[i].Params[j].Error);
                Params.Add(ParamObj);
            end;
            CurveObj.Add('params', Params);
            Curves.Add(CurveObj);
        end;
        R := HttpSend('PUT', Url('/curves'), Body.AsJSON);
        try
            Result := R.Get('message', '');
        finally
            R.Free;
        end;
    finally
        Body.Free;
    end;
end;

{ Removes one curve over the wire, by the handle its index resolves to.

  BY HANDLE for the reason SetCurveParameter above is: the model's order is
  derived, so an index the client still holds after an edit names a different
  curve - and deleting the wrong one is worse than refusing. }
function THttpFitService.DeleteCurve(ACurveIndex: longint): string;
var
    Id: string;
begin
    Id := GetCurveInstanceId(ACurveIndex);
    if Id = '' then
        raise EUserException.CreateFmt(
            'There is no curve %d in the model any more, so nothing was ' +
            'removed.', [ACurveIndex]);
    HttpSend('DELETE', Url(Format('/points/positions/%s', [Id])), '').Free;
    Result := '';
end;

{ Rebuilds the fitted curves as point sets the viewer can plot - the same thing
  the retired fit_client_proxy did with CreateNamedPointsSet + SetCurveTypeName. }
function THttpFitService.GetCurves: TSelfCopiedCompList;
var
    i, j: longint;
    Curve: TNamedPointsSet;
    P: TPointsData;
    C: TFPHTTPClient;
    R: TJSONObject;
    A: TJSONArray;
    Id: string;
begin
    Result := TSelfCopiedCompList.Create;
    try
        //  ONE read of the model, then one request per curve BY HANDLE. Asking
        //  for /curves/{i}/points by number needed the count and the points to
        //  agree about the order across several requests; a handle does not
        //  care what the order is, and says so when a curve has gone.
        R := HttpGet(Url('/curves'));
        try
            A := R.Arrays['curves'];
            //  Through Fetch like everything else: one client per request costs
            //  nothing measurable beside a round trip, and keeping the transport
            //  in one place is what makes this loop testable at all.
            begin
                for i := 0 to A.Count - 1 do
                begin
                    Id := TJSONObject(A.Items[i]).Get('id', '');
                    //  REPORTED, not skipped. A curve with no handle cannot be
                    //  addressed, so quietly dropping it would leave the chart
                    //  short of curves with nothing to say why - and the model
                    //  would look wrong rather than broken.
                    if Id = '' then
                        raise EUserException.Create(
                            'The compute server sent a curve with no ' +
                            'identifier, so the model could not be read.' +
                            LineEnding +
                            'The server is probably an older version than ' +
                            'this client.');
                    if not PointsFromJsonString(
                        Fetch(Url(Format('/curves/%s/points', [Id])),
                            REPLY_TIMEOUT_MS), P) then
                        Continue;
                    Curve := TNamedPointsSet.Create(nil);
                    Result.Add(Curve);
                    //  Carried, so the view can tell one instance from another
                    //  and address it back. The points alone cannot: two curves
                    //  of one type differ only in where they sit.
                    TryStrToCurveInstanceId(Id, Curve.FInstanceId);
                    Curve.SetCurveTypeName(P.Title);
                    Curve.FTitle := P.Title;
                    for j := 0 to High(P.X) do
                        Curve.AddNewPoint(P.X[j], P.Y[j]);
                end;
            end;
        finally
            R.Free;
        end;
    except
        Result.Free;
        Result := nil;
        raise;
    end;
end;

{ The parameter grid's data: one Curve_parameters per curve. }
function THttpFitService.GetCurveAttributes: TMSCRCurveList;
var
    R: TJSONObject;
    Curves, Params: TJSONArray;
    O: TJSONObject;
    CP: Curve_parameters;
    Container: TPersistentCurveParameterContainer;
    i, j: longint;
begin
    Result := TMSCRCurveList.Create;
    R := HttpGet(Url('/curves'));
    try
        Curves := R.Arrays['curves'];
        for i := 0 to Curves.Count - 1 do
        begin
            //  Build the item, then hand it to the list (Add takes the object and
            //  returns an index - it does not create one).
            CP := Curve_parameters.Create(nil);
            //  Curve_parameters is created with one placeholder parameter.
            CP.Params.Clear;
            Result.Add(CP);
            //  WHICH INSTANCE these parameters belong to, carried so the grid
            //  and the chart are talking about the same curve.
            TryStrToCurveInstanceId(
                TJSONObject(Curves.Items[i]).Get('id', ''), CP.FInstanceId);
            Params := TJSONObject(Curves.Items[i]).Arrays['params'];
            for j := 0 to Params.Count - 1 do
            begin
                O := TJSONObject(Params.Items[j]);
                Container := TPersistentCurveParameterContainer(CP.Params.Add);
                Container.Parameter.Name  := O.Get('name', '');
                Container.Parameter.Type_ := TParameterType(O.Get('type', 0));
                Container.Parameter.Error := O.Get('error', -1.0);
                //  Declared, not guessed: `kind` says what `value` is, so a
                //  label like "3" cannot be mistaken for the number 3.
                if O.Get('kind', '') = 'text' then
                    Container.Parameter.TypedValue := O.Get('value', '')
                else
                    Container.Parameter.TypedValue := O.Get('value', 0.0);
            end;
        end;
    finally
        R.Free;
    end;
end;

procedure THttpFitService.CreateCurveList;
begin
    RunAction('create-curve-list');
end;

{ ---------------- user-defined curve parameters ---------------- }

function THttpFitService.GetSpecialCurveParameters: Curve_parameters;
var
    R: TJSONObject;
    Params: TJSONArray;
    O: TJSONObject;
    Container: TPersistentCurveParameterContainer;
    j: longint;
begin
    Result := Curve_parameters.Create(nil);
    Result.Params.Clear;
    R := HttpGet(Url('/special-params'));
    try
        Params := R.Arrays['params'];
        for j := 0 to Params.Count - 1 do
        begin
            O := TJSONObject(Params.Items[j]);
            Container := TPersistentCurveParameterContainer(Result.Params.Add);
            Container.Parameter.Name  := O.Get('name', '');
            Container.Parameter.Value := O.Get('value', 0.0);
            Container.Parameter.Type_ := TParameterType(O.Get('type', 0));
        end;
    finally
        R.Free;
    end;
end;

procedure THttpFitService.ClearSpecialCurve;
begin
    HttpSend('DELETE', Url('/special-params'), '').Free;
end;

procedure THttpFitService.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters);
var
    Root: TJSONObject;
    Arr: TJSONArray;
    O: TJSONObject;
    i: longint;
begin
    Root := TJSONObject.Create;
    try
        Root.Add('expression', ACurveExpr);
        if Assigned(CP) then
        begin
            Arr := TJSONArray.Create;
            for i := 0 to CP.Count - 1 do
            begin
                O := TJSONObject.Create;
                O.Add('name', CP[i].Name);
                O.Add('value', CP[i].Value);
                O.Add('type', longint(CP[i].Type_));
                Arr.Add(O);
            end;
            Root.Add('params', Arr);
        end;
        HttpSend('PUT', Url('/special-params'), Root.AsJSON).Free;
    finally
        Root.Free;
        //  The contract hands ownership of CP to the service.
        CP.Free;
    end;
end;

{ ---------------- actions ---------------- }

function THttpFitService.RunAction(const AName: string;
    const ABody: string = ''): string;
var
    R: TJSONObject;
begin
    R := HttpSend('POST', Url('/actions/' + AName), ABody, NO_TIMEOUT);
    try
        Result := R.Get('message', '');
    finally
        R.Free;
    end;
end;

function THttpFitService.SmoothProfile: string;
begin
    Result := RunAction('smooth-profile');
end;

procedure THttpFitService.SubtractBackground(Auto: boolean);
begin
    if Auto then
        RunAction('subtract-background', '{"auto":true}')
    else
        RunAction('subtract-background', '{"auto":false}');
end;

function THttpFitService.DoAllAutomatically: string;
begin
    Result := RunAction('do-all-automatically');
end;

function THttpFitService.MinimizeDifference: string;
begin
    Result := RunAction('minimize-difference');
end;

function THttpFitService.MinimizeDifferenceAgain: string;
begin
    Result := RunAction('minimize-difference-again');
end;

function THttpFitService.MinimizeNumberOfCurves: string;
begin
    Result := RunAction('minimize-number-of-curves');
end;

function THttpFitService.ComputeCurveBounds: string;
begin
    Result := RunAction('compute-curve-bounds');
end;

function THttpFitService.ComputeBackgroundPoints: string;
begin
    Result := RunAction('compute-background-points');
end;

function THttpFitService.ComputeCurvePositions: string;
begin
    Result := RunAction('compute-curve-positions');
end;

function THttpFitService.SelectAllPointsAsCurvePositions: string;
begin
    Result := RunAction('select-all-points-as-curve-positions');
end;

procedure THttpFitService.StopAsyncOper;
begin
    RunAction('stop');
end;

function THttpFitService.SelectProfileInterval(
    StartPointIndex, StopPointIndex: longint): string;
begin
    Result := RunAction('select-profile-interval',
        Format('{"start":%d,"stop":%d}', [StartPointIndex, StopPointIndex]));
end;

function THttpFitService.SelectEntireProfile: string;
begin
    Result := RunAction('select-entire-profile');
end;

end.
