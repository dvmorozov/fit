{
This unit has been produced by ws_helper.
  Input unit name : "fit_server".
  This unit name  : "fit_server_proxy".
  Date            : "12.01.2009 11:43:17".
}

unit fit_server_proxy;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses
    SysUtils, Classes, TypInfo, int_points_set, base_service_intf,
    service_intf, int_fit_server;

type


    TFitServer_Proxy = class(TBaseProxy, IFitServer)
    protected
        class function GetServiceType(): PTypeInfo; override;
        function SmoothProfile(const ProblemID: integer): TResult;
        function SubtractBackground(const Auto: boolean;
            const ProblemID: integer): TResult;
        function DoAllAutomatically(const ProblemID: integer): TResult;
        function MinimizeDifference(const ProblemID: integer): TResult;
        function MinimizeNumberOfCurves(const ProblemID: integer): TResult;
        function ComputeCurveBounds(const ProblemID: integer): TResult;
        function ComputeCurvePositions(const ProblemID: integer): TResult;
        function ComputeBackgroundPoints(const ProblemID: integer): TResult;
        function StopAsyncOper(const ProblemID: integer): TResult;
        function AsyncOper(const ProblemID: integer): TBoolResult;
        function SelectArea(const StartPointIndex: integer;
            const StopPointIndex: integer; const ProblemID: integer): TResult;
        function ReturnToTotalProfile(const ProblemID: integer): TResult;
        function CreateCurveList(const ProblemID: integer): TResult;
        function SetProfilePointsSet(const PointsSet:
            TArrayOfFloatDoubleRemotable; const ProblemID: integer): TResult;
        function SetBackgroundPointsSet(
            const BackgroundPoints: TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
        function SetCurvePositions(
            const CurvePositions: TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
        function SetCurveBounds(
            const CurveBounds: TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
        function AddPointToBackground(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
        function AddPointToCurveBounds(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
        function AddPointToCurvePositions(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
        function GetProfilePointsSet(const ProblemID: integer): TPointsResult;
        function GetSelectedArea(const ProblemID: integer): TPointsResult;
        function GetBackgroundPoints(const ProblemID: integer): TPointsResult;
        function GetCurvePositions(const ProblemID: integer): TPointsResult;
        function GetCurveBounds(const ProblemID: integer): TPointsResult;
        function GetCalcProfilePointsSet(
            const ProblemID: integer): TPointsResult;
        function GetDeltaProfilePointsSet(const ProblemID: integer):
            TPointsResult;
        procedure SetCurveThresh(const CurveThresh: double;
            const ProblemID: integer);
        function GetMaxRFactor(const ProblemID: integer): double;
        procedure SetMaxRFactor(const MaxRFactor: double;
            const ProblemID: integer);
        function GetBackFactor(const ProblemID: integer): double;
        procedure SetBackFactor(const BackFactor: double;
            const ProblemID: integer);
        function GetCurveType(const ProblemID: integer): TCurveTypeId;
        procedure SetCurveType(const CurveTypeId: TCurveTypeId;
            const ProblemID: integer);
        function GetWaveLength(const ProblemID: integer): double;
        procedure SetWaveLength(const WaveLength: double;
            const ProblemID: integer);
        function GetCurveThresh(const ProblemID: integer): double;
        function GetState(const ProblemID: integer): integer;
        function ReplacePointInProfile(const PrevXValue: double;
            const PrevYValue: double; const NewXValue: double;
            const NewYValue: double; const ProblemID: integer): TResult;
        function ReplacePointInBackground(const PrevXValue: double;
            const PrevYValue: double; const NewXValue: double;
            const NewYValue: double; const ProblemID: integer): TResult;
        function ReplacePointInCurveBounds(const PrevXValue: double;
            const PrevYValue: double; const NewXValue: double;
            const NewYValue: double; const ProblemID: integer): TResult;
        function ReplacePointInCurvePositions(const PrevXValue: double;
            const PrevYValue: double; const NewXValue: double;
            const NewYValue: double; const ProblemID: integer): TResult;
        function CreateProblem(): integer;
        procedure DiscardProblem(const ProblemID: integer);
        function GetCurveCount(const ProblemID: integer): TIntResult;
        function GetCurvePoints(const SpecIndex: integer;
            const ProblemID: integer): TNamedPointsResult;
        function GetCurveParameterCount(const ProblemID: integer;
            const SpecIndex: integer): TIntResult;
        function GetCurveParameter(const ProblemID: integer;
            const SpecIndex: integer;
            const ParamIndex: integer): TSpecParamResult;
        function AddPointToData(const XValue: double; const YValue: double;
            const ProblemID: integer): TResult;
        function GetGraph(const Width: integer; const Height: integer;
            const ProblemID: integer): TPictureResult;
        function GetProfileChunk(const ProblemID: integer;
            const ChunkNum: integer): TPointsResult;
        function GetProfileChunkCount(const ProblemID: integer): TIntResult;
        function SetCurveParameter(const ProblemID: integer;
            const SpecIndex: integer; const ParamIndex: integer;
            const Value: double): TResult;
        function GetCalcTimeStr(const ProblemID: integer): TStringResult;
        function GetRFactorStr(const ProblemID: integer): TStringResult;
        function GetAbsRFactorStr(const ProblemID: integer): TStringResult;
        function GetSqrRFactorStr(const ProblemID: integer): TStringResult;
    end;

function wst_CreateInstance_IFitServer(const AFormat: string = 'SOAP:';
    const ATransport: string = 'HTTP:'): IFitServer;

implementation

uses wst_resources_imp, metadata_repository;

function wst_CreateInstance_IFitServer(const AFormat: string;
    const ATransport: string): IFitServer;
begin
    Result := TFitServer_Proxy.Create('IFitServer', AFormat +
        GetServiceDefaultFormatProperties(TypeInfo(IFitServer)),
        ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(IFitServer)));
end;

{ TFitServer_Proxy implementation }

class function TFitServer_Proxy.GetServiceType(): PTypeInfo;
begin
    Result := TypeInfo(IFitServer);
end;

function TFitServer_Proxy.SmoothProfile(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SmoothProfile', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SubtractBackground(const Auto: boolean;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SubtractBackground', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('Auto', TypeInfo(boolean), Auto);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.DoAllAutomatically(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('DoAllAutomatically', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.MinimizeDifference(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('MinimizeDifference', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.MinimizeNumberOfCurves(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('MinimizeNumberOfCurves', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ComputeCurveBounds(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ComputeCurveBounds', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ComputeCurvePositions(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ComputeCurvePositions', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ComputeBackgroundPoints(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ComputeBackgroundPoints', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.StopAsyncOper(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('StopAsyncOper', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.AsyncOper(const ProblemID: integer): TBoolResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('AsyncOper', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TBoolResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SelectArea(const StartPointIndex: integer;
    const StopPointIndex: integer; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SelectArea', GetTarget(), (Self as ICallContext));
        locSerializer.Put('StartPointIndex', TypeInfo(integer), StartPointIndex);
        locSerializer.Put('StopPointIndex', TypeInfo(integer), StopPointIndex);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ReturnToTotalProfile(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ReturnToTotalProfile', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.CreateCurveList(const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('CreateCurveList', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SetProfilePointsSet(
    const PointsSet: TArrayOfFloatDoubleRemotable; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetProfilePointsSet', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('PointsSet', TypeInfo(TArrayOfFloatDoubleRemotable),
            PointsSet);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SetBackgroundPointsSet(
    const BackgroundPoints: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetBackgroundPointsSet', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('BackgroundPoints', TypeInfo(TArrayOfFloatDoubleRemotable),
            BackgroundPoints);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SetCurvePositions(
    const CurvePositions: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetCurvePositions', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('CurvePositions', TypeInfo(TArrayOfFloatDoubleRemotable),
            CurvePositions);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SetCurveBounds(
    const CurveBounds: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetCurveBounds', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('CurveBounds', TypeInfo(TArrayOfFloatDoubleRemotable),
            CurveBounds);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.AddPointToBackground(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('AddPointToBackground', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('XValue', TypeInfo(double), XValue);
        locSerializer.Put('YValue', TypeInfo(double), YValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.AddPointToCurveBounds(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('AddPointToCurveBounds', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('XValue', TypeInfo(double), XValue);
        locSerializer.Put('YValue', TypeInfo(double), YValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.AddPointToCurvePositions(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('AddPointToCurvePositions', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('XValue', TypeInfo(double), XValue);
        locSerializer.Put('YValue', TypeInfo(double), YValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetProfilePointsSet(const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetProfilePointsSet', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetSelectedArea(const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetSelectedArea', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetBackgroundPoints(const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetBackgroundPoints', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurvePositions(const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurvePositions', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveBounds(const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveBounds', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCalcProfilePointsSet(
    const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCalcProfilePointsSet', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetDeltaProfilePointsSet(
    const ProblemID: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetDeltaProfilePointsSet', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.SetCurveThresh(const CurveThresh: double;
    const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetCurveThresh', GetTarget(), (Self as ICallContext));
        locSerializer.Put('CurveThresh', TypeInfo(double), CurveThresh);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetMaxRFactor(const ProblemID: integer): double;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetMaxRFactor', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(double), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.SetMaxRFactor(const MaxRFactor: double;
    const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetMaxRFactor', GetTarget(), (Self as ICallContext));
        locSerializer.Put('MaxRFactor', TypeInfo(double), MaxRFactor);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetBackFactor(const ProblemID: integer): double;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetBackFactor', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(double), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.SetBackFactor(const BackFactor: double;
    const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetBackFactor', GetTarget(), (Self as ICallContext));
        locSerializer.Put('BackFactor', TypeInfo(double), BackFactor);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveType(const ProblemID: integer): TCurveTypeId;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveType', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(integer), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.SetCurveType(const CurveTypeId: TCurveTypeId;
    const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetCurveType', GetTarget(), (Self as ICallContext));
        locSerializer.Put('CurveTypeId', TypeInfo(integer), CurveTypeId);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetWaveLength(const ProblemID: integer): double;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetWaveLength', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(double), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.SetWaveLength(const WaveLength: double;
    const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetWaveLength', GetTarget(), (Self as ICallContext));
        locSerializer.Put('WaveLength', TypeInfo(double), WaveLength);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveThresh(const ProblemID: integer): double;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveThresh', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(double), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetState(const ProblemID: integer): integer;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetState', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(integer), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ReplacePointInProfile(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double; const NewYValue: double;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ReplacePointInProfile', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('PrevXValue', TypeInfo(double), PrevXValue);
        locSerializer.Put('PrevYValue', TypeInfo(double), PrevYValue);
        locSerializer.Put('NewXValue', TypeInfo(double), NewXValue);
        locSerializer.Put('NewYValue', TypeInfo(double), NewYValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ReplacePointInBackground(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double; const NewYValue: double;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ReplacePointInBackground', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('PrevXValue', TypeInfo(double), PrevXValue);
        locSerializer.Put('PrevYValue', TypeInfo(double), PrevYValue);
        locSerializer.Put('NewXValue', TypeInfo(double), NewXValue);
        locSerializer.Put('NewYValue', TypeInfo(double), NewYValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ReplacePointInCurveBounds(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double; const NewYValue: double;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ReplacePointInCurveBounds', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('PrevXValue', TypeInfo(double), PrevXValue);
        locSerializer.Put('PrevYValue', TypeInfo(double), PrevYValue);
        locSerializer.Put('NewXValue', TypeInfo(double), NewXValue);
        locSerializer.Put('NewYValue', TypeInfo(double), NewYValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.ReplacePointInCurvePositions(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double; const NewYValue: double;
    const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('ReplacePointInCurvePositions', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('PrevXValue', TypeInfo(double), PrevXValue);
        locSerializer.Put('PrevYValue', TypeInfo(double), PrevYValue);
        locSerializer.Put('NewXValue', TypeInfo(double), NewXValue);
        locSerializer.Put('NewYValue', TypeInfo(double), NewYValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.CreateProblem(): integer;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('CreateProblem', GetTarget(), (Self as ICallContext));
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        strPrmName := 'result';
        locSerializer.Get(TypeInfo(integer), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

procedure TFitServer_Proxy.DiscardProblem(const ProblemID: integer);
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('DiscardProblem', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveCount(const ProblemID: integer): TIntResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveCount', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TIntResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurvePoints(const SpecIndex: integer;
    const ProblemID: integer): TNamedPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurvePoints', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('SpecIndex', TypeInfo(integer), SpecIndex);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TNamedPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveParameterCount(const ProblemID: integer;
    const SpecIndex: integer): TIntResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveParameterCount', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.Put('SpecIndex', TypeInfo(integer), SpecIndex);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TIntResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCurveParameter(const ProblemID: integer;
    const SpecIndex: integer; const ParamIndex: integer): TSpecParamResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCurveParameter', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.Put('SpecIndex', TypeInfo(integer), SpecIndex);
        locSerializer.Put('ParamIndex', TypeInfo(integer), ParamIndex);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TSpecParamResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.AddPointToData(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('AddPointToData', GetTarget(), (Self as ICallContext));
        locSerializer.Put('XValue', TypeInfo(double), XValue);
        locSerializer.Put('YValue', TypeInfo(double), YValue);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetGraph(const Width: integer; const Height: integer;
    const ProblemID: integer): TPictureResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetGraph', GetTarget(), (Self as ICallContext));
        locSerializer.Put('Width', TypeInfo(integer), Width);
        locSerializer.Put('Height', TypeInfo(integer), Height);
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPictureResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetProfileChunk(const ProblemID: integer;
    const ChunkNum: integer): TPointsResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetProfileChunk', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.Put('ChunkNum', TypeInfo(integer), ChunkNum);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TPointsResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetProfileChunkCount(const ProblemID: integer): TIntResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetProfileChunkCount', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TIntResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.SetCurveParameter(const ProblemID: integer;
    const SpecIndex: integer; const ParamIndex: integer;
    const Value: double): TResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('SetCurveParameter', GetTarget(),
            (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.Put('SpecIndex', TypeInfo(integer), SpecIndex);
        locSerializer.Put('ParamIndex', TypeInfo(integer), ParamIndex);
        locSerializer.Put('Value', TypeInfo(double), Value);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetCalcTimeStr(const ProblemID: integer): TStringResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetCalcTimeStr', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TStringResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetRFactorStr(const ProblemID: integer): TStringResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetRFactorStr', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TStringResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetAbsRFactorStr(const ProblemID: integer): TStringResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetAbsRFactorStr', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TStringResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;

function TFitServer_Proxy.GetSqrRFactorStr(const ProblemID: integer): TStringResult;
var
    locSerializer: IFormatterClient;
    strPrmName:    string;
begin
    locSerializer := GetSerializer();
    try
        locSerializer.BeginCall('GetSqrRFactorStr', GetTarget(), (Self as ICallContext));
        locSerializer.Put('ProblemID', TypeInfo(integer), ProblemID);
        locSerializer.EndCall();

        MakeCall();

        locSerializer.BeginCallRead((Self as ICallContext));
        TObject(Result) := nil;
        strPrmName      := 'Result';
        locSerializer.Get(TypeInfo(TStringResult), strPrmName, Result);

    finally
        locSerializer.Clear();
    end;
end;


initialization
  {$i ..\fit_server.wst}

  {$IF DECLARED(Register_fit_server_ServiceMetadata)}
    Register_fit_server_ServiceMetadata();
  {$IFEND}
end.
