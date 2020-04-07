{
This unit has been produced by ws_helper.
  Input unit name : "fit_server".
  This unit name  : "int_fit_server".
  Date            : "12.01.2009 11:43:17".
}
unit int_fit_server;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses
    base_service_intf, Classes, named_points_set, service_intf, SysUtils, TypInfo;

const
    sNAME_SPACE = 'fit_server';
    sUNIT_NAME  = 'fit_server';

type
    TResult     = class;
    TBoolResult = class;
    TPointsResult = class;
    TIntResult  = class;
    TNamedPointsResult = class;
    TSpecParamResult = class;
    TPictureResult = class;
    TStringResult = class;

    TResult = class(TBaseComplexRemotable)
    private
        FErrCode: integer;
        FErrMsg:  string;
    published
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TBoolResult = class(TBaseComplexRemotable)
    private
        F_Result: boolean;
        FErrCode: integer;
        FErrMsg:  string;
    published
        property _Result: boolean read F_Result write F_Result;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TPointsResult = class(TBaseComplexRemotable)
    private
        F_Result: TArrayOfFloatDoubleRemotable;
        FErrCode: integer;
        FErrMsg:  string;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property _Result: TArrayOfFloatDoubleRemotable read F_Result write F_Result;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TIntResult = class(TBaseComplexRemotable)
    private
        F_Result: integer;
        FErrCode: integer;
        FErrMsg:  string;
    published
        property _Result: integer read F_Result write F_Result;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TNamedPointsResult = class(TBaseComplexRemotable)
    private
        F_Result: TArrayOfFloatDoubleRemotable;
        FErrCode: integer;
        FErrMsg:  string;
        FName:    string;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property _Result: TArrayOfFloatDoubleRemotable read F_Result write F_Result;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
        property Name: string read FName write FName;
    end;

    TSpecParamResult = class(TBaseComplexRemotable)
    private
        FName:    string;
        FValue:   double;
        F_Type:   integer;
        FErrCode: integer;
        FErrMsg:  string;
    published
        property Name: string read FName write FName;
        property Value: double read FValue write FValue;
        property _Type: integer read F_Type write F_Type;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TPictureResult = class(TBaseComplexRemotable)
    private
        F_Result: TArrayOfInt8URemotable;
        FErrCode: integer;
        FErrMsg:  string;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property _Result: TArrayOfInt8URemotable read F_Result write F_Result;
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
    end;

    TStringResult = class(TBaseComplexRemotable)
    private
        FErrCode: integer;
        FErrMsg:  string;
        F_Result: string;
    published
        property ErrCode: integer read FErrCode write FErrCode;
        property ErrMsg: string read FErrMsg write FErrMsg;
        property _Result: string read F_Result write F_Result;
    end;

    IFitServer = interface
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
        function SelectProfileInterval(const StartPointIndex: integer;
            const StopPointIndex: integer; const ProblemID: integer): TResult;
        function SelectEntireProfile(const ProblemID: integer): TResult;
        function CreateCurveList(const ProblemID: integer): TResult;
        function SetProfilePointsSet(const PointsSet: TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
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
        function AddPointToRFactorBounds(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
        function AddPointToCurvePositions(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
        function GetProfilePointsSet(const ProblemID: integer): TPointsResult;
        function GetSelectedProfileInterval(const ProblemID: integer): TPointsResult;
        function GetBackgroundPoints(const ProblemID: integer): TPointsResult;
        function GetCurvePositions(const ProblemID: integer): TPointsResult;
        function SetRFactorBounds(const ProblemID: integer): TPointsResult;
        function GetCalcProfilePointsSet(const ProblemID: integer): TPointsResult;
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
        function AddPointToProfile(const XValue: double; const YValue: double;
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

procedure Register_fit_server_ServiceMetadata();

implementation

uses metadata_repository;

{ TPointsResult }

constructor TPointsResult.Create();
begin
    inherited Create();
    F_Result := TArrayOfFloatDoubleRemotable.Create();
end;

destructor TPointsResult.Destroy();
begin
    if Assigned(F_Result) then
        FreeAndNil(F_Result);
    inherited;
end;

{ TNamedPointsResult }

constructor TNamedPointsResult.Create();
begin
    inherited Create();
    F_Result := TArrayOfFloatDoubleRemotable.Create();
end;

destructor TNamedPointsResult.Destroy();
begin
    if Assigned(F_Result) then
        FreeAndNil(F_Result);
    inherited;
end;

{ TPictureResult }

constructor TPictureResult.Create();
begin
    inherited Create();
    F_Result := TArrayOfInt8URemotable.Create();
end;

destructor TPictureResult.Destroy();
begin
    if Assigned(F_Result) then
        FreeAndNil(F_Result);
    inherited;
end;


procedure Register_fit_server_ServiceMetadata();
var
    mm: IModuleMetadataMngr;
begin
    mm := GetModuleMetadataMngr();
    mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
    mm.SetServiceCustomData(
        sUNIT_NAME,
        'IFitServer',
        'FORMAT_Style',
        'rpc'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SmoothProfile',
        '_E_N_',
        'SmoothProfile'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SmoothProfile',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SmoothProfile',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SubtractBackground',
        '_E_N_',
        'SubtractBackground'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SubtractBackground',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SubtractBackground',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DoAllAutomatically',
        '_E_N_',
        'DoAllAutomatically'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DoAllAutomatically',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DoAllAutomatically',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeDifference',
        '_E_N_',
        'MinimizeDifference'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeDifference',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeDifference',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeNumberOfCurves',
        '_E_N_',
        'MinimizeNumberOfCurves'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeNumberOfCurves',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'MinimizeNumberOfCurves',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurveBounds',
        '_E_N_',
        'ComputeCurveBounds'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurveBounds',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurveBounds',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurvePositions',
        '_E_N_',
        'ComputeCurvePositions'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurvePositions',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeCurvePositions',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeBackgroundPoints',
        '_E_N_',
        'ComputeBackgroundPoints'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeBackgroundPoints',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ComputeBackgroundPoints',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'StopAsyncOper',
        '_E_N_',
        'StopAsyncOper'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'StopAsyncOper',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'StopAsyncOper',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AsyncOper',
        '_E_N_',
        'AsyncOper'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AsyncOper',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AsyncOper',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectProfileInterval',
        '_E_N_',
        'SelectProfileInterval'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectProfileInterval',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectProfileInterval',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectEntireProfile',
        '_E_N_',
        'SelectEntireProfile'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectEntireProfile',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SelectEntireProfile',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateCurveList',
        '_E_N_',
        'CreateCurveList'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateCurveList',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateCurveList',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetProfilePointsSet',
        '_E_N_',
        'SetProfilePointsSet'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetProfilePointsSet',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetProfilePointsSet',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackgroundPointsSet',
        '_E_N_',
        'SetBackgroundPointsSet'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackgroundPointsSet',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackgroundPointsSet',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurvePositions',
        '_E_N_',
        'SetCurvePositions'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurvePositions',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurvePositions',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveBounds',
        '_E_N_',
        'SetCurveBounds'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveBounds',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveBounds',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToBackground',
        '_E_N_',
        'AddPointToBackground'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToBackground',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToBackground',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToRFactorBounds',
        '_E_N_',
        'AddPointToRFactorBounds'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToRFactorBounds',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToRFactorBounds',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToCurvePositions',
        '_E_N_',
        'AddPointToCurvePositions'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToCurvePositions',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToCurvePositions',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfilePointsSet',
        '_E_N_',
        'GetProfilePointsSet'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfilePointsSet',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfilePointsSet',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSelectedProfileInterval',
        '_E_N_',
        'GetSelectedProfileInterval'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSelectedProfileInterval',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSelectedProfileInterval',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackgroundPoints',
        '_E_N_',
        'GetBackgroundPoints'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackgroundPoints',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackgroundPoints',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePositions',
        '_E_N_',
        'GetCurvePositions'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePositions',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePositions',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetRFactorBounds',
        '_E_N_',
        'SetRFactorBounds'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetRFactorBounds',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetRFactorBounds',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcProfilePointsSet',
        '_E_N_',
        'GetCalcProfilePointsSet'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcProfilePointsSet',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcProfilePointsSet',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetDeltaProfilePointsSet',
        '_E_N_',
        'GetDeltaProfilePointsSet'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetDeltaProfilePointsSet',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetDeltaProfilePointsSet',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveThresh',
        '_E_N_',
        'SetCurveThresh'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveThresh',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveThresh',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetMaxRFactor',
        '_E_N_',
        'GetMaxRFactor'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetMaxRFactor',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetMaxRFactor',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetMaxRFactor',
        '_E_N_',
        'SetMaxRFactor'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetMaxRFactor',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetMaxRFactor',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackFactor',
        '_E_N_',
        'GetBackFactor'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackFactor',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetBackFactor',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackFactor',
        '_E_N_',
        'SetBackFactor'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackFactor',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetBackFactor',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveType',
        '_E_N_',
        'GetCurveType'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveType',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveType',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveType',
        '_E_N_',
        'SetCurveType'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveType',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveType',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetWaveLength',
        '_E_N_',
        'GetWaveLength'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetWaveLength',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetWaveLength',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetWaveLength',
        '_E_N_',
        'SetWaveLength'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetWaveLength',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetWaveLength',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveThresh',
        '_E_N_',
        'GetCurveThresh'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveThresh',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveThresh',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetState',
        '_E_N_',
        'GetState'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetState',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetState',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInProfile',
        '_E_N_',
        'ReplacePointInProfile'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInProfile',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInProfile',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInBackground',
        '_E_N_',
        'ReplacePointInBackground'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInBackground',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInBackground',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurveBounds',
        '_E_N_',
        'ReplacePointInCurveBounds'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurveBounds',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurveBounds',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurvePositions',
        '_E_N_',
        'ReplacePointInCurvePositions'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurvePositions',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'ReplacePointInCurvePositions',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateProblem',
        '_E_N_',
        'CreateProblem'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateProblem',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'CreateProblem',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DiscardProblem',
        '_E_N_',
        'DiscardProblem'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DiscardProblem',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'DiscardProblem',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveCount',
        '_E_N_',
        'GetCurveCount'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveCount',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveCount',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePoints',
        '_E_N_',
        'GetCurvePoints'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePoints',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurvePoints',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameterCount',
        '_E_N_',
        'GetCurveParameterCount'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameterCount',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameterCount',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameter',
        '_E_N_',
        'GetCurveParameter'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameter',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCurveParameter',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToProfile',
        '_E_N_',
        'AddPointToProfile'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToProfile',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'AddPointToProfile',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetGraph',
        '_E_N_',
        'GetGraph'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetGraph',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetGraph',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunk',
        '_E_N_',
        'GetProfileChunk'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunk',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunk',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunkCount',
        '_E_N_',
        'GetProfileChunkCount'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunkCount',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetProfileChunkCount',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveParameter',
        '_E_N_',
        'SetCurveParameter'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveParameter',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'SetCurveParameter',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcTimeStr',
        '_E_N_',
        'GetCalcTimeStr'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcTimeStr',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetCalcTimeStr',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetRFactorStr',
        '_E_N_',
        'GetRFactorStr'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetRFactorStr',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetRFactorStr',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetAbsRFactorStr',
        '_E_N_',
        'GetAbsRFactorStr'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetAbsRFactorStr',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetAbsRFactorStr',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSqrRFactorStr',
        '_E_N_',
        'GetSqrRFactorStr'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSqrRFactorStr',
        'FORMAT_Input_EncodingStyle',
        'literal'
        );
    mm.SetOperationCustomData(
        sUNIT_NAME,
        'IFitServer',
        'GetSqrRFactorStr',
        'FORMAT_OutputEncodingStyle',
        'literal'
        );
end;


initialization
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TResult), 'TResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TBoolResult), 'TBoolResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TPointsResult), 'TPointsResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TIntResult), 'TIntResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TNamedPointsResult),
        'TNamedPointsResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TSpecParamResult),
        'TSpecParamResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TPictureResult), 'TPictureResult');
    GetTypeRegistry().Register(sNAME_SPACE, TypeInfo(TStringResult), 'TStringResult');

    GetTypeRegistry().ItemByTypeInfo[TypeInfo(TBoolResult)].RegisterExternalPropertyName(
        '_Result', 'Result');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(
        TPointsResult)].RegisterExternalPropertyName(
        '_Result', 'Result');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(TIntResult)].RegisterExternalPropertyName(
        '_Result', 'Result');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(
        TNamedPointsResult)].RegisterExternalPropertyName('_Result', 'Result');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(
        TSpecParamResult)].RegisterExternalPropertyName('_Type', 'Type');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(
        TPictureResult)].RegisterExternalPropertyName(
        '_Result', 'Result');
    GetTypeRegistry().ItemByTypeInfo[TypeInfo(
        TStringResult)].RegisterExternalPropertyName(
        '_Result', 'Result');

end.
