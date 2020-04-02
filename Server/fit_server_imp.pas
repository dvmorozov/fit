{
This unit has been produced by ws_helper.
  Input unit name : "fit_server".
  This unit name  : "fit_server_imp".
  Date            : "27.10.2008 14:19:52".
}
unit fit_server_imp;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, fit_server_app, component_list,
    MyExceptions, base_service_intf, server_service_intf,
    int_fit_server, title_points_set, points_set, int_points_set;

type
    //  metody etogo klassa vyzyvayutsya vneschnimi klientami,
    //  poetomu vyhod isklyuchenii za granitsy metodov nedopustim,
    //  krome togo metody ne dolzhny byt' tormozom v mnogopotochnoy srede,
    //  no dolzhny zaschischat' obschie dannye ot odnovremennogo dostupa;
    //  poskol'ku prilozhenie servernoe, to dolzhna byt' zaschita ot
    //  utechki pamyati;
    //  dannye ne mogut byt' svyazany s ekzemplyarom dannogo klassa,
    //  tak kak dlya kazhdogo vyzova sozayetsya novyi ekzemplyar klassa!
    TFitServer_ServiceImp = class(TBaseServiceImplementation, IFitServer)
    protected
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
        function SetBackgroundPointsSet(const BackgroundPoints:
            TArrayOfFloatDoubleRemotable; const ProblemID: integer): TResult;
        function SetCurvePositions(const CurvePositions:
            TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
        function SetCurveBounds(const CurveBounds: TArrayOfFloatDoubleRemotable;
            const ProblemID: integer): TResult;
        function AddPointToProfile(const XValue: double;
            const YValue: double; const ProblemID: integer): TResult;
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
        function GetDeltaProfilePointsSet(
            const ProblemID: integer): TPointsResult;
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
        function GetCurveCount(const ProblemID: longint): TIntResult;
        function GetCurvePoints(const SpecIndex: integer;
            const ProblemID: longint): TNamedPointsResult;
        function GetCurveParameterCount(const ProblemID: integer;
            const SpecIndex: integer): TIntResult;
        function GetCurveParameter(const ProblemID: integer;
            const SpecIndex: integer; const ParamIndex: integer): TSpecParamResult;
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

procedure RegisterFitServerImplementationFactory();

//  ne prostoe statsionarnoe prilozhenie, a servernoe
var
    ProblemList: TComponentList;
    CS: TRTLCriticalSection;        //  kriticheskaya sektsiya dostupa
   //  k spisku podzadach
implementation

uses config_objects, self_copied_component, (* LazJPEG *) LazPNG, app, form_main,
    Forms, fit_server_aux;

const
    InadmissibleProblemID: string = 'Inadmissible problem ID!';
    //'Inadmissible client ID!';
    InadmissibleChunkNum: string = 'Inadmissible chunk number!';
    InternalError: string = 'Internal service error. Error code: ';

{ The module is generated, so all hints are suppressed. }
{$hints off}

{ TFitServer_ServiceImp implementation }
function TFitServer_ServiceImp.SmoothProfile(const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end;
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.SmoothProfile(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SubtractBackground(const Auto: boolean;
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.SubtractBackground(
            Auto, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.DoAllAutomatically(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.DoAllAutomatically(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.MinimizeDifference(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.MinimizeDifference(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.MinimizeNumberOfCurves(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.MinimizeNumberOfCurves(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ComputeCurveBounds(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ComputeCurveBounds(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ComputeCurvePositions(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ComputeCurvePositions(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ComputeBackgroundPoints(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ComputeBackgroundPoints(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.StopAsyncOper(const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.StopAsyncOper(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCalcTimeStr(
    const ProblemID: integer): TStringResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := '';
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result._Result := Problem.FitStub.GetCalcTimeStr(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetRFactorStr(
    const ProblemID: integer): TStringResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := '';
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result._Result := Problem.FitStub.GetRFactorStr(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetAbsRFactorStr(
    const ProblemID: integer): TStringResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := '';
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result._Result := Problem.FitStub.GetAbsRFactorStr(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetSqrRFactorStr(
    const ProblemID: integer): TStringResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := '';
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result._Result := Problem.FitStub.GetSqrRFactorStr(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.AsyncOper(const ProblemID: integer): TBoolResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TBoolResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := False;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result._Result := Problem.FitStub.AsyncOper(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurveCount(
    const ProblemID: longint): TIntResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.GetCurveCount(
            Result._Result, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurveParameterCount(const ProblemID: integer;
    const SpecIndex: integer): TIntResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.GetCurveParameterCount(
            SpecIndex, Result._Result, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

const
    //SelectedAreaName: string = 'Selected area';
    //ProfileName: string = 'Data';
    SelectedAreaName: string = 'Experimental data';
    RFactorBoundsName: string = 'Spec.app.intervals';
    BackgroundPointsName: string = 'Background points';
    CurvePositionsName: string = 'Spec.positions';
    SummarizedName: string = 'Summarized';
    DeltaName: string      = 'Difference';

function TFitServer_ServiceImp.GetGraph(const Width: integer;
    const Height: integer; const ProblemID: integer): TPictureResult;
var
    Problem: TFitServerApp;
    //  !!! osvobozhdat' ne nuzhno, poskol'ku kopii ne sozdayutsya !!!
    Data, BackgroundPoints, GaussProfile, DeltaProfile, CurvePositions,
    RFactorBounds: TTitlePointsSet;
    Curves: TSelfCopiedCompList;
    Bitmap: TPNGImage;//TJPEGImage;
    Stream: TMemoryStream;
    i:    longint;
    B:    byte;
    R:    TRect;
    //SizeChanged: Boolean;
    EC:   longint;
    W, H: longint;
begin
    try
        Result := nil;
        Result := TPictureResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Assert(Assigned(Problem.Viewer));
        //  postroenie grafika
        W := Width;
        H := Height;
        if W < 0 then
            W := 400;
        if W > 1000 then
            W := 1000;
        if H < 0 then
            H := 300;
        if H > 1000 then
            H := 1000;

        if (Problem.Form.Chart.Bitmap.Width <> W) or
            (Problem.Form.Chart.Bitmap.Height <> H) then
        begin
            //SizeChanged := True;
            //  razmery formy bol'sche znacheniya ne imeyut -
            //  nuzhno ustanavlivat' tol'ko razmery bitmapa
            //Problem.Form.ClientWidth := W;
            //Problem.Form.ClientHeight := H;
            //Problem.Form.Chart.Width := W;
            //Problem.Form.Chart.Height := H;
            Problem.Form.Chart.Bitmap.Width  := W;
            Problem.Form.Chart.Bitmap.Height := H;
        end;
        //  !!! eto vyzyvaet iskluchenie !!!
        //Application.ProcessMessages;
        with Problem.Viewer do
        begin
            SetViewMarkers(True);
            Clear(Self);
            (*
            if SizeChanged then
            begin
                Paint;      //  !!! posle izmeneniya razmerov
                            //  pervyi raz risuet ne pravil'no;
                            //  eto pomogaet !!!
            end;
            *)
            if Problem.FitStub.GetSelectedProfileIntervalMode then
            begin
                Result.ErrCode :=
                    Problem.FitStub.GetSelectedProfileInterval(Data, Result.ErrMsg);
                if Result.ErrCode <> 0 then
                    Exit;

                //Data.Lambda := WaveLength;
                Data.Title := SelectedAreaName;
                PlotSelectedProfileInterval(nil, Data);
            end
            else
            begin
                Result.ErrCode :=
                    Problem.FitStub.GetProfilePointsSet(Data, Result.ErrMsg);
                if Result.ErrCode <> 0 then
                    Exit;

                //Data.Lambda := WaveLength;
                Data.Title := SelectedAreaName;
                PlotDataPoints(nil, Data);
            end;

            BackgroundPoints := nil;
            Result.ErrCode   := Problem.FitStub.GetBackgroundPoints(
                BackgroundPoints, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;

            if Assigned(BackgroundPoints) and
                (BackgroundPoints.PointsCount <> 0) then
            begin
                //BackgroundPoints.Lambda := WaveLength;
                BackgroundPoints.Title := BackgroundPointsName;
                PlotBackground(nil, BackgroundPoints);
            end;

            Result.ErrCode := Problem.FitStub.GetCalcProfilePointsSet(
                GaussProfile, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;

            if Assigned(GaussProfile) and
                (GaussProfile.PointsCount <> 0) then
            begin
                //GaussProfile.Lambda := WaveLength;
                GaussProfile.Title := SummarizedName;
                PlotGaussProfile(nil, GaussProfile);
            end;

            Result.ErrCode := Problem.FitStub.GetDeltaProfilePointsSet(
                DeltaProfile, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;

            if Assigned(DeltaProfile) and
                (DeltaProfile.PointsCount <> 0) then
            begin
                //DeltaProfile.Lambda := WaveLength;
                DeltaProfile.Title := DeltaName;
                PlotDeltaProfile(nil, DeltaProfile);
            end;

            Result.ErrCode := Problem.FitStub.GetCurvePositions(
                CurvePositions, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;

            if Assigned(CurvePositions) and
                (CurvePositions.PointsCount <> 0) then
            begin
                //CurvePositions.Lambda := WaveLength;
                CurvePositions.Title := CurvePositionsName;
                PlotCurvePositions(nil, CurvePositions);
            end;

            Result.ErrCode := Problem.FitStub.GetRFactorBounds(
                RFactorBounds, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;

            if Assigned(RFactorBounds) and
                (RFactorBounds.PointsCount <> 0) then
            begin
                //RFactorBounds.Lambda := WaveLength;
                RFactorBounds.Title := RFactorBoundsName;
                PlotRFactorBounds(nil, RFactorBounds);
            end;

            (*  poka ne predusmotreno masschtabirovanie risunka
                krivye risovat' ne nuzhno - vse slivaetsya *)
            Result.ErrCode := Problem.FitStub.GetCurvesList(
                Curves, Result.ErrMsg);
            if Result.ErrCode <> 0 then
                Exit;
            //  nebol'schoe kolichestvo krivyh vse-taki budem vyvodit'
            if Curves.Count <= 10 then
                PlotCurves(nil, Curves, nil);
            Paint;
        end;

        Bitmap := TPNGImage.Create;//TJPEGImage.Create;
        try
            //  ispol'zuyutsya real'nye razmery
            Bitmap.Width  := Problem.Form.Chart.Bitmap.Width;
            Bitmap.Height := Problem.Form.Chart.Bitmap.Height;

            R.Left   := 0;
            R.Top    := 0;
            R.Right  := Problem.Form.Chart.Bitmap.Width;
            R.Bottom := Problem.Form.Chart.Bitmap.Height;

            Bitmap.Canvas.CopyRect(R, Problem.Form.Chart.Bitmap.Canvas, R);
            //  sohranenie grafika
            Stream := TMemoryStream.Create;
            try
                Bitmap.SaveToStream(Stream);
                Result._Result := TArrayOfInt8URemotable.Create;
                Result._Result.SetLength(Stream.Size);
                Stream.Seek(0, soFromBeginning);

                for i := 0 to Stream.Size - 1 do
                begin
                    Stream.Read(B, 1);
                    Result._Result[i] := B;
                end;
            finally
                Stream.Free;
            end;
        finally
            Bitmap.Free;
        end;
        Result.ErrCode := 0;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

const
    ChunkSize: longint = 20;            //  chislo elementov v kuske

function TFitServer_ServiceImp.GetProfileChunk(const ProblemID: integer;
    const ChunkNum: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS, Temp: TPointsSet;
    i, ChunkCount: longint;
    EC: longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetProfilePointsSet(
            PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
        begin
            //  izvlekaetsya kusok
            ChunkCount := PS.PointsCount div ChunkSize;
            if PS.PointsCount mod ChunkSize <> 0 then
                Inc(ChunkCount);
            //  odin kusok vsegda rezerviruetsya dlya vvoda dannyh
            if ChunkCount = 0 then
                Inc(ChunkCount);

            if (ChunkNum > ChunkCount) or (ChunkNum < 1) then
                raise EUserException.Create(InadmissibleChunkNum);
            //  !!! kusok d.b. izvlechen iz uporyadochennyh dannyh !!!
            PS.Sort;

            Temp := TPointsSet.Create(nil);
            try
                for i := (ChunkNum - 1) * ChunkSize to ChunkNum * ChunkSize - 1 do
                begin
                    if i > PS.PointsCount - 1 then
                        Break;
                    Temp.AddNewPoint(PS.PointXCoord[i], PS.PointYCoord[i]);
                end;
                Result._Result := CreateRemotableArray(Temp);
            finally
                Temp.Free;
            end;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetProfileChunkCount(
    const ProblemID: integer): TIntResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.GetProfilePointsSet(
            PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
        begin
            Result._Result := PS.PointsCount div ChunkSize;
            if PS.PointsCount mod ChunkSize <> 0 then
                Inc(Result._Result);
            //  odin kusok vsegda rezerviruetsya dlya vvoda dannyh
            if Result._Result = 0 then
                Inc(Result._Result);
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurveParameter(const ProblemID: integer;
    const SpecIndex: integer; const ParamIndex: integer): TSpecParamResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TSpecParamResult.Create;  //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.GetCurveParameter(
            SpecIndex, ParamIndex, Result.Name, Result.Value,
            Result._Type, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetCurveParameter(const ProblemID: integer;
    const SpecIndex: integer; const ParamIndex: integer;
    const Value: double): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.SetCurveParameter(
            SpecIndex, ParamIndex, Value, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SelectProfileInterval(const StartPointIndex: integer;
    const StopPointIndex: integer; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.SelectProfileInterval(
            StartPointIndex, StopPointIndex, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SelectEntireProfile(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.SelectEntireProfile(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.CreateCurveList(
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.CreateCurveList(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetProfilePointsSet(
    const PointsSet: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    PS:      TTitlePointsSet;
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        PS      := CreateNamedPointsSet(PointsSet);
        try
            Result.ErrCode := Problem.FitStub.SetProfilePointsSet(
                PS, Result.ErrMsg);
        finally
            PS.Free;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetBackgroundPointsSet(
    const BackgroundPoints: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    PS:      TTitlePointsSet;
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        PS      := CreateNamedPointsSet(BackgroundPoints);
        try
            Result.ErrCode := Problem.FitStub.SetBackgroundPointsSet(
                PS, Result.ErrMsg);
        finally
            PS.Free;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetCurvePositions(
    const CurvePositions: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    PS:      TPointsSet;
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        PS      := CreateNamedPointsSet(CurvePositions);
        try
            Result.ErrCode := Problem.FitStub.SetCurvePositions(
                PS, Result.ErrMsg);
        finally
            PS.Free;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetCurveBounds(
    const CurveBounds: TArrayOfFloatDoubleRemotable;
    const ProblemID: integer): TResult;
var
    PS:      TPointsSet;
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        PS      := CreateNamedPointsSet(CurveBounds);
        try
            Result.ErrCode := Problem.FitStub.SetRFactorBounds(
                PS, Result.ErrMsg);
        finally
            PS.Free;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.AddPointToProfile(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode :=
            Problem.FitStub.AddPointToProfile(XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.AddPointToBackground(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode :=
            Problem.FitStub.AddPointToBackground(XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.AddPointToRFactorBounds(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.AddPointToRFactorBounds(
            XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.AddPointToCurvePositions(const XValue: double;
    const YValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.AddPointToCurvePositions(
            XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetProfilePointsSet(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    Result := nil;
    PS     := nil;
    try
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetProfilePointsSet(
            PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetSelectedProfileInterval(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    Result := nil;
    PS     := nil;
    try
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetSelectedProfileInterval(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetBackgroundPoints(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetBackgroundPoints(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurvePositions(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetCurvePositions(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.SetRFactorBounds(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetRFactorBounds(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCalcProfilePointsSet(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetCalcProfilePointsSet(
            PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetDeltaProfilePointsSet(
    const ProblemID: integer): TPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetDeltaProfilePointsSet(
            PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurvePoints(const SpecIndex: integer;
    const ProblemID: longint): TNamedPointsResult;
var
    Problem: TFitServerApp;
    PS:      TPointsSet;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TNamedPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    Result._Result := nil;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        //  Points - pryamoi ukazatel', a ne kopiya - ne osvobozhdat'!
        Result.ErrCode := Problem.FitStub.GetCurvePoints(SpecIndex,
            PS, Result.Name, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

procedure TFitServer_ServiceImp.SetCurveThresh(const CurveThresh: double;
    const ProblemID: integer);
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Problem.FitStub.SetCurveThresh(CurveThresh);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetMaxRFactor(const ProblemID: integer): double;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := Problem.FitStub.GetMaxRFactor;
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

procedure TFitServer_ServiceImp.SetMaxRFactor(const MaxRFactor: double;
    const ProblemID: integer);
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Problem.FitStub.SetMaxRFactor(MaxRFactor);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetBackFactor(const ProblemID: integer): double;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := Problem.FitStub.GetBackFactor;
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

procedure TFitServer_ServiceImp.SetBackFactor(const BackFactor: double;
    const ProblemID: integer);
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Problem.FitStub.SetBackFactor(BackFactor);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurveType(const ProblemID: integer): TCurveTypeId;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := StringToGuid('{00000000-0000-0000-0000-000000000000}');
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := Problem.FitStub.GetCurveType;
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

procedure TFitServer_ServiceImp.SetCurveType(const CurveTypeId: TCurveTypeId;
    const ProblemID: integer);
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Problem.FitStub.SetCurveType(CurveTypeId);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetWaveLength(const ProblemID: integer): double;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := Problem.FitStub.GetWaveLength;
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

procedure TFitServer_ServiceImp.SetWaveLength(const WaveLength: double;
    const ProblemID: integer);
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Problem.FitStub.SetWaveLength(WaveLength);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetCurveThresh(const ProblemID: integer): double;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := Problem.FitStub.GetCurveThresh;
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.GetState(const ProblemID: integer): integer;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Result := 0;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                Exit;  //  Leave... vysyvaetsya v takom sluchae!
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result  := integer(Problem.FitStub.GetState);
    except
        //  kod oschibki ne predusmotren
        on E: EUserException do
        begin
            WriteLog(E.Message, User);
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

function TFitServer_ServiceImp.ReplacePointInProfile(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double;
    const NewYValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ReplacePointInProfile(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ReplacePointInBackground(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double;
    const NewYValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ReplacePointInCurveBounds(const PrevXValue: double;
    const PrevYValue: double; const NewXValue: double;
    const NewYValue: double; const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.ReplacePointInCurvePositions(
    const PrevXValue: double; const PrevYValue: double;
    const NewXValue: double; const NewYValue: double;
    const ProblemID: integer): TResult;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do
        begin
            WriteLog(E.Message, Fatal);
            Exit;
        end
        else
            Exit;
    end;

    Result.ErrCode := -2;
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            //  proverka dopustimosti ProblemID
            if ProblemList.IndexOf(Pointer(ProblemID)) = -1 then
                //  Leave... vysyvaetsya v takom sluchae!
                raise EUserException.Create(InadmissibleProblemID);
        finally
            LeaveCriticalsection(CS);
        end;
        Problem := TFitServerApp(ProblemID);
        Result.ErrCode := Problem.FitStub.ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg  := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
end;

function TFitServer_ServiceImp.CreateProblem(): integer;
var
    Problem: TFitServerApp;
    EC:      longint;
begin
    Problem := nil;
    Result  := 0;    //  vozvrat v sluchae oschibki
    try
        Assert(Assigned(ProblemList));
        Problem := TFitServerApp.Create(nil);
        Result  := longint(Problem);

        EnterCriticalsection(CS);
        try
            ProblemList.Add(Problem);

            WriteLog('Problem created: ProblemID=' + IntToStr(Result) +
                '; Problem total=' + IntToStr(ProblemList.Count) +
                '; Time=' + DateTimeToStr(Now), Notification_);
        finally
            LeaveCriticalsection(CS);
        end;
    except
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Problem.Free;
        end
        else
            Problem.Free;
    end;
end;

procedure TFitServer_ServiceImp.DiscardProblem(const ProblemID: integer);
var
    EC: longint;
begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            if ProblemList.Remove(Pointer(ProblemID)) <> -1 then
                WriteLog('Problem removed: ProblemID=' + IntToStr(ProblemID) +
                    '; Problem total=' + IntToStr(ProblemList.Count) +
                    '; Time=' + DateTimeToStr(Now), Notification_);
        finally
            LeaveCriticalsection(CS);
        end;
    except
        //  kod oschibki ne predusmotren
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
        end;
    end;
end;

procedure RegisterFitServerImplementationFactory();
begin
    GetServiceImplementationRegistry().Register('IFitServer',
        TImplementationFactory.Create(TFitServer_ServiceImp,
        wst_GetServiceConfigText('IFitServer')) as
        IServiceImplementationFactory);
end;

{$hints on}

initialization
    InitCriticalSection(CS);
    ProblemList := TComponentList.Create(nil);
    ProblemList.SetState(cfActive);

finalization
    EnterCriticalsection(CS);
    try
        ProblemList.Free;
        ProblemList := nil;
    finally
        LeaveCriticalsection(CS);
    end;
    DoneCriticalSection(CS);
end.
