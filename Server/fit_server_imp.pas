{
This unit has been produced by ws_helper.
  Input unit name : "fit_server".
  This unit name  : "fit_server_imp".
  Date            : "27.10.2008 14:19:52".
}
Unit fit_server_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, fit_server_app, component_list,
    MyExceptions, base_service_intf, server_service_intf,
    int_fit_server, title_points_set, points_set, int_points_set;

Type
  //  metody etogo klassa vyzyvayutsya vneschnimi klientami,
  //  poetomu vyhod isklyuchenii za granitsy metodov nedopustim,
  //  krome togo metody ne dolzhny byt' tormozom v mnogopotochnoy srede,
  //  no dolzhny zaschischat' obschie dannye ot odnovremennogo dostupa;
  //  poskol'ku prilozhenie servernoe, to dolzhna byt' zaschita ot
  //  utechki pamyati;
  //  dannye ne mogut byt' svyazany s ekzemplyarom dannogo klassa,
  //  tak kak dlya kazhdogo vyzova sozayetsya novyi ekzemplyar klassa!
  TFitServer_ServiceImp=class(TBaseServiceImplementation, IFitServer)
  Protected
    function SmoothProfile(
      const  ProblemID : integer
    ):TResult;
    function SubtractBackground(
      const  Auto : boolean;
      const  ProblemID : integer
    ):TResult;
    function DoAllAutomatically(
      const  ProblemID : integer
    ):TResult;
    function MinimizeDifference(
      const  ProblemID : integer
    ):TResult;
    function MinimizeNumberOfCurves(
      const  ProblemID : integer
    ):TResult;
    function FindSpecimenIntervals(
      const  ProblemID : integer
    ):TResult;
    function FindSpecimenPositions(
      const  ProblemID : integer
    ):TResult;
    function ComputeBackgroundPoints(
      const  ProblemID : integer
    ):TResult;
    function StopAsyncOper(
      const  ProblemID : integer
    ):TResult;
    function AsyncOper(
      const  ProblemID : integer
    ):TBoolResult;
    function SelectArea(
      const  StartPointIndex : integer;
      const  StopPointIndex : integer;
      const  ProblemID : integer
    ):TResult;
    function ReturnToTotalProfile(
      const  ProblemID : integer
    ):TResult;
    function CreateSpecimenList(
      const  ProblemID : integer
    ):TResult;
    function SetProfilePointsSet(
      const  PointsSet : TArrayOfFloatDoubleRemotable;
      const  ProblemID : integer
    ):TResult;
    function SetBackgroundPointsSet(
      const  BackgroundPoints : TArrayOfFloatDoubleRemotable;
      const  ProblemID : integer
    ):TResult;
    function SetSpecimenPositions(
      const  SpecimenPositions : TArrayOfFloatDoubleRemotable;
      const  ProblemID : integer
    ):TResult;
    function SetSpecimenIntervals(
      const  SpecimenIntervals : TArrayOfFloatDoubleRemotable;
      const  ProblemID : integer
    ):TResult;
    function AddPointToData(
      const  XValue : Double;
      const  YValue : Double;
      const  ProblemID : integer
    ):TResult;
    function AddPointToBackground(
      const  XValue : Double;
      const  YValue : Double;
      const  ProblemID : integer
    ):TResult;
    function AddPointToSpecimenIntervals(
      const  XValue : Double;
      const  YValue : Double;
      const  ProblemID : integer
    ):TResult;
    function AddPointToSpecimenPositions(
      const  XValue : Double;
      const  YValue : Double;
      const  ProblemID : integer
    ):TResult;
    function GetProfilePointsSet(
      const  ProblemID : integer
    ):TPointsResult;
    function GetSelectedArea(
      const  ProblemID : integer
    ):TPointsResult;
    function GetBackgroundPoints(
      const  ProblemID : integer
    ):TPointsResult;
    function GetSpecimenPositions(
      const  ProblemID : integer
    ):TPointsResult;
    function GetSpecimenIntervals(
      const  ProblemID : integer
    ):TPointsResult;
    function GetCalcProfilePointsSet(
      const  ProblemID : integer
    ):TPointsResult;
    function GetDeltaProfilePointsSet(
      const  ProblemID : integer
    ):TPointsResult;
    procedure SetCurveThresh(
      const  CurveThresh : Double; 
      const  ProblemID : integer
    );
    function GetMaxRFactor(
      const  ProblemID : integer
    ):Double;
    procedure SetMaxRFactor(
      const  MaxRFactor : Double; 
      const  ProblemID : integer
    );
    function GetBackFactor(
      const  ProblemID : integer
    ):Double;
    procedure SetBackFactor(
      const  BackFactor : Double; 
      const  ProblemID : integer
    );
    function GetCurveType(
      const  ProblemID : integer
    ):TCurveTypeId;
    procedure SetCurveType(
      const  CurveTypeId : TCurveTypeId;
      const  ProblemID : integer
    );
    function GetWaveLength(
      const  ProblemID : integer
    ):Double;
    procedure SetWaveLength(
      const  WaveLength : Double; 
      const  ProblemID : integer
    );
    function GetCurveThresh(
      const  ProblemID : integer
    ):Double;
    function GetState(
      const  ProblemID : integer
    ):integer;
    function ReplacePointInProfile(
      const  PrevXValue : Double;
      const  PrevYValue : Double;
      const  NewXValue : Double;
      const  NewYValue : Double;
      const  ProblemID : integer
    ):TResult;
    function ReplacePointInBackground(
      const  PrevXValue : Double;
      const  PrevYValue : Double;
      const  NewXValue : Double;
      const  NewYValue : Double;
      const  ProblemID : integer
    ):TResult;
    function ReplacePointInSpecimenIntervals(
      const  PrevXValue : Double;
      const  PrevYValue : Double;
      const  NewXValue : Double;
      const  NewYValue : Double;
      const  ProblemID : integer
    ):TResult;
    function ReplacePointInSpecimenPositions(
      const  PrevXValue : Double;
      const  PrevYValue : Double;
      const  NewXValue : Double;
      const  NewYValue : Double;
      const  ProblemID : integer
    ):TResult;
    function CreateProblem():integer;
    procedure DiscardProblem(
       const ProblemID : integer
    );
    function GetSpecimenCount(
       const ProblemID : Longint
    ):TIntResult;
    function GetSpecimenPoints(
      const  SpecIndex : integer;
      const  ProblemID : Longint
    ):TNamedPointsResult;
    function GetSpecimenParameterCount(
      const  ProblemID : integer;
      const  SpecIndex : integer
    ):TIntResult;
    function GetSpecimenParameter(
      const  ProblemID : integer;
      const  SpecIndex : integer;
      const  ParamIndex : integer
    ):TSpecParamResult;
    function GetGraph(
      const  Width : integer;
      const  Height : integer;
      const  ProblemID : integer
    ):TPictureResult;
    function GetProfileChunk(
      const  ProblemID : integer;
      const  ChunkNum : integer
    ):TPointsResult;
    function GetProfileChunkCount(
      const  ProblemID : integer
    ):TIntResult;
    function SetSpecimenParameter(
      const  ProblemID : integer;
      const  SpecIndex : integer;
      const  ParamIndex : integer;
      const  Value : Double
    ):TResult;
    function GetCalcTimeStr(
      const  ProblemID : integer
    ):TStringResult;
    function GetRFactorStr(
      const  ProblemID : integer
    ):TStringResult;
    function GetAbsRFactorStr(
      const  ProblemID : integer
    ):TStringResult;
    function GetSqrRFactorStr(
      const  ProblemID : integer
    ):TStringResult;
  End;

procedure RegisterFitServerImplementationFactory();

//  ne prostoe statsionarnoe prilozhenie, a servernoe
var ProblemList: TComponentList;
    CS: TRTLCriticalSection;        //  kriticheskaya sektsiya dostupa
                                    //  k spisku podzadach
Implementation

uses config_objects, self_copied_component, (* LazJPEG *) LazPNG, app, form_main,
    Forms, fit_server_aux;

const
    InadmissibleProblemID: string = 'Inadmissible problem ID!';   //'Inadmissible client ID!';
    InadmissibleChunkNum: string = 'Inadmissible chunk number!';
    InternalError: string = 'Internal service error. Error code: ';

{ The module is generated, so all hints are suppressed. }
{$hints off}

{ TFitServer_ServiceImp implementation }
function TFitServer_ServiceImp.SmoothProfile(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end;
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SubtractBackground(
    const  Auto : boolean;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.DoAllAutomatically(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.MinimizeDifference(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC:LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.MinimizeNumberOfCurves(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.FindSpecimenIntervals(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.FindSpecimenPositions(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.ComputeBackgroundPoints(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.StopAsyncOper(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetCalcTimeStr(
    const  ProblemID : integer
    ):TStringResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetRFactorStr(
    const  ProblemID : integer
    ):TStringResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetAbsRFactorStr(
    const  ProblemID : integer
    ):TStringResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSqrRFactorStr(
    const  ProblemID : integer
    ):TStringResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TStringResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.AsyncOper(
    const  ProblemID : integer
    ):TBoolResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TBoolResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenCount(
    const ProblemID : Longint
    ):TIntResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetSpecimenCount(
            Result._Result, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenParameterCount(
    const  ProblemID : integer;
    const  SpecIndex : integer
    ):TIntResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetSpecimenParameterCount(
            SpecIndex, Result._Result, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

const
    //SelectedAreaName: string = 'Selected area';
    //ProfileName: string = 'Data';
    SelectedAreaName: string = 'Experimental data';
    RFactorIntervalsName: string = 'Spec.app.intervals';
    BackgroundPointsName: string = 'Background points';
    CurvePositionsName: string = 'Spec.positions';
    SummarizedName: string = 'Summarized';
    DeltaName: string = 'Difference';

function TFitServer_ServiceImp.GetGraph(
    const  Width : integer;
    const  Height : integer;
    const  ProblemID : integer
    ):TPictureResult;
var Problem: TFitServerApp;
    //  !!! osvobozhdat' ne nuzhno, poskol'ku kopii ne sozdayutsya !!!
    Data, BackgroundPoints, GaussProfile, DeltaProfile,
    CurvePositions, RFactorIntervals: TTitlePointsSet;
    Curves: TSelfCopiedCompList;
    Bitmap: TPNGImage;//TJPEGImage;
    Stream: TMemoryStream;
    i: LongInt;
    B: Byte;
    R: TRect;
    //SizeChanged: Boolean;
    EC: LongInt;
    W, H: LongInt;
Begin
    try
        Result := nil;
        Result := TPictureResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        W := Width; H := Height;
        if W < 0 then W := 400;
        if W > 1000 then W := 1000;
        if H < 0 then H := 300;
        if H > 1000 then H := 1000;

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
            Problem.Form.Chart.Bitmap.Width := W;
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
            if Problem.FitStub.GetSelectedAreaMode then
            begin
                Result.ErrCode := Problem.FitStub.GetSelectedArea(
                    Data, Result.ErrMsg);
                if Result.ErrCode <> 0 then Exit;
                
                //Data.Lambda := WaveLength;
                Data.Title := SelectedAreaName;
                PlotSelectedArea(nil, Data);
            end
            else
            begin
                Result.ErrCode := Problem.FitStub.GetProfilePointsSet(
                    Data, Result.ErrMsg);
                if Result.ErrCode <> 0 then Exit;

                //Data.Lambda := WaveLength;
                Data.Title := SelectedAreaName;
                PlotDataPoints(nil, Data);
            end;

            BackgroundPoints := nil;
            Result.ErrCode := Problem.FitStub.GetBackgroundPoints(
                BackgroundPoints, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            
            if Assigned(BackgroundPoints) and
                (BackgroundPoints.PointsCount <> 0) then
            begin
                //BackgroundPoints.Lambda := WaveLength;
                BackgroundPoints.Title := BackgroundPointsName;
                PlotBackground(nil, BackgroundPoints);
            end;
            
            Result.ErrCode := Problem.FitStub.GetCalcProfilePointsSet(
                GaussProfile, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            
            if Assigned(GaussProfile) and
                (GaussProfile.PointsCount <> 0) then
            begin
                //GaussProfile.Lambda := WaveLength;
                GaussProfile.Title := SummarizedName;
                PlotGaussProfile(nil, GaussProfile);
            end;

            Result.ErrCode := Problem.FitStub.GetDeltaProfilePointsSet(
                DeltaProfile, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            
            if Assigned(DeltaProfile) and
                (DeltaProfile.PointsCount <> 0) then
            begin
                //DeltaProfile.Lambda := WaveLength;
                DeltaProfile.Title := DeltaName;
                PlotDeltaProfile(nil, DeltaProfile);
            end;

            Result.ErrCode := Problem.FitStub.GetCurvePositions(
                CurvePositions, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            
            if Assigned(CurvePositions) and
                (CurvePositions.PointsCount <> 0) then
            begin
                //CurvePositions.Lambda := WaveLength;
                CurvePositions.Title := CurvePositionsName;
                PlotCurvePositions(nil, CurvePositions);
            end;

            Result.ErrCode := Problem.FitStub.GetRFactorIntervals(
                RFactorIntervals, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            
            if Assigned(RFactorIntervals) and
                (RFactorIntervals.PointsCount <> 0) then
            begin
                //RFactorIntervals.Lambda := WaveLength;
                RFactorIntervals.Title := RFactorIntervalsName;
                PlotRFactorIntervals(nil, RFactorIntervals);
            end;

            (*  poka ne predusmotreno masschtabirovanie risunka
                krivye risovat' ne nuzhno - vse slivaetsya *)
            Result.ErrCode := Problem.FitStub.GetCurvesList(
                Curves, Result.ErrMsg);
            if Result.ErrCode <> 0 then Exit;
            //  nebol'schoe kolichestvo krivyh vse-taki budem vyvodit'
            if Curves.Count <= 10 then
                PlotSpecimens(nil, Curves, nil);
            Paint;
        end;

        Bitmap := TPNGImage.Create;//TJPEGImage.Create;
        try
            //  ispol'zuyutsya real'nye razmery
            Bitmap.Width := Problem.Form.Chart.Bitmap.Width;
            Bitmap.Height := Problem.Form.Chart.Bitmap.Height;

            R.Left := 0; R.Top := 0;
            R.Right := Problem.Form.Chart.Bitmap.Width;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

const
    ChunkSize: LongInt = 20;            //  chislo elementov v kuske

function TFitServer_ServiceImp.GetProfileChunk(
    const  ProblemID : integer;
    const  ChunkNum : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS, Temp: TPointsSet;
    i, ChunkCount: LongInt;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            if PS.PointsCount mod ChunkSize <> 0 then Inc(ChunkCount);
            //  odin kusok vsegda rezerviruetsya dlya vvoda dannyh
            if ChunkCount = 0 then Inc(ChunkCount);

            if (ChunkNum > ChunkCount) or (ChunkNum < 1) then
                raise EUserException.Create(InadmissibleChunkNum);
            //  !!! kusok d.b. izvlechen iz uporyadochennyh dannyh !!!
            PS.Sort;
            
            Temp := TPointsSet.Create(nil);
            try
                for i := (ChunkNum - 1) * ChunkSize to ChunkNum * ChunkSize - 1 do
                begin
                    if i > PS.PointsCount - 1 then Break;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetProfileChunkCount(
    const  ProblemID : integer
    ):TIntResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TIntResult.Create;    //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            if PS.PointsCount mod ChunkSize <> 0 then Inc(Result._Result);
            //  odin kusok vsegda rezerviruetsya dlya vvoda dannyh
            if Result._Result = 0 then Inc(Result._Result);
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenParameter(
    const  ProblemID : integer;
    const  SpecIndex : integer;
    const  ParamIndex : integer
    ):TSpecParamResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TSpecParamResult.Create;  //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetSpecimenParameter(
            SpecIndex, ParamIndex, Result.Name, Result.Value,
            Result._Type, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SetSpecimenParameter(
    const  ProblemID : integer;
    const  SpecIndex : integer;
    const  ParamIndex : integer;
    const  Value : Double
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.SetSpecimenParameter(
            SpecIndex, ParamIndex, Value, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SelectArea(
    const  StartPointIndex : integer;
    const  StopPointIndex : integer;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.SelectArea(
            StartPointIndex, StopPointIndex, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.ReturnToTotalProfile(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.ReturnToTotalProfile(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.CreateSpecimenList(
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.CreateSpecimenList(Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SetProfilePointsSet(
    const  PointsSet : TArrayOfFloatDoubleRemotable;
    const  ProblemID : integer
    ):TResult;
var PS: TTitlePointsSet;
    Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        PS := CreateNamedPointsSet(PointsSet);
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SetBackgroundPointsSet(
    const  BackgroundPoints : TArrayOfFloatDoubleRemotable;
    const  ProblemID : integer
    ):TResult;
var PS: TTitlePointsSet;
    Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        PS := CreateNamedPointsSet(BackgroundPoints);
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SetSpecimenPositions(
    const  SpecimenPositions : TArrayOfFloatDoubleRemotable;
    const  ProblemID : integer
    ):TResult;
var PS: TPointsSet;
    Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        PS := CreateNamedPointsSet(SpecimenPositions);
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.SetSpecimenIntervals(
    const  SpecimenIntervals : TArrayOfFloatDoubleRemotable;
    const  ProblemID : integer
    ):TResult;
var PS: TPointsSet;
    Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        PS := CreateNamedPointsSet(SpecimenIntervals);
        try
            Result.ErrCode := Problem.FitStub.SetRFactorIntervals(
                PS, Result.ErrMsg);
        finally
            PS.Free;
        end;
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.AddPointToData(
    const  XValue : Double;
    const  YValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Problem.FitStub.AddPointToData(XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.AddPointToBackground(
    const  XValue : Double;
    const  YValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.AddPointToSpecimenIntervals(
    const  XValue : Double;
    const  YValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.AddPointToRFactorIntervals(
            XValue, YValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.AddPointToSpecimenPositions(
    const  XValue : Double;
    const  YValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetProfilePointsSet(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    Result := nil;
    PS := nil;
    try
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSelectedArea(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    Result := nil;
    PS := nil;
    try
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetSelectedArea(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetBackgroundPoints(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenPositions(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenIntervals(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetRFactorIntervals(PS, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetCalcProfilePointsSet(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetDeltaProfilePointsSet(
    const  ProblemID : integer
    ):TPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.GetSpecimenPoints(
    const SpecIndex : integer;
    const ProblemID : Longint
    ):TNamedPointsResult;
var Problem: TFitServerApp;
    PS: TPointsSet;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TNamedPointsResult.Create; //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.GetSpecimenPoints(SpecIndex,
            PS, Result.Name, Result.ErrMsg);
        if Result.ErrCode = 0 then
            Result._Result := CreateRemotableArray(PS);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

procedure TFitServer_ServiceImp.SetCurveThresh(
  const  CurveThresh : Double; 
  const  ProblemID : integer
);
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
End;

function TFitServer_ServiceImp.GetMaxRFactor(
  const  ProblemID : integer
):Double;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := Problem.FitStub.GetMaxRFactor;
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
End;

procedure TFitServer_ServiceImp.SetMaxRFactor(
  const  MaxRFactor : Double; 
  const  ProblemID : integer
);
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
End;

function TFitServer_ServiceImp.GetBackFactor(
  const  ProblemID : integer
):Double;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := Problem.FitStub.GetBackFactor;
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
End;

procedure TFitServer_ServiceImp.SetBackFactor(
  const  BackFactor : Double; 
  const  ProblemID : integer
);
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
End;

function TFitServer_ServiceImp.GetCurveType(
  const  ProblemID : integer
):TCurveTypeId;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := Problem.FitStub.GetCurveType;
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
End;

procedure TFitServer_ServiceImp.SetCurveType(
  const  CurveTypeId : TCurveTypeId;
  const  ProblemID : integer
);
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
End;

function TFitServer_ServiceImp.GetWaveLength(
  const  ProblemID : integer
):Double;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := Problem.FitStub.GetWaveLength;
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
End;

procedure TFitServer_ServiceImp.SetWaveLength(
  const  WaveLength : Double; 
  const  ProblemID : integer
);
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
End;

function TFitServer_ServiceImp.GetCurveThresh(
  const  ProblemID : integer
):Double;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := Problem.FitStub.GetCurveThresh;
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
End;

function TFitServer_ServiceImp.GetState(
  const  ProblemID : integer
):integer;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
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
        Result := integer(Problem.FitStub.GetState);
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
End;

function TFitServer_ServiceImp.ReplacePointInProfile(
    const  PrevXValue : Double;
    const  PrevYValue : Double;
    const  NewXValue : Double;
    const  NewYValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.ReplacePointInBackground(
    const  PrevXValue : Double;
    const  PrevYValue : Double;
    const  NewXValue : Double;
    const  NewYValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.ReplacePointInSpecimenIntervals(
    const  PrevXValue : Double;
    const  PrevYValue : Double;
    const  NewXValue : Double;
    const  NewYValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
        Result.ErrCode := Problem.FitStub.ReplacePointInRFactorIntervals(
            PrevXValue, PrevYValue, NewXValue, NewYValue, Result.ErrMsg);
    except
        on E: EUserException do
        begin
            Result.ErrCode := -1;
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.ReplacePointInSpecimenPositions(
    const  PrevXValue : Double;
    const  PrevYValue : Double;
    const  NewXValue : Double;
    const  NewYValue : Double;
    const  ProblemID : integer
    ):TResult;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    try
        Result := nil;
        Result := TResult.Create;   //  ob'ekty vozvraschayutsya po ssylke
    except
        on E: Exception do begin WriteLog(E.Message, Fatal); Exit; end
        else Exit;
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
            Result.ErrMsg := E.Message;
        end;
        on E: Exception do
        begin   //  EAssertionFailed priravnivaetsya k kriticheskim oschibkam
            EC := GetSeqErrorCode;
            WriteLog(E.Message + StrErrorID + IntToStr(EC), Fatal);
            Result.ErrMsg := InternalError + IntToStr(EC);
        end;
    end;
End;

function TFitServer_ServiceImp.CreateProblem():integer;
var Problem: TFitServerApp;
    EC: LongInt;
Begin
    Problem := nil;
    Result := 0;    //  vozvrat v sluchae oschibki
    try
        Assert(Assigned(ProblemList));
        Problem := TFitServerApp.Create(nil);
        Result := Longint(Problem);

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
        end else Problem.Free;
    end;
End;

procedure TFitServer_ServiceImp.DiscardProblem(
   const ProblemID : integer
);
var EC: LongInt;
Begin
    try
        Assert(Assigned(ProblemList));

        EnterCriticalsection(CS);
        try
            if ProblemList.Remove(Pointer(ProblemID)) <> -1 then
            begin
                WriteLog('Problem removed: ProblemID=' + IntToStr(ProblemID) +
                    '; Problem total=' + IntToStr(ProblemList.Count) +
                    '; Time=' + DateTimeToStr(Now), Notification_);
            end;
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
End;

procedure RegisterFitServerImplementationFactory();
Begin
    GetServiceImplementationRegistry().Register('IFitServer',
        TImplementationFactory.Create(TFitServer_ServiceImp,
            wst_GetServiceConfigText('IFitServer')) as
                IServiceImplementationFactory);
End;

{$hints on}

initialization
    InitCriticalSection(CS);
    ProblemList := TComponentList.Create(nil);
    ProblemList.SetState(cfActive);
    
finalization
    EnterCriticalsection(CS);
    try
        ProblemList.Free; ProblemList := nil;
    finally
        LeaveCriticalsection(CS);
    end;
    DoneCriticalSection(CS);
End.

