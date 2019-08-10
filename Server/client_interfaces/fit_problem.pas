{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of proxy class providing access to problem id.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_problem;

{$MODE Delphi}

interface

uses
    SysUtils, mscr_specimen_list, CommonTypes, PointsSet, SelfCopied, Classes,
    MyExceptions, IntPointsSet, TitlePointsSet, CurvePointsSet, CBRCComponent,
    int_fit_service, int_fit_server, NamedPointsSet,
    base_service_intf,
    fit_server_proxy                //  XML-RPC interface to server.
    ;

type
    { Converts error codes back into exceptions to provide
      centralized error handling in the client. Converts data
      to appropriate type. Should implement the same interface
      as the server. }
    TFitProblem = class(TCBRCComponent, IFitProblem)
    protected
        FProblemId: LongInt;        //  For now only single problem id is
                                    //  supported per client.
        FFitStub: IFitServer;       //  Access the server
                                    //  via XML-RPC (wst-5.0)
                                    
        { Getting / setting server attributes. }
        
        function GetMaxRFactor: Double;
        procedure SetMaxRFactor(AMaxRFactor: Double);
        function GetBackFactor: Double;
        procedure SetBackFactor(ABackFactor: Double);
        function GetCurveThresh: Double;
        procedure SetCurveThresh(ACurveThresh: Double);
        function GetCurveType: TCurveTypeId;
        procedure SetCurveType(ACurveType: TCurveTypeId);
        function GetState: TFitServerState;
        procedure SetWaveLength(AWaveLength: Double);
        function GetWaveLength: Double;

    public
        constructor Create(AOwner: TComponent); override;
        
        { GetXXXX methods create and return A NEW OBJECT, 
          responsibility to free it is put on calling code. }
        
        function SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
        function GetProfilePointsSet: TTitlePointsSet;
        function GetSelectedArea: TTitlePointsSet;

        function SetBackgroundPointsSet(ABackgroundPoints: TTitlePointsSet): string;
        function GetBackgroundPoints: TTitlePointsSet;

        function SetCurvePositions(ACurvePositions: TPointsSet): string;
        function GetCurvePositions: TTitlePointsSet;

        function SetRFactorIntervals(ARFactorIntervals: TPointsSet): string;
        function GetRFactorIntervals: TTitlePointsSet;
{$IFNDEF EXCLUDE_SOMETHING}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(
            ACurveExpr: string;
            { Nil means initialization. }
            CP: Curve_parameters
            );
{$ENDIF}
        procedure AddPointToData(XValue, YValue: Double);
        procedure AddPointToBackground(XValue, YValue: Double);
        procedure AddPointToRFactorIntervals(XValue, YValue: Double);
        procedure AddPointToCurvePositions(XValue, YValue: Double);

        procedure ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInRFactorIntervals(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);

        function GetSpecimenList: TMSCRSpecimenList;
        function GetSpecimenCount: LongInt;
        function GetSpecimenParameterCount(SpecIndex: LongInt): LongInt;
        procedure GetSpecimenParameter(SpecIndex: LongInt; ParamIndex: LongInt;
            var Name: string; var Value: Double; var Type_: LongInt);
        procedure SetSpecimenParameter(SpecIndex: LongInt; ParamIndex: LongInt;
            Value: Double);
        function GetCurvesList: TSelfCopiedCompList;

        function GetCalcProfilePointsSet: TTitlePointsSet;
        function GetDeltaProfilePointsSet: TTitlePointsSet;

        { Asynchronous (long) methods. }
        
        function SmoothProfile: string;
        procedure SubtractAllBackground(Auto: Boolean);
        function DoAllAutomatically: string;
        function FindGausses: string;
{$IFNDEF EXCLUDE_SOMETHING}
        function FindGaussesAgain: string;
{$ENDIF}
        function FindGaussesSequentially: string;
        function FindPeakBounds: string;
        function FindBackPoints: string;
        function FindPeakPositions: string;
{$IF DEFINED(FIT) OR DEFINED(FITPRO)}
        function AllPointsAsPeakPositions: string;
{$ENDIF}
{$IFDEF FITCGI}
        function GetGraph(
            const Width: LongInt; const Height: LongInt): TMemoryStream;
        function GetProfileChunk(const ChunkNum: LongInt): TTitlePointsSet;
        function GetProfileChunkCount: LongInt;
{$ENDIF}
        { Control methods. }
        
        procedure StopAsyncOper;
        function AsyncOper: Boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;

        { Synchronous methods. }
        
        function SelectArea(StartPointIndex, StopPointIndex: LongInt): string;
        function ReturnToTotalProfile: string;
        procedure CreateSpecimenList;
        
        { Should be called in appropriate server state. }
        procedure CreateProblem;
        function GetProblemId: LongInt;
        procedure SetProblemId(AProblemId: LongInt);
        procedure DiscardProblem(AProblemId: LongInt);

        property MaxRFactor: Double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: Double read GetBackFactor write SetBackFactor;
        property CurveThresh: Double read GetCurveThresh write SetCurveThresh;
        property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;
        property State: TFitServerState read GetState;
        property WaveLength: Double read GetWaveLength write SetWaveLength;
        property FitStub: IFitServer read FFitStub write FFitStub;
    end;

implementation
uses synapse_tcp_protocol, synapse_http_protocol, soap_formatter,
    binary_formatter, fit_server_aux,
    { This module contains global definitions which are used in all applications. }
    Main;

{========================== TFitClientProxy ===================================}

constructor TFitProblem.Create(AOwner: TComponent);
begin
    FFitStub :=
        TFitServer_Proxy.Create(
            'IFitServer',
            'binary:',
            'TCP:Address=' + InternalIP +
            ';Port=' + InternalPort + ';target=IFitServer'
            );
end;

procedure TFitProblem.CreateProblem;
begin
    FProblemId := FitStub.CreateProblem;
end;

procedure TFitProblem.DiscardProblem(AProblemId: LongInt);
begin
    FitStub.DiscardProblem(AProblemId);
end;

function TFitProblem.GetProblemId: LongInt;
begin
    Result := FProblemId;
end;

procedure TFitProblem.SetProblemId(AProblemId: LongInt);
begin
    FProblemId := AProblemId;
end;

function TFitProblem.GetMaxRFactor: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetMaxRFactor(FProblemId);
end;

procedure TFitProblem.SetMaxRFactor(AMaxRFactor: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetMaxRFactor(AMaxRFactor, FProblemId);
end;

function TFitProblem.GetBackFactor: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetBackFactor(FProblemId);
end;

procedure TFitProblem.SetBackFactor(ABackFactor: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetBackFactor(ABackFactor, FProblemId);
end;

function TFitProblem.GetCurveThresh: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetCurveThresh(FProblemId);
end;

procedure TFitProblem.SetCurveThresh(ACurveThresh: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetCurveThresh(ACurveThresh, FProblemId);
end;

function TFitProblem.GetCurveType: TCurveTypeId;
begin
    Assert(Assigned(FitStub));
    Result := TCurveTypeId(FitStub.GetCurveType(FProblemId));
end;

procedure TFitProblem.SetCurveType(ACurveType: TCurveTypeId);
begin
    Assert(Assigned(FitStub));
    FitStub.SetCurveType(ACurveType, FProblemId);
end;

{$IFNDEF EXCLUDE_SOMETHING}
procedure TFitProblem.SetSpecialCurveParameters(
    ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
                            //  pervonachal'nuyu initsializatsiyu
    );
var Res: LongInt;
    ErrMsg: string;
begin
    Assert(Assigned(FitStub));
    Res := FitStub.SetSpecialCurveParameters(ACurveExpr, CP, ErrMsg);
    case Res of
        //  pustaya stroka sozdaet osh. pri rabote v otladchike
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;
{$ENDIF}

function TFitProblem.GetState: TFitServerState;
begin
    Assert(Assigned(FitStub));
    Result := TFitServerState(FitStub.GetState(FProblemId));
end;

procedure TFitProblem.SetWaveLength(AWaveLength: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetWaveLength(AWaveLength, FProblemId);
end;

function TFitProblem.GetWaveLength: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetWaveLength(FProblemId);
end;

function TFitProblem.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
var Res: LongInt;
    ErrMsg: string;
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR := CreateRemotableArray(APointsSet);
    try
        R := FitStub.SetProfilePointsSet(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.GetProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetProfilePointsSet(FProblemId));
end;

function TFitProblem.GetSelectedArea: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSelectedArea(FProblemId));
end;

function TFitProblem.SetBackgroundPointsSet(
    ABackgroundPoints: TTitlePointsSet): string;
var Res: LongInt;
    ErrMsg: string;
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR := CreateRemotableArray(ABackgroundPoints);
    try
        R := FitStub.SetBackgroundPointsSet(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.GetBackgroundPoints: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetBackgroundPoints(FProblemId));
end;

function TFitProblem.SetCurvePositions(ACurvePositions: TPointsSet): string;
var Res: LongInt;
    ErrMsg: string;
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR := CreateRemotableArray(ACurvePositions);
    try
        R := FitStub.SetSpecimenPositions(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.GetCurvePositions: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSpecimenPositions(FProblemId));
end;

function TFitProblem.SetRFactorIntervals(ARFactorIntervals: TPointsSet): string;
var Res: LongInt;
    ErrMsg: string;
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR := CreateRemotableArray(ARFactorIntervals);
    try
        R := FitStub.SetSpecimenIntervals(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.GetRFactorIntervals: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSpecimenIntervals(FProblemId));
end;

{$IFDEF FITCGI}
function TFitProblem.GetGraph(
    const Width: LongInt; const Height: LongInt): TMemoryStream;
var R: TPictureResult;
    i: LongInt;
    B: Byte;
begin
    Assert(Assigned(FitStub));
    Result := TMemoryStream.Create;
    try
        R := FitStub.GetGraph(Width, Height, FProblemId);
        if not Assigned(R) then
            raise Exception.Create(OutOfServerResources);
        try
            case R.ErrCode of
                -1: raise EUserException.Create(R.ErrMsg);
                -2: raise Exception.Create(R.ErrMsg);
            end;
            Result.Size := R._Result.Length;
            Result.Seek(0, soFromBeginning);
            for i := 0 to R._Result.Length - 1 do
            begin
                B := R._Result.Item[i];
                Result.Write(B, 1);
            end;
        finally
            R.Free;
        end;
    except
        Result.Free; Result := nil;
        raise;
    end;
end;

function TFitProblem.GetProfileChunk(
    const ChunkNum: LongInt): TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetProfileChunk(FProblemId, ChunkNum));
end;

function TFitProblem.GetProfileChunkCount: LongInt;
var Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Count := FitStub.GetProfileChunkCount(FProblemId);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    try
        Result := Count._Result;
        case Count.ErrCode of
            -1: raise EUserException.Create(Count.ErrMsg);
            -2: raise Exception.Create(Count.ErrMsg);
        end;
    finally
        Count.Free;
    end;
end;
{$ENDIF}

{$IFNDEF EXCLUDE_SOMETHING}
function TFitProblem.GetSpecialCurveParameters: Curve_parameters;
var Res: LongInt;
    ErrMsg: string;
begin
    Assert(Assigned(FitStub));
    Res := FitStub.GetSpecialCurveParameters(Result, ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := Curve_parameters(Result.GetCopy);
end;
{$ENDIF}

function TFitProblem.GetSpecimenCount: LongInt;
var Res: LongInt;
    ErrMsg: string;
    Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Count := FitStub.GetSpecimenCount(FProblemId);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.GetSpecimenParameterCount(SpecIndex: LongInt): LongInt;
var Res: LongInt;
    ErrMsg: string;
    Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Count := FitStub.GetSpecimenParameterCount(FProblemId, SpecIndex);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.GetSpecimenParameter(
    SpecIndex: LongInt; ParamIndex: LongInt;
    var Name: string; var Value: Double; var Type_: LongInt);
var Res: LongInt;
    ErrMsg: string;
    R: TSpecParamResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    R := FitStub.GetSpecimenParameter(
        FProblemId, SpecIndex, ParamIndex);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;

    Name := R.Name;
    Value := R.Value;
    Type_ := R._Type;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.SetSpecimenParameter(
    SpecIndex: LongInt; ParamIndex: LongInt;
    Value: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    R := FitStub.SetSpecimenParameter(
        FProblemId, SpecIndex, ParamIndex, Value);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.GetSpecimenList: TMSCRSpecimenList;
var Res: LongInt;
    ErrMsg: string;
    Count: TIntResult;
    SpecIndex, SpecCount: LongInt;
    ParamIndex, ParamCount: LongInt;
    R: TSpecParamResult;
    CP: Curve_parameters;
    P: TSpecialCurveParameter;
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Result := TMSCRSpecimenList.Create(nil);
    try
        Count := FitStub.GetSpecimenCount(FProblemId);
        if not Assigned(Count) then
            raise Exception.Create(OutOfServerResources);

        Res := Count.ErrCode;
        ErrMsg := Count.ErrMsg;
        SpecCount := Count._Result;
        Count.Free;
        case Res of
            -1: raise EUserException.Create(ErrMsg);
            -2: raise Exception.Create(ErrMsg);
        end;

        for SpecIndex := 0 to SpecCount - 1 do
        begin
            Count := FitStub.GetSpecimenParameterCount(FProblemId, SpecIndex);
            if not Assigned(Count) then
                raise Exception.Create(OutOfServerResources);

            Res := Count.ErrCode;
            ErrMsg := Count.ErrMsg;
            ParamCount := Count._Result;
            Count.Free;
            case Res of
                -1: raise EUserException.Create(ErrMsg);
                -2: raise Exception.Create(ErrMsg);
            end;
            
            CP := Curve_parameters.Create(nil);
            //  udalyaetsya argument, sozdavaemyi po-umolchaniyu
            CP.Params.Clear;
            try
                for ParamIndex := 0 to ParamCount - 1 do
                begin
                    R := FitStub.GetSpecimenParameter(
                        FProblemId, SpecIndex, ParamIndex);
                    if not Assigned(R) then
                        raise Exception.Create(OutOfServerResources);
                    try
                        Res := R.ErrCode;
                        ErrMsg := R.ErrMsg;
                        case Res of
                            -1: raise EUserException.Create(ErrMsg);
                            -2: raise Exception.Create(ErrMsg);
                        end;
                        
                        P := TSpecialCurveParameter(CP.Params.Add);
                        P.Name := R.Name;
                        P.Value := R.Value;
                        P.Type_ := TParameterType(R._Type);
                    finally
                        R.Free;
                    end;
                end;
                Result.Add(CP);
            except
                CP.Free; raise;
            end;
        end;
    except
        Result.Free; Result := nil;
        raise;
    end;
end;

function TFitProblem.GetCurvesList: TSelfCopiedCompList;
var Res: LongInt;
    ErrMsg: string;
    SpecCount: TIntResult;
    SpecIndex, Count: LongInt;
    Points: TNamedPointsSet;
    R: TNamedPointsResult;
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Result := TSelfCopiedCompList.Create(nil);
    try
        SpecCount := FitStub.GetSpecimenCount(FProblemId);
        if not Assigned(SpecCount) then
            raise Exception.Create(OutOfServerResources);
            
        Res := SpecCount.ErrCode;
        ErrMsg := SpecCount.ErrMsg;
        Count := SpecCount._Result;
        SpecCount.Free;
        case Res of
            -1: raise EUserException.Create(ErrMsg);
            -2: raise Exception.Create(ErrMsg);
        end;

        for SpecIndex := 0 to Count - 1 do
        begin
            R := FitStub.GetSpecimenPoints(SpecIndex, FProblemId);
            if not Assigned(R) then
                raise Exception.Create(OutOfServerResources);

            try
                Res := R.ErrCode;
                ErrMsg := R.ErrMsg;
                case Res of
                    -1: raise EUserException.Create(ErrMsg);
                    -2: raise Exception.Create(ErrMsg);
                end;

                Points := CreateNamedPointsSet(R._Result);
                Result.Add(Points);
                Points.SetCurveTypeName(R.Name);
            finally
                R.Free;
            end;
        end;
    except
        Result.Free; Result := nil;
        raise;
    end;
end;

function TFitProblem.GetCalcProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetCalcProfilePointsSet(FProblemId));
end;

function TFitProblem.GetDeltaProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetDeltaProfilePointsSet(FProblemId));
end;

function TFitProblem.SmoothProfile: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SmoothProfile(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

procedure TFitProblem.SubtractAllBackground(Auto: Boolean);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SubtractAllBackground(Auto, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.DoAllAutomatically: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.DoAllAutomatically(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.FindGausses: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeDifference(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$IFNDEF EXCLUDE_SOMETHING}
function TFitProblem.FindGaussesAgain: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindGaussesAgain;
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;
{$ENDIF}

function TFitProblem.FindGaussesSequentially: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeNumberOfSpecimens(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.FindPeakBounds: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindSpecimenIntervals(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.FindBackPoints: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindBackPoints(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.FindPeakPositions: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindSpecimenPositions(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$IF DEFINED(FIT) OR DEFINED(FITPRO)}
function TFitProblem.AllPointsAsPeakPositions: string;
var Res: LongInt;
    ErrMsg: string;
begin
    Assert(Assigned(FitStub));
    Res := FitStub.AllPointsAsPeakPositions(ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;
{$ENDIF}

procedure TFitProblem.StopAsyncOper;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.StopAsyncOper(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.AsyncOper: Boolean;
var ErrMsg: string;
    R: TBoolResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AsyncOper(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetCalcTimeStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetCalcTimeStr(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetRFactorStr(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetAbsRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetAbsRFactorStr(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetSqrRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetSqrRFactorStr(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.SelectArea(StartPointIndex, StopPointIndex: LongInt): string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SelectArea(StartPointIndex, StopPointIndex, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.ReturnToTotalProfile: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReturnToTotalProfile(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

procedure TFitProblem.CreateSpecimenList;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.CreateSpecimenList(FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToData(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToData(XValue, YValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToBackground(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToBackground(XValue, YValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToRFactorIntervals(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToSpecimenIntervals(XValue, YValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToCurvePositions(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToSpecimenPositions(XValue, YValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInProfile(
        PrevXValue, PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInSpecimenIntervals(
        PrevXValue, PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInSpecimenPositions(
        PrevXValue, PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

begin
    SYNAPSE_RegisterTCP_Transport();
    SYNAPSE_RegisterHTTP_Transport();
end.



