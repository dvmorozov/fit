{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of proxy class calling server by means of XML-RPC.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_client_proxy;

{$MODE Delphi}

interface

uses
    SysUtils, MSCRDataClasses, CommonTypes, PointsSet, SelfCopied, Classes,
    MyExceptions, IntPointsSet, TitlePointsSet, CurvePointsSet, CBRCComponent,
    int_fit_service, fit_server, NamedPointsSet,
    base_service_intf,
    fit_server_proxy                //  Calls the server via network.
    ;

type
    { Converts error codes back into exceptions to provide
      centralized error handling in the client. Converts data
      to appropriate type. Should implement the same interface
      as the server. }
    TFitClientProxy = class(TCBRCComponent, IFitService)
    protected
        FProblemId: LongInt;        //  For now only single problem id is
                                    //  supported per client.
        FFitStub: IFitServer;       //  Access the server
                                    //  via XMP-RPC (wst-5.0)
                                    
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
        { Throws an exception when R = nil. }
        function ProcessPointsResult(R: TPointsResult): TTitlePointsSet;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        
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
        { Passed via URL parameter for CGI application, should be writeable. }
        property ProblemId: LongInt read FProblemId write FProblemId;
    end;

implementation
uses synapse_tcp_protocol, synapse_http_protocol, soap_formatter,
    binary_formatter,
    { This module contains global definitions
      which are used in all applications. }
    Main;

{$INCLUDE wst.inc}

const OutOfServerResources: string = 'Out of server resources.';

function CreateRemotableArray(
    APointsSet: TPointsSet): TArrayOfFloatDoubleRemotable;
var i, Count: LongInt;
begin
    Result := nil;
    if not Assigned(APointsSet) then Exit;

    Result := TArrayOfFloatDoubleRemotable.Create;
    //  tochki zagruzhayutsya poparno
    Count := APointsSet.PointsCount * 2;
    Result.SetLength(Count);
    i := 0;
    while i < Count do
    begin
        Result.Item[i] := APointsSet.PointXCoord[i div 2];
        Inc(i);
        Result.Item[i] := APointsSet.PointYCoord[i div 2];
        Inc(i);
    end;
end;

function CreateNamedPointsSet(
    ARemotable: TArrayOfFloatDoubleRemotable): TNamedPointsSet;
var i: LongInt;
    X, Y: Double;
begin
    //  trebuetsya dopustit' ravenstvo nil
    Result := nil;
    if not Assigned(ARemotable) then Exit;

    Assert(ARemotable.Length mod 2 = 0);
    Result := TNamedPointsSet.Create(nil);

    i := 0;
    while i < ARemotable.Length do
    begin
        X := ARemotable.Item[i];
        Inc(i);
        Y := ARemotable.Item[i];
        Inc(i);
        Result.AddNewPoint(X, Y);
    end;
end;

{========================== TFitClientProxy ===================================}

constructor TFitClientProxy.Create(AOwner: TComponent);
begin
    FFitStub :=
        TFitServer_Proxy.Create(
            'IFitServer',
            'binary:',
            'TCP:Address=' + InternalIP +
            ';Port=' + InternalPort + ';target=IFitServer'
            );
end;

destructor TFitClientProxy.Destroy;
begin
    DiscardProblem(FProblemId);
end;

procedure TFitClientProxy.CreateProblem;
begin
    FProblemId := FitStub.CreateProblem;
end;

procedure TFitClientProxy.DiscardProblem(AProblemId: LongInt);
begin
    FitStub.DiscardProblem(AProblemId);
end;

function TFitClientProxy.GetProblemId: LongInt;
begin
    Result := FProblemId;
end;

procedure TFitClientProxy.SetProblemId(AProblemId: LongInt);
begin
    FProblemId := AProblemId;
end;

function TFitClientProxy.GetMaxRFactor: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetMaxRFactor(ProblemID);
end;

procedure TFitClientProxy.SetMaxRFactor(AMaxRFactor: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetMaxRFactor(AMaxRFactor, ProblemID);
end;

function TFitClientProxy.GetBackFactor: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetBackFactor(ProblemID);
end;

procedure TFitClientProxy.SetBackFactor(ABackFactor: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetBackFactor(ABackFactor, ProblemID);
end;

function TFitClientProxy.GetCurveThresh: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetCurveThresh(ProblemID);
end;

procedure TFitClientProxy.SetCurveThresh(ACurveThresh: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetCurveThresh(ACurveThresh, ProblemID);
end;

function TFitClientProxy.GetCurveType: TCurveTypeId;
begin
    Assert(Assigned(FitStub));
    Result := TCurveTypeId(FitStub.GetCurveType(ProblemID));
end;

procedure TFitClientProxy.SetCurveType(ACurveType: TCurveTypeId);
begin
    Assert(Assigned(FitStub));
    FitStub.SetCurveType(ACurveType, ProblemID);
end;

{$IFNDEF EXCLUDE_SOMETHING}
procedure TFitClientProxy.SetSpecialCurveParameters(
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

function TFitClientProxy.GetState: TFitServerState;
begin
    Assert(Assigned(FitStub));
    Result := TFitServerState(FitStub.GetState(ProblemID));
end;

procedure TFitClientProxy.SetWaveLength(AWaveLength: Double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetWaveLength(AWaveLength, ProblemID);
end;

function TFitClientProxy.GetWaveLength: Double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetWaveLength(ProblemID);
end;

function TFitClientProxy.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
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
        R := FitStub.SetProfilePointsSet(ADR, ProblemID);
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

function TFitClientProxy.ProcessPointsResult(R: TPointsResult): TTitlePointsSet;
var Points: TPointsSet;
    Res: LongInt;
    ErrMsg: string;
begin
    Result := nil; Res := 0; ErrMsg := '';
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);

    try
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        Points := CreateNamedPointsSet(R._Result);
        try
            Res := R.ErrCode;
            ErrMsg := R.ErrMsg;
            if Assigned(Points) then
                Result := TTitlePointsSet.CreateFromPoints(nil, Points)
            else Result := nil;
        finally
            Points.Free;
        end;
    finally
        R.Free;
    end;

    case Res of
        -1: begin Result.Free; raise EUserException.Create(ErrMsg); end;
        -2: begin Result.Free; raise Exception.Create(ErrMsg); end;
    end;
end;

function TFitClientProxy.GetProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetProfilePointsSet(ProblemID));
end;

function TFitClientProxy.GetSelectedArea: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSelectedArea(ProblemID));
end;

function TFitClientProxy.SetBackgroundPointsSet(
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
        R := FitStub.SetBackgroundPointsSet(ADR, ProblemID);
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

function TFitClientProxy.GetBackgroundPoints: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetBackgroundPoints(ProblemID));
end;

function TFitClientProxy.SetCurvePositions(ACurvePositions: TPointsSet): string;
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
        R := FitStub.SetSpecimenPositions(ADR, ProblemID);
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

function TFitClientProxy.GetCurvePositions: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSpecimenPositions(ProblemID));
end;

function TFitClientProxy.SetRFactorIntervals(ARFactorIntervals: TPointsSet): string;
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
        R := FitStub.SetSpecimenIntervals(ADR, ProblemID);
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

function TFitClientProxy.GetRFactorIntervals: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetSpecimenIntervals(ProblemID));
end;

{$IFDEF FITCGI}
function TFitClientProxy.GetGraph(
    const Width: LongInt; const Height: LongInt): TMemoryStream;
var R: TPictureResult;
    i: LongInt;
    B: Byte;
begin
    Assert(Assigned(FitStub));
    Result := TMemoryStream.Create;
    try
        R := FitStub.GetGraph(Width, Height, ProblemID);
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

function TFitClientProxy.GetProfileChunk(
    const ChunkNum: LongInt): TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetProfileChunk(ProblemID, ChunkNum));
end;

function TFitClientProxy.GetProfileChunkCount: LongInt;
var Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Count := FitStub.GetProfileChunkCount(ProblemID);
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
function TFitClientProxy.GetSpecialCurveParameters: Curve_parameters;
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

function TFitClientProxy.GetSpecimenCount: LongInt;
var Res: LongInt;
    ErrMsg: string;
    Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Count := FitStub.GetSpecimenCount(ProblemID);
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

function TFitClientProxy.GetSpecimenParameterCount(SpecIndex: LongInt): LongInt;
var Res: LongInt;
    ErrMsg: string;
    Count: TIntResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    Count := FitStub.GetSpecimenParameterCount(ProblemID, SpecIndex);
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

procedure TFitClientProxy.GetSpecimenParameter(
    SpecIndex: LongInt; ParamIndex: LongInt;
    var Name: string; var Value: Double; var Type_: LongInt);
var Res: LongInt;
    ErrMsg: string;
    R: TSpecParamResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    R := FitStub.GetSpecimenParameter(
        ProblemID, SpecIndex, ParamIndex);
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

procedure TFitClientProxy.SetSpecimenParameter(
    SpecIndex: LongInt; ParamIndex: LongInt;
    Value: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
    R := FitStub.SetSpecimenParameter(
        ProblemID, SpecIndex, ParamIndex, Value);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetSpecimenList: TMSCRSpecimenList;
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
        Count := FitStub.GetSpecimenCount(ProblemID);
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
            Count := FitStub.GetSpecimenParameterCount(ProblemID, SpecIndex);
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
                        ProblemID, SpecIndex, ParamIndex);
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

function TFitClientProxy.GetCurvesList: TSelfCopiedCompList;
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
        SpecCount := FitStub.GetSpecimenCount(ProblemID);
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
            R := FitStub.GetSpecimenPoints(SpecIndex, ProblemID);
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

function TFitClientProxy.GetCalcProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetCalcProfilePointsSet(ProblemID));
end;

function TFitClientProxy.GetDeltaProfilePointsSet: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetDeltaProfilePointsSet(ProblemID));
end;

function TFitClientProxy.SmoothProfile: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SmoothProfile(ProblemID);
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

procedure TFitClientProxy.SubtractAllBackground(Auto: Boolean);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SubtractAllBackground(Auto, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.DoAllAutomatically: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.DoAllAutomatically(ProblemID);
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

function TFitClientProxy.FindGausses: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeDifference(ProblemID);
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
function TFitClientProxy.FindGaussesAgain: string;
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

function TFitClientProxy.FindGaussesSequentially: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeNumberOfSpecimens(ProblemID);
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

function TFitClientProxy.FindPeakBounds: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindSpecimenIntervals(ProblemID);
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

function TFitClientProxy.FindBackPoints: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindBackPoints(ProblemID);
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

function TFitClientProxy.FindPeakPositions: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.FindSpecimenPositions(ProblemID);
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
function TFitClientProxy.AllPointsAsPeakPositions: string;
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

procedure TFitClientProxy.StopAsyncOper;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.StopAsyncOper(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.AsyncOper: Boolean;
var ErrMsg: string;
    R: TBoolResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AsyncOper(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitClientProxy.GetCalcTimeStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetCalcTimeStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitClientProxy.GetRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitClientProxy.GetAbsRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetAbsRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitClientProxy.GetSqrRFactorStr: string;
var ErrMsg: string;
    R: TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetSqrRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitClientProxy.SelectArea(StartPointIndex, StopPointIndex: LongInt): string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SelectArea(StartPointIndex, StopPointIndex, ProblemID);
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

function TFitClientProxy.ReturnToTotalProfile: string;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReturnToTotalProfile(ProblemID);
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

procedure TFitClientProxy.CreateSpecimenList;
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.CreateSpecimenList(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToData(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToData(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToBackground(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToBackground(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToRFactorIntervals(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToSpecimenIntervals(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToCurvePositions(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToSpecimenPositions(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInProfile(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInSpecimenIntervals(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
    R: TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInSpecimenPositions(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
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



