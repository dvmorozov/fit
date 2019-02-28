{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of proxy class calling server part by different ways depending on compilation keys.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit FitClientProxy;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, MSCRDataClasses, CommonTypes, PointsSet, SelfCopied,
    MyExceptions, TitlePointsSet, CurvePointsSet, NamedPointsSet,
{$IFNDEF FIT}
    base_service_intf,
    fit_server_proxy                //  Calls the server via network.
{$ELSE}
    FitServerStub                   //  Calls the server directly.
{$ENDIF}
    , fit_server
    ;
  
type
{$IFNDEF FIT}
    TFitServerStub = IFitServer;    //  TFitServerStub is substituted by
                                    //  TFitClientProxy to access the server
                                    //  via network.
{$ENDIF}

{$IFDEF FITPRO} {$DEFINE USE_RESULT_PROCESSING} {$ENDIF}
{$IFDEF FITCGI} {$DEFINE USE_RESULT_PROCESSING} {$ENDIF}
{$IFDEF FITCLIENTINSERVER} {$DEFINE USE_RESULT_PROCESSING} {$ENDIF}     //  TODO: remove client functions from the server
    { Converts error codes back into exceptions to provide 
      centralized error handling in the client. Converts data 
      to appropriate type. Should impelement the same interface 
      as the server. }
    TFitClientProxy = class(TObject)
    protected
        FFitStub: TFitServerStub;
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
{$IFDEF USE_RESULT_PROCESSING}
        { Throws an exception when R = nil. }
        function ProcessPointsResult(R: TPointsResult): TTitlePointsSet;
{$ENDIF}
    public
        { GetXXXX methods create and return A NEW OBJECT, 
          responsibility to free it is put on calling code. }
        
        procedure SetProfilePointsSet(APointsSet: TTitlePointsSet);
        function GetProfilePointsSet: TTitlePointsSet;
        function GetSelectedArea: TTitlePointsSet;

        procedure SetBackgroundPointsSet(ABackgroundPoints: TTitlePointsSet);
        function GetBackgroundPoints: TTitlePointsSet;

        procedure SetCurvePositions(ACurvePositions: TPointsSet);
        function GetCurvePositions: TTitlePointsSet;

        procedure SetRFactorIntervals(ARFactorIntervals: TPointsSet);
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
        
        procedure SmoothProfile;
        procedure SubtractAllBackground(Auto: Boolean);
        procedure DoAllAutomatically;
        procedure FindGausses;
{$IFNDEF EXCLUDE_SOMETHING}
        procedure FindGaussesAgain;
{$ENDIF}
        procedure FindGaussesSequentially;
        procedure FindPeakBounds;
        procedure FindBackPoints;
        procedure FindPeakPositions;
{$IFDEF FIT}
        procedure AllPointsAsPeakPositions;
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
        
        procedure SelectArea(StartPointIndex, StopPointIndex: LongInt);
        procedure ReturnToTotalProfile;
        procedure CreateSpecimenList;

        property MaxRFactor: Double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: Double read GetBackFactor write SetBackFactor;
        property CurveThresh: Double read GetCurveThresh write SetCurveThresh;
        property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;
        property State: TFitServerState read GetState;
        property WaveLength: Double read GetWaveLength write SetWaveLength;

        property FitStub: TFitServerStub read FFitStub write FFitStub;
    end;

implementation

uses Main;

const OutOfServerResources: string = 'Out of server resources.';

{========================== TFitClientProxy ===================================}
function TFitClientProxy.GetMaxRFactor: Double;
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := FitStub.GetMaxRFactor(ProblemID);
{$ELSE}
    Result := FitStub.GetMaxRFactor;
{$ENDIF}
end;

procedure TFitClientProxy.SetMaxRFactor(AMaxRFactor: Double);
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    FitStub.SetMaxRFactor(AMaxRFactor, ProblemID);
{$ELSE}
    FitStub.SetMaxRFactor(AMaxRFactor);
{$ENDIF}
end;

function TFitClientProxy.GetBackFactor: Double;
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := FitStub.GetBackFactor(ProblemID);
{$ELSE}
    Result := FitStub.GetBackFactor;
{$ENDIF}
end;

procedure TFitClientProxy.SetBackFactor(ABackFactor: Double);
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    FitStub.SetBackFactor(ABackFactor, ProblemID);
{$ELSE}
    FitStub.SetBackFactor(ABackFactor);
{$ENDIF}
end;

function TFitClientProxy.GetCurveThresh: Double;
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := FitStub.GetCurveThresh(ProblemID);
{$ELSE}
    Result := FitStub.GetCurveThresh;
{$ENDIF}
end;

procedure TFitClientProxy.SetCurveThresh(ACurveThresh: Double);
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    FitStub.SetCurveThresh(ACurveThresh, ProblemID);
{$ELSE}
    FitStub.SetCurveThresh(ACurveThresh);
{$ENDIF}
end;

function TFitClientProxy.GetCurveType: TCurveTypeId;
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := TCurveTypeId(FitStub.GetCurveType(ProblemID));
{$ELSE}
    Result := TCurveTypeId(FitStub.GetCurveType);
{$ENDIF}
end;

procedure TFitClientProxy.SetCurveType(ACurveType: TCurveTypeId);
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    FitStub.SetCurveType(LongInt(ACurveType), ProblemID);
{$ELSE}
    FitStub.SetCurveType(ACurveType);
{$ENDIF}
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
{$IFNDEF FIT}
    Result := TFitServerState(FitStub.GetState(ProblemID));
{$ELSE}
    Result := TFitServerState(FitStub.GetState);
{$ENDIF}
end;

procedure TFitClientProxy.SetWaveLength(AWaveLength: Double);
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    FitStub.SetWaveLength(AWaveLength, ProblemID);
{$ELSE}
    FitStub.SetWaveLength(AWaveLength);
{$ENDIF}
end;

function TFitClientProxy.GetWaveLength: Double;
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := FitStub.GetWaveLength(ProblemID);
{$ELSE}
    Result := FitStub.GetWaveLength;
{$ENDIF}
end;

procedure TFitClientProxy.SetProfilePointsSet(APointsSet: TTitlePointsSet);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
{$ELSE}
    Res := FitStub.SetProfilePointsSet(APointsSet, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

{$IFDEF USE_RESULT_PROCESSING}
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
{$ENDIF}

function TFitClientProxy.GetProfilePointsSet: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    ErrMsg: string;
    Points: TPointsSet;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetProfilePointsSet(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetProfilePointsSet(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: begin Result.Free; raise EUserException.Create(ErrMsg); end;
        -2: begin Result.Free; raise Exception.Create(ErrMsg); end;
    end;
{$ENDIF}
end;

function TFitClientProxy.GetSelectedArea: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Result := ProcessPointsResult(FitStub.GetSelectedArea(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    Res := FitStub.GetSelectedArea(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: begin Result.Free; raise EUserException.Create(ErrMsg); end;
        -2: begin Result.Free; raise Exception.Create(ErrMsg); end;
    end;
{$ENDIF}
end;

procedure TFitClientProxy.SetBackgroundPointsSet(
    ABackgroundPoints: TTitlePointsSet);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
{$ELSE}
    Res := FitStub.SetBackgroundPointsSet(ABackgroundPoints, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetBackgroundPoints: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetBackgroundPoints(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetBackgroundPoints(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
{$ENDIF}
end;

procedure TFitClientProxy.SetCurvePositions(ACurvePositions: TPointsSet);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
{$ELSE}
    Res := FitStub.SetCurvePositions(ACurvePositions, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetCurvePositions: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetSpecimenPositions(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetCurvePositions(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
{$ENDIF}
end;

procedure TFitClientProxy.SetRFactorIntervals(ARFactorIntervals: TPointsSet);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    ADR: TArrayOfFloatDoubleRemotable;
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
{$ELSE}
    Res := FitStub.SetRFactorIntervals(ARFactorIntervals, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetRFactorIntervals: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetSpecimenIntervals(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetRFactorIntervals(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
{$ENDIF}
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
{$IFNDEF FIT}
    Count: TIntResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
    Count := FitStub.GetSpecimenCount(ProblemID);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
{$ELSE}
    Res := FitStub.GetSpecimenCount(Result, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetSpecimenParameterCount(SpecIndex: LongInt): LongInt;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    Count: TIntResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
    Count := FitStub.GetSpecimenParameterCount(ProblemID, SpecIndex);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
{$ELSE}
    Res := FitStub.GetSpecimenParameterCount(SpecIndex, Result, ErrMsg);
{$ENDIF}
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
{$IFNDEF FIT}
    R: TSpecParamResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
    R := FitStub.GetSpecimenParameter(
        ProblemID, SpecIndex, ParamIndex);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;

    Name := R.Name;
    Value := R.Value;
    Type_ := R._Type;
{$ELSE}
    Res := FitStub.GetSpecimenParameter(
        SpecIndex, ParamIndex, Name, Value, Type_, ErrMsg);
{$ENDIF}
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
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
    R := FitStub.SetSpecimenParameter(
        ProblemID, SpecIndex, ParamIndex, Value);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
{$ELSE}
    Res := FitStub.SetSpecimenParameter(
        SpecIndex, ParamIndex, Value, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.GetSpecimenList: TMSCRSpecimenList;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    Count: TIntResult;
    SpecIndex, SpecCount: LongInt;
    ParamIndex, ParamCount: LongInt;
    R: TSpecParamResult;
    CP: Curve_parameters;
    P: TSpecialCurveParameter;
{$ENDIF}
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
{$ELSE}
    Res := FitStub.GetSpecimenList(Result, ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := TMSCRSpecimenList(Result.GetCopy);
{$ENDIF}
end;

function TFitClientProxy.GetCurvesList: TSelfCopiedCompList;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    SpecCount: TIntResult;
    SpecIndex, Count: LongInt;
    Points: TNamedPointsSet;
    R: TNamedPointsResult;
{$ENDIF}
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res := 0; ErrMsg := '';
{$IFNDEF FIT}
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
                Points.SetTypeName(R.Name);
            finally
                R.Free;
            end;
        end;
    except
        Result.Free; Result := nil;
        raise;
    end;
{$ELSE}
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetCurvesList(Result, ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := TSelfCopiedCompList(Result.GetCopy);
{$ENDIF}
end;

function TFitClientProxy.GetCalcProfilePointsSet: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetCalcProfilePointsSet(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetCalcProfilePointsSet(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
{$ENDIF}
end;

function TFitClientProxy.GetDeltaProfilePointsSet: TTitlePointsSet;
{$IFDEF FIT}
var Res: LongInt;
    Points: TPointsSet;
    ErrMsg: string;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    Result := ProcessPointsResult(FitStub.GetDeltaProfilePointsSet(ProblemID));
{$ELSE}
    Points := nil;  //  chtoby ne bylo warning'a
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    Res := FitStub.GetDeltaProfilePointsSet(Points, ErrMsg);
    if Assigned(Points) then
        Result := TTitlePointsSet.CreateFromPoints(nil, Points)
    else Result := nil;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
{$ENDIF}
end;

procedure TFitClientProxy.SmoothProfile;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.SmoothProfile(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.SmoothProfile(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.SubtractAllBackground(Auto: Boolean);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.SubtractAllBackground(Auto, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.SubtractAllBackground(Auto, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.DoAllAutomatically;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.DoAllAutomatically(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.DoAllAutomatically(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.FindGausses;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.MinimizeDifference(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindGausses(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

{$IFNDEF EXCLUDE_SOMETHING}
procedure TFitClientProxy.FindGaussesAgain;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.FindGaussesAgain;
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindGaussesAgain(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;
{$ENDIF}

procedure TFitClientProxy.FindGaussesSequentially;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.MinimizeNumberOfSpecimens(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindGaussesSequentially(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.FindPeakBounds;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.FindSpecimenIntervals(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindPeakBounds(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.FindBackPoints;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.FindBackPoints(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindBackPoints(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.FindPeakPositions;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.FindSpecimenPositions(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.FindPeakPositions(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

{$IFDEF FIT}
procedure TFitClientProxy.AllPointsAsPeakPositions;
var Res: LongInt;
    ErrMsg: string;
begin
    Assert(Assigned(FitStub));
    Res := FitStub.AllPointsAsPeakPositions(ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;
{$ENDIF}

procedure TFitClientProxy.StopAsyncOper;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.StopAsyncOper(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.StopAsyncOper(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitClientProxy.AsyncOper: Boolean;
var ErrMsg: string;
{$IFNDEF FIT}
    R: TBoolResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.AsyncOper(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
{$ELSE}
    Result := FitStub.AsyncOper(ErrMsg);
{$ENDIF}
end;

function TFitClientProxy.GetCalcTimeStr: string;
var ErrMsg: string;
{$IFNDEF FIT}
    R: TStringResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.GetCalcTimeStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
{$ELSE}
    Result := FitStub.GetCalcTimeStr(ErrMsg);
{$ENDIF}
end;

function TFitClientProxy.GetRFactorStr: string;
var ErrMsg: string;
{$IFNDEF FIT}
    R: TStringResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.GetRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
{$ELSE}
    Result := FitStub.GetRFactorStr(ErrMsg);
{$ENDIF}
end;

function TFitClientProxy.GetAbsRFactorStr: string;
var ErrMsg: string;
{$IFNDEF FIT}
    R: TStringResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.GetAbsRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
{$ELSE}
    Result := FitStub.GetAbsRFactorStr(ErrMsg);
{$ENDIF}
end;

function TFitClientProxy.GetSqrRFactorStr: string;
var ErrMsg: string;
{$IFNDEF FIT}
    R: TStringResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.GetSqrRFactorStr(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
{$ELSE}
    Result := FitStub.GetSqrRFactorStr(ErrMsg);
{$ENDIF}
end;

procedure TFitClientProxy.SelectArea(StartPointIndex, StopPointIndex: LongInt);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.SelectArea(StartPointIndex, StopPointIndex, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.SelectArea(StartPointIndex, StopPointIndex, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReturnToTotalProfile;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.ReturnToTotalProfile(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.ReturnToTotalProfile(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.CreateSpecimenList;
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.CreateSpecimenList(ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.CreateSpecimenList(ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToData(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.AddPointToData(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.AddPointToData(XValue, YValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToBackground(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.AddPointToBackground(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.AddPointToBackground(XValue, YValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToRFactorIntervals(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.AddPointToSpecimenIntervals(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.AddPointToRFactorIntervals(XValue, YValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.AddPointToCurvePositions(XValue, YValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.AddPointToSpecimenPositions(XValue, YValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.AddPointToCurvePositions(XValue, YValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.ReplacePointInProfile(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.ReplacePointInData(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.ReplacePointInSpecimenIntervals(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.ReplacePointInRFactorIntervals(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitClientProxy.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var Res: LongInt;
    ErrMsg: string;
{$IFNDEF FIT}
    R: TResult;
{$ENDIF}
begin
    Assert(Assigned(FitStub));
{$IFNDEF FIT}
    R := FitStub.ReplacePointInSpecimenPositions(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ProblemID);
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);
    Res := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
{$ELSE}
    Res := FitStub.ReplacePointInCurvePositions(
        PrevXValue, PrevYValue, NewXValue, NewYValue, ErrMsg);
{$ENDIF}
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

end.



