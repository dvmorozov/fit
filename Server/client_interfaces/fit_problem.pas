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
    base_service_intf,
    Classes, curve_points_set,
    fit_server_proxy                //  XML-RPC interface to server.
    , int_fit_server, int_fit_service,
    int_points_set,
    mscr_specimen_list,
    MyExceptions, named_points_set, points_set, self_copied_component,
    SysUtils, title_points_set;

type
    { Converts error codes back into exceptions to provide
      centralized error handling in the client. Converts data
      to appropriate type. Should implement the same interface
      as the server. }
    TFitProblem = class(TInterfacedObject, IFitProblem)
    protected
        FProblemId: longint;        //  For now only single problem id is
        //  supported per client.
        FFitStub:   IFitServer;       //  Access the server
        //  via XML-RPC (wst-5.0)

        { Getting / setting server attributes. }

        function GetMaxRFactor: double;
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetBackFactor: double;
        procedure SetBackFactor(ABackFactor: double);
        function GetCurveThresh: double;
        procedure SetCurveThresh(ACurveThresh: double);
        function GetCurveType: TCurveTypeId;
        procedure SetCurveType(ACurveType: TCurveTypeId);
        function GetState: TFitServerState;
        procedure SetWaveLength(AWaveLength: double);
        function GetWaveLength: double;

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

        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet;
{$IFNDEF EXCLUDE_SOMETHING}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means initialization. }
            CP: Curve_parameters);
{$ENDIF}
        procedure AddPointToData(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        procedure ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);

        function GetCurveList: TMSCRCurveList;
        function GetCurveCount: longint;
        function GetCurveParameterCount(SpecIndex: longint): longint;
        procedure GetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        procedure SetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            Value: double);
        function GetCurvesList: TSelfCopiedCompList;

        function GetCalcProfilePointsSet: TTitlePointsSet;
        function GetDeltaProfilePointsSet: TTitlePointsSet;

        { Asynchronous (long) methods. }

        function SmoothProfile: string;
        procedure SubtractBackground(Auto: boolean);
        function DoAllAutomatically: string;
        function MinimizeDifference: string;
{$IFNDEF EXCLUDE_SOMETHING}
        function MinimizeDifferenceAgain: string;
{$ENDIF}
        function MinimizeNumberOfCurves: string;
        function ComputeCurveBounds: string;
        function ComputeBackgroundPoints: string;
        function ComputeCurvePositions: string;
        function SelectAllPointsAsCurvePositions: string;
{$IFDEF FITCGI}
        function GetGraph(const Width: longint;
            const Height: longint): TMemoryStream;
        function GetProfileChunk(const ChunkNum: longint): TTitlePointsSet;
        function GetProfileChunkCount: longint;
{$ENDIF}
        { Control methods. }

        procedure StopAsyncOper;
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;

        { Synchronous methods. }

        function SelectArea(StartPointIndex, StopPointIndex: longint): string;
        function ReturnToTotalProfile: string;
        procedure CreateCurveList;

        { Should be called in appropriate server state. }
        procedure CreateProblem;
        function GetProblemId: longint;
        procedure SetProblemId(AProblemId: longint);
        procedure DiscardProblem(AProblemId: longint);

        property MaxRFactor: double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: double read GetBackFactor write SetBackFactor;
        property CurveThresh: double read GetCurveThresh write SetCurveThresh;
        property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;
        property State: TFitServerState read GetState;
        property WaveLength: double read GetWaveLength write SetWaveLength;
        property FitStub: IFitServer read FFitStub write FFitStub;
    end;

implementation

uses app, binary_formatter, fit_server_aux,
    { This module contains global definitions which are used in all applications. }
    soap_formatter,
    synapse_http_protocol, synapse_tcp_protocol;

{========================== TFitClientProxy ===================================}

constructor TFitProblem.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    FFitStub :=
        TFitServer_Proxy.Create('IFitServer', 'binary:', 'TCP:Address=' +
        InternalIP + ';Port=' + InternalPort + ';target=IFitServer');
end;

procedure TFitProblem.CreateProblem;
begin
    FProblemId := FitStub.CreateProblem;
end;

procedure TFitProblem.DiscardProblem(AProblemId: longint);
begin
    FitStub.DiscardProblem(AProblemId);
end;

function TFitProblem.GetProblemId: longint;
begin
    Result := FProblemId;
end;

procedure TFitProblem.SetProblemId(AProblemId: longint);
begin
    FProblemId := AProblemId;
end;

function TFitProblem.GetMaxRFactor: double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetMaxRFactor(FProblemId);
end;

procedure TFitProblem.SetMaxRFactor(AMaxRFactor: double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetMaxRFactor(AMaxRFactor, FProblemId);
end;

function TFitProblem.GetBackFactor: double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetBackFactor(FProblemId);
end;

procedure TFitProblem.SetBackFactor(ABackFactor: double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetBackFactor(ABackFactor, FProblemId);
end;

function TFitProblem.GetCurveThresh: double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetCurveThresh(FProblemId);
end;

procedure TFitProblem.SetCurveThresh(ACurveThresh: double);
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
procedure TFitProblem.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
    //  pervonachal'nuyu initsializatsiyu
    );
var
    Res:    longint;
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

procedure TFitProblem.SetWaveLength(AWaveLength: double);
begin
    Assert(Assigned(FitStub));
    FitStub.SetWaveLength(AWaveLength, FProblemId);
end;

function TFitProblem.GetWaveLength: double;
begin
    Assert(Assigned(FitStub));
    Result := FitStub.GetWaveLength(FProblemId);
end;

function TFitProblem.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
var
    Res:    longint;
    ErrMsg: string;
    ADR:    TArrayOfFloatDoubleRemotable;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR    := CreateRemotableArray(APointsSet);
    try
        R := FitStub.SetProfilePointsSet(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
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
var
    Res:    longint;
    ErrMsg: string;
    ADR:    TArrayOfFloatDoubleRemotable;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR    := CreateRemotableArray(ABackgroundPoints);
    try
        R := FitStub.SetBackgroundPointsSet(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
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
var
    Res:    longint;
    ErrMsg: string;
    ADR:    TArrayOfFloatDoubleRemotable;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR    := CreateRemotableArray(ACurvePositions);
    try
        R := FitStub.SetCurvePositions(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    Res    := R.ErrCode;
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
    Result := ProcessPointsResult(FitStub.GetCurvePositions(FProblemId));
end;

function TFitProblem.SetRFactorBounds(ARFactorBounds: TPointsSet): string;
var
    Res:    longint;
    ErrMsg: string;
    ADR:    TArrayOfFloatDoubleRemotable;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    //  ob'ekt, peredannyi cherez parametr ne dolzhen osvobozhdat'sya!
    ADR    := CreateRemotableArray(ARFactorBounds);
    try
        R := FitStub.SetCurveBounds(ADR, FProblemId);
    finally
        ADR.Free;
    end;
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.GetRFactorBounds: TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetCurveBounds(FProblemId));
end;

{$IFDEF FITCGI}
function TFitProblem.GetGraph(const Width: longint;
    const Height: longint): TMemoryStream;
var
    R: TPictureResult;
    i: longint;
    B: byte;
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
        Result.Free;
        Result := nil;
        raise;
    end;
end;

function TFitProblem.GetProfileChunk(const ChunkNum: longint): TTitlePointsSet;
begin
    Assert(Assigned(FitStub));
    Result := ProcessPointsResult(FitStub.GetProfileChunk(FProblemId, ChunkNum));
end;

function TFitProblem.GetProfileChunkCount: longint;
var
    Count: TIntResult;
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
var
    Res:    longint;
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

function TFitProblem.GetCurveCount: longint;
var
    Res:    longint;
    ErrMsg: string;
    Count:  TIntResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    Count  := FitStub.GetCurveCount(FProblemId);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res    := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.GetCurveParameterCount(SpecIndex: longint): longint;
var
    Res:    longint;
    ErrMsg: string;
    Count:  TIntResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    Count  := FitStub.GetCurveParameterCount(FProblemId, SpecIndex);
    if not Assigned(Count) then
        raise Exception.Create(OutOfServerResources);

    Res    := Count.ErrCode;
    ErrMsg := Count.ErrMsg;
    Result := Count._Result;
    Count.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.GetCurveParameter(SpecIndex: longint;
    ParamIndex: longint; var Name: string; var Value: double; var Type_: longint);
var
    Res:    longint;
    ErrMsg: string;
    R:      TSpecParamResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    R      := FitStub.GetCurveParameter(FProblemId, SpecIndex, ParamIndex);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;

    Name  := R.Name;
    Value := R.Value;
    Type_ := R._Type;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.SetCurveParameter(SpecIndex: longint;
    ParamIndex: longint; Value: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    R      := FitStub.SetCurveParameter(FProblemId, SpecIndex, ParamIndex, Value);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);

    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.GetCurveList: TMSCRCurveList;
var
    Res:    longint;
    ErrMsg: string;
    Count:  TIntResult;
    SpecIndex, SpecCount: longint;
    ParamIndex, ParamCount: longint;
    R:      TSpecParamResult;
    CP:     Curve_parameters;
    P:      TSpecialCurveParameter;
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    Result := TMSCRCurveList.Create(nil);
    try
        Count := FitStub.GetCurveCount(FProblemId);
        if not Assigned(Count) then
            raise Exception.Create(OutOfServerResources);

        Res    := Count.ErrCode;
        ErrMsg := Count.ErrMsg;
        SpecCount := Count._Result;
        Count.Free;
        case Res of
            -1: raise EUserException.Create(ErrMsg);
            -2: raise Exception.Create(ErrMsg);
        end;

        for SpecIndex := 0 to SpecCount - 1 do
        begin
            Count := FitStub.GetCurveParameterCount(FProblemId, SpecIndex);
            if not Assigned(Count) then
                raise Exception.Create(OutOfServerResources);

            Res    := Count.ErrCode;
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
                    R := FitStub.GetCurveParameter(
                        FProblemId, SpecIndex, ParamIndex);
                    if not Assigned(R) then
                        raise Exception.Create(OutOfServerResources);
                    try
                        Res    := R.ErrCode;
                        ErrMsg := R.ErrMsg;
                        case Res of
                            -1: raise EUserException.Create(ErrMsg);
                            -2: raise Exception.Create(ErrMsg);
                        end;

                        P      := TSpecialCurveParameter(CP.Params.Add);
                        P.Name := R.Name;
                        P.Value := R.Value;
                        P.Type_ := TParameterType(R._Type);
                    finally
                        R.Free;
                    end;
                end;
                Result.Add(CP);
            except
                CP.Free;
                raise;
            end;
        end;
    except
        Result.Free;
        Result := nil;
        raise;
    end;
end;

function TFitProblem.GetCurvesList: TSelfCopiedCompList;
var
    Res:    longint;
    ErrMsg: string;
    SpecCount: TIntResult;
    SpecIndex, Count: longint;
    Points: TNamedPointsSet;
    R:      TNamedPointsResult;
begin
    Result := nil;  //  chtoby ne bylo warning'a
    Assert(Assigned(FitStub));
    Res    := 0;
    ErrMsg := '';
    Result := TSelfCopiedCompList.Create(nil);
    try
        SpecCount := FitStub.GetCurveCount(FProblemId);
        if not Assigned(SpecCount) then
            raise Exception.Create(OutOfServerResources);

        Res    := SpecCount.ErrCode;
        ErrMsg := SpecCount.ErrMsg;
        Count  := SpecCount._Result;
        SpecCount.Free;
        case Res of
            -1: raise EUserException.Create(ErrMsg);
            -2: raise Exception.Create(ErrMsg);
        end;

        for SpecIndex := 0 to Count - 1 do
        begin
            R := FitStub.GetCurvePoints(SpecIndex, FProblemId);
            if not Assigned(R) then
                raise Exception.Create(OutOfServerResources);

            try
                Res    := R.ErrCode;
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
        Result.Free;
        Result := nil;
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
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SmoothProfile(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

procedure TFitProblem.SubtractBackground(Auto: boolean);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SubtractBackground(Auto, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.DoAllAutomatically: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.DoAllAutomatically(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.MinimizeDifference: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeDifference(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$IFNDEF EXCLUDE_SOMETHING}
function TFitProblem.MinimizeDifferenceAgain: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeDifferenceAgain;
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$ENDIF}

function TFitProblem.MinimizeNumberOfCurves: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.MinimizeNumberOfCurves(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.ComputeCurveBounds: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ComputeCurveBounds(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.ComputeBackgroundPoints: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ComputeBackgroundPoints(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.ComputeCurvePositions: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ComputeCurvePositions(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$IF DEFINED(FIT) OR DEFINED(FITPRO) OR DEFINED(FITCGI)}
function TFitProblem.SelectAllPointsAsCurvePositions: string;
var
    Res:    longint;
    ErrMsg: string;
begin
    Assert(Assigned(FitStub));
    Res := FitStub.SelectAllPointsAsCurvePositions(ErrMsg);
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

{$ENDIF}

procedure TFitProblem.StopAsyncOper;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.StopAsyncOper(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

function TFitProblem.AsyncOper: boolean;
var
    ErrMsg: string;
    R:      TBoolResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AsyncOper(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetCalcTimeStr: string;
var
    ErrMsg: string;
    R:      TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetCalcTimeStr(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetRFactorStr: string;
var
    ErrMsg: string;
    R:      TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetRFactorStr(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetAbsRFactorStr: string;
var
    ErrMsg: string;
    R:      TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetAbsRFactorStr(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.GetSqrRFactorStr: string;
var
    ErrMsg: string;
    R:      TStringResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.GetSqrRFactorStr(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Result := R._Result;
    R.Free;
end;

function TFitProblem.SelectArea(StartPointIndex, StopPointIndex: longint): string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.SelectArea(StartPointIndex, StopPointIndex, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

function TFitProblem.ReturnToTotalProfile: string;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReturnToTotalProfile(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
    Result := ErrMsg;
end;

procedure TFitProblem.CreateCurveList;
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.CreateCurveList(FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToData(XValue, YValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToData(XValue, YValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToBackground(XValue, YValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToBackground(XValue, YValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToRFactorBounds(XValue, YValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToCurveBounds(XValue, YValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.AddPointToCurvePositions(XValue, YValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.AddPointToCurvePositions(XValue, YValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInProfile(PrevXValue, PrevYValue,
        NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInBackground(PrevXValue, PrevYValue,
        NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInRFactorBounds(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInCurveBounds(PrevXValue,
        PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
    ErrMsg := R.ErrMsg;
    R.Free;
    case Res of
        -1: raise EUserException.Create(ErrMsg);
        -2: raise Exception.Create(ErrMsg);
    end;
end;

procedure TFitProblem.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
var
    Res:    longint;
    ErrMsg: string;
    R:      TResult;
begin
    Assert(Assigned(FitStub));
    R := FitStub.ReplacePointInCurvePositions(PrevXValue,
        PrevYValue, NewXValue, NewYValue, FProblemId);
    if not Assigned(R) then
        raise Exception.Create(OutOfServerResources);
    Res    := R.ErrCode;
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
