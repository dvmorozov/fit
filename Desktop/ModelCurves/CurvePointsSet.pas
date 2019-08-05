{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of generic container for point set of all calcuated curves.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CurvePointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, SimpMath, SelfCopied, PointsSet, TitlePointsSet,
  data_loader;

type
    TParameterType = (
        { Created by user and non-variable. Such parameters are variated together for all
        instances in the given interval if it isn't disabled by special flag. }
        Shared,
        { Created by user and variable. }
        Variable,
        { Created by the application. }
        Calculated,
        { Argument of expression. Always variable. }
        Argument,
        { Non-variable parameter describing instance position. }
        InvariablePosition,
        { Variable parameter describing instance position. }
        VariablePosition
        );

    TSpecialCurveParameter = class(TCollectionItem)
    private
        FName: string;
        FValue: Double;
        FType: TParameterType;
        FVariationDisabled: Boolean;
        FVariationStep: Double;

        FSavedValue: Double;

        function GetValue_: string;
        procedure SetValue_(AValue: string);

    public
        constructor Create(Collection: TCollection); override;
        procedure CopyTo(const Dest: TSpecialCurveParameter);

        property SavedValue: Double read FSavedValue write FSavedValue;
        property Value: Double read FValue write FValue;
        property VariationDisabled: Boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: Double
            read FVariationStep write FVariationStep;

    published
        { Published for XML-serialization. }
        property Name: string read FName write FName;
        { String because some problem with XML-serialization as Double. }
        property Value_: string read GetValue_ write SetValue_;
        property Type_: TParameterType read FType write FType;
    end;

    { Generic type of instance parameter container. }
    Curve_parameters = class(TSelfCopiedComponent)
    protected
        FParams: TCollection;

    public
        { Initial parameters hash. Should be used for copying optimization. }
        SavedInitHash: Cardinal;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CopyParameters(const Dest: TObject); override;
        function GetParamValueByName(ParamName: string): double;
        procedure SetParamValueByName(ParamName: string; AValue: double);

    published
        { Published for XML-serialization. }
        property Params: TCollection read FParams write FParams;
    end;

    { Generic container for point set of all calcuated curves. TODO: must
      inherit Lambda from TNeutronPointsSet to adjust calculated and experimental
      curves on chart. }
    TCurvePointsSet = class(TTitlePointsSet)
    protected
        { List of curve parameters. }
        FParams: Curve_parameters;
        { List of variable parameters. }
        Links: TList;
        { Parameters with predefined semantics have constraints, which
          can be associated with curve points. Attributes store pointers
          to parameters with predefined semantics. Parameters are created
          in descendant constructors. }
        AmplitudeP: TSpecialCurveParameter;
        PositionP: TSpecialCurveParameter;
        SigmaP: TSpecialCurveParameter;
        ArgP: TSpecialCurveParameter;

        Fx0IsSet: Boolean;
        { X0 variation boundaries. }
        Fx0Low, Fx0High: Double;

        //  predostavlyayut dostup k var'iruemym parametram
        { Returns variable parameter by through index. }
        function GetParam(Index: LongInt): Double; virtual;
        { Sets value of variable parameter. }
        procedure SetParam(Index: LongInt; Value: Double); virtual;
        { Returns total number of variable parameters. }
        function GetParamCount: LongInt;
        
        { Returns value of parameter with given name. }
        function GetParamByName(Name: string): Double; virtual;
        procedure SetParamByName(Name: string; Value: Double); virtual;
        
        { Initializes pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); virtual;
        { Initializes indexes of variable parameters with predefined semantics. }
        procedure SetSpecParamVarIndex(
            P: TSpecialCurveParameter; Index: LongInt); virtual;

    protected
        Modified: Boolean;

    protected
        { Performs recalculation of all profile points. }
        procedure DoCalc(const Intervals: TPointsSet); virtual; abstract;
        { Multiplies profile points by given factor. }
        procedure ScaleCurve(const Factor: Double);
        { Performs intialization of variable list parameters and 
          set up of AmplIndex, PosIndex, SigmaIndex. }
        procedure InitLinks;
        
        { These functions don't perform profile recalculation and
          are used for initialization purposes (when Modified is set up). }
        
        procedure Setx0(Value: Double);
        procedure SetA(Value: Double);
        procedure SetSigma(Value: Double);
        function Getx0: Double;
        function GetA: Double;
        function GetSigma: Double;

    public
        { Indexes of attributes with predefined semantics. Indexes are 
          filled only in the case if parameters with requirede names are
          variable and are indexes in the List. It's necessary in accessing
          to variable parameters via Param[Index] it would be possible to
          set up parameters with predefined semantics in according to 
          restrictions. Must be public to assign optimization step. }
        { Curve amplitude. }
        AmplIndex: LongInt;
        { Curve position. }
        PosIndex: LongInt;
        { Curve width. }
        SigmaIndex: LongInt;

        { Boundaries for R-factor calculation. }
        MinX, MaxX: Double;
        { Flag designating that boundaries of R-factor calculation
          has been set up. If False R-factor is calculated for all 
          points of profile. }
        RangeDefined: Boolean;
        { Hash of initial parameter values. }
        InitHash: Cardinal;
        { Initial value of x0. It's used in some algorithms. }
        Initx0: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure SetParameters(AParams: Curve_parameters);
        { Set of pairs of indexes of X-coordinates forming intervals in
          which functions should be calcuated (boundaries are included). 
          Equity to nil designates that functions should be calculated 
          for all points. Indexes instead of coordinates are used to 
          avoid searching. }
        procedure ReCalc(const Intervals: TPointsSet);
        { Temporarily stores values of variable parameters in internal memory area. }
        procedure StoreParams;
        { Restores values of variable parameters from temporary storage. }
        procedure RestoreParams;
        procedure CopyParameters(const Dest: TObject); override;

        { Designate initialization of attributes with predefined semantics. }
        
        function Hasx0: Boolean;
        function HasA: Boolean;
        function HasSigma: Boolean;

        { Provides access to variable parameters for optimizer. }
        property Param[index: LongInt]: Double read GetParam write SetParam;
        property ParamCount: LongInt read GetParamCount;
        { Provides access to all parameters by name. }
        property ParamByName[Name: string]: Double read GetParamByName write SetParamByName;
        { Returns object containing all parameters. }
        property Params: Curve_parameters read FParams;
        { Properties provide access to attributes having predefined semantics for special algorithms.
          Use methods HasX before to check existense of such attributes. }

        { Variation of the parameter x0 is limited by two adjacent points. }
        property x0: Double read Getx0 write Setx0;
        property A: Double read GetA write SetA;
        property Sigma: Double read GetSigma write SetSigma;
    end;

implementation

{=========================== TCurvePointsSet ==================================}

constructor TCurvePointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FParams := Curve_parameters.Create(nil);
    Modified := True;
    AmplIndex := -1;
    PosIndex := -1;
    SigmaIndex := -1;
end;

destructor TCurvePointsSet.Destroy;
begin
    Links.Free;
    FParams.Free;
    inherited;
end;

procedure TCurvePointsSet.ReCalc(const Intervals: TPointsSet);
begin
    if Modified then
    begin
        DoCalc(Intervals);
        Modified := False;
    end;
end;

procedure TCurvePointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TCurvePointsSet(Dest).MinX := MinX;
    TCurvePointsSet(Dest).MaxX := MaxX;
    TCurvePointsSet(Dest).RangeDefined := RangeDefined;
    TCurvePointsSet(Dest).SetParameters(Curve_parameters(FParams.GetCopy));
    TCurvePointsSet(Dest).InitHash := InitHash;
end;

function TCurvePointsSet.GetParam(Index: LongInt): Double;
var P: TSpecialCurveParameter;
begin
    Assert(index < GetParamCount);
    
    if Index = PosIndex then Result := x0
    else
    if Index = AmplIndex then Result := A
    else
    if Index = SigmaIndex then Result := Sigma
    else
    begin
        P := TSpecialCurveParameter(Links.Items[index]);
        Result := P.Value;
    end;
end;

procedure TCurvePointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = PosIndex then x0 := Value
    else
    if Index = AmplIndex then A := Value
    else
    if Index = SigmaIndex then Sigma := Value
    else
    begin
        P := TSpecialCurveParameter(Links.Items[Index]);
        P.Value := Value;
    end;
end;

function TCurvePointsSet.GetParamByName(Name: string): Double;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    //  snachala proveryayutsya special'nye imena
    if UpperCase(Name) = 'X0' then
    begin
        Result := x0;
        Exit;
    end
    else
    if UpperCase(Name) = 'A' then
    begin
        Result := A;
        Exit;
    end
    else
    if UpperCase(Name) = 'SIGMA' then
    begin
        Result := Sigma;
        Exit;
    end
    else
    begin
        for i := 0 to FParams.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(FParams.Params.Items[i]);
            if UpperCase(P.Name) = UpperCase(Name) then
            begin
                Result := P.Value;
                Exit;
            end;
        end;
    end;
    Assert(False);
end;

procedure TCurvePointsSet.SetParamByName(Name: string; Value: Double);
var i: LongInt;
    P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Modified := True;
    //  snachala proveryayutsya special'nye imena
    if UpperCase(Name) = 'X0' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(x0): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        x0 := Value;
        Exit;
    end
    else
    if UpperCase(Name) = 'A' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(A): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        A := Value;
        Exit;
    end
    else
    if UpperCase(Name) = 'SIGMA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(Sigma): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        Sigma := Value;
        Exit;
    end
    else
    begin
        for i := 0 to FParams.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(FParams.Params.Items[i]);
            if UpperCase(P.Name) = UpperCase(Name) then
            begin
{$IFDEF WRITE_PARAMS_LOG}
                LogStr := IntToStr(LongInt(Self)) +
                    ' SetParamByName('+ Name +'): Value = ' + FloatToStr(Value);
                WriteLog(LogStr, Notification_);
{$ENDIF}
                P.Value := Value;
                Exit;
            end;
        end;
    end;
    Assert(False);
end;

function TCurvePointsSet.GetParamCount: LongInt;
begin
    Result := Links.Count;
end;

procedure TCurvePointsSet.Setx0(Value: Double);
var i: LongInt;
    TempDouble: Double;
    Highindex: LongInt;
    Lowindex: LongInt;
    P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(PositionP));
    P := PositionP;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetX0: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    Value := Abs(Value);
    if not Fx0IsSet then
    begin
        //  pervaya ustanovka parametra
        Fx0IsSet := True;
        P.Value := Value;
        Fx0Low := MIN_VALUE;
        Fx0High := MAX_VALUE;
        Highindex := -1;
        Lowindex := -1;
        //  opredelenie granits variatsii parametra
        for i := 0 to PointsCount - 1 do
        begin
            TempDouble := PointXCoord[i];
            if TempDouble < P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0Low - P.Value) then
                    Fx0Low := TempDouble;
                Lowindex := i;
            end;
            if TempDouble > P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0High - P.Value) then
                    Fx0High := TempDouble;
                Highindex := i;
            end;
        end;
        if Lowindex = -1 then Fx0Low := P.Value;
        if Highindex = -1 then Fx0High := P.Value;
    end
    else
    begin
        if Value < Fx0Low then begin P.Value := Fx0Low; Exit end;
        if Value > Fx0High then begin P.Value := Fx0High; Exit end;
        P.Value := Value;
    end;
end;

procedure TCurvePointsSet.SetA(Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(AmplitudeP));
    Modified := True;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetA: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    AmplitudeP.Value := Abs(Value);
end;

procedure TCurvePointsSet.SetSigma(Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(SigmaP));
    Modified := True;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetSigma: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    SigmaP.Value := Abs(Value);
    if SigmaP.Value = 0 then SigmaP.Value := TINY;
end;

function TCurvePointsSet.Hasx0: Boolean;
begin
    if Assigned(PositionP) then Result := True else Result := False;
end;

function TCurvePointsSet.HasA: Boolean;
begin
    if Assigned(AmplitudeP) then Result := True else Result := False;
end;

function TCurvePointsSet.HasSigma: Boolean;
begin
    if Assigned(SigmaP) then Result := True else Result := False;
end;

function TCurvePointsSet.Getx0: Double;
begin
    Assert(Assigned(PositionP));
    Result := PositionP.Value;
end;

function TCurvePointsSet.GetA: Double;
begin
    Assert(Assigned(AmplitudeP));
    Result := AmplitudeP.Value;
end;

function TCurvePointsSet.GetSigma: Double;
begin
    Assert(Assigned(SigmaP));
    Result := SigmaP.Value;
end;

procedure TCurvePointsSet.ScaleCurve(const Factor: Double);
var i: LongInt;
begin
    for i := 0 to PointsCount - 1 do
        Points[i][2] := Points[i][2] * Factor;
end;

procedure TCurvePointsSet.InitLinks;
var i, Index: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    Links.Free;
    Links := TList.Create;

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);

        if (P.Type_ = Variable) or
           (P.Type_ = VariablePosition) then
        begin
            //  zdes' tol'ko var'iruemye
            Index := Links.Add(P);
            //  opredelyayutsya indeksy parametrov s
            //  predopredelennoy semantikoy v spiske
            //  var'iruemyh parametrov
            SetSpecParamVarIndex(P, Index);
        end;
        if P.Type_ = Argument then ArgP := P;

        //  ustanavlivayutsya ukazateli na parametry
        //  s predopredelennoy semantikoy
        SetSpecParamPtr(P);
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TCurvePointsSet.SetSpecParamPtr(P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'SIGMA' then SigmaP := P;
    if UpperCase(P.Name) = 'A' then AmplitudeP := P;
    if (P.Type_ = VariablePosition) or
       (P.Type_ = InvariablePosition) then PositionP := P;
end;
//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TCurvePointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'SIGMA' then SigmaIndex := Index;
    if UpperCase(P.Name) = 'A' then AmplIndex := Index;
    if P.Type_ = VariablePosition then PosIndex := Index;
end;

procedure TCurvePointsSet.StoreParams;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        P.SavedValue := P.Value;
    end;
end;

procedure TCurvePointsSet.RestoreParams;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        P.Value := P.SavedValue;
    end;
    Modified := True;
end;

procedure TCurvePointsSet.SetParameters(AParams: Curve_parameters);
begin
    Assert(Assigned(AParams));

    FParams.Free; FParams := AParams;
    InitLinks;
end;

{======================== TSpecialCurveParameter ==============================}

constructor TSpecialCurveParameter.Create(Collection: TCollection);
begin
    inherited;
    FType := Calculated;
end;

procedure TSpecialCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    Dest.Name := Name;
    Dest.Value := Value;
    Dest.Type_ := Type_;
    Dest.SavedValue := SavedValue;
    Dest.VariationDisabled := VariationDisabled;
    Dest.VariationStep := VariationStep;
end;

function TSpecialCurveParameter.GetValue_: string;
begin
    Result := FloatToStr(FValue);
end;

procedure TSpecialCurveParameter.SetValue_(AValue: string);
begin
    FValue := StrToFloat(AValue);
end;

{========================== Curve_parameters ==================================}

constructor Curve_parameters.Create;
var P: TSpecialCurveParameter;
begin
    inherited;
    FParams := TCollection.Create(TSpecialCurveParameter);
    //  !!! pustaya kollektsiya zapisyvaetsya v XML-potok nekorrektno !!!
    P := TSpecialCurveParameter.Create(FParams);
    P.Name := 'x';
    P.Type_ := Argument;
    P.Value := 0;
end;

destructor Curve_parameters.Destroy;
begin
    FParams.Free;
    inherited;
end;

procedure Curve_parameters.CopyParameters(const Dest: TObject);
var i: LongInt;
    P, New: TSpecialCurveParameter;
begin
    inherited;

    Curve_parameters(Dest).Params.Clear;

    for i := 0 to Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Items[i]);
        New := TSpecialCurveParameter.Create(Curve_parameters(Dest).Params);
        P.CopyTo(New);
    end;
    Curve_parameters(Dest).SavedInitHash := SavedInitHash;
end;

function Curve_parameters.GetParamValueByName(ParamName: string): double;
var i: Integer;
    P: TSpecialCurveParameter;
begin
    for i := 0 to Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Items[i]);
        if P.Name = ParamName then
        begin
            Result := P.Value;
            break;
        end;
    end;
end;

procedure Curve_parameters.SetParamValueByName(ParamName: string; AValue: double);
var i: Integer;
    P: TSpecialCurveParameter;
begin
    for i := 0 to Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Items[i]);
        if P.Name = ParamName then
        begin
            P.Value := AValue;
            break;
        end;
    end;
end;

begin
end.
