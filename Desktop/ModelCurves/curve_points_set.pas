{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of generic container for point set of all calcuated curves.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit curve_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, self_copied_component, points_set,
    title_points_set, amplitude_curve_parameter, sigma_curve_parameter,
    position_curve_parameter, persistent_curve_parameter_container,
    special_curve_parameter, persistent_curve_parameters;

type
    { Generic container for point set of all calcuated curves. }
    TCurvePointsSet = class(TTitlePointsSet)
    protected
        { List of all curve parameters. }
        FParams: Curve_parameters;
        { List of variable parameters. }
        FVariableParameters: TList;
        { Set of variable parameters common for all curve types (meaning could
          be different in particular cases).
          Parameters with predefined semantics have constraints, which
          can be associated with curve points. Attributes store pointers
          to parameters with predefined semantics. Parameters are created
          in descendant constructors. }
        AmplitudeP: TAmplitudeCurveParameter;
        PositionP: TPositionCurveParameter;
        SigmaP: TSigmaCurveParameter;
        { It is used in TUserPointsSet. TODO: move it to TUserPointsSet. }
        ArgP: TSpecialCurveParameter;

        { Returns value of parameter with given name. }
        function GetValueByName(Name: string): Double; virtual;
        procedure SetValueByName(Name: string; Value: Double); virtual;

        { Returns variable parameter by through index. }
        function GetVariableValue(Index: LongInt): Double; virtual;
        { Returns optimization step for parameter. }
        function GetVariationStep(Index: LongInt): Double; virtual;
        { Sets value of variable parameter. }
        procedure SetVariableValue(Index: LongInt; Value: Double); virtual;
        { Sets optimization step for parameter. }
        procedure SetVariationStep(Index: LongInt; AStep: Double); virtual;
        { Returns total number of variable parameters. }
        function GetVariableCount: LongInt;

        { Initializes pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(Parameter: TSpecialCurveParameter); virtual;

    protected
        FRecalculate: Boolean;

        procedure AddParameter(Parameter: TSpecialCurveParameter);
        { Performs recalculation of all profile points. }
        procedure DoCalc(const Intervals: TPointsSet); virtual; abstract;
        { Multiplies profile points by given factor. }
        procedure ScaleCurve(const Factor: Double);
        { Performs intialization of variable list parameters. }
        procedure InitListOfVariableParameters;
        
        { These functions don't perform profile recalculation and
          are used for initialization purposes (when Modified is set up). }
        
        procedure Setx0(Value: Double);
        procedure SetA(Value: Double);
        function Getx0: Double;
        function GetA: Double;
        function GetSigma: Double;

    public
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

        { These methods are used to limit direct access to variable parameters. }

        function MinimumStepAchieved(VariableIndex: LongInt): Boolean;
        procedure InitVariationStep(VariableIndex: LongInt);

        { Return True if attributes with predefined semantics were assigned. }
        
        function Hasx0: Boolean;
        function HasA: Boolean;

        { Provides access to variable parameters for optimizer. }
        property VariableValues[index: LongInt]: Double
            read GetVariableValue write SetVariableValue;
        { Provides access to variation steps for optimizer. }
        property VariationSteps[index: LongInt]: Double
            read GetVariationStep write SetVariationStep;
        property VariableCount: LongInt read GetVariableCount;
        { Returns object containing all parameters. }
        property Parameters: Curve_parameters read FParams;

        { Provides access to all parameters by name. }
        property ValuesByName[Name: string]: Double
            read GetValueByName write SetValueByName;

        { Properties provide access to attributes having predefined semantics
          for special algorithms.
          Use methods HasX before to check existense of such attributes. }

        { Variation of the parameter x0 is limited by two adjacent points. }
        { TODO: remove setter. }
        property x0: Double read Getx0 write Setx0;
        { TODO: remove setter. }
        property A: Double read GetA write SetA;
        property Sigma: Double read GetSigma;
    end;

implementation

constructor TCurvePointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FParams := Curve_parameters.Create(nil);
    FRecalculate := True;
end;

destructor TCurvePointsSet.Destroy;
begin
    FVariableParameters.Free;
    FParams.Free;
    inherited;
end;

procedure TCurvePointsSet.ReCalc(const Intervals: TPointsSet);
begin
    if FRecalculate then
    begin
        DoCalc(Intervals);
        FRecalculate := False;
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

function TCurvePointsSet.MinimumStepAchieved(VariableIndex: LongInt): Boolean;
begin
    Assert((VariableIndex >= 0) and (VariableIndex < FVariableParameters.Count));
    Result := TSpecialCurveParameter(FVariableParameters[VariableIndex]).MinimumStepAchieved;
end;

procedure TCurvePointsSet.InitVariationStep(VariableIndex: LongInt);
begin
    Assert((VariableIndex >= 0) and (VariableIndex < FVariableParameters.Count));
    TSpecialCurveParameter(FVariableParameters[VariableIndex]).InitVariationStep;
end;

function TCurvePointsSet.GetVariableValue(Index: LongInt): Double;
var Parameter: TSpecialCurveParameter;
begin
    Assert(index < GetVariableCount);
    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    Result := Parameter.Value;
end;

function TCurvePointsSet.GetVariationStep(Index: LongInt): Double;
var Parameter: TSpecialCurveParameter;
begin
    Assert(Index < GetVariableCount);
    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    Assert(not Parameter.VariationDisabled);
    Result := Parameter.VariationStep;
end;

procedure TCurvePointsSet.SetVariableValue(Index: LongInt; Value: Double);
var Parameter: TSpecialCurveParameter;
begin
    Assert((Index < GetVariableCount) and (Index >= 0));
    FRecalculate := True;
    Parameter := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    Parameter.Value := Value;
end;

procedure TCurvePointsSet.SetVariationStep(Index: LongInt; AStep: Double);
var Parameter: TSpecialCurveParameter;
begin
    Assert((Index < GetVariableCount) and (Index >= 0));
    Parameter := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    Assert(not Parameter.VariationDisabled);
    Parameter.VariationStep := AStep;
end;

function TCurvePointsSet.GetValueByName(Name: string): Double;
begin
    Result := FParams.ValuesByName[Name];
end;

procedure TCurvePointsSet.SetValueByName(Name: string; Value: Double);
begin
    FRecalculate := True;
    FParams.ValuesByName[Name] := Value;
end;

function TCurvePointsSet.GetVariableCount: LongInt;
begin
    Result := FVariableParameters.Count;
end;

procedure TCurvePointsSet.Setx0(Value: Double);
begin
    Assert(Assigned(PositionP));
    FRecalculate := True;
    PositionP.Value := Value;
end;

procedure TCurvePointsSet.SetA(Value: Double);
begin
    Assert(Assigned(AmplitudeP));
    FRecalculate := True;
    AmplitudeP.Value := Value;
end;

function TCurvePointsSet.Hasx0: Boolean;
begin
    Result := Assigned(PositionP);
end;

function TCurvePointsSet.HasA: Boolean;
begin
    Result := Assigned(AmplitudeP);
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

procedure TCurvePointsSet.InitListOfVariableParameters;
var i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));

    FVariableParameters.Free;
    FVariableParameters := TList.Create;

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];

        if (Parameter.Type_ = Variable) or
           (Parameter.Type_ = VariablePosition) then
        begin
            FVariableParameters.Add(Parameter);
        end;
        SetSpecParamPtr(Parameter);
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TCurvePointsSet.SetSpecParamPtr(Parameter: TSpecialCurveParameter);
begin
    Assert(Assigned(Parameter));
    if UpperCase(Parameter.Name) = 'SIGMA' then
        SigmaP := TSigmaCurveParameter(Parameter);

    if UpperCase(Parameter.Name) = 'A' then
        AmplitudeP := TAmplitudeCurveParameter(Parameter);

    if (Parameter.Type_ = VariablePosition) or
       (Parameter.Type_ = InvariablePosition) then
       PositionP := TPositionCurveParameter(Parameter);

    if Parameter.Type_ = Argument then ArgP := Parameter;
end;

procedure TCurvePointsSet.StoreParams;
var i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];
        Parameter.SavedValue := Parameter.Value;
    end;
end;

procedure TCurvePointsSet.RestoreParams;
var i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];
        Parameter.Value := Parameter.SavedValue;
    end;
    FRecalculate := True;
end;

procedure TCurvePointsSet.SetParameters(AParams: Curve_parameters);
begin
    Assert(Assigned(AParams));

    FParams.Free; FParams := AParams;
    InitListOfVariableParameters;
end;

procedure TCurvePointsSet.AddParameter(Parameter: TSpecialCurveParameter);
var
    Container: TPersistentCurveParameterContainer;
begin
    Container := TPersistentCurveParameterContainer(FParams.Params.Add);
    try
        Container.Parameter := Parameter;
    except
        FParams.Params.Delete(Container.ID);
        Container.Free;
        raise;
    end;
end;

begin
end.
