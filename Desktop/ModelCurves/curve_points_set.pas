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
    amplitude_curve_parameter, Classes, persistent_curve_parameter_container,
    persistent_curve_parameters, points_set, position_curve_parameter,
    self_copied_component, sigma_curve_parameter, special_curve_parameter,
    SysUtils, title_points_set;

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
        FAmplitudeP: TAmplitudeCurveParameter;
        FPositionP: TPositionCurveParameter;
        FSigmaP: TSigmaCurveParameter;
        { It is used in TUserPointsSet. TODO: move it to TUserPointsSet. }
        FArgP: TSpecialCurveParameter;

        { Returns value of parameter with given name. }
        function GetValueByName(Name: string): double; virtual;
        procedure SetValueByName(Name: string; Value: double); virtual;

        { Returns variable parameter by through index. }
        function GetVariableValue(Index: longint): double; virtual;
        { Returns optimization step for parameter. }
        function GetVariationStep(Index: longint): double; virtual;
        { Sets value of variable parameter. }
        procedure SetVariableValue(Index: longint; Value: double); virtual;
        { Sets optimization step for parameter. }
        procedure SetVariationStep(Index: longint; AStep: double); virtual;
        { Returns total number of variable parameters. }
        function GetVariableCount: longint;

        { Initializes pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(Parameter: TSpecialCurveParameter); virtual;

    protected
        FRecalculate: boolean;

        procedure AddParameter(Parameter: TSpecialCurveParameter);
        { Performs recalculation of all profile points. }
        procedure DoCalc(const Bounds: TPointsSet); virtual; abstract;
        { Multiplies profile points by given factor. }
        procedure ScaleCurve(const Factor: double);
        { Performs intialization of variable list parameters. }
        procedure InitListOfVariableParameters;

        { These functions don't perform profile recalculation and
          are used for initialization purposes (when Modified is set up). }

        procedure Setx0(Value: double);
        procedure SetA(Value: double);
        function Getx0: double;
        function GetA: double;
        function GetSigma: double;

    public
        { Boundaries for R-factor calculation. }
        FMinX, FMaxX: double;
        { Flag designating that boundaries of R-factor calculation
          has been set up. If False R-factor is calculated for all 
          points of profile. }
        FRangeDefined: boolean;
        { Hash of initial parameter values. }
        FInitHash: cardinal;
        { Initial value of x0. It's used in some algorithms. }
        FInitx0: double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure SetParameters(AParams: Curve_parameters);
        { Set of pairs of indexes of X-coordinates forming intervals in
          which functions should be calcuated (boundaries are included). 
          Equity to nil designates that functions should be calculated 
          for all points. Indexes instead of coordinates are used to 
          avoid searching. }
        procedure ReCalc(const Bounds: TPointsSet);
        { Temporarily stores values of variable parameters in internal memory area. }
        procedure BackupParameters;
        { Restores values of variable parameters from temporary storage. }
        procedure RestoreParameters;
        procedure CopyParameters(Dest: TObject); override;

        { These methods are used to limit direct access to variable parameters. }

        function MinimumStepAchieved(Index: longint): boolean;
        procedure InitVariationStep(Index: longint);

        { Return True if attributes with predefined semantics were assigned. }

        function Hasx0: boolean;
        function HasA: boolean;

        { Provides access to variable parameters for optimizer. }
        property VariableValues[index: longint]: double
            read GetVariableValue write SetVariableValue;
        { Provides access to variation steps for optimizer. }
        property VariationSteps[index: longint]: double
            read GetVariationStep write SetVariationStep;
        property VariableCount: longint read GetVariableCount;
        { Returns object containing all parameters. }
        property Parameters: Curve_parameters read FParams;

        { Provides access to all parameters by name. }
        property ValuesByName[Name: string]: double
            read GetValueByName write SetValueByName;

        { Properties provide access to attributes having predefined semantics
          for special algorithms.
          Use methods HasX before to check existense of such attributes. }

        { Variation of the parameter x0 is limited by two adjacent points. }
        { TODO: remove setter. }
        property x0: double read Getx0 write Setx0;
        { TODO: remove setter. }
        property A: double read GetA write SetA;
        property Sigma: double read GetSigma;
    end;

implementation

constructor TCurvePointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FParams      := Curve_parameters.Create(nil);
    FRecalculate := True;
end;

destructor TCurvePointsSet.Destroy;
begin
    FVariableParameters.Free;
    FParams.Free;
    inherited;
end;

procedure TCurvePointsSet.ReCalc(const Bounds: TPointsSet);
begin
    if FRecalculate then
    begin
        DoCalc(Bounds);
        FRecalculate := False;
    end;
end;

procedure TCurvePointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TCurvePointsSet(Dest).FMinX := FMinX;
    TCurvePointsSet(Dest).FMaxX := FMaxX;
    TCurvePointsSet(Dest).FRangeDefined := FRangeDefined;
    TCurvePointsSet(Dest).SetParameters(Curve_parameters(FParams.GetCopy));
    TCurvePointsSet(Dest).FInitHash := FInitHash;
end;

function TCurvePointsSet.MinimumStepAchieved(Index: longint): boolean;
begin
    Assert((Index >= 0) and (Index < FVariableParameters.Count));

    Result := TSpecialCurveParameter(FVariableParameters[Index]).MinimumStepAchieved;
end;

procedure TCurvePointsSet.InitVariationStep(Index: longint);
begin
    Assert((Index >= 0) and (Index < FVariableParameters.Count));

    TSpecialCurveParameter(FVariableParameters[Index]).InitVariationStep;
end;

function TCurvePointsSet.GetVariableValue(Index: longint): double;
var
    Parameter: TSpecialCurveParameter;
begin
    Assert((index >= 0) and (index < GetVariableCount));

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    Result    := Parameter.Value;
end;

function TCurvePointsSet.GetVariationStep(Index: longint): double;
var
    Parameter: TSpecialCurveParameter;
begin
    Assert((Index >= 0) and (Index < GetVariableCount));

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    Assert(not Parameter.VariationDisabled);
    Result := Parameter.VariationStep;
end;

procedure TCurvePointsSet.SetVariableValue(Index: longint; Value: double);
var
    Parameter: TSpecialCurveParameter;
begin
    Assert((Index >= 0) and (Index < GetVariableCount));

    FRecalculate := True;
    Parameter    := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    Parameter.Value := Value;
end;

procedure TCurvePointsSet.SetVariationStep(Index: longint; AStep: double);
var
    Parameter: TSpecialCurveParameter;
begin
    Assert((Index < GetVariableCount) and (Index >= 0));

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    Assert(not Parameter.VariationDisabled);
    Parameter.VariationStep := AStep;
end;

function TCurvePointsSet.GetValueByName(Name: string): double;
begin
    Result := FParams.ValuesByName[Name];
end;

procedure TCurvePointsSet.SetValueByName(Name: string; Value: double);
begin
    FRecalculate := True;
    FParams.ValuesByName[Name] := Value;
end;

function TCurvePointsSet.GetVariableCount: longint;
begin
    Result := FVariableParameters.Count;
end;

procedure TCurvePointsSet.Setx0(Value: double);
begin
    Assert(Assigned(FPositionP));

    FRecalculate     := True;
    FPositionP.Value := Value;
end;

procedure TCurvePointsSet.SetA(Value: double);
begin
    Assert(Assigned(FAmplitudeP));

    FRecalculate      := True;
    FAmplitudeP.Value := Value;
end;

function TCurvePointsSet.Hasx0: boolean;
begin
    Result := Assigned(FPositionP);
end;

function TCurvePointsSet.HasA: boolean;
begin
    Result := Assigned(FAmplitudeP);
end;

function TCurvePointsSet.Getx0: double;
begin
    Assert(Assigned(FPositionP));

    Result := FPositionP.Value;
end;

function TCurvePointsSet.GetA: double;
begin
    Assert(Assigned(FAmplitudeP));
    Result := FAmplitudeP.Value;
end;

function TCurvePointsSet.GetSigma: double;
begin
    Assert(Assigned(FSigmaP));

    Result := FSigmaP.Value;
end;

procedure TCurvePointsSet.ScaleCurve(const Factor: double);
var
    i: longint;
begin
    for i := 0 to PointsCount - 1 do
        Points[i][2] := Points[i][2] * Factor;
end;

procedure TCurvePointsSet.InitListOfVariableParameters;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));

    FVariableParameters.Free;
    FVariableParameters := TList.Create;

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];

        if (Parameter.Type_ = Variable) or (Parameter.Type_ =
            VariablePosition) then
            FVariableParameters.Add(Parameter);
        SetSpecParamPtr(Parameter);
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TCurvePointsSet.SetSpecParamPtr(Parameter: TSpecialCurveParameter);
begin
    Assert(Assigned(Parameter));

    if UpperCase(Parameter.Name) = 'SIGMA' then
        FSigmaP := TSigmaCurveParameter(Parameter);

    if UpperCase(Parameter.Name) = 'A' then
        FAmplitudeP := TAmplitudeCurveParameter(Parameter);

    if (Parameter.Type_ = VariablePosition) or
        (Parameter.Type_ = InvariablePosition) then
        FPositionP := TPositionCurveParameter(Parameter);

    if Parameter.Type_ = Argument then
        FArgP := Parameter;
end;

procedure TCurvePointsSet.BackupParameters;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];
        Parameter.SavedValue := Parameter.Value;
    end;
end;

procedure TCurvePointsSet.RestoreParameters;
var
    i: longint;
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

    FParams.Free;
    FParams := AParams;
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
