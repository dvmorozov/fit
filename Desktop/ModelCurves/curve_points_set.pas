// SPDX-License-Identifier: GPL-3.0-or-later
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
    amplitude_curve_parameter, checks, Classes, curve_instance_id, Math,
    persistent_curve_parameter_container,
    persistent_curve_parameters, points_set, position_curve_parameter,
    self_copied_component, sigma_curve_parameter, special_curve_parameter,
    SysUtils, title_points_set;

type
    { Generic container for point set of all calcuated curves. }
    TCurvePointsSet = class(TTitlePointsSet)
    protected
        { List of all curve parameters. }
        FParams: Curve_parameters;
        { Contains only references to variable parameters. }
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
        function GetValueByName(Name: string): double;
        function GetTypedByName(Name: string): Variant;
        procedure SetTypedByName(Name: string; const AValue: Variant); virtual;
        procedure SetValueByName(Name: string; Value: double); virtual;

        { Returns variable parameter by through index. }
        function GetVariableValue(Index: longint): double; virtual;
        function GetVariableName(Index: longint): string;
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
        procedure DoCalc; virtual; abstract;
        { Performs intialization of variable list parameters. }
        procedure InitListOfVariableParameters;

        { These functions don't perform profile recalculation and
          are used for initialization purposes (when Modified is set up). }

        procedure Setx0(Value: double);
        procedure SetA(Value: double);
        procedure SetSigma(Value: double);
        function Getx0: double;
        function GetA: double;
        function GetSigma: double;

    public
        { WHERE THIS CURVE'S FIRST POINT SITS IN THE PROFILE IT BELONGS TO.

          A curve holds only the samples it covers, so its own index and the
          profile's are no longer the same number. Everything that sums a curve
          into a profile - or reads one alongside one - translates through this.

          It is set in exactly ONE place, TFitTask.CreatePointsFor, in the same
          loop that copies the x values in, so the offset and the coordinates
          cannot disagree: they come from the same walk over the same profile.
          After that the point array is sealed (AddNewPoint and friends refuse),
          so nothing can shift the points out from under it.

          -1 until a window has been established, which is what SetWindow does. }
        FFirstSampleIndex: longint;
        { Set by SetWindow: the points ARE the curve's extent and may not grow. }
        FSealed: boolean;
        { WHICH INSTANCE this is. An opaque handle, issued once to the pick
          this curve is built from and inherited by every instance rebuilt from
          that pick, so the values a fit found are given back to the right
          curve. TFitTask.RestoreCurveValues is what reads it.

          NOT the curve TYPE - that is GetCurveTypeId, a per-class constant.
          See curve_instance_id for why the two are separate types. }
        FInstanceId: TCurveInstanceId;
        { Initial value of x0. It's used in some algorithms. }
        FInitx0: double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure SetParameters(AParams: Curve_parameters);
        { WHERE THIS CURVE EXISTS, as a fact about the instance.

          Unbounded by default, which is the truth for every curve whose tails
          are small but real - a Gaussian is never exactly zero, so it occupies
          the whole fitted interval and always did.

          A compactly supported curve overrides both: it is EXACTLY zero outside
          [SupportMin, SupportMax], and the samples out there are not part of it. }
        function SupportMin: double; virtual;
        function SupportMax: double; virtual;
        { Whether the sample at AX is one of this curve's own.

          Asked per sample rather than derived from the range, because a curve
          may decline a sample INSIDE it: two patterns placed end to end share
          one x, and it belongs to exactly one of them. Carrying it in both, one
          of them holding an exact zero, is the very thing this change removes. }
        function CoversSample(const AX: double): boolean; virtual;

        { Gives this curve the stretch of AProfile it covers, and seals it.

          This is the only way a curve gets points. It copies the x values from
          the profile and records where it started in the SAME walk, so the
          offset cannot disagree with the coordinates, and it then refuses
          further additions: a curve's extent is decided when it is built. }
        procedure SetWindow(AProfile: TPointsSet; AFirst, ALast: longint);
        { True once SetWindow has run. }
        function HasWindow: boolean;

        { Adds this curve's values into ATarget, and takes them out again. The
          translation from the curve's own index to the target's lives HERE and
          nowhere else, so no call site does the arithmetic. }
        procedure AddTo(ATarget: TPointsSet);
        procedure SubtractFrom(ATarget: TPointsSet);
        procedure AddNewPoint(XValue, YValue: double); override;

        { Recalculates the curve's own points if anything changed since the last
          time. It once took a set of index PAIRS encoded as coordinates so that
          part of a curve could be recomputed; every caller passed nil, and it
          was a second place where a curve's array index doubled as a profile
          index. }
        procedure ReCalc;
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
        function HasSigma: boolean;

        { Provides access to variable parameters for optimizer. }
        property VariableValues[index: longint]: double
            read GetVariableValue write SetVariableValue;
        { Provides access to variation steps for optimizer. }
        property VariationSteps[index: longint]: double
            read GetVariationStep write SetVariationStep;
        { Which parameter the optimizer's index n refers to. The variable
          list is built by filtering FParams on type, so the mapping is not
          obvious from outside - and "which parameters is this fit actually
          moving?" is the first question to ask of a fit that will not
          converge. }
        property VariableNames[index: longint]: string read GetVariableName;
        property VariableCount: longint read GetVariableCount;
        { Returns object containing all parameters. }
        property Parameters: Curve_parameters read FParams;

        { Provides access to all parameters by name. }
        property ValuesByName[Name: string]: double
            read GetValueByName write SetValueByName;

        { The same single member as ValuesByName, but WITH its type - for values
          that are not quantities (identity, labels). }
        property TypedByName[Name: string]: Variant
            read GetTypedByName write SetTypedByName;

        { Properties provide access to attributes having predefined semantics
          for special algorithms.
          Use methods HasX before to check existense of such attributes. }

        { Variation of the parameter x0 is limited by two adjacent points. }
        { TODO: remove setter. }
        property x0: double read Getx0 write Setx0;
        { TODO: remove setter. }
        property A: double read GetA write SetA;
        property Sigma: double read GetSigma write SetSigma;
    end;

implementation

constructor TCurvePointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FParams      := Curve_parameters.Create(nil);
    FRecalculate := True;
    //  No window yet: a curve is created before it is told which stretch of the
    //  profile it covers, and HasWindow is what distinguishes the two states.
    FFirstSampleIndex := -1;
end;

destructor TCurvePointsSet.Destroy;
begin
    FVariableParameters.Free;
    FParams.Free;
    inherited;
end;

procedure TCurvePointsSet.ReCalc;
begin
    if FRecalculate then
    begin
        DoCalc;
        FRecalculate := False;
    end;
end;

function TCurvePointsSet.SupportMin: double;
begin
    Result := NegInfinity;
end;

function TCurvePointsSet.SupportMax: double;
begin
    Result := Infinity;
end;

function TCurvePointsSet.CoversSample(const AX: double): boolean;
begin
    Result := (AX >= SupportMin) and (AX <= SupportMax);
end;

function TCurvePointsSet.HasWindow: boolean;
begin
    Result := FFirstSampleIndex >= 0;
end;

procedure TCurvePointsSet.SetWindow(AProfile: TPointsSet;
    AFirst, ALast: longint);
var
    i: longint;
begin
    CheckAssigned(AProfile, 'the profile a curve takes its samples from');
    CheckThat((AFirst >= 0) and (ALast < AProfile.PointsCount) and (AFirst <= ALast),
        'a curve window must be a non-empty stretch of the profile');

    FSealed := False;
    Clear;
    //  The x values come from the profile and the offset is recorded in the SAME
    //  walk, so "which sample is my point i" has one answer rather than two that
    //  could drift apart.
    FFirstSampleIndex := AFirst;
    for i := AFirst to ALast do
        AddNewPoint(AProfile.PointXCoord[i], 0);
    FSealed := True;
end;

procedure TCurvePointsSet.AddNewPoint(XValue, YValue: double);
begin
    //  Sealed rather than merely documented: a curve that grew after its window
    //  was set would be summed into the wrong place from then on, and silently,
    //  because the fit still converges.
    //
    //  Pascal cannot un-inherit this, so it is refused at run time and a
    //  registry-walking test asserts every curve type refuses it. The
    //  compile-time form of the guarantee needs a curve to stop BEING a
    //  TPointsSet - see the roadmap.
    CheckThat(not FSealed,
        'curve_points_set: a curve''s points are its extent, decided when it is ' +
        'built, and cannot be added to afterwards');
    inherited AddNewPoint(XValue, YValue);
end;

procedure TCurvePointsSet.AddTo(ATarget: TPointsSet);
var
    i: longint;
begin
    CheckAssigned(ATarget, 'the profile a curve is summed into');
    CheckThat(HasWindow, 'a curve with no window over the profile cannot be summed into it');
    CheckThat(FFirstSampleIndex + PointsCount <= ATarget.PointsCount,
        'a curve must not reach past the end of the profile it is summed into');
    for i := 0 to PointsCount - 1 do
        ATarget.PointYCoord[FFirstSampleIndex + i] :=
            ATarget.PointYCoord[FFirstSampleIndex + i] + PointYCoord[i];
end;

procedure TCurvePointsSet.SubtractFrom(ATarget: TPointsSet);
var
    i: longint;
begin
    CheckAssigned(ATarget, 'the profile a curve is subtracted from');
    CheckThat(HasWindow, 'a curve with no window over the profile cannot be subtracted from it');
    CheckThat(FFirstSampleIndex + PointsCount <= ATarget.PointsCount,
        'a curve must not reach past the end of the profile it is subtracted from');
    for i := 0 to PointsCount - 1 do
        ATarget.PointYCoord[FFirstSampleIndex + i] :=
            ATarget.PointYCoord[FFirstSampleIndex + i] - PointYCoord[i];
end;

procedure TCurvePointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TCurvePointsSet(Dest).FFirstSampleIndex := FFirstSampleIndex;
    TCurvePointsSet(Dest).FSealed := FSealed;
    TCurvePointsSet(Dest).SetParameters(Curve_parameters(FParams.GetCopy));
    TCurvePointsSet(Dest).FInstanceId := FInstanceId;
    //  COPIED TOO, and for the same reason as the handle: both say which
    //  instance this is. TFitService collects COPIES of the tasks' curves,
    //  so anything server-side asking "which curve is this, and where was it
    //  seeded?" would otherwise read nothing from every one of them.
    TCurvePointsSet(Dest).FInitx0 := FInitx0;
end;

function TCurvePointsSet.MinimumStepAchieved(Index: longint): boolean;
begin
    CheckIndex(Index, FVariableParameters.Count, 'the parameters this curve lets the fit vary');

    Result := TSpecialCurveParameter(FVariableParameters[Index]).MinimumStepAchieved;
end;

procedure TCurvePointsSet.InitVariationStep(Index: longint);
begin
    CheckIndex(Index, FVariableParameters.Count, 'the parameters this curve lets the fit vary');

    TSpecialCurveParameter(FVariableParameters[Index]).InitVariationStep;
end;

function TCurvePointsSet.GetVariableValue(Index: longint): double;
var
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(index, GetVariableCount, 'the parameters this curve lets the fit vary');

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    Result    := Parameter.Value;
end;

function TCurvePointsSet.GetVariationStep(Index: longint): double;
var
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(Index, GetVariableCount, 'the parameters this curve lets the fit vary');

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[index]);
    CheckThat(not Parameter.VariationDisabled, 'a parameter the fit may not vary has no variation step to read');
    Result := Parameter.VariationStep;
end;

procedure TCurvePointsSet.SetVariableValue(Index: longint; Value: double);
var
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(Index, GetVariableCount, 'the parameters this curve lets the fit vary');

    FRecalculate := True;
    Parameter    := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    Parameter.Value := Value;
end;

procedure TCurvePointsSet.SetVariationStep(Index: longint; AStep: double);
var
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(Index, GetVariableCount, 'the parameters this curve lets the fit vary');

    Parameter := TSpecialCurveParameter(FVariableParameters.Items[Index]);
    CheckThat(not Parameter.VariationDisabled, 'a parameter the fit may not vary cannot be given a variation step');
    Parameter.VariationStep := AStep;
end;

function TCurvePointsSet.GetValueByName(Name: string): double;
begin
    Result := FParams.ValuesByName[Name];
end;

function TCurvePointsSet.GetTypedByName(Name: string): Variant;
begin
    Result := FParams.TypedByName[Name];
end;

procedure TCurvePointsSet.SetTypedByName(Name: string; const AValue: Variant);
begin
    FParams.TypedByName[Name] := AValue;
end;

procedure TCurvePointsSet.SetValueByName(Name: string; Value: double);
begin
    FRecalculate := True;
    FParams.ValuesByName[Name] := Value;
end;

function TCurvePointsSet.GetVariableName(Index: longint): string;
begin
    CheckIndex(Index, GetVariableCount, 'the parameters this curve lets the fit vary');

    Result := TSpecialCurveParameter(FVariableParameters.Items[Index]).Name;
end;

function TCurvePointsSet.GetVariableCount: longint;
begin
    Result := FVariableParameters.Count;
end;

procedure TCurvePointsSet.Setx0(Value: double);
begin
    CheckAssigned(FPositionP, 'the parameter that places this curve');

    FRecalculate     := True;
    FPositionP.Value := Value;
end;

procedure TCurvePointsSet.SetA(Value: double);
begin
    CheckAssigned(FAmplitudeP, 'the amplitude parameter of this curve');

    FRecalculate      := True;
    FAmplitudeP.Value := Value;
end;

procedure TCurvePointsSet.SetSigma(Value: double);
begin
    CheckAssigned(FSigmaP, 'the width parameter of this curve');

    FRecalculate  := True;
    FSigmaP.Value := Value;
end;

function TCurvePointsSet.Hasx0: boolean;
begin
    Result := Assigned(FPositionP);
end;

function TCurvePointsSet.HasA: boolean;
begin
    Result := Assigned(FAmplitudeP);
end;

function TCurvePointsSet.HasSigma: boolean;
begin
    Result := Assigned(FSigmaP);
end;

function TCurvePointsSet.Getx0: double;
begin
    CheckAssigned(FPositionP, 'the parameter that places this curve');

    Result := FPositionP.Value;
end;

function TCurvePointsSet.GetA: double;
begin
    CheckAssigned(FAmplitudeP, 'the amplitude parameter of this curve');
    Result := FAmplitudeP.Value;
end;

function TCurvePointsSet.GetSigma: double;
begin
    CheckAssigned(FSigmaP, 'the width parameter of this curve');

    Result := FSigmaP.Value;
end;

procedure TCurvePointsSet.InitListOfVariableParameters;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    CheckAssigned(FParams, 'the full parameter list of this curve');

    FVariableParameters.Free;
    FVariableParameters := TList.Create;

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];

        if (Parameter.Type_ = Variable) or (Parameter.Type_ = VariablePosition) or
            (Parameter.Type_ = Amplitude) or (Parameter.Type_ = Width) then
            FVariableParameters.Add(Parameter);
        SetSpecParamPtr(Parameter);
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TCurvePointsSet.SetSpecParamPtr(Parameter: TSpecialCurveParameter);
begin
    CheckAssigned(Parameter, 'the parameter whose special role is being recorded');

    //  Role by explicit type takes precedence; the name is a convenience
    //  fall-back so existing/convention-named curves keep working.
    if (Parameter.Type_ = Width) or (UpperCase(Parameter.Name) = 'SIGMA') then
        FSigmaP := TSigmaCurveParameter(Parameter);

    if (Parameter.Type_ = Amplitude) or (UpperCase(Parameter.Name) = 'A') then
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
    CheckAssigned(FParams, 'the full parameter list of this curve');

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
    CheckAssigned(FParams, 'the full parameter list of this curve');

    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := FParams[i];
        Parameter.Value := Parameter.SavedValue;
    end;
    FRecalculate := True;
end;

procedure TCurvePointsSet.SetParameters(AParams: Curve_parameters);
begin
    CheckAssigned(AParams, 'the parameter list being given to this curve');

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
