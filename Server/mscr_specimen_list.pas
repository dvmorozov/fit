// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of auxiliary data containers.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit mscr_specimen_list;

{$MODE Delphi}

interface

uses Classes, curve_list, Math, persistent_curve_parameters, SimpMath,
    special_curve_parameter, SysUtils, argument_axis, checks;

const
    { Display mode constants. }
    XCM_2T    = 0;
    XCM_T     = 1;
    XCM_SINTL = 2;
    XCM_IDENTITY = 3;   //  general: show the raw argument as loaded (no wavelength)
    XCM_CUSTOM   = 4;   //  user-defined: display via forward/inverse formulas
    { The axis the selected curve type defines (TNamedPointsSet.CreatePreferredAxis).
      The default: the model knows what its argument means - a diffraction peak is
      fitted against a scattering angle, a wave pattern against a plain
      position - and every other mode is the user overriding that. }
    XCM_CURVE    = 5;

{ The single source mapping a display mode (XCM_*) to an argument axis. Used by
  the curve list, the viewer and the UI so the transforms are never duplicated.
  The caller owns the result. }
function CreateAxisForMode(AMode: longint; AWaveLength: double;
    const ACustomName, ACustomUnit, ACustomForward, ACustomInverse: string):
    TArgumentAxis;
{ The display mode to start a session on. A persisted mode counts only when the
  user actually picked one: a settings file written before XCM_CURVE existed says
  XCM_2T merely because that used to be the hard-coded default, and honouring it
  would pin every existing user to a diffraction axis whatever they model. }
function EffectiveViewMode(AStoredMode: longint; AChosenByUser: boolean): longint;
{ The axis label shown to the user for a mode: 'Name [unit]' (or just 'Name'
  when the axis has no unit, e.g. the general 'Position' axis). }
{ THE MODE A SAVED SETTING ACTUALLY RESOLVES TO, once the two things it depends
  on are known.

  A saved mode is a wish, not a fact. Sin(theta)/lambda divides by a wavelength,
  and if none is known the axis cannot be computed at all - so a session that
  ended on that axis and reopened against a profile with no wavelength would
  block on the wavelength dialog before the window is even up. A custom axis is
  a pair of formulas, and without them the mode names nothing.

  In both cases the answer is the model's own axis, which every curve type can
  always supply. Returning the wish instead is how start-up fails on a setting
  the user cannot see to correct.

  Pass AWaveLength = 0 for "none known", and empty strings for "no custom axis
  was saved". }
function UsableViewMode(AStoredMode: longint; AChosenByUser: boolean;
    AWaveLength: double;
    const ACustomForward, ACustomInverse: string): longint;

function AxisLabelForMode(AMode: longint; AWaveLength: double;
    const ACustomName, ACustomUnit, ACustomForward, ACustomInverse: string): string;

type
    { Defines container for curves.
      Allows to input/output angles in different representations. 
      In copying data from grid verifies them and adds to the list 
      only data for which corresponding rows are correct. }
    TMSCRCurveList = class(TCurveListBase)
    private
        { Builds the diffraction-angle axis for the current view mode + wavelength.
          The single source of truth for the 2*Theta / Theta / Sin(Theta)/Lambda
          transforms (caller frees the result). }
        function CreateAxis: TArgumentAxis;
    protected
        function RecalcParamValue(P: TSpecialCurveParameter): double; override;
        procedure ReverseCalcParamValue(P: TSpecialCurveParameter;
            NewValue: double); override;

    public
        { Vawelength at which neutronogram was recorded. }
        FWaveLength:    double;
        { It is supposed that data are given in 2 * Theta format. }
        FViewMode:      longint;
        { Definition of the user-defined (XCM_CUSTOM) axis: display name, unit and
          the forward/inverse display formulas in terms of x. }
        FCustomName:    string;
        FCustomUnit:    string;
        FCustomForward: string;
        FCustomInverse: string;

        function GetCopy: TObject; override;
        procedure CopyParameters(Dest: TObject); override;
    end;

    { Container of curves which is stored in XML-stream. }
    Parameters_list = class(TComponent)
    private
        FParameters: TMSCRCurveList;

    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;

    published
        property Parameters: TMSCRCurveList read FParameters write FParameters;
    end;

implementation

uses
    curve_types_singleton, int_curve_factory, int_curve_type_selector,
    named_points_set;

{ The axis defined by the curve type the user is modelling with. Raises when the
  selected id is not registered in this build: that is a broken selection, and
  silently falling back to some other axis would mislabel every position the user
  reads off the chart and the grid. }
function CreateAxisForSelectedCurveType(AWaveLength: double): TArgumentAxis;
var
    Selector: ICurveTypeSelector;
    TypeId: TCurveTypeId;
    CurveClass: TCurveClass;
begin
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    TypeId := Selector.GetSelectedCurveType;
    CurveClass := FindCurveClassById(TypeId);
    if not Assigned(CurveClass) then
        raise Exception.CreateFmt(
            'No curve type is registered under id %s, so the axis it defines ' +
            'cannot be determined.', [GUIDToString(TypeId)]);
    Result := CurveClass.CreatePreferredAxis(AWaveLength);
end;

function CreateAxisForMode(AMode: longint; AWaveLength: double;
    const ACustomName, ACustomUnit, ACustomForward, ACustomInverse: string):
    TArgumentAxis;
var
    Mode: TDiffractionMode;
begin
    if AMode = XCM_IDENTITY then
    begin
        Result := TIdentityAxis.Create;
        Exit;
    end;
    if AMode = XCM_CUSTOM then
    begin
        Result := TExpressionAxis.Create(ACustomName, ACustomUnit,
            ACustomForward, ACustomInverse);
        Exit;
    end;
    if AMode = XCM_CURVE then
    begin
        Result := CreateAxisForSelectedCurveType(AWaveLength);
        Exit;
    end;
    case AMode of
        XCM_T:     Mode := dmTheta;
        XCM_SINTL: Mode := dmSinThetaOverLambda;
    else
        Mode := dmTwoTheta;     //  coordinates are originally given in 2*Theta
    end;
    Result := TDiffractionAngleAxis.Create(Mode, AWaveLength);
end;

function EffectiveViewMode(AStoredMode: longint; AChosenByUser: boolean): longint;
begin
    if AChosenByUser then
        Result := AStoredMode
    else
        Result := XCM_CURVE;
end;

function UsableViewMode(AStoredMode: longint; AChosenByUser: boolean;
    AWaveLength: double;
    const ACustomForward, ACustomInverse: string): longint;
begin
    //  WHETHER THE SETTING COUNTS AT ALL comes first: a mode the user never
    //  chose is not a wish, it is whatever the previous session happened to
    //  leave behind.
    Result := EffectiveViewMode(AStoredMode, AChosenByUser);
    if (Result = XCM_SINTL) and (AWaveLength = 0) then
        Result := XCM_CURVE;
    //  BOTH formulas, not either: the axis has to convert in each direction,
    //  and one of them alone means a value that can be shown and not read back,
    //  or read back and not shown.
    if (Result = XCM_CUSTOM) and
        ((ACustomForward = '') or (ACustomInverse = '')) then
        Result := XCM_CURVE;
end;

function AxisLabelForMode(AMode: longint; AWaveLength: double;
    const ACustomName, ACustomUnit, ACustomForward, ACustomInverse: string): string;
var
    Axis: TArgumentAxis;
begin
    Axis := CreateAxisForMode(AMode, AWaveLength, ACustomName, ACustomUnit,
        ACustomForward, ACustomInverse);
    try
        Result := Axis.DisplayName;
        if Axis.UnitName <> '' then
            Result := Result + ' [' + Axis.UnitName + ']';
    finally
        Axis.Free;
    end;
end;

function TMSCRCurveList.CreateAxis: TArgumentAxis;
begin
    Result := CreateAxisForMode(FViewMode, FWaveLength, FCustomName, FCustomUnit,
        FCustomForward, FCustomInverse);
end;

function TMSCRCurveList.RecalcParamValue(P: TSpecialCurveParameter): double;
var
    Axis: TArgumentAxis;
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) then
    begin
        Axis := CreateAxis;
        try
            Result := Axis.ToDisplay(P.Value);
        finally
            Axis.Free;
        end;
    end
    else
        Result := P.Value;
end;

procedure TMSCRCurveList.ReverseCalcParamValue(P: TSpecialCurveParameter;
    NewValue: double);
var
    Axis: TArgumentAxis;
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) then
    begin
        Axis := CreateAxis;
        try
            P.Value := Axis.FromDisplay(NewValue);
        finally
            Axis.Free;
        end;
    end
    else
        P.Value := NewValue;
end;

function TMSCRCurveList.GetCopy: TObject;
begin
    Result := TMSCRCurveList.Create;
    CopyParameters(Result);
end;

procedure TMSCRCurveList.CopyParameters(Dest: TObject);
begin
    CheckAssigned(Dest, 'the curve list being copied into');

    inherited;

    TMSCRCurveList(Dest).FWaveLength   := FWaveLength;
    TMSCRCurveList(Dest).FViewMode := FViewMode;
    TMSCRCurveList(Dest).FCustomName    := FCustomName;
    TMSCRCurveList(Dest).FCustomUnit    := FCustomUnit;
    TMSCRCurveList(Dest).FCustomForward := FCustomForward;
    TMSCRCurveList(Dest).FCustomInverse := FCustomInverse;
end;

{ Parameters_list }

constructor Parameters_list.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    FParameters := TMSCRCurveList.Create;
end;

destructor Parameters_list.Destroy;
begin
    FParameters.Free;
    inherited Destroy;
end;

{$warnings off}
initialization
    DecimalSeparator := '.';

end.
{$warnings on}
