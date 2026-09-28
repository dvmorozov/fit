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
    special_curve_parameter, SysUtils, coordinate_axis, axis_mode_registry,
    checks;

type
    { Defines container for curves.
      Allows to input/output angles in different representations. 
      In copying data from grid verifies them and adds to the list 
      only data for which corresponding rows are correct. }
    TMSCRCurveList = class(TCurveListBase)
    private
        { The argument axis in force, built for each conversion (caller frees
          the result). The single source of the transform between a stored
          position and the one the grid shows. }
        function CreateAxis: TCoordinateAxis;
    protected
        function RecalcParamValue(P: TSpecialCurveParameter): double; override;
        procedure ReverseCalcParamValue(P: TSpecialCurveParameter;
            NewValue: double); override;

    public
        { Vawelength at which neutronogram was recorded. }
        FWaveLength:    double;
        { The mode the argument is shown in (axis_mode_registry), RESOLVED -
          never the automatic entry, which only the window can resolve because
          only it knows the model, the data and the selection. '' until the
          window says: the position as stored. }
        FArgumentMode:  string;
        { The user's own argument axis, for the custom mode. }
        FArgumentDefinition: TAxisDefinition;

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
    axis_choice;

function TMSCRCurveList.CreateAxis: TCoordinateAxis;
var
    NoPreferences: TAxisPreferences;
begin
    //  NOTHING CHOSE, so the position as stored - built here, not looked up.
    //  The compute server formats parameters through this list too and never
    //  registers a mode: asking the registry there would raise over a value
    //  that needs no transform at all.
    if Trim(FArgumentMode) = '' then
        Exit(TNamedAxis.Create('Position', ''));
    //  No preferences: the mode is already resolved, and what the automatic
    //  rule would name the quantity changes only a caption, never a position.
    NoPreferences := Default(TAxisPreferences);
    Result := CreateAxisFor(adArgument, FArgumentMode, NoPreferences,
        FWaveLength, FArgumentDefinition);
end;

{ POSITIONS ONLY, and only through the argument axis. An Amplitude is not
  carried through the value axis although it is a value: it is a height above
  whatever lies beneath the curve, not a point on the vertical axis, so a
  logarithm of it would not be where the curve is drawn. }
function TMSCRCurveList.RecalcParamValue(P: TSpecialCurveParameter): double;
var
    Axis: TCoordinateAxis;
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
    Axis: TCoordinateAxis;
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

    TMSCRCurveList(Dest).FWaveLength         := FWaveLength;
    TMSCRCurveList(Dest).FArgumentMode       := FArgumentMode;
    TMSCRCurveList(Dest).FArgumentDefinition := FArgumentDefinition;
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
