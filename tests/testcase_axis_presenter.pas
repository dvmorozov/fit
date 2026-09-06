// SPDX-License-Identifier: GPL-3.0-or-later
{ Presenter-level test for argument-axis selection: when the user picks a display
  mode (the Data -> Argument Transformation menu), the app must build the matching
  IArgumentAxis and show the right axis label. This pins that selection logic
  headlessly - it is exactly the wiring whose defects (wrong caption, wrong axis)
  the earlier UI bugs showed - without instantiating a window. The label + axis
  factory are single-sourced in mscr_specimen_list and reused by the main form's
  UpdateAxisLabel, so testing them here covers the presenter path. }
unit testcase_axis_presenter;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, argument_axis, mscr_specimen_list;
type
  TAxisPresenterTest = class(TTestCase)
  private
    function LabelOf(AMode: longint): string;
  published
    procedure IdentityModeLabelsPositionWithNoUnit;
    procedure TwoThetaModeLabelsDegrees;
    procedure ThetaModeLabelsDegrees;
    procedure SinThetaOverLambdaModeLabelsReciprocalAngstrom;
    procedure CustomModeUsesUserNameAndUnit;
    procedure CustomModeWithoutUnitOmitsBrackets;
    procedure SelectedAxisMatchesTheModesTransform;
  end;

implementation

const
  //  A representative wavelength for the sin(theta)/lambda mode.
  CuKa = 1.54056;

function TAxisPresenterTest.LabelOf(AMode: longint): string;
begin
  //  Non-custom modes ignore the custom fields.
  Result := AxisLabelForMode(AMode, CuKa, '', '', '', '');
end;

procedure TAxisPresenterTest.IdentityModeLabelsPositionWithNoUnit;
begin
  //  The general default: raw argument, no wavelength, no unit.
  AssertEquals('identity label', 'Position', LabelOf(XCM_IDENTITY));
end;

procedure TAxisPresenterTest.TwoThetaModeLabelsDegrees;
begin
  AssertEquals('2*Theta label', '2*Theta [deg]', LabelOf(XCM_2T));
end;

procedure TAxisPresenterTest.ThetaModeLabelsDegrees;
begin
  AssertEquals('Theta label', 'Theta [deg]', LabelOf(XCM_T));
end;

procedure TAxisPresenterTest.SinThetaOverLambdaModeLabelsReciprocalAngstrom;
begin
  AssertEquals('Sin(Theta)/Lambda label', 'Sin(Theta)/Lambda [1/A]',
    LabelOf(XCM_SINTL));
end;

procedure TAxisPresenterTest.CustomModeUsesUserNameAndUnit;
begin
  AssertEquals('custom label', 'Energy [eV]',
    AxisLabelForMode(XCM_CUSTOM, 0, 'Energy', 'eV', 'x', 'x'));
end;

procedure TAxisPresenterTest.CustomModeWithoutUnitOmitsBrackets;
begin
  //  An empty unit must not produce a dangling '[]'.
  AssertEquals('custom label, no unit', 'Log',
    AxisLabelForMode(XCM_CUSTOM, 0, 'Log', '', 'ln(x)', 'exp(x)'));
end;

procedure TAxisPresenterTest.SelectedAxisMatchesTheModesTransform;
var
  Axis: TArgumentAxis;
begin
  //  Selecting Theta must build an axis that halves 2*Theta (raw is 2*Theta deg),
  //  i.e. the label and the transform come from the same selection - the property
  //  the main form relies on when it relabels and replots together.
  Axis := CreateAxisForMode(XCM_T, CuKa, '', '', '', '');
  try
    AssertEquals('theta halves 2theta', 30.0, Axis.ToDisplay(60.0), 1e-9);
    AssertEquals('theta name', 'Theta', Axis.DisplayName);
  finally
    Axis.Free;
  end;
end;

initialization
  RegisterTest('unit', TAxisPresenterTest);
end.
