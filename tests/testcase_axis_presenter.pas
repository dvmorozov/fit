// SPDX-License-Identifier: GPL-3.0-or-later
{ Presenter-level test for axis selection: when the user picks a mode (the Data
  > Argument Transformation and Data > Value Transformation menus, or the
  chart's own), the window must build the matching axis, write its title on the
  chart, caption the pointer's readout with it and tick the entry. All of that
  is read off TChartAxes, which is what the form asks - so testing it here
  covers the presenter path headlessly. The defects this pins were each a
  caption that disagreed with the transform, or a readout still saying
  "Intensity" over a price. }
unit testcase_axis_presenter;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  coordinate_axis, axis_mode_registry, axis_mode_registration, axis_choice,
  diffraction_axis_modes, chart_axes;
type
  TAxisPresenterTest = class(TTestCase)
  private
    FAxes: TChartAxes;
    function TitleOf(ADimension: TAxisDimension; const AModeId: string): string;
    function Prefs(const AData: string): TAxisPreferences;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GeneralPositionLabelsPositionWithNoUnit;
    procedure TwoThetaModeLabelsDegrees;
    procedure ThetaModeLabelsDegrees;
    procedure SinThetaOverLambdaModeLabelsReciprocalAngstrom;
    procedure CustomModeUsesUserNameAndUnit;
    procedure CustomModeWithoutUnitOmitsBrackets;
    procedure SelectedAxisMatchesTheModesTransform;
    //  The value axis, which had no modes at all.
    procedure TheValueAxisIsNamedByTheData;
    procedure ALogarithmicValueAxisDrawsTheLogAndReadsTheValue;
    procedure ACustomValueAxisIsNotTheArgumentsCustomAxis;
    //  The readout beside the chart.
    procedure TheReadoutIsCaptionedByTheAxisInForce;
    procedure DatesChangeNoAxisThatDoesNotReadThem;
    //  The menu.
    procedure AutomaticStaysTickedWhileItResolves;
    procedure ChoosingAnEntryTicksItAndMakesItTheUsers;
    procedure ChoosingNothingIsChoosingAutomatic;
    procedure OnlyAnAngleReadsTheWavelength;
    procedure SinThetaOverLambdaAsksForAWavelengthOnlyWithoutOne;
    procedure ACustomAxisAlwaysOpensItsDialog;
    procedure AnOrdinaryModeNeedsNothing;
    //  When the window has to redraw.
    procedure NewDataSayingSomethingElseIsAChange;
    procedure TheSameAnswerAgainIsNoChange;
    procedure AChosenAxisIsNotChangedByNewData;
    procedure ANewQuantityUnderALogAxisIsAChangeOfTitle;
    //  A new document.
    procedure ResettingPutsBothAxesBackOnAutomatic;
  end;

implementation

const
  //  A representative wavelength for the sin(theta)/lambda mode.
  CuKa = 1.54056;

procedure TAxisPresenterTest.SetUp;
begin
  RegisterAllAxisModes;
  FAxes := TChartAxes.Create;
  FAxes.SetWaveLength(CuKa);
end;

procedure TAxisPresenterTest.TearDown;
begin
  FreeAndNil(FAxes);
end;

function TAxisPresenterTest.TitleOf(ADimension: TAxisDimension;
  const AModeId: string): string;
begin
  FAxes.Choose(ADimension, AModeId);
  Result := FAxes.Axis(ADimension).Title;
end;

function TAxisPresenterTest.Prefs(const AData: string): TAxisPreferences;
begin
  Result.Model := nil;
  Result.ModelFallback := nil;
  Result.Data := AData;
  Result.SelectedType := '';
end;

procedure TAxisPresenterTest.GeneralPositionLabelsPositionWithNoUnit;
begin
  //  The general default: raw argument, no wavelength, no unit.
  AssertEquals('identity label', 'Position', TitleOf(adArgument, PositionAxisModeId));
end;

procedure TAxisPresenterTest.TwoThetaModeLabelsDegrees;
begin
  AssertEquals('2*Theta label', '2*Theta [deg]', TitleOf(adArgument, TwoThetaAxisModeId));
end;

procedure TAxisPresenterTest.ThetaModeLabelsDegrees;
begin
  AssertEquals('Theta label', 'Theta [deg]', TitleOf(adArgument, ThetaAxisModeId));
end;

procedure TAxisPresenterTest.SinThetaOverLambdaModeLabelsReciprocalAngstrom;
begin
  AssertEquals('Sin(Theta)/Lambda label', 'Sin(Theta)/Lambda [1/A]',
    TitleOf(adArgument, SinThetaOverLambdaAxisModeId));
end;

procedure TAxisPresenterTest.CustomModeUsesUserNameAndUnit;
var
  Definition: TAxisDefinition;
begin
  Definition.Name := 'Energy';
  Definition.UnitName := 'eV';
  Definition.Forward := 'x';
  Definition.Inverse := 'x';
  FAxes.SetDefinition(adArgument, Definition);
  AssertEquals('custom label', 'Energy [eV]', TitleOf(adArgument, CustomAxisModeId));
end;

procedure TAxisPresenterTest.CustomModeWithoutUnitOmitsBrackets;
var
  Definition: TAxisDefinition;
begin
  //  An empty unit must not produce a dangling '[]'.
  Definition.Name := 'Log';
  Definition.UnitName := '';
  Definition.Forward := 'ln(x)';
  Definition.Inverse := 'exp(x)';
  FAxes.SetDefinition(adArgument, Definition);
  AssertEquals('custom label, no unit', 'Log', TitleOf(adArgument, CustomAxisModeId));
end;

procedure TAxisPresenterTest.SelectedAxisMatchesTheModesTransform;
begin
  //  Selecting Theta must build an axis that halves 2*Theta (raw is 2*Theta deg),
  //  i.e. the label and the transform come from the same selection - the property
  //  the main form relies on when it relabels and replots together.
  FAxes.Choose(adArgument, ThetaAxisModeId);
  AssertEquals('theta halves 2theta', 30.0, FAxes.Axis(adArgument).ToDisplay(60.0), 1e-9);
  AssertEquals('theta name', 'Theta', FAxes.Axis(adArgument).DisplayName);
end;

procedure TAxisPresenterTest.TheValueAxisIsNamedByTheData;
begin
  FAxes.SetPreferences(adValue, Prefs(IntensityAxisModeId));
  AssertEquals('Intensity', FAxes.Axis(adValue).Title);
  FAxes.SetPreferences(adValue, Prefs(''));
  AssertEquals('follows the data it is told about', 'Value',
    FAxes.Axis(adValue).Title);
end;

procedure TAxisPresenterTest.ALogarithmicValueAxisDrawsTheLogAndReadsTheValue;
begin
  FAxes.SetPreferences(adValue, Prefs(IntensityAxisModeId));
  FAxes.Choose(adValue, LogarithmicAxisModeId);
  AssertEquals('Intensity, log scale', FAxes.Axis(adValue).Title);
  AssertEquals('drawn at the logarithm', 3.0, FAxes.Axis(adValue).ToDisplay(1000), 1e-12);
  AssertEquals('read in the value', '1000.00', FAxes.Axis(adValue).ReadoutText(3));
  AssertEquals('captioned by the quantity', 'Intensity:',
    FAxes.Axis(adValue).ReadoutCaption);
end;

procedure TAxisPresenterTest.ACustomValueAxisIsNotTheArgumentsCustomAxis;
var
  Definition: TAxisDefinition;
begin
  Definition.Name := 'Energy';
  Definition.UnitName := 'eV';
  Definition.Forward := 'x';
  Definition.Inverse := 'x';
  FAxes.SetDefinition(adArgument, Definition);
  Definition.Name := 'Counts';
  Definition.UnitName := 'k';
  Definition.Forward := 'x/1000';
  Definition.Inverse := 'x*1000';
  FAxes.SetDefinition(adValue, Definition);
  FAxes.Choose(adArgument, CustomAxisModeId);
  FAxes.Choose(adValue, CustomAxisModeId);
  AssertEquals('Energy [eV]', FAxes.Axis(adArgument).Title);
  AssertEquals('Counts [k]', FAxes.Axis(adValue).Title);
end;

procedure TAxisPresenterTest.TheReadoutIsCaptionedByTheAxisInForce;
begin
  //  The right-hand panel said "Position:" and "Intensity:" whatever was drawn.
  FAxes.SetPreferences(adArgument, Prefs(ThetaAxisModeId));
  FAxes.SetPreferences(adValue, Prefs(IntensityAxisModeId));
  AssertEquals('Theta:', FAxes.Axis(adArgument).ReadoutCaption);
  AssertEquals('Intensity:', FAxes.Axis(adValue).ReadoutCaption);
  FAxes.Choose(adArgument, TwoThetaAxisModeId);
  AssertEquals('and follows a choice', '2*Theta:', FAxes.Axis(adArgument).ReadoutCaption);
end;

procedure TAxisPresenterTest.DatesChangeNoAxisThatDoesNotReadThem;
var
  Dates: TAxisDates;
  Before: string;
begin
  //  The bars' dates go to whichever mode reads them - a module's date axis
  //  (TBarDateAxis) - and change nothing for one that does not.
  FAxes.SetPreferences(adArgument, Prefs(ThetaAxisModeId));
  Before := FAxes.Axis(adArgument).ReadoutText(2);
  SetLength(Dates, 3);
  Dates[0] := EncodeDate(2020, 9, 24);
  Dates[1] := EncodeDate(2020, 9, 25);
  Dates[2] := EncodeDate(2020, 9, 28);
  FAxes.SetArgumentDates(Dates);
  AssertEquals(Before, FAxes.Axis(adArgument).ReadoutText(2));
  AssertEquals('nor the value', 'Value', FAxes.Axis(adValue).Title);
end;

procedure TAxisPresenterTest.AutomaticStaysTickedWhileItResolves;
begin
  FAxes.SetPreferences(adArgument, Prefs(ThetaAxisModeId));
  AssertTrue('Automatic is ticked', FAxes.IsTicked(adArgument, AutomaticAxisModeId));
  AssertFalse('not the mode it resolves to', FAxes.IsTicked(adArgument, ThetaAxisModeId));
  AssertEquals(ThetaAxisModeId, FAxes.ResolvedModeId(adArgument));
end;

procedure TAxisPresenterTest.ChoosingAnEntryTicksItAndMakesItTheUsers;
begin
  FAxes.Choose(adValue, LogarithmicAxisModeId);
  AssertTrue(FAxes.IsTicked(adValue, LogarithmicAxisModeId));
  AssertFalse(FAxes.IsTicked(adValue, AutomaticAxisModeId));
  AssertTrue('remembered as a choice', FAxes.Choice(adValue).ChosenByUser);
  AssertFalse('the other coordinate is untouched', FAxes.Choice(adArgument).ChosenByUser);
end;

procedure TAxisPresenterTest.ChoosingNothingIsChoosingAutomatic;
begin
  FAxes.Choose(adValue, '');
  AssertEquals(AutomaticAxisModeId, FAxes.Choice(adValue).ModeId);
end;

procedure TAxisPresenterTest.OnlyAnAngleReadsTheWavelength;
begin
  //  Set Rule Parameters is offered exactly while this is true.
  FAxes.Choose(adArgument, ThetaAxisModeId);
  AssertTrue('theta', FAxes.ReadsWaveLength);
  FAxes.Choose(adArgument, PositionAxisModeId);
  AssertFalse('a plain position', FAxes.ReadsWaveLength);
  FAxes.Choose(adArgument, AutomaticAxisModeId);
  FAxes.SetPreferences(adArgument, Prefs(TwoThetaAxisModeId));
  AssertTrue('automatic, resolving to an angle', FAxes.ReadsWaveLength);
end;

procedure TAxisPresenterTest.SinThetaOverLambdaAsksForAWavelengthOnlyWithoutOne;
begin
  AssertTrue('one is known', FAxes.NeedsParameter(SinThetaOverLambdaAxisModeId) = apNone);
  FAxes.SetWaveLength(0);
  AssertTrue('none is known', FAxes.NeedsParameter(SinThetaOverLambdaAxisModeId) = apWaveLength);
  AssertTrue('theta never asks', FAxes.NeedsParameter(ThetaAxisModeId) = apNone);
end;

procedure TAxisPresenterTest.ACustomAxisAlwaysOpensItsDialog;
begin
  //  That is how its formulas are given, and changed.
  AssertTrue(FAxes.NeedsParameter(CustomAxisModeId) = apDefinition);
end;

procedure TAxisPresenterTest.AnOrdinaryModeNeedsNothing;
begin
  AssertTrue(FAxes.NeedsParameter(LogarithmicAxisModeId) = apNone);
  AssertTrue('nor does an unknown one', FAxes.NeedsParameter('vendor.absent') = apNone);
end;

procedure TAxisPresenterTest.NewDataSayingSomethingElseIsAChange;
begin
  FAxes.SetPreferences(adArgument, Prefs(ThetaAxisModeId));
  AssertTrue(FAxes.UpdatePreferences(adArgument, Prefs(TwoThetaAxisModeId)));
  AssertEquals('2*Theta [deg]', FAxes.Axis(adArgument).Title);
end;

procedure TAxisPresenterTest.TheSameAnswerAgainIsNoChange;
begin
  //  Every state refresh asks; only an answer that differs may redraw.
  FAxes.SetPreferences(adArgument, Prefs(ThetaAxisModeId));
  AssertFalse(FAxes.UpdatePreferences(adArgument, Prefs(ThetaAxisModeId)));
end;

procedure TAxisPresenterTest.AChosenAxisIsNotChangedByNewData;
begin
  FAxes.Choose(adArgument, ThetaAxisModeId);
  AssertFalse('the user''s choice is what is drawn',
    FAxes.UpdatePreferences(adArgument, Prefs(TwoThetaAxisModeId)));
end;

procedure TAxisPresenterTest.ANewQuantityUnderALogAxisIsAChangeOfTitle;
begin
  //  The mode stays Logarithmic; what it is a logarithm OF is new.
  FAxes.Choose(adValue, LogarithmicAxisModeId);
  FAxes.SetPreferences(adValue, Prefs(IntensityAxisModeId));
  AssertTrue(FAxes.UpdatePreferences(adValue, Prefs('')));
  AssertEquals('Value, log scale', FAxes.Axis(adValue).Title);
end;

procedure TAxisPresenterTest.ResettingPutsBothAxesBackOnAutomatic;
var
  Definition: TAxisDefinition;
begin
  //  A NEW PROJECT STARTS ON AUTOMATIC: the axes are the project's, and the
  //  last one's choice - or its formulas - must not follow into the next.
  Definition.Name := 'Energy';
  Definition.UnitName := 'eV';
  Definition.Forward := 'x';
  Definition.Inverse := 'x';
  FAxes.SetDefinition(adValue, Definition);
  FAxes.Choose(adArgument, ThetaAxisModeId);
  FAxes.Choose(adValue, CustomAxisModeId);
  FAxes.Reset;
  AssertEquals(AutomaticAxisModeId, FAxes.Choice(adArgument).ModeId);
  AssertEquals(AutomaticAxisModeId, FAxes.Choice(adValue).ModeId);
  AssertFalse('chosen by nobody', FAxes.Choice(adArgument).ChosenByUser);
  AssertFalse(FAxes.Choice(adValue).ChosenByUser);
  AssertEquals('and no formulas of the last one', '',
    FAxes.Choice(adValue).Definition.Forward);
  AssertEquals('drawn as automatic too', 'Position', FAxes.Axis(adArgument).Title);
end;

initialization
  RegisterTest('unit', TAxisPresenterTest);
end.
