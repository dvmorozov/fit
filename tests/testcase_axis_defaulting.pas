// SPDX-License-Identifier: GPL-3.0-or-later
{ The axes must belong to the MODEL AND THE DATA, not to a global setting and
  not to whatever the Tools list happens to select.

  Two defects, one after the other. First the argument axis was a document-wide
  mode defaulting to 2*Theta, so a wave pattern fitted against a plain position
  was captioned "2*Theta [deg]". The fix asked the SELECTED curve type - and a
  model of wave patterns over a price series was still captioned "2*Theta
  [deg]", because the Tools list still had a diffraction peak selected, while
  the value was "Intensity" whatever it was.

  These tests drive the chain the way the window does: a curve type selected
  through TFitClient.SelectCurveType (the call the menu handler makes), the
  model's curves held by the client as a refresh from the server leaves them,
  the data's coordinates as the loader said them, TFitClient.AxisPreferences,
  and TChartAxes - asserting the TITLE the user would read. A break anywhere in
  type -> preference -> rule -> axis -> caption fails a test here. }
unit testcase_axis_defaulting;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry,
  coordinate_axis, axis_mode_registry, axis_mode_registration, axis_choice,
  diffraction_axis_modes, chart_axes,
  mscr_specimen_list, named_points_set, special_curve_parameter,
  title_points_set, self_copied_component,
  fit_client, http_fit_service,
  //  Referencing these units links them, so their self-registration runs.
  gauss_points_set, pearson7_points_set, user_points_set;

type
  { The real client with the transport stubbed out, so selecting a type goes
    through exactly the code the menu handler calls, without a server process. }
  TSilentFitService = class(THttpFitService)
  public
    SentCurveTypeId: TCurveTypeId;
    function GetCurveType: TCurveTypeId; override;
    procedure SetCurveType(ACurveTypeId: TCurveTypeId); override;
  end;

  { Holds curves the way a refresh from the server leaves them: by title, the
    only statement of a curve's type the client receives. }
  TModelClient = class(TFitClient)
  public
    procedure HoldCurvesTitled(const ATitles: array of string);
  end;

  { A position parameter, which is what the grid transforms. Only the members
    the transform reads are meaningful; the optimiser hooks are never called. }
  TTestPositionParameter = class(TSpecialCurveParameter)
  public
    constructor Create;
    function CreateCopy: TSpecialCurveParameter; override;
    procedure InitVariationStep; override;
    procedure InitValue; override;
    function MinimumStepAchieved: boolean; override;
  end;

  { Exposes the two protected hooks the parameters grid calls, so the displayed
    position can be tested without a window. }
  TGridCurveList = class(TMSCRCurveList)
  public
    function ShownValue(P: TSpecialCurveParameter): double;
    procedure StoreEditedValue(P: TSpecialCurveParameter; NewValue: double);
  end;

  TAxisDefaultingTest = class(TTestCase)
  private
    FClient: TModelClient;
    FSvc: TSilentFitService;
    FAxes: TChartAxes;
    { The title the chart would show for ADimension, with the client's
      preferences handed to the axes as the window hands them. }
    function TitleOf(ADimension: TAxisDimension): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { The first defect, end to end. }
    procedure ADiffractionPeakIsShownOnTheScatteringAngle;
    { The axis really follows the selection while nothing is placed. }
    procedure TheAxisFollowsEveryChangeOfCurveTypeOverAnEmptyModel;
    { A formula entered by the user carries no field's meaning. }
    procedure AUserCurveIsShownOnAPlainPositionAxis;
    { The second defect: a model of other curves outranks the selection. }
    procedure AModelOfOtherCurvesIsNotCaptionedByTheSelectedPeak;
    procedure APlacedPeakKeepsItsAxisWhateverIsSelected;
    { The data speaks when the model does not. }
    procedure ADiffractionProfileSaysSoBeforeAnyPeakIsPlaced;
    { An explicit choice is an override: it ignores every preference. }
    procedure AnExplicitModeOutranksTheModelAndTheData;
    { Positions the grid shows must map back to the stored value exactly. }
    procedure PositionsRoundTripOnTheAutomaticAxis;
    { The parameters grid reports positions through the axis in force. }
    procedure TheGridReportsPositionsOnTheAxisInForce;
  end;

implementation

const
  //  A representative wavelength; only the diffraction family reads it.
  CuKa = 1.54056;

procedure TModelClient.HoldCurvesTitled(const ATitles: array of string);
var
  Curves: TSelfCopiedCompList;
  Curve: TTitlePointsSet;
  i: longint;
begin
  Curves := TSelfCopiedCompList.Create;
  for i := 0 to High(ATitles) do
  begin
    Curve := TTitlePointsSet.Create(nil);
    Curve.FTitle := ATitles[i];
    Curves.Add(Curve);
  end;
  ReplaceCurves(Curves);
end;

constructor TTestPositionParameter.Create;
begin
  inherited Create;
  FName := 'x0';
  FType := VariablePosition;
end;

function TTestPositionParameter.CreateCopy: TSpecialCurveParameter;
begin
  Result := TTestPositionParameter.Create;
  CopyTo(Result);
end;

procedure TTestPositionParameter.InitVariationStep; begin end;
procedure TTestPositionParameter.InitValue; begin end;
function TTestPositionParameter.MinimumStepAchieved: boolean; begin Result := True; end;

function TGridCurveList.ShownValue(P: TSpecialCurveParameter): double;
begin
  Result := RecalcParamValue(P);
end;

procedure TGridCurveList.StoreEditedValue(P: TSpecialCurveParameter;
  NewValue: double);
begin
  ReverseCalcParamValue(P, NewValue);
end;

function TSilentFitService.GetCurveType: TCurveTypeId;
begin
  Result := SentCurveTypeId;
end;

procedure TSilentFitService.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
  SentCurveTypeId := ACurveTypeId;
end;

procedure TAxisDefaultingTest.SetUp;
begin
  RegisterAllAxisModes;
  //  Nothing listens on this port: these tests must not touch the network.
  FSvc := TSilentFitService.Create('http://127.0.0.1:9');
  FClient := TModelClient.Create;
  FClient.FitService := FSvc;
  FAxes := TChartAxes.Create;
  FAxes.SetWaveLength(CuKa);
end;

procedure TAxisDefaultingTest.TearDown;
begin
  FreeAndNil(FAxes);
  FreeAndNil(FClient);
  FreeAndNil(FSvc);
end;

function TAxisDefaultingTest.TitleOf(ADimension: TAxisDimension): string;
begin
  FAxes.SetPreferences(ADimension, FClient.AxisPreferences(ADimension));
  Result := FAxes.Axis(ADimension).Title;
end;

procedure TAxisDefaultingTest.ADiffractionPeakIsShownOnTheScatteringAngle;
begin
  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('a Gaussian peak keeps the diffraction axis',
    '2*Theta [deg]', TitleOf(adArgument));
  AssertEquals('and its value is an intensity', 'Intensity', TitleOf(adValue));

  //  A lineshape defined through the shared formula base gets it too, so the
  //  preference is inherited rather than repeated per type.
  FClient.SelectCurveType(TPearson7PointsSet.GetCurveTypeId);
  AssertEquals('and so does a formula lineshape',
    '2*Theta [deg]', TitleOf(adArgument));
end;

procedure TAxisDefaultingTest.TheAxisFollowsEveryChangeOfCurveTypeOverAnEmptyModel;
begin
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('starts on the plain axis', 'Position', TitleOf(adArgument));

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('follows the switch to a peak',
    '2*Theta [deg]', TitleOf(adArgument));

  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('and back again - it does not latch',
    'Position', TitleOf(adArgument));
end;

procedure TAxisDefaultingTest.AUserCurveIsShownOnAPlainPositionAxis;
begin
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('a user formula has no inherent abscissa',
    'Position', TitleOf(adArgument));
  AssertEquals('nor an inherent value', 'Value', TitleOf(adValue));
end;

procedure TAxisDefaultingTest.AModelOfOtherCurvesIsNotCaptionedByTheSelectedPeak;
begin
  //  The screenshot's situation, with curves of a type this build does not
  //  know standing in for a module's patterns: curves are placed, a peak is
  //  selected.
  FClient.HoldCurvesTitled(['Unregistered pattern [1]', 'Unregistered pattern [2]']);
  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('the selection says what is placed NEXT, not what is placed',
    'Position', TitleOf(adArgument));
  AssertEquals('Value', TitleOf(adValue));
end;

procedure TAxisDefaultingTest.APlacedPeakKeepsItsAxisWhateverIsSelected;
begin
  FClient.HoldCurvesTitled([TGaussPointsSet.GetCurveTypeName + ' [1]']);
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('the model speaks', '2*Theta [deg]', TitleOf(adArgument));
  AssertEquals('Intensity', TitleOf(adValue));
end;

procedure TAxisDefaultingTest.ADiffractionProfileSaysSoBeforeAnyPeakIsPlaced;
begin
  FClient.SetDataCoordinateMode(adArgument, TwoThetaAxisModeId);
  FClient.SetDataCoordinateMode(adValue, IntensityAxisModeId);
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('2*Theta [deg]', TitleOf(adArgument));
  AssertEquals('Intensity', TitleOf(adValue));
end;

procedure TAxisDefaultingTest.AnExplicitModeOutranksTheModelAndTheData;
begin
  FClient.SetDataCoordinateMode(adArgument, SinThetaOverLambdaAxisModeId);
  FClient.HoldCurvesTitled([TGaussPointsSet.GetCurveTypeName + ' [1]']);
  FAxes.Choose(adArgument, ThetaAxisModeId);
  AssertEquals('Theta stays Theta', 'Theta [deg]', TitleOf(adArgument));
  FAxes.Choose(adArgument, PositionAxisModeId);
  AssertEquals('the general axis stays too', 'Position', TitleOf(adArgument));
end;

procedure TAxisDefaultingTest.PositionsRoundTripOnTheAutomaticAxis;
const
  Raw = 37.25;
begin
  //  This is the grid path: RecalcParamValue shows ToDisplay, the user edits,
  //  ReverseCalcParamValue stores FromDisplay. A mismatch silently moves a peak.
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  TitleOf(adArgument);
  AssertEquals('a plain-axis position round-trips', Raw,
    FAxes.Axis(adArgument).FromDisplay(FAxes.Axis(adArgument).ToDisplay(Raw)), 1e-9);

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  TitleOf(adArgument);
  AssertEquals('peak position round-trips', Raw,
    FAxes.Axis(adArgument).FromDisplay(FAxes.Axis(adArgument).ToDisplay(Raw)), 1e-9);
end;

procedure TAxisDefaultingTest.TheGridReportsPositionsOnTheAxisInForce;
const
  Raw = 40.0;
var
  List: TGridCurveList;
  Param: TTestPositionParameter;
begin
  //  The caption is only half the defect: the numbers in the Curve Attributes
  //  grid go through the same axis, so the grid is handed the RESOLVED mode -
  //  the window's job, done here as the window does it.
  List := TGridCurveList.Create;
  Param := TTestPositionParameter.Create;
  try
    List.FWaveLength := CuKa;
    Param.Value := Raw;

    FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
    TitleOf(adArgument);
    List.FArgumentMode := FAxes.ResolvedModeId(adArgument);
    AssertEquals('a plain-axis position is shown as itself',
      Raw, List.ShownValue(Param), 1e-9);

    FAxes.Choose(adArgument, ThetaAxisModeId);
    List.FArgumentMode := FAxes.ResolvedModeId(adArgument);
    AssertEquals('on Theta, half the stored 2*Theta', Raw / 2,
      List.ShownValue(Param), 1e-9);

    //  Editing the shown value must store back the raw value the fit uses.
    List.StoreEditedValue(Param, 27.75);
    AssertEquals('an edited position is stored in 2*Theta',
      55.5, Param.Value, 1e-9);
  finally
    Param.Free;
    List.Free;
  end;
end;

initialization
  //  UNIT, not integration. Every test here drives the client through
  //  TSilentFitService - a THttpFitService descendant whose transport methods are
  //  overridden to do nothing - so no socket is opened and no server is needed.
  RegisterTest('unit', TAxisDefaultingTest);
end.
