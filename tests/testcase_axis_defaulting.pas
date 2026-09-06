// SPDX-License-Identifier: GPL-3.0-or-later
{ The x-axis must belong to the MODEL, not to a global setting. Before this, the
  argument axis was a document-wide mode defaulting to 2*Theta, so a non-diffraction
  wave pattern - fitted against a plain position - was captioned "2*Theta [deg]",
  and the positions in the parameters grid were reported in a diffraction angle
  that means nothing for that data.

  The fix gives every curve class its own axis (TNamedPointsSet.CreatePreferredAxis)
  and a display mode, XCM_CURVE, that asks the selected class for it. These tests
  drive the whole chain the way the UI does: they select a curve type through
  TFitClient.SelectCurveType - the one entry point the menu handler uses - and
  then assert the caption the user would actually read, via AxisLabelForMode.
  Asserting the LABEL rather than an internal value is deliberate: a break
  anywhere in class -> axis -> mode -> caption fails the test. }
unit testcase_axis_defaulting;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry,
  argument_axis, mscr_specimen_list, named_points_set, special_curve_parameter,
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
    FClient: TFitClient;
    FSvc: TSilentFitService;
    { The caption the chart would show under AMode for the current selection. }
    function LabelOf(AMode: longint): string;
    { The axis the grid would use to report positions under AMode. }
    function AxisOf(AMode: longint): TArgumentAxis;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { The reported defect, end to end. }
    procedure ADiffractionPeakIsShownOnTheScatteringAngle;
    { The axis really follows the model rather than latching on the first pick. }
    procedure TheAxisFollowsEveryChangeOfCurveType;
    { A formula entered by the user carries no diffraction meaning. }
    procedure AUserCurveIsShownOnAPlainPositionAxis;
    { An explicit choice is an override: it ignores the model's preference. }
    procedure AnExplicitModeOutranksTheCurveType;
    { Positions the grid shows must map back to the stored value exactly. }
    procedure PositionsRoundTripOnTheCurveDefinedAxis;
    { The parameters grid reports positions through the same curve-defined axis. }
    procedure TheGridReportsPositionsOnTheCurveDefinedAxis;
  end;

implementation

const
  //  A representative wavelength; only the diffraction family reads it.
  CuKa = 1.54056;

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
  //  Nothing listens on this port: these tests must not touch the network.
  FSvc := TSilentFitService.Create('http://127.0.0.1:9');
  FClient := TFitClient.Create;
  FClient.FitService := FSvc;
end;

procedure TAxisDefaultingTest.TearDown;
begin
  FreeAndNil(FClient);
  FreeAndNil(FSvc);
end;

function TAxisDefaultingTest.LabelOf(AMode: longint): string;
begin
  //  The custom-axis arguments matter only to XCM_CUSTOM.
  Result := AxisLabelForMode(AMode, CuKa, 'Custom', 'u', 'x', 'x');
end;

function TAxisDefaultingTest.AxisOf(AMode: longint): TArgumentAxis;
begin
  Result := CreateAxisForMode(AMode, CuKa, 'Custom', 'u', 'x', 'x');
end;

procedure TAxisDefaultingTest.ADiffractionPeakIsShownOnTheScatteringAngle;
begin
  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('a Gaussian peak keeps the diffraction axis',
    '2*Theta [deg]', LabelOf(XCM_CURVE));

  //  A lineshape defined through the shared formula base gets it too, so the
  //  preference is inherited rather than repeated per type.
  FClient.SelectCurveType(TPearson7PointsSet.GetCurveTypeId);
  AssertEquals('and so does a formula lineshape',
    '2*Theta [deg]', LabelOf(XCM_CURVE));
end;

procedure TAxisDefaultingTest.TheAxisFollowsEveryChangeOfCurveType;
begin
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('starts on the plain axis', 'Position', LabelOf(XCM_CURVE));

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('follows the switch to a peak',
    '2*Theta [deg]', LabelOf(XCM_CURVE));

  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('and back again - it does not latch',
    'Position', LabelOf(XCM_CURVE));
end;

procedure TAxisDefaultingTest.AUserCurveIsShownOnAPlainPositionAxis;
begin
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('a user formula has no inherent abscissa',
    'Position', LabelOf(XCM_CURVE));
end;

procedure TAxisDefaultingTest.AnExplicitModeOutranksTheCurveType;
begin
  //  Whatever the model would prefer, a mode the user picked from the menu is
  //  the mode that is shown.
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  AssertEquals('Theta stays Theta over a plain-axis curve type',
    'Theta [deg]', LabelOf(XCM_T));
  AssertEquals('and the user-defined axis stays too',
    'Custom [u]', LabelOf(XCM_CUSTOM));

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('the general axis stays over a peak as well',
    'Position', LabelOf(XCM_IDENTITY));
end;

procedure TAxisDefaultingTest.PositionsRoundTripOnTheCurveDefinedAxis;
const
  Raw = 37.25;
var
  Axis: TArgumentAxis;
begin
  //  This is the grid path: RecalcParamValue shows ToDisplay, the user edits,
  //  ReverseCalcParamValue stores FromDisplay. A mismatch silently moves a peak.
  FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
  Axis := AxisOf(XCM_CURVE);
  try
    AssertEquals('a plain-axis position round-trips', Raw,
      Axis.FromDisplay(Axis.ToDisplay(Raw)), 1e-9);
  finally
    Axis.Free;
  end;

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  Axis := AxisOf(XCM_CURVE);
  try
    AssertEquals('peak position round-trips', Raw,
      Axis.FromDisplay(Axis.ToDisplay(Raw)), 1e-9);
  finally
    Axis.Free;
  end;
end;

procedure TAxisDefaultingTest.TheGridReportsPositionsOnTheCurveDefinedAxis;
const
  Raw = 40.0;
var
  List: TGridCurveList;
  Param: TTestPositionParameter;
begin
  //  The caption is only half the defect: the numbers in the Curve Positions
  //  grid went through the same global mode, so a plain-axis position at 40 was
  //  reported as an angle. The grid must read the axis off the model too.
  List := TGridCurveList.Create;
  Param := TTestPositionParameter.Create;
  try
    List.FViewMode := XCM_CURVE;
    List.FWaveLength := CuKa;
    Param.Value := Raw;

    FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
    AssertEquals('a plain-axis position is shown as itself',
      Raw, List.ShownValue(Param), 1e-9);

    FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    AssertEquals('a peak is shown in 2*Theta, which is how it is stored',
      Raw, List.ShownValue(Param), 1e-9);

    //  Editing the shown value must store back the raw value the fit uses.
    FClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
    List.StoreEditedValue(Param, 55.5);
    AssertEquals('an edited position is stored as entered',
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
  //  It was in the slow half because it is nogui-only, which is a property of the
  //  BINARY it links into rather than of what it depends on; the two were
  //  conflated. Coverage is measured over the unit half, which is why the axis
  //  defaulting logic read as untested.
  RegisterTest('unit', TAxisDefaultingTest);
end.
