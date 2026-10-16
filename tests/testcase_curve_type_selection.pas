// SPDX-License-Identifier: GPL-3.0-or-later
{ Selecting a curve type has to reach two places at once: the client-side
  registry, which draws the checkmark in Model \ Curve Type and names curves in
  the legend, and the compute server, which decides what is actually fitted.
  The menu handler used to update only the first, so the menu showed
  "Asym. Pseudo-Voigt" while the server went on fitting with its own default -
  the alphabetically first registered type, "2 br. Pseudo-Voigt" - and that is
  the name that came back in the results. TFitClient.SelectCurveType is the one
  entry point the UI may call; these tests pin down that it updates both. }
unit testcase_curve_type_selection;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry,
  fit_client, http_fit_service, named_points_set,
  curve_types_singleton, int_curve_type_selector, int_curve_type_iterator,
  //  Referencing these units links them, so their self-registration runs.
  gauss_points_set, asym_pseudo_voigt_points_set,
  two_branches_pseudo_voigt_points_set;

type
  { The real client-side service with the transport stubbed out: it records the
    curve type it was told about, so the test can see exactly what the client
    would have sent, without needing a server process. }
  TRecordingFitService = class(THttpFitService)
  public
    SentCurveTypeId: TCurveTypeId;
    SetCurveTypeCalls: integer;
    function GetCurveType: TCurveTypeId; override;
    procedure SetCurveType(ACurveTypeId: TCurveTypeId); override;
  end;

  TCurveTypeSelectionTest = class(TTestCase)
  private
    FClient: TFitClient;
    FSvc: TRecordingFitService;
    function SelectedName: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { The client-side half: the registry follows the selection. }
    procedure SelectingUpdatesTheClientRegistry;
    { The server-facing half: the selection is pushed to the server. }
    procedure SelectingTellsTheServer;
    { Both halves agree - the invariant the bug broke. }
    procedure TheMenuAndTheServerNeverDisagree;
    { The default the server falls back to is not the one the user picked, so
      forgetting to push really does change the fitted type. }
    procedure TheDefaultTypeIsNotTheSelectedOne;
  end;

implementation

function TRecordingFitService.GetCurveType: TCurveTypeId;
begin
  Result := SentCurveTypeId;
end;

procedure TRecordingFitService.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
  SentCurveTypeId := ACurveTypeId;
  Inc(SetCurveTypeCalls);
end;

procedure TCurveTypeSelectionTest.SetUp;
begin
  //  Nothing listens on this port: these tests must not touch the network.
  FSvc := TRecordingFitService.Create('http://127.0.0.1:9');
  FClient := TFitClient.Create;
  FClient.FitService := FSvc;
end;

procedure TCurveTypeSelectionTest.TearDown;
begin
  FreeAndNil(FClient);
  FreeAndNil(FSvc);
end;

{ The name the curve-type menu would show as checked. }
function TCurveTypeSelectionTest.SelectedName: string;
var
  Selector: ICurveTypeSelector;
  Iter: ICurveTypeIterator;
  Selected: TCurveTypeId;
begin
  Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
  Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
  Selected := Selector.GetSelectedCurveType;
  Result := '';
  Iter.FirstCurveType;
  while True do
  begin
    if IsEqualGUID(Iter.GetCurveTypeId, Selected) then
    begin
      Result := Iter.GetCurveTypeName;
      Break;
    end;
    if Iter.EndCurveType then Break
    else Iter.NextCurveType;
  end;
end;

procedure TCurveTypeSelectionTest.SelectingUpdatesTheClientRegistry;
begin
  FClient.SelectCurveType(TAsymPseudoVoigtPointsSet.GetCurveTypeId);
  AssertEquals('the menu checks the type the user picked',
    'Asym. Pseudo-Voigt', SelectedName);

  //  And it follows a second pick, rather than latching on the first.
  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertEquals('the menu follows the next pick', 'Gaussian', SelectedName);
end;

procedure TCurveTypeSelectionTest.SelectingTellsTheServer;
begin
  FClient.SelectCurveType(TAsymPseudoVoigtPointsSet.GetCurveTypeId);
  AssertEquals('the server was told once', 1, FSvc.SetCurveTypeCalls);
  AssertTrue('the server got the selected type',
    IsEqualGUID(TAsymPseudoVoigtPointsSet.GetCurveTypeId, FSvc.SentCurveTypeId));

  FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertTrue('and the next one', IsEqualGUID(
    TGaussPointsSet.GetCurveTypeId, FSvc.SentCurveTypeId));
end;

procedure TCurveTypeSelectionTest.TheMenuAndTheServerNeverDisagree;
var
  Selector: ICurveTypeSelector;
begin
  Selector := TCurveTypesSingleton.CreateCurveTypeSelector;

  FClient.SelectCurveType(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
  AssertTrue('menu and server agree', IsEqualGUID(
    Selector.GetSelectedCurveType, FSvc.SentCurveTypeId));

  FClient.SelectCurveType(TAsymPseudoVoigtPointsSet.GetCurveTypeId);
  AssertTrue('menu and server still agree after a change', IsEqualGUID(
    Selector.GetSelectedCurveType, FSvc.SentCurveTypeId));
  AssertEquals('and it is what the user picked', 'Asym. Pseudo-Voigt',
    SelectedName);
end;

procedure TCurveTypeSelectionTest.TheDefaultTypeIsNotTheSelectedOne;
var
  Iter: ICurveTypeIterator;
begin
  //  Registration selects the alphabetically first type by default, on the
  //  server just as on the client. If that happened to be the type the tests
  //  pick, an unpushed selection would look correct by accident.
  Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
  Iter.FirstCurveType;
  AssertFalse('the default is not the type these tests select',
    IsEqualGUID(Iter.GetCurveTypeId,
      TAsymPseudoVoigtPointsSet.GetCurveTypeId));
end;

initialization
  //  UNIT: TRecordingFitService is a THttpFitService with its transport
  //  overridden to record instead of send, so nothing leaves the process.
  RegisterTest('unit', TCurveTypeSelectionTest);
end.
