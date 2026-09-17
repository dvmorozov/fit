// SPDX-License-Identifier: GPL-3.0-or-later
{ Headless round-trip test for persisted user settings (Settings_v1), pinning the
  argument-axis persistence (ViewMode) through the same TXMLConfig calls the main
  form uses in ReadSettings/WriteSettings. }
unit testcase_settings_persistence;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, Laz_XMLCfg, Laz_XMLStreaming,
  app_settings, fit_loss, mscr_specimen_list;
type
  TSettingsPersistenceTest = class(TTestCase)
  private
    procedure FindClass(Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
    function RoundTrip(AViewMode: longint): longint;
  published
    procedure PersistsIdentityViewMode;
    procedure PersistsDefaultViewMode;
    procedure PersistsCustomAxisDefinition;
    procedure PersistsMinimizerKind;
    procedure PersistsLossKind;
    procedure PersistsSelectedCurveType;
    procedure AnOlderSettingsFileHasNoCurveType;
    procedure AnOlderSettingsFileLoadsOntoTheCorrectedRFactor;
    procedure AnExplicitAxisChoiceSurvivesARestart;
    procedure AnOlderSettingsFileFallsBackToTheCurveDefinedAxis;
  end;

implementation

procedure TSettingsPersistenceTest.FindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if AClassName = Settings_v1.ClassName then
    ComponentClass := Settings_v1;
end;

{ Writes a Settings_v1 with the given ViewMode to a temp XML file, reads it back
  into a fresh instance and returns the recovered ViewMode. }
function TSettingsPersistenceTest.RoundTrip(AViewMode: longint): longint;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
begin
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.ViewMode := AViewMode;
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    Result := Loaded.ViewMode;
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

procedure TSettingsPersistenceTest.PersistsIdentityViewMode;
begin
  //  XCM_IDENTITY = 3
  AssertEquals('identity view mode survives round-trip', 3, RoundTrip(3));
end;

procedure TSettingsPersistenceTest.PersistsDefaultViewMode;
begin
  //  XCM_2T = 0 (legacy default)
  AssertEquals('default view mode survives round-trip', 0, RoundTrip(0));
end;

procedure TSettingsPersistenceTest.PersistsCustomAxisDefinition;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
begin
  //  The user-defined (XCM_CUSTOM) axis formulas must survive a restart, else the
  //  restored custom mode would come up undefined.
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.ViewMode := 4;   //  XCM_CUSTOM
    Saved.CustomAxisName := 'Log';
    Saved.CustomAxisUnit := 'u';
    Saved.CustomAxisForward := 'ln(x)';
    Saved.CustomAxisInverse := 'exp(x)';
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    AssertEquals('view mode', 4, Loaded.ViewMode);
    AssertEquals('forward formula', 'ln(x)', Loaded.CustomAxisForward);
    AssertEquals('inverse formula', 'exp(x)', Loaded.CustomAxisInverse);
    AssertEquals('name', 'Log', Loaded.CustomAxisName);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

procedure TSettingsPersistenceTest.PersistsMinimizerKind;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
begin
  //  The chosen minimizer (MIN_KIND_* constant) must survive a restart. Uses an
  //  arbitrary non-default value (1) to prove the field round-trips, independent
  //  of which algorithms exist today.
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.MinimizerKind := 1;
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    AssertEquals('minimizer kind', 1, Loaded.MinimizerKind);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

{ The curve type the last session ended on must come back, or every session
  starts on whatever the registry happens to list first - which is how a user
  who works exclusively with one model still has to re-pick it every time. }
procedure TSettingsPersistenceTest.PersistsSelectedCurveType;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
const
  ID = '{B1E4A6D2-5C37-4A1E-9F68-2D70C4A1F001}';
begin
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.SelectedCurveType := ID;
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    AssertEquals('the curve type survives a restart', ID,
      Loaded.SelectedCurveType);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

{ A settings file written before this existed has no curve type in it, so the
  property is never assigned and keeps its constructed value. That value must be
  EMPTY - "never chosen" - so the registry default applies, rather than some id
  that would silently move an existing user onto a different model. }
procedure TSettingsPersistenceTest.AnOlderSettingsFileHasNoCurveType;
var
  Fresh: Settings_v1;
begin
  Fresh := Settings_v1.Create(nil);
  try
    AssertEquals('an unset curve type means "use the default"', '',
      Fresh.SelectedCurveType);
  finally
    Fresh.Free;
  end;
end;

procedure TSettingsPersistenceTest.PersistsLossKind;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
begin
  //  The chosen objective must survive a restart, like the minimizer beside it.
  //  Uses a non-default kind, so a field that silently failed to round-trip
  //  would come back as the default and be caught.
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.LossKind := LOSS_KIND_RELATIVE;
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @FindClass, nil);
    finally
      Cfg.Free;
    end;
    AssertEquals('loss kind', LOSS_KIND_RELATIVE, Loaded.LossKind);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

{ A settings file written before the objective was selectable has no such entry,
  so the field keeps its constructed value. That value must be the objective we
  would have chosen - which is why LOSS_KIND_RFACTOR is 0 rather than the
  historical form. An upgrade must not quietly move anyone onto a worse
  objective. }
procedure TSettingsPersistenceTest.AnOlderSettingsFileLoadsOntoTheCorrectedRFactor;
var
  S: Settings_v1;
begin
  S := Settings_v1.Create(nil);
  try
    AssertEquals('a settings object that was never told which objective to use',
      LOSS_KIND_RFACTOR, S.LossKind);
  finally
    S.Free;
  end;
end;

{ The axis a restart would come up on for a stored (mode, chosen) pair. Writes
  and reads the settings for real, then applies the rule the main form applies. }
function RestoredMode(AViewMode: longint; AChosenByUser: boolean;
  ATest: TSettingsPersistenceTest): longint;
var
  FileName: string;
  Cfg: TXMLConfig;
  Saved, Loaded: Settings_v1;
begin
  FileName := GetTempFileName('', 'fitset') + '.xml';
  Saved := Settings_v1.Create(nil);
  try
    Saved.ViewMode := AViewMode;
    Saved.ViewModeChosenByUser := AChosenByUser;
    Cfg := TXMLConfig.Create(FileName);
    try
      WriteComponentToXMLConfig(Cfg, 'Component', Saved);
      Cfg.Flush;
    finally
      Cfg.Free;
    end;
  finally
    Saved.Free;
  end;

  Loaded := Settings_v1.Create(nil);
  try
    Cfg := TXMLConfig.Create(FileName);
    try
      ReadComponentFromXMLConfig(Cfg, 'Component', TComponent(Loaded),
        @ATest.FindClass, nil);
    finally
      Cfg.Free;
    end;
    Result := EffectiveViewMode(Loaded.ViewMode, Loaded.ViewModeChosenByUser);
  finally
    Loaded.Free;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

procedure TSettingsPersistenceTest.AnExplicitAxisChoiceSurvivesARestart;
begin
  //  Once the user has picked an axis from the menu it must outrank whatever the
  //  selected curve type would prefer - on this run and on every later one.
  AssertEquals('a chosen Theta axis comes back as Theta',
    XCM_T, RestoredMode(XCM_T, True, Self));
  AssertEquals('a chosen general axis comes back as the general axis',
    XCM_IDENTITY, RestoredMode(XCM_IDENTITY, True, Self));
end;

procedure TSettingsPersistenceTest.AnOlderSettingsFileFallsBackToTheCurveDefinedAxis;
begin
  //  A file written before this existed carries ViewMode = XCM_2T merely because
  //  that was the hard-coded default, and no record of a choice. Reading it as a
  //  deliberate choice would caption every non-diffraction curve '2*Theta' forever -
  //  the very defect this change removes.
  AssertEquals('an unchosen 2*Theta yields the curve-defined axis',
    XCM_CURVE, RestoredMode(XCM_2T, False, Self));
end;

initialization
  RegisterTest('integration', TSettingsPersistenceTest);
end.
