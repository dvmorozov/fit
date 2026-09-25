// SPDX-License-Identifier: GPL-3.0-or-later
{ Headless round-trip test for persisted user settings (Settings_v1), through
  the same TXMLConfig calls the main form uses in ReadSettings/WriteSettings.
  The axes are not among them: they are the project's (project_ui_context). }
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
  published
    procedure PersistsMinimizerKind;
    procedure PersistsLossKind;
    procedure PersistsSelectedCurveType;
    procedure AnOlderSettingsFileHasNoCurveType;
    procedure AnOlderSettingsFileLoadsOntoTheCorrectedRFactor;
  end;

implementation

procedure TSettingsPersistenceTest.FindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if AClassName = Settings_v1.ClassName then
    ComponentClass := Settings_v1;
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

initialization
  RegisterTest('integration', TSettingsPersistenceTest);
end.
