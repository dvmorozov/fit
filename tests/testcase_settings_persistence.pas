// SPDX-License-Identifier: GPL-3.0-or-later
{ The window's settings through a real settings.json and back - the calls the
  main form makes in ReadSettings and WriteSettings (app_settings.ReadAppSettings,
  WriteAppSettings) over this machine's settings (machine_settings), on a file in
  the temporary directory. The axes are not among them: they are the project's
  (project_ui_context).

  It used to re-enact the form's TXMLConfig calls against a config.xml of its
  own, which proved the streamer worked and nothing about what the form did;
  config.xml is now only read, once, by legacy_settings_import. }
unit testcase_settings_persistence;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  app_settings, fit_loss, machine_settings;
type
  TSettingsPersistenceTest = class(TTestCase)
  private
    FPath: string;
    { Writes ASaved as a session ending would, then reads it as the next
      session starting would. The caller owns the result. }
    function AcrossARestart(ASaved: Settings_v1): Settings_v1;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PersistsMinimizerKind;
    procedure PersistsLossKind;
    procedure PersistsSelectedCurveType;
    procedure AnOlderSettingsFileHasNoCurveType;
    procedure AnOlderSettingsFileLoadsOntoTheCorrectedRFactor;
  end;

implementation

procedure TSettingsPersistenceTest.SetUp;
begin
  FPath := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-settings-persistence.json';
  DeleteFile(FPath);
end;

procedure TSettingsPersistenceTest.TearDown;
begin
  UseMachineSettings('');
  DeleteFile(FPath);
end;

function TSettingsPersistenceTest.AcrossARestart(ASaved: Settings_v1): Settings_v1;
begin
  UseMachineSettings(FPath);
  WriteAppSettings(MachineSettings, ASaved);
  //  A new process: nothing held, the same file named.
  UseMachineSettings('');
  UseMachineSettings(FPath);
  Result := Settings_v1.Create(nil);
  ReadAppSettings(MachineSettings, Result);
end;

procedure TSettingsPersistenceTest.PersistsMinimizerKind;
var
  Saved, Loaded: Settings_v1;
begin
  //  The chosen minimizer (MIN_KIND_* constant) must survive a restart. Uses an
  //  arbitrary non-default value (1) to prove the field round-trips, independent
  //  of which algorithms exist today.
  Saved := Settings_v1.Create(nil);
  Loaded := nil;
  try
    Saved.MinimizerKind := 1;
    Loaded := AcrossARestart(Saved);
    AssertEquals('minimizer kind', 1, Loaded.MinimizerKind);
  finally
    Loaded.Free;
    Saved.Free;
  end;
end;

{ The curve type the last session ended on must come back, or every session
  starts on whatever the registry happens to list first - which is how a user
  who works exclusively with one model still has to re-pick it every time. }
procedure TSettingsPersistenceTest.PersistsSelectedCurveType;
const
  ID = '{B1E4A6D2-5C37-4A1E-9F68-2D70C4A1F001}';
var
  Saved, Loaded: Settings_v1;
begin
  Saved := Settings_v1.Create(nil);
  Loaded := nil;
  try
    Saved.SelectedCurveType := ID;
    Loaded := AcrossARestart(Saved);
    AssertEquals('the curve type survives a restart', ID,
      Loaded.SelectedCurveType);
  finally
    Loaded.Free;
    Saved.Free;
  end;
end;

{ A settings file written before this existed has no curve type in it, so the
  property is never assigned and keeps its constructed value. That value must be
  EMPTY - "never chosen" - so the registry default applies, rather than some id
  that would silently move an existing user onto a different model. }
procedure TSettingsPersistenceTest.AnOlderSettingsFileHasNoCurveType;
var
  Loaded: Settings_v1;
begin
  UseMachineSettings(FPath);
  MachineSettings.SetStr(AppSettingsSection, 'ServerUrl', 'http://older');
  UseMachineSettings('');
  UseMachineSettings(FPath);
  Loaded := Settings_v1.Create(nil);
  try
    ReadAppSettings(MachineSettings, Loaded);
    AssertEquals('an unset curve type means "use the default"', '',
      Loaded.SelectedCurveType);
  finally
    Loaded.Free;
  end;
end;

procedure TSettingsPersistenceTest.PersistsLossKind;
var
  Saved, Loaded: Settings_v1;
begin
  //  The chosen objective must survive a restart, like the minimizer beside it.
  //  Uses a non-default kind, so a field that silently failed to round-trip
  //  would come back as the default and be caught.
  Saved := Settings_v1.Create(nil);
  Loaded := nil;
  try
    Saved.LossKind := LOSS_KIND_RELATIVE;
    Loaded := AcrossARestart(Saved);
    AssertEquals('loss kind', LOSS_KIND_RELATIVE, Loaded.LossKind);
  finally
    Loaded.Free;
    Saved.Free;
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
  //  Writes and reads a real file.
  RegisterTest('integration', TSettingsPersistenceTest);
end.
