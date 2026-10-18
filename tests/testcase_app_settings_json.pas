// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The window's settings as the 'app' section of this machine's
settings.json.)

WHAT IT REPLACES: config.xml, written through the component streamer when the
window closed and read back through it when the window opened - code inside
TFormMain, which no test could reach, so the suites re-enacted its calls
against a file of their own. The window now calls ReadAppSettings and
WriteAppSettings, and these tests call exactly those.

THE DEFAULTS STILL MATTER MOST: a key missing from the section must leave the
constructed value alone, because several of them say "the user never chose"
(testcase_settings_model asserts them by value).
}
unit testcase_app_settings_json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    app_settings, machine_settings, mock_machine_settings;

type
    TAppSettingsJSONTest = class(TTestCase)
    private
        FMachine: TMemoryMachineSettings;
        FSaved, FLoaded: Settings_v1;
        procedure Start(AHasFile: boolean; const AStored: string = '');
        { Every field away from its default, each to a distinct value. }
        procedure ChangeEveryField(S: Settings_v1);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EveryPersistedFieldSurvivesTheRoundTrip;
        procedure TheSettingsAreTheMachinesAppSection;
        procedure TheSectionHoldsOnlyTheSettingsOwnFields;
        procedure AnOlderSectionLeavesTheMissingFieldsAtTheirDefaults;
        procedure AFirstRunReadsTheDefaults;
        procedure AKeyFromANewerBuildIsReadPastAndKeptOnWriting;
        procedure AValueOfTheWrongKindKeepsTheDefault;
        procedure AKeyIsReadInAnyCase;
        procedure ReadingWritesNothing;
    end;

implementation

procedure TAppSettingsJSONTest.SetUp;
begin
    FMachine := nil;
    FSaved := Settings_v1.Create(nil);
    FLoaded := Settings_v1.Create(nil);
end;

procedure TAppSettingsJSONTest.TearDown;
begin
    FreeAndNil(FLoaded);
    FreeAndNil(FSaved);
    FreeAndNil(FMachine);
end;

procedure TAppSettingsJSONTest.Start(AHasFile: boolean; const AStored: string);
begin
    FreeAndNil(FMachine);
    FMachine := TMemoryMachineSettings.CreateWith(AHasFile, AStored);
end;

procedure TAppSettingsJSONTest.ChangeEveryField(S: Settings_v1);
begin
    S.Reserved := 7;
    S.MinimizerKind := 2;
    S.LossKind := 1;
    S.SelectedCurveType := '{0B0E4B7C-0000-0000-0000-000000000001}';
    S.ServerUrl := 'http://compute.example:8080';
    S.Weighting := 'none';
    S.DownloadFolder := '/data/downloads';
    S.AnimationMode := True;
    S.ReportZoom := 140;
    S.ExplainZoom := 85;
    S.Theme := 'dark';
end;

procedure TAppSettingsJSONTest.EveryPersistedFieldSurvivesTheRoundTrip;
var
    Written: string;
begin
    //  ONE test over every field: what breaks a published property is a change
    //  to the property list, which takes all of them out together. Each value
    //  is distinct from its default, so a field that was not written shows as
    //  a default rather than as a match.
    Start(False);
    ChangeEveryField(FSaved);
    WriteAppSettings(FMachine, FSaved);
    Written := FMachine.Stored;
    //  The next session: a new process reading the file.
    Start(True, Written);
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals('the reserved field', 7, FLoaded.Reserved);
    AssertEquals('the minimizer', 2, FLoaded.MinimizerKind);
    AssertEquals('the objective', 1, FLoaded.LossKind);
    AssertEquals('the curve type', '{0B0E4B7C-0000-0000-0000-000000000001}',
        FLoaded.SelectedCurveType);
    AssertEquals('the server', 'http://compute.example:8080', FLoaded.ServerUrl);
    AssertEquals('the weighting', 'none', FLoaded.Weighting);
    AssertEquals('the download folder', '/data/downloads', FLoaded.DownloadFolder);
    AssertTrue('animation', FLoaded.AnimationMode);
    AssertEquals('the report''s size', 140, FLoaded.ReportZoom);
    AssertEquals('the Explain pane''s size', 85, FLoaded.ExplainZoom);
    AssertEquals('the theme', 'dark', FLoaded.Theme);
end;

procedure TAppSettingsJSONTest.TheSettingsAreTheMachinesAppSection;
begin
    //  ONE FILE FOR WHAT THIS MACHINE REMEMBERS: the window's settings are a
    //  section of it, beside the recent list and the modules' choices - which
    //  writing them leaves as they were.
    Start(True, '{"preferences": {"probe.column": "high"}}');
    FSaved.ServerUrl := 'http://compute.example:8080';
    WriteAppSettings(FMachine, FSaved);
    AssertEquals('http://compute.example:8080',
        FMachine.Str(AppSettingsSection, 'ServerUrl'));
    AssertEquals('high', FMachine.Str('preferences', 'probe.column'));
end;

procedure TAppSettingsJSONTest.TheSectionHoldsOnlyTheSettingsOwnFields;
var
    S: TJSONObject;
begin
    //  Name and Tag are TComponent's, published for a form designer; a
    //  settings file has no use for either, and a user reading it should not
    //  wonder what they mean.
    Start(False);
    WriteAppSettings(FMachine, FSaved);
    S := FMachine.Section(AppSettingsSection);
    try
        AssertNull('no Name', S.Find('Name'));
        AssertNull('no Tag', S.Find('Tag'));
        AssertNotNull('but the settings', S.Find('Weighting'));
    finally
        S.Free;
    end;
end;

procedure TAppSettingsJSONTest.AnOlderSectionLeavesTheMissingFieldsAtTheirDefaults;
var
    Fresh: Settings_v1;
begin
    //  A file written before a field existed says nothing about it, and the
    //  field keeps what a fresh object has - "never chosen" stays never chosen.
    Start(True, '{"app": {"ServerUrl": "http://old.example"}}');
    ReadAppSettings(FMachine, FLoaded);
    Fresh := Settings_v1.Create(nil);
    try
        AssertEquals('http://old.example', FLoaded.ServerUrl);
        AssertEquals(Fresh.SelectedCurveType, FLoaded.SelectedCurveType);
        AssertEquals(Fresh.Weighting, FLoaded.Weighting);
        AssertEquals(Fresh.ReportZoom, FLoaded.ReportZoom);
        AssertEquals(Fresh.AnimationMode, FLoaded.AnimationMode);
        //  Empty: Follow System, which is how such a file always opened.
        AssertEquals('', FLoaded.Theme);
    finally
        Fresh.Free;
    end;
end;

procedure TAppSettingsJSONTest.AFirstRunReadsTheDefaults;
begin
    Start(False);
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals('poisson', FLoaded.Weighting);
    AssertEquals(-1, FLoaded.Reserved);
end;

procedure TAppSettingsJSONTest.AKeyFromANewerBuildIsReadPastAndKeptOnWriting;
begin
    //  THE REPORT config.xml once had: a field this build has never heard of
    //  made the reader raise, and the window then threw every setting away. It
    //  is read past here - and, a step further, kept on writing, so going back
    //  to the older build and forward again loses nothing.
    Start(True, '{"app": {"FieldFromANewerBuild": "anything", "Weighting": "none"}}');
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals('what this build knows is still read', 'none', FLoaded.Weighting);
    FLoaded.ServerUrl := 'http://compute.example:8080';
    WriteAppSettings(FMachine, FLoaded);
    AssertEquals('and the newer build''s field is still there', 'anything',
        FMachine.Str(AppSettingsSection, 'FieldFromANewerBuild'));
end;

procedure TAppSettingsJSONTest.AValueOfTheWrongKindKeepsTheDefault;
begin
    //  A hand edit: text where a number goes, a number where a flag goes. That
    //  field keeps its default; the rest are still read.
    Start(True, '{"app": {"ReportZoom": "big", "AnimationMode": 1, ' +
        '"Weighting": 5, "MinimizerKind": 2.5, "ServerUrl": "http://kept"}}');
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals(FSaved.ReportZoom, FLoaded.ReportZoom);
    AssertFalse(FLoaded.AnimationMode);
    AssertEquals('poisson', FLoaded.Weighting);
    AssertEquals(0, FLoaded.MinimizerKind);
    AssertEquals('http://kept', FLoaded.ServerUrl);
end;

procedure TAppSettingsJSONTest.AKeyIsReadInAnyCase;
begin
    //  As Pascal names its properties, and as the file compares every key.
    Start(True, '{"app": {"serverurl": "http://any.case"}}');
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals('http://any.case', FLoaded.ServerUrl);
end;

procedure TAppSettingsJSONTest.ReadingWritesNothing;
begin
    Start(True, '{"app": {"Weighting": "none"}}');
    ReadAppSettings(FMachine, FLoaded);
    AssertEquals(0, FMachine.Writes);
end;

initialization
    //  The file is the memory double's: nothing here touches the disk.
    RegisterTest('unit', TAppSettingsJSONTest);
end.
