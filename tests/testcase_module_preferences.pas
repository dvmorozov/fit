// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A module's own choices, kept between sessions without the framework
knowing what they are.)

The framework once kept which price column a .csv file is read by in its own
settings - a field's choice in the framework's file, under the framework's name.
That choice is the price-series module's now, and a module needs somewhere to keep
what its user chose; these tests are the contract it is kept under.

WHERE IT IS KEPT: the 'preferences' section of this machine's settings.json
(machine_settings), beside the window's settings and the recent list - no longer
a file of its own.
}
unit testcase_module_preferences;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, machine_settings,
    module_preferences;

type
    { In memory: what every test binary gets unless it names a file. }
    TModulePreferencesTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        procedure APreferenceNeverChosenIsItsDefault;
        procedure AChoiceIsReadBack;
        procedure AnotherKeyIsAnotherChoice;
        procedure ALineBreakCannotSplitAChoiceInTwo;
        procedure NothingIsWrittenUntilTheApplicationNamesAFile;
        procedure AChoiceIsKeptInTheMachinesPreferences;
    end;

    { Through a file, as the application keeps them. }
    TModulePreferencesFileTest = class(TTestCase)
    private
        FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
        { A restart: what was held forgotten, the same file named again. }
        procedure Restart;
    published
        procedure AChoiceSurvivesARestart;
        procedure NoFileYetIsNoChoiceYet;
        procedure AFileThatCannotBeWrittenIsNotAFault;
    end;

implementation

procedure TModulePreferencesTest.SetUp;
begin
    UseMachineSettings('');
end;

procedure TModulePreferencesTest.APreferenceNeverChosenIsItsDefault;
begin
    AssertEquals('close', ModulePreference('probe.column', 'close'));
    AssertEquals('', ModulePreference('probe.column'));
end;

procedure TModulePreferencesTest.AChoiceIsReadBack;
begin
    SetModulePreference('probe.column', 'high');
    AssertEquals('high', ModulePreference('probe.column', 'close'));
end;

procedure TModulePreferencesTest.AnotherKeyIsAnotherChoice;
begin
    SetModulePreference('probe.column', 'high');
    AssertEquals('bar', ModulePreference('probe.argument', 'bar'));
end;

procedure TModulePreferencesTest.ALineBreakCannotSplitAChoiceInTwo;
begin
    //  One line per choice in the file: a value carrying a break would read
    //  back as a second, forged key.
    SetModulePreference('probe.column', 'high' + LineEnding + 'probe.other=1');
    AssertEquals('', ModulePreference('probe.other'));
end;

procedure TModulePreferencesTest.NothingIsWrittenUntilTheApplicationNamesAFile;
begin
    //  A test binary that forgets to name a file must not write the user's own
    //  configuration: the default is to keep nothing on disk.
    UseMachineSettings('');
    SetModulePreference('probe.column', 'high');
    UseMachineSettings('');
    AssertEquals('forgotten with the session', '', ModulePreference('probe.column'));
end;

procedure TModulePreferencesTest.AChoiceIsKeptInTheMachinesPreferences;
begin
    //  ONE FILE FOR WHAT THIS MACHINE REMEMBERS: a module's choice is a key in
    //  its section, not a line in a file of its own.
    SetModulePreference('probe.column', 'high');
    AssertEquals('high', MachineSettings.Str(ModulePreferencesSection, 'probe.column'));
end;

procedure TModulePreferencesFileTest.SetUp;
begin
    FPath := GetTempFileName('', 'fitprefs') + '.json';
    UseMachineSettings(FPath);
end;

procedure TModulePreferencesFileTest.Restart;
begin
    UseMachineSettings('');
    UseMachineSettings(FPath);
end;

procedure TModulePreferencesFileTest.TearDown;
begin
    UseMachineSettings('');
    if FileExists(FPath) then
        DeleteFile(FPath);
end;

procedure TModulePreferencesFileTest.AChoiceSurvivesARestart;
begin
    SetModulePreference('probe.column', 'high');
    Restart;
    AssertEquals('high', ModulePreference('probe.column', 'close'));
end;

procedure TModulePreferencesFileTest.NoFileYetIsNoChoiceYet;
begin
    AssertFalse(FileExists(FPath));
    AssertEquals('close', ModulePreference('probe.column', 'close'));
end;

procedure TModulePreferencesFileTest.AFileThatCannotBeWrittenIsNotAFault;
begin
    //  A folder that cannot be made costs the user the choice between
    //  sessions, not the session. A FILE stands where the folder would go:
    //  a folder merely missing is made.
    with TStringList.Create do
    try
        Text := 'x';
        SaveToFile(FPath);
    finally
        Free;
    end;
    UseMachineSettings(FPath + PathDelim + 'no-such-dir' + PathDelim + 'p.json');
    SetModulePreference('probe.column', 'high');
    AssertEquals('still kept for the session', 'high',
        ModulePreference('probe.column'));
end;

initialization
    RegisterTest('unit', TModulePreferencesTest);
    //  Writes and reads a file.
    RegisterTest('integration', TModulePreferencesFileTest);
end.
