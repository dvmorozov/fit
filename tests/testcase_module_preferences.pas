// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A module's own choices, kept between sessions without the framework
knowing what they are.)

The framework once kept which price column a .csv file is read by in its own
settings - a field's choice in the framework's file, under the framework's name.
That choice is the price-series module's now, and a module needs somewhere to keep
what its user chose; these tests are the contract it is kept under.
}
unit testcase_module_preferences;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, module_preferences;

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
    end;

    { Through a file, as the application keeps them. }
    TModulePreferencesFileTest = class(TTestCase)
    private
        FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AChoiceSurvivesARestart;
        procedure NoFileYetIsNoChoiceYet;
        procedure AFileThatCannotBeWrittenIsNotAFault;
    end;

implementation

procedure TModulePreferencesTest.SetUp;
begin
    UseModulePreferencesFile('');
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
    UseModulePreferencesFile('');
    SetModulePreference('probe.column', 'high');
    UseModulePreferencesFile('');
    AssertEquals('forgotten with the session', '', ModulePreference('probe.column'));
end;

procedure TModulePreferencesFileTest.SetUp;
begin
    FPath := GetTempFileName('', 'fitprefs') + '.ini';
    UseModulePreferencesFile(FPath);
end;

procedure TModulePreferencesFileTest.TearDown;
begin
    UseModulePreferencesFile('');
    if FileExists(FPath) then
        DeleteFile(FPath);
end;

procedure TModulePreferencesFileTest.AChoiceSurvivesARestart;
begin
    SetModulePreference('probe.column', 'high');
    //  A restart: nothing in memory, the file named again.
    UseModulePreferencesFile('');
    UseModulePreferencesFile(FPath);
    AssertEquals('high', ModulePreference('probe.column', 'close'));
end;

procedure TModulePreferencesFileTest.NoFileYetIsNoChoiceYet;
begin
    AssertFalse(FileExists(FPath));
    AssertEquals('close', ModulePreference('probe.column', 'close'));
end;

procedure TModulePreferencesFileTest.AFileThatCannotBeWrittenIsNotAFault;
begin
    //  A read-only configuration directory costs the user the choice between
    //  sessions, not the session.
    UseModulePreferencesFile(FPath + PathDelim + 'no-such-dir' + PathDelim + 'p.ini');
    SetModulePreference('probe.column', 'high');
    AssertEquals('still kept for the session', 'high',
        ModulePreference('probe.column'));
end;

initialization
    RegisterTest('unit', TModulePreferencesTest);
    //  Writes and reads a file.
    RegisterTest('integration', TModulePreferencesFileTest);
end.
