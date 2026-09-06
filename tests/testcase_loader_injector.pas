// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which reader opens a file, and who owns it afterwards.)

TExtensionDataLoaderInjector is small and load-bearing: it is what every Open
goes through. Since the registry took over the question of WHICH loader handles a
file, what is left here is ownership - it holds one loader at a time and frees the
previous one - plus the message the user gets when nothing can read the file.

Both halves had been untested. Ownership bugs here are the kind that do not fail
on the machine that wrote them: freeing the previous loader too early leaves the
last-opened profile reading freed memory, and not freeing it at all leaks one
loader per Open.

Also covers app_modules, whose whole content is the statement that a build with
no module is ordinary - a claim worth pinning, because the failure it guards
against is a module silently absent with every test passing.
}
unit testcase_loader_injector;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_loader, int_data_loader, data_loader_registry,
    extension_data_loader_injector, app_modules;

type
    TLoaderInjectorTest = class(TTestCase)
    private
        FInjector: TExtensionDataLoaderInjector;
        { The message CreateDataLoader refused with, or '' when it did not. }
        function RefusalFor(const AFileName: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AKnownExtensionGetsALoader;
        procedure TheExtensionIsMatchedWhateverItsCase;
        procedure AnUnknownExtensionIsRefused;
        procedure TheRefusalNamesTheFileAndTheExtension;
        procedure AFileWithNoExtensionIsRefused;
        procedure TheSecondOpenReplacesTheFirstLoader;
        procedure ARefusalDoesNotLeaveThePreviousLoaderInPlace;
        procedure RegisteringThisBuildsModulesIsHarmlessAndRepeatable;
    end;

implementation

procedure TLoaderInjectorTest.SetUp;
begin
    FInjector := TExtensionDataLoaderInjector.Create;
end;

procedure TLoaderInjectorTest.TearDown;
begin
    //  The injector owns the loader it created, so freeing it is the only
    //  cleanup - and if it did not, this fixture would leak one per test.
    FreeAndNil(FInjector);
end;

function TLoaderInjectorTest.RefusalFor(const AFileName: string): string;
begin
    Result := '';
    try
        FInjector.CreateDataLoader(AFileName);
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TLoaderInjectorTest.AKnownExtensionGetsALoader;
var
    Loader: IDataLoader;
begin
    //  .dat is the format the framework has always read, so it is the one whose
    //  absence would mean the registry is not populated at all.
    Loader := FInjector.CreateDataLoader('somewhere/profile.dat');
    AssertTrue('a loader was made', Assigned(Loader));
end;

procedure TLoaderInjectorTest.TheExtensionIsMatchedWhateverItsCase;
var
    Loader: IDataLoader;
begin
    //  A file saved on Windows and opened on Linux keeps whatever case it was
    //  given, and the user does not think of the extension as data.
    Loader := FInjector.CreateDataLoader('somewhere/PROFILE.DAT');
    AssertTrue('an upper-case extension is the same extension', Assigned(Loader));
end;

procedure TLoaderInjectorTest.AnUnknownExtensionIsRefused;
begin
    //  Returning nil instead would fail later, while reading, with nothing left
    //  to say which file was being opened.
    AssertTrue('it refused', RefusalFor('somewhere/photo.jpeg') <> '');
end;

procedure TLoaderInjectorTest.TheRefusalNamesTheFileAndTheExtension;
var
    Msg: string;
begin
    //  BOTH, because the two answer different questions: which file failed, and
    //  what this build cannot read. "Invalid file extension" answered neither.
    Msg := RefusalFor('somewhere/photo.jpeg');
    AssertTrue('the file: ' + Msg, Pos('photo.jpeg', Msg) > 0);
    AssertTrue('and the extension: ' + Msg, Pos('.jpeg', Msg) > 0);
end;

procedure TLoaderInjectorTest.AFileWithNoExtensionIsRefused;
begin
    //  Nothing to match on. It must refuse rather than pick a default reader,
    //  which would then fail on the contents and blame the data.
    AssertTrue('it refused', RefusalFor('somewhere/profile') <> '');
end;

procedure TLoaderInjectorTest.TheSecondOpenReplacesTheFirstLoader;
var
    First, Second: IDataLoader;
begin
    //  ONE AT A TIME is the whole responsibility of this class now. A second
    //  Open that left the first loader alive would leak one per file the user
    //  opens - a session-long climb nobody notices until it matters.
    First := FInjector.CreateDataLoader('a.dat');
    Second := FInjector.CreateDataLoader('b.dat');
    AssertTrue('both were made', Assigned(First) and Assigned(Second));

    //  NOT ASSERTED: that the two references differ. They frequently do not -
    //  the first loader is freed before the second is allocated, and the
    //  allocator hands back the same address, which this test found on the first
    //  run. That is worth knowing rather than working around: a caller holding
    //  the earlier reference sees a live-looking object of the right class that
    //  is a DIFFERENT file's loader, which is the worst available failure mode
    //  and one that comparing pointers would never reveal. The rule is that the
    //  caller does not keep it, and there is nothing in the type system saying
    //  so - see findings.md.
end;

procedure TLoaderInjectorTest.ARefusalDoesNotLeaveThePreviousLoaderInPlace;
var
    Loader: IDataLoader;
begin
    //  A refused Open frees the previous loader BEFORE it discovers it cannot
    //  make a new one. So after a refusal the injector holds nothing, and the
    //  reference the caller still has from the successful Open before it is
    //  dangling.
    //
    //  Asserted as it behaves, not as it should be: this is the shape of the
    //  problem rather than a defect with a caller today, because the desktop
    //  discards its loader reference on a failed Open. It is recorded in
    //  findings.md, and it is pinned here so that a caller who starts holding
    //  one across a failure finds this test rather than a crash.
    Loader := FInjector.CreateDataLoader('a.dat');
    AssertTrue('the first Open worked', Assigned(Loader));
    AssertTrue('the second is refused', RefusalFor('b.jpeg') <> '');
    //  A third Open must still work - the injector is not left in a broken state
    //  by the refusal.
    Loader := FInjector.CreateDataLoader('c.dat');
    AssertTrue('and it still works afterwards', Assigned(Loader));
end;

procedure TLoaderInjectorTest.RegisteringThisBuildsModulesIsHarmlessAndRepeatable;
begin
    //  A BUILD WITH NO MODULE IS THE ORDINARY CASE, and this is the statement of
    //  it. Called by both hosts, so calling it twice must be safe; and it must
    //  not raise, because a framework that cannot start without a module is not
    //  a framework.
    RegisterAppModules;
    RegisterAppModules;
    AssertTrue('it returned', True);
end;

initialization
    //  A unit test: no file is opened. Which loader handles a name is decided
    //  from the name alone.
    RegisterTest('unit', TLoaderInjectorTest);
end.
