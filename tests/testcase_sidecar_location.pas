// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Finding the sidecar's script and interpreter on a tree built for the
purpose.)

THE HALF THAT HAS TO OPEN DIRECTORIES. Which paths are worth trying is a
decision, it lives in sidecar_launch, and testcase_sidecar_launch states it
without touching a file. What is left in python_sidecar is the other half: list
what is actually checked out beside this repository, and ask the disk which of
the candidates is really there. That cannot be asserted without a disk, so this
suite builds a tree, points the search at it, and reads back what it found.

WHAT IT COSTS WHEN THIS IS WRONG, and it has been. A module build's fit_server
runs from its own repository, which holds no fit_backend.py: the script is in the
framework repository checked out BESIDE it. When that search failed, every
private build had no sidecar at all and every feature needing one answered "the
Python component could not be started" - on machines with a working Python, a
working venv and the script sitting one directory across. The failure names
nothing about paths, so no test above this level can catch it.

IT IS THE REAL SEARCH, not a re-implementation: LocateSidecarIn is what
LocatePython calls, with the binary's directory and the machine's environment
home as arguments rather than read from the process. Those two arguments are the
only reason this is possible at all.
}
unit testcase_sidecar_location;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    python_sidecar, sidecar_launch;

type
    TSidecarLocationTest = class(TTestCase)
    private
        { Everything this test writes, removed whole in TearDown. }
        FRoot: string;
        { The directory the repositories sit in - what an umbrella candidate
          resolves to. }
        function Umbrella: string;
        { The directory a module build's fit_server runs from: <pack>/Worker/o,
          three levels below the umbrella. }
        function BinDir: string;
        { Creates ARelative under FRoot as a directory. Returns its full path,
          ending in a separator. }
        function MakeDir(const ARelative: string): string;
        { Creates an empty file at ARelative under FRoot. }
        procedure MakeFile(const ARelative: string);
        { Gives ARepo, a directory under the umbrella, the framework's sidecar
          script. }
        procedure MakeRepoWithScript(const ARepo: string);
        { Creates a virtual environment named AName under FRoot with an
          interpreter where this platform puts one, and answers the home that
          holds it. }
        function MakePyHome(const AName: string): string;
        procedure RemoveTree(const APath: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The script.
        procedure TheScriptBesideTheBinaryIsUsedWhenThereIsOne;
        procedure TheScriptInARepositoryBesideThisOneIsFoundToo;
        procedure TheRepositoriesAreTriedInSortedOrderNotDiskOrder;
        procedure NoScriptAnywhereMeansNoSidecarAtAll;

        //  The interpreter.
        procedure TheSharedEnvironmentSuppliesTheInterpreter;
        procedure AnEnvironmentWithoutAnInterpreterFallsBackToTheSystemPython;
    end;

implementation

const
    ScriptName = 'fit_backend.py';

procedure TSidecarLocationTest.SetUp;
begin
    FRoot := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-sidecar-' + FormatDateTime('hhnnsszzz', Now) +
        IntToStr(Random(100000)) + PathDelim;
    ForceDirectories(FRoot);
    //  THREE LEVELS between the binary and the umbrella - <umbrella>/<repo>/
    //  Worker/o - because that is where a module build's server really sits,
    //  and the depth is the whole question the sibling search answers.
    MakeDir('umbrella' + PathDelim + 'pack' + PathDelim + 'Worker' +
        PathDelim + 'o');
end;

procedure TSidecarLocationTest.TearDown;
begin
    if (FRoot <> '') and DirectoryExists(FRoot) then
        RemoveTree(FRoot);
    FRoot := '';
end;

function TSidecarLocationTest.Umbrella: string;
begin
    Result := FRoot + 'umbrella' + PathDelim;
end;

function TSidecarLocationTest.BinDir: string;
begin
    Result := Umbrella + 'pack' + PathDelim + 'Worker' + PathDelim + 'o' +
        PathDelim;
end;

function TSidecarLocationTest.MakeDir(const ARelative: string): string;
begin
    Result := IncludeTrailingPathDelimiter(FRoot + ARelative);
    if not ForceDirectories(Result) then
        Fail('could not create ' + Result);
end;

procedure TSidecarLocationTest.MakeFile(const ARelative: string);
var
    F: TFileStream;
begin
    ForceDirectories(ExtractFilePath(FRoot + ARelative));
    F := TFileStream.Create(FRoot + ARelative, fmCreate);
    F.Free;
end;

procedure TSidecarLocationTest.MakeRepoWithScript(const ARepo: string);
begin
    MakeFile('umbrella' + PathDelim + ARepo + PathDelim + 'Worker' +
        PathDelim + 'py' + PathDelim + ScriptName);
end;

function TSidecarLocationTest.MakePyHome(const AName: string): string;
begin
    Result := FRoot + AName;
    //  The layout sidecar_launch.SidecarPythonCandidates expects: a 'sidecar'
    //  environment under the home, with the interpreter where this platform's
    //  venv puts it. Built for THIS platform only - the other path is not a
    //  candidate here and a file there would assert nothing.
{$IFDEF WINDOWS}
    MakeFile(AName + PathDelim + 'sidecar' + PathDelim + 'Scripts' +
        PathDelim + 'python.exe');
{$ELSE}
    MakeFile(AName + PathDelim + 'sidecar' + PathDelim + 'bin' +
        PathDelim + 'python');
{$ENDIF}
end;

procedure TSidecarLocationTest.RemoveTree(const APath: string);
var
    Rec: TSearchRec;
    Full: string;
begin
    Full := IncludeTrailingPathDelimiter(APath);
    if FindFirst(Full + '*', faAnyFile, Rec) = 0 then
    begin
        repeat
            if (Rec.Name = '.') or (Rec.Name = '..') then
                Continue;
            if (Rec.Attr and faDirectory) <> 0 then
                RemoveTree(Full + Rec.Name)
            else
                DeleteFile(Full + Rec.Name);
        until FindNext(Rec) <> 0;
        FindClose(Rec);
    end;
    RemoveDir(Full);
end;

{ ---- the script ------------------------------------------------------------ }

procedure TSidecarLocationTest.TheScriptBesideTheBinaryIsUsedWhenThereIsOne;
var
    PyExe, Script: string;
begin
    //  AN INSTALLED TREE: py/ sits beside the binary. It is the first thing
    //  looked for, and finding it must stop the search - listing the
    //  neighbouring checkouts of a machine that has none is wasted work at
    //  best, and on an installed machine there is no umbrella to list.
    MakeFile('umbrella' + PathDelim + 'pack' + PathDelim + 'Worker' +
        PathDelim + 'o' + PathDelim + 'py' + PathDelim + ScriptName);
    MakeRepoWithScript('fit');

    LocateSidecarIn(BinDir, '', PyExe, Script);
    AssertEquals('the script beside the binary', ExpandFileName(BinDir +
        'py' + PathDelim + ScriptName), Script);
end;

procedure TSidecarLocationTest.TheScriptInARepositoryBesideThisOneIsFoundToo;
var
    PyExe, Script: string;
begin
    //  THE DEFECT THIS EXISTS FOR. The pack keeps no framework file, so nothing
    //  around its own binary matches and the only copy of the script is in the
    //  repository next door. Before this search every private build reported
    //  the Python component as unstartable.
    MakeRepoWithScript('fit');

    LocateSidecarIn(BinDir, '', PyExe, Script);
    AssertEquals('the framework repository''s script',
        ExpandFileName(Umbrella + 'fit' + PathDelim + 'Worker' + PathDelim +
            'py' + PathDelim + ScriptName), Script);
end;

procedure TSidecarLocationTest.TheRepositoriesAreTriedInSortedOrderNotDiskOrder;
var
    PyExe, Script: string;
begin
    //  TWO CHECKOUTS THAT BOTH ANSWER, created in the order that is NOT the
    //  answer. Without the sort the winner is whichever the file system happens
    //  to return first, so two machines with the same checkout would run
    //  different scripts - and the one that broke would do so for a reason
    //  nothing in the build could be asked about.
    MakeRepoWithScript('zz-created-first');
    MakeRepoWithScript('aa-created-second');

    LocateSidecarIn(BinDir, '', PyExe, Script);
    AssertEquals('the alphabetically first repository, not the older one',
        ExpandFileName(Umbrella + 'aa-created-second' + PathDelim + 'Worker' +
            PathDelim + 'py' + PathDelim + ScriptName), Script);
end;

procedure TSidecarLocationTest.NoScriptAnywhereMeansNoSidecarAtAll;
var
    PyExe, Script: string;
begin
    //  NOTHING IS INVENTED when the search comes up empty: no script and no
    //  interpreter either. A path guessed here would be handed to TProcess and
    //  fail as a launch error, which reads as a broken Python rather than an
    //  absent one - and the caller's own answer is "the sidecar is simply
    //  unavailable", which it can only give if both outputs are empty.
    LocateSidecarIn(BinDir, '', PyExe, Script);
    AssertEquals('no script', '', Script);
    AssertEquals('and no interpreter to run it with', '', PyExe);
end;

{ ---- the interpreter ------------------------------------------------------- }

procedure TSidecarLocationTest.TheSharedEnvironmentSuppliesTheInterpreter;
var
    PyExe, Script: string;
    Home: string;
begin
    //  THE ENVIRONMENT IS PER MACHINE and is handed in, not discovered: this is
    //  the path a server takes on a machine where the prerequisites step has
    //  been run, and getting it wrong means falling back to a system Python
    //  that cannot import lmfit - which the user sees as "no sidecar here".
    MakeRepoWithScript('fit');
    Home := MakePyHome('pyhome');

    LocateSidecarIn(BinDir, Home, PyExe, Script);
    AssertTrue('the interpreter is under the home given: ' + PyExe,
        Pos(ExpandFileName(Home), PyExe) = 1);
    AssertTrue('and it is the sidecar environment''s: ' + PyExe,
        Pos('sidecar', PyExe) > 0);
end;

procedure TSidecarLocationTest.AnEnvironmentWithoutAnInterpreterFallsBackToTheSystemPython;
var
    PyExe, Script: string;
begin
    //  A HOME THAT HOLDS NOTHING is the same case as no home at all. The
    //  fallback may still have the libraries, and the sidecar reports its own
    //  failure if it does not - so this is a bare name for PATH to resolve,
    //  never a path into the tree that was just searched.
    MakeRepoWithScript('fit');

    LocateSidecarIn(BinDir, FRoot + 'nothing-here', PyExe, Script);
    AssertEquals('the system python', SystemPython, PyExe);
    AssertTrue('the script was still found', Script <> '');
end;

initialization
    //  A unit test that writes files, which is the point: everything below the
    //  candidate lists is a question for a real directory. No process is
    //  started and no port is bound, and the tree is its own - nothing here
    //  reads the checkout it is running from.
    RegisterTest('unit', TSidecarLocationTest);
end.
