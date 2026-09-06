// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What every data loader does around its parser: making the point set,
handing out copies, reloading, and what it calls the source in a message.)

THE PARSERS ARE TESTED ELSEWHERE, one file per format. What is here is the
template method they all sit in - the half that owns the point set, and that
every format inherits without thinking about it.

WHY THE COPY MATTERS MORE THAN IT LOOKS. The loader keeps the set it parsed and
hands out COPIES, because the client that receives one puts it on a chart, edits
it, and subtracts a background from it. Handing out the loader's own set instead
would make Reload - "read this file again, I have made a mess of it" - give the
user back the mess they were trying to discard, since the thing they edited and
the thing being reloaded would be one object.

RELOADING IS THE OTHER HALF, and it clears in place rather than replacing. The
set has been handed to the chart, the grid and the client by then, all of which
hold the pointer; replacing the object would leave every one of them drawing
something freed.

AND THE SOURCE NAME IS NOT DECORATION. It is what a format error quotes, and the
two ways in differ: a file has a path worth printing, lines in memory have none.
Quoting an empty path would read as a missing file - the user goes looking for
something that is not the problem.
}
unit testcase_data_loader;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_loader, dat_file_loader, points_set, title_points_set;

type
    TDataLoaderTest = class(TTestCase)
    private
        FLoader: TDATFileLoader;
        { Two points, in the plainest form the DAT parser accepts. }
        function TwoPoints: TStringList;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Loading from lines, which is the same parse a file takes.
        procedure LinesAreParsedIntoPoints;
        procedure LoadingAgainReplacesWhatWasThereBefore;
        procedure LoadingNothingLeavesAnEmptySet;

        //  The copies the loader hands out.
        procedure ACopyCarriesThePoints;
        procedure ACopyIsNotTheLoadersOwnSet;
        procedure EditingACopyDoesNotTouchTheLoader;
        procedure TwoCopiesAreIndependentOfEachOther;

        //  Opening a file that is not there.
        procedure LoadingAFileThatIsNotThereIsRefused;
        procedure ThatRefusalIsAUserErrorRatherThanADefect;

        //  Reloading.
        procedure ReloadingWithoutAFileIsRefused;
        procedure ARefusedReloadLeavesTheDataAlone;

        //  What a message calls the source.
        procedure LinesInMemoryAreNamedForWhatTheyAre;
    end;

implementation

procedure TDataLoaderTest.SetUp;
begin
    FLoader := TDATFileLoader.Create(nil);
end;

procedure TDataLoaderTest.TearDown;
begin
    FreeAndNil(FLoader);
end;

function TDataLoaderTest.TwoPoints: TStringList;
begin
    Result := TStringList.Create;
    Result.Add('10.0 100.0');
    Result.Add('11.0 110.0');
end;

{ ---- loading from lines ---------------------------------------------------- }

procedure TDataLoaderTest.LinesAreParsedIntoPoints;
var
    Lines: TStringList;
    P: TTitlePointsSet;
begin
    //  THE SAME PATH A FILE LOAD TAKES - the file version only reads the lines
    //  first - so a test through here cannot pass over a parser the application
    //  does not actually use.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals('both points', 2, P.PointsCount);
        AssertEquals('the first abscissa', 10.0, P.PointXCoord[0], 1E-9);
        AssertEquals('and its ordinate', 100.0, P.PointYCoord[0], 1E-9);
    finally
        P.Free;
    end;
end;

procedure TDataLoaderTest.LoadingAgainReplacesWhatWasThereBefore;
var
    Lines: TStringList;
    P: TTitlePointsSet;
begin
    //  CLEARED, NOT APPENDED TO. Opening a second file would otherwise show the
    //  user both data sets at once, which looks like the first file having more
    //  points than it does.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals('still two', 2, P.PointsCount);
    finally
        P.Free;
    end;
end;

procedure TDataLoaderTest.LoadingNothingLeavesAnEmptySet;
var
    Lines: TStringList;
    P: TTitlePointsSet;
begin
    //  A SET, NOT NIL. An empty file is a thing that happens, and every caller
    //  downstream asks the set how many points it has rather than whether it
    //  exists.
    Lines := TStringList.Create;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals('no points', 0, P.PointsCount);
    finally
        P.Free;
    end;
end;

{ ---- the copies the loader hands out --------------------------------------- }

procedure TDataLoaderTest.ACopyCarriesThePoints;
var
    Lines: TStringList;
    P: TTitlePointsSet;
begin
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals(2, P.PointsCount);
    finally
        P.Free;
    end;
end;

procedure TDataLoaderTest.ACopyIsNotTheLoadersOwnSet;
var
    Lines: TStringList;
    A, B: TTitlePointsSet;
begin
    //  A NEW OBJECT EVERY TIME. Handing out the loader's own set would make
    //  Reload - "read this file again, I have made a mess of it" - hand the user
    //  back the mess, because the thing they edited and the thing being reloaded
    //  would be the same object.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    A := FLoader.GetPointsSetCopy;
    B := FLoader.GetPointsSetCopy;
    try
        AssertTrue('two distinct sets', A <> B);
    finally
        A.Free;
        B.Free;
    end;
end;

procedure TDataLoaderTest.EditingACopyDoesNotTouchTheLoader;
var
    Lines: TStringList;
    A, B: TTitlePointsSet;
begin
    //  What the client does to every set it is given: subtract a background,
    //  smooth it, edit a point in the table. None of that may reach the loader,
    //  or the file on disk stops being what Reload goes back to.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    A := FLoader.GetPointsSetCopy;
    try
        A.PointYCoord[0] := 999;
    finally
        A.Free;
    end;
    B := FLoader.GetPointsSetCopy;
    try
        AssertEquals('the loader still holds what it parsed',
            100.0, B.PointYCoord[0], 1E-9);
    finally
        B.Free;
    end;
end;

procedure TDataLoaderTest.TwoCopiesAreIndependentOfEachOther;
var
    Lines: TStringList;
    A, B: TTitlePointsSet;
begin
    //  Two callers hold copies at once - the chart and the client - and neither
    //  may see the other's edits.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    A := FLoader.GetPointsSetCopy;
    B := FLoader.GetPointsSetCopy;
    try
        A.PointYCoord[0] := 999;
        AssertEquals('the other is untouched', 100.0, B.PointYCoord[0], 1E-9);
    finally
        A.Free;
        B.Free;
    end;
end;

{ ---- reloading ------------------------------------------------------------- }

procedure TDataLoaderTest.LoadingAFileThatIsNotThereIsRefused;
var
    Raised: boolean;
begin
    //  A FILE THE USER CHOSE AND THAT IS NO LONGER THERE is a state the
    //  application can reach: the dialog names a path, and the file can be
    //  moved or deleted between the choosing and the reading. This used to be
    //  an Assert, which is compiled out of a release build - so the release
    //  binary went on to parse a file it had not read.
    Raised := False;
    try
        FLoader.LoadDataSet('a-file-that-is-not-there.dat');
    except
        on E: EFileNotExists do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TDataLoaderTest.ThatRefusalIsAUserErrorRatherThanADefect;
var
    Kind: string;
begin
    //  WHICH CLASS MATTERS, and it is the whole reason this is not a
    //  checks.pas call: a missing file is an ordinary outcome the user should
    //  read a message about, not the program being wrong about itself. Reload
    //  already answered EFileNotExists for the same condition; LoadDataSet now
    //  agrees with it.
    Kind := 'nothing was raised';
    try
        FLoader.LoadDataSet('a-file-that-is-not-there.dat');
    except
        on E: EFileNotExists do
            Kind := 'user error';
        on E: Exception do
            Kind := E.ClassName;
    end;
    AssertEquals('a missing file is an ordinary outcome, not a defect',
        'user error', Kind);
end;

procedure TDataLoaderTest.ReloadingWithoutAFileIsRefused;
var
    Lines: TStringList;
    Raised: boolean;
begin
    //  LINES IN MEMORY CANNOT BE RELOADED - there is nothing to read again. The
    //  menu item is offered whenever data is loaded, and data can arrive this
    //  way, so the refusal is a state the application can reach.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    Raised := False;
    try
        FLoader.Reload;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TDataLoaderTest.ARefusedReloadLeavesTheDataAlone;
var
    Lines: TStringList;
    P: TTitlePointsSet;
begin
    //  REFUSED BEFORE THE SET IS CLEARED, which is the whole reason the check
    //  comes first. The set has been handed to the chart and the grid by now, so
    //  emptying it and only then discovering there is nothing to refill it from
    //  would leave the user looking at a blank chart with no way back.
    Lines := TwoPoints;
    try
        FLoader.LoadFromLines(Lines);
    finally
        Lines.Free;
    end;
    try
        FLoader.Reload;
    except
        on Exception do ;
    end;
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals('the points are still there', 2, P.PointsCount);
    finally
        P.Free;
    end;
end;

{ ---- what a message calls the source --------------------------------------- }

procedure TDataLoaderTest.LinesInMemoryAreNamedForWhatTheyAre;
var
    Lines: TStringList;
    Message_: string;
begin
    //  NOT AN EMPTY PATH. A format error quoting '' reads as a missing file, and
    //  the user goes looking for something that is not the problem.
    Lines := TStringList.Create;
    Message_ := '';
    try
        Lines.Add('this is not a DAT file at all');
        try
            FLoader.LoadFromLines(Lines);
        except
            on E: Exception do
                Message_ := E.Message;
        end;
    finally
        Lines.Free;
    end;
    if Message_ <> '' then
        AssertTrue('the source is named, and not as a path: ' + Message_,
            Pos('supplied lines', Message_) > 0);
end;

initialization
    //  A unit test: lines in memory and a point set. Nothing here opens a file -
    //  reading one is what makes a loader test an integration test by this
    //  project's rule, and the parse is the same either way.
    RegisterTest('unit', TDataLoaderTest);
end.
