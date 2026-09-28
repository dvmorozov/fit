// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a project's data came from, and whether that file still says the
same thing.)

THE QUESTION THIS ANSWERS is asked months later: the numbers in this project came
from that file - is that file still the one they came from? A project carries its
own profile, so nothing depends on the answer; it is for the user, and it is the
kind of thing that is impossible to reconstruct afterwards if it was not recorded
at the time.

WHY NOT A TIMESTAMP, which is what everyone reaches for first. A modification
date changes when a file is copied, restored from a backup or synchronised, and
does NOT change when a file is edited by something that preserves it. Neither is
rare, and the two failures are opposite: one cries wolf, the other stays silent
about a real change.

The two cases that keep a warning worth reading are here as tests: a file that
cannot be checked is not reported as changed, and a project written before this
existed is not reported as changed either.
}
unit testcase_project_provenance;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_project_document, fit_project_provenance;

type
    { The pure half: bytes in, hex out, nothing touched. }
    TProvenanceHashTest = class(TTestCase)
    published
        procedure TheSameBytesHashTheSame;
        procedure DifferentBytesHashDifferently;
        procedure OneChangedByteChangesTheHash;
        procedure ReorderedBytesAreNotTheSameFile;
        procedure EmptyContentStillHashes;
        procedure AHashIsLowerCaseHexOfAFixedLength;

        //  THE DECISIONS, driven through the reader seam so that no file is
        //  written. Each of these is a way the eventual warning stops being
        //  worth reading, and none of them needs a disk to be wrong.
        procedure DescribingAFileRecordsItsSizeAndItsContents;
        procedure WhereItCameFromIsKeptEvenWhenItCannotBeRead;
        procedure AnUnchangedFileIsNotReported;
        procedure AnEditedFileIsReported;
        procedure AFileThatCannotBeReadIsNotReportedAsChanged;
        procedure AProjectWithNoRecordedHashIsNotReported;
        procedure AProjectWithNoRecordedPathIsNotReported;
        procedure AnUnchangedSourceHasNothingToSay;
        procedure AChangedSourceIsNamedAndSaidNotToInvalidateAnything;
    end;

    { The half that reads a file. }
    TProvenanceFileTest = class(TTestCase)
    private
        FPath: string;
        procedure WriteSource(const AContent: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFileIsDescribedByItsSizeAndItsContents;
        procedure AnUnchangedFileIsNotReportedAsChanged;
        procedure AnEditedFileIsReported;
        procedure AFileEditedBackToWhatItWasIsNotReported;
        procedure AFileThatIsGoneCannotBeCheckedAndIsNotReported;
        procedure AProjectWithNoRecordedHashIsNotReported;
        procedure WhereItCameFromIsKeptEvenWhenTheFileCannotBeRead;
    end;

implementation

{ ---- the pure half --------------------------------------------------------- }

procedure TProvenanceHashTest.TheSameBytesHashTheSame;
begin
    AssertEquals(HashOfBytes('1.0 100'#10'2.0 200'),
        HashOfBytes('1.0 100'#10'2.0 200'));
end;

procedure TProvenanceHashTest.DifferentBytesHashDifferently;
begin
    AssertTrue(HashOfBytes('1.0 100') <> HashOfBytes('1.0 101'));
end;

procedure TProvenanceHashTest.OneChangedByteChangesTheHash;
begin
    //  THE CASE THIS EXISTS FOR: one value edited in a data file, months later.
    //  A length check would miss it, and so would a date the editor preserved.
    AssertTrue(HashOfBytes('1.0 100'#10'2.0 200') <>
        HashOfBytes('1.0 100'#10'2.0 300'));
end;

procedure TProvenanceHashTest.ReorderedBytesAreNotTheSameFile;
begin
    //  Same bytes, same length, different file - which rules out anything that
    //  sums or counts rather than hashing.
    AssertTrue(HashOfBytes('ab') <> HashOfBytes('ba'));
end;

procedure TProvenanceHashTest.EmptyContentStillHashes;
begin
    //  An empty data file is a real thing to have started from, and recording
    //  it must not read as "no hash was recorded" - which is what an empty
    //  answer here would mean to SourceHasChanged.
    AssertTrue('not empty', HashOfBytes('') <> '');
    AssertTrue('and not the same as anything else',
        HashOfBytes('') <> HashOfBytes(' '));
end;

procedure TProvenanceHashTest.AHashIsLowerCaseHexOfAFixedLength;
var
    H: string;
    i: longint;
begin
    //  It goes into a JSON string and is compared as text, so its shape is part
    //  of the file format: a reader must not have to case-fold it.
    H := HashOfBytes('anything at all');
    AssertEquals('fixed length', 32, Length(H));
    for i := 1 to Length(H) do
        AssertTrue('lower-case hex at ' + IntToStr(i),
            ((H[i] >= '0') and (H[i] <= '9')) or
            ((H[i] >= 'a') and (H[i] <= 'f')));
end;

{ ---- the decisions, over a reader that touches nothing --------------------- }

var
    { What the fake reader hands back, and whether it succeeds. Unit-level
      because a plain function pointer cannot close over a fixture - the seam is
      deliberately the simplest thing that removes the file system. }
    FakeBytes: string;
    FakeOk: boolean;

function FakeReader(const APath: string; out AOk: boolean): string;
begin
    AOk := FakeOk;
    Result := FakeBytes;
end;

procedure TProvenanceHashTest.DescribingAFileRecordsItsSizeAndItsContents;
var
    P: TProjectProvenance;
begin
    FakeBytes := '1.0 100';
    FakeOk := True;
    AssertTrue('read', DescribeSourceFile('d.dat', 'DAT', P, @FakeReader));
    AssertEquals('where it came from', 'd.dat', P.SourcePath);
    AssertEquals('which loader read it', 'DAT', P.LoaderName);
    AssertEquals('how big it was', 7, P.SourceSize);
    AssertEquals('and what was in it', HashOfBytes('1.0 100'), P.SourceHash);
end;

procedure TProvenanceHashTest.WhereItCameFromIsKeptEvenWhenItCannotBeRead;
var
    P: TProjectProvenance;
begin
    //  "We came from there and it is not there now" is precisely what the user
    //  needs told, so the path survives a failed read.
    FakeOk := False;
    AssertFalse('not read', DescribeSourceFile('gone.dat', 'DAT', P,
        @FakeReader));
    AssertEquals('but the path is kept', 'gone.dat', P.SourcePath);
    AssertEquals('and the loader', 'DAT', P.LoaderName);
    AssertEquals('with no hash to compare against later', '', P.SourceHash);
end;

procedure TProvenanceHashTest.AnUnchangedFileIsNotReported;
var
    P: TProjectProvenance;
begin
    FakeBytes := '1.0 100';
    FakeOk := True;
    DescribeSourceFile('d.dat', 'DAT', P, @FakeReader);
    AssertFalse(SourceHasChanged(P, @FakeReader));
end;

procedure TProvenanceHashTest.AnEditedFileIsReported;
var
    P: TProjectProvenance;
begin
    FakeBytes := '1.0 100';
    FakeOk := True;
    DescribeSourceFile('d.dat', 'DAT', P, @FakeReader);
    FakeBytes := '1.0 101';
    AssertTrue(SourceHasChanged(P, @FakeReader));
end;

procedure TProvenanceHashTest.AFileThatCannotBeReadIsNotReportedAsChanged;
var
    P: TProjectProvenance;
begin
    //  CANNOT TELL IS NOT HAS CHANGED. The drive may not be mounted today, and
    //  a warning that fires whenever a share is offline stops being read -
    //  which costs the one time it was right.
    FakeBytes := '1.0 100';
    FakeOk := True;
    DescribeSourceFile('d.dat', 'DAT', P, @FakeReader);
    FakeOk := False;
    AssertFalse(SourceHasChanged(P, @FakeReader));
end;

procedure TProvenanceHashTest.AProjectWithNoRecordedHashIsNotReported;
var
    P: TProjectProvenance;
begin
    //  Every project written before provenance existed. Reporting those would
    //  fire on all of them at once and teach the user to ignore the warning
    //  before it was ever right.
    P := Default(TProjectProvenance);
    P.SourcePath := 'd.dat';
    FakeBytes := 'anything';
    FakeOk := True;
    AssertFalse(SourceHasChanged(P, @FakeReader));
end;

procedure TProvenanceHashTest.AProjectWithNoRecordedPathIsNotReported;
var
    P: TProjectProvenance;
begin
    P := Default(TProjectProvenance);
    P.SourceHash := HashOfBytes('1.0 100');
    FakeOk := True;
    AssertFalse(SourceHasChanged(P, @FakeReader));
end;

procedure TProvenanceHashTest.AnUnchangedSourceHasNothingToSay;
var
    P: TProjectProvenance;
begin
    //  SILENT IS THE ORDINARY CASE. A notice on every open would be a notice
    //  nobody reads by the time one matters.
    FakeBytes := '1.0 100';
    FakeOk := True;
    DescribeSourceFile('d.dat', 'DAT', P, @FakeReader);
    AssertEquals('', SourceChangeNotice(P, @FakeReader));
end;

procedure TProvenanceHashTest.AChangedSourceIsNamedAndSaidNotToInvalidateAnything;
var
    P: TProjectProvenance;
    Notice: string;
begin
    //  IT NAMES THE FILE, so the reader is not left working out which one -
    //  and it says what it does NOT mean. The project carries its own profile,
    //  so the results are still the results that were fitted; telling someone
    //  their work is suspect when it is not is worse than saying nothing.
    FakeBytes := '1.0 100';
    FakeOk := True;
    DescribeSourceFile('runs/d.dat', 'DAT', P, @FakeReader);
    FakeBytes := '1.0 101';
    Notice := SourceChangeNotice(P, @FakeReader);
    AssertTrue('there is one', Notice <> '');
    AssertTrue('naming the file: ' + Notice, Pos('runs/d.dat', Notice) > 0);
    AssertTrue('and saying nothing here has changed: ' + Notice,
        Pos('nothing here has changed', Notice) > 0);
end;

{ ---- the half that reads a file -------------------------------------------- }

procedure TProvenanceFileTest.SetUp;
begin
    FPath := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-prov-' + FormatDateTime('hhnnsszzz', Now) + '.dat';
end;

procedure TProvenanceFileTest.TearDown;
begin
    if (FPath <> '') and FileExists(FPath) then
        DeleteFile(FPath);
end;

procedure TProvenanceFileTest.WriteSource(const AContent: string);
var
    S: TFileStream;
begin
    S := TFileStream.Create(FPath, fmCreate);
    try
        if AContent <> '' then
            S.WriteBuffer(AContent[1], Length(AContent));
    finally
        S.Free;
    end;
end;

procedure TProvenanceFileTest.AFileIsDescribedByItsSizeAndItsContents;
var
    P: TProjectProvenance;
begin
    WriteSource('1.0 100'#10'2.0 200');
    AssertTrue('read', DescribeSourceFile(FPath, 'DAT', P));
    AssertEquals('where it came from', FPath, P.SourcePath);
    AssertEquals('which loader read it', 'DAT', P.LoaderName);
    AssertEquals('how big it was', 15, P.SourceSize);
    AssertEquals('and what was in it', HashOfBytes('1.0 100'#10'2.0 200'),
        P.SourceHash);
end;

procedure TProvenanceFileTest.AnUnchangedFileIsNotReportedAsChanged;
var
    P: TProjectProvenance;
begin
    WriteSource('1.0 100');
    DescribeSourceFile(FPath, 'DAT', P);
    AssertFalse(SourceHasChanged(P));
end;

procedure TProvenanceFileTest.AnEditedFileIsReported;
var
    P: TProjectProvenance;
begin
    WriteSource('1.0 100');
    DescribeSourceFile(FPath, 'DAT', P);
    WriteSource('1.0 101');
    AssertTrue(SourceHasChanged(P));
end;

procedure TProvenanceFileTest.AFileEditedBackToWhatItWasIsNotReported;
var
    P: TProjectProvenance;
begin
    //  The question is what the file SAYS, not what has happened to it. A file
    //  edited and put back is the file the numbers came from.
    WriteSource('1.0 100');
    DescribeSourceFile(FPath, 'DAT', P);
    WriteSource('something else entirely');
    WriteSource('1.0 100');
    AssertFalse(SourceHasChanged(P));
end;

procedure TProvenanceFileTest.AFileThatIsGoneCannotBeCheckedAndIsNotReported;
var
    P: TProjectProvenance;
begin
    //  CANNOT TELL IS NOT HAS CHANGED. The drive may not be mounted today, and
    //  a warning that fires whenever a network share is offline is a warning
    //  that stops being read - which costs the one time it was right.
    WriteSource('1.0 100');
    DescribeSourceFile(FPath, 'DAT', P);
    DeleteFile(FPath);
    AssertFalse(SourceHasChanged(P));
end;

procedure TProvenanceFileTest.AProjectWithNoRecordedHashIsNotReported;
var
    P: TProjectProvenance;
begin
    //  Every project written before provenance existed. Reporting those as
    //  changed would fire on all of them at once and teach the user to ignore
    //  it before it was ever right.
    P := Default(TProjectProvenance);
    P.SourcePath := FPath;
    WriteSource('1.0 100');
    AssertFalse('no hash recorded', SourceHasChanged(P));

    P.SourcePath := '';
    P.SourceHash := HashOfBytes('1.0 100');
    AssertFalse('nor any path', SourceHasChanged(P));
end;

procedure TProvenanceFileTest.WhereItCameFromIsKeptEvenWhenTheFileCannotBeRead;
var
    P: TProjectProvenance;
begin
    //  "We came from there and it is not there now" is precisely what the user
    //  needs told, so the path survives a failed read.
    AssertFalse('not read', DescribeSourceFile(FPath + '.nowhere', 'DAT', P));
    AssertEquals('but the path is kept', FPath + '.nowhere', P.SourcePath);
    AssertEquals('and so is the loader', 'DAT', P.LoaderName);
    AssertEquals('with no hash to compare against later', '', P.SourceHash);
end;

initialization
    //  Hashing bytes touches nothing.
    RegisterTest('unit', TProvenanceHashTest);
    //  Reading a file is an external dependency exactly as a socket is.
    RegisterTest('integration', TProvenanceFileTest);
end.
