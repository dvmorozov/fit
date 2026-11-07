// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The project file's container: a set of named parts, and what survives
being read by a build that does not understand all of them.)

WHY A CONTAINER OF PARTS AT ALL. A project file has to carry sections that do not
exist yet - a recipe for batch runs, a module's analysis, the provenance of data
fetched from a public source. If those were members of one document, every future
feature would edit one schema that every other feature also edits. A part is the
unit of extension instead: a feature adds one, and nothing else is touched.

THE INVARIANT THAT MAKES THAT REAL, and the reason this unit exists rather than a
call to a zip library at the point of use: A PART THIS BUILD DOES NOT UNDERSTAND
IS WRITTEN BACK UNCHANGED. Opening a newer project in an older build and saving
it must not destroy what the newer one wrote. Nothing about a version number
achieves that on its own - it is a property of the read/modify/write path, and it
is checked here.

WHY THE SURFACE IS A STREAM. The filesystem is an external dependency by this
project's own rule, and a test that needs one is an integration test. Everything
this unit decides - the part list, the ordering, the preservation - is decided
over bytes, so it is driven here through a TMemoryStream and the whole codec
stays in the measured unit half. Only the thin file wrappers touch a disk.
}
unit testcase_project_archive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_project_archive;

type
    TProjectArchiveTest = class(TTestCase)
    private
        { Writes AParts to a stream and reads them straight back. }
        function RoundTrip(const AParts: TProjectParts): TProjectParts;
        { The part named AName, or '' when there is none. }
        function ContentOf(const AParts: TProjectParts;
            const AName: string): string;
        { A two-part archive, as an ordinary project would hold. }
        function TwoParts: TProjectParts;
    published
        procedure APartWrittenIsThePartReadBack;
        procedure EveryPartSurvivesTogether;
        procedure APartNameMayCarryAPathSeparator;
        procedure ContentIsCarriedByteForByte;
        procedure AnEmptyPartIsStillAPart;
        procedure AnArchiveOfNoPartsIsNotAProject;

        //  The invariant the whole design rests on.
        procedure APartTheReaderDoesNotKnowIsWrittenBackUnchanged;
        procedure ReplacingOnePartLeavesTheOthersAlone;
        procedure ReplacingAPartThatIsNotThereAddsIt;

        //  What is not an archive.
        procedure BytesThatAreNotAnArchiveAreRefused;
        procedure AnEmptyStreamIsRefused;
        procedure ATruncatedArchiveIsRefused;

        //  Looking a part up.
        procedure APartIsFoundByItsExactName;
        procedure AnAbsentPartIsReportedRatherThanGuessed;
    end;

implementation

const
    Manifest = '{"formatVersion":1}';
    Problem  = '{"profile":{"x":[1,2],"y":[3,4]}}';

function TProjectArchiveTest.RoundTrip(
    const AParts: TProjectParts): TProjectParts;
var
    S: TMemoryStream;
begin
    Result := nil;
    S := TMemoryStream.Create;
    try
        AssertTrue('written', WriteProjectArchive(AParts, S));
        S.Position := 0;
        AssertTrue('read back', ReadProjectArchive(S, Result));
    finally
        S.Free;
    end;
end;

function TProjectArchiveTest.ContentOf(const AParts: TProjectParts;
    const AName: string): string;
begin
    if not PartContent(AParts, AName, Result) then
        Result := '';
end;

function TProjectArchiveTest.TwoParts: TProjectParts;
begin
    Result := nil;
    Result := WithPart(Result, 'manifest.json', Manifest);
    Result := WithPart(Result, 'problem.json', Problem);
end;

procedure TProjectArchiveTest.APartWrittenIsThePartReadBack;
var
    Got: TProjectParts;
begin
    Got := RoundTrip(WithPart(nil, 'manifest.json', Manifest));
    AssertEquals('one part', 1, Length(Got));
    AssertEquals('its name', 'manifest.json', Got[0].Name);
    AssertEquals('its content', Manifest, Got[0].Content);
end;

procedure TProjectArchiveTest.EveryPartSurvivesTogether;
var
    Got: TProjectParts;
begin
    Got := RoundTrip(TwoParts);
    AssertEquals('both', 2, Length(Got));
    AssertEquals('', Manifest, ContentOf(Got, 'manifest.json'));
    AssertEquals('', Problem, ContentOf(Got, 'problem.json'));
end;

procedure TProjectArchiveTest.APartNameMayCarryAPathSeparator;
var
    Got: TProjectParts;
begin
    //  A module's own document lives at modules/<name>.json, so the name is a
    //  path and must come back as the same path - not rewritten to the host
    //  platform's separator, which would make a project written on Windows name
    //  a different part when read on Linux.
    Got := RoundTrip(WithPart(nil, 'modules/sample.json', '{"marks":[]}'));
    AssertEquals('one part', 1, Length(Got));
    AssertEquals('the name is the name that was written',
        'modules/sample.json', Got[0].Name);
    AssertEquals('', '{"marks":[]}', Got[0].Content);
end;

procedure TProjectArchiveTest.ContentIsCarriedByteForByte;
var
    Got: TProjectParts;
    Payload: string;
begin
    //  THE CONTAINER HAS NO OPINION ABOUT THE CONTENT. It carries bytes: the
    //  sections are UTF-8 JSON, and a title taken from a file name may hold any
    //  byte at all. Anything that re-encoded here would corrupt a section
    //  without failing, which is the worst way for a document format to break.
    Payload := 'line1'#13#10'line2'#10'tab'#9'"quote"\backslash '#$C2#$B1' end';
    Got := RoundTrip(WithPart(nil, 'problem.json', Payload));
    AssertEquals('every byte, including the line endings and the non-ASCII one',
        Payload, Got[0].Content);
end;

procedure TProjectArchiveTest.AnEmptyPartIsStillAPart;
var
    Got: TProjectParts;
begin
    //  A section a project holds but has nothing in yet is not the same as one
    //  it does not hold, and the difference has to survive: an absent section
    //  means "this build wrote none", an empty one means "there is nothing in
    //  it". A restore reads those differently.
    Got := RoundTrip(WithPart(nil, 'ui.json', ''));
    AssertEquals('the part is there', 1, Length(Got));
    AssertEquals('and it is empty', '', Got[0].Content);
end;

procedure TProjectArchiveTest.AnArchiveOfNoPartsIsNotAProject;
var
    S: TMemoryStream;
    Got: TProjectParts;
begin
    //  CHARACTERISED, and it is the right answer rather than a limitation to
    //  work around. Writing no parts succeeds and produces a container with an
    //  empty directory, which reads back as "not an archive" - and a project
    //  file always carries a manifest, so the document layer never writes one.
    //  Pinned here so that nobody later mistakes the refusal for a defect and
    //  makes an empty file load as an empty project, which is the one outcome
    //  that would lose a user's work quietly.
    S := TMemoryStream.Create;
    try
        AssertTrue('writing nothing is not itself a failure',
            WriteProjectArchive(nil, S));
        S.Position := 0;
        AssertFalse('but there is no project in it', ReadProjectArchive(S, Got));
        AssertEquals('and nothing is handed back', 0, Length(Got));
    finally
        S.Free;
    end;
end;

procedure TProjectArchiveTest.APartTheReaderDoesNotKnowIsWrittenBackUnchanged;
var
    Once, Twice: TProjectParts;
begin
    //  THE ONE THIS UNIT EXISTS FOR. An older build opens a project a newer one
    //  wrote, changes the section it understands, and saves. The section it has
    //  never heard of must come back out of the file exactly as it went in -
    //  otherwise opening a project in yesterday's build silently destroys work
    //  done in today's, and no version number in a header prevents that.
    Once := RoundTrip(WithPart(TwoParts, 'future/recipe.json',
        '{"steps":[{"do":"something this build cannot name"}]}'));

    //  What a reader that knows only two of the three would do: rewrite one,
    //  leave the rest as read, save.
    Twice := RoundTrip(WithPart(Once, 'problem.json', '{"profile":{}}'));

    AssertEquals('all three parts are still there', 3, Length(Twice));
    AssertEquals('the edit landed', '{"profile":{}}',
        ContentOf(Twice, 'problem.json'));
    AssertEquals('and the unknown part is byte for byte what it was',
        '{"steps":[{"do":"something this build cannot name"}]}',
        ContentOf(Twice, 'future/recipe.json'));
end;

procedure TProjectArchiveTest.ReplacingOnePartLeavesTheOthersAlone;
var
    Parts: TProjectParts;
begin
    Parts := WithPart(TwoParts, 'problem.json', 'replaced');
    AssertEquals('no part was added', 2, Length(Parts));
    AssertEquals('the named one changed', 'replaced',
        ContentOf(Parts, 'problem.json'));
    AssertEquals('the other did not', Manifest,
        ContentOf(Parts, 'manifest.json'));
end;

procedure TProjectArchiveTest.ReplacingAPartThatIsNotThereAddsIt;
var
    Parts: TProjectParts;
begin
    Parts := WithPart(TwoParts, 'ui.json', '{"tab":1}');
    AssertEquals('appended', 3, Length(Parts));
    AssertEquals('', '{"tab":1}', ContentOf(Parts, 'ui.json'));
    AssertEquals('and it went on the end, so the order is stable',
        'ui.json', Parts[2].Name);
end;

procedure TProjectArchiveTest.BytesThatAreNotAnArchiveAreRefused;
var
    S: TMemoryStream;
    Got: TProjectParts;
    Junk: string;
begin
    //  FALSE, not an exception. This reads a file the user chose, and a wrong
    //  choice - a .dat, a document, anything - has to come back as "that is not
    //  a project" rather than as a fault.
    Junk := 'this is not a zip archive, it is a sentence';
    S := TMemoryStream.Create;
    try
        S.Write(Junk[1], Length(Junk));
        S.Position := 0;
        AssertFalse('refused', ReadProjectArchive(S, Got));
    finally
        S.Free;
    end;
end;

procedure TProjectArchiveTest.AnEmptyStreamIsRefused;
var
    S: TMemoryStream;
    Got: TProjectParts;
begin
    S := TMemoryStream.Create;
    try
        AssertFalse('refused', ReadProjectArchive(S, Got));
    finally
        S.Free;
    end;
end;

procedure TProjectArchiveTest.ATruncatedArchiveIsRefused;
var
    Whole, Cut: TMemoryStream;
    Got: TProjectParts;
begin
    //  A file that stopped being written - a full disk, a killed process. It is
    //  a real archive for most of its length, so nothing but actually reading
    //  it can tell.
    Whole := TMemoryStream.Create;
    Cut := TMemoryStream.Create;
    try
        AssertTrue('written', WriteProjectArchive(TwoParts, Whole));
        Whole.Position := 0;
        Cut.CopyFrom(Whole, Whole.Size div 2);
        Cut.Position := 0;
        AssertFalse('refused', ReadProjectArchive(Cut, Got));
    finally
        Whole.Free;
        Cut.Free;
    end;
end;

procedure TProjectArchiveTest.APartIsFoundByItsExactName;
var
    Parts: TProjectParts;
    Content: string;
begin
    Parts := TwoParts;
    AssertTrue('found', PartContent(Parts, 'problem.json', Content));
    AssertEquals('', Problem, Content);
    AssertEquals('by index too', 1, IndexOfPart(Parts, 'problem.json'));
end;

procedure TProjectArchiveTest.AnAbsentPartIsReportedRatherThanGuessed;
var
    Parts: TProjectParts;
    Content: string;
begin
    //  No nearest match and no case folding: a part name is an identifier in a
    //  format, and answering a question about 'Problem.json' with the contents
    //  of 'problem.json' would make one platform's file readable and another's
    //  not, depending on nothing visible.
    Parts := TwoParts;
    AssertFalse('not there', PartContent(Parts, 'results.json', Content));
    AssertEquals('and nothing is handed back', '', Content);
    AssertEquals('nor by index', -1, IndexOfPart(Parts, 'results.json'));
    AssertEquals('and the name is compared exactly', -1,
        IndexOfPart(Parts, 'Problem.json'));
end;

initialization
    //  A UNIT test: every byte goes through a TMemoryStream, so nothing here
    //  touches the filesystem. The file wrappers are tested separately, and
    //  those are integration tests.
    RegisterTest('unit', TProjectArchiveTest);
end.
