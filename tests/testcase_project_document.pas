// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The project document: what a saved session holds, what it deliberately
does not, and what survives a round trip through a build that knows less than the
one that wrote it.)

THE DOCUMENT IS THE INPUTS PLUS THE FITTED VALUES, AND NOTHING ELSE. The server
demolishes and rebuilds every curve instance from its inputs on every model edit,
so a file that stores those inputs stores exactly what the running engine stores.
Anything derived - the calculated profile, the delta, where the built curves sit,
the per-round attribute report - is a second source of truth that can disagree
with the model, and a stale derived value that quietly contradicts its inputs is
this codebase's signature failure. The counter-case is the fitted values
themselves: they come from an optimiser run rather than from the inputs, so they
MUST be stored, and re-attaching them under their handles is the entire point.

WHAT THESE TESTS ARE FOR, in order of weight:

  * a section survives a round trip unchanged - the ordinary thing;
  * a MEMBER this build has never heard of survives it too, which is what makes
    the format extensible rather than merely versioned;
  * a handle stays TEXT. A GUID written as a JSON number arrives as 0, which is
    a defect this protocol has already had once, and here it would silently
    orphan every fitted value in the file;
  * a version from the future is refused with a message naming it, rather than
    half-read;
  * a missing optional section is empty, not a failure: a project saved before a
    fit ever ran has no results.
}
unit testcase_project_document;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry,
    fit_project_archive, fit_project_document, fit_project_json;

type
    TProjectDocumentTest = class(TTestCase)
    private
        { A document with something in every section. }
        function AFullDocument: TProjectDocument;
        { Writes ADoc to parts and reads it straight back. }
        function RoundTrip(const ADoc: TProjectDocument): TProjectDocument;
    published
        //  The shape of the file.
        procedure EverySectionThisBuildWritesIsAPartOfTheArchive;
        procedure ADocumentWithNoManifestIsNotAProject;
        procedure BytesThatAreNotAnArchiveAreNotAProject;

        //  What survives.
        procedure TheProfileSurvivesARoundTrip;
        procedure EveryPickKeepsItsHandleThroughTheFile;
        procedure AHandleIsWrittenAsTextRatherThanANumber;
        procedure TheSettingsSurviveARoundTrip;
        procedure TheFittedValuesAndTheirErrorsSurvive;
        procedure WhetherAnInstanceWasFittedSurvives;
        procedure TheSelectedIntervalSurvivesAsIndices;
        procedure TheUserDefinedFormulaSurvives;
        procedure TheWorkingUiContextSurvives;
        procedure TheProvenanceOfTheSourceFileSurvives;

        //  What is deliberately absent.
        procedure NoDerivedSetHasAPlaceInTheDocument;

        //  Versioning and extension.
        procedure AVersionFromTheFutureIsRefusedNamingIt;
        procedure AVersionThisBuildCanReadIsAccepted;
        procedure AMemberThisBuildDoesNotKnowIsWrittenBackOut;
        procedure APartThisBuildDoesNotKnowIsWrittenBackOut;
        procedure AMissingOptionalSectionIsEmptyRatherThanAFailure;
        procedure AnAbsentResultsSectionMeansNothingWasFitted;

        //  Which part belongs to which module.
        procedure AModulePartIsNamedAfterItsModule;
        procedure APartThatIsNotAModulesNamesNoModule;
        procedure AModuleDocumentSurvivesARoundTrip;
        procedure AModulePartForAModuleThisBuildLacksIsStillRead;

        //  When a project was made, and when it was last written.
        procedure StampingWritesAModifiedTimeInUtc;
        procedure ARewriteKeepsTheTimeTheProjectCameIntoBeing;
        procedure AFirstWriteIsCreatedAndModifiedAtOnce;
        procedure TheStampsSurviveARoundTrip;

        //  A section that is the wrong shape. Each of these is a hand-edited
        //  or truncated file, and none may take the rest of the project
        //  down with it.
        procedure ASectionOfTheWrongShapeIsSkippedNotFatal;
        procedure ACurveThatIsNotAnObjectIsSkipped;
        procedure AParameterThatIsNotAnObjectIsSkipped;
        procedure APointSetThatIsNotAnObjectReadsAsEmpty;
    end;

implementation

function TProjectDocumentTest.AFullDocument: TProjectDocument;
begin
    Result := EmptyProjectDocument;

    SetLength(Result.Profile.X, 3);
    SetLength(Result.Profile.Y, 3);
    Result.Profile.X[0] := 1.5; Result.Profile.Y[0] := 10;
    Result.Profile.X[1] := 2.5; Result.Profile.Y[1] := 20;
    Result.Profile.X[2] := 3.5; Result.Profile.Y[2] := 30;
    Result.Profile.Title := 'profile';

    SetLength(Result.Background.X, 1);
    SetLength(Result.Background.Y, 1);
    Result.Background.X[0] := 1.5; Result.Background.Y[0] := 9;

    SetLength(Result.Bounds.X, 2);
    SetLength(Result.Bounds.Y, 2);
    Result.Bounds.X[0] := 1.5; Result.Bounds.Y[0] := 0;
    Result.Bounds.X[1] := 3.5; Result.Bounds.Y[1] := 0;

    SetLength(Result.Positions.X, 2);
    SetLength(Result.Positions.Y, 2);
    SetLength(Result.Positions.Ids, 2);
    Result.Positions.X[0] := 2.5; Result.Positions.Y[0] := 20;
    Result.Positions.Ids[0] := '0a0a0a0a-1111-2222-3333-444444444444';
    Result.Positions.X[1] := 3.5; Result.Positions.Y[1] := 30;
    Result.Positions.Ids[1] := '0b0b0b0b-1111-2222-3333-444444444444';

    Result.Settings.CurveTypeId := '{FF4E399C-0000-0000-0000-000000000000}';
    Result.Settings.WaveLength := 1.54056;
    Result.Settings.MaxRFactor := 0.05;
    Result.Settings.BackFactor := 0.5;
    Result.Settings.CurveThresh := 0.01;
    Result.Settings.MinimizerKind := 1;
    Result.Settings.LossKind := 2;
    Result.Settings.Weighting := 'poisson';
    Result.Settings.BackgroundVariationEnabled := True;
    Result.Settings.CurveScalingEnabled := True;

    Result.SelectedIntervalInForce := True;
    Result.SelectedIntervalFrom := 1;
    Result.SelectedIntervalTo := 2;

    Result.HasUserCurve := True;
    Result.UserCurveExpression := 'A*exp(-x/tau)';
    SetLength(Result.UserCurveParams, 1);
    Result.UserCurveParams[0].Name := 'tau';
    Result.UserCurveParams[0].Value := 3.25;
    Result.UserCurveParams[0].Error := -1;

    SetLength(Result.Curves, 1);
    Result.Curves[0].Id := '0a0a0a0a-1111-2222-3333-444444444444';
    Result.Curves[0].Fitted := True;
    SetLength(Result.Curves[0].Params, 2);
    Result.Curves[0].Params[0].Name := 'sigma';
    Result.Curves[0].Params[0].Value := 1.234567890123456;
    Result.Curves[0].Params[0].Error := 0.001;
    Result.Curves[0].Params[1].Name := 'A';
    Result.Curves[0].Params[1].Value := 100;
    Result.Curves[0].Params[1].Error := -1;

    Result.RFactor := 0.0321;
    Result.Statistics.Valid := True;
    Result.Statistics.DataPoints := 3;
    Result.Statistics.Params := 2;
    Result.Statistics.DegreesOfFreedom := 1;
    Result.Statistics.ChiSquare := 1.5;
    Result.Statistics.ReducedChiSquare := 1.5;
    Result.Statistics.RSquared := 0.99;
    Result.Statistics.AIC := 4.5;
    Result.Statistics.BIC := 5.5;

    Result.Provenance.SourcePath := 'C:\data\2.dat';
    Result.Provenance.SourceSize := 4096;
    Result.Provenance.SourceHash := 'abc123';
    Result.Provenance.LoaderName := 'DAT';
    Result.Provenance.AppVersion := '1.2.0.1720';

    Result.HasUi := True;
    Result.Ui.ViewMode := 2;
    Result.Ui.ViewModeChosenByUser := True;
    Result.Ui.SelectionMode := 3;
    Result.Ui.ActiveTab := 1;
    Result.Ui.SelectedCurveId := '0a0a0a0a-1111-2222-3333-444444444444';
    Result.Ui.CustomAxisName := 'd';
    Result.Ui.CustomAxisUnit := 'A';
    Result.Ui.CustomAxisForward := 'x*2';
    Result.Ui.CustomAxisInverse := 'x/2';
end;

function TProjectDocumentTest.RoundTrip(
    const ADoc: TProjectDocument): TProjectDocument;
var
    Parts: TProjectParts;
    Fault: string;
    Opened: boolean;
begin
    Parts := ProjectToParts(ADoc);
    Result := EmptyProjectDocument;
    Opened := ProjectFromParts(Parts, Result, Fault);
    AssertTrue('read back: ' + Fault, Opened);
end;

procedure TProjectDocumentTest.EverySectionThisBuildWritesIsAPartOfTheArchive;
var
    Parts: TProjectParts;
begin
    //  A REGISTRY WALK, not a list repeated here: every section this build
    //  declares must actually be written, so adding a section without wiring it
    //  in fails the suite rather than producing a file quietly missing it.
    Parts := ProjectToParts(AFullDocument);
    AssertTrue('the manifest is always there',
        IndexOfPart(Parts, ProjectManifestPart) >= 0);
    AssertTrue('the inputs', IndexOfPart(Parts, ProjectProblemPart) >= 0);
    AssertTrue('the results', IndexOfPart(Parts, ProjectResultsPart) >= 0);
    AssertTrue('the working context', IndexOfPart(Parts, ProjectUiPart) >= 0);
end;

procedure TProjectDocumentTest.ADocumentWithNoManifestIsNotAProject;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  The manifest is what says this is a project and what version it is.
    //  Without it there is nothing to check a version against, and reading the
    //  other parts anyway would mean guessing at their shape.
    Parts := nil;
    Parts := WithPart(Parts, ProjectProblemPart, '{"profile":{"x":[],"y":[]}}');
    AssertFalse('refused', ProjectFromParts(Parts, Doc, Fault));
    AssertTrue('and says why', Fault <> '');
end;

procedure TProjectDocumentTest.BytesThatAreNotAnArchiveAreNotAProject;
var
    S: TMemoryStream;
    Doc: TProjectDocument;
    Fault: string;
    Junk: string;
begin
    //  The user chose a file. Choosing a .dat by mistake has to be reported as
    //  "that is not a project", never as a crash.
    Junk := 'not an archive';
    S := TMemoryStream.Create;
    try
        S.Write(Junk[1], Length(Junk));
        S.Position := 0;
        AssertFalse('refused', ReadProjectFromStream(S, Doc, Fault));
        AssertTrue('and says why', Fault <> '');
    finally
        S.Free;
    end;
end;

procedure TProjectDocumentTest.TheProfileSurvivesARoundTrip;
var
    Got: TProjectDocument;
begin
    //  THE PROFILE AS THE SERVER HOLDS IT, not the file it came from:
    //  subtracting a background and smoothing both rewrite it in place and are
    //  not replayable from the source, so the source's points would restore a
    //  different problem from the one that was saved.
    Got := RoundTrip(AFullDocument);
    AssertEquals('every sample', 3, Length(Got.Profile.X));
    AssertEquals('', 2.5, Got.Profile.X[1], 1e-12);
    AssertEquals('', 30.0, Got.Profile.Y[2], 1e-12);
    AssertEquals('the background picks', 1, Length(Got.Background.X));
    AssertEquals('the fit interval, as a pair', 2, Length(Got.Bounds.X));
end;

procedure TProjectDocumentTest.EveryPickKeepsItsHandleThroughTheFile;
var
    Got: TProjectDocument;
begin
    //  THE WHOLE POINT OF THE FILE. Without the handles the picks restore and
    //  the values do not attach to them, so the model comes back at its seeds
    //  and the fit silently starts over.
    Got := RoundTrip(AFullDocument);
    AssertEquals('one handle per pick', 2, Length(Got.Positions.Ids));
    AssertEquals('and each with its own pick',
        '0a0a0a0a-1111-2222-3333-444444444444', Got.Positions.Ids[0]);
    AssertEquals('', '0b0b0b0b-1111-2222-3333-444444444444',
        Got.Positions.Ids[1]);
    AssertEquals('beside the abscissa it belongs to', 3.5, Got.Positions.X[1],
        1e-12);
end;

procedure TProjectDocumentTest.AHandleIsWrittenAsTextRatherThanANumber;
var
    Parts: TProjectParts;
    Problem: string;
begin
    //  A GUID WRITTEN AS A JSON NUMBER ARRIVES AS 0. That is not hypothetical -
    //  it is a defect this protocol has already had, and it is why the curves
    //  route carries a `kind` field. Here the cost would be worse than a wrong
    //  display: every fitted value in the file would orphan at once.
    Parts := ProjectToParts(AFullDocument);
    AssertTrue('found', PartContent(Parts, ProjectProblemPart, Problem));
    AssertTrue('the handle is quoted in the file',
        Pos('"0a0a0a0a-1111-2222-3333-444444444444"', Problem) > 0);
end;

procedure TProjectDocumentTest.TheSettingsSurviveARoundTrip;
var
    Got: TProjectDocument;
begin
    Got := RoundTrip(AFullDocument);
    AssertEquals('the curve type', '{FF4E399C-0000-0000-0000-000000000000}',
        Got.Settings.CurveTypeId);
    AssertEquals('', 1.54056, Got.Settings.WaveLength, 1e-12);
    AssertEquals('', 0.05, Got.Settings.MaxRFactor, 1e-12);
    AssertEquals('', 0.5, Got.Settings.BackFactor, 1e-12);
    AssertEquals('', 0.01, Got.Settings.CurveThresh, 1e-12);
    AssertEquals('', 1, Got.Settings.MinimizerKind);
    AssertEquals('the objective', 2, Got.Settings.LossKind);
    AssertEquals('', 'poisson', Got.Settings.Weighting);
    AssertTrue('', Got.Settings.BackgroundVariationEnabled);
    AssertTrue('', Got.Settings.CurveScalingEnabled);
end;

procedure TProjectDocumentTest.TheFittedValuesAndTheirErrorsSurvive;
var
    Got: TProjectDocument;
begin
    //  AT FULL PRECISION. A pick's abscissa and the value stored against its
    //  curve are compared elsewhere with tolerances as tight as 1e-9, so a
    //  value that loses digits here comes back as a different value.
    Got := RoundTrip(AFullDocument);
    AssertEquals('one curve', 1, Length(Got.Curves));
    AssertEquals('under its handle',
        '0a0a0a0a-1111-2222-3333-444444444444', Got.Curves[0].Id);
    AssertEquals('two parameters', 2, Length(Got.Curves[0].Params));
    AssertEquals('by name', 'sigma', Got.Curves[0].Params[0].Name);
    AssertEquals('to the last digit', 1.234567890123456,
        Got.Curves[0].Params[0].Value, 1e-15);
    AssertEquals('with its error', 0.001, Got.Curves[0].Params[0].Error, 1e-15);
    AssertEquals('and "no estimate" stays -1', -1.0,
        Got.Curves[0].Params[1].Error, 1e-15);
    AssertEquals('the R-factor of the fit that was saved', 0.0321,
        Got.RFactor, 1e-12);
    AssertTrue('and its statistics', Got.Statistics.Valid);
    AssertEquals('', 0.99, Got.Statistics.RSquared, 1e-12);
end;

procedure TProjectDocumentTest.WhetherAnInstanceWasFittedSurvives;
var
    Doc, Got: TProjectDocument;
begin
    //  IT CANNOT BE DERIVED from the values, because every instance has values
    //  from the moment it is placed. A project saved before any fit carries
    //  seeds; one saved after carries results; and restoring the first as
    //  though it were the second refuses moves to protect a fit that never
    //  happened.
    Got := RoundTrip(AFullDocument);
    AssertTrue('a fitted curve says so', Got.Curves[0].Fitted);

    Doc := AFullDocument;
    Doc.Curves[0].Fitted := False;
    Got := RoundTrip(Doc);
    AssertFalse('and an unfitted one says that', Got.Curves[0].Fitted);
end;

procedure TProjectDocumentTest.TheSelectedIntervalSurvivesAsIndices;
var
    Got: TProjectDocument;
begin
    //  INDICES, not coordinates. The interval is a window on the profile, and
    //  the profile stored here may have been smoothed - so a coordinate would
    //  not necessarily name a sample any more, while an index always does.
    Got := RoundTrip(AFullDocument);
    AssertTrue('in force', Got.SelectedIntervalInForce);
    AssertEquals('', 1, Got.SelectedIntervalFrom);
    AssertEquals('', 2, Got.SelectedIntervalTo);
end;

procedure TProjectDocumentTest.TheUserDefinedFormulaSurvives;
var
    Got: TProjectDocument;
begin
    //  Without it a project whose curve type is the user-defined one cannot be
    //  rebuilt at all: the engine refuses to build that type with no formula.
    Got := RoundTrip(AFullDocument);
    AssertTrue('there is one', Got.HasUserCurve);
    AssertEquals('', 'A*exp(-x/tau)', Got.UserCurveExpression);
    AssertEquals('with its parameters', 1, Length(Got.UserCurveParams));
    AssertEquals('', 'tau', Got.UserCurveParams[0].Name);
    AssertEquals('', 3.25, Got.UserCurveParams[0].Value, 1e-12);
end;

procedure TProjectDocumentTest.TheWorkingUiContextSurvives;
var
    Got: TProjectDocument;
begin
    //  What the user would otherwise have to set up again by hand. Not window
    //  geometry: that is per-machine, and a project opened on a different
    //  display would restore a window off-screen.
    Got := RoundTrip(AFullDocument);
    AssertTrue('', Got.HasUi);
    AssertEquals('the argument axis', 2, Got.Ui.ViewMode);
    AssertTrue('and that the user chose it', Got.Ui.ViewModeChosenByUser);
    AssertEquals('the picking mode', 3, Got.Ui.SelectionMode);
    AssertEquals('the tab in front', 1, Got.Ui.ActiveTab);
    AssertEquals('the selected curve, by handle rather than by row',
        '0a0a0a0a-1111-2222-3333-444444444444', Got.Ui.SelectedCurveId);
    AssertEquals('the user-defined axis', 'd', Got.Ui.CustomAxisName);
    AssertEquals('', 'x*2', Got.Ui.CustomAxisForward);
end;

procedure TProjectDocumentTest.TheProvenanceOfTheSourceFileSurvives;
var
    Got: TProjectDocument;
begin
    //  PROVENANCE, not a dependency: the profile itself is in the file, so the
    //  project opens on a machine that has never seen the source. This is what
    //  lets the application say where the data came from and notice that it has
    //  changed since.
    Got := RoundTrip(AFullDocument);
    AssertEquals('', 'C:\data\2.dat', Got.Provenance.SourcePath);
    AssertEquals('', 4096, Got.Provenance.SourceSize);
    AssertEquals('', 'abc123', Got.Provenance.SourceHash);
    AssertEquals('', 'DAT', Got.Provenance.LoaderName);
    AssertEquals('', '1.2.0.1720', Got.Provenance.AppVersion);
end;

procedure TProjectDocumentTest.NoDerivedSetHasAPlaceInTheDocument;
var
    Parts: TProjectParts;
    Problem, Results: string;

    procedure Absent(const AWhat: string);
    begin
        AssertEquals('the inputs do not carry ' + AWhat, 0,
            Pos(AWhat, Problem));
        AssertEquals('nor do the results', 0, Pos(AWhat, Results));
    end;

begin
    //  AN EXECUTABLE STATEMENT OF THE RULE, so that adding a derived set later
    //  fails here rather than in a support conversation about a project whose
    //  chart disagrees with its own model.
    //
    //  The model is demolished and rebuilt from its inputs on every edit, so
    //  everything below is a pure function of what IS stored. Storing it too
    //  would make a second source of truth that can go stale, and a stale
    //  derived value that quietly contradicts its inputs is this codebase's
    //  signature failure.
    Parts := ProjectToParts(AFullDocument);
    PartContent(Parts, ProjectProblemPart, Problem);
    PartContent(Parts, ProjectResultsPart, Results);
    Absent('calcProfile');
    Absent('deltaProfile');
    Absent('calcPositions');
    Absent('curvePoints');
end;

procedure TProjectDocumentTest.AVersionFromTheFutureIsRefusedNamingIt;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  REFUSED, and it says what it needs. Half-reading a file written by a
    //  build that changed the meaning of a field is how a project comes back
    //  subtly wrong instead of not at all - and the second is recoverable.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectManifestPart,
        '{"formatVersion":99,"minReaderVersion":99}');
    AssertFalse('refused', ProjectFromParts(Parts, Doc, Fault));
    AssertTrue('and names the version it needs', Pos('99', Fault) > 0);
end;

procedure TProjectDocumentTest.AVersionThisBuildCanReadIsAccepted;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  A FILE FROM A NEWER BUILD THAT ONLY ADDED THINGS still opens. That is
    //  the whole reason minReaderVersion is separate from formatVersion: an
    //  additive change does not bump what a reader must support, so it does not
    //  lock out builds that would read it perfectly well.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectManifestPart,
        Format('{"formatVersion":99,"minReaderVersion":%d}',
        [ProjectFormatVersion]));
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
end;

procedure TProjectDocumentTest.AMemberThisBuildDoesNotKnowIsWrittenBackOut;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault, Problem: string;
    Opened: boolean;
begin
    //  EXTENSION, NOT MERELY VERSIONING. A newer build adds a member to a
    //  section this build also writes. Opening the project here and saving it
    //  must not delete that member - otherwise a user who opens yesterday's
    //  build once loses work done in today's, with nothing to show for it.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectProblemPart,
        '{"unknownToThisBuild":{"deep":[1,2,3]}}');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('read: ' + Fault, Opened);

    Parts := ProjectToParts(Doc);
    AssertTrue('found', PartContent(Parts, ProjectProblemPart, Problem));
    AssertTrue('the member this build never heard of came back out',
        Pos('unknownToThisBuild', Problem) > 0);
    AssertTrue('and so did what was under it', Pos('deep', Problem) > 0);
end;

procedure TProjectDocumentTest.APartThisBuildDoesNotKnowIsWrittenBackOut;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault, Recipe: string;
    Opened: boolean;
begin
    //  The same rule one level up: a whole SECTION a later feature adds.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, 'future/recipe.json', '{"steps":[1,2]}');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('read: ' + Fault, Opened);

    Parts := ProjectToParts(Doc);
    AssertTrue('the part survived', PartContent(Parts, 'future/recipe.json',
        Recipe));
    AssertEquals('byte for byte', '{"steps":[1,2]}', Recipe);
end;

procedure TProjectDocumentTest.AMissingOptionalSectionIsEmptyRatherThanAFailure;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  A project saved from a session that never touched the chart has no UI
    //  section. That is an ordinary file, not a damaged one.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectUiPart, '');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
    AssertFalse('and it simply has no working context', Doc.HasUi);
end;

procedure TProjectDocumentTest.AnAbsentResultsSectionMeansNothingWasFitted;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  A project saved after placing picks but before pressing Fit. It must
    //  restore the model at its seeds, and must NOT claim a fit happened - the
    //  fitted flag decides whether a move is refused and whether a rebuild
    //  re-seeds.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectResultsPart, '');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
    AssertEquals('no curves carry values', 0, Length(Doc.Curves));
    AssertFalse('and no statistics are claimed', Doc.Statistics.Valid);
end;

procedure TProjectDocumentTest.AModulePartIsNamedAfterItsModule;
begin
    AssertEquals('modules/sample.json', ModulePartName('sample'));
    AssertEquals('and back again', 'sample',
        ModuleOfPartName('modules/sample.json'));
end;

procedure TProjectDocumentTest.APartThatIsNotAModulesNamesNoModule;
begin
    //  The framework's own parts are not a module's, and neither is anything a
    //  later feature adds at the top level. Read as one, a module would be
    //  posted a document it never wrote.
    AssertEquals('the manifest', '', ModuleOfPartName(ProjectManifestPart));
    AssertEquals('the inputs', '', ModuleOfPartName(ProjectProblemPart));
    AssertEquals('a future part', '', ModuleOfPartName('future/recipe.json'));
    AssertEquals('the folder itself', '', ModuleOfPartName('modules/'));
    AssertEquals('the right prefix and no extension', '',
        ModuleOfPartName('modules/sample'));
end;

procedure TProjectDocumentTest.AModuleDocumentSurvivesARoundTrip;
var
    Doc, Got: TProjectDocument;
begin
    Doc := AFullDocument;
    SetLength(Doc.ModuleDocuments, 1);
    Doc.ModuleDocuments[0].Module := 'sample';
    Doc.ModuleDocuments[0].Content := '{"marks":[1,2]}';
    Got := RoundTrip(Doc);
    AssertEquals('one module part', 1, Length(Got.ModuleDocuments));
    AssertEquals('', 'sample', Got.ModuleDocuments[0].Module);
    AssertEquals('unchanged', '{"marks":[1,2]}',
        Got.ModuleDocuments[0].Content);
end;

procedure TProjectDocumentTest.AModulePartForAModuleThisBuildLacksIsStillRead;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    //  READ GENERICALLY, not by asking which modules exist. A project may carry
    //  the state of a pack this build does not have, and dropping it on load
    //  would destroy it on the next save - the one case where a user loses work
    //  by opening their project in the wrong build.
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, 'modules/notinthisbuild.json', '{"kept":true}');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
    AssertEquals('it was read', 1, Length(Doc.ModuleDocuments));
    AssertEquals('', 'notinthisbuild', Doc.ModuleDocuments[0].Module);

    Parts := ProjectToParts(Doc);
    AssertTrue('and written back out',
        IndexOfPart(Parts, 'modules/notinthisbuild.json') >= 0);
end;

procedure TProjectDocumentTest.StampingWritesAModifiedTimeInUtc;
var
    Doc: TProjectDocument;
begin
    //  A form that sorts as text and states its zone. A project file may be
    //  read by anything, and a local time in an unstated zone is not a time.
    Doc := EmptyProjectDocument;
    StampProject(Doc, '', EncodeDate(2026, 9, 5) + EncodeTime(14, 3, 9, 0));
    AssertEquals('2026-09-05T14:03:09Z', Doc.ModifiedUtc);
end;

procedure TProjectDocumentTest.ARewriteKeepsTheTimeTheProjectCameIntoBeing;
var
    Doc: TProjectDocument;
begin
    //  "Created" means when this project came into being, not when it was last
    //  written - a re-save that reset it would make the field mean the same as
    //  the one beside it, and the first of the two is the one nobody can
    //  reconstruct afterwards.
    Doc := EmptyProjectDocument;
    StampProject(Doc, '2026-01-02T03:04:05Z',
        EncodeDate(2026, 9, 5) + EncodeTime(14, 3, 9, 0));
    AssertEquals('carried', '2026-01-02T03:04:05Z', Doc.CreatedUtc);
    AssertEquals('and the write is now', '2026-09-05T14:03:09Z',
        Doc.ModifiedUtc);
end;

procedure TProjectDocumentTest.AFirstWriteIsCreatedAndModifiedAtOnce;
var
    Doc: TProjectDocument;
begin
    //  Nothing to carry: this is where the project came into being.
    Doc := EmptyProjectDocument;
    StampProject(Doc, '', EncodeDate(2026, 9, 5) + EncodeTime(14, 3, 9, 0));
    AssertEquals(Doc.ModifiedUtc, Doc.CreatedUtc);
    AssertTrue('and it is not empty', Doc.CreatedUtc <> '');
end;

procedure TProjectDocumentTest.TheStampsSurviveARoundTrip;
var
    Doc, Got: TProjectDocument;
begin
    //  They are in the manifest, which is the part that says what this file is;
    //  written and not read back, they would be decoration.
    Doc := AFullDocument;
    StampProject(Doc, '2026-01-02T03:04:05Z',
        EncodeDate(2026, 9, 5) + EncodeTime(14, 3, 9, 0));
    Got := RoundTrip(Doc);
    AssertEquals('2026-01-02T03:04:05Z', Got.CreatedUtc);
    AssertEquals('2026-09-05T14:03:09Z', Got.ModifiedUtc);
end;

{ Reads ADoc back from a problem section written by hand. }
function ReadProblem(const AProblemJson: string;
    out ADoc: TProjectDocument): boolean;
var
    Parts: TProjectParts;
    Fault: string;
begin
    Parts := WithPart(nil, ProjectManifestPart,
        '{"formatVersion":1,"minReaderVersion":1}');
    Parts := WithPart(Parts, ProjectProblemPart, AProblemJson);
    Result := ProjectFromParts(Parts, ADoc, Fault);
end;

procedure TProjectDocumentTest.ASectionOfTheWrongShapeIsSkippedNotFatal;
var
    Doc: TProjectDocument;
begin
    //  A hand-edited or truncated file. Every one of these members is read
    //  only when it is the shape it should be, so what is wrong is left at its
    //  default and the rest of the project still opens - a project that fails
    //  entirely because one member is a number is a project nobody can rescue.
    AssertTrue('opened', ReadProblem(
        '{"settings":7,"selectedInterval":"no","userCurve":[],' +
        '"provenance":42,"profile":{"x":[1],"y":[2]}}', Doc));
    AssertEquals('the part that was readable was read', 1,
        Length(Doc.Profile.X));
    AssertEquals('and the rest is at its defaults', '',
        Doc.Settings.CurveTypeId);
    AssertFalse('no interval', Doc.SelectedIntervalInForce);
    AssertFalse('no user curve', Doc.HasUserCurve);
    AssertEquals('and nothing claimed about where the data came from', '',
        Doc.Provenance.SourcePath);
end;

procedure TProjectDocumentTest.ACurveThatIsNotAnObjectIsSkipped;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectResultsPart,
        '{"curves":[7,{"id":"a","fitted":true,"params":[]}]}');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
    AssertEquals('the real one is still read', 'a', Doc.Curves[1].Id);
end;

procedure TProjectDocumentTest.AParameterThatIsNotAnObjectIsSkipped;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
    Opened: boolean;
begin
    Parts := ProjectToParts(AFullDocument);
    Parts := WithPart(Parts, ProjectResultsPart,
        '{"curves":[{"id":"a","params":["nonsense",' +
        '{"name":"sigma","value":2.5}]}]}');
    Opened := ProjectFromParts(Parts, Doc, Fault);
    AssertTrue('opened: ' + Fault, Opened);
    AssertEquals('the readable parameter survives', 'sigma',
        Doc.Curves[0].Params[1].Name);
end;

procedure TProjectDocumentTest.APointSetThatIsNotAnObjectReadsAsEmpty;
var
    Doc: TProjectDocument;
begin
    //  Empty rather than refused: a set that cannot be read holds no points,
    //  which is a state the application already handles everywhere.
    AssertTrue('opened', ReadProblem('{"profile":5,"background":"x"}', Doc));
    AssertEquals('', 0, Length(Doc.Profile.X));
    AssertEquals('', 0, Length(Doc.Background.X));
end;

initialization
    //  A unit test: records, JSON and a TMemoryStream. Nothing outside the
    //  process.
    RegisterTest('unit', TProjectDocumentTest);
end.
