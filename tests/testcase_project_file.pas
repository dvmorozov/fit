// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A project written to a real file, opened again, and carried on with -
which is the whole feature, end to end.)

AN INTEGRATION TEST, by this project's own rule: it writes and reads files. That
is the only external dependency it takes - the engine is in process and no
optimiser is run to convergence - and it is the dependency that matters here,
because every layer below this one is covered over streams and records and the
one thing none of them can answer is whether the bytes survive a disk.

WHAT IT IS REALLY FOR. The headline use case: save a partly-converged fit, come
back, reopen it, and continue from where it stopped rather than from the seeds.
Everything else in the project-file work exists to make that sentence true, and
this is the test that says it is. If the handles did not survive the file, the
values would attach to nothing and the model would come back at its starting
guess - which looks like a working restore until someone notices the fit is gone.
}
unit testcase_project_file;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, points_set, title_points_set,
    gauss_points_set, fit_project_archive, fit_project_document,
    fit_project_session, fit_project_file;

type
    TProjectFileTest = class(TTestCase)
    private
        FService: TFitService;
        FPath: string;
        procedure GivenAFittedProblem(AService: TFitService);
        function SigmaOfFirstCurve(AService: TFitService): double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AProjectWrittenToDiskReopensIdentical;
        procedure AFittedModelReopensWithTheValuesTheFitFound;
        procedure AndTheReopenedModelIsStillMarkedAsFitted;
        procedure ContinuingAfterAReopenStartsFromTheSavedValues;
        procedure AFileThatIsNotAProjectIsReportedRatherThanCrashing;
        procedure AProjectThatIsNotThereIsReportedRatherThanCrashing;
        procedure SavingOverAnExistingProjectReplacesIt;
        procedure ASectionThisBuildCannotReadSurvivesBeingSaved;
        procedure ARestoredModelHasNotRecomputedItsRFactor;
        procedure AReopenedProjectDrawsTheSameCalculatedProfile;
        procedure AMovedPickInAReopenedProjectReseedsRatherThanOrphaning;
    end;

implementation

const
    HandleA = '{0A0A0A0A-1111-2222-3333-444444444444}';

procedure TProjectFileTest.SetUp;
begin
    FService := TFitService.Create;
    FPath := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-test-' + FormatDateTime('hhnnsszzz', Now) + '.fitproj';
end;

procedure TProjectFileTest.TearDown;
begin
    FreeAndNil(FService);
    if (FPath <> '') and FileExists(FPath) then
        DeleteFile(FPath);
end;

procedure TProjectFileTest.GivenAFittedProblem(AService: TFitService);
var
    P, B, Picks: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
    Values: TCurveValuesList;
    Svc: IFitService;
    i: longint;
begin
    Svc := AService;
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    AService.SetProfilePointsSet(P);
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    Svc.SetWaveLength(1.54056);

    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    AService.SetRFactorBounds(B);

    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(10, 110);
    SetLength(Ids, 1);
    Ids[0] := HandleA;
    AService.SetCurvePositions(Picks, Ids);

    //  A width nothing would arrive at by itself - the synthetic peak's sigma is
    //  2.5 - so a curve carrying 0.37 afterwards can only have got it from here.
    SetLength(Values, 1);
    Values[0].CurveIndex := 0;
    Values[0].Fitted := True;
    SetLength(Values[0].Params, 1);
    Values[0].Params[0].Name := 'sigma';
    Values[0].Params[0].Value := 0.37;
    Values[0].Params[0].Error := 0.004;
    Svc.SetCurveValues(Values);
end;

function TProjectFileTest.SigmaOfFirstCurve(AService: TFitService): double;
var
    j: longint;
    Nm: string;
    V: double;
    T: longint;
begin
    Result := 0;
    for j := 0 to AService.GetCurveParameterCount(0) - 1 do
    begin
        AService.GetCurveParameter(0, j, Nm, V, T);
        if Nm = 'sigma' then
            Exit(V);
    end;
end;

procedure TProjectFileTest.AProjectWrittenToDiskReopensIdentical;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
begin
    GivenAFittedProblem(FService);
    AssertTrue('written: ' + Fault,
        SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault));
    AssertTrue('the file is there', FileExists(FPath));

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        AssertEquals('the profile came back', 21,
            Reopened.GetProfilePointsSet.PointsCount);
        AssertEquals('the pick came back', 1,
            Reopened.GetCurvePositions.PointsCount);
        AssertEquals('and the model was rebuilt from it', 1,
            Reopened.GetCurveCount);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.AFittedModelReopensWithTheValuesTheFitFound;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
begin
    //  THE SENTENCE THE WHOLE FEATURE HAS TO MAKE TRUE.
    GivenAFittedProblem(FService);
    SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault);

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        AssertEquals('the width the fit found, out of a file, in an engine ' +
            'that has never run one', 0.37, SigmaOfFirstCurve(Reopened), 1e-9);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.AndTheReopenedModelIsStillMarkedAsFitted;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
begin
    //  The flag travels too. Without it the next rebuild re-seeds, a fitted
    //  pick cannot be moved, and an instance the model loses is reported as new
    //  rather than as an orphan.
    GivenAFittedProblem(FService);
    SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault);

    Reopened := TFitService.Create;
    try
        OpenProjectFile(Reopened, FPath, Doc, Fault);
        AssertTrue('an optimiser had been here', Reopened.AnyCurveIsFitted);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.ContinuingAfterAReopenStartsFromTheSavedValues;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
    Values: TCurveValuesList;
    Svc: IFitService;
begin
    //  THE INCREMENTAL CASE. Reopen, edit the model further, and what was
    //  restored is still the starting point - not the seeds. Written as a
    //  second value write rather than a fit, because running the optimiser to
    //  convergence is a different kind of test; what is checked here is that
    //  the model being continued FROM is the restored one.
    GivenAFittedProblem(FService);
    SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault);

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        Svc := Reopened;

        //  One more round of work on the restored model, touching a DIFFERENT
        //  parameter.
        SetLength(Values, 1);
        Values[0].CurveIndex := 0;
        Values[0].Fitted := True;
        SetLength(Values[0].Params, 1);
        Values[0].Params[0].Name := 'A';
        Values[0].Params[0].Value := 123.5;
        Svc.SetCurveValues(Values);

        AssertEquals('the restored width is untouched by the new round', 0.37,
            SigmaOfFirstCurve(Reopened), 1e-9);
        AssertTrue('and the model is still the one that was restored',
            Reopened.IndexOfCurveInstance(Doc.Positions.Ids[0]) >= 0);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.AFileThatIsNotAProjectIsReportedRatherThanCrashing;
var
    Fault: string;
    Doc: TProjectDocument;
    F: TFileStream;
    Junk: string;
begin
    //  The user picked a .dat by mistake. An ordinary mistake, and it must not
    //  disturb the problem they already had open.
    Junk := 'this is a data file, not a project';
    F := TFileStream.Create(FPath, fmCreate);
    try
        F.Write(Junk[1], Length(Junk));
    finally
        F.Free;
    end;
    AssertFalse('refused', OpenProjectFile(FService, FPath, Doc, Fault));
    AssertTrue('and says why', Fault <> '');
end;

procedure TProjectFileTest.AProjectThatIsNotThereIsReportedRatherThanCrashing;
var
    Fault: string;
    Doc: TProjectDocument;
begin
    AssertFalse('refused',
        OpenProjectFile(FService, FPath + '.nowhere', Doc, Fault));
    AssertTrue('and names it', Pos('nowhere', Fault) > 0);
end;

procedure TProjectFileTest.SavingOverAnExistingProjectReplacesIt;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
begin
    //  A second save must leave one project in the file, not two documents'
    //  worth of parts. The container is rewritten whole rather than added to.
    GivenAFittedProblem(FService);
    AssertTrue('first save',
        SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault));
    AssertTrue('second save',
        SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument, FPath, Fault));

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        AssertEquals('one pick, not two', 1,
            Reopened.GetCurvePositions.PointsCount);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.ASectionThisBuildCannotReadSurvivesBeingSaved;
var
    Fault: string;
    Doc, Reread: TProjectDocument;
    Parts: TProjectParts;
    Content: string;
    S: TFileStream;
    Reopened: TFitService;
begin
    //  THE PROMISE THE WHOLE PART-BASED DESIGN EXISTS FOR, through real files:
    //  a section written by a build that knows more than this one is still
    //  there after this one has opened the project and saved it.
    //
    //  Every layer's own test of this passed while it was broken end to end,
    //  because the capture started from an empty document and so had nothing
    //  to preserve. Nobody would have noticed: the file still opens, and what
    //  goes missing is a section this build cannot display anyway.
    GivenAFittedProblem(FService);
    SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument,
        FPath, Fault);

    //  A newer build's section, put into the file as that build would have.
    S := TFileStream.Create(FPath, fmOpenRead or fmShareDenyNone);
    try
        ReadProjectArchive(S, Parts);
    finally
        S.Free;
    end;
    Parts := WithPart(Parts, 'future/recipe.json', '{"steps":[1,2]}');
    S := TFileStream.Create(FPath, fmCreate);
    try
        WriteProjectArchive(Parts, S);
    finally
        S.Free;
    end;

    //  This build opens it, and saves it back.
    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        AssertTrue('saved: ' + Fault,
            SaveProjectFile(Reopened, EmptyProjectClientContext, Doc, FPath,
                Fault));
    finally
        Reopened.Free;
    end;

    S := TFileStream.Create(FPath, fmOpenRead or fmShareDenyNone);
    try
        AssertTrue('still an archive', ReadProjectArchive(S, Parts));
    finally
        S.Free;
    end;
    AssertTrue('the newer build''s section is still there',
        PartContent(Parts, 'future/recipe.json', Content));
    AssertEquals('byte for byte', '{"steps":[1,2]}', Content);

    //  And this build's own sections still read.
    Reopened := TFitService.Create;
    try
        AssertTrue('and it is still a project this build can open',
            OpenProjectFile(Reopened, FPath, Reread, Fault));
        AssertEquals('with its model', 1, Reopened.GetCurveCount);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.ARestoredModelHasNotRecomputedItsRFactor;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
    Svc: IFitService;
    Reported: double;
begin
    //  CHARACTERISED, and it is why nothing compares the saved R-factor with a
    //  restored one. The obvious diagnostic - does the fit this project
    //  recorded still come out the same - was written, and could never fire:
    //  restoring does not recompute, and the engine answers "Not calculated"
    //  until something is fitted.
    //
    //  Pinned here so the next person to reach for that check learns it from a
    //  test rather than from shipping a safeguard that is not one. If restoring
    //  ever does recompute, this test fails and the check becomes possible.
    GivenAFittedProblem(FService);
    SaveProjectFile(FService, EmptyProjectClientContext, EmptyProjectDocument,
        FPath, Fault);

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        Svc := Reopened;
        AssertFalse('nothing to compare against: "' + Svc.GetRFactorStr + '"',
            TryStrToFloat(Trim(Svc.GetRFactorStr), Reported));
        //  AND NOR HAD THE MODEL THAT WAS SAVED. This fixture writes the
        //  values a fit would have found without running one, so neither side
        //  of the comparison exists - which is the ordinary case for a project
        //  whose values were restored rather than computed, and the second
        //  reason the check was not worth having.
        AssertTrue('no fit ran, so none was recorded either', Doc.RFactor < 0);
    finally
        Reopened.Free;
    end;
end;

procedure TProjectFileTest.AReopenedProjectDrawsTheSameCalculatedProfile;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
    Svc: IFitService;
    Before, After: TTitlePointsSet;
    i: longint;
    Biggest: double;
begin
    //  WHAT THE USER ACTUALLY SEES. Every other test here reads values back out
    //  of the report; the calculated profile is built from the CURVES, so it is
    //  the only thing that can tell a project that restored its numbers from
    //  one that restored a model those numbers are attached to. It is also what
    //  is drawn, so this is the assertion closest to "it looks the same".
    GivenAFittedProblem(FService);
    Svc := FService;
    Before := Svc.GetCalcProfilePointsSet;
    try
        AssertTrue('there is a calculated profile to compare',
            Before.PointsCount > 0);

        AssertTrue('saved', SaveProjectFile(FService, EmptyProjectClientContext,
            EmptyProjectDocument, FPath, Fault));

        Reopened := TFitService.Create;
        try
            AssertTrue('opened: ' + Fault,
                OpenProjectFile(Reopened, FPath, Doc, Fault));
            Svc := Reopened;
            After := Svc.GetCalcProfilePointsSet;
            try
                AssertEquals('the same samples', Before.PointsCount,
                    After.PointsCount);
                Biggest := 0;
                for i := 0 to After.PointsCount - 1 do
                    if Abs(After.PointYCoord[i] - Before.PointYCoord[i]) >
                        Biggest then
                        Biggest := Abs(After.PointYCoord[i] -
                            Before.PointYCoord[i]);
                AssertTrue('and the same curve drawn from them: worst sample ' +
                    FloatToStr(Biggest), Biggest < 1e-9);
            finally
                After.Free;
            end;
        finally
            Reopened.Free;
        end;
    finally
        Before.Free;
    end;
end;

procedure TProjectFileTest.AMovedPickInAReopenedProjectReseedsRatherThanOrphaning;
var
    Fault: string;
    Doc: TProjectDocument;
    Reopened: TFitService;
    Svc: IFitService;
    Moved: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
begin
    //  THE EDIT THE HANDLE EXISTS FOR. A pick may be MOVED after a project is
    //  reopened, and the instance must go with it rather than being orphaned:
    //  the handle is issued, not derived from the position, so the curve keeps
    //  the shape the fit found and is re-seeded where the pick now is.
    //
    //  Orphaning here would be silent - a model with the right number of curves
    //  and one of them sitting at a position nobody asked for.
    GivenAFittedProblem(FService);
    AssertTrue('saved', SaveProjectFile(FService, EmptyProjectClientContext,
        EmptyProjectDocument, FPath, Fault));

    Reopened := TFitService.Create;
    try
        AssertTrue('opened: ' + Fault,
            OpenProjectFile(Reopened, FPath, Doc, Fault));
        Svc := Reopened;
        AssertEquals('one curve came back', 1, Svc.GetCurveCount);

        //  The same handle at a new position, which is what the client sends
        //  when a marker is dragged.
        Moved := TTitlePointsSet.Create(nil);
        Moved.AddNewPoint(12, 110);
        SetLength(Ids, 1);
        Ids[0] := Doc.Positions.Ids[0];
        Reopened.SetCurvePositions(Moved, Ids);

        AssertEquals('still one curve, not a second one', 1,
            Svc.GetCurveCount);
        AssertTrue('and it is still the same instance',
            Svc.IndexOfCurveInstance(Doc.Positions.Ids[0]) >= 0);
        AssertEquals('sitting where the pick was moved to', 12.0,
            Svc.GetCurvePositions.PointXCoord[0], 1e-9);
    finally
        Reopened.Free;
    end;
end;

initialization
    //  AN INTEGRATION TEST: it writes and reads real files. Nothing else about
    //  it is external - the engine is in process and no fit runs to
    //  convergence - but the file system is an external dependency exactly as a
    //  socket is, and that is the criterion.
    RegisterTest('integration', TProjectFileTest);
end.
