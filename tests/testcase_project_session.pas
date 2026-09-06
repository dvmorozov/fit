// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Capturing a live problem into a project document and applying one back
- the round trip the whole feature exists for.)

WHAT IS DRIVEN. A real TFitService, in process. Not a mock of IFitService: the
thing worth checking is that a captured document, applied to a FRESH engine,
produces the same problem - and a mock would only prove that the capture called
the getters and the apply called the setters, which is precisely the class of
green-suite-over-a-dead-path this codebase keeps producing. Nothing here crosses
a process boundary, touches a file or runs the optimiser to convergence, so by
this project's rule it is still a unit test.

THE INVARIANT THAT MATTERS MOST is the last one: a model whose values came from
a fit, captured and applied to a new engine, comes back with those values and
still marked as fitted. Everything else in this file supports that. If the
handles did not survive, the values would attach to nothing and the model would
sit at its seeds - which looks like a working restore until you notice the fit
has been thrown away.
}
unit testcase_project_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Variants, fpcunit, testregistry,
    int_fit_service, fit_service, points_set, title_points_set,
    gauss_points_set, persistent_curve_parameters,
    fit_project_archive, fit_project_json,
    fit_project_document, fit_project_session, fit_project_restore;

type
    TProjectSessionTest = class(TTestCase)
    private
        FSource: TFitService;
        FTarget: TFitService;
        { Builds a problem in AService: a peaked profile, one interval, picks. }
        procedure GivenAProblem(AService: TFitService);
        { The client-side half of a capture, with nothing interesting in it. }
        function NoContext: TProjectClientContext;
        { The same, plus a user-defined formula the client owns. }
        function WithAUserCurve: TProjectClientContext;
        { Captures FSource and applies it to FTarget. }
        function Transfer: TProjectDocument;
        { A document whose selected interval cannot be applied to its own
          profile - a file written against a longer one, or edited by hand. }
        function ADocumentWithAnImpossibleInterval: TProjectDocument;
        { The value of the named parameter of AService's first curve. }
        function FirstCurveParam(AService: TFitService;
            const AName: string): double;
        { The abscissae AService holds picks at, as text. }
        function PickedAt(AService: TFitService): string;
        { The content of a part the document carries, or ''. }
        function PartOf(const ADoc: TProjectDocument;
            const AName: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What a restore does when a step will not go through.
        procedure AFailedStepStopsTheRestoreAndSaysWhichOne;
        procedure AndNothingAfterThatStepIsApplied;

        //  What a capture takes.
        procedure TheDocumentHoldsEveryPickTheServiceHolds;
        procedure EveryPickIsCapturedWithItsOwnHandle;
        procedure TheProfileAndTheBackgroundAndTheBoundsAreCaptured;
        procedure TheSettingsAreCaptured;
        procedure EveryCurveIsCapturedUnderItsHandle;
        procedure TheClientsOwnContextIsCarriedThrough;

        //  What a capture must NOT take.
        procedure NoDerivedSetIsCaptured;

        //  What may go into the file at all
        procedure ANumberIsAQuantityAndIsStored;
        procedure ALabelIsNotAQuantityAndIsLeftOut;
        procedure AValueTheServiceCouldNotReportIsNotAQuantity;

        //  What an apply restores.
        procedure ApplyingToAFreshEngineRestoresThePicks;
        procedure ApplyingRestoresTheHandlesSoValuesCanAttach;
        procedure ApplyingRestoresTheSettings;
        procedure ApplyingAnEmptyDocumentIsHarmless;

        //  The one the feature exists for.
        procedure AFittedModelSurvivesTheRoundTripWithItsValues;
        procedure AndIsStillReportedAsFittedAfterwards;
        procedure AModelThatWasNeverFittedDoesNotClaimToHaveBeen;
        procedure APartTheBuildDidNotUnderstandSurvivesTheNextSave;
        procedure AProblemWithNoFitCapturesNoRFactor;
        procedure TheProjectRecordsWhenItWasWritten;
        procedure ACurveTypeThisBuildDoesNotHaveDoesNotStopTheRestore;

        //  THE USER-DEFINED CURVE. Its formula comes from the client, its
        //  parameters from the engine, and without both the engine refuses
        //  to build that type at all - so a project using it would restore
        //  an empty model.
        procedure TheUserCurveFormulaAndItsParametersAreCaptured;
        procedure ApplyingThemPutsTheFormulaBack;
        procedure AProjectWithNoUserCurveCapturesNone;
    end;

implementation

const
    HandleA = '{0A0A0A0A-1111-2222-3333-444444444444}';
    HandleB = '{0B0B0B0B-1111-2222-3333-444444444444}';

procedure TProjectSessionTest.SetUp;
begin
    FSource := TFitService.Create;
    FTarget := TFitService.Create;
end;

procedure TProjectSessionTest.TearDown;
begin
    FreeAndNil(FSource);
    FreeAndNil(FTarget);
end;

procedure TProjectSessionTest.GivenAProblem(AService: TFitService);
var
    P, B, Picks: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
    Svc: IFitService;
    i: longint;
begin
    //  Through the interface, which is what the client holds and what the
    //  session unit under test is given. Several of the settings are protected
    //  on the class and published only here, so this is also the only way to
    //  reach them - and it is the right one.
    Svc := AService;
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    AService.SetProfilePointsSet(P);
    //  By name: the curve-type selection is process-global, and these tests
    //  name a parameter.
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    Svc.SetWaveLength(1.54056);
    Svc.SetMaxRFactor(0.042);
    Svc.SetLossKind(1);

    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    AService.SetRFactorBounds(B);

    AService.AddPointToBackground(0, 10);

    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(6, 20);
    Picks.AddNewPoint(14, 20);
    SetLength(Ids, 2);
    Ids[0] := HandleA;
    Ids[1] := HandleB;
    AService.SetCurvePositions(Picks, Ids);
end;

function TProjectSessionTest.NoContext: TProjectClientContext;
begin
    Result := EmptyProjectClientContext;
end;

function TProjectSessionTest.Transfer: TProjectDocument;
var
    Fault: string;
    Applied: boolean;
begin
    Result := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    Applied := ApplyProject(FTarget, Result, Fault);
    AssertTrue('applied: ' + Fault, Applied);
end;

function TProjectSessionTest.FirstCurveParam(AService: TFitService;
    const AName: string): double;
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
        if Nm = AName then
            Exit(V);
    end;
end;

function TProjectSessionTest.PickedAt(AService: TFitService): string;
var
    i: longint;
    Picks: TTitlePointsSet;
begin
    Result := '';
    Picks := AService.GetCurvePositions;
    if Picks = nil then
        Exit;
    for i := 0 to Picks.PointsCount - 1 do
    begin
        if Result <> '' then
            Result := Result + ',';
        Result := Result + FloatToStr(Picks.PointXCoord[i]);
    end;
end;

function TProjectSessionTest.PartOf(const ADoc: TProjectDocument;
    const AName: string): string;
begin
    if not PartContent(ProjectToParts(ADoc), AName, Result) then
        Result := '';
end;

procedure TProjectSessionTest.TheDocumentHoldsEveryPickTheServiceHolds;
var
    Doc: TProjectDocument;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('both picks', 2, Length(Doc.Positions.X));
    AssertEquals('', 6.0, Doc.Positions.X[0], 1e-12);
    AssertEquals('', 14.0, Doc.Positions.X[1], 1e-12);
end;

procedure TProjectSessionTest.EveryPickIsCapturedWithItsOwnHandle;
var
    Doc: TProjectDocument;
begin
    //  Without this the picks restore and nothing can be attached to them, so
    //  the model comes back at its seeds - a restore that looks like it worked.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('one handle per pick', 2, Length(Doc.Positions.Ids));
    AssertTrue('the first pick names a curve', Doc.Positions.Ids[0] <> '');
    AssertTrue('and so does the second', Doc.Positions.Ids[1] <> '');
    AssertTrue('and they are different curves',
        Doc.Positions.Ids[0] <> Doc.Positions.Ids[1]);
end;

procedure TProjectSessionTest.TheProfileAndTheBackgroundAndTheBoundsAreCaptured;
var
    Doc: TProjectDocument;
begin
    //  THE PROFILE AS THE ENGINE HOLDS IT. Subtracting a background and
    //  smoothing rewrite it in place, so the source file's points would restore
    //  a different problem from the one that was saved.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('every sample', 21, Length(Doc.Profile.X));
    AssertEquals('the background pick', 1, Length(Doc.Background.X));
    AssertEquals('the interval, as a pair', 2, Length(Doc.Bounds.X));
end;

procedure TProjectSessionTest.TheSettingsAreCaptured;
var
    Doc: TProjectDocument;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('the curve type the user chose',
        GUIDToString(TGaussPointsSet.GetCurveTypeId), Doc.Settings.CurveTypeId);
    AssertEquals('', 1.54056, Doc.Settings.WaveLength, 1e-9);
    AssertEquals('', 0.042, Doc.Settings.MaxRFactor, 1e-9);
    AssertEquals('the objective', 1, Doc.Settings.LossKind);
end;

procedure TProjectSessionTest.EveryCurveIsCapturedUnderItsHandle;
var
    Doc: TProjectDocument;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('one per pick', 2, Length(Doc.Curves));
    AssertTrue('under a handle', Doc.Curves[0].Id <> '');
    AssertTrue('with its parameters', Length(Doc.Curves[0].Params) > 0);
    AssertTrue('named', Doc.Curves[0].Params[0].Name <> '');
end;

procedure TProjectSessionTest.TheClientsOwnContextIsCarriedThrough;
var
    Ctx: TProjectClientContext;
    Doc: TProjectDocument;
begin
    //  HANDED IN RATHER THAN READ. The argument axis, the picking mode, the
    //  tab in front and where the data came from are the CLIENT's, and the
    //  engine has never been told any of them. A capture that tried to read
    //  them from the service would be inventing them.
    Ctx := EmptyProjectClientContext;
    Ctx.HasUi := True;
    Ctx.Ui.ViewMode := 2;
    Ctx.Ui.ActiveTab := 1;
    Ctx.Provenance.SourcePath := 'Data/2.dat';
    Ctx.SelectedIntervalInForce := True;
    Ctx.SelectedIntervalFrom := 3;
    Ctx.SelectedIntervalTo := 17;

    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, Ctx, EmptyProjectDocument);
    AssertTrue('', Doc.HasUi);
    AssertEquals('', 2, Doc.Ui.ViewMode);
    AssertEquals('', 1, Doc.Ui.ActiveTab);
    AssertEquals('', 'Data/2.dat', Doc.Provenance.SourcePath);
    AssertTrue('', Doc.SelectedIntervalInForce);
    AssertEquals('as indices', 3, Doc.SelectedIntervalFrom);
    AssertEquals('', 17, Doc.SelectedIntervalTo);
end;

procedure TProjectSessionTest.ANumberIsAQuantityAndIsStored;
begin
    //  The ordinary case: an amplitude, a position, a width.
    AssertTrue('a float', ValueIsAQuantity(1.75));
    AssertTrue('an integer', ValueIsAQuantity(3));
    AssertTrue('and zero, which is a perfectly good answer',
        ValueIsAQuantity(0.0));
end;

procedure TProjectSessionTest.ALabelIsNotAQuantityAndIsLeftOut;
begin
    //  A PATTERN'S IDENTITY. A model may hold parameters that are not
    //  quantities - a wave's label, the handle of its parent - and the file
    //  carries values as doubles, so these arrived as 0. Storing that was
    //  harmless; writing it back over a rebuilt instance was not.
    AssertFalse('a label', ValueIsAQuantity('wave 3'));
    AssertFalse('a handle', ValueIsAQuantity(
        '{D40376F8-0498-4D7F-9906-6AE1D6656ED0}'));
    //  NOT EVEN ONE THAT LOOKS LIKE A NUMBER. It is text in the model, and a
    //  restore has to hand back what the model holds.
    AssertFalse('a numeral that is text', ValueIsAQuantity('12'));
end;

procedure TProjectSessionTest.AValueTheServiceCouldNotReportIsNotAQuantity;
begin
    //  Null is what the HTTP client answers for a parameter it could not read,
    //  and it must not be written back as a zero either.
    AssertFalse('null', ValueIsAQuantity(Null));
end;

procedure TProjectSessionTest.NoDerivedSetIsCaptured;
var
    Doc: TProjectDocument;
begin
    //  The calculated profile, the delta and the fitted positions are all
    //  rebuilt from what IS captured. Taking them would make a second source of
    //  truth that can disagree with the model - and a stale one is invisible.
    //  Asserted through the document's own shape: it has nowhere to put them.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertEquals('the picks are the user''s, not the model''s answer', 2,
        Length(Doc.Positions.X));
    AssertEquals('and they are where the user put them', 6.0,
        Doc.Positions.X[0], 1e-12);
end;

procedure TProjectSessionTest.ApplyingToAFreshEngineRestoresThePicks;
begin
    GivenAProblem(FSource);
    Transfer;
    AssertEquals('the picks came across', '6,14', PickedAt(FTarget));
    AssertEquals('and the model was rebuilt from them', 2,
        FTarget.GetCurveCount);
end;

procedure TProjectSessionTest.ApplyingRestoresTheHandlesSoValuesCanAttach;
var
    Doc: TProjectDocument;
begin
    //  THE JOINT THE WHOLE FEATURE TURNS ON. The handles a capture recorded
    //  must be the handles the restored model carries, or the values in the
    //  document name curves that do not exist.
    GivenAProblem(FSource);
    Doc := Transfer;
    AssertTrue('the first handle survived',
        FTarget.IndexOfCurveInstance(Doc.Positions.Ids[0]) >= 0);
    AssertTrue('and the second',
        FTarget.IndexOfCurveInstance(Doc.Positions.Ids[1]) >= 0);
end;

procedure TProjectSessionTest.ApplyingRestoresTheSettings;
var
    Svc: IFitService;
begin
    GivenAProblem(FSource);
    Transfer;
    Svc := FTarget;
    AssertEquals('the curve type', GUIDToString(TGaussPointsSet.GetCurveTypeId),
        GUIDToString(Svc.GetCurveType));
    AssertEquals('', 1.54056, Svc.GetWaveLength, 1e-9);
    AssertEquals('', 0.042, Svc.GetMaxRFactor, 1e-9);
    AssertEquals('the objective', 1, Svc.GetLossKind);
end;

procedure TProjectSessionTest.ApplyingAnEmptyDocumentIsHarmless;
var
    Fault: string;
    Applied: boolean;
begin
    //  A NEW, EMPTY PROJECT. Nothing to restore but the settings, and nothing
    //  about that is an error - the plan emits no step for a section that is
    //  not there.
    Applied := ApplyProject(FTarget, EmptyProjectDocument, Fault);
    AssertTrue('applied: ' + Fault, Applied);
    AssertEquals('and nothing was built', 0, FTarget.GetCurveCount);
end;

procedure TProjectSessionTest.AFittedModelSurvivesTheRoundTripWithItsValues;
var
    Values: TCurveValuesList;
    Sigma: double;
begin
    //  THE ONE THE FEATURE EXISTS FOR, short of an actual optimiser run: values
    //  that could only have come from a fit - the synthetic peak's sigma is
    //  2.5, and nothing seeds 0.37 - go into the source engine, through a
    //  document, and come out of a FRESH engine unchanged.
    GivenAProblem(FSource);
    SetLength(Values, 1);
    Values[0].CurveIndex := 0;
    Values[0].Fitted := True;
    SetLength(Values[0].Params, 1);
    Values[0].Params[0].Name := 'sigma';
    Values[0].Params[0].Value := 0.37;
    Values[0].Params[0].Error := 0.004;
    FSource.SetCurveValues(Values);

    Transfer;
    Sigma := FirstCurveParam(FTarget, 'sigma');
    AssertEquals('the width the fit found, in an engine that never ran one',
        0.37, Sigma, 1e-9);
end;

procedure TProjectSessionTest.AndIsStillReportedAsFittedAfterwards;
var
    Values: TCurveValuesList;
begin
    //  THE FLAG TRAVELS TOO, and it has to: it is what stops the next rebuild
    //  re-seeding, what allows a fitted pick to be moved, and what makes a lost
    //  instance an orphan rather than a new curve. Values without it restore a
    //  model that looks fitted and behaves as though it never was.
    GivenAProblem(FSource);
    SetLength(Values, 1);
    Values[0].CurveIndex := 0;
    Values[0].Fitted := True;
    SetLength(Values[0].Params, 1);
    Values[0].Params[0].Name := 'sigma';
    Values[0].Params[0].Value := 0.37;
    FSource.SetCurveValues(Values);
    AssertTrue('the source says so', FSource.AnyCurveIsFitted);

    Transfer;
    AssertTrue('and so does the engine it was restored into',
        FTarget.AnyCurveIsFitted);
end;

procedure TProjectSessionTest.AModelThatWasNeverFittedDoesNotClaimToHaveBeen;
begin
    //  The counterpart, and the reason the flag is captured rather than assumed
    //  from the presence of values: every instance has values from the moment
    //  it is placed. A project saved before pressing Fit must restore a model
    //  that can still be edited freely.
    GivenAProblem(FSource);
    AssertFalse('nothing has been fitted', FSource.AnyCurveIsFitted);
    Transfer;
    AssertFalse('and the restored model does not pretend otherwise',
        FTarget.AnyCurveIsFitted);
end;

function TProjectSessionTest.WithAUserCurve: TProjectClientContext;
begin
    //  THE FORMULA IS THE CLIENT'S. The server will not report the expression
    //  it is fitting - findings.md records that as a defect of its own - so the
    //  side that has it hands it over.
    Result := EmptyProjectClientContext;
    Result.HasUserCurve := True;
    Result.UserCurveExpression := 'A*exp(-x/tau)';
end;

procedure TProjectSessionTest.TheUserCurveFormulaAndItsParametersAreCaptured;
var
    Doc: TProjectDocument;
    CP: Curve_parameters;
    Svc: IFitService;
begin
    GivenAProblem(FSource);
    Svc := FSource;
    CP := Curve_parameters.Create(nil);
    Svc.SetSpecialCurveParameters('A*exp(-x/tau)', CP);

    Doc := CaptureProject(FSource, WithAUserCurve, EmptyProjectDocument);
    AssertTrue('there is one', Doc.HasUserCurve);
    AssertEquals('the formula', 'A*exp(-x/tau)', Doc.UserCurveExpression);
    AssertTrue('and its parameters came from the engine',
        Length(Doc.UserCurveParams) > 0);
end;

procedure TProjectSessionTest.ApplyingThemPutsTheFormulaBack;
var
    Doc: TProjectDocument;
    CP: Curve_parameters;
    Svc: IFitService;
    Fault: string;
    Applied: boolean;
begin
    //  Without the formula the engine refuses to build the user-defined type,
    //  so a restore would raise at the first rebuild and leave nothing.
    GivenAProblem(FSource);
    Svc := FSource;
    CP := Curve_parameters.Create(nil);
    Svc.SetSpecialCurveParameters('A*exp(-x/tau)', CP);
    Doc := CaptureProject(FSource, WithAUserCurve, EmptyProjectDocument);

    Applied := ApplyProject(FTarget, Doc, Fault);
    AssertTrue('applied: ' + Fault, Applied);
    CP := (FTarget as IFitService).GetSpecialCurveParameters;
    try
        AssertTrue('the parameters are there', Assigned(CP));
    finally
        CP.Free;
    end;
end;

procedure TProjectSessionTest.AProjectWithNoUserCurveCapturesNone;
var
    Doc: TProjectDocument;
begin
    //  Capturing one for a problem whose curve type is a built-in would leave
    //  the restored engine holding a user curve nobody asked for - and
    //  deleting one is its own operation with its own meaning.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertFalse('none', Doc.HasUserCurve);
    AssertEquals('and no parameters for one', 0, Length(Doc.UserCurveParams));
end;

procedure TProjectSessionTest.APartTheBuildDidNotUnderstandSurvivesTheNextSave;
var
    Previous, Doc: TProjectDocument;
begin
    //  THE FORMAT'S ONE REAL PROMISE, and it was broken end to end while every
    //  test of the mechanism passed. fit_project_json preserves an unknown part
    //  correctly - four tests say so - but a capture that started from an empty
    //  document handed it nothing to preserve, so opening a project written by
    //  a newer build and saving it deleted whatever that build had added.
    //
    //  Nobody would have noticed: the file still opens, and what is missing is
    //  a section this build has no way to display.
    Previous := EmptyProjectDocument;
    Previous.AsRead := WithPart(nil, 'future/recipe.json', '{"steps":[1,2]}');

    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, Previous);
    AssertEquals('carried through untouched', '{"steps":[1,2]}',
        PartOf(Doc, 'future/recipe.json'));
end;

procedure TProjectSessionTest.AProblemWithNoFitCapturesNoRFactor;
var
    Doc: TProjectDocument;
begin
    //  NEGATIVE, not zero, and not whatever text the engine happened to answer.
    //  Zero is a perfect fit; "no fit has run" must not read as one, and the
    //  notice that compares a project's R-factor with the restored model's is
    //  silent on exactly this value - so getting it wrong here would make that
    //  notice fire on every project saved before pressing Fit.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertTrue('no fit has run: ' + FloatToStr(Doc.RFactor), Doc.RFactor < 0);
end;

procedure TProjectSessionTest.TheProjectRecordsWhenItWasWritten;
var
    Doc: TProjectDocument;
begin
    //  Both stamps are filled in by the capture. They were in the manifest and
    //  written empty by every save, which is how a field becomes decoration.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    AssertTrue('written', Doc.ModifiedUtc <> '');
    AssertEquals('and a first write is created at the same moment',
        Doc.ModifiedUtc, Doc.CreatedUtc);
end;

procedure TProjectSessionTest.ACurveTypeThisBuildDoesNotHaveDoesNotStopTheRestore;
var
    Doc: TProjectDocument;
    Fault: string;
    Applied: boolean;
    Svc: IFitService;
begin
    //  A project naming a curve type that came with an analysis pack this
    //  build does not carry. The engine refuses an unregistered type -
    //  correctly, it could not build a model from it - so before this the whole
    //  restore failed and the project was unopenable.
    //
    //  Everything else in it is still exactly what it was: the profile, the
    //  picks, every other setting. Losing all of that to one absent curve type
    //  is a far worse answer than opening on the type the engine already has.
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, NoContext, EmptyProjectDocument);
    Doc.Settings.CurveTypeId := '{D1D1D1D1-9999-9999-9999-999999999999}';

    Applied := ApplyProject(FTarget, Doc, Fault);
    AssertTrue('the project still opens: ' + Fault, Applied);
    AssertEquals('with the picks it carried', '6,14', PickedAt(FTarget));
    Svc := FTarget;
    AssertTrue('and a curve type the engine actually has',
        not IsEqualGUID(Svc.GetCurveType,
            StringToGUID('{D1D1D1D1-9999-9999-9999-999999999999}')));
end;

{ ---- a step that will not go through --------------------------------------- }

function TProjectSessionTest.ADocumentWithAnImpossibleInterval: TProjectDocument;
begin
    //  A SELECTED INTERVAL THAT IS NOT INSIDE THE PROFILE, which is what a file
    //  edited by hand, truncated, or written against a longer profile looks
    //  like. The engine refuses it - rightly - and the question is what the
    //  restore does with that refusal.
    Result := EmptyProjectDocument;
    SetLength(Result.Profile.X, 3);
    SetLength(Result.Profile.Y, 3);
    Result.Profile.X[0] := 0; Result.Profile.Y[0] := 1;
    Result.Profile.X[1] := 1; Result.Profile.Y[1] := 2;
    Result.Profile.X[2] := 2; Result.Profile.Y[2] := 3;
    Result.SelectedIntervalInForce := True;
    Result.SelectedIntervalFrom := 5;
    Result.SelectedIntervalTo := 99;
    //  Something AFTER the failing step, so "it stopped" can be told from "it
    //  carried on and happened to change nothing".
    SetLength(Result.Bounds.X, 2);
    SetLength(Result.Bounds.Y, 2);
    Result.Bounds.X[0] := 0; Result.Bounds.Y[0] := 0;
    Result.Bounds.X[1] := 2; Result.Bounds.Y[1] := 0;
end;

procedure TProjectSessionTest.AFailedStepStopsTheRestoreAndSaysWhichOne;
var
    Fault: string;
begin
    //  NOT AN EXCEPTION, and not a half-restored model: the user gets a
    //  sentence saying which part of their project did not come back. Carrying
    //  on would build the rest on top of a step that did not happen, and the
    //  result would be a project that opened and was quietly wrong.
    AssertFalse('the restore fails',
        ApplyProject(FTarget, ADocumentWithAnImpossibleInterval, Fault));
    AssertTrue('and names the step: ' + Fault,
        Pos(RestoreStepName(rsSelectInterval), Fault) > 0);
end;

procedure TProjectSessionTest.AndNothingAfterThatStepIsApplied;
var
    Fault: string;
    Svc: IFitService;
    Bounds: TTitlePointsSet;
begin
    //  The bounds come AFTER the interval in the restore order, so a restore
    //  that carried on past the failure would have pushed them.
    ApplyProject(FTarget, ADocumentWithAnImpossibleInterval, Fault);
    Svc := FTarget;
    Bounds := Svc.GetRFactorBounds;
    try
        AssertEquals('nothing was pushed after the step that failed', 0,
            Bounds.PointsCount);
    finally
        Bounds.Free;
    end;
end;

initialization
    //  A unit test by this project's rule: two ordinary objects, no process
    //  boundary, no file, and no run to convergence.
    RegisterTest('unit', TProjectSessionTest);
end.
