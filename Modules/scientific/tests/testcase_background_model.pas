// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The background: taken out of the data by hand, and fitted as part of
the model.)

TWO WAYS TO DEAL WITH A BACKGROUND, and this unit is about keeping them apart.

  * MANUAL SUBTRACTION (Model > Background > Subtract) rewrites the profile. It
    is the user's own edit of their data, it has always worked this way, and it
    must go on working exactly this way. The first class below PINS it through
    the REST surface - written before anything else in the background work
    changed, so a later change that alters it fails here by name.

  * THE BACKGROUND AS A MODEL CURVE is fitted with the peaks and leaves the
    profile as measured. See docs/internal/decisions.md, "Background element".

Every test enters where the application enters: the REST router the compute
server runs, or TFitClient over the loopback transport.
}
unit testcase_background_model;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, Math, fpcunit, testregistry, fpjson,
    fit_rest_api, fit_points_json, points_set, title_points_set,
    background_search, testcase_rest_api,
    int_ui_host, fit_client, mock_ui_host, mock_fit_viewer,
    mock_loopback_fit_service, gauss_points_set, polynomial_background_points_set,
    exponential_background_points_set, power_law_background_points_set,
    pseudo_voigt_points_set, fit_task, curve_points_set, named_points_set,
    self_copied_component, dat_file_loader, curve_types_singleton,
    int_curve_type_iterator, int_curve_factory, int_fit_service, MyExceptions,
    fit_service, fit_project_document, fit_project_session, mscr_specimen_list,
    persistent_curve_parameters, formula_points_set, explanation,
    special_curve_parameter, user_curve_parameter, curve_builder_registry,
    fit_statistics, sample_coverage;

type
    { THE TASK, DRIVEN DIRECTLY. Built through the production constructor; the
      subclass only reaches the protected members the engine itself calls -
      the reduction strategies and the optimiser's parameter walk - so a test
      exercises the same code a fit does, without running one. }
    TProbeFitTask = class(TFitTask)
    public
        function ProbeSmallAmplitudeDeletion: boolean;
        function ProbeMaxDerivativeDeletion(var ADeleted: TCurvePointsSet): boolean;
        function ProbeMinimalAmplitudeDeletion(var ADeleted: TCurvePointsSet): boolean;
        procedure ProbePutBack(ADeleted: TCurvePointsSet);
        { Sets the first shared parameter, as the simplex does. }
        procedure ProbeSetFirstShared(AValue: double);
        function SharedCount: longint;
        function ProbeReductionRFactor: double;
        function ProbeRFactor: double;
        { How many parameters the optimiser would walk, decomposing or not. }
        function ProbeParametersWalked(ADecomposing: boolean): longint;
        { The same walk, reading each parameter's value and first step as the
          simplex does at every one. }
        function ProbeParametersRead: longint;
    end;

    { A CURVE TYPE PLACED FROM ITS OWN MARKUP, as a module's are - so the
      builder path of TFitTask.RecreateCurves, which a background has to
      survive, can be reached without a module in the build. COMPLETE, because
      it is registered for the whole test process and every registry-walking
      test meets it: an explanation with a limitation, a formula, no position. }
    TTestMarkupPointsSet = class(TFormulaPointsSet)
    protected
        function GetNativeExpression: string; override;
    public
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        class function PlacedByPointSet: string; override;
        class function Explanation: TExplanation; override;
    end;

    { A SERVICE WHOSE PROTECTED PLACEMENTS A TEST CALLS DIRECTLY. }
    TProbeFitService = class(TFitService)
    public
        { The three placements the engine asks the scientific module's peak
          search for, as the automatic run and Compute Automatically call
          them. }
        procedure ProbeAutomaticPositions;
        procedure ProbeComputePositions;
        procedure ProbeComputeBounds;
    end;

    { A RUN THE MODEL'S BUILDER REFUSES leaves the problem as it found it: the
      data loaded, the fit intervals where the user put them, and no "waiting
      for data". The builder is the test markup's, told to refuse the way a
      module's refuses a run with nothing marked. }
    TRefusedRunTest = class(TTestCase)
    protected
        procedure TearDown; override;
    published
        procedure ARefusedAutomaticRunLeavesTheProblemAsItWas;
    end;

    TBackgroundServiceTest = class(TTestCase)
    published
        procedure ThePeakSearchPlacesPositionsAndBoundsForAPeakType;
        { The automatic run's starting curves: one on every sample inside the
          fit intervals the clipping gives, as the original algorithm had. }
        procedure TheAutomaticRunStartsOneCurveOnEverySampleOfEveryInterval;
    end;

    { THE BACKGROUND IN ONE FIT INTERVAL: built after the peaks, kept across a
      rebuild, and passed by every rule written for peaks. Unit tests - a task,
      a synthetic profile, no optimiser. }
    TBackgroundTaskTest = class(TTestCase)
    private
        FTask: TProbeFitTask;
        { A task over a sloped peak with picks at the given x, peaks of
          APeakType and a background of ABackground (GUID_NULL for none). }
        procedure BuildTask(const APeakType, ABackground: TGuid;
            const APicks: array of double);
        function Curves: TSelfCopiedCompList;
        function LastCurve: TNamedPointsSet;
        function PeakCount: longint;
    protected
        procedure TearDown; override;
    published
        procedure NoBackgroundTypeMeansNoBackground;
        procedure RecreatingPutsTheBackgroundLast;
        procedure ARebuildKeepsTheBackgroundItFitted;
        procedure ABackgroundOfAnotherTypeIsReplaced;
        procedure ABackgroundWithNoPicksIsTheWholeModel;
        procedure TheBackgroundIsNotAPeaksSharedParameterSource;
        procedure SettingASharedParameterSkipsCurvesWithoutIt;
        procedure TheWalkVisitsTheSharedWidthOnceAfterEveryCurve;
        procedure SmallAmplitudeDeletionSkipsTheBackground;
        procedure DerivativeDeletionIsNotDisabledByTheBackground;
        procedure MinimalAmplitudeDeletionIsNotDisabledByTheBackground;
        procedure OnePeakAndABackgroundLeaveNothingToDelete;
        procedure ARestoredCurveGoesBeforeTheBackground;
        //  WHAT "GOOD ENOUGH" MEANS WHILE CURVES ARE REDUCED.
        procedure WithoutABackgroundTheReductionMeasuresAsBefore;
        procedure WithABackgroundItMeasuresThePeaksAboveIt;
        procedure WhileDecomposingTheBackgroundHoldsItsSeed;
        procedure AndTheFinalFitVariesItWithThePeaks;
        procedure ABackgroundAloneIsVariedEvenWhileDecomposing;
        //  A module's builder places the peaks; the background still follows.
        procedure ABuilderPathStillGetsTheBackground;
    end;

    { MANUAL SUBTRACTION, PINNED. Characterisation tests: they describe what the
      program does today, so they passed the first time they ran. Each was made
      to fail against a deliberately broken subtraction before being trusted -
      see the commit that introduced them. }
    TBackgroundSubtractionPinTest = class(TRestApiTestBase)
    private
        function ProblemWithSlopedPeak: longint;
        function ProfileOf(AId: longint): TPointsData;
    published
        procedure SubtractingAutomaticallyTakesOffTheChordThroughTheProposedPoints;
        procedure SubtractingBySelectedPointsUsesThePickedPoints;
        procedure SubtractingTwiceSubtractsAgain;
        procedure SubtractingLeavesNoBackgroundPointsBehind;
        procedure SubtractingCarriesEachPickOntoTheSubtractedProfile;
    end;

    { The same gesture through the client, as Model > Background > Subtract >
      By Selected Points performs it: the client's own picks are sent, then the
      verb, then the profile is read back. }
    { The client over the loopback transport. NOT registered: no tests. }
    TBackgroundClientBase = class(TTestCase)
    protected
        FApi: TFitRestApi;
        FSvc: TLoopbackFitService;
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FView: TMockFitViewer;
        FClient: TFitClient;
        procedure SetUp; override;
        procedure TearDown; override;
        { The sloped peak on the server, over [0, 20], with picks at 6 and 10. }
        procedure GivenAModelWithTwoPeaks;
    end;

    TBackgroundSubtractionClientTest = class(TBackgroundClientBase)
    published
        procedure SubtractingByTheClientsPicksRewritesTheServersProfile;
        procedure TheWindowsPicksFollowTheSubtractedProfile;
    end;

    { The background through the client, where the user's gestures enter. }
    TBackgroundClientTest = class(TBackgroundClientBase)
    published
        procedure DeletingTheBackgroundCurveRemovesTheElement;
        procedure AndThePicksAreLeftAlone;
        procedure ItStaysGoneAfterTheNextEdit;
        //  Model > Background > Curve, as the menu calls it.
        procedure ChoosingABackgroundThroughTheClientBuildsIt;
        procedure TheClientReportsTheBackgroundTheModelHas;
        procedure AndItIsRedrawn;
        //  THE TYPE OF EACH CURVE, as the client transport reads it back.
        procedure EachCurvesTypeIsReadOverTheWire;
        //  SAVED AND REOPENED THROUGH THE WIRE - the session a desktop runs.
        procedure TheBackgroundSurvivesSaveAndOpenOverTheWire;
        //  WHAT THE AUTOMATIC RUN SAYS reaches the window's status line - that
        //  it replaced the fit intervals the user set, for one.
        procedure TheAutomaticRunsNoteReachesTheClient;
        procedure TheStatusLineShowsTheNoteAndOtherwiseTheUsualHint;
        procedure ThenTheSamplesNoCurveCoversBeforeTheUsualHint;
        //  WHICH TYPE each curve's attributes say it is - what the Model panel
        //  reads - in the engine and over the wire.
        procedure TheAttributesSayWhichTypeEachCurveIs;
    end;

    { A client that runs the server call on the calling thread, so a test sees
      its completion without a message loop. }
    TSynchronousClient = class(TFitClient)
    protected
        procedure RunAsync(AOp: TServerOp; ADone: TThreadMethod); override;
    end;

    { The fixture the background's REST tests share. NOT registered: it
      declares no test. }
    TBackgroundRestBase = class(TRestApiTestBase)
    protected
        { A sloped peak, a Gaussian peak type, the interval [0, 20] - or the
          bounds given - and one pick at 10 unless ANoPick. }
        function BackgroundProblem(ANoPick: boolean = False): longint;
        function BackgroundProblemWithBounds(const ABounds: array of double): longint;
        { PUTs ABody to the settings; answers the status and the reply's
          message. }
        function PutSettings(AId: longint; const ABody: string;
            out AMessage: string): longint;
        function ChooseBackground(AId: longint; const ATypeId: TGuid): longint;
        { GET /curves, owned by the caller. }
        function Curves(AId: longint): TJSONObject;
        function CurveCount(AId: longint): longint;
        { How many curves of the model are of type ATypeId. }
        function CountOfType(AId: longint; const ATypeId: TGuid): longint;
        { The handle of the first curve of type ATypeId, lower-cased. }
        function HandleOfType(AId: longint; const ATypeId: TGuid): string;
        function SettingsOf(AId: longint): TJSONObject;
        { The points of the first curve of type ATypeId, as the chart gets them. }
        function PointsOfType(AId: longint; const ATypeId: TGuid): TPointsData;
        { The named parameter of the first curve of type ATypeId. }
        function ParamOfType(AId: longint; const ATypeId: TGuid;
            const AName: string): double;
        { Writes the named parameter of the first curve of type ATypeId as the
          Curve Attributes table does: PUT /curves/{handle}/params/{j}. }
        function PutParamOfType(AId: longint; const ATypeId: TGuid;
            const AName: string; AValue: double): longint;
        { PUTs AValue to sigma of the first curve, a Gaussian; the reply,
          owned by the caller. }
        function PutGaussSigma(AId: longint; AValue: double): TJSONObject;
        function Fit(AId: longint): longint;
        function RFactorOf(AId: longint): double;
    end;

    { THE BACKGROUND AS MODEL INPUT, over REST, without fitting anything: what
      the settings route accepts and refuses, and what the model then holds.
      A background is chosen with PUT /settings {"backgroundCurveType": ...} -
      one field beside curveType, not a verb of its own. }
    TBackgroundModelRestTest = class(TBackgroundRestBase)
    published
        procedure AddingABackgroundCurveBuildsOneBesideThePicks;
        procedure ThePicksAreLeftAsTheUserPutThem;
        procedure TheBackgroundIsLastInItsInterval;
        procedure TheSettingsSayWhichBackgroundTheModelHas;
        { A MODEL OF PEAKS ALWAYS HAS ONE BACKGROUND CURVE (2026-10-31, with
          the user): Chebyshev unless another shape is chosen, and none is not
          a choice - Model > Background > Curve offered None, and Fit >
          Automatically then quietly added a Quadratic. }
        procedure AModelOfPeaksHasAChebyshevBackgroundUnlessAnotherIsChosen;
        procedure NoBackgroundIsRefusedForAModelOfPeaks;
        procedure BackgroundAloneMakesTheModelDrawable;
        procedure OneBackgroundServesEveryFitInterval;
        procedure TheBackgroundKeepsItsHandleAcrossAnEdit;
        procedure ChangingTheBackgroundTypeGivesItANewHandle;
        procedure TheBackgroundIsSeededFromTheBaseline;
        procedure ANewBackgroundIsDrawnAtTheLevelItWasSeededAt;
        procedure OfferedBackgroundIdsAreAdopted;
        procedure AMalformedBackgroundIdIsRefused;
        procedure ABackgroundTypeCannotBeTheCurveType;
        procedure APeakTypeCannotBeTheBackground;
        procedure APowerLawBackgroundIsRefusedOnDataThatReachesZero;
        procedure AnUnknownBackgroundTypeIsRefused;
        procedure EveryCurveSaysWhatTypeItIs;
        procedure ANewProfileKeepsTheChosenBackground;
        //  The retired Enable Variation key is accepted and ignored.
        procedure ARetiredBackgroundVariationKeyIsIgnored;
        //  THE LIMITS ON A CURVE'S VALUES, as the server holds them.
        procedure ABackgroundCurveIsNeverDrawnBelowZero;
        procedure APeakIsNeverWiderThanItsFitInterval;
        //  What the reply to a parameter write says, for any REST client.
        procedure AWriteTheModelHoldsOtherwiseIsAnsweredWithTheValueAndWhy;
        procedure AWriteHeldAsWrittenIsAnsweredWithTheValueAlone;
    end;

    { THE BACKGROUND FITTED. Each of these runs the optimiser to convergence,
      and the real-data ones read Data/1.dat - integration by the project's own
      rule. }
    TBackgroundModelFitTest = class(TBackgroundRestBase)
    private
        { A profile of APeaks Gaussians on the background ACurve computes,
          over x = AFrom..ATo, as a wire point set. }
        function PeaksOnCurveJson(ACurve: TNamedPointsSet; AFrom, ATo: double;
            const APeakX, APeakA: array of double): string;
        { A problem over a window of Data/1.dat, with the picks given. }
        function OneDatProblem(AFrom, ATo: double;
            const APicks: array of double): longint;
        { A curved baseline under two peaks, with nothing placed. }
        function NewAutomaticProblem: longint;
    published
        procedure ABackgroundCurveIsFittedWithThePeaks;
        procedure TheProfileIsLeftAsMeasured;
        procedure AFittedBackgroundIsMarkedFittedAndKeptAcrossAnEdit;
        procedure EveryBackgroundShapeRecoversItsOwnCurve;
        { FOUND IN USE: on Tb2Fe17 data each fit interval fitted a line of its
          own, and the lines pointed every way - the model had lost the
          background as one smooth curve under the pattern. }
        procedure TheFittedBackgroundIsOneCurveUnderEveryInterval;
        procedure TheLowAngleTailOf1DatIsFittedByAFallingBackground;
        procedure TheHighAngleTailOf1DatIsFittedByAParabolaOnItsFloor;
        procedure APseudoVoigtModelWithABackgroundFits;
        procedure TheDrawnCurvesSumToTheModel;
        procedure ReducingCurvesNeverRemovesTheBackground;
        { FOUND IN USE (Tb2Fe17, 2026-10-31): the intervals ended wherever the
          noise first turned up, and the background started from a walk that
          stayed 25 counts under the data's floor. Both now come from the
          chosen background curve, clipped onto the floor. }
        procedure TheIntervalsFoundAreThePeaksAboveTheChosenBackground;
        procedure AndTheBackgroundStartsOnTheFloor;
        { And the picks: Model > Curve Positions > Auto cut at a fraction of
          the highest point over the raised floor, which every sample passed. }
        procedure ThePositionsFoundAreThePeaksAboveTheChosenBackground;
        { The picks follow the intervals: one on every sample inside them, a
          low broad hump beside a tall peak included, none outside. }
        procedure EveryIntervalSampleIsAPickAndNoneOutsideIsOne;
        { THE RUN LOG NAMES EVERY STAGE, the ones left out with why (asked by
          the user, 2026-10-31: the automatic run set the background aside
          for most of an hour and nothing said so). }
        procedure AFitsRunLogNamesItsStagesAndWhatItSkipped;
        procedure AnAutomaticRunsRunLogNamesItsStages;

        //  THE AUTOMATIC RUN fits the background instead of subtracting it.
        procedure TheAutomaticRunLeavesTheProfileAsMeasured;
        procedure TheAutomaticRunFitsTheModelsBackground;
        { THE BACKGROUND'S TERMS (2026-11-02, with the user): on Tb2Fe17 the
          six-term Chebyshev could not follow an amorphous hump twenty
          degrees wide, the hump became a fit interval with a starting curve
          on every sample, and the first fit of it had not ended in twelve
          hours. The run now starts the background at twelve terms and tunes
          the count by BIC after each turn. }
        procedure TheAutomaticRunFollowsAHumpSixTermsCannotFollow;
        procedure TheAutomaticRunDropsTheTermsAFlatFloorDoesNotNeed;
        { THE BACKGROUND IS FITTED TO THE FLOOR, NOT TO WHAT THE PEAK CURVES
          LEAVE (2026-11-02, with the user): fitted to the data minus the peaks,
          a hump the starting curves already sat on left it nothing to improve,
          and those curves were never reduced. }
        procedure TheCurvesOnAHumpAreNotKeptWhenTheBackgroundCanTakeIt;
        { ONE SEARCH FOR THE INTERVALS (2026-11-03, with the user): Fit Intervals >
          Auto by hand and the automatic run's own search clip the same
          background the same way, so they find the same intervals. }
        procedure FitIntervalsAutoFindsWhatTheAutomaticRunFinds;
        { FOUND IN USE: one interval over the whole series, Fit >
          Automatically, and the sample no curve covered was gone from the
          figure - the run had put its own intervals in place of the user's and
          said nothing. }
        procedure AnAutomaticRunSaysItReplacedTheIntervalsTheUserSet;
        procedure AndSaysNothingOfIntervalsWhenThereWereNone;
        procedure AnAutomaticRunKeepsTheBackgroundTheUserChose;
        procedure AnAutomaticRunAfterAManualSubtractionDoesNotSubtractAgain;
        //  NOT ON 1.dat's TAIL: the decomposition seeds a curve on every sample
        //  of every peak, and 2theta = 140..172 is finely sampled and full of
        //  small peaks, so one run there reduces ~50 curves at minutes per fit -
        //  the algorithm's own cost, unchanged by the background work. The tail
        //  is fitted with a background by the two tests above instead.
    end;

{ A peak on a sloped, curved baseline, as a wire point set. Shared with the
  model tests in this unit. }
function SlopedPeakJson: string;
{ What manual subtraction does to AData given the background points ABack:
  the piecewise chord through them taken off every sample between the first and
  the last, every background point left at zero, everything outside untouched.
  Written out here, independently of the engine, so the pin is a statement
  rather than a second call to the code it pins. }
function ChordSubtracted(const AData: TPointsData;
    const ABackX, ABackY: array of double): TPointsData;

implementation

uses
    SimpMath, chebyshev_background_points_set, jsonparser;

{ 0..100 in steps of 0.5: a floor of 50 with noise of about 7 under peaks of
  200 at 30 and 120 at 70 - as background_clipping's own tests have it. }
function PeaksOnANoisyFloorJson: string;
var
    P: TPointsData;
    i, N: longint;
    x: double;
    Seed: longword;
begin
    N := 201;
    P := Default(TPointsData);
    SetLength(P.X, N);
    SetLength(P.Y, N);
    for i := 0 to N - 1 do
    begin
        x := i * 0.5;
        Seed := longword(i) * 2654435761 + 12345;
        Seed := Seed xor (Seed shr 13);
        Seed := Seed * 1274126177;
        P.X[i] := x;
        P.Y[i] := 50 + 200 * Exp(-Sqr(x - 30) / 2) + 120 * Exp(-Sqr(x - 70) / 2) +
            ((Seed mod 10000) / 10000 - 0.5) * Sqrt(12) * Sqrt(50);
    end;
    Result := PointsToJsonString(P);
end;

{ 0..100 in steps of 0.5: a floor of 50 with noise of about 7 under a tall
  peak of 200 at 30 and a low hump of 25 - 3 to 4 noise units - at 45, which a
  search cutting at a fraction of the highest point does not take for a peak of
  its own. Its sigma is 2: a hump five degrees wide is too sharp for a
  twelve-term background to take for the floor, as a broad one (sigma 5, which
  this had until 2026-11-02) is - and rightly, that is what the terms are for
  (decisions.md, "The Chebyshev's terms are a value, tuned by BIC"). }
function PeakAndLowHumpJson: string;
var
    P: TPointsData;
    i, N: longint;
    x: double;
    Seed: longword;
begin
    N := 201;
    P := Default(TPointsData);
    SetLength(P.X, N);
    SetLength(P.Y, N);
    for i := 0 to N - 1 do
    begin
        x := i * 0.5;
        Seed := longword(i) * 2654435761 + 12345;
        Seed := Seed xor (Seed shr 13);
        Seed := Seed * 1274126177;
        P.X[i] := x;
        P.Y[i] := 50 + 200 * Exp(-Sqr(x - 30) / 2) +
            25 * Exp(-Sqr(x - 45) / 8) +
            ((Seed mod 10000) / 10000 - 0.5) * Sqrt(12) * Sqrt(50);
    end;
    Result := PointsToJsonString(P);
end;

{ Fails unless the picks are exactly the samples 0, 0.5 ... 100 lying inside
  the intervals ABounds (pairs of first and last x). }
procedure AssertPicksAreTheIntervalSamples(const ABounds, APicks: TPointsData);
var
    i, k, Inside: longint;
    x: double;
    InAnInterval, Picked: boolean;
begin
    TAssert.AssertTrue('there are intervals', Length(ABounds.X) >= 2);
    Inside := 0;
    for i := 0 to 200 do
    begin
        x := i * 0.5;
        InAnInterval := False;
        k := 0;
        while k + 1 <= High(ABounds.X) do
        begin
            if (x >= ABounds.X[k] - 1e-9) and (x <= ABounds.X[k + 1] + 1e-9) then
                InAnInterval := True;
            Inc(k, 2);
        end;
        Picked := False;
        for k := 0 to High(APicks.X) do
            if Abs(APicks.X[k] - x) < 1e-9 then
                Picked := True;
        TAssert.AssertEquals(Format('the sample at %g is picked exactly when ' +
            'it is inside an interval', [x]), InAnInterval, Picked);
        if InAnInterval then
            Inc(Inside);
    end;
    TAssert.AssertEquals('one pick per sample inside the intervals', Inside,
        Length(APicks.X));
end;

function SlopedPeakJson: string;
var
    P: TPointsData;
    x: double;
    n: longint;
begin
    P := Default(TPointsData);
    P.Title := 'profile';
    n := 0;
    x := 0;
    while x <= 20 + 1e-9 do
    begin
        SetLength(P.X, n + 1);
        SetLength(P.Y, n + 1);
        P.X[n] := x;
        P.Y[n] := 40 + 1.5 * x + 0.05 * Sqr(x - 8) + GaussPoint(100, 1.2, 10, x);
        Inc(n);
        x := x + 0.5;
    end;
    Result := PointsToJsonString(P);
end;

function ChordSubtracted(const AData: TPointsData;
    const ABackX, ABackY: array of double): TPointsData;
var
    i, k: longint;
    Chord: double;
begin
    Result := AData;
    Result.X := Copy(AData.X);
    Result.Y := Copy(AData.Y);
    for i := 0 to High(Result.X) do
        for k := 0 to High(ABackX) - 1 do
            if (Result.X[i] >= ABackX[k] - 1e-9) and
               (Result.X[i] <= ABackX[k + 1] + 1e-9) then
            begin
                Chord := ABackY[k] + (Result.X[i] - ABackX[k]) *
                    (ABackY[k + 1] - ABackY[k]) / (ABackX[k + 1] - ABackX[k]);
                Result.Y[i] := AData.Y[i] - Chord;
                Break;
            end;
end;

{ Sorted copies of a point set's coordinates, for ChordSubtracted. }
procedure SortedCoordinates(APoints: TPointsSet; out AX, AY: TDoubleDynArray);
var
    i: longint;
begin
    APoints.Sort;
    SetLength(AX, APoints.PointsCount);
    SetLength(AY, APoints.PointsCount);
    for i := 0 to APoints.PointsCount - 1 do
    begin
        AX[i] := APoints.PointXCoord[i];
        AY[i] := APoints.PointYCoord[i];
    end;
end;

function PointsSetOf(const AData: TPointsData): TPointsSet;
var
    i: longint;
begin
    Result := TPointsSet.Create(nil);
    for i := 0 to High(AData.X) do
        Result.AddNewPoint(AData.X[i], AData.Y[i]);
end;

procedure AssertSameProfile(const AMsg: string; const AExpected, AActual: TPointsData);
var
    i: longint;
begin
    TAssert.AssertEquals(AMsg + ': the same grid', Length(AExpected.X), Length(AActual.X));
    for i := 0 to High(AExpected.X) do
    begin
        TAssert.AssertEquals(AMsg + ': x of sample ' + IntToStr(i),
            AExpected.X[i], AActual.X[i], 1e-9);
        TAssert.AssertEquals(AMsg + ': y of sample ' + IntToStr(i),
            AExpected.Y[i], AActual.Y[i], 1e-6);
    end;
end;

{ What automatic subtraction would take off AData: the chord through the points
  background_search proposes for it. }
function AutomaticallySubtracted(const AData: TPointsData): TPointsData;
var
    Data, Back: TPointsSet;
    BX, BY: TDoubleDynArray;
begin
    Data := PointsSetOf(AData);
    try
        Back := ProposeBackgroundPoints(Data);
        try
            SortedCoordinates(Back, BX, BY);
        finally
            Back.Free;
        end;
    finally
        Data.Free;
    end;
    Result := ChordSubtracted(AData, BX, BY);
end;

{ ---- TBackgroundSubtractionPinTest ---- }

function TBackgroundSubtractionPinTest.ProblemWithSlopedPeak: longint;
var
    Code: longint;
begin
    Result := NewProblem;
    Call('PUT', Format('/problems/%d/profile', [Result]), SlopedPeakJson, Code).Free;
    AssertEquals('the profile is accepted', 200, Code);
end;

function TBackgroundSubtractionPinTest.ProfileOf(AId: longint): TPointsData;
var
    Code: longint;
    Body: string;
begin
    FApi.Handle('GET', Format('/problems/%d/profile', [AId]), '', Code, Body);
    AssertEquals('the profile is readable', 200, Code);
    AssertTrue('and decodes', PointsFromJsonString(Body, Result));
end;

procedure TBackgroundSubtractionPinTest.
    SubtractingAutomaticallyTakesOffTheChordThroughTheProposedPoints;
var
    Id, Code: longint;
    Before, Expected: TPointsData;
begin
    Id := ProblemWithSlopedPeak;
    Before := ProfileOf(Id);
    Expected := AutomaticallySubtracted(Before);

    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code).Free;
    AssertEquals('subtracted', 200, Code);

    AssertSameProfile('the profile less the chord', Expected, ProfileOf(Id));
end;

procedure TBackgroundSubtractionPinTest.SubtractingBySelectedPointsUsesThePickedPoints;
var
    Id, Code: longint;
    Before, Expected, Picks: TPointsData;
begin
    Id := ProblemWithSlopedPeak;
    Before := ProfileOf(Id);

    //  Two picks the user made, on samples of the profile: the ends.
    Picks := Default(TPointsData);
    Picks.X := [Before.X[0], Before.X[High(Before.X)]];
    Picks.Y := [Before.Y[0], Before.Y[High(Before.Y)]];
    Call('PUT', Format('/problems/%d/background', [Id]),
        PointsToJsonString(Picks), Code).Free;
    AssertEquals('the picks are accepted', 200, Code);

    Expected := ChordSubtracted(Before, Picks.X, Picks.Y);

    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":false}', Code).Free;
    AssertEquals('subtracted', 200, Code);

    AssertSameProfile('the profile less the line through the picks',
        Expected, ProfileOf(Id));
end;

procedure TBackgroundSubtractionPinTest.SubtractingTwiceSubtractsAgain;
var
    Id, Code: longint;
    Once, Expected: TPointsData;
begin
    //  THERE IS NO REASON TO REFUSE SUBTRACTING TWICE - the service says so in
    //  a comment, and it is the user's data to edit. The second subtraction is
    //  the same algorithm over the already-subtracted profile.
    Id := ProblemWithSlopedPeak;
    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code).Free;
    AssertEquals('first subtraction', 200, Code);
    Once := ProfileOf(Id);
    Expected := AutomaticallySubtracted(Once);

    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code).Free;
    AssertEquals('the second is accepted too', 200, Code);

    AssertSameProfile('subtracted again', Expected, ProfileOf(Id));
end;

procedure TBackgroundSubtractionPinTest.SubtractingLeavesNoBackgroundPointsBehind;
var
    Id, Code: longint;
    Body: string;
    Back: TPointsData;
begin
    Id := ProblemWithSlopedPeak;
    Call('POST', Format('/problems/%d/actions/compute-background-points', [Id]),
        '', Code).Free;
    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":false}', Code).Free;
    AssertEquals('subtracted', 200, Code);

    FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
    AssertTrue('decodes', PointsFromJsonString(Body, Back));
    AssertEquals('the points it used are gone', 0, Length(Back.X));
end;

procedure TBackgroundSubtractionPinTest.
    SubtractingCarriesEachPickOntoTheSubtractedProfile;
const
    First = '0a0a0a0a-1111-2222-3333-000000000001';
    Second = '0a0a0a0a-1111-2222-3333-000000000002';
var
    Id, Code: longint;
    Body: string;
    Before, After, Picks: TPointsData;
begin
    //  A PICK IS A SAMPLE OF THE PROFILE, and a new curve's amplitude is
    //  seeded from its height: left at the height it had over the background,
    //  it would seed every curve built after Subtract with the background
    //  still in it. Where it is and the handle it carries are the user's, and
    //  stay.
    Id := ProblemWithSlopedPeak;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Before := ProfileOf(Id);
    Picks := Default(TPointsData);
    Picks.X := [Before.X[10], Before.X[20]];
    Picks.Y := [Before.Y[10], Before.Y[20]];
    Picks.Ids := [First, Second];
    Call('PUT', Format('/problems/%d/positions', [Id]),
        PointsToJsonString(Picks), Code).Free;
    AssertEquals('the picks are accepted', 200, Code);

    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code).Free;
    AssertEquals('subtracted', 200, Code);
    After := ProfileOf(Id);

    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue('the picks decode', PointsFromJsonString(Body, Picks));
    AssertEquals('none added or lost', 2, Length(Picks.X));
    AssertEquals('each where it was picked', Before.X[10], Picks.X[0], 1e-9);
    AssertEquals(Before.X[20], Picks.X[1], 1e-9);
    AssertEquals('with its handle', 2, Length(Picks.Ids));
    AssertEquals(First, LowerCase(Picks.Ids[0]));
    AssertEquals(Second, LowerCase(Picks.Ids[1]));
    AssertTrue('the premise: the profile moved there',
        Abs(After.Y[10] - Before.Y[10]) > 1);
    AssertEquals('and on the profile as it now is', After.Y[10], Picks.Y[0], 1e-9);
    AssertEquals(After.Y[20], Picks.Y[1], 1e-9);
end;

{ ---- TBackgroundSubtractionClientTest ---- }

procedure TBackgroundClientBase.SetUp;
begin
    FApi := TFitRestApi.Create;
    FSvc := TLoopbackFitService.Create(FApi);
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    FView := TMockFitViewer.Create;
    //  THE APPLICATION'S CONSTRUCTOR, which is the one that makes the empty
    //  point sets a pick is added to - the bare inherited one leaves them nil.
    FClient := TFitClient.CreateWithInjector(nil);
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TBackgroundClientBase.TearDown;
begin
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FSvc);
    FreeAndNil(FApi);
end;

procedure TBackgroundSubtractionClientTest.
    SubtractingByTheClientsPicksRewritesTheServersProfile;
var
    Before, Expected, After: TPointsData;
    Profile, Got: TTitlePointsSet;
    i: longint;
begin
    AssertTrue('the profile decodes', PointsFromJsonString(SlopedPeakJson, Before));
    Profile := TTitlePointsSet.Create(nil);
    try
        for i := 0 to High(Before.X) do
            Profile.AddNewPoint(Before.X[i], Before.Y[i]);
        FSvc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;

    //  Two picks at the ends, made in the client the way the chart makes them.
    FClient.AddPointToBackground(Before.X[0], Before.Y[0]);
    FClient.AddPointToBackground(Before.X[High(Before.X)], Before.Y[High(Before.Y)]);
    Expected := ChordSubtracted(Before,
        [Before.X[0], Before.X[High(Before.X)]],
        [Before.Y[0], Before.Y[High(Before.Y)]]);

    FClient.SubtractBackground(False);

    Got := FSvc.GetProfilePointsSet;
    try
        After := Default(TPointsData);
        SetLength(After.X, Got.PointsCount);
        SetLength(After.Y, Got.PointsCount);
        for i := 0 to Got.PointsCount - 1 do
        begin
            After.X[i] := Got.PointXCoord[i];
            After.Y[i] := Got.PointYCoord[i];
        end;
    finally
        Got.Free;
    end;
    AssertSameProfile('the server holds the profile less the line', Expected, After);
end;

procedure TBackgroundSubtractionClientTest.TheWindowsPicksFollowTheSubtractedProfile;
var
    Before: TPointsData;
    Profile, Got: TTitlePointsSet;
    Picks: TPointsSet;
    i, Sample: longint;
begin
    //  WHAT THE CHART DRAWS: the client's own copy of the picks. Left as it
    //  was, every pick floated above the subtracted profile by the background
    //  taken off, until some later gesture read them again.
    AssertTrue('the profile decodes', PointsFromJsonString(SlopedPeakJson, Before));
    Profile := TTitlePointsSet.Create(nil);
    try
        for i := 0 to High(Before.X) do
            Profile.AddNewPoint(Before.X[i], Before.Y[i]);
        FSvc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;
    FClient.AddPointToCurvePositions(Before.X[10], Before.Y[10]);

    FClient.SubtractBackground(True);

    Picks := FClient.GetCurvePositions;
    AssertTrue('the window holds the pick', Assigned(Picks) and
        (Picks.PointsCount = 1));
    Got := FSvc.GetProfilePointsSet;
    try
        Sample := Got.IndexOfValueX(Before.X[10]);
        AssertTrue('the premise: the profile moved there',
            Abs(Got.PointYCoord[Sample] - Before.Y[10]) > 1);
        AssertEquals('on the profile as it now is', Got.PointYCoord[Sample],
            Picks.PointYCoord[0], 1e-9);
    finally
        Got.Free;
    end;
end;

{ ---- TBackgroundModelRestTest ---- }

function TBackgroundRestBase.BackgroundProblemWithBounds(
    const ABounds: array of double): longint;
var
    Code, i: longint;
    B: TPointsData;
begin
    Result := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    AssertEquals('the peak type is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/profile', [Result]), SlopedPeakJson, Code).Free;
    AssertEquals('the profile is accepted', 200, Code);
    B := Default(TPointsData);
    SetLength(B.X, Length(ABounds));
    SetLength(B.Y, Length(ABounds));
    for i := 0 to High(ABounds) do
    begin
        B.X[i] := ABounds[i];
        B.Y[i] := 0;
    end;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        PointsToJsonString(B), Code).Free;
    AssertEquals('the interval is accepted', 200, Code);
end;

function TBackgroundRestBase.BackgroundProblem(ANoPick: boolean): longint;
var
    Code: longint;
begin
    Result := BackgroundProblemWithBounds([0, 20]);
    if ANoPick then
        Exit;
    Call('PUT', Format('/problems/%d/positions', [Result]),
        '{"x":[10],"y":[140]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
end;

function TBackgroundRestBase.PutSettings(AId: longint; const ABody: string;
    out AMessage: string): longint;
var
    R: TJSONObject;
begin
    R := Call('PUT', Format('/problems/%d/settings', [AId]), ABody, Result);
    try
        AMessage := '';
        if Assigned(R) then
            AMessage := R.Get('error', R.Get('message', ''));
    finally
        R.Free;
    end;
end;

function TBackgroundRestBase.ChooseBackground(AId: longint;
    const ATypeId: TGuid): longint;
var
    Msg: string;
begin
    Result := PutSettings(AId, Format('{"backgroundCurveType":"%s"}',
        [GUIDToString(ATypeId)]), Msg);
end;

function TBackgroundRestBase.Curves(AId: longint): TJSONObject;
var
    Code: longint;
begin
    Result := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    AssertEquals('the curves are readable', 200, Code);
end;

function TBackgroundRestBase.CurveCount(AId: longint): longint;
var
    R: TJSONObject;
begin
    R := Curves(AId);
    try
        Result := R.Arrays['curves'].Count;
    finally
        R.Free;
    end;
end;

function TBackgroundRestBase.CountOfType(AId: longint;
    const ATypeId: TGuid): longint;
var
    R: TJSONObject;
    A: TJSONArray;
    i: longint;
begin
    Result := 0;
    R := Curves(AId);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''), GUIDToString(ATypeId)) then
                Inc(Result);
    finally
        R.Free;
    end;
end;

function TBackgroundRestBase.HandleOfType(AId: longint;
    const ATypeId: TGuid): string;
var
    R: TJSONObject;
    A: TJSONArray;
    i: longint;
begin
    Result := '';
    R := Curves(AId);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''), GUIDToString(ATypeId)) then
                Exit(LowerCase(A.Objects[i].Get('id', '')));
    finally
        R.Free;
    end;
end;

function TBackgroundRestBase.SettingsOf(AId: longint): TJSONObject;
var
    Code: longint;
begin
    Result := Call('GET', Format('/problems/%d/settings', [AId]), '', Code);
    AssertEquals('the settings are readable', 200, Code);
end;

function TBackgroundRestBase.PointsOfType(AId: longint;
    const ATypeId: TGuid): TPointsData;
var
    Code: longint;
    Body: string;
begin
    FApi.Handle('GET', Format('/problems/%d/curves/%s/points',
        [AId, HandleOfType(AId, ATypeId)]), '', Code, Body);
    AssertEquals('the curve''s points are readable', 200, Code);
    AssertTrue('and decode', PointsFromJsonString(Body, Result));
end;

function TBackgroundRestBase.ParamOfType(AId: longint; const ATypeId: TGuid;
    const AName: string): double;
var
    R: TJSONObject;
    A, Params: TJSONArray;
    i, j: longint;
begin
    Result := NaN;
    R := Curves(AId);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''), GUIDToString(ATypeId)) then
            begin
                Params := A.Objects[i].Arrays['params'];
                for j := 0 to Params.Count - 1 do
                    if Params.Objects[j].Get('name', '') = AName then
                        Exit(Params.Objects[j].Get('value', 0.0));
            end;
    finally
        R.Free;
    end;
    Fail('no curve of that type has a parameter ' + AName);
end;

function TBackgroundRestBase.PutParamOfType(AId: longint; const ATypeId: TGuid;
    const AName: string; AValue: double): longint;
var
    R: TJSONObject;
    A, Params: TJSONArray;
    i, j, Index: longint;
    Handle: string;
begin
    Handle := '';
    Index := -1;
    R := Curves(AId);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''), GUIDToString(ATypeId)) then
            begin
                Handle := A.Objects[i].Get('id', '');
                Params := A.Objects[i].Arrays['params'];
                for j := 0 to Params.Count - 1 do
                    if Params.Objects[j].Get('name', '') = AName then
                        Index := j;
                Break;
            end;
    finally
        R.Free;
    end;
    AssertTrue('a curve of that type has a parameter ' + AName, Index >= 0);
    Call('PUT', Format('/problems/%d/curves/%s/params/%d', [AId, Handle, Index]),
        Format('{"value":%s}', [FloatToStr(AValue)]), Result).Free;
end;

function TBackgroundRestBase.Fit(AId: longint): longint;
begin
    Call('POST', Format('/problems/%d/actions/minimize-difference', [AId]), '',
        Result).Free;
end;

function TBackgroundRestBase.RFactorOf(AId: longint): double;
var
    R: TJSONObject;
    Code: longint;
begin
    R := Call('GET', Format('/problems/%d/rfactor', [AId]), '', Code);
    try
        AssertEquals('the R-factor is readable', 200, Code);
        Result := R.Get('curMin', -1.0);
    finally
        R.Free;
    end;
end;

procedure TBackgroundModelRestTest.AddingABackgroundCurveBuildsOneBesideThePicks;
var
    Id: longint;
begin
    Id := BackgroundProblem;
    //  The model starts with the registry's default background beside the peak.
    AssertEquals('a peak and the default background before', 2, CurveCount(Id));
    AssertEquals('the background is accepted', 200,
        ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    AssertEquals('still a peak and ONE background: the choice replaces the default',
        2, CurveCount(Id));
    AssertEquals('one of them the background', 1,
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    AssertEquals('and one the peak', 1,
        CountOfType(Id, TGaussPointsSet.GetCurveTypeId));
end;

procedure TBackgroundModelRestTest.ThePicksAreLeftAsTheUserPutThem;
var
    Id, Code: longint;
    Body: string;
    P: TPointsData;
begin
    //  A BACKGROUND IS NOT PLACED BY A PICK, and adding one must not invent one
    //  - the pick set is model input and a pick at the background's reference
    //  would build a second peak there.
    Id := BackgroundProblem;
    ChooseBackground(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, P));
    AssertEquals('still the one pick', 1, Length(P.X));
    AssertEquals('where it was', 10, P.X[0], 1e-9);
end;

procedure TBackgroundModelRestTest.TheBackgroundIsLastInItsInterval;
var
    Id: longint;
    R: TJSONObject;
    A: TJSONArray;
begin
    //  The engine keeps it last so a curve reduction that puts a peak back
    //  puts it before the background, and every index-based mapping of the
    //  curves to a backend's outcome stays put.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    R := Curves(Id);
    try
        A := R.Arrays['curves'];
        AssertTrue(SameText(GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            A.Objects[A.Count - 1].Get('curveType', '')));
    finally
        R.Free;
    end;
end;

procedure TBackgroundModelRestTest.TheSettingsSayWhichBackgroundTheModelHas;
var
    Id: longint;
    S: TJSONObject;
    Ids: TJSONArray;
begin
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    S := SettingsOf(Id);
    try
        AssertTrue('the type', SameText(
            GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            S.Get('backgroundCurveType', '')));
        Ids := S.Arrays['backgroundCurveIds'];
        AssertEquals('one handle', 1, Ids.Count);
        AssertEquals('the curve''s own',
            HandleOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId),
            LowerCase(Ids.Strings[0]));
    finally
        S.Free;
    end;
end;

procedure TBackgroundModelRestTest.AModelOfPeaksHasAChebyshevBackgroundUnlessAnotherIsChosen;
var
    Id: longint;
    S: TJSONObject;
begin
    Id := BackgroundProblem;
    S := SettingsOf(Id);
    try
        AssertEquals('Chebyshev', GUIDToString(
            TChebyshevBackgroundPointsSet.GetCurveTypeId),
            S.Get('backgroundCurveType', 'absent'));
    finally
        S.Free;
    end;
    AssertEquals('the peak and its background', 2, CurveCount(Id));
end;

procedure TBackgroundModelRestTest.NoBackgroundIsRefusedForAModelOfPeaks;
var
    Id: longint;
    Msg: string;
begin
    Id := BackgroundProblem;
    AssertTrue('refused', PutSettings(Id, '{"backgroundCurveType":""}', Msg) <> 200);
    AssertTrue('and it says why: ' + Msg, Pos('background', LowerCase(Msg)) > 0);
    AssertEquals('the background is still there', 2, CurveCount(Id));
end;

procedure TBackgroundModelRestTest.BackgroundAloneMakesTheModelDrawable;
var
    Id, Code: longint;
    Body: string;
    P: TPointsData;
begin
    //  A MODEL OF ONLY A BACKGROUND IS A MODEL: the user can place it before any
    //  peak, see it drawn, and fit it. Without picks nothing used to be built
    //  at all.
    Id := BackgroundProblem(True);
    AssertEquals('only the default background yet', 1, CurveCount(Id));
    ChooseBackground(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('the chosen background alone', 1, CurveCount(Id));
    AssertEquals('of the chosen shape', 1,
        CountOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId));
    FApi.Handle('GET', Format('/problems/%d/calc-profile', [Id]), '', Code, Body);
    AssertEquals(200, Code);
    AssertTrue(PointsFromJsonString(Body, P));
    AssertTrue('and a calculated profile to draw', Length(P.X) > 0);
end;

procedure TBackgroundModelRestTest.OneBackgroundServesEveryFitInterval;
var
    Id: longint;
    S: TJSONObject;
    Got: TPointsData;
begin
    Id := BackgroundProblemWithBounds([0, 8, 10, 20]);
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('two intervals, one background', 1,
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    S := SettingsOf(Id);
    try
        AssertEquals(1, S.Arrays['backgroundCurveIds'].Count);
    finally
        S.Free;
    end;
    //  Over the whole profile, the gap between the intervals included.
    Got := PointsOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertTrue('from the first sample', Got.X[0] <= 1e-9);
    AssertTrue('to the last', Got.X[High(Got.X)] >= 20 - 1e-9);
end;

procedure TBackgroundModelRestTest.TheBackgroundKeepsItsHandleAcrossAnEdit;
var
    Id, Code: longint;
    Before: string;
begin
    //  Every edit rebuilds every instance. The background has to come back as
    //  the same instance, or the values a fit found for it are orphaned by the
    //  next pick.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Before := HandleOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Call('POST', Format('/problems/%d/points/positions', [Id]),
        '{"x":4,"y":60}', Code).Free;
    AssertEquals('the pick is added', 200, Code);
    AssertEquals('two peaks now', 2, CountOfType(Id, TGaussPointsSet.GetCurveTypeId));
    AssertEquals('the same background', Before,
        HandleOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
end;

procedure TBackgroundModelRestTest.ChangingTheBackgroundTypeGivesItANewHandle;
var
    Id: longint;
    Linear: string;
begin
    //  A DIFFERENT SHAPE HAS NO FITTED VALUES: a line's slope means nothing to
    //  an exponential. A new handle is what keeps the old values from being
    //  restored onto the new shape.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Linear := HandleOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    ChooseBackground(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('the line is gone', 0,
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    AssertTrue('the parabola is a new instance', Linear <>
        HandleOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId));
end;

procedure TBackgroundModelRestTest.TheBackgroundIsSeededFromTheBaseline;
var
    Id, i: longint;
    R: TJSONObject;
    A, Params: TJSONArray;
    B0: double;
    Found: boolean;
begin
    //  Where the sloped peak's baseline starts: 40 + 0.05 * 64 = 43.2. A seed
    //  from the data's own baseline lands near it; a seed of zero would be the
    //  whole profile away.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Found := False;
    B0 := 0;
    R := Curves(Id);
    try
        A := R.Arrays['curves'];
        Params := A.Objects[A.Count - 1].Arrays['params'];
        for i := 0 to Params.Count - 1 do
            if Params.Objects[i].Get('name', '') = 'b0' then
            begin
                B0 := Params.Objects[i].Get('value', 0.0);
                Found := True;
            end;
    finally
        R.Free;
    end;
    AssertTrue('it has a level', Found);
    AssertEquals('near the baseline at the start', 43.2, B0, 5);
end;

{ WITH CURVE SCALING ON, the default, every curve is drawn times the model's
  scale - the data's integral over the model's. A pick seeds its peak at the
  sample's full height, baseline included, so beside a background seeded at the
  baseline the model overshot the data and the scale drew everything smaller:
  the background below the baseline it was seeded from, by a factor that was
  0.46 on 2.dat after a stopped Fit > Automatically. A new background now comes
  with its peaks brought down to the data above it, so the scale is one and the
  level the table shows is the level the chart draws. One drawn level equal to
  its own parameter is the whole statement: the scale is one model-wide, so
  every peak is then drawn at its own values too. (A peak's height is not a
  parameter to compare with - a Gaussian's A is its area.) }
procedure TBackgroundModelRestTest.ANewBackgroundIsDrawnAtTheLevelItWasSeededAt;
var
    Id, i: longint;
    Linear: TGuid;
    Drawn: TPointsData;
    Level, Xr, AtXr: double;
begin
    Id := BackgroundProblem;
    Linear := TLinearBackgroundPointsSet.GetCurveTypeId;
    AssertEquals(200, ChooseBackground(Id, Linear));

    //  b0 is the level at xr, the first sample the seed was fitted through.
    Level := ParamOfType(Id, Linear, 'b0');
    Xr := ParamOfType(Id, Linear, 'xr');
    AssertEquals('seeded at the baseline, about 43', 43.2, Level, 5);
    Drawn := PointsOfType(Id, Linear);
    AssertTrue('the background has points', Length(Drawn.Y) > 0);
    AtXr := NaN;
    for i := 0 to High(Drawn.X) do
        if Abs(Drawn.X[i] - Xr) < 1e-9 then
            AtXr := Drawn.Y[i];
    AssertFalse('the reference is a sample', IsNan(AtXr));
    AssertEquals(Format('drawn at the level it was seeded at, %.2f', [Level]),
        Level, AtXr, 1e-3 * Level);
end;

procedure TBackgroundModelRestTest.OfferedBackgroundIdsAreAdopted;
var
    Id: longint;
    Msg: string;
begin
    //  THE WRITE SIDE OF backgroundCurveIds - a project restore, handing back
    //  the handle its saved values are filed under.
    Id := BackgroundProblem;
    AssertEquals('accepted', 200, PutSettings(Id, Format(
        '{"backgroundCurveType":"%s","backgroundCurveIds":' +
        '["0a0a0a0a-1111-2222-3333-444444444444"]}',
        [GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId)]), Msg));
    AssertEquals('the offered handle', '0a0a0a0a-1111-2222-3333-444444444444',
        HandleOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
end;

procedure TBackgroundModelRestTest.AMalformedBackgroundIdIsRefused;
var
    Id: longint;
    Msg: string;
begin
    Id := BackgroundProblem;
    AssertEquals('refused', 400, PutSettings(Id, Format(
        '{"backgroundCurveType":"%s","backgroundCurveIds":["not-a-handle"]}',
        [GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId)]), Msg));
    AssertTrue('saying what', Pos('not-a-handle', Msg) > 0);
end;

procedure TBackgroundModelRestTest.ABackgroundTypeCannotBeTheCurveType;
var
    Id: longint;
    Msg: string;
begin
    Id := BackgroundProblem;
    AssertEquals('refused', 400, PutSettings(Id, Format('{"curveType":"%s"}',
        [GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId)]), Msg));
    AssertTrue('and says where it belongs: ' + Msg,
        Pos('Model > Background > Curve', Msg) > 0);
end;

procedure TBackgroundModelRestTest.APeakTypeCannotBeTheBackground;
var
    Id: longint;
    Msg: string;
begin
    Id := BackgroundProblem;
    AssertEquals('refused', 400, PutSettings(Id, Format(
        '{"backgroundCurveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Msg));
    AssertTrue('saying it is a peak: ' + Msg, Pos('peak shape', Msg) > 0);
    AssertEquals('nothing was added: the peak and the default background', 2,
        CurveCount(Id));
end;

procedure TBackgroundModelRestTest.APowerLawBackgroundIsRefusedOnDataThatReachesZero;
var
    Id: longint;
    Msg: string;
begin
    //  The sloped peak starts at x = 0.
    Id := BackgroundProblem;
    AssertEquals('refused', 400, PutSettings(Id, Format(
        '{"backgroundCurveType":"%s"}',
        [GUIDToString(TPowerLawBackgroundPointsSet.GetCurveTypeId)]), Msg));
    AssertTrue('saying why: ' + Msg, Pos('greater than zero', Msg) > 0);
end;

procedure TBackgroundModelRestTest.AnUnknownBackgroundTypeIsRefused;
var
    Id: longint;
    Msg: string;
begin
    Id := BackgroundProblem;
    AssertEquals('refused', 400, PutSettings(Id,
        '{"backgroundCurveType":"{11111111-2222-3333-4444-555555555555}"}', Msg));
    AssertTrue('naming it: ' + Msg, Pos('11111111', Msg) > 0);
end;

procedure TBackgroundModelRestTest.EveryCurveSaysWhatTypeItIs;
var
    Id, i: longint;
    R: TJSONObject;
    A: TJSONArray;
begin
    //  THE CLIENT USED TO READ A CURVE'S TYPE BACK FROM ITS TITLE. With two
    //  types in one model, the wire says it.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    R := Curves(Id);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            AssertTrue('curve ' + IntToStr(i) + ' names a type',
                A.Objects[i].Get('curveType', '') <> '');
    finally
        R.Free;
    end;
end;

{ ---- TTestMarkupPointsSet ---- }

const
    TestMarkupSet = 'test-background-markup';

constructor TTestMarkupPointsSet.Create(AOwner: TComponent);
var
    P: TSpecialCurveParameter;
begin
    inherited Create(AOwner);
    P := TUserCurveParameter.Create;
    P.Name := 'q';
    P.Type_ := Variable;
    AddParameter(P);
    InitListOfVariableParameters;
end;

function TTestMarkupPointsSet.GetNativeExpression: string;
begin
    Result := 'q+0*x';
end;

class function TTestMarkupPointsSet.GetCurveTypeName: string;
begin
    Result := 'Test markup (background tests)';
end;

class function TTestMarkupPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{6a17bca1-c2d3-4168-a4cc-c91249f49c5c}');
end;

class function TTestMarkupPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := MaximumsAndMinimums;
end;

class function TTestMarkupPointsSet.PlacedByPointSet: string;
begin
    Result := TestMarkupSet;
end;

class function TTestMarkupPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A curve type the background tests place from a markup of their own.',
        esModelChoice);
    AddParagraph(Result, 'It exists only in the test binary, to reach the ' +
        'path a module''s curve types take through the engine.');
    AddLimitation(Result, 'It is a test fixture and describes no data.');
end;

{ The builder a module would register: one Gaussian at x = 10. }
var
    { Makes the test markup's builder refuse, as a module's does when nothing is
      marked (TRefusedRunTest). }
    RefuseTestMarkupBuild: boolean = False;

function BuildTestMarkup(ATask, AStoredValues: TObject): boolean;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    Task := TFitTask(ATask);
    if RefuseTestMarkupBuild then
        raise EUserException.Create('Nothing is marked to build the model from.');
    Curve := Task.NewInstanceOfType(TGaussPointsSet.GetCurveTypeId, 10);
    Task.CreatePointsFor(Curve);
    Task.AddBuiltCurve(Curve, TMSCRCurveList(AStoredValues));
    Result := True;
end;

procedure TBackgroundTaskTest.ABuilderPathStillGetsTheBackground;
begin
    //  THE BUILDER EXITS THE PEAK BUILD EARLY - it is the whole of it for a
    //  markup-placed type - and the background is put back after the peaks
    //  whichever way they were built.
    BuildTask(TTestMarkupPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, []);
    AssertEquals('the built curve and the background', 2, Curves.Count);
    AssertEquals('the builder''s curve first',
        GUIDToString(TGaussPointsSet.GetCurveTypeId),
        GUIDToString(TNamedPointsSet(Curves.Items[0]).GetCurveTypeId));
    AssertTrue('the background last', LastCurve.IsBackground);
end;

{ ---- TRefusedRunTest ---- }

procedure TRefusedRunTest.TearDown;
begin
    RefuseTestMarkupBuild := False;
end;

procedure TRefusedRunTest.ARefusedAutomaticRunLeavesTheProblemAsItWas;
var
    Svc: TFitService;
    Profile: TTitlePointsSet;
    i: longint;
    Msg: string;
begin
    Svc := TFitService.Create;
    try
        Profile := TTitlePointsSet.Create(nil);
        for i := 0 to 40 do
            Profile.AddNewPoint(i, 5 + 100 * Exp(-Sqr((i - 20) / 3)));
        Svc.SetProfilePointsSet(Profile);
        Profile.Free;
        Svc.SetCurveType(TTestMarkupPointsSet.GetCurveTypeId);
        Svc.AddPointToRFactorBounds(2, 0);
        Svc.AddPointToRFactorBounds(38, 0);

        RefuseTestMarkupBuild := True;
        Msg := '';
        try
            Svc.DoAllAutomatically;
        except
            on E: EUserException do
                Msg := E.Message;
        end;
        AssertEquals('refused, and only that said',
            'Nothing is marked to build the model from.', Msg);
        //  Copies, which are the caller's to free.
        Profile := Svc.GetProfilePointsSet;
        try
            AssertTrue('the data is still loaded',
                Assigned(Profile) and (Profile.PointsCount = 41));
        finally
            Profile.Free;
        end;
        Profile := Svc.GetRFactorBounds;
        try
            AssertTrue('the fit interval is where it was',
                Assigned(Profile) and (Profile.PointsCount = 2));
        finally
            Profile.Free;
        end;
        AssertTrue('the problem is not back to waiting for data',
            Svc.GetState <> ProfileWaiting);
    finally
        Svc.Free;
    end;
end;

{ ---- TProbeFitService ---- }

procedure TProbeFitService.ProbeAutomaticPositions;
begin
    ComputeCurvePositionsForAutoIfPlacedByPositions;
end;

procedure TProbeFitService.ProbeComputePositions;
begin
    ComputeCurvePositionsAlg;
end;

procedure TProbeFitService.ProbeComputeBounds;
begin
    ComputeCurveBoundsAlg;
end;

{ The engine asks the scientific module's peak search (peak_search, moved
  2026-10-18) for a type placed at curve positions: the automatic run's
  positions, Compute Automatically's positions and its fit intervals all
  come back from it, at the fraction the module keeps for the problem. }
procedure TBackgroundServiceTest.ThePeakSearchPlacesPositionsAndBoundsForAPeakType;
var
    Svc: TProbeFitService;
    Data: TPointsData;
    Profile: TTitlePointsSet;
    i: longint;
begin
    AssertTrue(PointsFromJsonString(SlopedPeakJson, Data));
    Profile := TTitlePointsSet.Create(nil);
    Svc := TProbeFitService.Create;
    try
        for i := 0 to High(Data.X) do
            Profile.AddNewPoint(Data.X[i], Data.Y[i]);
        Svc.SetProfilePointsSet(Profile);
        Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
        Svc.ProbeAutomaticPositions;
        AssertTrue('the automatic run''s positions',
            Svc.GetCurvePositions.PointsCount > 0);
        Svc.ProbeComputePositions;
        AssertTrue('Compute Automatically''s positions',
            Svc.GetCurvePositions.PointsCount > 0);
        Svc.ProbeComputeBounds;
        AssertTrue('and its fit intervals',
            Svc.GetRFactorBounds.PointsCount >= 2);
    finally
        Svc.Free;
        Profile.Free;
    end;
end;

procedure TBackgroundServiceTest.TheAutomaticRunStartsOneCurveOnEverySampleOfEveryInterval;
var
    Svc: TProbeFitService;
    Data, Bounds, Picks: TPointsData;
    Profile: TTitlePointsSet;
    i: longint;
begin
    //  The path the automatic run takes (DoAllAutomaticallyAlg), entered where
    //  it enters: the positions and then the intervals, nothing placed.
    AssertTrue(PointsFromJsonString(PeakAndLowHumpJson, Data));
    Profile := TTitlePointsSet.Create(nil);
    Svc := TProbeFitService.Create;
    try
        for i := 0 to High(Data.X) do
            Profile.AddNewPoint(Data.X[i], Data.Y[i]);
        Svc.SetProfilePointsSet(Profile);
        Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
        Svc.SetBackgroundCurveType(TChebyshevBackgroundPointsSet.GetCurveTypeId, nil);
        Svc.ProbeAutomaticPositions;
        Svc.ProbeComputeBounds;
        Bounds := Default(TPointsData);
        SetLength(Bounds.X, Svc.GetRFactorBounds.PointsCount);
        for i := 0 to High(Bounds.X) do
            Bounds.X[i] := Svc.GetRFactorBounds.PointXCoord[i];
        Picks := Default(TPointsData);
        SetLength(Picks.X, Svc.GetCurvePositions.PointsCount);
        for i := 0 to High(Picks.X) do
            Picks.X[i] := Svc.GetCurvePositions.PointXCoord[i];
        AssertPicksAreTheIntervalSamples(Bounds, Picks);
        for i := 0 to Svc.GetCurvePositions.PointsCount - 1 do
            AssertTrue(Format('the pick at %g starts a curve with an amplitude',
                [Svc.GetCurvePositions.PointXCoord[i]]),
                Svc.GetCurvePositions.PointYCoord[i] > 0);
    finally
        Svc.Free;
        Profile.Free;
    end;
end;

{ ---- TProbeFitTask ---- }

function TProbeFitTask.ProbeSmallAmplitudeDeletion: boolean;
begin
    Result := DeleteCurvesWithSmallAmplitude;
end;

function TProbeFitTask.ProbeMaxDerivativeDeletion(
    var ADeleted: TCurvePointsSet): boolean;
begin
    Result := DeleteCurveWithMaxExpDerivative(ADeleted);
end;

function TProbeFitTask.ProbeMinimalAmplitudeDeletion(
    var ADeleted: TCurvePointsSet): boolean;
begin
    Result := DeleteCurveWithMinimalAmplitude(ADeleted);
end;

procedure TProbeFitTask.ProbePutBack(ADeleted: TCurvePointsSet);
begin
    PutBackDeletedCurve(ADeleted);
end;

procedure TProbeFitTask.ProbeSetFirstShared(AValue: double);
begin
    FCommonVaryingFlag := True;
    FCommonVaryingIndex := 0;
    SetParam(AValue);
end;

function TProbeFitTask.ProbeReductionRFactor: double;
begin
    Result := GetReductionRFactor;
end;

function TProbeFitTask.ProbeRFactor: double;
begin
    Result := GetRFactor;
end;

function TProbeFitTask.ProbeParametersWalked(ADecomposing: boolean): longint;
begin
    FDecomposing := ADecomposing;
    try
        Result := 0;
        SetFirstParam;
        while not EndOfCycle do
        begin
            Inc(Result);
            SetNextParam;
        end;
    finally
        FDecomposing := False;
    end;
end;

function TProbeFitTask.ProbeParametersRead: longint;
var
    Value, Step: double;
begin
    Result := 0;
    SetFirstParam;
    while not EndOfCycle do
    begin
        Value := GetParam;
        Step := GetVariationStep;
        if IsNan(Value) or IsNan(Step) then
            Exit(-1);
        Inc(Result);
        SetNextParam;
    end;
end;

function TProbeFitTask.SharedCount: longint;
begin
    Result := FCommonVariableParameters.Count;
end;

{ ---- TBackgroundTaskTest ---- }

procedure TBackgroundTaskTest.BuildTask(const APeakType, ABackground: TGuid;
    const APicks: array of double);
var
    Data: TPointsData;
    Profile, Picks: TPointsSet;
    i, k: longint;
begin
    AssertTrue(PointsFromJsonString(SlopedPeakJson, Data));
    Profile := TPointsSet.Create(nil);
    for i := 0 to High(Data.X) do
        Profile.AddNewPoint(Data.X[i], Data.Y[i]);
    Picks := TPointsSet.Create(nil);
    for k := 0 to High(APicks) do
        for i := 0 to High(Data.X) do
            if Abs(Data.X[i] - APicks[k]) < 1e-9 then
                Picks.AddNewPoint(Data.X[i], Data.Y[i]);
    FTask := TProbeFitTask.Create(nil, True);
    FTask.CurveTypeId := APeakType;
    FTask.BackgroundCurveTypeId := ABackground;
    FTask.SetProfilePointsSet(Profile);
    FTask.SetCurvePositions(Picks);
    FTask.RecreateCurves(nil);
end;

procedure TBackgroundTaskTest.TearDown;
begin
    FreeAndNil(FTask);
end;

function TBackgroundTaskTest.Curves: TSelfCopiedCompList;
begin
    Result := FTask.GetCurves;
end;

function TBackgroundTaskTest.LastCurve: TNamedPointsSet;
begin
    Result := TNamedPointsSet(Curves.Items[Curves.Count - 1]);
end;

function TBackgroundTaskTest.PeakCount: longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to Curves.Count - 1 do
        if not TNamedPointsSet(Curves.Items[i]).IsBackground then
            Inc(Result);
end;

procedure TBackgroundTaskTest.NoBackgroundTypeMeansNoBackground;
begin
    //  THE ZERO VALUE MEANS NONE, which is what a task built through the
    //  inherited constructor - and every task before this existed - holds.
    BuildTask(TGaussPointsSet.GetCurveTypeId, GUID_NULL, [6, 10]);
    AssertEquals('the two peaks and nothing else', 2, Curves.Count);
end;

procedure TBackgroundTaskTest.RecreatingPutsTheBackgroundLast;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    AssertEquals('two peaks and a background', 3, Curves.Count);
    AssertTrue('the background is last', LastCurve.IsBackground);
    AssertEquals('of the type asked for',
        GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
        GUIDToString(LastCurve.GetCurveTypeId));
end;

procedure TBackgroundTaskTest.ARebuildKeepsTheBackgroundItFitted;
var
    Before: TNamedPointsSet;
begin
    //  THE REBUILD A REFIT DOES (MinimizeDifferenceAgain) keeps every curve
    //  the task already holds - the background included, with what it found.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [10]);
    Before := LastCurve;
    Before.ValuesByName['b1'] := 0.123;
    FTask.RecreateCurves(nil);
    AssertTrue('the same object', Before = LastCurve);
    AssertEquals('with its values', 0.123, LastCurve.ValuesByName['b1'], 1e-12);
    AssertEquals('and still one', 2, Curves.Count);
end;

procedure TBackgroundTaskTest.ABackgroundOfAnotherTypeIsReplaced;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [10]);
    FTask.BackgroundCurveTypeId := TExponentialBackgroundPointsSet.GetCurveTypeId;
    FTask.RecreateCurves(nil);
    AssertEquals('one background still', 2, Curves.Count);
    AssertEquals('of the new shape',
        GUIDToString(TExponentialBackgroundPointsSet.GetCurveTypeId),
        GUIDToString(LastCurve.GetCurveTypeId));
end;

procedure TBackgroundTaskTest.ABackgroundWithNoPicksIsTheWholeModel;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TQuadraticBackgroundPointsSet.GetCurveTypeId, []);
    AssertEquals('the background alone', 1, Curves.Count);
    AssertTrue(LastCurve.IsBackground);
end;

procedure TBackgroundTaskTest.TheBackgroundIsNotAPeaksSharedParameterSource;
begin
    //  The pseudo-Voigt's width is varied once for all its peaks; a background
    //  has none, and must not be where the task learns what is shared.
    BuildTask(TPseudoVoigtPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [10]);
    AssertEquals('sigma, from the peak', 1, FTask.SharedCount);
end;

procedure TBackgroundTaskTest.SettingASharedParameterSkipsCurvesWithoutIt;
var
    i: longint;
begin
    //  THE CRASH THIS PREVENTS: a shared parameter was written into EVERY curve
    //  by name, and a curve without that name refuses the write - so a
    //  pseudo-Voigt model with a background stopped at its first shared step.
    BuildTask(TPseudoVoigtPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    FTask.ProbeSetFirstShared(0.3);
    for i := 0 to Curves.Count - 1 do
        if not TNamedPointsSet(Curves.Items[i]).IsBackground then
            AssertEquals('every peak got it', 0.3,
                TNamedPointsSet(Curves.Items[i]).ValuesByName['sigma'], 1e-12);
end;

{ The pseudo-Voigt's width is one parameter for all its peaks: the optimiser
  walks each peak's own parameters and then the shared width, once - read
  from the shared set, not from a curve. }
procedure TBackgroundTaskTest.TheWalkVisitsTheSharedWidthOnceAfterEveryCurve;
var
    i, Own: longint;
begin
    BuildTask(TPseudoVoigtPointsSet.GetCurveTypeId, GUID_NULL, [6, 10]);
    Own := 0;
    for i := 0 to Curves.Count - 1 do
        Inc(Own, TCurvePointsSet(Curves.Items[i]).VariableCount);
    AssertEquals('sigma, shared', 1, FTask.SharedCount);
    AssertEquals('every peak''s own, then sigma once', Own + 1,
        FTask.ProbeParametersRead);
end;

procedure TBackgroundTaskTest.SmallAmplitudeDeletionSkipsTheBackground;
var
    i: longint;
    C: TNamedPointsSet;
begin
    //  A background has no amplitude, and that used to make this strategy give
    //  up on the whole task - as well as putting the background itself in
    //  line to be deleted for being "small".
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    for i := 0 to Curves.Count - 1 do
    begin
        C := TNamedPointsSet(Curves.Items[i]);
        if C.IsBackground then
            Continue;
        if Abs(C.x0 - 6) < 1e-9 then
            C.A := 1e-6
        else
            C.A := 100;
    end;
    AssertTrue('the tiny peak went', FTask.ProbeSmallAmplitudeDeletion);
    AssertEquals('one peak left', 1, PeakCount);
    AssertTrue('and the background stayed', LastCurve.IsBackground);
end;

procedure TBackgroundTaskTest.DerivativeDeletionIsNotDisabledByTheBackground;
var
    Deleted: TCurvePointsSet;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    Deleted := nil;
    try
        AssertTrue('a peak was taken', FTask.ProbeMaxDerivativeDeletion(Deleted));
        AssertFalse('not the background',
            TNamedPointsSet(Deleted).IsBackground);
        AssertTrue('which is still there', LastCurve.IsBackground);
    finally
        Deleted.Free;
    end;
end;

procedure TBackgroundTaskTest.MinimalAmplitudeDeletionIsNotDisabledByTheBackground;
var
    Deleted: TCurvePointsSet;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    Deleted := nil;
    try
        AssertTrue('a peak was taken', FTask.ProbeMinimalAmplitudeDeletion(Deleted));
        AssertFalse('not the background', TNamedPointsSet(Deleted).IsBackground);
    finally
        Deleted.Free;
    end;
end;

procedure TBackgroundTaskTest.OnePeakAndABackgroundLeaveNothingToDelete;
var
    Deleted: TCurvePointsSet;
begin
    //  THE STOP COUNT IS OF PEAKS. The last peak is never taken, and the
    //  background is not counted as a second one.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [10]);
    Deleted := nil;
    AssertFalse('by slope', FTask.ProbeMaxDerivativeDeletion(Deleted));
    AssertFalse('by size', FTask.ProbeMinimalAmplitudeDeletion(Deleted));
    AssertEquals('both still there', 2, Curves.Count);
end;

procedure TBackgroundTaskTest.ARestoredCurveGoesBeforeTheBackground;
var
    Deleted: TCurvePointsSet;
begin
    //  A REDUCTION THAT OVERSHOT PUTS A PEAK BACK. At the end of the list it
    //  would sit after the background, and every index-based mapping of the
    //  curves - a formula backend's outcome among them - would shift by one.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    Deleted := nil;
    AssertTrue(FTask.ProbeMinimalAmplitudeDeletion(Deleted));
    FTask.ProbePutBack(Deleted);
    AssertEquals('all three again', 3, Curves.Count);
    AssertTrue('the background still last', LastCurve.IsBackground);
end;

procedure TBackgroundModelRestTest.ANewProfileKeepsTheChosenBackground;
var
    Id, Code: longint;
    S: TJSONObject;
begin
    //  A CHOICE ABOUT THE MODEL, like the peak type, not a pick on the data -
    //  so it outlives the profile it was made on. Its handles do not: they were
    //  issued for instances of the old data, and there are no intervals yet.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Call('PUT', Format('/problems/%d/profile', [Id]), SlopedPeakJson, Code).Free;
    AssertEquals('another profile', 200, Code);
    S := SettingsOf(Id);
    try
        AssertTrue('the background kept', SameText(
            GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            S.Get('backgroundCurveType', '')));
        AssertEquals('with no handles yet', 0, S.Arrays['backgroundCurveIds'].Count);
    finally
        S.Free;
    end;
end;

{ ---- the retired variation key ---- }

procedure TBackgroundModelRestTest.ARetiredBackgroundVariationKeyIsIgnored;
var
    Id: longint;
    Msg: string;
begin
    //  A client written while Enable Variation existed still sends its key, and
    //  a background curve is added beside it as it always could be - the key
    //  is read as nothing, not refused.
    Id := BackgroundProblem;
    AssertEquals('accepted', 200, PutSettings(Id, Format(
        '{"backgroundVariation":true,"backgroundCurveType":"%s"}',
        [GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId)]), Msg));
    AssertEquals('the background is there', 1,
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
end;

{ ---- TBackgroundModelFitTest ---- }

function TBackgroundModelFitTest.PeaksOnCurveJson(ACurve: TNamedPointsSet;
    AFrom, ATo: double; const APeakX, APeakA: array of double): string;
var
    Grid: TPointsSet;
    P: TPointsData;
    i, k: longint;
    x: double;
begin
    Grid := TPointsSet.Create(nil);
    try
        x := AFrom;
        while x <= ATo + 1e-9 do
        begin
            Grid.AddNewPoint(x, 0);
            x := x + 0.25;
        end;
        ACurve.SetWindow(Grid, 0, Grid.PointsCount - 1);
        ACurve.ReCalc;
        P := Default(TPointsData);
        P.Title := 'profile';
        SetLength(P.X, Grid.PointsCount);
        SetLength(P.Y, Grid.PointsCount);
        for i := 0 to Grid.PointsCount - 1 do
        begin
            P.X[i] := Grid.PointXCoord[i];
            P.Y[i] := ACurve.PointYCoord[i];
            for k := 0 to High(APeakX) do
                P.Y[i] := P.Y[i] + GaussPoint(APeakA[k], 0.9, APeakX[k], P.X[i]);
        end;
    finally
        Grid.Free;
    end;
    Result := PointsToJsonString(P);
end;

function TBackgroundModelFitTest.OneDatProblem(AFrom, ATo: double;
    const APicks: array of double): longint;
var
    Loader: TDATFileLoader;
    PS: TTitlePointsSet;
    P, B, Picks: TPointsData;
    i, k, Code: longint;
begin
    Loader := TDATFileLoader.Create(nil);
    try
        Loader.LoadDataSet(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
            DirectorySeparator + 'Data' + DirectorySeparator + '1.dat'));
        PS := Loader.GetPointsSetCopy;
    finally
        Loader.Free;
    end;
    P := Default(TPointsData);
    Picks := Default(TPointsData);
    try
        for i := 0 to PS.PointsCount - 1 do
        begin
            //  THE WINDOW IS THE PROFILE, not a fit interval in the whole of
            //  1.dat: the background is one curve under the whole profile, and
            //  these tests ask what it does over this stretch alone.
            if (PS.PointXCoord[i] < AFrom) or (PS.PointXCoord[i] > ATo) then
                Continue;
            SetLength(P.X, Length(P.X) + 1);
            SetLength(P.Y, Length(P.Y) + 1);
            P.X[High(P.X)] := PS.PointXCoord[i];
            P.Y[High(P.Y)] := PS.PointYCoord[i];
            for k := 0 to High(APicks) do
                if Abs(PS.PointXCoord[i] - APicks[k]) < 1e-6 then
                begin
                    SetLength(Picks.X, Length(Picks.X) + 1);
                    SetLength(Picks.Y, Length(Picks.Y) + 1);
                    Picks.X[High(Picks.X)] := PS.PointXCoord[i];
                    Picks.Y[High(Picks.Y)] := PS.PointYCoord[i];
                end;
        end;
    finally
        PS.Free;
    end;
    AssertEquals('every pick is a sample of 1.dat', Length(APicks), Length(Picks.X));

    Result := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Result]), PointsToJsonString(P),
        Code).Free;
    AssertEquals('1.dat is accepted', 200, Code);
    B := Default(TPointsData);
    B.X := [AFrom, ATo];
    B.Y := [0, 0];
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        PointsToJsonString(B), Code).Free;
    AssertEquals('the window is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Result]),
        PointsToJsonString(Picks), Code).Free;
    AssertEquals('the picks are accepted', 200, Code);
end;

procedure TBackgroundModelFitTest.TheIntervalsFoundAreThePeaksAboveTheChosenBackground;
var
    Id, Code, k: longint;
    Body: string;
    Bounds: TPointsData;
    Holds30, Holds70: boolean;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), PeaksOnANoisyFloorJson,
        Code).Free;
    AssertEquals(200, ChooseBackground(Id,
        TChebyshevBackgroundPointsSet.GetCurveTypeId));
    Call('POST', Format('/problems/%d/actions/compute-curve-bounds', [Id]), '',
        Code).Free;
    AssertEquals('Fit Intervals > Auto', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, Bounds));
    AssertEquals('two intervals, one per peak', 4, Length(Bounds.X));
    Holds30 := False;
    Holds70 := False;
    k := 0;
    while k + 1 <= High(Bounds.X) do
    begin
        if (Bounds.X[k] <= 30) and (30 <= Bounds.X[k + 1]) then
        begin
            Holds30 := True;
            AssertTrue(Format('the peak at 30 is bounded on its floor: %g..%g',
                [Bounds.X[k], Bounds.X[k + 1]]),
                (Bounds.X[k] >= 24) and (Bounds.X[k] <= 27.5) and
                (Bounds.X[k + 1] >= 32.5) and (Bounds.X[k + 1] <= 36));
        end;
        if (Bounds.X[k] <= 70) and (70 <= Bounds.X[k + 1]) then
            Holds70 := True;
        Inc(k, 2);
    end;
    AssertTrue('one interval holds the peak at 30', Holds30);
    AssertTrue('and one the peak at 70', Holds70);
end;

procedure TBackgroundModelFitTest.ThePositionsFoundAreThePeaksAboveTheChosenBackground;
var
    Id, Code, i: longint;
    Body: string;
    Picks: TPointsData;
    Near30, Near70: boolean;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), PeaksOnANoisyFloorJson,
        Code).Free;
    AssertEquals(200, ChooseBackground(Id,
        TChebyshevBackgroundPointsSet.GetCurveTypeId));
    Call('POST', Format('/problems/%d/actions/compute-curve-positions', [Id]), '',
        Code).Free;
    AssertEquals('Curve Positions > Auto', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, Picks));
    Near30 := False;
    Near70 := False;
    for i := 0 to High(Picks.X) do
    begin
        if Abs(Picks.X[i] - 30) <= 1 then
            Near30 := True;
        if Abs(Picks.X[i] - 70) <= 1 then
            Near70 := True;
    end;
    //  NONE ON THE FLOOR: every pick within the interval of a peak, which
    //  reaches about 6 to either side of its top (the bounds test above).
    //  Every sample of the interval is a pick now, not only the tops the peak
    //  search found, which spanned about 3.
    for i := 0 to High(Picks.X) do
        AssertTrue(Format('the pick at %g is on a peak, not the floor',
            [Picks.X[i]]), (Abs(Picks.X[i] - 30) <= 6.5) or
            (Abs(Picks.X[i] - 70) <= 6.5));
    AssertTrue('at the peak at 30', Near30);
    AssertTrue('and at 70', Near70);
end;

procedure TBackgroundModelFitTest.EveryIntervalSampleIsAPickAndNoneOutsideIsOne;
var
    Id, Code, i: longint;
    Body: string;
    Bounds, Picks: TPointsData;
    HumpPicked: boolean;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), PeakAndLowHumpJson,
        Code).Free;
    AssertEquals(200, ChooseBackground(Id,
        TChebyshevBackgroundPointsSet.GetCurveTypeId));
    Call('POST', Format('/problems/%d/actions/compute-curve-bounds', [Id]), '',
        Code).Free;
    AssertEquals('Fit Intervals > Auto', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, Bounds));
    Call('POST', Format('/problems/%d/actions/compute-curve-positions', [Id]), '',
        Code).Free;
    AssertEquals('Curve Positions > Auto', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, Picks));
    AssertPicksAreTheIntervalSamples(Bounds, Picks);
    HumpPicked := False;
    for i := 0 to High(Picks.X) do
        if Abs(Picks.X[i] - 45) <= 0.5 then
            HumpPicked := True;
    AssertTrue('the low hump has its curves too', HumpPicked);
end;

{ The titles of the Run Log of problem AId, one per line. }
function RunLogTitles(AApi: TFitRestApi; AId: longint): string;
var
    Code, i: longint;
    Body: string;
    O: TJSONObject;
    A: TJSONArray;
begin
    Result := '';
    AApi.Handle('GET', Format('/problems/%d/progress?since=0&snapshot=0', [AId]),
        '', Code, Body);
    O := TJSONObject(GetJSON(Body));
    try
        A := O.Arrays['events'];
        for i := 0 to A.Count - 1 do
            Result := Result + A.Objects[i].Get('title', '') + LineEnding;
    finally
        O.Free;
    end;
end;

procedure TBackgroundModelFitTest.AFitsRunLogNamesItsStagesAndWhatItSkipped;
var
    Id: longint;
    Titles: string;
begin
    Id := BackgroundProblem;
    AssertEquals('fitted', 200, Fit(Id));
    Titles := RunLogTitles(FApi, Id);
    AssertTrue('the peaks were not searched for - the picks are the user''s: ' +
        Titles, Pos('Finding the peaks - skipped', Titles) > 0);
    AssertTrue('the intervals marked were used', Pos(
        'Finding the fit intervals - skipped', Titles) > 0);
    AssertTrue('the peaks were fitted', Pos('Fitting the peaks', Titles) > 0);
    AssertTrue('the background is fitted with the peaks in a fit you start',
        Pos('Fitting the background to the floor of the data - skipped',
        Titles) > 0);
    AssertTrue('in turns with the background',
        Pos('Fitting the background in turns', Titles) > 0);
end;

procedure TBackgroundModelFitTest.AnAutomaticRunsRunLogNamesItsStages;
var
    Id, Code: longint;
    Titles: string;
begin
    Id := NewAutomaticProblem;
    Call('POST', Format('/problems/%d/actions/do-all-automatically', [Id]), '',
        Code).Free;
    AssertEquals('ran', 200, Code);
    Titles := RunLogTitles(FApi, Id);
    AssertTrue('the peaks found: ' + Titles, (Pos('Finding the peaks', Titles) > 0)
        and (Pos('Finding the peaks - skipped', Titles) = 0));
    AssertTrue('the intervals found', (Pos('Finding the fit intervals', Titles) > 0)
        and (Pos('Finding the fit intervals - skipped', Titles) = 0));
    AssertTrue('curves removed', Pos('Removing curves that pay for nothing',
        Titles) > 0);
    AssertTrue('the background fitted to the floor, before the reduction: ' +
        Titles, (Pos('Fitting the background to the floor of the data', Titles) > 0)
        and (Pos('Fitting the background to the floor of the data - skipped',
        Titles) = 0));
    AssertTrue('and in that order',
        (Pos('Finding the fit intervals', Titles) <
         Pos('Fitting the background to the floor of the data', Titles)) and
        (Pos('Fitting the background to the floor of the data', Titles) <
         Pos('Removing curves that pay for nothing', Titles)));
    //  THEN THE TURNS, as any fit takes them.
    AssertTrue('the background is fitted in turns after it',
        (Pos('Fitting the background in turns', Titles) > 0) and
        (Pos('Fitting the background in turns - skipped', Titles) = 0));
end;

procedure TBackgroundModelFitTest.AndTheBackgroundStartsOnTheFloor;
var
    Id, Code, i: longint;
    Drawn: TPointsData;
    Mean: double;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), PeaksOnANoisyFloorJson,
        Code).Free;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        '{"x":[0,100],"y":[0,0]}', Code).Free;
    Call('PUT', Format('/problems/%d/positions', [Id]), '{"x":[30,70],"y":[250,170]}',
        Code).Free;
    AssertEquals(200, ChooseBackground(Id,
        TChebyshevBackgroundPointsSet.GetCurveTypeId));
    //  BEFORE ANY FIT: the seed is what is drawn.
    Drawn := PointsOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId);
    AssertTrue('the background is drawn', Length(Drawn.Y) > 0);
    Mean := 0;
    for i := 0 to High(Drawn.Y) do
        Mean := Mean + Drawn.Y[i] / Length(Drawn.Y);
    AssertEquals('it starts on the floor, not under it', 50, Mean, 3);
end;

procedure TBackgroundModelFitTest.ABackgroundCurveIsFittedWithThePeaks;
var
    Truth: TNamedPointsSet;
    Drawn: TPointsData;
    B0, Xr: double;
    i: longint;
    Id, Code: longint;
    Json: string;
    Picks: TPointsData;
begin
    //  Two peaks on a known parabola; the fit has to find the parabola.
    Truth := TQuadraticBackgroundPointsSet.Create(nil);
    try
        Truth.ValuesByName['xr'] := 0;
        Truth.ValuesByName['b0'] := 50;
        Truth.ValuesByName['b1'] := 2;
        Truth.ValuesByName['b2'] := -0.04;
        Json := PeaksOnCurveJson(Truth, 0, 20, [7, 13], [120, 80]);
    finally
        Truth.Free;
    end;
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), Json, Code).Free;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        '{"x":[0,20],"y":[0,0]}', Code).Free;
    Picks := Default(TPointsData);
    Picks.X := [7, 13];
    Picks.Y := [140, 100];
    Call('PUT', Format('/problems/%d/positions', [Id]), PointsToJsonString(Picks),
        Code).Free;
    AssertEquals(200, ChooseBackground(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId));

    AssertEquals('fitted', 200, Fit(Id));
    AssertTrue('a good fit: ' + FloatToStr(RFactorOf(Id)), RFactorOf(Id) < 1e-6);

    //  WHAT THE USER SEES is the drawn background, and it is the parabola.
    Drawn := PointsOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    for i := 0 to High(Drawn.X) do
        AssertEquals('at x = ' + FloatToStr(Drawn.X[i]),
            50 + 2 * Drawn.X[i] - 0.04 * Sqr(Drawn.X[i]), Drawn.Y[i], 0.5);

    //  THE COEFFICIENTS CARRY THE MODEL-WIDE SCALING FACTOR, as a peak's
    //  amplitude always has: with curve scaling on, the engine fits the shape
    //  and one factor maps the whole model onto the data, and the parameters are
    //  reported before it. Their RATIOS are what the fit found - slope and
    //  curvature relative to the level.
    //  About the reference the fit's seed chose: the truth, 50 + 2 x - 0.04
    //  x^2, written about xr has the level and slope it has there.
    Xr := ParamOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId, 'xr');
    B0 := ParamOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId, 'b0');
    AssertEquals('slope per level', (2 - 0.08 * Xr) / (50 + 2 * Xr - 0.04 * Sqr(Xr)),
        ParamOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId, 'b1') / B0,
        1e-3);
    AssertEquals('curvature per level', -0.04 / (50 + 2 * Xr - 0.04 * Sqr(Xr)),
        ParamOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId, 'b2') / B0,
        1e-4);
end;

procedure TBackgroundModelFitTest.TheProfileIsLeftAsMeasured;
var
    Id, Code: longint;
    Before, After: string;
begin
    //  THE POINT OF A BACKGROUND IN THE MODEL: nothing is taken out of the data.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Before);
    AssertEquals('fitted', 200, Fit(Id));
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, After);
    AssertEquals('the same profile', Before, After);
end;

procedure TBackgroundModelFitTest.AFittedBackgroundIsMarkedFittedAndKeptAcrossAnEdit;
var
    Id, Code, i: longint;
    R: TJSONObject;
    A: TJSONArray;
    Slope: double;
    Fitted: boolean;
begin
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('fitted', 200, Fit(Id));
    Slope := ParamOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId, 'b1');

    Fitted := False;
    R := Curves(Id);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''),
                GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId)) then
                Fitted := A.Objects[i].Get('fitted', False);
    finally
        R.Free;
    end;
    AssertTrue('an optimiser produced its values', Fitted);

    //  A pick added after the fit rebuilds every instance; the background comes
    //  back with what the fit found, not with its seed.
    Call('POST', Format('/problems/%d/points/positions', [Id]),
        '{"x":4,"y":60}', Code).Free;
    AssertEquals(200, Code);
    AssertEquals('the fitted slope survives the edit', Slope,
        ParamOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId, 'b1'), 1e-9);
end;

procedure TBackgroundModelFitTest.EveryBackgroundShapeRecoversItsOwnCurve;
var
    Classes: TCurveClasses;
    Cls: TCurveClass;
    k: longint;
    Truth: TNamedPointsSet;
    Id, Code, i: longint;
    Json: string;
    Want, Got: TPointsData;
    Worst, Level: double;
    Found: boolean;
begin
    //  WALKED OVER THE REGISTRY, so a module's own background shape is held to
    //  it too. Each shape's truth is ITSELF, seeded from a smooth falling
    //  baseline - so the profile is exactly representable by it, and whatever
    //  the fit leaves is the fit's doing.
    //  THE CLASSES TAKEN FIRST: the registry's iterator is one shared object,
    //  and a fit looks curve types up through it - walked across the fits, the
    //  walk was sent back to the start each time and never ended.
    Found := False;
    Classes := RegisteredCurveClasses;
    for k := 0 to High(Classes) do
    begin
        Cls := Classes[k];
        if Cls.IsBackground then
        begin
            Found := True;
            Truth := Cls.Create(nil);
            try
                Truth.SeedFromBaseline([1, 6, 11, 16, 21],
                    [300, 200, 150, 125, 110]);
                Json := PeaksOnCurveJson(Truth, 1, 21, [11], [150]);
                AssertTrue(PointsFromJsonString(Json, Want));
                //  The background alone, over the same grid.
                for i := 0 to High(Want.X) do
                    Want.Y[i] := Truth.PointYCoord[i];
            finally
                Truth.Free;
            end;

            Id := NewProblem;
            Call('PUT', Format('/problems/%d/settings', [Id]),
                Format('{"curveType":"%s"}',
                [GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code).Free;
            Call('PUT', Format('/problems/%d/profile', [Id]), Json, Code).Free;
            Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
                '{"x":[1,21],"y":[0,0]}', Code).Free;
            Call('PUT', Format('/problems/%d/positions', [Id]),
                '{"x":[11],"y":[300]}', Code).Free;
            AssertEquals(Cls.GetCurveTypeName + ' is accepted', 200,
                ChooseBackground(Id, Cls.GetCurveTypeId));
            AssertEquals(Cls.GetCurveTypeName + ' fits', 200, Fit(Id));

            Got := PointsOfType(Id, Cls.GetCurveTypeId);
            AssertEquals(Length(Want.X), Length(Got.X));
            Worst := 0;
            Level := 0;
            for i := 0 to High(Want.X) do
            begin
                Worst := Max(Worst, Abs(Got.Y[i] - Want.Y[i]));
                Level := Level + Want.Y[i] / Length(Want.X);
            end;
            AssertTrue(Format('%s: the fitted background is within 5%% of its ' +
                'truth everywhere (worst %.3g of %.3g)',
                [Cls.GetCurveTypeName, Worst, Level]), Worst < 0.05 * Level);
        end;
    end;
    AssertTrue('a background shape was walked', Found);
end;

procedure TBackgroundModelFitTest.TheFittedBackgroundIsOneCurveUnderEveryInterval;
var
    Truth: TNamedPointsSet;
    Id, Code, i: longint;
    Json: string;
    Want, Got: TPointsData;
    Worst, Level: double;
begin
    //  A curved baseline under two peaks, each in an interval of its own with
    //  bare baseline between them.
    Truth := TQuadraticBackgroundPointsSet.Create(nil);
    try
        Truth.SeedFromBaseline([1, 11, 21, 31, 41], [300, 220, 175, 155, 150]);
        Json := PeaksOnCurveJson(Truth, 1, 41, [11, 31], [150, 150]);
        AssertTrue(PointsFromJsonString(Json, Want));
        for i := 0 to High(Want.X) do
            Want.Y[i] := Truth.PointYCoord[i];
    finally
        Truth.Free;
    end;
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}',
        [GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), Json, Code).Free;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        '{"x":[6,16,26,36],"y":[0,0,0,0]}', Code).Free;
    AssertEquals('the intervals are accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[11,31],"y":[300,300]}', Code).Free;
    AssertEquals(200, ChooseBackground(Id,
        TQuadraticBackgroundPointsSet.GetCurveTypeId));
    AssertEquals('it fits', 200, Fit(Id));

    AssertEquals('one background for both intervals', 1,
        CountOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId));
    Got := PointsOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('over every sample', Length(Want.X), Length(Got.X));
    Worst := 0;
    Level := 0;
    for i := 0 to High(Want.X) do
    begin
        Worst := Max(Worst, Abs(Got.Y[i] - Want.Y[i]));
        Level := Level + Want.Y[i] / Length(Want.X);
    end;
    AssertTrue(Format('the background is its truth everywhere, within 5%% ' +
        '(worst %.3g of %.3g)', [Worst, Level]), Worst < 0.05 * Level);
end;

{ THE SHAPES THAT FIT A TAIL, on the data's floor. They were compared with no
  background at all; a model of peaks always has one now (2026-10-31), so each
  is held to the floor of the data it was chosen for. }
procedure TBackgroundModelFitTest.TheLowAngleTailOf1DatIsFittedByAFallingBackground;
var
    Id, k: longint;
    Drawn: TPointsData;
    Shape: TGuid;
begin
    //  Data/1.dat from 2theta = 3 to 20: the falling tail, 3377 counts down to
    //  about 770, and the peak at 11.2.
    for k := 0 to 1 do
    begin
        if k = 0 then
            Shape := TExponentialBackgroundPointsSet.GetCurveTypeId
        else
            Shape := TPowerLawBackgroundPointsSet.GetCurveTypeId;
        Id := OneDatProblem(3, 20, [11.2]);
        AssertEquals(200, ChooseBackground(Id, Shape));
        AssertEquals(200, Fit(Id));
        AssertTrue(Format('a good fit, R %.4g', [RFactorOf(Id)]),
            RFactorOf(Id) < 1e-3);
        //  FALLING, as the tail does - not on the floor at 20: a decay with no
        //  level of its own sinks under it there, carried by the model's
        //  other freedoms, and that is the shape's limitation, not the fit's.
        Drawn := PointsOfType(Id, Shape);
        AssertTrue('it falls', Drawn.Y[0] > Drawn.Y[High(Drawn.Y)]);
    end;
end;

procedure TBackgroundModelFitTest.TheHighAngleTailOf1DatIsFittedByAParabolaOnItsFloor;
var
    Id: longint;
    Drawn: TPointsData;
begin
    //  2theta = 140 to 172: the baseline rises from about 775 to about 1150,
    //  under the peaks at 148.0 and 159.6 - and under a dozen small ones from
    //  154 on that the model is not given.
    Id := OneDatProblem(140, 172, [148.0, 159.6]);
    AssertEquals(200, ChooseBackground(Id,
        TQuadraticBackgroundPointsSet.GetCurveTypeId));
    AssertEquals(200, Fit(Id));
    AssertTrue(Format('a good fit, R %.4g', [RFactorOf(Id)]), RFactorOf(Id) < 1e-3);

    //  IT IS THE BASELINE: at both ends of the window, where there is no peak,
    //  it sits on the data's floor. Not everywhere - in the middle it also
    //  carries the small peaks nobody placed, which is what a least-squares
    //  background does with intensity the model has no curve for, and which
    //  its explanation says.
    Drawn := PointsOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('on the floor at 140', 775, Drawn.Y[0], 0.1 * 775);
    AssertEquals('and at 172', 1155, Drawn.Y[High(Drawn.Y)], 0.1 * 1155);
end;

procedure TBackgroundModelFitTest.APseudoVoigtModelWithABackgroundFits;
var
    Id, Code: longint;
    Msg: string;
begin
    //  The model with a SHARED parameter beside the background - the one the
    //  shared-parameter write used to crash on.
    Id := BackgroundProblem;
    AssertEquals(200, PutSettings(Id, Format('{"curveType":"%s"}',
        [GUIDToString(TPseudoVoigtPointsSet.GetCurveTypeId)]), Msg));
    Call('PUT', Format('/problems/%d/positions', [Id]), '{"x":[10],"y":[140]}',
        Code).Free;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('fitted', 200, Fit(Id));
    AssertTrue('and well: ' + FloatToStr(RFactorOf(Id)), RFactorOf(Id) < 0.01);
end;

procedure TBackgroundModelFitTest.TheDrawnCurvesSumToTheModel;
var
    Id, Code, i, j: longint;
    R: TJSONObject;
    A: TJSONArray;
    Body: string;
    Calc, One: TPointsData;
    Sum: array of double;
begin
    //  CURVE SCALING STAYS MODEL-WIDE: the drawn peaks and the drawn background
    //  are scaled by the same factor as the calculated profile, so what the
    //  user sees adds up to what was fitted.
    Id := BackgroundProblem;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals(200, Fit(Id));
    FApi.Handle('GET', Format('/problems/%d/calc-profile', [Id]), '', Code, Body);
    AssertTrue(PointsFromJsonString(Body, Calc));
    SetLength(Sum, Length(Calc.X));
    R := Curves(Id);
    try
        A := R.Arrays['curves'];
        for i := 0 to A.Count - 1 do
        begin
            FApi.Handle('GET', Format('/problems/%d/curves/%s/points',
                [Id, A.Objects[i].Get('id', '')]), '', Code, Body);
            AssertTrue(PointsFromJsonString(Body, One));
            AssertEquals('every curve spans the interval', Length(Calc.X), Length(One.X));
            for j := 0 to High(One.Y) do
                Sum[j] := Sum[j] + One.Y[j];
        end;
    finally
        R.Free;
    end;
    for j := 0 to High(Sum) do
        AssertEquals('sample ' + IntToStr(j), Calc.Y[j], Sum[j], 1e-6 * Max(1, Abs(Calc.Y[j])));
end;

procedure TBackgroundModelFitTest.ReducingCurvesNeverRemovesTheBackground;
var
    Id, Code: longint;
begin
    //  CURVE REDUCTION IS FOR PEAKS. Seeded with more peaks than the data has,
    //  it takes the surplus away - and never the background.
    Id := BackgroundProblem(True);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[4,8,10,12,16],"y":[50,70,140,70,70]}', Code).Free;
    ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId);
    Call('POST', Format('/problems/%d/actions/minimize-number-of-curves', [Id]),
        '', Code).Free;
    AssertEquals('reduced', 200, Code);
    AssertEquals('the background is still there', 1,
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    AssertTrue('and still last',
        CurveCount(Id) > CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
end;

{ ---- TBackgroundClientTest ---- }

procedure TBackgroundClientBase.GivenAModelWithTwoPeaks;
var
    Data: TPointsData;
    P, B: TTitlePointsSet;
    i: longint;
begin
    AssertTrue(PointsFromJsonString(SlopedPeakJson, Data));
    //  BY NAME: a new problem starts from the process-wide selection, which
    //  another test in this process may have moved.
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    P := TTitlePointsSet.Create(nil);
    try
        for i := 0 to High(Data.X) do
            P.AddNewPoint(Data.X[i], Data.Y[i]);
        FSvc.SetProfilePointsSet(P);
    finally
        P.Free;
    end;
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    //  Handed over: this setter frees its argument, as the engine's does.
    FSvc.SetRFactorBounds(B);
    FSvc.AddPointToCurvePositions(6, 60);
    FSvc.AddPointToCurvePositions(10, 140);
end;

procedure TBackgroundClientTest.DeletingTheBackgroundCurveRemovesTheElement;
var
    Ids: TCurveInstanceIdList;
    Said: string;
begin
    //  Model > Delete Curve on the background's row. A model whose type takes a
    //  background ALWAYS has one, so the deletion is refused in words - raised
    //  to the client, which shows it - and the model is left as it was.
    GivenAModelWithTwoPeaks;
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('two peaks and a background', 3, FSvc.GetCurveCount);
    Ids := FSvc.GetBackgroundCurveIds;
    AssertEquals(1, Length(Ids));

    Said := '';
    try
        FClient.DeleteCurve(Ids[0]);
    except
        on E: Exception do
            Said := E.Message;
    end;
    AssertTrue('refused, and it says why: ' + Said,
        Pos('background', LowerCase(Said)) > 0);
    AssertEquals('the model is unchanged', 3, FSvc.GetCurveCount);
    AssertEquals('and keeps its background',
        GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
        GUIDToString(FSvc.GetBackgroundCurveType));
end;

procedure TBackgroundClientTest.AndThePicksAreLeftAlone;
var
    Picks: TTitlePointsSet;
begin
    //  A refused deletion touches nothing, the picks included.
    GivenAModelWithTwoPeaks;
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    try
        FClient.DeleteCurve(FSvc.GetBackgroundCurveIds[0]);
    except
        on E: Exception do ;  //  the refusal is the previous test's subject
    end;
    Picks := FSvc.GetCurvePositions;
    try
        AssertEquals('both picks', 2, Picks.PointsCount);
    finally
        Picks.Free;
    end;
end;

procedure TBackgroundClientTest.ItStaysGoneAfterTheNextEdit;
begin
    //  What stays is the background: after a refused deletion and another
    //  pick the model holds three peaks and its one background, not two.
    GivenAModelWithTwoPeaks;
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    try
        FClient.DeleteCurve(FSvc.GetBackgroundCurveIds[0]);
    except
        on E: Exception do ;  //  refused; see DeletingTheBackgroundCurveRemovesTheElement
    end;
    FSvc.AddPointToCurvePositions(14, 60);
    AssertEquals('three peaks and the background', 4, FSvc.GetCurveCount);
end;

{ ---- the automatic run ---- }

function AutomaticRun(AApi: TFitRestApi; AId: longint; out AMessage: string): longint;
var
    Body: string;
    D: TJSONData;
begin
    AApi.Handle('POST', Format('/problems/%d/actions/do-all-automatically', [AId]),
        '', Result, Body);
    AMessage := '';
    D := GetJSON(Body);
    try
        if D is TJSONObject then
            AMessage := TJSONObject(D).Get('message', '');
    finally
        D.Free;
    end;
end;

{ A curved baseline under two peaks, and nothing placed - the problem the
  automatic run is for. }
function CurvedBaselineJson: string;
var
    P: TPointsData;
    x: double;
    n: longint;
begin
    P := Default(TPointsData);
    P.Title := 'profile';
    n := 0;
    x := 0;
    while x <= 30 + 1e-9 do
    begin
        SetLength(P.X, n + 1);
        SetLength(P.Y, n + 1);
        P.X[n] := x;
        //  A BOWL, lowest in the middle - the shape a diffractogram's
        //  background has and the one the background search is written for.
        P.Y[n] := 200 - 8 * x + 0.3 * Sqr(x) + GaussPoint(300, 1.5, 9, x) +
            GaussPoint(200, 1.8, 21, x);
        Inc(n);
        //  COARSE, as the live animation's automatic test is: the run seeds a
        //  curve on every point of every peak and removes them one by one, so
        //  the grid decides how long a test of it takes.
        x := x + 1.0;
    end;
    Result := PointsToJsonString(P);
end;

function TBackgroundModelFitTest.NewAutomaticProblem: longint;
var
    Code: longint;
begin
    Result := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Result]), CurvedBaselineJson,
        Code).Free;
end;

procedure TBackgroundModelFitTest.TheAutomaticRunLeavesTheProfileAsMeasured;
var
    Id, Code: longint;
    Before, After, Msg: string;
begin
    //  NOTHING IS TAKEN OUT OF THE DATA. The run used to subtract a background
    //  first - an edit of the measurement that had to be remembered, and was
    //  not, across a saved project.
    Id := NewAutomaticProblem;
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Before);
    AssertEquals('the run is accepted', 200, AutomaticRun(FApi, Id, Msg));
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, After);
    AssertEquals('the profile is the measurement', Before, After);
end;

{ 0..160 in steps of 0.5: a floor of 20 under a broad hump of 80 (sigma 14,
  at 30) and two narrow peaks of 300 and 200 at 60 and 110, with the noise of
  counts. The hump is what a Chebyshev of six terms over this range cannot
  follow and twelve can. }
function HumpOnAFloorJson: string;
var
    P: TPointsData;
    i, N: longint;
    x, y: double;
    Seed: longword;
begin
    N := 321;
    P := Default(TPointsData);
    SetLength(P.X, N);
    SetLength(P.Y, N);
    for i := 0 to N - 1 do
    begin
        x := i * 0.5;
        Seed := longword(i) * 2654435761 + 12345;
        Seed := Seed xor (Seed shr 13);
        Seed := Seed * 1274126177;
        y := 20 + 80 * Exp(-Sqr(x - 30) / (2 * Sqr(14.0))) +
            300 * Exp(-Sqr(x - 60) / (2 * Sqr(1.0))) +
            200 * Exp(-Sqr(x - 110) / (2 * Sqr(1.0)));
        P.X[i] := x;
        P.Y[i] := y + ((Seed mod 10000) / 10000 - 0.5) * Sqrt(12) * Sqrt(y);
    end;
    Result := PointsToJsonString(P);
end;

{ 0..160 in steps of 0.5: a floor of 20 under a hump of 60 (sigma 6, at 20) near
  the start of the pattern, as an amorphous halo is, and two narrow peaks at 60
  and 110. Twelve terms cannot follow it; thirty or so can. }
function EdgeHumpJson: string;
var
    P: TPointsData;
    i, N: longint;
    x, y: double;
    Seed: longword;
begin
    N := 321;
    P := Default(TPointsData);
    SetLength(P.X, N);
    SetLength(P.Y, N);
    for i := 0 to N - 1 do
    begin
        x := i * 0.5;
        Seed := longword(i) * 2654435761 + 12345;
        Seed := Seed xor (Seed shr 13);
        Seed := Seed * 1274126177;
        y := 20 + 60 * Exp(-Sqr(x - 20) / (2 * Sqr(6.0))) +
            300 * Exp(-Sqr(x - 60) / (2 * Sqr(1.0))) +
            200 * Exp(-Sqr(x - 110) / (2 * Sqr(1.0)));
        P.X[i] := x;
        P.Y[i] := y + ((Seed mod 10000) / 10000 - 0.5) * Sqrt(12) * Sqrt(y) * 0.3;
    end;
    Result := PointsToJsonString(P);
end;

procedure TBackgroundModelFitTest.FitIntervalsAutoFindsWhatTheAutomaticRunFinds;
var
    Id, Code: longint;
    Msg, ByHand, ByTheRun: string;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), EdgeHumpJson,
        Code).Free;
    Call('POST', Format('/problems/%d/actions/compute-curve-bounds', [Id]), '',
        Code).Free;
    AssertEquals('Fit Intervals > Auto', 200, Code);
    FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '', Code,
        ByHand);
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '', Code,
        ByTheRun);
    AssertEquals('the same intervals', ByTheRun, ByHand);
end;

procedure TBackgroundModelFitTest.TheCurvesOnAHumpAreNotKeptWhenTheBackgroundCanTakeIt;
var
    Id, Code, i, j, OnTheHump: longint;
    Msg: string;
    Drawn: TPointsData;
    Hump, Terms: double;
    Listed: TJSONObject;
    A, Params: TJSONArray;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), EdgeHumpJson,
        Code).Free;
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    Terms := ParamOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId, 'terms');
    AssertTrue(Format('the terms grew to what the hump needs (%g)', [Terms]),
        Terms >= 20);
    Drawn := PointsOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId);
    Hump := 0;
    for i := 0 to High(Drawn.X) do
        if Abs(Drawn.X[i] - 20) < 0.01 then
            Hump := Drawn.Y[i];
    AssertTrue(Format('the background follows the hump: %g at 20, of 80', [Hump]),
        Hump > 68);
    //  NO CURVE IS LEFT ON THE HUMP: what it holds is the background's.
    OnTheHump := 0;
    Listed := Curves(Id);
    try
        A := Listed.Arrays['curves'];
        for i := 0 to A.Count - 1 do
            if SameText(A.Objects[i].Get('curveType', ''),
                GUIDToString(TGaussPointsSet.GetCurveTypeId)) then
            begin
                Params := A.Objects[i].Arrays['params'];
                for j := 0 to Params.Count - 1 do
                    if (Params.Objects[j].Get('name', '') = 'x0') and
                       (Params.Objects[j].Get('value', 1000.0) < 36) then
                        Inc(OnTheHump);
            end;
    finally
        Listed.Free;
    end;
    AssertTrue(Format('no more than a stray curve on the hump (%d)', [OnTheHump]),
        OnTheHump <= 2);
end;

procedure TBackgroundModelFitTest.TheAutomaticRunFollowsAHumpSixTermsCannotFollow;
var
    Id, Code, i, Peaks: longint;
    Msg: string;
    Drawn: TPointsData;
    Hump: double;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), HumpOnAFloorJson,
        Code).Free;
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    //  AT LEAST THE START: tuning may add terms, never takes the run below
    //  twelve on a pattern of this size without BIC saying so, and the hump
    //  needs them.
    AssertTrue(Format('the background has the terms a hump needs (%g)',
        [ParamOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId, 'terms')]),
        ParamOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId,
            'terms') >= 10);
    Drawn := PointsOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId);
    Hump := 0;
    for i := 0 to High(Drawn.X) do
        if Abs(Drawn.X[i] - 30) < 0.01 then
            Hump := Drawn.Y[i];
    AssertTrue(Format('the background follows the hump: %g at 30, of 100', [Hump]),
        Hump > 85);
    Peaks := CountOfType(Id, TGaussPointsSet.GetCurveTypeId);
    AssertTrue(Format('the hump did not become peaks (%d)', [Peaks]), Peaks <= 8);
end;

procedure TBackgroundModelFitTest.TheAutomaticRunDropsTheTermsAFlatFloorDoesNotNeed;
var
    Id, Code: longint;
    Msg, Titles: string;
    Terms: double;
begin
    //  A flat floor under two peaks: twelve terms start, and BIC takes the
    //  ones that fit only noise away, turn by turn, saying so in the Run Log.
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    Call('PUT', Format('/problems/%d/profile', [Id]), PeaksOnANoisyFloorJson,
        Code).Free;
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    Terms := ParamOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId,
        'terms');
    AssertTrue(Format('fewer terms than the twelve it started with (%g)', [Terms]),
        Terms < 12);
    Titles := RunLogTitles(FApi, Id);
    AssertTrue('the Run Log says so: ' + Titles,
        Pos('Background went from', Titles) > 0);
end;

procedure TBackgroundModelFitTest.TheAutomaticRunFitsTheModelsBackground;
var
    Id, Peaks: longint;
    Msg: string;
begin
    //  DECIDED WITH THE USER (2026-10-31): a model of peaks always has one
    //  background curve, the Chebyshev unless another is chosen, and the
    //  automatic run fits it - it used to add a quadratic to a model with none.
    Id := NewAutomaticProblem;
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    AssertEquals('one Chebyshev background under the peaks', 1,
        CountOfType(Id, TChebyshevBackgroundPointsSet.GetCurveTypeId));
    Peaks := CountOfType(Id, TGaussPointsSet.GetCurveTypeId);
    //  THE BASELINE DID NOT BECOME PEAKS - the failure the old subtraction
    //  guard existed for. Two peaks in the data; a few curves for them.
    AssertTrue(Format('a few peaks, not one per sample (%d)', [Peaks]),
        (Peaks >= 2) and (Peaks <= 8));
end;

procedure TBackgroundModelFitTest.AnAutomaticRunSaysItReplacedTheIntervalsTheUserSet;
var
    Id, Code: longint;
    Msg: string;
begin
    Id := NewAutomaticProblem;
    //  NARROWER THAN THE RUN'S OWN, which on this profile is the whole of it:
    //  an interval the run does not replace has nothing to report.
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        '{"x":[5,25],"y":[0,0]}', Code).Free;
    AssertEquals('the interval is accepted', 200, Code);
    AssertEquals('the run is accepted', 200, AutomaticRun(FApi, Id, Msg));
    AssertTrue('it says what it replaced: ' + Msg,
        Pos('Fit > Automatically replaced the fit intervals (x = 5 to 25)',
        Msg) > 0);
end;

procedure TBackgroundModelFitTest.AndSaysNothingOfIntervalsWhenThereWereNone;
var
    Id: longint;
    Msg: string;
begin
    Id := NewAutomaticProblem;
    AssertEquals('the run is accepted', 200, AutomaticRun(FApi, Id, Msg));
    AssertEquals('nothing of intervals: ' + Msg, 0, Pos('fit intervals', Msg));
end;

procedure TBackgroundModelFitTest.AnAutomaticRunKeepsTheBackgroundTheUserChose;
var
    Id: longint;
    Msg: string;
begin
    Id := NewAutomaticProblem;
    AssertEquals(200, ChooseBackground(Id, TLinearBackgroundPointsSet.GetCurveTypeId));
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    AssertTrue('the user''s line is fitted',
        CountOfType(Id, TLinearBackgroundPointsSet.GetCurveTypeId) >= 1);
    AssertEquals('and no second background is added', 0,
        CountOfType(Id, TQuadraticBackgroundPointsSet.GetCurveTypeId));
end;

procedure TBackgroundModelFitTest.AnAutomaticRunAfterAManualSubtractionDoesNotSubtractAgain;
var
    Id, Code: longint;
    Msg, Subtracted, After: string;
begin
    //  THE USER'S OWN EDIT OF THEIR DATA STANDS, and nothing is taken off it a
    //  second time - the fault that a saved project used to bring back.
    Id := NewAutomaticProblem;
    Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code).Free;
    AssertEquals(200, Code);
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Subtracted);
    AssertEquals(200, AutomaticRun(FApi, Id, Msg));
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, After);
    AssertEquals('the subtracted profile, not subtracted again', Subtracted, After);
end;

procedure TBackgroundClientTest.ChoosingABackgroundThroughTheClientBuildsIt;
begin
    GivenAModelWithTwoPeaks;
    FClient.BackgroundCurveType := TLinearBackgroundPointsSet.GetCurveTypeId;
    AssertEquals('two peaks and a background on the server', 3, FSvc.GetCurveCount);
end;

procedure TBackgroundClientTest.TheClientReportsTheBackgroundTheModelHas;
begin
    GivenAModelWithTwoPeaks;
    //  A model whose type takes a background has the registry's default from
    //  the start, so the client reports it, not "none".
    AssertEquals('the default shape from the start', GUIDToString(
        FSvc.GetBackgroundCurveType), GUIDToString(FClient.BackgroundCurveType));
    AssertTrue('and it is a background', not IsEqualGUID(
        FClient.BackgroundCurveType, GUID_NULL));
    FSvc.SetBackgroundCurveType(TQuadraticBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('the server''s, read back',
        GUIDToString(TQuadraticBackgroundPointsSet.GetCurveTypeId),
        GUIDToString(FClient.BackgroundCurveType));
end;

procedure TBackgroundClientTest.AndItIsRedrawn;
begin
    //  The model changed; the chart has to say so without another click.
    GivenAModelWithTwoPeaks;
    FView.Log.Clear;
    FClient.BackgroundCurveType := TLinearBackgroundPointsSet.GetCurveTypeId;
    AssertTrue('the curves were plotted again', FView.Plotted('PlotCurves'));
end;

procedure TBackgroundTaskTest.WithoutABackgroundTheReductionMeasuresAsBefore;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId, GUID_NULL, [6, 10]);
    FTask.ComputeProfile;
    AssertEquals('the same figure', FTask.ProbeRFactor,
        FTask.ProbeReductionRFactor, 1e-15);
end;

procedure TBackgroundTaskTest.WithABackgroundItMeasuresThePeaksAboveIt;
var
    i: longint;
    Back: TNamedPointsSet;
    Obs, Calc, Scale, SumSq, SumAbove: double;
    Profile: TPointsSet;
begin
    //  THE CEILING (Fit > Set Max Acceptable Difference) WAS TUNED FOR A
    //  SUBTRACTED PROFILE. The R-factor divides by the data, and with the
    //  baseline left in the data a whole peak can go without the figure passing
    //  it. So a reduction with a background curve in the model measures over
    //  the signal ABOVE the fitted background: what it measured when the
    //  background had been subtracted.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    FTask.ComputeProfile;
    Back := LastCurve;
    Profile := FTask.ProfilePoints;
    Scale := FTask.GetScalingFactor;
    SumSq := 0;
    SumAbove := 0;
    for i := 0 to Profile.PointsCount - 1 do
    begin
        Obs := Profile.PointYCoord[i];
        Calc := FTask.GetCalcProfile.PointYCoord[i] * Scale;
        SumSq := SumSq + Sqr(Calc - Obs);
        SumAbove := SumAbove + Obs - Back.PointYCoord[i] * Scale;
    end;
    AssertEquals('over the peaks alone', SumSq / Sqr(SumAbove),
        FTask.ProbeReductionRFactor, 1e-12 * SumSq / Sqr(SumAbove));
    AssertTrue('which is stricter than the figure reported',
        FTask.ProbeReductionRFactor > FTask.ProbeRFactor);
end;

procedure TBackgroundTaskTest.WhileDecomposingTheBackgroundHoldsItsSeed;
begin
    //  A REDUCTION THE USER STARTS with a background curve in the model: the
    //  background stays at the seed it took from the data's baseline, and only
    //  the peaks move. Left free, it traded intensity with every peak the
    //  reduction tried to remove, and the removal was rolled back.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    //  Two Gaussians of three parameters each.
    AssertEquals('the peaks alone', 6, FTask.ProbeParametersWalked(True));
end;

procedure TBackgroundTaskTest.AndTheFinalFitVariesItWithThePeaks;
begin
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, [6, 10]);
    //  ...and the line's level and slope.
    AssertEquals('the peaks and the background', 8,
        FTask.ProbeParametersWalked(False));
end;

procedure TBackgroundTaskTest.ABackgroundAloneIsVariedEvenWhileDecomposing;
begin
    //  Nothing else to vary: holding it would leave the optimiser nothing.
    BuildTask(TGaussPointsSet.GetCurveTypeId,
        TLinearBackgroundPointsSet.GetCurveTypeId, []);
    AssertEquals('the background', 2, FTask.ProbeParametersWalked(True));
end;

procedure TBackgroundClientTest.EachCurvesTypeIsReadOverTheWire;
begin
    GivenAModelWithTwoPeaks;
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    AssertEquals('a peak', GUIDToString(TGaussPointsSet.GetCurveTypeId),
        GUIDToString(FSvc.GetCurveTypeOf(0)));
    AssertEquals('the background, last',
        GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
        GUIDToString(FSvc.GetCurveTypeOf(FSvc.GetCurveCount - 1)));
    AssertEquals('nothing past the end', GUIDToString(GUID_NULL),
        GUIDToString(FSvc.GetCurveTypeOf(FSvc.GetCurveCount)));
end;

procedure TBackgroundClientTest.TheBackgroundSurvivesSaveAndOpenOverTheWire;
var
    Values: TCurveValuesList;
    Doc: TProjectDocument;
    OtherApi: TFitRestApi;
    Other: TLoopbackFitService;
    Fault, Nm: string;
    Back, i, T: longint;
    V, Slope: double;
begin
    GivenAModelWithTwoPeaks;
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    Back := FSvc.GetCurveCount - 1;
    SetLength(Values, 1);
    Values[0].CurveIndex := Back;
    Values[0].Fitted := True;
    SetLength(Values[0].Params, 1);
    Values[0].Params[0].Name := 'b1';
    Values[0].Params[0].Value := 0.321;
    Values[0].Params[0].Error := -1;
    FSvc.SetCurveValues(Values);

    Doc := CaptureProject(FSvc, EmptyProjectClientContext, EmptyProjectDocument);

    OtherApi := TFitRestApi.Create;
    Other := TLoopbackFitService.Create(OtherApi);
    try
        AssertTrue('reopened: ' + Fault, ApplyProject(Other, Doc, Fault));
        AssertEquals('the background type',
            GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            GUIDToString(Other.GetBackgroundCurveType));
        Back := Other.GetCurveCount - 1;
        AssertEquals('as the last curve',
            GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            GUIDToString(Other.GetCurveTypeOf(Back)));
        Slope := NaN;
        for i := 0 to Other.GetCurveParameterCount(Back) - 1 do
        begin
            Other.GetCurveParameter(Back, i, Nm, V, T);
            if Nm = 'b1' then
                Slope := V;
        end;
        AssertEquals('with its fitted slope', 0.321, Slope, 1e-9);
        AssertTrue('and still marked fitted', Other.IsCurveFitted(Back));
    finally
        Other.Free;
        OtherApi.Free;
    end;
end;

{ ---- the automatic run's note ---- }

procedure TSynchronousClient.RunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    AOp;
    ADone;
end;

procedure TBackgroundClientTest.TheAutomaticRunsNoteReachesTheClient;
var
    Client: TSynchronousClient;
    Narrow: TTitlePointsSet;
begin
    //  THE GAP THIS CLOSES: the server said it, the REST reply carried it, and
    //  the client dropped the text every operation returns - so what the run
    //  said about itself never reached the screen, with every test green.
    //  The interval set here is narrower than the run's own, which replaces it
    //  and says so.
    GivenAModelWithTwoPeaks;
    Narrow := TTitlePointsSet.Create(nil);
    Narrow.AddNewPoint(5, 0);
    Narrow.AddNewPoint(15, 0);
    FSvc.SetRFactorBounds(Narrow);
    Client := TSynchronousClient.CreateWithInjector(nil);
    try
        Client.FitService := FSvc;
        Client.FFitViewer := FView;
        Client.DoAllAutomatically;
        AssertTrue('the note: ' + Client.OperationNote,
            Pos('replaced the fit intervals', Client.OperationNote) > 0);
    finally
        Client.FFitViewer := nil;
        Client.FitService := nil;
        Client.Free;
    end;
end;

procedure TBackgroundClientTest.TheStatusLineShowsTheNoteAndOtherwiseTheUsualHint;
begin
    AssertEquals('the note when there is one', 'Background handled.',
        FinishedStatusText('Ready.', 'Background handled.'));
    AssertEquals('the usual hint when there is none', 'Ready.',
        FinishedStatusText('Ready.', ''));
    AssertEquals('whitespace is no note', 'Ready.',
        FinishedStatusText('Ready.', '   '));
end;

procedure TBackgroundClientTest.ThenTheSamplesNoCurveCoversBeforeTheUsualHint;
var
    S: TFitStatistics;
begin
    S := EmptyFitStatistics;
    S.UncoveredRanges := UncoveredRanges([0, 1], [False, True]);
    AssertTrue('the samples when there are any',
        Pos('covered by no curve', FinishedStatusText('Ready.', '', S)) > 0);
    AssertEquals('the run''s own note still first', 'Background handled.',
        FinishedStatusText('Ready.', 'Background handled.', S));
    AssertEquals('and the usual hint when neither', 'Ready.',
        FinishedStatusText('Ready.', '', EmptyFitStatistics));
end;

procedure TBackgroundClientTest.TheAttributesSayWhichTypeEachCurveIs;
var
    Attrs: TMSCRCurveList;
begin
    GivenAModelWithTwoPeaks;
    FSvc.SetBackgroundCurveType(TLinearBackgroundPointsSet.GetCurveTypeId);
    Attrs := FSvc.GetCurveAttributes;
    try
        AssertEquals('a peak', GUIDToString(TGaussPointsSet.GetCurveTypeId),
            GUIDToString(Curve_parameters(Attrs.Items[0]).FCurveTypeId));
        AssertEquals('the background',
            GUIDToString(TLinearBackgroundPointsSet.GetCurveTypeId),
            GUIDToString(Curve_parameters(Attrs.Items[Attrs.Count - 1]).FCurveTypeId));
    finally
        Attrs.Free;
    end;
end;

procedure TBackgroundModelRestTest.ABackgroundCurveIsNeverDrawnBelowZero;
var
    Id, i, Code: longint;
    Drawn: TPointsData;
    Lowest, Xr, AtXr: double;
    Quadratic: TGuid;
begin
    //  A background is a count. Typed into the table as a parabola that dips
    //  13 below zero, it is drawn - and sent to the chart - lifted until its
    //  lowest point touches zero, and the level the table shows is the level
    //  that is drawn.
    Id := BackgroundProblem;
    //  Without the model-wide scaling factor, which multiplies what is drawn
    //  and not the coefficients (see ABackgroundCurveIsFittedWithThePeaks), so
    //  the level in the table and the level on the chart are the same number.
    Call('PUT', Format('/problems/%d/settings', [Id]), '{"curveScaling":false}',
        Code).Free;
    AssertEquals('scaling is switched off', 200, Code);
    Quadratic := TQuadraticBackgroundPointsSet.GetCurveTypeId;
    AssertEquals(200, ChooseBackground(Id, Quadratic));
    //  The reference at the first sample, whatever the seed chose: the
    //  parabola below dips under zero 4 to the right of it.
    Drawn := PointsOfType(Id, Quadratic);
    AssertEquals(200, PutParamOfType(Id, Quadratic, 'xr', Drawn.X[0]));
    AssertEquals(200, PutParamOfType(Id, Quadratic, 'b2', 0.5));
    AssertEquals(200, PutParamOfType(Id, Quadratic, 'b1', -4));
    AssertEquals(200, PutParamOfType(Id, Quadratic, 'b0', -5));

    Drawn := PointsOfType(Id, Quadratic);
    AssertTrue('the background has points', Length(Drawn.Y) > 0);
    Lowest := Infinity;
    for i := 0 to High(Drawn.Y) do
        Lowest := Min(Lowest, Drawn.Y[i]);
    AssertEquals('its lowest point touches zero', 0, Lowest, 1e-6);

    Xr := ParamOfType(Id, Quadratic, 'xr');
    AtXr := NaN;
    for i := 0 to High(Drawn.X) do
        if Abs(Drawn.X[i] - Xr) < 1e-9 then
            AtXr := Drawn.Y[i];
    AssertFalse('the reference is a sample', IsNan(AtXr));
    AssertEquals('the level shown is the level drawn',
        ParamOfType(Id, Quadratic, 'b0'), AtXr, 1e-6);
end;

function TBackgroundRestBase.PutGaussSigma(AId: longint;
    AValue: double): TJSONObject;
var
    R: TJSONObject;
    A, Params: TJSONArray;
    j, Index, Code: longint;
    Handle: string;
begin
    R := Curves(AId);
    try
        A := R.Arrays['curves'];
        Handle := A.Objects[0].Get('id', '');
        Params := A.Objects[0].Arrays['params'];
        Index := -1;
        for j := 0 to Params.Count - 1 do
            if Params.Objects[j].Get('name', '') = 'sigma' then
                Index := j;
    finally
        R.Free;
    end;
    Result := Call('PUT', Format('/problems/%d/curves/%s/params/%d',
        [AId, Handle, Index]), Format('{"value":%s}', [FloatToStr(AValue)]), Code);
    AssertEquals('the write is accepted', 200, Code);
end;

procedure TBackgroundModelRestTest.AWriteTheModelHoldsOtherwiseIsAnsweredWithTheValueAndWhy;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    Id := BackgroundProblemWithBounds([4, 14]);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[10],"y":[140]}', Code).Free;
    R := PutGaussSigma(Id, 50);
    try
        AssertEquals('the value held', 10 / (2 * Sqrt(2 * Ln(2))),
            R.Get('value', 0.0), 1e-9);
        AssertTrue('and why: ' + R.AsJSON,
            Pos('Limits on parameter values', R.Get('note', '')) > 0);
    finally
        R.Free;
    end;
end;

procedure TBackgroundModelRestTest.AWriteHeldAsWrittenIsAnsweredWithTheValueAlone;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    Id := BackgroundProblemWithBounds([4, 14]);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[10],"y":[140]}', Code).Free;
    R := PutGaussSigma(Id, 1.5);
    try
        AssertEquals(1.5, R.Get('value', 0.0), 1e-9);
        AssertNull('no note: ' + R.AsJSON, R.Find('note'));
    finally
        R.Free;
    end;
end;

procedure TBackgroundModelRestTest.APeakIsNeverWiderThanItsFitInterval;
var
    Id, Code: longint;
begin
    //  A width wider than the interval it is fitted in is a hump under the
    //  whole interval, not a peak. The interval here is 4 .. 14.
    Id := BackgroundProblemWithBounds([4, 14]);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[10],"y":[140]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
    AssertEquals(200, PutParamOfType(Id, TGaussPointsSet.GetCurveTypeId,
        'sigma', 50));
    //  A Gaussian's sigma is a standard deviation, so it is held where the
    //  peak is the interval's 10 wide at half maximum: 10 / 2.3548.
    AssertEquals('held where the peak is as wide as the interval',
        10 / (2 * Sqrt(2 * Ln(2))),
        ParamOfType(Id, TGaussPointsSet.GetCurveTypeId, 'sigma'), 1e-9);
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(TTestMarkupPointsSet);
    RegisterCurveBuilder(TestMarkupSet, @BuildTestMarkup);
    RegisterTest('unit', TBackgroundServiceTest);
    RegisterTest('unit', TRefusedRunTest);
    RegisterTest('unit', TBackgroundClientTest);
    RegisterTest('unit', TBackgroundTaskTest);
    RegisterTest('integration', TBackgroundModelFitTest);
    RegisterTest('unit', TBackgroundSubtractionPinTest);
    RegisterTest('unit', TBackgroundSubtractionClientTest);
    RegisterTest('unit', TBackgroundModelRestTest);
end.
