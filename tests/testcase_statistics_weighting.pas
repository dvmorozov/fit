// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the fit's figures are weighted, through the route the window
reads.)

A MODEL WHOSE DATA ARE NOT COUNTS. The reduced chi-squared, AIC and BIC divide
each difference by 1/sqrt(value) - counting statistics - which is right for a
diffraction profile and meaningless for a price. A curve type says its data are
not counts by naming a noise rule that measures (TNamedPointsSet.NoiseRule),
and GET /stats then weighs every difference by that noise, says so in its
"weighting" field, and scores AIC and BIC in the known-variance forms a
reduction by BIC compares.

THE RED TEST ENTERS WHERE THE APPLICATION DOES: the curve type is chosen, a
curve picked and fitted through the REST verbs, and the figures read from
/stats - the reply the status bar is filled from. The type is the test's own, a
Gaussian with a noise of its choosing, because the framework ships no type
whose data are not counts and a module's must not be named here. The REST
tests are integration tests: they run the optimiser.

AND THE STRIP IS JUDGED BY THE SAME NOISE: where the difference is not noise -
the shaded stretches /stats names, and the Difference structure strip GET
/difference-structure sends - is measured against the noise the type measures,
over the same fit window, both computed by the server.
}
unit testcase_statistics_weighting;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    test_curve_types, named_points_set, fit_reduction, curve_types_singleton,
    int_curve_factory, fit_statistics, fit_points_json,
    difference_structure, testcase_rest_api,
    fit_window,
    mscr_specimen_list, persistent_curve_parameters, noise_rule,
    test_noise_rules, residual_randomness, sample_coverage;

type
    { The rule of the Gaussian below: a constant noise of its Variance. }
    TMeasuredNoiseRule = class(TTestNoiseRule)
    public
        class function Variances(const AY: array of double): TNoiseVariances;
            override;
    end;

    { A Gaussian whose data are not counts: a constant noise of Variance.
      Registered for the whole test process, so it inherits a complete
      explanation from the Gaussian rather than writing one of its own. }
    TMeasuredNoiseGaussPointsSet = class(TTestGaussPointsSet)
    public
        { The one-argument constructor the engine builds a registered type
          with, building the Gaussian's parameters. }
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function NoiseRule: TNoiseRuleClass; override;
    public
        { The noise every point has; each test sets its own. }
        class var Variance: double;
    end;

    TStatisticsWeightingRestTest = class(TRestApiTestBase)
    private
        { A problem whose one curve, of ATypeId, has been fitted. }
        function FittedProblemOf(const ATypeId: TGuid): longint;
        { The statistics /stats sends for problem AId; the caller frees. }
        function StatisticsOf(AId: longint): TJSONObject;
        { A problem whose one curve of ATypeId, picked at 100, has been
          fitted to the ripple profile inside the fit intervals ABoundsX. }
        function RippleFittedOf(const ATypeId: TGuid;
            const ABoundsX: array of double): longint;
        { The strip GET /difference-structure sends for problem AId. }
        function StructureOf(AId: longint): TDifferenceStructure;
        { The y of point set ASet of problem AId, over the wire. }
        function ValuesOf(AId: longint; const ASet: string): TDoubleArray;
    published
        procedure AModelThatMeasuresItsNoiseIsScoredByItOverRest;
        procedure AModelOfCountsIsScoredAsItAlwaysWas;
        procedure AReductionByBicWeighsTheNoiseTheModelMeasures;
        procedure WhereTheDifferenceIsNotNoiseIsJudgedByTheTypesNoiseOverRest;
        //  THE STRIP UNDER THE CHART IS THE SERVER'S, over the same window
        //  and noise as the statistics (fit_service_statistics).
        procedure TheStripJudgesTheDifferenceAgainstTheNoiseTheTypeMeasuresOverRest;
        procedure TheStripOfAModelOfCountsIsJudgedByTheNoiseOfCountsOverRest;
        procedure TheStripReachesItsLevelWhereTheStretchesAreOverRest;
        procedure TheStripCoversOnlyTheFitIntervalsOverRest;
        procedure NothingFittedSendsAnEmptyStripOverRest;
    end;

implementation

const
    MeasuredNoiseGaussId: TGuid = '{9A4C2E71-5B3D-4F08-8C6A-1E7D3B5F2A94}';
    { The ripple profile's samples. }
    RippleSamples = 200;

{ A peak of 100 at x = 100 - or, with ARipple, the same with a ripple of
  amplitude 3 and period 10 on it, which a single Gaussian cannot follow. }
function RippleAt(AX: double; ARipple: boolean): double;
begin
    Result := 100 * Exp(-Sqr(AX - 100) / (2 * Sqr(15)));
    if ARipple then
        Result := Result + 3 * Sin(2 * Pi * AX / 10);
end;

{ The profile of RippleAt at every sample, as a points reply. }
function RippleProfileJson(ARipple: boolean): string;
var
    P: TPointsData;
    i: longint;
begin
    P := Default(TPointsData);
    SetLength(P.X, RippleSamples);
    SetLength(P.Y, RippleSamples);
    for i := 0 to RippleSamples - 1 do
    begin
        P.X[i] := i;
        P.Y[i] := RippleAt(i, ARipple);
    end;
    Result := PointsToJsonString(P);
end;

constructor TMeasuredNoiseGaussPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner, 0);
end;

class function TMeasuredNoiseGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian of data that are not counts (test)';
end;

class function TMeasuredNoiseGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := MeasuredNoiseGaussId;
end;

class function TMeasuredNoiseGaussPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TMeasuredNoiseRule;
end;

class function TMeasuredNoiseRule.Variances(
    const AY: array of double): TNoiseVariances;
var
    i: longint;
begin
    SetLength(Result, Length(AY));
    for i := 0 to High(Result) do
        Result[i] := TMeasuredNoiseGaussPointsSet.Variance;
end;

function TStatisticsWeightingRestTest.FittedProblemOf(
    const ATypeId: TGuid): longint;
var
    Code: longint;
begin
    Result := ProblemReadyForPicks;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(ATypeId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Result]),
        '{"x":[10],"y":[100]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Result]),
        '', Code).Free;
    AssertEquals('fitted', 200, Code);
end;

function TStatisticsWeightingRestTest.StatisticsOf(AId: longint): TJSONObject;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/stats', [AId]), '', Code);
    try
        AssertEquals('stats answer', 200, Code);
        AssertTrue('statistics: ' + R.AsJSON, R.Find('statistics') is TJSONObject);
        Result := TJSONObject(R.Extract('statistics'));
    finally
        R.Free;
    end;
    AssertTrue('valid: ' + Result.AsJSON, Result.Get('valid', False));
end;

procedure TStatisticsWeightingRestTest.AModelThatMeasuresItsNoiseIsScoredByItOverRest;
var
    Id, n, k: longint;
    S: TJSONObject;
    AtOne, AtFour: double;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    Id := FittedProblemOf(MeasuredNoiseGaussId);
    S := StatisticsOf(Id);
    try
        AssertEquals('says how it was weighted', 'measured-noise',
            S.Get('weighting', ''));
        AtOne := S.Get('chiSquare', 0.0);
        n := S.Get('dataPoints', 0);
        k := S.Get('params', 0);
        AssertTrue('there is a difference to weigh: ' + S.AsJSON, AtOne > 0);
        //  THE KNOWN-VARIANCE FORMS, the score a reduction by BIC compares.
        AssertEquals('BIC is the reduction''s', ReductionBic(AtOne, n, k),
            S.Get('bic', 0.0), 1e-9 * Max(1, Abs(AtOne)));
        AssertEquals('AIC is chi2 + 2k', AtOne + 2 * k, S.Get('aic', 0.0),
            1e-9 * Max(1, Abs(AtOne)));
    finally
        S.Free;
    end;
    //  The same model over a noise four times as large: every squared
    //  difference over 4 - so it is THE TYPE'S noise the figures use.
    TMeasuredNoiseGaussPointsSet.Variance := 4;
    S := StatisticsOf(Id);
    try
        AtFour := S.Get('chiSquare', 0.0);
    finally
        S.Free;
    end;
    AssertEquals('each difference over the type''s noise', AtOne / 4, AtFour,
        1e-9 * AtOne);
end;

procedure TStatisticsWeightingRestTest.AModelOfCountsIsScoredAsItAlwaysWas;
var
    Id, n, k: longint;
    S: TJSONObject;
    Chi2: double;
begin
    //  ADDITIVE: a Gaussian's figures are weighted as counts, lmfit's forms,
    //  and the reply carries no field it did not carry before.
    Id := FittedProblemOf(TTestGaussPointsSet.GetCurveTypeId);
    S := StatisticsOf(Id);
    try
        AssertNull('no weighting field for counts: ' + S.AsJSON,
            S.Find('weighting'));
        Chi2 := Max(S.Get('chiSquare', 0.0), 1e-250);
        n := S.Get('dataPoints', 0);
        k := S.Get('params', 0);
        AssertEquals('BIC as lmfit reports it', n * Ln(Chi2 / n) + Ln(n) * k,
            S.Get('bic', 0.0), 1e-6 * Max(1, Abs(S.Get('bic', 0.0))));
    finally
        S.Free;
    end;
end;

{ THE BIC A REDUCTION COMPARES IS THE BIC THE WINDOW SHOWS: both divide each
  squared difference by the noise the model's curve types measure, or the run
  would choose its curves by one score and report another. Entered as the user
  does - Fit > Minimize Number of Curves by BIC over REST, a curve on every
  sample of one clean peak - and read back from the run log, where every
  removal BIC favoured states the score with the curve.

  A NOISE SO LARGE THAT NO DIFFERENCE COUNTS: with a variance of 1e12 and
  values near 100, chi2 is below a millionth, so every score is the parameters'
  cost alone, k ln n - a whole multiple of ln 21 to the log's one decimal.
  Weighed by the noise fit_reduction measures from these noiseless data
  instead, chi2 is in the thousands and the scores fall anywhere. }
procedure TStatisticsWeightingRestTest.AReductionByBicWeighsTheNoiseTheModelMeasures;
const
    Samples = 21;
var
    Id, Code, i, Scores, At, From: longint;
    R: TJSONObject;
    Events: TJSONArray;
    Detail: string;
    Score, Multiple: double;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1e12;
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","reductionCriterion":"bic"}',
        [GUIDToString(MeasuredNoiseGaussId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/profile', [Id]),
        CoarseGaussianProfileJson, Code).Free;
    Call('POST',
        Format('/problems/%d/actions/select-all-points-as-curve-positions',
        [Id]), '', Code).Free;
    Call('POST',
        Format('/problems/%d/actions/minimize-number-of-curves', [Id]), '',
        Code).Free;
    AssertEquals('the reduction completes', 200, Code);

    R := Call('GET', Format('/problems/%d/progress?since=0', [Id]), '', Code);
    try
        Events := TJSONArray(R.Find('events'));
        AssertTrue('the run said something: ' + R.AsJSON, Assigned(Events));
        Scores := 0;
        for i := 0 to Events.Count - 1 do
        begin
            Detail := Events.Objects[i].Get('detail', '');
            At := Pos(' with it', Detail);
            if At = 0 then
                Continue;
            //  The score is the number just before " with it", in both the
            //  removed and the kept-as-a-pattern wording.
            From := At;
            while (From > 1) and (Detail[From - 1] in ['0'..'9', '.', ',', '-']) do
                Dec(From);
            //  Read with the settings it was written with (run_event_text's
            //  Format), whatever the decimal separator.
            Score := StrToFloat(Copy(Detail, From, At - From));
            Multiple := Score / Ln(Samples);
            AssertTrue(Format('a score of the parameters alone: %s (%.3f ln n)',
                [Detail, Multiple]), Abs(Multiple - Round(Multiple)) < 0.03);
            Inc(Scores);
        end;
        AssertTrue('the reduction removed curves and said by what score: ' +
            Events.AsJSON, Scores > 0);
    finally
        R.Free;
    end;
end;

{ WHERE THE DIFFERENCE IS NOT NOISE IS JUDGED BY THE NOISE THE TYPE MEASURES,
  as the figures are: the stretches /stats names follow the type's noise. A
  ripple of amplitude 3 that one Gaussian cannot follow is named against a noise
  of variance one, and is nothing against a noise of variance a million. Judged
  by the noise of counts whatever the type said, both answers were the same -
  and a price, whose noise grows with its level, had its quiet years named as
  structure and its loud ones read as noise. }
procedure TStatisticsWeightingRestTest.WhereTheDifferenceIsNotNoiseIsJudgedByTheTypesNoiseOverRest;
var
    Id, Code: longint;
    S: TJSONObject;
begin
    Id := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(MeasuredNoiseGaussId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/profile', [Id]), RippleProfileJson(True),
        Code).Free;
    AssertEquals('the profile is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        Format('{"x":[0,%d],"y":[0,0]}', [RippleSamples - 1]), Code).Free;
    AssertEquals('the interval is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[100],"y":[100]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
        '', Code).Free;
    AssertEquals('fitted', 200, Code);

    TMeasuredNoiseGaussPointsSet.Variance := 1;
    S := StatisticsOf(Id);
    try
        AssertTrue('the ripple is named against a noise of one: ' + S.AsJSON,
            (S.Find('nonRandomRanges') is TJSONArray) and
            (S.Arrays['nonRandomRanges'].Count > 0));
    finally
        S.Free;
    end;
    TMeasuredNoiseGaussPointsSet.Variance := 1e6;
    S := StatisticsOf(Id);
    try
        AssertTrue('judged: ' + S.AsJSON, S.Find('nonRandomRanges') is TJSONArray);
        AssertEquals('and is nothing against a noise of a million: ' + S.AsJSON,
            0, S.Arrays['nonRandomRanges'].Count);
    finally
        S.Free;
    end;
end;

function TStatisticsWeightingRestTest.RippleFittedOf(const ATypeId: TGuid;
    const ABoundsX: array of double): longint;
var
    Code, i: longint;
    Xs, Ys: string;
begin
    Result := NewProblem;
    Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(ATypeId)]), Code).Free;
    AssertEquals('the type is accepted', 200, Code);
    Call('PUT', Format('/problems/%d/profile', [Result]),
        RippleProfileJson(True), Code).Free;
    AssertEquals('the profile is accepted', 200, Code);
    Xs := '';
    Ys := '';
    for i := 0 to High(ABoundsX) do
    begin
        if i > 0 then
        begin
            Xs := Xs + ',';
            Ys := Ys + ',';
        end;
        Xs := Xs + IntToStr(Round(ABoundsX[i]));
        Ys := Ys + '0';
    end;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        Format('{"x":[%s],"y":[%s]}', [Xs, Ys]), Code).Free;
    AssertEquals('the intervals are accepted', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Result]),
        '{"x":[100],"y":[100]}', Code).Free;
    AssertEquals('the pick is accepted', 200, Code);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Result]),
        '', Code).Free;
    AssertEquals('fitted', 200, Code);
end;

function TStatisticsWeightingRestTest.StructureOf(
    AId: longint): TDifferenceStructure;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/difference-structure', [AId]), '',
        Code);
    try
        AssertEquals('the strip answers: ' + R.AsJSON, 200, Code);
        AssertTrue('a strip: ' + R.AsJSON,
            DifferenceStructureFromJson(R, Result));
    finally
        R.Free;
    end;
end;

function TStatisticsWeightingRestTest.ValuesOf(AId: longint;
    const ASet: string): TDoubleArray;
var
    Code: longint;
    R: TJSONObject;
    P: TPointsData;
begin
    R := Call('GET', Format('/problems/%d/%s', [AId, ASet]), '', Code);
    try
        AssertEquals(ASet + ' answers', 200, Code);
        AssertTrue(ASet + ' is points', PointsFromJson(R, P));
    finally
        R.Free;
    end;
    Result := P.Y;
end;

{ THE STRIP MEASURES THE DIFFERENCE AGAINST THE NOISE THE TYPE MEASURES, as
  the stretches and the figures do: four times the noise, a quarter of the
  power over it, at every sample. Standardised by the noise of counts whatever
  the type said, the two strips were the same - and the S&P 500's quiet years
  peaked at sixty thousand times a noise that was not theirs. }
procedure TStatisticsWeightingRestTest.TheStripJudgesTheDifferenceAgainstTheNoiseTheTypeMeasuresOverRest;
var
    Id, i: longint;
    AtOne, AtFour: TDifferenceStructure;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    Id := RippleFittedOf(MeasuredNoiseGaussId, [0, RippleSamples - 1]);
    AtOne := StructureOf(Id);
    TMeasuredNoiseGaussPointsSet.Variance := 4;
    AtFour := StructureOf(Id);
    AssertEquals('one value per sample of the interval', RippleSamples,
        Length(AtOne.Power));
    AssertTrue('the ripple stands out against a noise of one',
        MaxValue(AtOne.Power) > 10);
    for i := 0 to High(AtOne.Power) do
        AssertEquals(Format('a quarter of the power at sample %d', [i]),
            AtOne.Power[i] / 4, AtFour.Power[i], 1e-9 * Max(1, AtOne.Power[i]));
end;

{ A model of a type of counts is judged by the rule of counts that type names
  (here the test peak's, test_noise_rules), measured over the whole profile -
  over the data and the model the server holds. }
procedure TStatisticsWeightingRestTest.TheStripOfAModelOfCountsIsJudgedByTheNoiseOfCountsOverRest;
var
    Id, i: longint;
    S, Expected: TDifferenceStructure;
    X, Data, Fit: TDoubleArray;
begin
    Id := RippleFittedOf(TTestGaussPointsSet.GetCurveTypeId,
        [0, RippleSamples - 1]);
    S := StructureOf(Id);
    Data := ValuesOf(Id, 'profile');
    Fit := ValuesOf(Id, 'calc-profile');
    SetLength(X, Length(Data));
    for i := 0 to High(X) do
        X[i] := i;
    Expected := DifferenceStructureOf(FitWindowOf(X, Data, Fit,
        TTestCountsNoiseRule.Variances(Data), [0, RippleSamples - 1]));
    AssertTrue('something judged', Length(Expected.Power) > 0);
    AssertEquals('samples', Length(Expected.Power), Length(S.Power));
    AssertEquals('the level', Expected.Level, S.Level, 1e-9 * Expected.Level);
    for i := 0 to High(Expected.Power) do
        AssertEquals(Format('the power at sample %d', [i]), Expected.Power[i],
            S.Power[i], 1e-9 * Max(1, Expected.Power[i]));
end;

{ ONE MECHANISM: the strip reaches its level at exactly the samples the
  stretches /stats names - the two are judged over one window. }
procedure TStatisticsWeightingRestTest.TheStripReachesItsLevelWhereTheStretchesAreOverRest;
var
    Id, i, Named: longint;
    S: TDifferenceStructure;
    Stats: TJSONObject;
    Verdict: TResidualRandomness;
    Inside: boolean;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    Id := RippleFittedOf(MeasuredNoiseGaussId, [0, RippleSamples - 1]);
    S := StructureOf(Id);
    Stats := StatisticsOf(Id);
    try
        ReadRandomnessJson(Stats, Verdict);
    finally
        Stats.Free;
    end;
    AssertTrue('stretches named', Length(Verdict.NonRandomRanges) > 0);
    Named := 0;
    for i := 0 to High(S.X) do
    begin
        Inside := InRanges(Verdict.NonRandomRanges, S.X[i]);
        AssertEquals(Format('sample %g named exactly where a stretch is',
            [S.X[i]]), Inside, S.Power[i] >= S.Level);
        if Inside then
            Inc(Named);
    end;
    AssertTrue('some sample named', Named > 0);
end;

{ ONLY THE SAMPLES OF THE FIT INTERVALS: the gap between two is not judged
  and not sent - what the window drew over the whole profile while a run held
  no interval in its copy. }
procedure TStatisticsWeightingRestTest.TheStripCoversOnlyTheFitIntervalsOverRest;
var
    Id, i: longint;
    S: TDifferenceStructure;
    SeenFirst, SeenSecond: boolean;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    Id := RippleFittedOf(MeasuredNoiseGaussId, [10, 80, 90, 190]);
    S := StructureOf(Id);
    AssertTrue('something judged', Length(S.X) > 0);
    SeenFirst := False;
    SeenSecond := False;
    for i := 0 to High(S.X) do
    begin
        AssertTrue(Format('sample %g inside an interval', [S.X[i]]),
            ((S.X[i] >= 10) and (S.X[i] <= 80)) or
            ((S.X[i] >= 90) and (S.X[i] <= 190)));
        if S.X[i] <= 80 then
        begin
            AssertEquals('the first interval''s', 0, S.SpanOf[i]);
            SeenFirst := True;
        end
        else
        begin
            AssertEquals('the second interval''s', 1, S.SpanOf[i]);
            SeenSecond := True;
        end;
    end;
    AssertTrue('both intervals', SeenFirst and SeenSecond);
    AssertEquals('every sample of both', (80 - 10 + 1) + (190 - 90 + 1),
        Length(S.X));
end;

procedure TStatisticsWeightingRestTest.NothingFittedSendsAnEmptyStripOverRest;
var
    S: TDifferenceStructure;
begin
    S := StructureOf(NewProblem);
    AssertEquals('nothing', 0, Length(S.X));
    AssertTrue('no level', IsInfinite(S.Level));
end;

initialization
    //  A noise to measure by until a test sets its own: the registry's walk
    //  asks every registered type's rule.
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TMeasuredNoiseGaussPointsSet);
    RegisterTest('integration', TStatisticsWeightingRestTest);
    //  The client over a table of replies: nothing leaves the process.
end.
