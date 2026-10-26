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

AND THE WINDOW JUDGES BY THE SAME NOISE: where the difference is not noise -
the shaded stretches /stats names, and the Difference structure strip the
client draws from what it holds (model_noise) - is measured against the noise
the type measures. The client's tests are unit tests over a table of replies.
}
unit testcase_statistics_weighting;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    gauss_points_set, named_points_set, fit_reduction, curve_types_singleton,
    int_curve_factory, fit_statistics, fit_points_json,
    mock_http_transport, mock_fit_viewer, difference_structure,
    testcase_client_async_commands, testcase_rest_api, model_noise,
    fit_window, counts_noise_rule,
    mscr_specimen_list, persistent_curve_parameters, noise_rule,
    test_noise_rules;

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
    TMeasuredNoiseGaussPointsSet = class(TGaussPointsSet)
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
    published
        procedure AModelThatMeasuresItsNoiseIsScoredByItOverRest;
        procedure AModelOfCountsIsScoredAsItAlwaysWas;
        procedure AReductionByBicWeighsTheNoiseTheModelMeasures;
        procedure WhereTheDifferenceIsNotNoiseIsJudgedByTheTypesNoiseOverRest;
    end;

    { THE STRIP UNDER THE CHART - Difference structure - as the window builds
      it, from what the client holds: a model of the type above, read through
      the replies the window reads. }
    TStructureNoiseClientTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TSyncClient;
        { The structure the strip draws once the client holds a fitted model
          of ACurveType over the ripple profile. }
        function StructureOfAModelOf(const ACurveType: TGuid): TDifferenceStructure;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheStripJudgesTheDifferenceAgainstTheNoiseTheTypeMeasures;
        procedure TheStripOfAModelOfCountsIsJudgedByTheNoiseOfCounts;
    end;

    { Which noise the client reads off the curve types its model states. }
    TModelNoiseOfAttributesTest = class(TTestCase)
    private
        FAttrs: TMSCRCurveList;
        { A curve of type ATypeId in the list. }
        procedure AddCurveOf(const ATypeId: TGuid);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure WithNoAttributesTheDataAreCounts;
        procedure ACurveOfATypeThatMeasuresItsNoiseDecides;
        procedure ACurveOfCountsBesideItLeavesTheNoiseMeasured;
        procedure AModelOfCountsIsMeasuredAsCounts;
        procedure ATypeNotStatedOrNotRegisteredTakesNoPart;
        procedure WithNoCurveStatedTheSelectedTypesRuleIsAsked;
        procedure ACurveStatedOutranksTheSelectedType;
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
    Id := FittedProblemOf(TGaussPointsSet.GetCurveTypeId);
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

{ ---------------------- the strip, as the window draws it ------------------- }

const
    BASE = 'http://localhost:8080';

procedure TStructureNoiseClientTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TSyncClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TStructureNoiseClientTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

function TStructureNoiseClientTest.StructureOfAModelOf(
    const ACurveType: TGuid): TDifferenceStructure;
begin
    //  Every route a fit's completion re-reads; the model a Gaussian on the
    //  peak alone, so the difference is the ripple.
    FSvc.Reply('profile', RippleProfileJson(True));
    FSvc.Reply('calc-profile', RippleProfileJson(False));
    FSvc.Reply('rfactor-bounds',
        Format('{"title":"s","x":[0,%d],"y":[0,0]}', [RippleSamples - 1]));
    FSvc.Reply('selected-interval', '{"title":"i","x":[],"y":[]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[],"y":[]}');
    FSvc.Reply('background', '{"title":"b","x":[],"y":[]}');
    FSvc.Reply('curve-positions', '{"title":"q","x":[100],"y":[100]}');
    FSvc.Reply('calc-positions', '{"title":"r","x":[100],"y":[100]}');
    FSvc.Reply('curves', Format('{"ok":true,"curves":[{' +
        '"id":"{11111111-1111-1111-1111-111111111111}","curveType":"%s",' +
        '"fitted":true,"params":[],' +
        '"points":{"title":"g","x":[100],"y":[100]}}]}',
        [GUIDToString(ACurveType)]));
    FSvc.Reply('stats', '{"ok":true}');
    FClient.MinimizeDifference;
    Result := FClient.ResidualStructure;
    AssertEquals('one value per sample of the interval', RippleSamples,
        Length(Result.Power));
end;

{ THE STRIP MEASURES THE DIFFERENCE AGAINST THE NOISE THE TYPE MEASURES, as
  the server's stretches and the figures do: four times the noise, a quarter
  of the power over it, at every sample. Standardised by the noise of counts
  whatever the type said, the two strips were the same - and the S&P 500's
  quiet years peaked at sixty thousand times a noise that was not theirs. }
procedure TStructureNoiseClientTest.TheStripJudgesTheDifferenceAgainstTheNoiseTheTypeMeasures;
var
    AtOne, AtFour: TDifferenceStructure;
    i: longint;
begin
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    AtOne := StructureOfAModelOf(MeasuredNoiseGaussId);
    TMeasuredNoiseGaussPointsSet.Variance := 4;
    AtFour := StructureOfAModelOf(MeasuredNoiseGaussId);
    AssertTrue('the ripple stands out against a noise of one',
        MaxValue(AtOne.Power) > 10);
    for i := 0 to High(AtOne.Power) do
        AssertEquals(Format('a quarter of the power at sample %d', [i]),
            AtOne.Power[i] / 4, AtFour.Power[i], 1e-9 * Max(1, AtOne.Power[i]));
end;

{ A model of a type of counts is judged by the rule of counts its module
  names (counts_noise_rule), measured over the whole profile. }
procedure TStructureNoiseClientTest.TheStripOfAModelOfCountsIsJudgedByTheNoiseOfCounts;
var
    S, Before: TDifferenceStructure;
    X, Data, Fit: array of double;
    i: longint;
begin
    S := StructureOfAModelOf(TGaussPointsSet.GetCurveTypeId);
    SetLength(X, RippleSamples);
    SetLength(Data, RippleSamples);
    SetLength(Fit, RippleSamples);
    for i := 0 to RippleSamples - 1 do
    begin
        X[i] := i;
        Data[i] := RippleAt(i, True);
        Fit[i] := RippleAt(i, False);
    end;
    Before := DifferenceStructureOf(FitWindowOf(X, Data, Fit,
        TCountsNoiseRule.Variances(Data), [0, RippleSamples - 1]));
    AssertEquals('the level', Before.Level, S.Level, 1e-9 * Before.Level);
    for i := 0 to High(Before.Power) do
        AssertEquals(Format('the power at sample %d', [i]), Before.Power[i],
            S.Power[i], 1e-9 * Max(1, Before.Power[i]));
end;

{ ------------------- the noise read off the stated types ------------------- }

procedure TModelNoiseOfAttributesTest.SetUp;
begin
    FAttrs := TMSCRCurveList.Create;
    TMeasuredNoiseGaussPointsSet.Variance := 2.5;
end;

procedure TModelNoiseOfAttributesTest.TearDown;
begin
    FreeAndNil(FAttrs);
end;

procedure TModelNoiseOfAttributesTest.AddCurveOf(const ATypeId: TGuid);
var
    CP: Curve_parameters;
begin
    CP := Curve_parameters.Create(nil);
    CP.FCurveTypeId := ATypeId;
    FAttrs.Add(CP);
end;

procedure TModelNoiseOfAttributesTest.WithNoAttributesTheDataAreCounts;
begin
    AssertEquals('none held', 0,
        Length(ModelNoiseOfAttributes(nil, [1, 2, 3], GUID_NULL)));
    AssertEquals('none in the list', 0,
        Length(ModelNoiseOfAttributes(FAttrs, [1, 2, 3], GUID_NULL)));
end;

procedure TModelNoiseOfAttributesTest.ACurveOfATypeThatMeasuresItsNoiseDecides;
var
    Noise: TNoiseVariances;
begin
    AddCurveOf(MeasuredNoiseGaussId);
    Noise := ModelNoiseOfAttributes(FAttrs, [1, 2, 3], GUID_NULL);
    AssertEquals('one per point', 3, Length(Noise));
    AssertEquals('the type''s', 2.5, Noise[2], 0);
end;

{ A model may hold several types; the first that measures decides, as the
  server's ModelResidualNoise has it. }
procedure TModelNoiseOfAttributesTest.ACurveOfCountsBesideItLeavesTheNoiseMeasured;
var
    Noise: TNoiseVariances;
begin
    AddCurveOf(TGaussPointsSet.GetCurveTypeId);
    AddCurveOf(MeasuredNoiseGaussId);
    Noise := ModelNoiseOfAttributes(FAttrs, [1, 2], GUID_NULL);
    AssertEquals('one per point', 2, Length(Noise));
    AssertEquals('the measuring type''s', 2.5, Noise[0], 0);
end;

procedure TModelNoiseOfAttributesTest.AModelOfCountsIsMeasuredAsCounts;
var
    Noise, Counts: TNoiseVariances;
    i: longint;
begin
    AddCurveOf(TGaussPointsSet.GetCurveTypeId);
    Noise := ModelNoiseOfAttributes(FAttrs, [3, 5, 4, 8, 6, 9], GUID_NULL);
    Counts := TCountsNoiseRule.Variances([3, 5, 4, 8, 6, 9]);
    AssertEquals('one per point', 6, Length(Noise));
    for i := 0 to High(Counts) do
        AssertEquals('the rule of counts', Counts[i], Noise[i], 0);
end;

{ A server that predates curveType states none; a type this build does not
  register cannot be asked. Neither is a guess at the noise. }
procedure TModelNoiseOfAttributesTest.ATypeNotStatedOrNotRegisteredTakesNoPart;
const
    Unregistered: TGuid = '{0B1D2F3A-4C5E-4F60-8A7B-9C0D1E2F3A4B}';
begin
    AddCurveOf(GUID_NULL);
    AddCurveOf(Unregistered);
    AssertEquals('counts', 0,
        Length(ModelNoiseOfAttributes(FAttrs, [1, 2], GUID_NULL)));
end;

{ The first fit of picks is animated before the window has read any curve:
  they are being built of the type selected, so its rule is asked. }
procedure TModelNoiseOfAttributesTest.WithNoCurveStatedTheSelectedTypesRuleIsAsked;
var
    Noise: TNoiseVariances;
begin
    Noise := ModelNoiseOfAttributes(FAttrs, [1, 2, 3], MeasuredNoiseGaussId);
    AssertEquals('one per point', 3, Length(Noise));
    AssertEquals('the selected type''s', 2.5, Noise[1], 0);
    AssertEquals('none held either', 3,
        Length(ModelNoiseOfAttributes(nil, [1, 2, 3], MeasuredNoiseGaussId)));
end;

procedure TModelNoiseOfAttributesTest.ACurveStatedOutranksTheSelectedType;
var
    Noise, Counts: TNoiseVariances;
begin
    AddCurveOf(TGaussPointsSet.GetCurveTypeId);
    Noise := ModelNoiseOfAttributes(FAttrs, [3, 5, 4, 8, 6, 9],
        MeasuredNoiseGaussId);
    Counts := TCountsNoiseRule.Variances([3, 5, 4, 8, 6, 9]);
    AssertEquals('the model''s own type''s rule', Counts[0], Noise[0], 0);
end;

initialization
    //  A noise to measure by until a test sets its own: the registry's walk
    //  asks every registered type's rule.
    TMeasuredNoiseGaussPointsSet.Variance := 1;
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TMeasuredNoiseGaussPointsSet);
    RegisterTest('integration', TStatisticsWeightingRestTest);
    //  The client over a table of replies: nothing leaves the process.
    RegisterTest('unit', TStructureNoiseClientTest);
    RegisterTest('unit', TModelNoiseOfAttributesTest);
end.
