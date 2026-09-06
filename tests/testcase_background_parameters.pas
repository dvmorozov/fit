// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which background coefficient an index means, and which are folded.)

THE BACKGROUND IS A SHIFTED QUADRATIC and, when the fit is allowed to vary it,
the optimiser addresses its four coefficients by number. The mapping from index
to coefficient was written out twice on the task - once to read the current value
and once to write a proposed one - and had no test either way, because reaching
either copy means an optimiser running with background variation switched on.

TWO COPIES OF ONE ORDERING is what these tests are really about. A reordering
applied to one copy and not the other has the optimiser reading the curvature and
writing the offset: the fit runs, every step lands on the wrong coefficient, and
the background comes out as a shape nobody asked for with no error anywhere.
Neither copy is wrong in isolation, which is exactly why nothing caught it. So the
central test is the ROUND TRIP - write through one, read through the other - and
it is asked per index with a distinct value each, because a mapping that swapped
two coefficients round-trips perfectly if both are written before either is read.

AND THE ASYMMETRY IS ASSERTED RATHER THAN TOLERATED. Two of the four are stored
as magnitudes and two are signed. That is a property of the parameterisation, not
an oversight: a background that curves downwards or sits below zero would be
subtracted from the data and add signal that was never measured. It also means the
optimiser's step into the folded half comes back reflected, which is worth being
able to look up when a simplex stalls.
}
unit testcase_background_parameters;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, background_parameters;

type
    TBackgroundParametersTest = class(TTestCase)
    private
        FA, FB, FC, FX0: double;
        procedure PutBack(AIndex: longint; AValue: double);
        function ReadBack(AIndex: longint): double;
    protected
        procedure SetUp; override;
    published
        //  The ordering.
        procedure EachIndexNamesItsOwnCoefficient;
        procedure AndWritesToTheSameOneItReadsFrom;
        procedure WritingOneLeavesTheOtherThreeAlone;
        procedure TheNamedIndicesAreTheNumbersTheOptimiserUses;
        procedure ThereAreFourOfThem;

        //  Outside the four.
        procedure AnIndexPastTheEndReadsZero;
        procedure AndWritesNothing;

        //  What is folded, and what that costs.
        procedure TheCurvatureAndTheOffsetAreHeldNonNegative;
        procedure TheSlopeAndTheCentreKeepTheirSign;
        procedure AFoldedCoefficientReflectsTheStepRatherThanRefusingIt;
        procedure AndTheReaderDoesNotFoldASecondTime;
        procedure ExactlyTwoOfTheFourAreFolded;
    end;

implementation

procedure TBackgroundParametersTest.SetUp;
begin
    //  DISTINCT AND NON-ZERO, so a mapping that read the wrong field answers a
    //  wrong number rather than a plausible one.
    FA := 11;
    FB := 22;
    FC := 33;
    FX0 := 44;
end;

procedure TBackgroundParametersTest.PutBack(AIndex: longint; AValue: double);
begin
    SetBackgroundParameter(AIndex, AValue, FA, FB, FC, FX0);
end;

function TBackgroundParametersTest.ReadBack(AIndex: longint): double;
begin
    Result := BackgroundParameter(AIndex, FA, FB, FC, FX0);
end;

{ --------------------------------- the ordering ----------------------------- }

procedure TBackgroundParametersTest.EachIndexNamesItsOwnCoefficient;
begin
    //  The reader, against the fields directly. This is the copy the optimiser
    //  asks for the current value before proposing a step from it.
    AssertEquals('curvature', FA, ReadBack(BACKGROUND_CURVATURE), 1e-12);
    AssertEquals('slope', FB, ReadBack(BACKGROUND_SLOPE), 1e-12);
    AssertEquals('offset', FC, ReadBack(BACKGROUND_OFFSET), 1e-12);
    AssertEquals('centre', FX0, ReadBack(BACKGROUND_CENTRE), 1e-12);
end;

procedure TBackgroundParametersTest.AndWritesToTheSameOneItReadsFrom;
var
    i: longint;
begin
    //  THE TEST THE TWO COPIES EXIST FOR. Written and read back one index at a
    //  time, with a different value each - because a mapping that swapped two
    //  coefficients round-trips perfectly if both are written before either is
    //  read, and would pass a test that wrote all four and then checked all
    //  four.
    for i := 0 to BACKGROUND_PARAMETER_COUNT - 1 do
    begin
        SetUp;
        PutBack(i, 100 + i);
        AssertEquals(Format('index %d round-trips', [i]),
            100 + i, ReadBack(i), 1e-12);
    end;
end;

procedure TBackgroundParametersTest.WritingOneLeavesTheOtherThreeAlone;
var
    i, j: longint;
begin
    //  ONE AT A TIME IS WHAT THE OPTIMISER DOES - it varies a single coordinate
    //  per step - so a setter that wrote through to a shared field, or fell
    //  through a case without a break, would move coefficients the simplex
    //  believes it is holding still.
    for i := 0 to BACKGROUND_PARAMETER_COUNT - 1 do
    begin
        SetUp;
        PutBack(i, 999);
        for j := 0 to BACKGROUND_PARAMETER_COUNT - 1 do
            if j <> i then
                AssertEquals(Format('writing %d left %d alone', [i, j]),
                    11 + 11 * j, ReadBack(j), 1e-12);
    end;
end;

procedure TBackgroundParametersTest.TheNamedIndicesAreTheNumbersTheOptimiserUses;
begin
    //  THE NAMES ARE FOR READERS; THE NUMBERS ARE THE CONTRACT. The optimiser
    //  counts coordinates from zero and knows nothing of these constants, so
    //  renaming is free and renumbering is not.
    AssertEquals('curvature is 0', 0, BACKGROUND_CURVATURE);
    AssertEquals('slope is 1', 1, BACKGROUND_SLOPE);
    AssertEquals('offset is 2', 2, BACKGROUND_OFFSET);
    AssertEquals('centre is 3', 3, BACKGROUND_CENTRE);
end;

procedure TBackgroundParametersTest.ThereAreFourOfThem;
begin
    //  The task checks a proposed index against this before mapping it, so it
    //  is the count that decides what "past the end" means.
    AssertEquals('four coefficients', 4, BACKGROUND_PARAMETER_COUNT);
end;

{ ------------------------------ outside the four ---------------------------- }

procedure TBackgroundParametersTest.AnIndexPastTheEndReadsZero;
begin
    //  ZERO RATHER THAN A RAISE, because the caller has already checked the
    //  index and this runs inside the objective - millions of times per fit.
    //  Characterised so that it is a known answer rather than whatever the
    //  compiler left in the result.
    AssertEquals('one past the end', 0.0,
        ReadBack(BACKGROUND_PARAMETER_COUNT), 1e-12);
    AssertEquals('and a negative index', 0.0, ReadBack(-1), 1e-12);
end;

procedure TBackgroundParametersTest.AndWritesNothing;
begin
    //  NOT a write to the nearest coefficient, which is what an unguarded case
    //  with an else branch would do - and it would corrupt the background from
    //  an index that was already out of range.
    PutBack(BACKGROUND_PARAMETER_COUNT, 999);
    PutBack(-1, 999);
    AssertEquals('curvature', 11.0, FA, 1e-12);
    AssertEquals('slope', 22.0, FB, 1e-12);
    AssertEquals('offset', 33.0, FC, 1e-12);
    AssertEquals('centre', 44.0, FX0, 1e-12);
end;

{ --------------------------- what is folded, and why ------------------------ }

procedure TBackgroundParametersTest.TheCurvatureAndTheOffsetAreHeldNonNegative;
begin
    //  A background that curves downwards or sits below zero is not a
    //  background: subtracted from the data it ADDS signal that was never
    //  measured, and the curves fitted on top of it are fitted to that.
    AssertTrue('curvature is folded',
        BackgroundParameterIsFolded(BACKGROUND_CURVATURE));
    AssertTrue('offset is folded',
        BackgroundParameterIsFolded(BACKGROUND_OFFSET));
end;

procedure TBackgroundParametersTest.TheSlopeAndTheCentreKeepTheirSign;
begin
    //  A background may perfectly well fall across the range, and its centre
    //  may be anywhere - including left of the data. Folding either would make
    //  a legitimate shape unreachable.
    AssertFalse('slope is signed',
        BackgroundParameterIsFolded(BACKGROUND_SLOPE));
    AssertFalse('centre is signed',
        BackgroundParameterIsFolded(BACKGROUND_CENTRE));
end;

procedure TBackgroundParametersTest.AFoldedCoefficientReflectsTheStepRatherThanRefusingIt;
begin
    //  REFLECTED, NOT CLAMPED AND NOT REFUSED, and the difference matters to
    //  whoever is wondering why a simplex is not converging: a step to -5 lands
    //  on +5, so the objective at the proposed point is not the objective the
    //  optimiser thinks it evaluated.
    PutBack(BACKGROUND_CURVATURE, -5);
    AssertEquals('curvature came back positive', 5.0,
        ReadBack(BACKGROUND_CURVATURE), 1e-12);
    PutBack(BACKGROUND_OFFSET, -7);
    AssertEquals('offset too', 7.0, ReadBack(BACKGROUND_OFFSET), 1e-12);

    //  And the signed pair really is signed, asserted here as well so the
    //  contrast is in one place.
    PutBack(BACKGROUND_SLOPE, -5);
    AssertEquals('slope kept its sign', -5.0,
        ReadBack(BACKGROUND_SLOPE), 1e-12);
    PutBack(BACKGROUND_CENTRE, -7);
    AssertEquals('centre kept its sign', -7.0,
        ReadBack(BACKGROUND_CENTRE), 1e-12);
end;

procedure TBackgroundParametersTest.AndTheReaderDoesNotFoldASecondTime;
begin
    //  FOLDED ON THE WAY IN ONLY. Folded again on the way out, a negative that
    //  arrived some other way - a saved model, a value set directly - would be
    //  hidden from whoever is looking for it, and the background drawn would
    //  not be the background stored.
    FA := -3;
    AssertEquals('the reader hands back what is there', -3.0,
        ReadBack(BACKGROUND_CURVATURE), 1e-12);
end;

procedure TBackgroundParametersTest.ExactlyTwoOfTheFourAreFolded;
var
    i, Folded: longint;
begin
    //  COUNTED OVER THE WHOLE RANGE, so a fifth coefficient added later has to
    //  be considered rather than inheriting whichever answer the predicate
    //  happens to give it.
    Folded := 0;
    for i := 0 to BACKGROUND_PARAMETER_COUNT - 1 do
        if BackgroundParameterIsFolded(i) then
            Inc(Folded);
    AssertEquals('two folded, two signed', 2, Folded);
end;

initialization
    //  A unit test: an index and four doubles. No task, no optimiser, no
    //  background - which is what two copies of this mapping on a task could
    //  not be asked without.
    RegisterTest('unit', TBackgroundParametersTest);
end.
