// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The solution representations the minimizers are built on.)

TFloatDecision is what the downhill-simplex algorithm actually uses -
TDownhillSimplexDecision descends from it - so its copying, comparison and
parameter bookkeeping are load-bearing for every fit this application runs.
TByteDecision and TTwoDimFloatDecision are the genetic-algorithm siblings: this
application never constructs one, but they are part of the package and so part of
the target.

ONE METHOD USED TO BE DELIBERATELY NOT EXECUTED, and the reason turned out to be
one word. TByteDecision.ExchangeWithOuter wrote through TFloatDecision's setter
into a TByteDecision's byte array - an eight-byte write into a one-byte slot,
which corrupts the heap rather than failing, so a test that called it would have
failed somewhere else entirely, later, in whatever allocation was overwritten.
The guard on the line above the write had already proved the type; only the cast
beneath it named the wrong one. It is corrected, and the method is exercised
here like every other.
}
unit testcase_decisions;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Contnrs, fpcunit, testregistry, Decisions;

type
    TFloatDecisionTest = class(TTestCase)
    private
        FD: TFloatDecision;
        function Make(const AValues: array of double): TFloatDecision;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure GrowingTheParameterCountZeroFillsTheNewSlots;
        procedure ShrinkingDropsTheTail;
        procedure AnIndexPastTheEndIsRefused;
        procedure ANegativeIndexIsRefused;
        procedure ACopyHasTheSameValuesAndIsIndependent;
        procedure ACopyCarriesTheEvaluation;
        procedure CoincideIsTrueWithinTheTolerance;
        procedure CoincideIsFalseOutsideTheTolerance;
        procedure CoincideRefusesAForeignType;
        procedure InvertNegates;
        procedure ExchangeSwapsTwoParameters;
        procedure ExchangeWithOuterSwapsAcrossTwoDecisions;
        procedure ExchangeWithOuterRefusesAForeignType;
        procedure CopyParameterOverwritesTheDestination;
    end;

    TByteDecisionTest = class(TTestCase)
    private
        FD: TByteDecision;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ParametersRoundTrip;
        procedure AnIndexPastTheEndIsRefused;
        procedure InvertIsBitwiseNotArithmetic;
        procedure ExchangeSwapsTwoParameters;
        procedure ACopyHasTheSameBytes;
        procedure CoincideComparesExactlyNotApproximately;
        procedure CoincideRefusesAForeignType;
        procedure ExchangeWithOuterSwapsOneParameterBetweenTwo;
        procedure ExchangeWithOuterRefusesAForeignType;
    end;

    TTwoDimFloatDecisionTest = class(TTestCase)
    private
        FD: TTwoDimFloatDecision;
        procedure Fill(AD: TTwoDimFloatDecision; ABase: double);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure GenesAndParametersAreSizedIndependently;
        procedure TheSelectedGeneChoosesTheRow;
        procedure ASelectedGeneOutOfRangeIsRefused;
        procedure InvertNegatesOneCell;
        procedure ExchangeSwapsAcrossGenes;
        procedure CopyParameterCopiesAcrossGenes;
        procedure ExchangeWithOuterSwapsBetweenDecisions;
        procedure ACopyReproducesEveryGene;
        procedure CoincideOnlyEverComparesOneGeneOfTheOther;
        procedure InvertBlockNegatesARectangle;
        procedure CopyBlockShiftsARectangle;
        procedure CopyBlockWrapsPastTheEnd;
        procedure ExchangeBlocksWithOuterSwapsARectangle;

        //  The guards, and the wrap the other way - see the group.
        procedure ReadingAParameterPastTheEndIsRefused;
        procedure WritingOnePastTheEndIsRefused;
        procedure ANegativeParameterIndexIsRefusedBothWays;
        procedure CoincideRefusesAForeignType;
        procedure ExchangeWithOuterRefusesAForeignType;
        procedure AndItsRefusalNamesTheTypeItWasGiven;
        procedure AnUnsizedDecisionHasNoGenesRatherThanFaulting;
        procedure AForwardWrapRotatesTheGenes;
        procedure ABackwardWrapDoesNotAndNothingCallsIt;
    end;

    TDecisionsListTest = class(TTestCase)
    private
        FList: TDecisionsList;
        function Add(const AEvaluation: double): TFloatDecision;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheAbsoluteMinimumIsTheSmallestEvaluation;
        procedure TheAbsoluteMaximumIsTheLargestEvaluation;
        procedure AnEmptyListRefusesToAnswer;
        procedure TheMaximumBelowALimitIsFound;
        procedure TheMinimumAboveALimitIsFound;
        procedure TheSearchStartsWhereItIsTold;
        procedure NothingUnderTheLimitAnswersNil;
        procedure MembershipIsByValueNotByIdentity;
        procedure TheAscendingComparatorOrdersByEvaluation;
        procedure TheDescendingComparatorIsItsMirror;

        //  ONE DESCENDING LIST HID FOUR BRANCHES - see the group in the body.
        procedure TheBoundedMaximumIsFoundOnAnUnsortedList;
        procedure AndTheBoundedMinimum;
        procedure TheAbsoluteMaximumIsFoundWhenItIsNotFirst;
        procedure ABoundedSearchStopsAtTheLimitItself;
        procedure AmongTiesTheBoundedSearchKeepsTheLast;
        procedure EveryBoundedSearchRefusesAnEmptyList;
        procedure AndSoDoesEveryAbsoluteOne;
    end;

implementation

{ ---- TFloatDecision -------------------------------------------------------- }

procedure TFloatDecisionTest.SetUp;
begin
    FD := TFloatDecision.Create(nil);
end;

procedure TFloatDecisionTest.TearDown;
begin
    FreeAndNil(FD);
end;

function TFloatDecisionTest.Make(const AValues: array of double): TFloatDecision;
var
    i: longint;
begin
    Result := TFloatDecision.Create(nil);
    Result.ParametersNumber := Length(AValues);
    for i := Low(AValues) to High(AValues) do
        Result.Parameters[i] := AValues[i];
end;

procedure TFloatDecisionTest.GrowingTheParameterCountZeroFillsTheNewSlots;
begin
    //  A grown decision must not carry whatever the heap held: the simplex reads
    //  every parameter, so an uninitialised slot is a random starting point.
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 1;
    FD.Parameters[1] := 2;
    FD.ParametersNumber := 4;
    AssertEquals('count', 4, FD.ParametersNumber);
    AssertEquals('kept', 1.0, FD.Parameters[0], 1e-12);
    AssertEquals('kept', 2.0, FD.Parameters[1], 1e-12);
    AssertEquals('new slot is zero', 0.0, FD.Parameters[2], 1e-12);
    AssertEquals('and the next', 0.0, FD.Parameters[3], 1e-12);
end;

procedure TFloatDecisionTest.ShrinkingDropsTheTail;
begin
    FD.ParametersNumber := 3;
    FD.Parameters[0] := 7;
    FD.ParametersNumber := 1;
    AssertEquals('count', 1, FD.ParametersNumber);
    AssertEquals('the survivor is unchanged', 7.0, FD.Parameters[0], 1e-12);
end;

procedure TFloatDecisionTest.AnIndexPastTheEndIsRefused;
var
    Raised: boolean;
begin
    //  Named exception rather than a silent out-of-bounds read, which in a release
    //  build would return whatever was next in memory as a parameter value.
    FD.ParametersNumber := 2;
    Raised := False;
    try
        FD.Parameters[2] := 1;
    except
        on EFloatDecision do Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TFloatDecisionTest.ANegativeIndexIsRefused;
var
    Raised: boolean;
    Dummy: double;
begin
    FD.ParametersNumber := 2;
    Raised := False;
    try
        Dummy := FD.Parameters[-1];
    except
        on EFloatDecision do Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TFloatDecisionTest.ACopyHasTheSameValuesAndIsIndependent;
var
    Copy: TAbstractDecision;
begin
    //  The simplex keeps a copy of the best solution found so far
    //  (DownhillSimplexServer.UpdateResults), so a shallow copy would let the
    //  running search overwrite the best result it had already banked.
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 1;
    FD.Parameters[1] := 2;
    Copy := FD.GetCopy;
    try
        AssertEquals('values copied', 1.0, TFloatDecision(Copy).Parameters[0], 1e-12);
        FD.Parameters[0] := 99;
        AssertEquals('and the copy is not a view onto the original',
            1.0, TFloatDecision(Copy).Parameters[0], 1e-12);
    finally
        Copy.Free;
    end;
end;

procedure TFloatDecisionTest.ACopyCarriesTheEvaluation;
var
    Copy: TAbstractDecision;
begin
    //  Without this the banked best solution would compare as evaluation 0, which
    //  for an R-factor is better than anything real.
    FD.ParametersNumber := 1;
    FD.Evaluation := 0.125;
    Copy := FD.GetCopy;
    try
        AssertEquals(0.125, Copy.Evaluation, 1e-12);
    finally
        Copy.Free;
    end;
end;

procedure TFloatDecisionTest.CoincideIsTrueWithinTheTolerance;
var
    Other: TFloatDecision;
begin
    //  Compared with a tolerance, not exactly: two simplex vertices that have
    //  converged onto one point differ in the last bits, and treating them as
    //  distinct is what stops a search recognising it has finished.
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 1;
    FD.Parameters[1] := 2;
    Other := Make([1 + 1e-9, 2 - 1e-9]);
    try
        AssertTrue('within tolerance', FD.Coincide(Other));
    finally
        Other.Free;
    end;
end;

procedure TFloatDecisionTest.CoincideIsFalseOutsideTheTolerance;
var
    Other: TFloatDecision;
begin
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 1;
    FD.Parameters[1] := 2;
    Other := Make([1, 2.001]);
    try
        AssertFalse('outside tolerance', FD.Coincide(Other));
    finally
        Other.Free;
    end;
end;

procedure TFloatDecisionTest.CoincideRefusesAForeignType;
var
    Other: TByteDecision;
    Raised: boolean;
begin
    //  The comparison reinterprets the argument as its own class, so a foreign
    //  type has to be refused before it is read - the alternative is comparing
    //  doubles against bytes at the same offsets.
    FD.ParametersNumber := 1;
    Other := TByteDecision.Create(nil);
    Raised := False;
    try
        try
            FD.Coincide(Other);
        except
            on EFloatDecision do Raised := True;
        end;
        AssertTrue('refused by name', Raised);
    finally
        Other.Free;
    end;
end;

procedure TFloatDecisionTest.InvertNegates;
begin
    FD.ParametersNumber := 1;
    FD.Parameters[0] := 2.5;
    FD.InvertParameter(0);
    AssertEquals('negated', -2.5, FD.Parameters[0], 1e-12);
    FD.InvertParameter(0);
    AssertEquals('and back', 2.5, FD.Parameters[0], 1e-12);
end;

procedure TFloatDecisionTest.ExchangeSwapsTwoParameters;
begin
    FD.ParametersNumber := 3;
    FD.Parameters[0] := 1;
    FD.Parameters[1] := 2;
    FD.Parameters[2] := 3;
    FD.ExchangeParameters(0, 2);
    AssertEquals('', 3.0, FD.Parameters[0], 1e-12);
    AssertEquals('the middle is untouched', 2.0, FD.Parameters[1], 1e-12);
    AssertEquals('', 1.0, FD.Parameters[2], 1e-12);
end;

procedure TFloatDecisionTest.ExchangeWithOuterSwapsAcrossTwoDecisions;
var
    Other: TFloatDecision;
begin
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 1;
    Other := Make([9, 9]);
    try
        FD.ExchangeWithOuter(Other, 0);
        AssertEquals('mine took theirs', 9.0, FD.Parameters[0], 1e-12);
        AssertEquals('and theirs took mine', 1.0, Other.Parameters[0], 1e-12);
    finally
        Other.Free;
    end;
end;

procedure TFloatDecisionTest.ExchangeWithOuterRefusesAForeignType;
var
    Other: TByteDecision;
    Raised: boolean;
begin
    FD.ParametersNumber := 1;
    Other := TByteDecision.Create(nil);
    Other.ParametersNumber := 1;
    Raised := False;
    try
        try
            FD.ExchangeWithOuter(Other, 0);
        except
            on EFloatDecision do Raised := True;
        end;
        AssertTrue('refused before anything was written', Raised);
    finally
        Other.Free;
    end;
end;

procedure TFloatDecisionTest.CopyParameterOverwritesTheDestination;
begin
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 5;
    FD.Parameters[1] := 7;
    FD.CopyParameter(0, 1);
    AssertEquals('source unchanged', 5.0, FD.Parameters[0], 1e-12);
    AssertEquals('destination overwritten', 5.0, FD.Parameters[1], 1e-12);
end;

{ ---- TByteDecision --------------------------------------------------------- }

procedure TByteDecisionTest.SetUp;
begin
    FD := TByteDecision.Create(nil);
end;

procedure TByteDecisionTest.TearDown;
begin
    FreeAndNil(FD);
end;

procedure TByteDecisionTest.ParametersRoundTrip;
begin
    FD.ParametersNumber := 3;
    FD.Parameters[0] := 0;
    FD.Parameters[1] := 128;
    FD.Parameters[2] := 255;
    AssertEquals('', 0, FD.Parameters[0]);
    AssertEquals('', 128, FD.Parameters[1]);
    AssertEquals('the full byte range', 255, FD.Parameters[2]);
end;

procedure TByteDecisionTest.AnIndexPastTheEndIsRefused;
var
    Raised: boolean;
begin
    FD.ParametersNumber := 1;
    Raised := False;
    try
        FD.Parameters[5] := 1;
    except
        on EByteDecision do Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TByteDecisionTest.InvertIsBitwiseNotArithmetic;
begin
    //  NOT, not negation - the float sibling negates, and a reader who assumed
    //  the two behaved alike would be wrong. 0 inverts to 255, not to 0.
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 0;
    FD.Parameters[1] := 1;
    FD.InvertParameter(0);
    FD.InvertParameter(1);
    AssertEquals('not 0 is 255', 255, FD.Parameters[0]);
    AssertEquals('not 1 is 254', 254, FD.Parameters[1]);
end;

procedure TByteDecisionTest.ExchangeSwapsTwoParameters;
begin
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 10;
    FD.Parameters[1] := 20;
    FD.ExchangeParameters(0, 1);
    AssertEquals('', 20, FD.Parameters[0]);
    AssertEquals('', 10, FD.Parameters[1]);
end;

procedure TByteDecisionTest.ACopyHasTheSameBytes;
var
    Copy: TAbstractDecision;
begin
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 3;
    FD.Parameters[1] := 4;
    FD.Evaluation := 1.5;
    Copy := FD.GetCopy;
    try
        AssertEquals('', 3, TByteDecision(Copy).Parameters[0]);
        AssertEquals('', 4, TByteDecision(Copy).Parameters[1]);
        AssertEquals('evaluation too', 1.5, Copy.Evaluation, 1e-12);
    finally
        Copy.Free;
    end;
end;

procedure TByteDecisionTest.CoincideComparesExactlyNotApproximately;
var
    Other: TByteDecision;
begin
    //  Bytes are exact, so unlike the float sibling there is no tolerance: a
    //  difference of one is a difference.
    FD.ParametersNumber := 1;
    FD.Parameters[0] := 7;
    Other := TByteDecision.Create(nil);
    try
        Other.ParametersNumber := 1;
        Other.Parameters[0] := 7;
        AssertTrue('equal', FD.Coincide(Other));
        Other.Parameters[0] := 8;
        AssertFalse('one apart is not equal', FD.Coincide(Other));
    finally
        Other.Free;
    end;
end;

procedure TByteDecisionTest.CoincideRefusesAForeignType;
var
    Other: TFloatDecision;
    Raised: boolean;
begin
    FD.ParametersNumber := 1;
    Other := TFloatDecision.Create(nil);
    Raised := False;
    try
        try
            FD.Coincide(Other);
        except
            on EByteDecision do Raised := True;
        end;
        AssertTrue('refused by name', Raised);
    finally
        Other.Free;
    end;
end;

procedure TByteDecisionTest.ExchangeWithOuterSwapsOneParameterBetweenTwo;
var
    Other: TByteDecision;
begin
    //  THIS WAS IGNORED FOR A ONE-WORD DEFECT. The last line of
    //  ExchangeWithOuter read
    //
    //      TFloatDecision(Decision)[ParamNum] := TempByte;
    //
    //  casting a TByteDecision - which the guard on the line above has just
    //  proved it is - to TFloatDecision, so an eight-byte double went into a
    //  one-byte slot at eight times the intended offset. Corrected, and this
    //  now runs: one named parameter changes hands, and only that one.
    FD.ParametersNumber := 2;
    FD.Parameters[0] := 3;
    FD.Parameters[1] := 4;

    Other := TByteDecision.Create(nil);
    try
        Other.ParametersNumber := 2;
        Other.Parameters[0] := 7;
        Other.Parameters[1] := 9;

        FD.ExchangeWithOuter(Other, 0);

        AssertEquals('this one took the other value', 7, FD.Parameters[0]);
        AssertEquals('and the other took this one', 3, Other.Parameters[0]);
        //  The parameter NOT named is untouched on both sides - which is the
        //  whole point of exchanging one rather than swapping the decisions.
        AssertEquals('this one other parameter', 4, FD.Parameters[1]);
        AssertEquals('and the other one', 9, Other.Parameters[1]);
    finally
        Other.Free;
    end;
end;

procedure TByteDecisionTest.ExchangeWithOuterRefusesAForeignType;
var
    Other: TFloatDecision;
    Raised: boolean;
begin
    //  The guard that made the cast safe to correct: by the time the write
    //  happens the type is settled, so the only question was which name to
    //  write beneath it.
    Other := TFloatDecision.Create(nil);
    Raised := False;
    try
        Other.ParametersNumber := 2;
        try
            FD.ExchangeWithOuter(Other, 0);
        except
            on EByteDecision do
                Raised := True;
        end;
        AssertTrue('refused by name', Raised);
    finally
        Other.Free;
    end;
end;

{ ---- TTwoDimFloatDecision -------------------------------------------------- }

procedure TTwoDimFloatDecisionTest.SetUp;
begin
    FD := TTwoDimFloatDecision.Create(nil);
    //  Genes first, then parameters: SetGenesNumber sizes each new row to the
    //  CURRENT parameter count, and SetParametersNumber resizes every existing
    //  row - so either order works, but only if both are set.
    FD.GenesNumber := 2;
    FD.ParametersNumber := 3;
end;

procedure TTwoDimFloatDecisionTest.TearDown;
begin
    FreeAndNil(FD);
end;

procedure TTwoDimFloatDecisionTest.Fill(AD: TTwoDimFloatDecision; ABase: double);
var
    g, p: longint;
begin
    for g := 0 to AD.GenesNumber - 1 do
    begin
        AD.SelectedGene := g;
        for p := 0 to AD.ParametersNumber - 1 do
            AD.Parameters[p] := ABase + g * 10 + p;
    end;
end;

procedure TTwoDimFloatDecisionTest.GenesAndParametersAreSizedIndependently;
begin
    AssertEquals('genes', 2, FD.GenesNumber);
    AssertEquals('parameters', 3, FD.ParametersNumber);
    FD.GenesNumber := 4;
    AssertEquals('more genes', 4, FD.GenesNumber);
    //  A gene added after the parameter count was set must still be the right
    //  width, or reading its last parameter is out of bounds.
    FD.SelectedGene := 3;
    FD.Parameters[2] := 1;
    AssertEquals('the new gene is full width', 1.0, FD.Parameters[2], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.TheSelectedGeneChoosesTheRow;
begin
    //  The indexed property reads whichever gene is selected, so the selection is
    //  part of the address - the class is a cursor over a matrix, not a matrix.
    Fill(FD, 0);
    FD.SelectedGene := 0;
    AssertEquals('', 1.0, FD.Parameters[1], 1e-12);
    FD.SelectedGene := 1;
    AssertEquals('the same index, a different gene', 11.0, FD.Parameters[1], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.ASelectedGeneOutOfRangeIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        FD.SelectedGene := 5;
    except
        on ETwoDimFloatDecision do Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TTwoDimFloatDecisionTest.InvertNegatesOneCell;
begin
    Fill(FD, 1);
    FD.InvertParameter(1, 2);
    FD.SelectedGene := 1;
    AssertEquals('negated', -(1 + 10 + 2.0), FD.Parameters[2], 1e-12);
    AssertEquals('its neighbour untouched', 1 + 10 + 1.0, FD.Parameters[1], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.ExchangeSwapsAcrossGenes;
begin
    Fill(FD, 0);
    //  gene0[0] = 0, gene1[2] = 12
    FD.ExchangeParameters(0, 0, 1, 2);
    FD.SelectedGene := 0;
    AssertEquals('', 12.0, FD.Parameters[0], 1e-12);
    FD.SelectedGene := 1;
    AssertEquals('', 0.0, FD.Parameters[2], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.CopyParameterCopiesAcrossGenes;
begin
    Fill(FD, 0);
    FD.CopyParameter(0, 0, 1, 2);
    FD.SelectedGene := 0;
    AssertEquals('source unchanged', 0.0, FD.Parameters[0], 1e-12);
    FD.SelectedGene := 1;
    AssertEquals('destination overwritten', 0.0, FD.Parameters[2], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.ExchangeWithOuterSwapsBetweenDecisions;
var
    Other: TTwoDimFloatDecision;
begin
    Fill(FD, 0);
    Other := TTwoDimFloatDecision.Create(nil);
    try
        Other.GenesNumber := 2;
        Other.ParametersNumber := 3;
        Fill(Other, 100);
        FD.ExchangeWithOuter(Other, 0, 1, 0);
        FD.SelectedGene := 0;
        AssertEquals('mine took theirs', 110.0, FD.Parameters[0], 1e-12);
        Other.SelectedGene := 1;
        AssertEquals('and theirs took mine', 0.0, Other.Parameters[0], 1e-12);
    finally
        Other.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.ACopyReproducesEveryGene;
var
    Copy: TAbstractDecision;
begin
    Fill(FD, 0);
    FD.Evaluation := 0.5;
    Copy := FD.GetCopy;
    try
        AssertEquals('genes', 2, TTwoDimFloatDecision(Copy).GenesNumber);
        TTwoDimFloatDecision(Copy).SelectedGene := 1;
        AssertEquals('the second gene came too', 12.0,
            TTwoDimFloatDecision(Copy).Parameters[2], 1e-12);
        AssertEquals('evaluation', 0.5, Copy.Evaluation, 1e-12);
    finally
        Copy.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.CoincideOnlyEverComparesOneGeneOfTheOther;
var
    Other: TTwoDimFloatDecision;
begin
    //  A DEFECT, characterised rather than fixed - this unit is in a sibling
    //  repository. The loop advances its OWN selected gene:
    //
    //      for i := 0 to GenesNumber - 1 do
    //      begin
    //          SelectedGene := i;                       //  self only
    //          ... Abs(TwoDimFloatDecision[j] - Self[j]) ...
    //
    //  but never the other decision's, and the indexed property reads whichever
    //  gene is selected. So gene i of this one is compared against whatever row
    //  the other happened to have selected last - which means two IDENTICAL
    //  multi-gene decisions report as different, unless every gene holds the same
    //  values as every other.
    //
    //  Unreachable from this application: nothing here constructs a
    //  TTwoDimFloatDecision, and the simplex uses TFloatDecision, whose own
    //  Coincide is single-row and correct (see TFloatDecisionTest). Pinned so the
    //  behaviour is on record and a fix is visibly a change.
    Fill(FD, 0);
    Other := TTwoDimFloatDecision.Create(nil);
    try
        Other.GenesNumber := 2;
        Other.ParametersNumber := 3;
        Fill(Other, 0);
        AssertFalse('two identical decisions do NOT compare equal, which is the bug',
            FD.Coincide(Other));

        //  What it does do correctly: with every gene identical, the row mix-up
        //  cannot show, and equal decisions compare equal.
        FD.GenesNumber := 2;
        FD.ParametersNumber := 1;
        Other.GenesNumber := 2;
        Other.ParametersNumber := 1;
        FD.SelectedGene := 0; FD.Parameters[0] := 5;
        FD.SelectedGene := 1; FD.Parameters[0] := 5;
        Other.SelectedGene := 0; Other.Parameters[0] := 5;
        Other.SelectedGene := 1; Other.Parameters[0] := 5;
        AssertTrue('uniform genes compare equal', FD.Coincide(Other));

        //  And a real difference is still detected.
        Other.SelectedGene := 1;
        Other.Parameters[0] := 999;
        AssertFalse('a difference is found', FD.Coincide(Other));
    finally
        Other.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.InvertBlockNegatesARectangle;
begin
    //  Inherited from TTwoDimDecision, which implements it as a loop over
    //  InvertParameter - so this also proves the abstract base's loop bounds are
    //  inclusive at both ends.
    Fill(FD, 1);
    FD.InvertBlock(0, 1, 0, 1);
    FD.SelectedGene := 0;
    AssertEquals('', -1.0, FD.Parameters[0], 1e-12);
    AssertEquals('', -2.0, FD.Parameters[1], 1e-12);
    AssertEquals('outside the block, untouched', 3.0, FD.Parameters[2], 1e-12);
    FD.SelectedGene := 1;
    AssertEquals('', -11.0, FD.Parameters[0], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.CopyBlockShiftsARectangle;
begin
    Fill(FD, 0);
    //  Copy gene 0's first two parameters one gene down.
    FD.CopyBlock(0, 0, 0, 1, 1, 0);
    FD.SelectedGene := 1;
    AssertEquals('', 0.0, FD.Parameters[0], 1e-12);
    AssertEquals('', 1.0, FD.Parameters[1], 1e-12);
    AssertEquals('outside the block, untouched', 12.0, FD.Parameters[2], 1e-12);
    FD.SelectedGene := 0;
    AssertEquals('the source is unchanged', 0.0, FD.Parameters[0], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.CopyBlockWrapsPastTheEnd;
begin
    //  The offsets wrap rather than clipping, which is what makes this usable as a
    //  genetic shift operator - and is invisible unless the offset is tested past
    //  the end. An intermediate buffer is used precisely so source and
    //  destination may overlap.
    Fill(FD, 0);
    FD.CopyBlock(1, 1, 0, 0, 1, 0);   //  gene 1 -> gene 2, which does not exist
    FD.SelectedGene := 0;
    AssertEquals('wrapped round to the first gene', 10.0,
        FD.Parameters[0], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.ExchangeBlocksWithOuterSwapsARectangle;
var
    Other: TTwoDimFloatDecision;
begin
    Fill(FD, 0);
    Other := TTwoDimFloatDecision.Create(nil);
    try
        Other.GenesNumber := 2;
        Other.ParametersNumber := 3;
        Fill(Other, 100);
        FD.ExchangeBlocksWithOuter(Other, 0, 0, 0, 1);
        FD.SelectedGene := 0;
        AssertEquals('', 100.0, FD.Parameters[0], 1e-12);
        AssertEquals('', 101.0, FD.Parameters[1], 1e-12);
        AssertEquals('outside the block', 2.0, FD.Parameters[2], 1e-12);
        Other.SelectedGene := 0;
        AssertEquals('and the other way', 0.0, Other.Parameters[0], 1e-12);
    finally
        Other.Free;
    end;
end;

{ ---- TDecisionsList -------------------------------------------------------- }

procedure TDecisionsListTest.SetUp;
begin
    FList := TDecisionsList.Create;
end;

procedure TDecisionsListTest.TearDown;
begin
    //  TComponentList owns its items, so the list frees the decisions.
    FreeAndNil(FList);
end;

function TDecisionsListTest.Add(const AEvaluation: double): TFloatDecision;
begin
    Result := TFloatDecision.Create(nil);
    Result.ParametersNumber := 1;
    Result.Parameters[0] := AEvaluation;
    Result.Evaluation := AEvaluation;
    FList.Add(Result);
end;

procedure TDecisionsListTest.TheAbsoluteMinimumIsTheSmallestEvaluation;
begin
    Add(3); Add(1); Add(2);
    AssertEquals(1.0, FList.GetAbsoluteMin.Evaluation, 1e-12);
end;

procedure TDecisionsListTest.TheAbsoluteMaximumIsTheLargestEvaluation;
begin
    Add(3); Add(1); Add(2);
    AssertEquals(3.0, FList.GetAbsoluteMax.Evaluation, 1e-12);
end;

procedure TDecisionsListTest.AnEmptyListRefusesToAnswer;
var
    Raised: boolean;
begin
    //  There is no minimum of nothing, and answering nil would make the caller
    //  dereference it - so it is refused by name instead.
    Raised := False;
    try
        FList.GetAbsoluteMin;
    except
        on EDecisionsList do Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TDecisionsListTest.TheMaximumBelowALimitIsFound;
begin
    //  The bracketing search the algorithm uses to walk down a sorted population.
    Add(10); Add(7); Add(4); Add(1);
    AssertEquals('the largest at or under 8', 7.0,
        FList.GetMaxDecision(0, 8).Evaluation, 1e-12);
    AssertEquals('the limit itself counts', 7.0,
        FList.GetMaxDecision(0, 7).Evaluation, 1e-12);
end;

procedure TDecisionsListTest.TheMinimumAboveALimitIsFound;
begin
    Add(1); Add(4); Add(7); Add(10);
    AssertEquals('the smallest at or over 5', 7.0,
        FList.GetMinDecision(0, 5).Evaluation, 1e-12);
    AssertEquals('the limit itself counts', 4.0,
        FList.GetMinDecision(0, 4).Evaluation, 1e-12);
end;

procedure TDecisionsListTest.TheSearchStartsWhereItIsTold;
begin
    //  StartIndex exists so a caller can step through successive brackets without
    //  re-examining what it has already taken.
    Add(10); Add(7); Add(4);
    AssertEquals('from the top', 10.0,
        FList.GetMaxDecision(0, 100).Evaluation, 1e-12);
    AssertEquals('skipping the first', 7.0,
        FList.GetMaxDecision(1, 100).Evaluation, 1e-12);
end;

procedure TDecisionsListTest.NothingUnderTheLimitAnswersNil;
begin
    //  nil, not the closest miss: a bracket with nothing in it has to be
    //  distinguishable from a bracket whose best member is poor.
    Add(10); Add(20);
    AssertTrue('no maximum under 5', FList.GetMaxDecision(0, 5) = nil);
    AssertTrue('no minimum over 50', FList.GetMinDecision(0, 50) = nil);
end;

procedure TDecisionsListTest.MembershipIsByValueNotByIdentity;
var
    Probe: TFloatDecision;
begin
    //  Membership goes through Coincide, so an equal-but-distinct solution counts
    //  as present. That is the point: it is how a population avoids duplicates.
    Add(1); Add(2);
    Probe := TFloatDecision.Create(nil);
    try
        Probe.ParametersNumber := 1;
        Probe.Parameters[0] := 2;
        AssertTrue('an equal decision counts as present',
            FList.HasThisDecision(Probe));
        Probe.Parameters[0] := 99;
        AssertFalse('an unequal one does not', FList.HasThisDecision(Probe));
    finally
        Probe.Free;
    end;
end;

procedure TDecisionsListTest.TheAscendingComparatorOrdersByEvaluation;
var
    Lo, Hi: TFloatDecision;
begin
    Lo := Add(1);
    Hi := Add(2);
    AssertEquals('smaller first', -1, EvalUpSortFunc(Lo, Hi));
    AssertEquals('larger later', 1, EvalUpSortFunc(Hi, Lo));
    AssertEquals('equal is a tie', 0, EvalUpSortFunc(Lo, Lo));
end;

procedure TDecisionsListTest.TheDescendingComparatorIsItsMirror;
var
    Lo, Hi: TFloatDecision;
begin
    //  The two searches above require opposite orderings, so the pair has to be
    //  exact mirrors - not merely "both sort".
    Lo := Add(1);
    Hi := Add(2);
    AssertEquals(EvalUpSortFunc(Lo, Hi), -EvalDownSortFunc(Lo, Hi));
    AssertEquals(EvalUpSortFunc(Hi, Lo), -EvalDownSortFunc(Hi, Lo));
    AssertEquals('equal is a tie either way', 0, EvalDownSortFunc(Lo, Lo));
end;

{ ------------- the four searches, and what one test list concealed ---------- }

{ FOUR SEARCHES OVER THE SAME LIST: the largest evaluation at or under a limit,
  the smallest at or over one, and the absolute extremes. Each is a loop with a
  first-candidate branch and an improvement branch, and in three of the four the
  IMPROVEMENT branch had never run.

  ONE TEST-DATA DECISION DID THAT. Every test here built the same descending list
  - 10, 7, 4, 1 - which is what GetMaxDecision's own comment demands ("Items must
  be sorted by decreasing of estimation value"). On a descending list the first
  eligible candidate IS the answer for the bounded maximum and for the absolute
  maximum, so those loops never improve on it. The list is sorted, the tests pass,
  and half of each function is dead.

  AND THE PRECONDITION TURNS OUT NOT TO BE REQUIRED. The improvement branch is a
  plain linear scan, so the search finds the right answer on an unsorted list too
  - which is worth knowing, because the population these run over is sorted by a
  comparator elsewhere and a change there would otherwise be assumed to break
  them. Asserted rather than assumed, on lists deliberately not in the documented
  order.

  These functions decide which decision the optimiser KEEPS, so an improvement
  branch that quietly did nothing would mean a search that always returns its
  first plausible candidate - a fit that converges, on the wrong member of the
  population, with nothing to show it. }

procedure TDecisionsListTest.TheBoundedMaximumIsFoundOnAnUnsortedList;
begin
    //  ASCENDING, so the first eligible candidate is the WORST one and only the
    //  improvement branch can reach the answer.
    Add(1); Add(4); Add(7); Add(10);
    AssertEquals('the largest at or under 8', 7.0,
        FList.GetMaxDecision(0, 8).Evaluation, 1e-12);
    //  And shuffled, so neither the first nor the last is the answer.
    FList.Clear;
    Add(4); Add(10); Add(7); Add(1);
    AssertEquals('still the largest at or under 8', 7.0,
        FList.GetMaxDecision(0, 8).Evaluation, 1e-12);
end;

procedure TDecisionsListTest.AndTheBoundedMinimum;
begin
    //  The mirror, and it needs a DESCENDING list to reach its improvement
    //  branch - the opposite of what the maximum needs, which is why one list
    //  could never exercise both.
    Add(10); Add(7); Add(4); Add(1);
    AssertEquals('the smallest at or over 3', 4.0,
        FList.GetMinDecision(0, 3).Evaluation, 1e-12);
    FList.Clear;
    Add(7); Add(1); Add(4); Add(10);
    AssertEquals('still the smallest at or over 3', 4.0,
        FList.GetMinDecision(0, 3).Evaluation, 1e-12);
end;

procedure TDecisionsListTest.TheAbsoluteMaximumIsFoundWhenItIsNotFirst;
begin
    //  THE EXISTING TEST HAD ITS MAXIMUM AT INDEX 0, so the comparison that
    //  replaces the running answer never fired. A maximum search that always
    //  returns element zero is right on any descending list and wrong on every
    //  other.
    Add(1); Add(4); Add(10); Add(7);
    AssertEquals('the largest anywhere', 10.0,
        FList.GetAbsoluteMax.Evaluation, 1e-12);
    AssertEquals('and the smallest', 1.0,
        FList.GetAbsoluteMin.Evaluation, 1e-12);
end;

procedure TDecisionsListTest.ABoundedSearchStopsAtTheLimitItself;
begin
    //  THE EARLY EXIT. Nothing can beat a candidate sitting exactly on the
    //  limit, so the scan stops - and it stops from the improvement branch as
    //  well as from the first-candidate one, which is the copy that had never
    //  run. Observable only through the answer: a later, equal candidate would
    //  replace it under the >= comparison if the scan carried on.
    Add(1); Add(8); Add(8); Add(4);
    AssertEquals('the limit itself is the answer', 8.0,
        FList.GetMaxDecision(0, 8).Evaluation, 1e-12);
    AssertTrue('and it is the FIRST one on the limit, not the last',
        FList.GetMaxDecision(0, 8) = FList.Items[1]);
end;

procedure TDecisionsListTest.AmongTiesTheBoundedSearchKeepsTheLast;
begin
    //  '>= Max' RATHER THAN '> Max', which decides which of two equal
    //  candidates is kept. It matters because the decisions are distinct objects
    //  with distinct parameters - equal evaluation, different solution - so the
    //  optimiser carries forward whichever this picks.
    //
    //  Characterised, not endorsed: the two are equally good by the only measure
    //  the search has, and preferring the last is as defensible as the first.
    //  Pinned so that changing the comparison is a decision.
    Add(1); Add(5); Add(5); Add(2);
    AssertTrue('the later of the two equals',
        FList.GetMaxDecision(0, 6) = FList.Items[2]);
end;

procedure TDecisionsListTest.EveryBoundedSearchRefusesAnEmptyList;
var
    MaxRaised, MinRaised: boolean;
begin
    //  ASKED OF BOTH, because the existing empty-list test covered one function
    //  and each carries its own copy of the guard. Refusing rather than
    //  answering nil is right here: an empty population is a broken algorithm
    //  state, not a search that found nothing.
    MaxRaised := False;
    try
        FList.GetMaxDecision(0, 10);
    except
        on E: EDecisionsList do
            MaxRaised := True;
    end;
    MinRaised := False;
    try
        FList.GetMinDecision(0, 0);
    except
        on E: EDecisionsList do
            MinRaised := True;
    end;
    AssertTrue('the bounded maximum refuses', MaxRaised);
    AssertTrue('and the bounded minimum', MinRaised);
end;

procedure TDecisionsListTest.AndSoDoesEveryAbsoluteOne;
var
    MaxRaised, MinRaised: boolean;
begin
    MaxRaised := False;
    try
        FList.GetAbsoluteMax;
    except
        on E: EDecisionsList do
            MaxRaised := True;
    end;
    MinRaised := False;
    try
        FList.GetAbsoluteMin;
    except
        on E: EDecisionsList do
            MinRaised := True;
    end;
    AssertTrue('the absolute maximum refuses', MaxRaised);
    AssertTrue('and the absolute minimum', MinRaised);
end;

{ ------------ the two-dimensional decision's guards, and the wrap ----------- }

{ THE GENETIC REPRESENTATION - genes by parameters - and much less exercised than
  the flat one, because the fits this project ships use the simplex rather than
  the genetic search. That is exactly why its guards matter: the code is live,
  reachable from a build that enables it, and nothing had ever driven its edges.

  A REFUSAL HERE IS AN INDEX ERROR CAUGHT, and the alternative is not a wrong
  answer but a read or write outside a dynamic array - which in a release build
  is whatever happens to be next in memory. }

procedure TTwoDimFloatDecisionTest.ReadingAParameterPastTheEndIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        FD[FD.ParametersNumber];
    except
        on E: ETwoDimFloatDecision do
            Raised := True;
    end;
    AssertTrue('reading past the end is refused', Raised);
end;

procedure TTwoDimFloatDecisionTest.WritingOnePastTheEndIsRefused;
var
    Raised: boolean;
begin
    //  BOTH DIRECTIONS, because they are two guards on two methods - and the
    //  write is the one that would corrupt rather than merely misread.
    Raised := False;
    try
        FD[FD.ParametersNumber] := 1.0;
    except
        on E: ETwoDimFloatDecision do
            Raised := True;
    end;
    AssertTrue('writing past the end is refused', Raised);
end;

procedure TTwoDimFloatDecisionTest.ANegativeParameterIndexIsRefusedBothWays;
var
    ReadRaised, WriteRaised: boolean;
begin
    //  THE OTHER HALF OF EACH GUARD. The two conditions are written as one
    //  expression per method, so a test only for the upper bound leaves the
    //  lower one unasserted - and a negative index into a dynamic array reads
    //  backwards from its first element.
    ReadRaised := False;
    try
        FD[-1];
    except
        on E: ETwoDimFloatDecision do
            ReadRaised := True;
    end;
    WriteRaised := False;
    try
        FD[-1] := 1.0;
    except
        on E: ETwoDimFloatDecision do
            WriteRaised := True;
    end;
    AssertTrue('reading a negative index is refused', ReadRaised);
    AssertTrue('and writing one', WriteRaised);
end;

procedure TTwoDimFloatDecisionTest.CoincideRefusesAForeignType;
var
    Other: TFloatDecision;
    Raised: boolean;
begin
    //  COMPARING TWO REPRESENTATIONS IS MEANINGLESS, and the class uses an
    //  `absolute` alias for the argument - so without this guard a flat decision
    //  would be read as a two-dimensional one and its single array walked as an
    //  array of arrays.
    Other := TFloatDecision.Create(nil);
    try
        Other.ParametersNumber := 3;
        Raised := False;
        try
            FD.Coincide(Other);
        except
            on E: ETwoDimFloatDecision do
                Raised := True;
        end;
        AssertTrue('refused', Raised);
    finally
        Other.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.ExchangeWithOuterRefusesAForeignType;
var
    Other: TFloatDecision;
    Raised: boolean;
begin
    //  THE SAME MISTAKE WITH A WRITE IN IT. This one exchanges values, so a
    //  foreign type accepted would write a Double through the other class's
    //  setter - which is how the byte-decision heap corruption in this unit
    //  happened, and its comment two hundred lines up records it.
    Other := TFloatDecision.Create(nil);
    try
        Other.ParametersNumber := 3;
        Raised := False;
        try
            FD.ExchangeWithOuter(Other, 0, 0, 0);
        except
            on E: ETwoDimFloatDecision do
                Raised := True;
        end;
        AssertTrue('refused', Raised);
    finally
        Other.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.AndItsRefusalNamesTheTypeItWasGiven;
var
    Other: TFloatDecision;
    Msg: string;
begin
    //  THE ONE MESSAGE IN THIS UNIT THAT CARRIES A DETAIL, and it is worth
    //  keeping: the reader is a developer who has passed the wrong decision from
    //  somewhere in a population, and the class name is the only thing that says
    //  where to look.
    Other := TFloatDecision.Create(nil);
    try
        Other.ParametersNumber := 3;
        Msg := '';
        try
            FD.ExchangeWithOuter(Other, 0, 0, 0);
        except
            on E: ETwoDimFloatDecision do
                Msg := E.Message;
        end;
        AssertTrue('it names the class it was handed: ' + Msg,
            Pos('TFloatDecision', Msg) > 0);
    finally
        Other.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.AnUnsizedDecisionHasNoGenesRatherThanFaulting;
var
    D: TTwoDimFloatDecision;
begin
    //  BEFORE EITHER DIMENSION IS SET. The gene count is read from the length of
    //  an array that does not exist yet, so it answers zero rather than
    //  dereferencing - which matters because the population is built by asking
    //  each fresh decision its size.
    D := TTwoDimFloatDecision.Create(nil);
    try
        AssertEquals('no genes yet', 0, D.GenesNumber);
    finally
        D.Free;
    end;
end;

procedure TTwoDimFloatDecisionTest.AForwardWrapRotatesTheGenes;
begin
    //  THE FORWARD WRAP IS CORRECT, and it is asserted here so that the
    //  asymmetry in the next test is a comparison rather than a bare complaint.
    //  Shifting the whole block forward by one gene on a two-gene decision
    //  rotates it: gene 0's row lands on gene 1, and gene 1's wraps to gene 0.
    Fill(FD, 100);
    FD.CopyBlock(0, 1, 0, 2, 1, 0);
    FD.SelectedGene := 1;
    AssertEquals('gene 0 arrived at gene 1', 100.0, FD[0], 1e-12);
    FD.SelectedGene := 0;
    AssertEquals('and gene 1 wrapped round to gene 0', 110.0, FD[0], 1e-12);
end;

procedure TTwoDimFloatDecisionTest.ABackwardWrapDoesNotAndNothingCallsIt;
begin
    //  CHARACTERISED, AND DELIBERATELY NOT CHANGED.
    //
    //  The two wraps are separate expressions. Forward, for an index past the
    //  end, it is `Index - N * (Index div N)`, which is a correct modular wrap -
    //  the test above shows the rotation. Backward, for a negative index, it is
    //  `(N - 1) + (Index + N * (Abs(Index) div N))`, and for Index = -1 that is
    //  N - 2 where a rotation needs N - 1. It is off by one.
    //
    //  So a backward shift of one on a two-gene decision sends BOTH rows to gene
    //  0 - the second overwriting the first - and leaves gene 1 as it was. Data
    //  is lost rather than rotated, and on a genetic search that would duplicate
    //  one gene and drop another, biasing the population with nothing to show it.
    //
    //  WHY IT IS NOT FIXED HERE. CopyBlock has no caller: nothing in either
    //  repository invokes it outside this fixture, and the genetic
    //  representation it belongs to is not what the shipped fits use - they use
    //  the simplex. Rewriting arithmetic in an uncalled method, on my reading of
    //  what it ought to compute rather than on a failure anybody has seen, would
    //  be a guess dressed as a fix. Pinned instead, so the behaviour is known
    //  and a future caller meets a test rather than a surprise.
    Fill(FD, 100);
    FD.CopyBlock(0, 1, 0, 2, -1, 0);
    FD.SelectedGene := 0;
    AssertEquals('both rows landed on gene 0, the later one winning',
        110.0, FD[0], 1e-12);
    FD.SelectedGene := 1;
    AssertEquals('and gene 1 was never written', 110.0, FD[0], 1e-12);
end;

initialization
    //  Unit tests: arrays of numbers and a list, no process and no file.
    RegisterTest('unit', TFloatDecisionTest);
    RegisterTest('unit', TByteDecisionTest);
    RegisterTest('unit', TTwoDimFloatDecisionTest);
    RegisterTest('unit', TDecisionsListTest);
end.
