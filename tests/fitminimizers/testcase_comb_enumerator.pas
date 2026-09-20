// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Walking every combination of several discrete quantities by one index.)

WHAT IT IS FOR. A search over several discrete choices - which curve type, which
axis, which of n options apiece - is driven by a single integer running from zero
to the number of combinations, and this turns that integer into one index per
quantity. The whole value of the class is that the mapping is a BIJECTION: every
combination reachable exactly once, so a sweep visits each and none twice.

AND ONLY THE ONE-QUANTITY CASE HAD EVER RUN. With a single quantity the
decomposition loop runs zero times - its bound is `ValuesNumber - 2` - and the
combination count takes an addition rather than a multiplication. So the class
was exercised precisely where it does nothing, and the arithmetic it exists for
was untouched: four lines, and they are the product and the div/mod chain.

There was no fixture for it at all; its 94% came from the simplex server's tests
driving it with one quantity.

THE BIJECTION IS THE TEST. Asserting a few hand-computed tuples would pass on an
off-by-one that shifted every tuple by the same amount; walking the whole range
and requiring each tuple to be new, and the count of them to be the count
claimed, would not.
}
unit testcase_comb_enumerator;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, CombEnumerator;

type
    TCombEnumeratorTest = class(TTestCase)
    private
        FE: TCombEnumerator;
        { The tuple for a through index, as 'i0/i1/...'. }
        function TupleAt(ACombination: longint): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  One quantity, which is all that had ever run.
        procedure OneQuantityHasAsManyCombinationsAsItHasValues;
        procedure AndItsIndexIsTheCombinationItself;

        //  Several, which is what it exists for.
        procedure TwoQuantitiesMultiply;
        procedure ThreeQuantitiesMultiplyToo;
        procedure EveryCombinationOfTwoIsReachedExactlyOnce;
        procedure AndOfThree;
        procedure TheLastQuantityVariesFastest;

        //  A quantity with no values, which is arithmetically odd.
        procedure AQuantityWithNoValuesIsAddedRatherThanMultiplied;
    end;

implementation

procedure TCombEnumeratorTest.SetUp;
begin
    FE := TCombEnumerator.Create;
end;

procedure TCombEnumeratorTest.TearDown;
begin
    FreeAndNil(FE);
end;

function TCombEnumeratorTest.TupleAt(ACombination: longint): string;
var
    i: longint;
begin
    FE.CurrentComb := ACombination;
    Result := '';
    for i := 0 to FE.ValuesNumber - 1 do
    begin
        if Result <> '' then
            Result := Result + '/';
        Result := Result + IntToStr(FE.ValueIndex[i]);
    end;
end;

{ ------------------------------- one quantity ------------------------------- }

procedure TCombEnumeratorTest.OneQuantityHasAsManyCombinationsAsItHasValues;
begin
    FE.AddNumberOfValues(4);
    AssertEquals('four values, four combinations', 4, FE.CombNumber);
end;

procedure TCombEnumeratorTest.AndItsIndexIsTheCombinationItself;
var
    k: longint;
begin
    //  THE DEGENERATE CASE, and the only one the class had ever been driven
    //  with. The decomposition loop does not run at all here, so this says
    //  nothing about the arithmetic below - which is the point.
    FE.AddNumberOfValues(4);
    for k := 0 to 3 do
        AssertEquals(Format('combination %d', [k]), IntToStr(k), TupleAt(k));
end;

{ ---------------------------- several quantities ---------------------------- }

procedure TCombEnumeratorTest.TwoQuantitiesMultiply;
begin
    //  THE PRODUCT, which is the line that never ran: with one quantity the
    //  count is accumulated by ADDITION and only a second one reaches the
    //  multiply.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(3);
    AssertEquals('2 x 3', 6, FE.CombNumber);
end;

procedure TCombEnumeratorTest.ThreeQuantitiesMultiplyToo;
begin
    //  A third, because the product is accumulated in a loop and two quantities
    //  exercise only its first iteration.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(3);
    FE.AddNumberOfValues(4);
    AssertEquals('2 x 3 x 4', 24, FE.CombNumber);
end;

procedure TCombEnumeratorTest.EveryCombinationOfTwoIsReachedExactlyOnce;
var
    Seen: TStringList;
    k: longint;
    T: string;
begin
    //  THE BIJECTION, and the reason this is a walk rather than a few expected
    //  tuples: an off-by-one that shifted every tuple by the same amount would
    //  satisfy any hand-computed example that happened to be shifted with it,
    //  and would still visit one combination twice and another never.
    //
    //  A sweep that revisits a combination wastes an evaluation; one that skips
    //  a combination cannot find the answer that lies in it, and reports the
    //  best of what it did visit as though it had looked everywhere.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(3);
    Seen := TStringList.Create;
    try
        for k := 0 to FE.CombNumber - 1 do
        begin
            T := TupleAt(k);
            AssertTrue(Format('combination %d (%s) was already seen', [k, T]),
                Seen.IndexOf(T) < 0);
            Seen.Add(T);
        end;
        AssertEquals('as many distinct tuples as combinations',
            FE.CombNumber, Seen.Count);
    finally
        Seen.Free;
    end;
end;

procedure TCombEnumeratorTest.AndOfThree;
var
    Seen: TStringList;
    k: longint;
    T: string;
begin
    //  THREE DIMENSIONS, because the decomposition is a chain: each step divides
    //  by the number of combinations of everything to its right, and with two
    //  quantities that right-hand count is a single number rather than a
    //  product. Only three dimensions make the inner call compute anything.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(3);
    FE.AddNumberOfValues(4);
    Seen := TStringList.Create;
    try
        for k := 0 to FE.CombNumber - 1 do
        begin
            T := TupleAt(k);
            AssertTrue(Format('combination %d (%s) was already seen', [k, T]),
                Seen.IndexOf(T) < 0);
            Seen.Add(T);
        end;
        AssertEquals('twenty-four distinct tuples', 24, Seen.Count);
    finally
        Seen.Free;
    end;
end;

procedure TCombEnumeratorTest.TheLastQuantityVariesFastest;
begin
    //  THE ORDER, which the bijection tests cannot see: a mapping that varied
    //  the FIRST quantity fastest is also a bijection, and would still be wrong
    //  for any caller that assumes consecutive indices differ in the last
    //  choice - which is what makes a sweep cache-friendly and what makes a
    //  partial sweep cover whole values of the first quantity.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(3);
    AssertEquals('the first combination', '0/0', TupleAt(0));
    AssertEquals('the next moves the LAST index', '0/1', TupleAt(1));
    AssertEquals('and again', '0/2', TupleAt(2));
    AssertEquals('then the first index advances', '1/0', TupleAt(3));
    AssertEquals('the last combination', '1/2', TupleAt(5));
end;

{ ------------------------- a quantity with no values ------------------------ }

procedure TCombEnumeratorTest.AQuantityWithNoValuesIsAddedRatherThanMultiplied;
begin
    //  CHARACTERISED. The count is accumulated with `if Result <> 0 and n <> 0
    //  then Result := Result * n else Result := Result + n`, so a quantity of
    //  zero values does not zero the product - it leaves it alone.
    //
    //  Arithmetically that is not a count of combinations: with a choice that
    //  has no options there are no combinations at all. Treating it as absent is
    //  the more useful answer for a caller assembling a sweep out of optional
    //  quantities, and it is what the code does - but nothing said so, and a
    //  reader would reasonably expect zero.
    FE.AddNumberOfValues(2);
    FE.AddNumberOfValues(0);
    FE.AddNumberOfValues(3);
    AssertEquals('the empty quantity is skipped, not fatal', 6, FE.CombNumber);
end;

initialization
    //  A unit test: a list of sizes in, a tuple out. No optimiser, no fit.
    RegisterTest('unit', TCombEnumeratorTest);
end.
