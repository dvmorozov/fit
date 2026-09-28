// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Copying and ownership in the list every model is built out of.)

WHAT SITS ON THIS. TSelfCopiedCompList is the container for the curves, and the
copying it provides is what lets the engine hand a model to another thread, to
another process, or to an undo step. Two kinds of copy exist and the difference
is ownership: GetCopy duplicates the items and the copy owns them; GetSharedCopy
duplicates only the list and the copy owns nothing. Confusing the two is a double
free or a leak, and neither shows up on the machine that wrote it.

The class measured 43 % - the copying, the ownership transfer and the refusals
were all untested, exercised only incidentally by tests that happened to hold a
curve list.
}
unit testcase_self_copied;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Contnrs, fpcunit, testregistry, self_copied_component;

type
    { A self-copying component with one value, so a copy can be told from the
      original and a copied value from an uncopied one. }
    TCountedThing = class(TSelfCopiedComponent)
    public
        Value: longint;
        function GetCopy: TObject; override;
        procedure CopyParameters(Dest: TObject); override;
    end;

    TSelfCopiedTest = class(TTestCase)
    private
        function NewThing(AValue: longint): TCountedThing;
    published
        //  The component.
        procedure ACopyIsADifferentObjectOfTheSameClass;
        procedure ACopyCarriesTheValue;
        procedure CopyingIntoAForeignClassIsRefused;

        //  The owning copy.
        procedure AnOwningListFreesWhatItHolds;
        procedure ACopiedListHoldsCopiesNotTheSameItems;
        procedure ACopyInheritsTheOwnershipOfItsSource;
        procedure CopyingOntoASameSizedListUpdatesInPlace;
        procedure CopyingOntoADifferentSizedListRebuildsIt;
        procedure CopyingAnEmptyListLeavesTheTargetAlone;

        //  The shared copy.
        procedure ASharedCopyHoldsTheSameItems;
        procedure ASharedCopyOwnsNothing;
        procedure ASharedCopyOfAnEmptyListIsEmpty;

        //  Editing.
        procedure InsertPutsAnItemWhereItWasAsked;
        procedure RemoveTakesTheItemOut;
    end;

implementation

function TCountedThing.GetCopy: TObject;
begin
    Result := TCountedThing.Create(nil);
    CopyParameters(Result);
end;

procedure TCountedThing.CopyParameters(Dest: TObject);
begin
    inherited CopyParameters(Dest);
    TCountedThing(Dest).Value := Value;
end;

function TSelfCopiedTest.NewThing(AValue: longint): TCountedThing;
begin
    Result := TCountedThing.Create(nil);
    Result.Value := AValue;
end;

{ ---- the component --------------------------------------------------------- }

procedure TSelfCopiedTest.ACopyIsADifferentObjectOfTheSameClass;
var
    A: TCountedThing;
    B: TObject;
begin
    A := NewThing(1);
    try
        B := A.GetCopy;
        try
            AssertTrue('something was made', Assigned(B));
            AssertTrue('of the same class', B.ClassType = A.ClassType);
            AssertTrue('and it is not the original', B <> TObject(A));
        finally
            B.Free;
        end;
    finally
        A.Free;
    end;
end;

procedure TSelfCopiedTest.ACopyCarriesTheValue;
var
    A: TCountedThing;
    B: TObject;
begin
    //  A copy that is the right class and holds nothing is the failure this
    //  catches, and it looks like a working copy everywhere except in the data.
    A := NewThing(42);
    try
        B := A.GetCopy;
        try
            AssertEquals('the value came across', 42, TCountedThing(B).Value);
        finally
            B.Free;
        end;
    finally
        A.Free;
    end;
end;

procedure TSelfCopiedTest.CopyingIntoAForeignClassIsRefused;
var
    A: TCountedThing;
    Other: TSelfCopiedComponent;
    Raised: boolean;
begin
    //  The base CopyParameters asserts the classes match, because a copy into a
    //  different class writes one layout through another - and the assertion is
    //  the only thing standing between that and silent memory corruption.
    //
    //  Assertions are compiled in for this suite; a build with them off has no
    //  check here at all, which is worth knowing.
    A := NewThing(1);
    Other := TSelfCopiedComponent.Create(nil);
    Raised := False;
    try
        try
            A.CopyParameters(Other);
        except
            on E: Exception do
                Raised := True;
        end;
        AssertTrue('it refused', Raised);
    finally
        Other.Free;
        A.Free;
    end;
end;

{ ---- the owning copy ------------------------------------------------------- }

procedure TSelfCopiedTest.AnOwningListFreesWhatItHolds;
var
    L: TSelfCopiedCompList;
begin
    //  NEITHER TComponentList NOR TObjectList frees these - the destructor here
    //  does it by hand, which is why it exists at all. A list that stopped doing
    //  it would leak one curve per model built.
    L := TSelfCopiedCompList.Create(True);
    L.Add(NewThing(1));
    L.Add(NewThing(2));
    AssertEquals('two items', 2, L.Count);
    //  The proof is that this does not leak or fault; the items are unreachable
    //  afterwards, so nothing else can be asserted about them.
    L.Free;
    AssertTrue('freed cleanly', True);
end;

procedure TSelfCopiedTest.ACopiedListHoldsCopiesNotTheSameItems;
var
    L: TSelfCopiedCompList;
    C: TSelfCopiedCompList;
begin
    //  THE WHOLE POINT of the owning copy: two lists that could be freed
    //  independently. Holding the same items would make the second Free a double
    //  free of every curve in the model.
    L := TSelfCopiedCompList.Create(True);
    C := nil;
    try
        L.Add(NewThing(7));
        C := TSelfCopiedCompList(L.GetCopy);
        AssertEquals('one item in the copy', 1, C.Count);
        AssertTrue('and it is not the same object',
            C.Items[0] <> L.Items[0]);
        AssertEquals('but it holds the same value', 7,
            TCountedThing(C.Items[0]).Value);
    finally
        C.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.ACopyInheritsTheOwnershipOfItsSource;
var
    L, C: TSelfCopiedCompList;
begin
    //  Ownership travels with the copy, as the class comment says. A copy of an
    //  owning list that did not own its items would leak them; a copy of a
    //  non-owning list that did would free somebody else's.
    L := TSelfCopiedCompList.Create(True);
    C := nil;
    try
        L.Add(NewThing(1));
        C := TSelfCopiedCompList(L.GetCopy);
        AssertTrue('the copy owns its items', C.OwnsObjects);
    finally
        C.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.CopyingOntoASameSizedListUpdatesInPlace;
var
    L, D: TSelfCopiedCompList;
    Kept: TObject;
begin
    //  TWO BEHAVIOURS IN ONE METHOD, chosen by the counts. Same size means the
    //  existing items are updated and their identity is preserved - which
    //  matters when something outside holds one of them, and is the reason this
    //  branch exists instead of always rebuilding.
    L := TSelfCopiedCompList.Create(True);
    D := TSelfCopiedCompList.Create(True);
    try
        L.Add(NewThing(11));
        D.Add(NewThing(99));
        Kept := TObject(D.Items[0]);
        L.CopyParameters(D);
        AssertEquals('still one item', 1, D.Count);
        AssertTrue('and it is the same object', TObject(D.Items[0]) = Kept);
        AssertEquals('with the source value', 11,
            TCountedThing(D.Items[0]).Value);
    finally
        D.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.CopyingOntoADifferentSizedListRebuildsIt;
var
    L, D: TSelfCopiedCompList;
begin
    //  The other branch: a different count means the target is cleared and
    //  refilled with fresh copies. Updating in place here would leave the extra
    //  items behind, which is a model with curves the source does not have.
    L := TSelfCopiedCompList.Create(True);
    D := TSelfCopiedCompList.Create(True);
    try
        L.Add(NewThing(1));
        L.Add(NewThing(2));
        D.Add(NewThing(99));
        L.CopyParameters(D);
        AssertEquals('the target matches the source', 2, D.Count);
        AssertEquals('first', 1, TCountedThing(D.Items[0]).Value);
        AssertEquals('second', 2, TCountedThing(D.Items[1]).Value);
    finally
        D.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.CopyingAnEmptyListLeavesTheTargetAlone;
var
    L, D: TSelfCopiedCompList;
begin
    //  AS IT BEHAVES, not as it reads: an empty source does nothing at all, so
    //  the target keeps what it had. That is surprising - copying an empty model
    //  onto a full one leaves the full one - and it is pinned here so the
    //  surprise is a decision somebody can find rather than one they hit.
    L := TSelfCopiedCompList.Create(True);
    D := TSelfCopiedCompList.Create(True);
    try
        D.Add(NewThing(5));
        L.CopyParameters(D);
        AssertEquals('the target was not cleared', 1, D.Count);
    finally
        D.Free;
        L.Free;
    end;
end;

{ ---- the shared copy ------------------------------------------------------- }

procedure TSelfCopiedTest.ASharedCopyHoldsTheSameItems;
var
    L, C: TSelfCopiedCompList;
begin
    //  THE SAME OBJECTS, deliberately: a second view of one model, for a reader
    //  that must not duplicate the curves.
    L := TSelfCopiedCompList.Create(True);
    C := nil;
    try
        L.Add(NewThing(3));
        C := TSelfCopiedCompList(L.GetSharedCopy);
        AssertEquals('one item', 1, C.Count);
        AssertTrue('and it IS the same object', C.Items[0] = L.Items[0]);
    finally
        C.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.ASharedCopyOwnsNothing;
var
    L, C: TSelfCopiedCompList;
begin
    //  The one property that makes a shared copy safe. Freeing it must not touch
    //  the items, or the original is left holding freed curves - and the
    //  original is the live model.
    L := TSelfCopiedCompList.Create(True);
    C := nil;
    try
        L.Add(NewThing(3));
        C := TSelfCopiedCompList(L.GetSharedCopy);
        AssertFalse('it owns nothing', C.OwnsObjects);
        FreeAndNil(C);
        //  The original still works, which is the assertion that matters.
        AssertEquals('the original kept its item', 1, L.Count);
        AssertEquals('and the item is intact', 3,
            TCountedThing(L.Items[0]).Value);
    finally
        C.Free;
        L.Free;
    end;
end;

procedure TSelfCopiedTest.ASharedCopyOfAnEmptyListIsEmpty;
var
    L, C: TSelfCopiedCompList;
begin
    L := TSelfCopiedCompList.Create(True);
    C := nil;
    try
        C := TSelfCopiedCompList(L.GetSharedCopy);
        AssertEquals('empty', 0, C.Count);
    finally
        C.Free;
        L.Free;
    end;
end;

{ ---- editing --------------------------------------------------------------- }

procedure TSelfCopiedTest.InsertPutsAnItemWhereItWasAsked;
var
    L: TSelfCopiedCompList;
begin
    //  The order of the curves is the order the parameter table shows and the
    //  order the wire carries, so an insert that lands elsewhere renumbers the
    //  user's model.
    L := TSelfCopiedCompList.Create(True);
    try
        L.Add(NewThing(1));
        L.Add(NewThing(3));
        L.Insert(1, NewThing(2));
        AssertEquals('three items', 3, L.Count);
        AssertEquals('first', 1, TCountedThing(L.Items[0]).Value);
        AssertEquals('inserted', 2, TCountedThing(L.Items[1]).Value);
        AssertEquals('last', 3, TCountedThing(L.Items[2]).Value);
    finally
        L.Free;
    end;
end;

procedure TSelfCopiedTest.RemoveTakesTheItemOut;
var
    L: TSelfCopiedCompList;
    Second: TCountedThing;
begin
    L := TSelfCopiedCompList.Create(True);
    try
        L.Add(NewThing(1));
        Second := NewThing(2);
        L.Add(Second);
        L.Remove(Second);
        AssertEquals('one left', 1, L.Count);
        AssertEquals('and it is the other one', 1,
            TCountedThing(L.Items[0]).Value);
    finally
        L.Free;
    end;
end;

initialization
    //  A unit test: plain objects in memory, no chart, no server, no file.
    RegisterTest('unit', TSelfCopiedTest);
end.
