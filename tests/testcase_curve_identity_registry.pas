// SPDX-License-Identifier: GPL-3.0-or-later
{ The maintained model's bookkeeping: which instance each pick stands for.

  WHAT THESE TESTS ARE REALLY ABOUT. The server rebuilds every curve instance
  from the picks on every model edit, so the fitted values have to be re-attached
  to objects that did not exist a moment ago. This registry is what says which is
  which. Every failure it can have is silent in the application - a curve quietly
  back at its starting guess, or wearing another curve's shape - so the cases
  below are the ones that used to be unrepresentable rather than the ones that
  are easy to write:

    * a handle survives a rebuild (issuing twice for one pick gives one handle);
    * a handle survives a MOVE, which the hash it replaced could not;
    * a positionless curve, which no pick places, still gets a STABLE handle;
    * a duplicate is reported rather than absorbed;
    * "fitted" means an optimiser ran, not "has values". }
unit testcase_curve_identity_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    curve_instance_id, curve_identity_registry;

type
    TCurveIdentityRegistryTest = class(TTestCase)
    private
        FRegistry: TCurveIdentityRegistry;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What survives a rebuild's pruning
        procedure APickThatIsGoneTakesItsInstanceWithIt;
        procedure APickThatIsStillThereKeepsIts;
        procedure APositionlessInstanceIsNeverPrunedByPicks;
        procedure AnInstanceAMODULEPlacedIsNeverPrunedByPicks;
        procedure AndItKeepsWhatTheFitFoundAboutIt;
        procedure DraggingAPacksMarkupMovesItsInstanceAndKeepsBoth;
        procedure ANewRegistryHoldsNothing;
        procedure APickGetsAHandle;
        procedure IssuingTwiceForOnePickGivesOneHandle;
        procedure DifferentPicksGetDifferentHandles;
        procedure AnUnknownPickHasNoHandle;
        procedure APositionlessCurveGetsAStableHandle;
        procedure PositionlessAndSeededDoNotCollide;
        procedure AMovedPickKeepsItsHandle;
        procedure MovingAPickThatWasNeverBuiltIsNotAnError;
        procedure AMovedFittedPickAsksToBeReseeded;
        procedure AMovedUnfittedPickDoesNotAskToBeReseeded;
        procedure TheReseedRequestIsClearedOnceHonoured;
        procedure ADeletedPickLosesItsHandle;
        procedure DeletingAPickLeavesTheOthersAlone;
        procedure DeletingAnUnknownPickIsNotAnError;
        procedure AHandleCanBeDroppedDirectly;
        procedure AModuleCanBringItsOwnHandle;
        procedure ReAdoptingTheSameHandleForTheSameSeedIsARebuild;
        procedure AnAdoptedHandleFollowsItsMarkupWhenThatMoves;
        procedure AHandleThatWasNeverIssuedIsRefused;
        procedure FittedMeansAnOptimiserRanNotThatValuesExist;
        procedure ANewFitReplacesWhatWasFitted;
        procedure AnIdTheModelNoLongerHoldsIsIgnoredWhenMarkingFitted;
        procedure ClearForgetsEverything;
        procedure TheEntriesCanBeEnumerated;
        procedure EnumeratingPastTheEndIsRefused;
        procedure TheEmptyHandleMatchesNothing;

        //  What deleting one instance has to take with it.
        procedure DeletingAPickPlacedCurveTakesThePickToo;
        procedure DeletingAPositionlessCurveTakesOnlyItsHandle;
        procedure DeletingAModulesCurveTakesTheMarkupThatPlacedIt;
    end;

implementation

{ ---- what deleting one instance has to take with it ---- }

procedure TCurveIdentityRegistryTest.DeletingAPickPlacedCurveTakesThePickToo;
var
    Id: TCurveInstanceId;
begin
    //  Leaving the pick standing would put a fresh instance there on the next
    //  rebuild - the curve back, unfitted, and the deletion undone.
    Id := FRegistry.IssueForSeed(6.0);
    AssertTrue('the pick goes with it',
        RemovalOf(FRegistry.Item(0)) = crPickAndIdentity);
    AssertTrue('and it is the instance just issued', FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.DeletingAPositionlessCurveTakesOnlyItsHandle;
begin
    //  The formula curve: no pick placed it, so there is none to remove, and
    //  what the model holds afterwards is the rebuild's to decide.
    FRegistry.IssueForSlot(0);
    AssertTrue('the handle alone',
        RemovalOf(FRegistry.Item(0)) = crIdentityOnly);
end;

procedure TCurveIdentityRegistryTest.DeletingAModulesCurveTakesTheMarkupThatPlacedIt;
begin
    //  THE CASE THAT HAD NO ANSWER. An instance a pack placed HAS a seed, so
    //  read as pick-placed it would have deleted a pick that placed some other
    //  curve or nothing at all - and the markup would have put the pattern
    //  straight back on the next rebuild, which is a Delete that visibly does
    //  nothing.
    FRegistry.Adopt(NewCurveInstanceId, 42.0, True);
    AssertTrue('the markup, which is the module''s to remove',
        RemovalOf(FRegistry.Item(0)) = crMarkupThatPlacedIt);
end;

procedure TCurveIdentityRegistryTest.DraggingAPacksMarkupMovesItsInstanceAndKeepsBoth;
var
    Id: TCurveInstanceId;
begin
    //  A PACK'S MARKUP CAN BE DRAGGED, and the identity belongs to the markup
    //  rather than to where it happens to sit - so the next rebuild adopts the
    //  same handle at a new position. Both things have to survive that: that a
    //  module placed it (or the next prune drops it) and what a fit found about
    //  it (or a converged model reads as a freshly placed one).
    Id := NewCurveInstanceId;
    FRegistry.Adopt(Id, 42.0, True);
    FRegistry.MarkFitted([Id]);

    FRegistry.Adopt(Id, 77.0, True);

    AssertTrue('still fitted after the move', FRegistry.IsFitted(Id));
    FRegistry.KeepOnlySeeds([]);
    AssertTrue('and still not the pick set''s to drop', FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.APickThatIsGoneTakesItsInstanceWithIt;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(6.0);
    FRegistry.KeepOnlySeeds([14.0]);
    AssertFalse('the pick was deleted, so the instance is gone',
        FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.APickThatIsStillThereKeepsIts;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(6.0);
    FRegistry.KeepOnlySeeds([6.0, 14.0]);
    AssertTrue('still picked, still here', FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.APositionlessInstanceIsNeverPrunedByPicks;
var
    Id: TCurveInstanceId;
begin
    //  The user-defined formula curve: no pick places it, so no pick can make
    //  it stale. It goes when its fit interval does.
    Id := FRegistry.IssueForSlot(0);
    FRegistry.KeepOnlySeeds([]);
    AssertTrue('kept', FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.AnInstanceAMODULEPlacedIsNeverPrunedByPicks;
var
    Id: TCurveInstanceId;
begin
    //  THE DEFECT THIS WAS WRITTEN FOR. An analysis pack builds its instances
    //  from its own markup and picks nothing, so the pick set is EMPTY while
    //  the model is full - and pruning against it dropped every one of them on
    //  every rebuild. The handle was re-adopted immediately afterwards, so the
    //  model looked right and nothing crashed; what was silently lost each time
    //  is below.
    Id := NewCurveInstanceId;
    FRegistry.Adopt(Id, 42.0, True);
    FRegistry.KeepOnlySeeds([]);
    AssertTrue('a module''s instance is not the pick set''s to drop',
        FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.AndItKeepsWhatTheFitFoundAboutIt;
var
    Id: TCurveInstanceId;
begin
    //  WHAT WAS LOST: an instance that an optimiser produced values for was
    //  reported as merely placed after the next rebuild - so a saved project
    //  recorded fitted=false for every pattern in it, and "continue this fit"
    //  could not tell a converged model from a freshly placed one.
    Id := NewCurveInstanceId;
    FRegistry.Adopt(Id, 42.0, True);
    FRegistry.MarkFitted([Id]);
    FRegistry.KeepOnlySeeds([]);
    AssertTrue('still fitted', FRegistry.IsFitted(Id));
end;

procedure TCurveIdentityRegistryTest.SetUp;
begin
    FRegistry := TCurveIdentityRegistry.Create;
end;

procedure TCurveIdentityRegistryTest.TearDown;
begin
    FreeAndNil(FRegistry);
end;

procedure TCurveIdentityRegistryTest.ANewRegistryHoldsNothing;
begin
    AssertEquals('nothing yet', 0, FRegistry.Count);
    AssertFalse('and nothing is fitted', FRegistry.AnyFitted);
end;

procedure TCurveIdentityRegistryTest.APickGetsAHandle;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(12.5);
    AssertTrue('a handle was issued', IsCurveInstanceId(Id));
    AssertEquals('and recorded', 1, FRegistry.Count);
    AssertTrue('and is findable by its pick',
        SameCurveInstanceId(Id, FRegistry.IdForSeed(12.5)));
    AssertTrue('and by itself', FRegistry.Has(Id));
end;

{ THE REBUILD CASE, and the reason the registry exists. Every model edit builds
  the instance again from the same pick; it must come back as the same instance,
  not a second one. }
procedure TCurveIdentityRegistryTest.IssuingTwiceForOnePickGivesOneHandle;
var
    First, Second: TCurveInstanceId;
begin
    First  := FRegistry.IssueForSeed(12.5);
    Second := FRegistry.IssueForSeed(12.5);

    AssertTrue('the same handle came back',
        SameCurveInstanceId(First, Second));
    AssertEquals('and no second instance was invented', 1, FRegistry.Count);
end;

procedure TCurveIdentityRegistryTest.DifferentPicksGetDifferentHandles;
var
    A, B: TCurveInstanceId;
begin
    A := FRegistry.IssueForSeed(1.0);
    B := FRegistry.IssueForSeed(2.0);
    AssertFalse('two picks, two instances', SameCurveInstanceId(A, B));
    AssertEquals('both recorded', 2, FRegistry.Count);
end;

procedure TCurveIdentityRegistryTest.AnUnknownPickHasNoHandle;
begin
    FRegistry.IssueForSeed(1.0);
    AssertFalse('nothing is seeded there',
        IsCurveInstanceId(FRegistry.IdForSeed(99.0)));
end;

{ The user-defined formula curve has no position parameter, so no pick places
  it. A fresh handle on every rebuild would orphan its fitted values every time
  - silently, which is the whole failure mode being removed. }
procedure TCurveIdentityRegistryTest.APositionlessCurveGetsAStableHandle;
var
    First, Second: TCurveInstanceId;
begin
    First  := FRegistry.IssueForSlot(0);
    Second := FRegistry.IssueForSlot(0);

    AssertTrue('the handle is stable across rebuilds',
        SameCurveInstanceId(First, Second));
    AssertEquals('one instance, not two', 1, FRegistry.Count);
    AssertTrue('and it is findable',
        SameCurveInstanceId(First, FRegistry.IdForSlot(0)));

    //  One per fit interval, so a second interval is a second instance.
    AssertFalse('a different interval is a different instance',
        SameCurveInstanceId(First, FRegistry.IssueForSlot(1)));
    AssertFalse('an interval with no curve has no handle',
        IsCurveInstanceId(FRegistry.IdForSlot(7)));
end;

{ A positionless entry has no meaningful seed. Looking one up by seed must not
  find it, or a pick at x=0 would collide with every formula curve. }
procedure TCurveIdentityRegistryTest.PositionlessAndSeededDoNotCollide;
var
    Slot, Seed: TCurveInstanceId;
begin
    Slot := FRegistry.IssueForSlot(0);
    Seed := FRegistry.IssueForSeed(0.0);

    AssertFalse('they are different instances',
        SameCurveInstanceId(Slot, Seed));
    AssertTrue('the pick finds the seeded one',
        SameCurveInstanceId(Seed, FRegistry.IdForSeed(0.0)));
    AssertTrue('and the slot finds the positionless one',
        SameCurveInstanceId(Slot, FRegistry.IdForSlot(0)));
end;

{ WHAT THE HASH COULD NOT DO. The old key was computed from the seed, so moving
  a pick changed it and orphaned everything stored under it - which is why the
  move used to be refused outright. }
procedure TCurveIdentityRegistryTest.AMovedPickKeepsItsHandle;
var
    Before, After: TCurveInstanceId;
begin
    Before := FRegistry.IssueForSeed(10.0);
    AssertTrue('the move was applied', FRegistry.TakeSeedFrom(10.0, 20.0));

    After := FRegistry.IdForSeed(20.0);
    AssertTrue('the instance kept its identity',
        SameCurveInstanceId(Before, After));
    AssertFalse('and is no longer at the old pick',
        IsCurveInstanceId(FRegistry.IdForSeed(10.0)));
    AssertEquals('still one instance', 1, FRegistry.Count);
end;

procedure TCurveIdentityRegistryTest.MovingAPickThatWasNeverBuiltIsNotAnError;
begin
    //  Ordinary: the user may move a pick before anything has been built from
    //  it. Nothing to carry, so nothing to report.
    AssertFalse('nothing was seeded there',
        FRegistry.TakeSeedFrom(5.0, 6.0));
    AssertEquals('and nothing was invented', 0, FRegistry.Count);
end;

{ Moving a fitted pick keeps the shape the optimiser found and takes the
  position from the new pick. Without the flag the curve would be restored at
  its old x0 and the marker would move without the curve. }
procedure TCurveIdentityRegistryTest.AMovedFittedPickAsksToBeReseeded;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(10.0);
    FRegistry.MarkFitted([Id]);
    AssertFalse('nothing to re-seed before the move',
        FRegistry.NeedsReseed(Id));

    FRegistry.TakeSeedFrom(10.0, 20.0);
    AssertTrue('the position must come from the new pick',
        FRegistry.NeedsReseed(Id));
    AssertTrue('but the fit is not discarded', FRegistry.IsFitted(Id));
end;

procedure TCurveIdentityRegistryTest.AMovedUnfittedPickDoesNotAskToBeReseeded;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(10.0);
    FRegistry.TakeSeedFrom(10.0, 20.0);
    //  Nothing was ever found for it, so it is simply built at its new pick.
    AssertFalse('nothing to re-seed from', FRegistry.NeedsReseed(Id));
end;

procedure TCurveIdentityRegistryTest.TheReseedRequestIsClearedOnceHonoured;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(10.0);
    FRegistry.MarkFitted([Id]);
    FRegistry.TakeSeedFrom(10.0, 20.0);
    AssertTrue('asked', FRegistry.NeedsReseed(Id));

    FRegistry.ClearReseed(Id);
    AssertFalse('and not asked twice', FRegistry.NeedsReseed(Id));
    //  A rebuild that re-seeded once must not keep discarding the position on
    //  every later rebuild.
    FRegistry.IssueForSeed(20.0);
    AssertFalse('still not asked', FRegistry.NeedsReseed(Id));
end;

procedure TCurveIdentityRegistryTest.ADeletedPickLosesItsHandle;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(10.0);
    AssertTrue('deleted', FRegistry.RemoveSeed(10.0));
    AssertEquals('and gone', 0, FRegistry.Count);
    AssertFalse('and no longer known', FRegistry.Has(Id));
end;

{ Deleting one pick must leave every other curve's identity - and so its fit -
  exactly where it was. }
procedure TCurveIdentityRegistryTest.DeletingAPickLeavesTheOthersAlone;
var
    A, B, C: TCurveInstanceId;
begin
    A := FRegistry.IssueForSeed(1.0);
    B := FRegistry.IssueForSeed(2.0);
    C := FRegistry.IssueForSeed(3.0);
    FRegistry.MarkFitted([A, B, C]);

    AssertTrue('the middle one goes', FRegistry.RemoveSeed(2.0));

    AssertEquals('two left', 2, FRegistry.Count);
    AssertTrue('the first kept its handle',
        SameCurveInstanceId(A, FRegistry.IdForSeed(1.0)));
    AssertTrue('and the last did too',
        SameCurveInstanceId(C, FRegistry.IdForSeed(3.0)));
    AssertTrue('and both kept their fit', FRegistry.IsFitted(A));
    AssertTrue('both', FRegistry.IsFitted(C));
    AssertFalse('the deleted one is gone', FRegistry.Has(B));
end;

procedure TCurveIdentityRegistryTest.DeletingAnUnknownPickIsNotAnError;
begin
    FRegistry.IssueForSeed(1.0);
    AssertFalse('nothing there to delete', FRegistry.RemoveSeed(9.0));
    AssertEquals('and nothing was disturbed', 1, FRegistry.Count);
end;

procedure TCurveIdentityRegistryTest.AHandleCanBeDroppedDirectly;
var
    Id: TCurveInstanceId;
begin
    //  What a module's builder does when the markup that placed a curve goes:
    //  it knows the handle, not a pick.
    Id := FRegistry.IssueForSlot(0);
    AssertTrue('dropped', FRegistry.RemoveId(Id));
    AssertEquals('and gone', 0, FRegistry.Count);
    AssertFalse('dropping it twice is not an error',
        FRegistry.RemoveId(Id));
end;

{ A module identifies its own curves, so the framework adopts the handle rather
  than issuing one - and the module's nested patterns keep resolving. }
procedure TCurveIdentityRegistryTest.AModuleCanBringItsOwnHandle;
var
    Mine: TCurveInstanceId;
begin
    Mine := NewCurveInstanceId;
    FRegistry.Adopt(Mine, 7.5);

    AssertEquals('recorded', 1, FRegistry.Count);
    AssertTrue('under the handle the module chose', FRegistry.Has(Mine));
    AssertTrue('and findable by its seed',
        SameCurveInstanceId(Mine, FRegistry.IdForSeed(7.5)));
end;

procedure TCurveIdentityRegistryTest.ReAdoptingTheSameHandleForTheSameSeedIsARebuild;
var
    Mine: TCurveInstanceId;
begin
    Mine := NewCurveInstanceId;
    FRegistry.Adopt(Mine, 7.5);
    FRegistry.Adopt(Mine, 7.5);
    AssertEquals('one instance, rebuilt - not two', 1, FRegistry.Count);
end;

{ A MODULE'S MARKUP CAN BE DRAGGED, and its identity belongs to the markup rather
  than to where the markup happens to sit. So the same handle arriving at a new
  seed is that instance having MOVED - the case the whole mechanism exists to
  make survivable - and not two instances claiming one identity.

  Getting this wrong is not theoretical: refusing it made dragging the START of a
  placed pattern raise, where dragging its END did not, for no reason a user
  could see. Two LIVE instances sharing a handle is a different question, and it
  is answered where the curves of one build pass are visible - see
  TFitTask.AddBuiltCurve. }
procedure TCurveIdentityRegistryTest.AnAdoptedHandleFollowsItsMarkupWhenThatMoves;
var
    Mine: TCurveInstanceId;
begin
    Mine := NewCurveInstanceId;
    FRegistry.Adopt(Mine, 7.5);
    FRegistry.MarkFitted([Mine]);

    //  The markup was dragged: same identity, new place.
    FRegistry.Adopt(Mine, 9.0);

    AssertEquals('still one instance', 1, FRegistry.Count);
    AssertTrue('under the same handle', FRegistry.Has(Mine));
    AssertTrue('now found at the new place',
        SameCurveInstanceId(Mine, FRegistry.IdForSeed(9.0)));
    AssertFalse('and not at the old one',
        IsCurveInstanceId(FRegistry.IdForSeed(7.5)));
    //  What the move is FOR: the values found for it are still its own.
    AssertTrue('and it is still the instance the fit produced values for',
        FRegistry.IsFitted(Mine));
end;

procedure TCurveIdentityRegistryTest.AHandleThatWasNeverIssuedIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        FRegistry.Adopt(NoCurveInstanceId, 1.0);
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('an empty handle is not an identity', Raised);
    AssertEquals('and nothing was added', 0, FRegistry.Count);
end;

{ The distinction FCurveAttributes could never make: it held current values
  whether an optimiser had ever run or not. }
procedure TCurveIdentityRegistryTest.FittedMeansAnOptimiserRanNotThatValuesExist;
var
    A, B: TCurveInstanceId;
begin
    A := FRegistry.IssueForSeed(1.0);
    B := FRegistry.IssueForSeed(2.0);

    AssertFalse('placing a curve does not fit it', FRegistry.IsFitted(A));
    AssertFalse('nor the other', FRegistry.AnyFitted);

    FRegistry.MarkFitted([A]);
    AssertTrue('the fitted one is fitted', FRegistry.IsFitted(A));
    AssertFalse('the other is not', FRegistry.IsFitted(B));
    AssertTrue('and the model has a fit in it', FRegistry.AnyFitted);
    AssertTrue('askable by pick too', FRegistry.FittedAtSeed(1.0));
    AssertFalse('and by the other pick', FRegistry.FittedAtSeed(2.0));
    AssertFalse('an unknown pick is not fitted',
        FRegistry.FittedAtSeed(99.0));
end;

procedure TCurveIdentityRegistryTest.ANewFitReplacesWhatWasFitted;
var
    A, B: TCurveInstanceId;
begin
    A := FRegistry.IssueForSeed(1.0);
    B := FRegistry.IssueForSeed(2.0);

    FRegistry.MarkFitted([A]);
    //  A later fit produced values for B only - so A is not carrying optimiser
    //  results any more, whatever the earlier round found.
    FRegistry.MarkFitted([B]);

    AssertFalse('the earlier one no longer counts', FRegistry.IsFitted(A));
    AssertTrue('the later one does', FRegistry.IsFitted(B));
end;

procedure TCurveIdentityRegistryTest.AnIdTheModelNoLongerHoldsIsIgnoredWhenMarkingFitted;
var
    A, Gone: TCurveInstanceId;
begin
    A := FRegistry.IssueForSeed(1.0);
    Gone := NewCurveInstanceId;

    //  A curve whose pick was deleted while the fit ran: the fit still reports
    //  it, the model no longer holds it. Not an error - the user removed it.
    FRegistry.MarkFitted([A, Gone]);

    AssertTrue('the one still in the model is fitted', FRegistry.IsFitted(A));
    AssertFalse('the one that went is not resurrected',
        FRegistry.Has(Gone));
    AssertEquals('and nothing was added', 1, FRegistry.Count);
end;

procedure TCurveIdentityRegistryTest.ClearForgetsEverything;
var
    Id: TCurveInstanceId;
begin
    Id := FRegistry.IssueForSeed(1.0);
    FRegistry.IssueForSlot(0);
    FRegistry.MarkFitted([Id]);

    FRegistry.Clear;

    AssertEquals('a new profile is a new problem', 0, FRegistry.Count);
    AssertFalse('nothing is fitted', FRegistry.AnyFitted);
    AssertFalse('and nothing is known', FRegistry.Has(Id));
end;

procedure TCurveIdentityRegistryTest.TheEntriesCanBeEnumerated;
var
    A: TCurveInstanceId;
    E: TCurveIdentity;
begin
    A := FRegistry.IssueForSeed(3.25);
    FRegistry.MarkFitted([A]);

    E := FRegistry.Item(0);
    AssertTrue('the handle', SameCurveInstanceId(A, E.Id));
    AssertEquals('the seed', 3.25, E.Seed, 1e-12);
    AssertFalse('placed by a pick', E.Positionless);
    AssertTrue('and fitted', E.Fitted);

    FRegistry.IssueForSlot(2);
    E := FRegistry.Item(1);
    AssertTrue('the formula curve has no pick', E.Positionless);
    AssertEquals('and belongs to its interval', 2, E.Slot);
end;

procedure TCurveIdentityRegistryTest.EnumeratingPastTheEndIsRefused;
var
    Raised: longint;

    procedure Expect(APosition: longint);
    begin
        try
            FRegistry.Item(APosition);
        except
            on E: Exception do
                Inc(Raised);
        end;
    end;

begin
    Raised := 0;
    FRegistry.IssueForSeed(1.0);
    Expect(-1);
    Expect(1);
    Expect(99);
    AssertEquals('every out-of-range position is refused', 3, Raised);
end;

{ The empty handle is what an instance carries before one is issued. If it
  matched an entry, every un-identified curve would resolve to the same one. }
procedure TCurveIdentityRegistryTest.TheEmptyHandleMatchesNothing;
begin
    FRegistry.IssueForSeed(1.0);
    AssertFalse('not held', FRegistry.Has(NoCurveInstanceId));
    AssertFalse('not fitted', FRegistry.IsFitted(NoCurveInstanceId));
    AssertFalse('nothing to re-seed', FRegistry.NeedsReseed(NoCurveInstanceId));
    AssertFalse('and not droppable', FRegistry.RemoveId(NoCurveInstanceId));
    //  Clearing it is a no-op rather than a fault: callers clear by handle
    //  without first asking whether the handle is real.
    FRegistry.ClearReseed(NoCurveInstanceId);
    AssertEquals('nothing was disturbed', 1, FRegistry.Count);
end;

initialization
    //  A UNIT test: no process, no filesystem, no optimiser.
    RegisterTest('unit', TCurveIdentityRegistryTest);
end.
