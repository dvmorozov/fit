// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which routes the compute server answers without taking the problem's
lock.)

THE SERVER IS THREADED and one problem may be reached by several connections at
once, so whatever touches the engine is serialised behind that problem's lock.
Two kinds of request must not be:

  * the progress routes, which the client polls twice a second for as long as it
    is open. Waiting behind the operation they report on defeats the entire
    point of polling them - the client would learn a fit had finished only after
    it had; and

  * DELETE of the problem itself, which destroys the lock. Taking a lock in
    order to free it is a way to wait forever.

BOTH FAILURES ARE INVISIBLE FROM OUTSIDE. Nothing returns an error either way.
Too narrow, and the interface stops updating for the length of every fit, which
a user reports as the application hanging. Too wide, and something that touches
the engine runs unlocked in a threaded server, which is a data race that shows
up as a wrong answer somewhere else entirely.

WHY THE FIRST HALF IS NOT WRITTEN HERE. The three progress route names are
rest_polling's answer, and this function used to carry its own copy of them
matched by a different rule. Two copies of one list is how the fourth polled
route gets added to the documented home and missed here. AnUnlockedRouteIsExactly
APolledRouteOrADeletion states that as a property rather than as a coincidence,
so the copy cannot come back.
}
unit testcase_route_locking;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_rest_api, rest_polling;

type
    TRouteLockingTest = class(TTestCase)
    published
        //  The progress routes.
        procedure TheStateRouteIsAnsweredUnlocked;
        procedure SoAreTheAsyncAndRFactorRoutes;
        procedure TheMethodDoesNotMatterForAProgressRead;

        //  Deleting the problem.
        procedure DeletingTheProblemIsUnlocked;
        procedure ButOnlyTheProblemItself;
        procedure AndOnlyForDelete;

        //  Everything else.
        procedure AnActionTakesTheLock;
        procedure ReadingTheModelTakesTheLock;
        procedure WritingPointsTakesTheLock;
        procedure TheProblemsCollectionItselfTakesNoProblemLock;

        //  The property that keeps the two lists from drifting apart.
        procedure AnUnlockedRouteIsExactlyAPolledRouteOrADeletion;

        //  Inherited from rest_polling, and new here: the old copy could do
        //  neither.
        procedure AFullUrlAnswersTheSameAsABarePath;
        procedure TheProgressNamesAreMatchedWhateverTheirCase;
    end;

implementation

{ ---------------------------- the progress routes --------------------------- }

procedure TRouteLockingTest.TheStateRouteIsAnsweredUnlocked;
begin
    AssertTrue('state is polled and must not wait',
        IsUnlockedRoute('GET', '/problems/1/state'));
end;

procedure TRouteLockingTest.SoAreTheAsyncAndRFactorRoutes;
begin
    AssertTrue('async', IsUnlockedRoute('GET', '/problems/1/async'));
    AssertTrue('rfactor', IsUnlockedRoute('GET', '/problems/1/rfactor'));
end;

procedure TRouteLockingTest.TheMethodDoesNotMatterForAProgressRead;
begin
    //  The progress half is about the PATH. A method test there would make the
    //  answer depend on something the polling contract says nothing about.
    AssertTrue('GET', IsUnlockedRoute('GET', '/problems/7/state'));
    AssertTrue('POST', IsUnlockedRoute('POST', '/problems/7/state'));
    AssertTrue('DELETE', IsUnlockedRoute('DELETE', '/problems/7/state'));
end;

{ --------------------------- deleting the problem --------------------------- }

procedure TRouteLockingTest.DeletingTheProblemIsUnlocked;
begin
    //  It destroys the lock. Taking one in order to free it is a way to wait
    //  forever; the registry's own lock guards this instead.
    AssertTrue('the problem itself',
        IsUnlockedRoute('DELETE', '/problems/1'));
end;

procedure TRouteLockingTest.ButOnlyTheProblemItself;
begin
    //  Deleting something INSIDE the problem leaves the problem - and its lock
    //  - standing, and touches the engine, so it must be serialised.
    AssertFalse('a curve', IsUnlockedRoute('DELETE', '/problems/1/curves/3'));
    AssertFalse('a point',
        IsUnlockedRoute('DELETE', '/problems/1/profile/points/2'));
end;

procedure TRouteLockingTest.AndOnlyForDelete;
begin
    //  Reading or replacing the problem is ordinary engine work at the same
    //  path; only the destruction is special.
    AssertFalse('GET', IsUnlockedRoute('GET', '/problems/1'));
    AssertFalse('POST', IsUnlockedRoute('POST', '/problems/1'));
    AssertFalse('PUT', IsUnlockedRoute('PUT', '/problems/1'));
end;

{ ------------------------------ everything else ----------------------------- }

procedure TRouteLockingTest.AnActionTakesTheLock;
begin
    //  Starting a fit is the operation the progress routes report ON. If this
    //  were unlocked there would be nothing to serialise against.
    AssertFalse('fit', IsUnlockedRoute('POST', '/problems/1/actions/fit'));
end;

procedure TRouteLockingTest.ReadingTheModelTakesTheLock;
begin
    //  A read of the model mid-fit would see it half-rebuilt. These are not
    //  polled, so nothing is waiting on them twice a second.
    AssertFalse('curves', IsUnlockedRoute('GET', '/problems/1/curves'));
    AssertFalse('calc profile',
        IsUnlockedRoute('GET', '/problems/1/calc-profile'));
end;

procedure TRouteLockingTest.WritingPointsTakesTheLock;
begin
    AssertFalse('profile', IsUnlockedRoute('POST', '/problems/1/profile'));
    AssertFalse('positions',
        IsUnlockedRoute('POST', '/problems/1/points/positions'));
    //  AND SO DOES REMOVING ONE. It is a DELETE, and the one unlocked DELETE is
    //  the problem itself - which is exactly the mistake this asserts against,
    //  because a delete that edits the model under a running fit is what the
    //  lock exists for.
    AssertFalse('one position',
        IsUnlockedRoute('DELETE', '/problems/1/points/positions/A1B2'));
end;

procedure TRouteLockingTest.TheProblemsCollectionItselfTakesNoProblemLock;
begin
    //  One segment: there is no problem id in it, so there is no problem lock
    //  to take. The caller only consults this when it HAS found a session, so
    //  the answer here is academic - stated because a future caller reading it
    //  as "unlocked" would be reading it wrong.
    AssertFalse('creating a problem', IsUnlockedRoute('POST', '/problems'));
end;

{ ------------------- the property that stops the drift ---------------------- }

procedure TRouteLockingTest.AnUnlockedRouteIsExactlyAPolledRouteOrADeletion;
const
    Paths: array[0..8] of string = (
        '/problems/1/state', '/problems/1/async', '/problems/1/rfactor',
        '/problems/1/curves', '/problems/1/actions/fit', '/problems/1',
        '/problems', '/problems/1/profile', '/problems/1/calc-profile');
var
    i: longint;
    Expected: boolean;
begin
    //  THE ANTI-DUPLICATION PROPERTY. The three progress names are not written
    //  in this file and not written in fit_rest_api either - both ask
    //  rest_polling. Stating the relation as an identity means a fourth polled
    //  route added to rest_polling is unlocked here automatically, and a copy
    //  reintroduced into IsUnlockedRoute fails as soon as the two disagree.
    for i := 0 to High(Paths) do
    begin
        Expected := IsPolledRoute(Paths[i]);
        AssertEquals('GET ' + Paths[i], Expected,
            IsUnlockedRoute('GET', Paths[i]));
    end;
end;

{ ------------------------ inherited from rest_polling ----------------------- }

procedure TRouteLockingTest.AFullUrlAnswersTheSameAsABarePath;
begin
    //  NEW, and free: the copy this replaced split the path into segments and
    //  counted them, so a full URL had a scheme and a host among its segments
    //  and could never match. Nothing passes a URL here today; it answers the
    //  same as the bare path now if anything ever does.
    AssertEquals('the same call classified the same way',
        IsUnlockedRoute('GET', '/problems/1/state'),
        IsUnlockedRoute('GET', 'http://127.0.0.1:8787/problems/1/state'));
    AssertTrue('and both say unlocked',
        IsUnlockedRoute('GET', 'http://127.0.0.1:8787/problems/1/state'));
end;

procedure TRouteLockingTest.TheProgressNamesAreMatchedWhateverTheirCase;
begin
    //  Also new: the copy compared the segment exactly, so a client spelling
    //  the route with a capital would have been serialised behind the fit it
    //  was polling.
    AssertTrue('State', IsUnlockedRoute('GET', '/problems/1/State'));
    AssertTrue('RFACTOR', IsUnlockedRoute('GET', '/problems/1/RFACTOR'));
end;

initialization
    //  Unit tests: one predicate over two strings. No server, no socket, no
    //  problem registry.
    RegisterTest('unit', TRouteLockingTest);
end.
