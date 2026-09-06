// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which REST routes are heartbeats, and which are the user doing something.)

WHY THIS IS ITS OWN UNIT. These assertions used to live in TLogTest, which is an
INTEGRATION test - it writes log files - so the whole class ran only in the slow
half and none of it counted toward line coverage. The assertions themselves touch
no file, no socket and no process: they call one pure function over string
literals. `rest_polling.pas` therefore sat at 0 % while being, in fact, tested.

That is worth recognising as a shape rather than a one-off: a unit-level
assertion parked in a class that happens to need a fixture is invisible to the
measurement, and the fix is a class of its own rather than a looser rule about
what counts.

What it defends: the client holds a full URL and the server a bare path, and they
must classify the same call identically. A route treated as a heartbeat by one
side and as ordinary by the other produces a log where the two halves of one call
sit at different tiers - so one vanishes and the other looks unanswered.
}
unit testcase_rest_polling;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, rest_polling;

type
    TRestPollingTest = class(TTestCase)
    published
        procedure ABarePathAndAFullUrlAgree;
        procedure AllThreePolledRoutesAreRecognised;
        procedure ATrailingSlashDoesNotHideIt;
        procedure AQueryStringDoesNotHideIt;
        procedure TheMatchIsCaseInsensitive;
        procedure AnActionIsNotAPoll;
        procedure AProfilePushIsNotAPoll;
        procedure ARouteEndingInSomethingElseIsNotAPoll;
        procedure DegenerateInputIsNotAPoll;
    end;

implementation

procedure TRestPollingTest.ABarePathAndAFullUrlAgree;
begin
    //  THE POINT OF THE FUNCTION. Two processes, two spellings of one call.
    AssertEquals('the same call classified the same way',
        IsPolledRoute('/problems/1/state'),
        IsPolledRoute('http://127.0.0.1:8787/problems/1/state'));
    AssertTrue('and both say poll', IsPolledRoute('/problems/1/state'));
end;

procedure TRestPollingTest.AllThreePolledRoutesAreRecognised;
begin
    //  Named individually rather than in a loop: if a fourth polled route is
    //  added, this test should be the thing that has to be edited.
    AssertTrue('state', IsPolledRoute('/problems/1/state'));
    AssertTrue('async', IsPolledRoute('/problems/12/async'));
    AssertTrue('rfactor', IsPolledRoute('/problems/3/rfactor'));
end;

procedure TRestPollingTest.ATrailingSlashDoesNotHideIt;
begin
    AssertTrue('one slash', IsPolledRoute('/problems/1/state/'));
    AssertTrue('more than one', IsPolledRoute('/problems/1/state///'));
end;

procedure TRestPollingTest.AQueryStringDoesNotHideIt;
begin
    AssertTrue('a parameter', IsPolledRoute('/problems/1/state?x=1'));
    AssertTrue('an empty query', IsPolledRoute('/problems/1/state?'));
    AssertTrue('a query after a trailing slash',
        IsPolledRoute('/problems/1/state/?x=1'));
end;

procedure TRestPollingTest.TheMatchIsCaseInsensitive;
begin
    //  The function lower-cases before comparing, so a caller that built the URL
    //  from a differently-cased constant still agrees with the other side.
    AssertTrue('upper', IsPolledRoute('/problems/1/STATE'));
    AssertTrue('mixed', IsPolledRoute('/problems/1/Async'));
end;

procedure TRestPollingTest.AnActionIsNotAPoll;
begin
    //  An action is the user doing something and belongs in the ordinary log.
    AssertFalse(IsPolledRoute('/problems/1/actions/minimize-difference'));
end;

procedure TRestPollingTest.AProfilePushIsNotAPoll;
begin
    //  THE ONE THAT MATTERS MOST: this is the route whose full-profile payload
    //  made every repaint expensive, and it must stay visible in the ordinary log
    //  rather than being demoted to heartbeat noise.
    AssertFalse('profile', IsPolledRoute('/problems/1/profile'));
    AssertFalse('curves', IsPolledRoute('/problems/1/curves'));
end;

procedure TRestPollingTest.ARouteEndingInSomethingElseIsNotAPoll;
begin
    AssertFalse('health', IsPolledRoute('/health'));
    AssertFalse('problems', IsPolledRoute('/problems'));
    //  Only the LAST segment decides, so a polled name earlier in the path must
    //  not carry: this is a route ABOUT state, not the state poll itself.
    AssertFalse('state earlier in the path',
        IsPolledRoute('/problems/state/history'));
end;

procedure TRestPollingTest.DegenerateInputIsNotAPoll;
begin
    //  Not an error either. Both sides call this on whatever they hold, and a
    //  raise here would take down a log call rather than a request.
    AssertFalse('empty', IsPolledRoute(''));
    AssertFalse('a bare slash', IsPolledRoute('/'));
    AssertFalse('only slashes', IsPolledRoute('///'));
    AssertFalse('a bare query', IsPolledRoute('?x=1'));
end;

initialization
    //  A unit test: one pure function over string literals.
    RegisterTest('unit', TRestPollingTest);
end.
