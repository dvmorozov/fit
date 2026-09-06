// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The problem a session works in, and writing a parameter back to the
curve that still owns it.)

EVERY REQUEST IS SCOPED TO A PROBLEM. The server holds one per session and every
route carries its id, so the very first call of a session has to create one
before it can do anything else - lazily, because the client is constructed long
before anybody loads data. That laziness is the whole of it: created twice, a
session would silently split in half, with the profile in one problem and the
curves in another; never created, every route is malformed.

WRITING A PARAMETER BACK IS THE OTHER HALF, and it is where an index goes stale.

THE ORDER OF THE MODEL IS DERIVED. Curves follow the intervals and the picks
inside them, so a fit that removes a curve, or a pick that adds one, renumbers
everything after it. The parameters table is holding indices taken before that -
it is redrawn from them - so by the time the user finishes typing into a cell,
the index in hand can name a different curve.

Writing by index would then put the value the user typed for one peak into
another peak's width. Both are plausible numbers, both are in range, and the
table shows exactly what was typed. Nothing anywhere says the model just changed
under the edit.

So the index is resolved to the curve's own HANDLE first, and an index that no
longer names a curve is refused with a sentence the user can read - rather than
writing into whichever curve happens to sit there now.
}
unit testcase_service_problem;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    http_fit_service, MyExceptions, mock_http_transport;

type
    TServiceProblemTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        procedure GivenTwoCurves;
        { How many requests so far named APath. }
        function CallsTo(const APath: string): longint;
        { How many times a problem was CREATED - the request to /problems with
          no id after it, which every later route contains as a prefix. }
        function ProblemsCreated: longint;
        { The last request, as the log recorded it. }
        function LastCall: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The problem every request is scoped to.
        procedure AProblemIsCreatedBeforeTheFirstRealRequest;
        procedure ItIsCreatedOnlyOnce;
        procedure EveryRouteCarriesIt;
        procedure AServerThatWillNotCreateOneIsReported;

        //  Writing a parameter back.
        procedure AParameterIsWrittenToTheCurvesOwnHandle;
        procedure TheValueIsSentAtFullPrecision;
        procedure AStaleCurveIndexIsRefused;
        procedure TheRefusalNamesTheIndexTheUserWasEditing;
        procedure TheRefusalIsAMessageNotAFault;
        procedure NothingIsWrittenWhenTheCurveIsGone;

        //  Reading one back the other way.
        procedure AParameterReportsItsNameValueAndKind;
        procedure AParameterOutOfRangeReportsNothingRatherThanStale;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TServiceProblemTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
end;

procedure TServiceProblemTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

procedure TServiceProblemTest.GivenTwoCurves;
begin
    FSvc.Reply('curves',
        '{"curves":[' +
        '{"id":"A1","params":[{"name":"A","value":10.5,"type":1}]},' +
        '{"id":"B2","params":[{"name":"sigma","value":0.25,"type":1}]}' +
        ']}');
end;

function TServiceProblemTest.CallsTo(const APath: string): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
        if Pos(APath, FSvc.Log.Calls[i]) > 0 then
            Inc(Result);
end;

function TServiceProblemTest.ProblemsCreated: longint;
var
    i: longint;
    C: string;
begin
    //  MATCHED AT THE END, because '/problems' is a prefix of every other route
    //  in the session - '/problems/1/state' and the rest - so a plain substring
    //  count would report the whole session as problem creations.
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
    begin
        C := FSvc.Log.Calls[i];
        if (Pos('/problems ', C) > 0) or (Pos('/problems)', C) > 0) then
            Inc(Result);
    end;
end;

function TServiceProblemTest.LastCall: string;
begin
    Result := '';
    if FSvc.Log.Calls.Count > 0 then
        Result := FSvc.Log.Calls[FSvc.Log.Calls.Count - 1];
end;

{ ---- the problem every request is scoped to -------------------------------- }

procedure TServiceProblemTest.AProblemIsCreatedBeforeTheFirstRealRequest;
begin
    //  LAZILY, because the client is constructed at start-up and the first
    //  request may not come for minutes. Creating one eagerly would have every
    //  launch open a session on a server the user may never use.
    FSvc.Reply('state', '{"state":0}');
    FSvc.GetState;
    AssertEquals('a problem was created', 1, ProblemsCreated);
end;

procedure TServiceProblemTest.ItIsCreatedOnlyOnce;
begin
    //  CREATED TWICE, A SESSION SPLITS IN HALF: the profile goes into one
    //  problem and the curves into another, and neither half can see the other.
    //  What the user sees is data that loaded and a fit that has nothing to fit.
    FSvc.Reply('state', '{"state":0}');
    FSvc.GetState;
    FSvc.GetState;
    FSvc.GetState;
    AssertEquals('still just the one', 1, ProblemsCreated);
end;

procedure TServiceProblemTest.EveryRouteCarriesIt;
begin
    //  The id is in the path of every request. A route built without it names a
    //  collection rather than a session, and the server has no way to know
    //  whose data is being asked for.
    FSvc.Reply('state', '{"state":0}');
    FSvc.GetState;
    //  The LOG, not LastUrl: that records the last WRITE, and reading the state
    //  is a GET - so it would still hold the problem-creation POST.
    AssertTrue('the id is in the path: ' + LastCall,
        Pos('/problems/1/', LastCall) > 0);
end;

procedure TServiceProblemTest.AServerThatWillNotCreateOneIsReported;
var
    Raised: boolean;
begin
    //  A SERVER THAT ANSWERED WITHOUT AN ID. Carrying on would build every
    //  subsequent route around an id of -1 and fail on each of them in turn,
    //  so the user would meet a string of failures rather than the one that
    //  actually happened.
    FSvc.Reply('problems', '{"ok":true}');
    Raised := False;
    try
        FSvc.GetState;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused at once', Raised);
end;

{ ---- writing a parameter back ---------------------------------------------- }

procedure TServiceProblemTest.AParameterIsWrittenToTheCurvesOwnHandle;
begin
    //  BY HANDLE, NOT BY THE INDEX THE CALLER HOLDS. The model's order is
    //  derived from the intervals and the picks inside them, so an index taken
    //  before an edit can name a different curve after it.
    GivenTwoCurves;
    FSvc.SetCurveParameter(1, 0, 3.5);
    AssertTrue('the second curve''s handle is in the path: ' + FSvc.LastUrl,
        Pos('/curves/B2/params/0', FSvc.LastUrl) > 0);
end;

procedure TServiceProblemTest.TheValueIsSentAtFullPrecision;
begin
    //  Seventeen digits, which is what it takes to write a double and read the
    //  same one back. Rounded, a value drifts every time the table is refreshed
    //  - and it is refreshed on every fit cycle.
    GivenTwoCurves;
    FSvc.SetCurveParameter(0, 0, 1/3);
    AssertTrue('not rounded: ' + FSvc.LastBody,
        Pos('0.33333333333333', FSvc.LastBody) > 0);
end;

procedure TServiceProblemTest.AStaleCurveIndexIsRefused;
var
    Raised: boolean;
begin
    //  THE DEFECT THIS GUARD EXISTS FOR. Without it the value the user typed
    //  for one peak is written into another peak's parameter: both plausible,
    //  both in range, and the table shows exactly what was typed.
    GivenTwoCurves;
    Raised := False;
    try
        FSvc.SetCurveParameter(9, 0, 3.5);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TServiceProblemTest.TheRefusalNamesTheIndexTheUserWasEditing;
var
    Message_: string;
begin
    //  The user was typing into a row. Told which row is gone, they can see
    //  what happened; told only that something failed, they retype the value
    //  into the same row and it fails again.
    GivenTwoCurves;
    Message_ := '';
    try
        FSvc.SetCurveParameter(9, 0, 3.5);
    except
        on E: Exception do
            Message_ := E.Message;
    end;
    AssertTrue('the index is in the message: ' + Message_,
        Pos('9', Message_) > 0);
    AssertTrue('and it says the value was not changed: ' + Message_,
        Pos('not changed', Message_) > 0);
end;

procedure TServiceProblemTest.TheRefusalIsAMessageNotAFault;
var
    Kind: string;
begin
    //  EUserException, DELIBERATELY. The window's last-resort handler logs a
    //  fault at Fatal and stops the state poll; a model that changed under an
    //  edit is not a fault, and reporting it as one would disconnect the user
    //  from the server over an ordinary race.
    GivenTwoCurves;
    Kind := '';
    try
        FSvc.SetCurveParameter(9, 0, 3.5);
    except
        on E: Exception do
            Kind := E.ClassName;
    end;
    AssertEquals(EUserException.ClassName, Kind);
end;

procedure TServiceProblemTest.NothingIsWrittenWhenTheCurveIsGone;
begin
    //  REFUSED BEFORE ANYTHING IS SENT. A write that went out and was rejected
    //  by the server would be the same defect one layer further away, and the
    //  reply is not read.
    GivenTwoCurves;
    FSvc.Log.Clear;
    try
        FSvc.SetCurveParameter(9, 0, 3.5);
    except
        on Exception do ;
    end;
    AssertEquals('no write went out', 0, CallsTo('/params/'));
end;

{ ---- reading one back the other way ---------------------------------------- }

procedure TServiceProblemTest.AParameterReportsItsNameValueAndKind;
var
    Name: string;
    Value: double;
    Kind: longint;
begin
    //  All three together, because the table needs the name to label the
    //  column, the value to show, and the kind to decide whether the cell may
    //  be edited at all.
    GivenTwoCurves;
    FSvc.GetCurveParameter(1, 0, Name, Value, Kind);
    AssertEquals('the name', 'sigma', Name);
    AssertEquals('the value', 0.25, Value, 1E-9);
    AssertEquals('and the kind', 1, Kind);
end;

procedure TServiceProblemTest.AParameterOutOfRangeReportsNothingRatherThanStale;
var
    Name: string;
    Value: double;
    Kind: longint;
begin
    //  THE OUTPUTS ARE CLEARED FIRST. They are var parameters, so a caller that
    //  reused the variables from the previous cell would otherwise be handed
    //  the previous cell's answer for a parameter that does not exist - the
    //  table would repeat a value down the column and look plausible doing it.
    GivenTwoCurves;
    Name := 'left over';
    Value := 99;
    Kind := 7;
    FSvc.GetCurveParameter(9, 0, Name, Value, Kind);
    AssertEquals('the name was cleared', '', Name);
    AssertEquals('and the value', 0.0, Value, 1E-9);
    AssertEquals('and the kind', 0, Kind);
end;

initialization
    //  A unit test: the service over a mock transport. No socket and no server.
    RegisterTest('unit', TServiceProblemTest);
end.
