// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Members of a point set, addressed one at a time or several at once,
through REST.)

WHAT A MEMBER'S ADDRESS IS. A pick of the curve positions carries the handle of
the curve seeded from it, so it is addressed by that handle. The background
points and the fit interval bounds carry no handle, and need none: one member
per abscissa is the rule both sets are kept by, so the abscissa names the
member. The profile's samples are the data rather than picks, and are refused
by name.

SEVERAL AT ONCE is the set's own address with the members named in the body.
Model > Clear Model is the gesture that needs it: one request per curve, each
rebuilding every curve left, took minutes on a model of a few hundred.
}
unit testcase_point_set_members;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson,
    fit_points_json, testcase_rest_api;

type
    TPointSetMembersRestTest = class(TRestApiTestBase)
    private
        { A problem with two picked curves; their handles, in model order. }
        function ProblemWithTwoCurves(out AFirst, ASecond: string): longint;
        { A problem whose background holds (3.5, 7.25) and (5, 1). }
        function ProblemWithTwoBackgroundPoints: longint;
        function PointsOf(AId: longint; const ASet: string): TPointsData;
    published
        procedure ManyPicksGoInOneRequestWithTheirCurves;
        procedure TheReplySaysHowManyWentAndTheModelLeft;
        procedure NamingNoMemberNeverEmptiesTheSet;
        procedure AnUnknownHandleAmongManyRemovesNothing;
        procedure ABackgroundPointIsAddressedByItsAbscissa;
        procedure AnAbscissaNoMemberHasIs404AndAddsNothing;
        procedure ManyBackgroundPointsGoInOneRequest;
        procedure AFitIntervalBoundIsAddressedByItsAbscissa;
        procedure TheProfilesSamplesAreRefusedByName;
        procedure ASetNobodyKeepsIsRefusedByName;
        //  A MOVE NAMES THE POINT BY WHERE IT IS, and one the set does not
        //  hold is not an instruction to put a point there.
        procedure AMoveFromWhereNoPickIsIs404AndAddsNothing;
        procedure AMoveOfABackgroundPointItDoesNotHoldIs404;
        { The profile keeps its one exception: its table's spare row sends a
          typed value as a move from an empty row (grid_edit), and it is
          added. }
        procedure AMoveIntoTheProfilesSpareRowStillAddsTheSample;
    end;

implementation

function TPointSetMembersRestTest.ProblemWithTwoCurves(
    out AFirst, ASecond: string): longint;
var
    Code: longint;
    R: TJSONObject;
    Curves: TJSONArray;
begin
    Result := ProblemReadyForPicks;
    R := Call('POST', Format('/problems/%d/points/positions', [Result]),
        '{"x":6,"y":1}', Code);
    R.Free;
    R := Call('POST', Format('/problems/%d/points/positions', [Result]),
        '{"x":14,"y":1}', Code);
    R.Free;
    R := Call('GET', Format('/problems/%d/curves', [Result]), '', Code);
    try
        Curves := R.Arrays['curves'];
        AssertEquals('two curves to remove', 2, Curves.Count);
        AFirst := TJSONObject(Curves.Items[0]).Get('id', '');
        ASecond := TJSONObject(Curves.Items[1]).Get('id', '');
    finally
        R.Free;
    end;
end;

function TPointSetMembersRestTest.ProblemWithTwoBackgroundPoints: longint;
var
    Code: longint;
    R: TJSONObject;
begin
    Result := NewProblem;
    R := Call('PUT', Format('/problems/%d/profile', [Result]),
        GaussianProfileJson, Code);
    R.Free;
    R := Call('POST', Format('/problems/%d/points/background', [Result]),
        '{"x":3.5,"y":7.25}', Code);
    R.Free;
    R := Call('POST', Format('/problems/%d/points/background', [Result]),
        '{"x":5,"y":1}', Code);
    R.Free;
    AssertEquals('two background points to start with', 2,
        Length(PointsOf(Result, 'background').X));
end;

function TPointSetMembersRestTest.PointsOf(AId: longint;
    const ASet: string): TPointsData;
var
    Code: longint;
    Body: string;
begin
    FApi.Handle('GET', Format('/problems/%d/%s', [AId, ASet]), '', Code, Body);
    AssertTrue(ASet + ' readable', PointsFromJsonString(Body, Result));
end;

procedure TPointSetMembersRestTest.ManyPicksGoInOneRequestWithTheirCurves;
var
    Id, Code: longint;
    R: TJSONObject;
    A, B: string;
begin
    Id := ProblemWithTwoCurves(A, B);
    R := Call('DELETE', Format('/problems/%d/points/positions', [Id]),
        Format('{"members":["%s","%s"]}', [A, B]), Code);
    try
        AssertEquals('removed: ' + R.AsJSON, 200, Code);
    finally
        R.Free;
    end;
    AssertEquals('no curve left', '', HandlesOf(Id));
    //  AND NO PICK, which is what keeps them gone: the model is rebuilt from
    //  its picks, so one left behind would put its curve straight back.
    AssertEquals('no pick left', 0, Length(PointsOf(Id, 'positions').X));
end;

procedure TPointSetMembersRestTest.TheReplySaysHowManyWentAndTheModelLeft;
var
    Id, Code: longint;
    R: TJSONObject;
    A, B: string;
begin
    Id := ProblemWithTwoCurves(A, B);
    R := Call('DELETE', Format('/problems/%d/points/positions', [Id]),
        Format('{"members":["%s"]}', [A]), Code);
    try
        AssertEquals('removed: ' + R.AsJSON, 200, Code);
        AssertEquals('one went', 1, R.Get('removed', -1));
        AssertEquals('nothing was refused', '', R.Get('refused', '?'));
        //  THE REFRESHED MODEL, as the one-member route answers: the client
        //  needs it next, and asking again would be a second round trip.
        AssertEquals('the curve left', 1, R.Arrays['curves'].Count);
    finally
        R.Free;
    end;
end;

procedure TPointSetMembersRestTest.NamingNoMemberNeverEmptiesTheSet;
var
    Id, Code: longint;
    R: TJSONObject;
    A, B: string;
begin
    //  A DELETE OF THE SET ITSELF is not "remove everything". Emptying a set is
    //  a PUT of an empty one; reading a body that names nothing as "all" would
    //  turn a client's empty selection into a cleared model.
    Id := ProblemWithTwoCurves(A, B);
    R := Call('DELETE', Format('/problems/%d/points/positions', [Id]), '', Code);
    try
        AssertEquals('no body is refused', 400, Code);
    finally
        R.Free;
    end;
    R := Call('DELETE', Format('/problems/%d/points/positions', [Id]),
        '{"members":[]}', Code);
    try
        AssertEquals('an empty list is refused: ' + R.AsJSON, 400, Code);
    finally
        R.Free;
    end;
    AssertEquals('both curves still there', 2, Length(PointsOf(Id, 'positions').X));
end;

procedure TPointSetMembersRestTest.AnUnknownHandleAmongManyRemovesNothing;
var
    Id, Code: longint;
    R: TJSONObject;
    A, B: string;
begin
    //  404 FOR THE WHOLE REQUEST, as PUT /curves answers an unknown handle: the
    //  model has moved on since the caller read it, and removing the rest of
    //  a list drawn up against another model is a guess.
    Id := ProblemWithTwoCurves(A, B);
    R := Call('DELETE', Format('/problems/%d/points/positions', [Id]),
        Format('{"members":["%s","{99999999-9999-9999-9999-999999999999}"]}',
        [A]), Code);
    try
        AssertEquals('not found: ' + R.AsJSON, 404, Code);
    finally
        R.Free;
    end;
    AssertEquals('and nothing went', 2, Length(PointsOf(Id, 'positions').X));
end;

procedure TPointSetMembersRestTest.ABackgroundPointIsAddressedByItsAbscissa;
var
    Id, Code: longint;
    R: TJSONObject;
    Left: TPointsData;
begin
    //  NOTHING BUT THE ABSCISSA. Removing one used to mean posting the point
    //  again, ordinate and all, and letting AddPoint's toggle take it - so a
    //  caller that had the ordinate slightly wrong ADDED a point instead.
    Id := ProblemWithTwoBackgroundPoints;
    R := Call('DELETE', Format('/problems/%d/points/background/3.5', [Id]), '',
        Code);
    try
        AssertEquals('removed: ' + R.AsJSON, 200, Code);
    finally
        R.Free;
    end;
    Left := PointsOf(Id, 'background');
    AssertEquals('one point left', 1, Length(Left.X));
    AssertEquals('the other one', 5, Left.X[0], 1E-12);
end;

procedure TPointSetMembersRestTest.AnAbscissaNoMemberHasIs404AndAddsNothing;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    //  The toggle's danger, closed: an address that names nothing is not an
    //  instruction to create something there.
    Id := ProblemWithTwoBackgroundPoints;
    R := Call('DELETE', Format('/problems/%d/points/background/4', [Id]), '',
        Code);
    try
        AssertEquals('not found: ' + R.AsJSON, 404, Code);
    finally
        R.Free;
    end;
    AssertEquals('nothing added', 2, Length(PointsOf(Id, 'background').X));

    R := Call('DELETE', Format('/problems/%d/points/background/ABC', [Id]), '',
        Code);
    try
        AssertEquals('not an abscissa at all: ' + R.AsJSON, 400, Code);
    finally
        R.Free;
    end;
end;

procedure TPointSetMembersRestTest.ManyBackgroundPointsGoInOneRequest;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    Id := ProblemWithTwoBackgroundPoints;
    R := Call('DELETE', Format('/problems/%d/points/background', [Id]),
        '{"members":[3.5,5]}', Code);
    try
        AssertEquals('removed: ' + R.AsJSON, 200, Code);
        AssertEquals('both went', 2, R.Get('removed', -1));
    finally
        R.Free;
    end;
    AssertEquals('none left', 0, Length(PointsOf(Id, 'background').X));
end;

procedure TPointSetMembersRestTest.AFitIntervalBoundIsAddressedByItsAbscissa;
var
    Id, Code: longint;
    R: TJSONObject;
    Left: TPointsData;
begin
    Id := ProblemReadyForPicks;
    R := Call('DELETE', Format('/problems/%d/points/rfactor-bounds/20', [Id]),
        '', Code);
    try
        AssertEquals('removed: ' + R.AsJSON, 200, Code);
    finally
        R.Free;
    end;
    Left := PointsOf(Id, 'rfactor-bounds');
    AssertEquals('one bound left', 1, Length(Left.X));
    AssertEquals('the other one', 0, Left.X[0], 1E-12);
end;

procedure TPointSetMembersRestTest.TheProfilesSamplesAreRefusedByName;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    //  REFUSED BY NAME, not ignored: the samples are the data the model is
    //  fitted to, replaced as a whole, and a caller asking about them should
    //  learn which sets this answers for rather than get a 404 that reads as
    //  "no such point".
    Id := NewProblem;
    R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
        Code);
    R.Free;
    R := Call('DELETE', Format('/problems/%d/points/profile/1', [Id]), '', Code);
    try
        AssertEquals('refused: ' + R.AsJSON, 400, Code);
        AssertTrue('and it says which sets are: ' + R.AsJSON,
            Pos('background', R.AsJSON) > 0);
    finally
        R.Free;
    end;
end;

procedure TPointSetMembersRestTest.ASetNobodyKeepsIsRefusedByName;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    Id := NewProblem;
    R := Call('DELETE', Format('/problems/%d/points/nonesuch', [Id]),
        '{"members":[1]}', Code);
    try
        AssertEquals('refused: ' + R.AsJSON, 400, Code);
        AssertTrue('naming the set: ' + R.AsJSON, Pos('nonesuch', R.AsJSON) > 0);
    finally
        R.Free;
    end;
end;

procedure TPointSetMembersRestTest.AMoveFromWhereNoPickIsIs404AndAddsNothing;
var
    Id, Code: longint;
    R: TJSONObject;
begin
    //  The fit interval's bounds are 0 and 20; nothing sits at 7.
    Id := ProblemReadyForPicks;
    R := Call('PUT', Format('/problems/%d/points/rfactor-bounds', [Id]),
        '{"prevX":7,"prevY":0,"x":8,"y":0}', Code);
    try
        AssertEquals('not found: ' + R.AsJSON, 404, Code);
        AssertTrue('and says what it looked for: ' + R.AsJSON,
            Pos('rfactor-bounds', R.Get('error', '')) > 0);
    finally
        R.Free;
    end;
    AssertEquals('no bound added', 2, Length(PointsOf(Id, 'rfactor-bounds').X));

    R := Call('PUT', Format('/problems/%d/points/positions', [Id]),
        '{"prevX":7,"prevY":1,"x":8,"y":1}', Code);
    try
        AssertEquals('no pick to move: ' + R.AsJSON, 404, Code);
    finally
        R.Free;
    end;
    AssertEquals('no pick added', 0, Length(PointsOf(Id, 'positions').X));
    AssertEquals('so no curve either', '', HandlesOf(Id));
end;

procedure TPointSetMembersRestTest.AMoveOfABackgroundPointItDoesNotHoldIs404;
var
    Id, Code: longint;
    R: TJSONObject;
    B: TPointsData;
begin
    Id := ProblemWithTwoBackgroundPoints;
    R := Call('PUT', Format('/problems/%d/points/background', [Id]),
        '{"prevX":4,"prevY":1,"x":6,"y":2}', Code);
    try
        AssertEquals('not found: ' + R.AsJSON, 404, Code);
    finally
        R.Free;
    end;
    B := PointsOf(Id, 'background');
    AssertEquals('nothing added', 2, Length(B.X));

    //  And one it holds still moves.
    R := Call('PUT', Format('/problems/%d/points/background', [Id]),
        '{"prevX":5,"prevY":1,"x":6,"y":2}', Code);
    try
        AssertEquals('moved: ' + R.AsJSON, 200, Code);
    finally
        R.Free;
    end;
    B := PointsOf(Id, 'background');
    AssertEquals('still two', 2, Length(B.X));
    AssertEquals('the moved one is where it went', 6, B.X[1], 0);
end;

procedure TPointSetMembersRestTest.AMoveIntoTheProfilesSpareRowStillAddsTheSample;
var
    Id, Code: longint;
    R: TJSONObject;
    Before: longint;
    P: TPointsData;
begin
    //  A profile with no sample at (0, 0), which the empty row would name.
    Id := NewProblem;
    P := Default(TPointsData);
    P.Title := 'profile';
    SetLength(P.X, 3);
    SetLength(P.Y, 3);
    P.X[0] := 1; P.Y[0] := 10;
    P.X[1] := 2; P.Y[1] := 20;
    P.X[2] := 3; P.Y[2] := 10;
    R := Call('PUT', Format('/problems/%d/profile', [Id]), PointsToJsonString(P),
        Code);
    R.Free;
    Before := Length(PointsOf(Id, 'profile').X);
    AssertEquals('the profile the test begins with', 3, Before);
    //  An empty row reads as (0, 0): the value typed there is a new sample.
    R := Call('PUT', Format('/problems/%d/points/profile', [Id]),
        '{"prevX":0,"prevY":0,"x":30,"y":5}', Code);
    try
        AssertEquals('added: ' + R.AsJSON, 200, Code);
    finally
        R.Free;
    end;
    AssertEquals('one sample more', Before + 1, Length(PointsOf(Id, 'profile').X));
end;

initialization
    RegisterTest('unit', TPointSetMembersRestTest);
end.
