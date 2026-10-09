// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A replacement: a project is built on a new problem and switched to whole.)

WHY. Open Project used to put a document into the one live problem step by step
- the profile first - and a step refused later left the earlier ones done, under
a window still showing the project open before; its next save wrote the mixture
into that project's file (findings.md). Now the HTTP client builds the document
on a NEW problem (BeginReplacement), seeded with the current problem's settings
so that what a document leaves unstated keeps the engine's value as before, and
either switches to it whole (CommitReplacement, the old problem deleted) or
discards it (AbandonReplacement) - leaving the old problem untouched.

These tests read which problem each request addressed.
}
unit testcase_problem_replacement;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, mock_http_transport;

type
    TProblemReplacementTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        function Calls: TStringList;
        function Saw(const ALine: string): boolean;
        { Whether one request line holds both AUrlPart and ABodyPart. }
        function SawOn(const AUrlPart, ABodyPart: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AReplacementAddressesANewProblem;
        procedure ItStartsFromTheCurrentProblemsSettings;
        procedure CommittedTheOldProblemIsDiscarded;
        procedure AbandonedTheOldProblemIsBackUntouched;
    end;

implementation

procedure TProblemReplacementTest.SetUp;
begin
    FSvc := TMockHttpService.Create('http://localhost:8080');
    //  The problem the window has, problem 1.
    FSvc.Reply('problems', '{"ok":true,"id":1}');
    FSvc.GetMaxRFactor;
    //  The next one the server makes is problem 2.
    FSvc.Reply('problems', '{"ok":true,"id":2}');
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.042,"lossKind":1,' +
        '"curveType":"{00000000-0000-0000-0000-000000000001}",' +
        '"modelMixesModules":false}');
end;

procedure TProblemReplacementTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

function TProblemReplacementTest.Calls: TStringList;
begin
    Result := TStringList.Create;
    Result.Text := FSvc.Log.AsText;
end;

function TProblemReplacementTest.Saw(const ALine: string): boolean;
var
    L: TStringList;
    i: longint;
begin
    Result := False;
    L := Calls;
    try
        for i := 0 to L.Count - 1 do
            if Pos(ALine, L[i]) > 0 then
                Exit(True);
    finally
        L.Free;
    end;
end;

function TProblemReplacementTest.SawOn(const AUrlPart,
    ABodyPart: string): boolean;
var
    L: TStringList;
    i: longint;
begin
    Result := False;
    L := Calls;
    try
        for i := 0 to L.Count - 1 do
            if (Pos(AUrlPart, L[i]) > 0) and (Pos(ABodyPart, L[i]) > 0) then
                Exit(True);
    finally
        L.Free;
    end;
end;

procedure TProblemReplacementTest.AReplacementAddressesANewProblem;
begin
    AssertTrue('a replacement over HTTP', FSvc.BeginReplacement);
    FSvc.SetMaxRFactor(0.5);
    AssertTrue('the write went to the new problem',
        SawOn('PUT(http://localhost:8080/problems/2/settings', '5.0000'));
    FSvc.AbandonReplacement;
end;

{ What a document leaves unstated keeps the engine's value - the CURRENT
  problem's, as it did when documents were put into it in place. The curve type
  and the read-only fields stay behind: the document states the type, and a new
  problem holds no model to mix. }
procedure TProblemReplacementTest.ItStartsFromTheCurrentProblemsSettings;
begin
    FSvc.BeginReplacement;
    try
        AssertTrue('the current settings read',
            Saw('GET(http://localhost:8080/problems/1/settings)'));
        AssertTrue('its ceiling, given to the new problem',
            SawOn('PUT(http://localhost:8080/problems/2/settings', '"maxRFactor"'));
        AssertTrue('and its loss',
            SawOn('PUT(http://localhost:8080/problems/2/settings', '"lossKind"'));
        AssertFalse('not its curve type',
            SawOn('PUT(http://localhost:8080/problems/2/settings', '"curveType"'));
        AssertFalse('nor what it only reports',
            SawOn('PUT(http://localhost:8080/problems/2/settings',
            '"modelMixesModules"'));
    finally
        FSvc.AbandonReplacement;
    end;
end;

procedure TProblemReplacementTest.CommittedTheOldProblemIsDiscarded;
begin
    FSvc.BeginReplacement;
    FSvc.CommitReplacement;
    AssertTrue('the old problem deleted',
        Saw('DELETE(http://localhost:8080/problems/1 '));
    FSvc.SetMaxRFactor(0.5);
    AssertTrue('and the new one is the problem now',
        SawOn('PUT(http://localhost:8080/problems/2/settings', '5.0000'));
end;

procedure TProblemReplacementTest.AbandonedTheOldProblemIsBackUntouched;
var
    L: TStringList;
    i: longint;
begin
    FSvc.BeginReplacement;
    FSvc.SetMaxRFactor(0.5);
    FSvc.AbandonReplacement;
    AssertTrue('the new problem deleted',
        Saw('DELETE(http://localhost:8080/problems/2 '));
    FSvc.SetMaxRFactor(0.25);
    AssertTrue('the old one is the problem again',
        SawOn('PUT(http://localhost:8080/problems/1/settings', '2.5000'));
    //  And nothing in between was written to it.
    L := Calls;
    try
        for i := 0 to L.Count - 1 do
            AssertFalse('the old problem was not written while replaced: ' + L[i],
                (Pos('/problems/1/', L[i]) > 0) and (Pos('5.0000', L[i]) > 0));
    finally
        L.Free;
    end;
end;

initialization
    //  A UNIT test: the transport is a double.
    RegisterTest('unit', TProblemReplacementTest);
end.
