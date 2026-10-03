// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The server's problems: made, found, and released.)

DELETE /problems/<id> releases a problem; every other route finds one by its
id first. Stated here on the registry itself, in process - the routes over it
are tested over HTTP in the integration half.
}
unit testcase_session_registry;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_server_session;

type
    TSessionRegistryTest = class(TTestCase)
    private
        FRegistry: TSessionRegistry;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AProblemIsFoundUntilItIsReleased;
        procedure ReleasingAProblemThatIsNotThereChangesNothing;
        procedure EachProblemHasItsOwnId;
    end;

implementation

procedure TSessionRegistryTest.SetUp;
begin
    FRegistry := TSessionRegistry.Create;
end;

procedure TSessionRegistryTest.TearDown;
begin
    FreeAndNil(FRegistry);
end;

procedure TSessionRegistryTest.AProblemIsFoundUntilItIsReleased;
var
    Id: longint;
begin
    Id := FRegistry.CreateProblem;
    AssertTrue('found', Assigned(FRegistry.Find(Id)));
    FRegistry.Discard(Id);
    AssertFalse('and gone once released', Assigned(FRegistry.Find(Id)));
    AssertEquals(0, FRegistry.Count);
end;

{ A second DELETE, or one for an id another client already released, is not
  an error the server can do anything about. }
procedure TSessionRegistryTest.ReleasingAProblemThatIsNotThereChangesNothing;
var
    Id: longint;
begin
    Id := FRegistry.CreateProblem;
    FRegistry.Discard(Id + 1000);
    AssertEquals('the other problem stays', 1, FRegistry.Count);
    AssertTrue(Assigned(FRegistry.Find(Id)));
end;

procedure TSessionRegistryTest.EachProblemHasItsOwnId;
var
    A, B: longint;
begin
    A := FRegistry.CreateProblem;
    B := FRegistry.CreateProblem;
    AssertTrue(A <> B);
    FRegistry.Discard(A);
    AssertTrue('releasing one leaves the other', Assigned(FRegistry.Find(B)));
end;

initialization
    RegisterTest('unit', TSessionRegistryTest);
end.
