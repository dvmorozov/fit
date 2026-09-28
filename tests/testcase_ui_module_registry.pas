// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The rules that keep a build's UI modules unambiguous.)

WHY IT WAS UNTESTED. int_ui_host is the contract between a feature module and the
application window, and the framework deliberately ships no module - the one that
exists is in a separate private repository. So the registry had nothing to
register and measured zero covered lines, while being what decides whether a
module's menu appears at all.

THE REGISTRY IS PROCESS-WIDE AND HAS NO WAY BACK. Every test therefore uses names
of its own and asserts against a count taken before it registered, never against
an absolute one; and a module the registry KEPT must not be freed, because
-SIcorba counts no references and the registry would be left holding a vtable
pointer into reclaimed memory. See mock_support, and testcase_module_registry,
which has the same shape for the same reason.
}
unit testcase_ui_module_registry;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_ui_host, module_view_types, mock_ui_module;

type
    TUiModuleRegistryTest = class(TTestCase)
    private
        FOwned: TList;
        function NewModule(const AName: string): TMockUiModule;
        { Registers AModule and says whether the registry kept it. Only a module
          it did NOT keep may be freed by this fixture. }
        function WasKept(AModule: TMockUiModule): boolean;
        { True when registration raised. }
        function Refused(AModule: TMockUiModule): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARegisteredModuleIsCountedOnce;
        procedure ARegisteredModuleIsHandedBack;
        procedure ANamelessModuleIsRefused;
        procedure ANilModuleIsRefused;
        procedure TheSameModuleTwiceIsKeptOnce;
        procedure TwoModulesOfTheSameNameAreOneModule;
        procedure TwoDifferentModulesAreBothKept;
        procedure TheRegistrationOrderIsThePresentationOrder;
        procedure ARefusedModuleIsNotCounted;
        procedure TheReturnedArrayIsSafeToWalk;
    end;

implementation

procedure TUiModuleRegistryTest.SetUp;
begin
    FOwned := TList.Create;
end;

procedure TUiModuleRegistryTest.TearDown;
var
    i: longint;
begin
    for i := 0 to FOwned.Count - 1 do
        TMockUiModule(FOwned[i]).Free;
    FOwned.Free;
    FOwned := nil;
end;

function TUiModuleRegistryTest.NewModule(const AName: string): TMockUiModule;
begin
    Result := TMockUiModule.Create(AName);
end;

function TUiModuleRegistryTest.WasKept(AModule: TMockUiModule): boolean;
var
    Before: longint;
begin
    Before := UiModuleCount;
    Result := False;
    try
        RegisterUiModule(AModule);
        Result := UiModuleCount > Before;
    except
        on Exception do
            Result := False;
    end;
    if not Result then
        FOwned.Add(AModule);
end;

function TUiModuleRegistryTest.Refused(AModule: TMockUiModule): boolean;
var
    Before: longint;
begin
    Before := UiModuleCount;
    Result := False;
    try
        RegisterUiModule(AModule);
    except
        on Exception do
            Result := True;
    end;
    AssertEquals('a refusal registers nothing', Before, UiModuleCount);
    if Assigned(AModule) then
        FOwned.Add(AModule);
end;

procedure TUiModuleRegistryTest.ARegisteredModuleIsCountedOnce;
var
    Before: longint;
begin
    Before := UiModuleCount;
    AssertTrue('kept', WasKept(NewModule('ui-counted')));
    AssertEquals('the count went up by one', Before + 1, UiModuleCount);
end;

procedure TUiModuleRegistryTest.ARegisteredModuleIsHandedBack;
var
    Modules: TUiModuleArray;
    i: longint;
    Found: boolean;
begin
    //  Registering and counting is not enough: the host builds its menu from
    //  what RegisteredUiModules hands back, so a module counted but not returned
    //  is a module whose menu never appears.
    AssertTrue('kept', WasKept(NewModule('ui-handed-back')));
    Modules := RegisteredUiModules;
    Found := False;
    for i := 0 to High(Modules) do
        if Modules[i].Name = 'ui-handed-back' then
            Found := True;
    AssertTrue('it is in the list the host walks', Found);
end;

procedure TUiModuleRegistryTest.ANamelessModuleIsRefused;
begin
    //  The name IS the identity - it is what the duplicate check compares and
    //  what a host reports in a diagnostic. A nameless module would collide with
    //  the next nameless one and be silently dropped.
    AssertTrue('refused', Refused(NewModule('')));
end;

procedure TUiModuleRegistryTest.ANilModuleIsRefused;
begin
    //  Accepting it would put a nil in the array the host walks, and the failure
    //  would surface later, while building a menu, naming nothing.
    AssertTrue('refused', Refused(nil));
end;

procedure TUiModuleRegistryTest.TheSameModuleTwiceIsKeptOnce;
var
    M: TMockUiModule;
    Before: longint;
begin
    //  ORDINARY, not an error: registration runs from every host that builds a
    //  window, and one process can build more than one.
    M := NewModule('ui-twice');
    AssertTrue('kept the first time', WasKept(M));
    Before := UiModuleCount;
    RegisterUiModule(M);
    AssertEquals('and ignored the second', Before, UiModuleCount);
end;

procedure TUiModuleRegistryTest.TwoModulesOfTheSameNameAreOneModule;
var
    Second: TMockUiModule;
    Before: longint;
begin
    //  Two DIFFERENT objects claiming one name. The second is ignored rather
    //  than refused, so a build that links a module twice still starts - but it
    //  must not contribute its menu twice, which would give the user two entries
    //  that look identical and only one of which works.
    AssertTrue('the first is kept', WasKept(NewModule('ui-same-name')));
    Before := UiModuleCount;
    Second := NewModule('ui-same-name');
    AssertFalse('the second is not kept', WasKept(Second));
    AssertEquals('and nothing was added', Before, UiModuleCount);
end;

procedure TUiModuleRegistryTest.TwoDifferentModulesAreBothKept;
var
    Before: longint;
begin
    Before := UiModuleCount;
    AssertTrue('the first', WasKept(NewModule('ui-both-a')));
    AssertTrue('the second', WasKept(NewModule('ui-both-b')));
    AssertEquals('both counted', Before + 2, UiModuleCount);
end;

procedure TUiModuleRegistryTest.TheRegistrationOrderIsThePresentationOrder;
var
    Modules: TUiModuleArray;
    FirstAt, SecondAt, i: longint;
begin
    //  The host builds the menu by walking this array, so the order here is the
    //  order the user sees. Anything that reorders it - a hash, a sort - moves
    //  entries between releases for no reason the user can see.
    AssertTrue('the first', WasKept(NewModule('ui-order-1')));
    AssertTrue('the second', WasKept(NewModule('ui-order-2')));
    Modules := RegisteredUiModules;
    FirstAt := -1;
    SecondAt := -1;
    for i := 0 to High(Modules) do
    begin
        if Modules[i].Name = 'ui-order-1' then
            FirstAt := i;
        if Modules[i].Name = 'ui-order-2' then
            SecondAt := i;
    end;
    AssertTrue('both are present', (FirstAt >= 0) and (SecondAt >= 0));
    AssertTrue('and in the order they registered', FirstAt < SecondAt);
end;

procedure TUiModuleRegistryTest.ARefusedModuleIsNotCounted;
var
    Before: longint;
begin
    Before := UiModuleCount;
    AssertTrue('refused', Refused(NewModule('')));
    AssertEquals('the count did not move', Before, UiModuleCount);
end;

procedure TUiModuleRegistryTest.TheReturnedArrayIsSafeToWalk;
var
    Modules: TUiModuleArray;
    i: longint;
begin
    //  Every element is a live module. A nil among them would fault the host
    //  while it builds its window, which is before there is anything to report
    //  the fault in.
    Modules := RegisteredUiModules;
    AssertEquals('the array length matches the count',
        UiModuleCount, Length(Modules));
    for i := 0 to High(Modules) do
    begin
        AssertTrue('element is assigned', Assigned(Modules[i]));
        AssertTrue('and answers its name', Modules[i].Name <> '');
    end;
end;

initialization
    //  A unit test: a mock module and a process-wide list, no window anywhere.
    RegisterTest('unit', TUiModuleRegistryTest);
end.
