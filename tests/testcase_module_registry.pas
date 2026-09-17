// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The rules that keep a build's modules unambiguous.)

Every rule here is about ambiguity that would otherwise be resolved by
REGISTRATION ORDER - two modules with one name, two modules claiming one
resource - leaving the loser installed in name only and reachable by nothing. The
registry refuses instead, which is the right choice and was entirely untested:
reaching it needs a module, and the framework deliberately contains none. A mock
module is the whole unlock.

THE REGISTRY IS PROCESS-GLOBAL AND HAS NO UNREGISTER, because a module registers
once at start-up. So every name used below is unique to its test, and counts are
asserted as relative changes - these tests must not care what else ran first.
}
unit testcase_module_registry;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_app_module, mock_app_module, module_registry;

type
    { What became of a registration. Three outcomes, not two: a duplicate NAME is
      neither accepted nor refused - it is ignored, deliberately, because the
      client and the compute server both register the same module. That third
      state also decides who frees the mock. }
    TRegisterOutcome = (roAccepted, roIgnored, roRefused);

    TModuleRegistryTest = class(TTestCase)
    private
        FOwned: TList;
        { A mock the fixture will free, whatever the test does with it. }
        function NewModule(const AName: string): TMockAppModule;
        { Registers AModule and reports what the registry did with it. }
        function Register_(AModule: TMockAppModule): TRegisterOutcome;
        { Shorthand for the common assertion. }
        function Refused(AModule: TMockAppModule): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARegisteredModuleIsFoundByName;
        procedure AModuleIsCountedOnce;
        procedure ANamelessModuleIsRefused;
        procedure ADuplicateNameIsIgnoredNotRefused;
        procedure TwoModulesClaimingOneResourceAreRefused;
        procedure AnUnknownNameIsNotFound;
        procedure TheLookupIsByExactName;
        procedure AResourceReportsWhatItNeeds;
        procedure AnUnknownResourceIsNotFound;
        procedure KnownResourcesNamesWhatCouldHaveBeenAsked;
        procedure RegisteredModulesListsWhatWasRegistered;
        //  Registered with something missing.
        procedure ANilModuleIsRefused;
        procedure AResourceWithNoNameIsRefused;
    end;

implementation

procedure TModuleRegistryTest.SetUp;
begin
    FOwned := TList.Create;
end;

procedure TModuleRegistryTest.TearDown;
var
    i: longint;
begin
    //  THE MOCKS OUTLIVE THE TEST BY NECESSITY. The registry keeps whatever it
    //  accepted for the life of the process and offers no way to take it back, so
    //  a mock that WAS accepted must not be freed - the registry would be left
    //  holding a bare vtable pointer into reclaimed memory (-SIcorba counts no
    //  references; see mock_support). Only the refused ones are freed here.
    for i := 0 to FOwned.Count - 1 do
        TMockAppModule(FOwned[i]).Free;
    FOwned.Free;
    FOwned := nil;
end;

function TModuleRegistryTest.NewModule(const AName: string): TMockAppModule;
begin
    Result := TMockAppModule.Create;
    Result.SetName(AName);
end;

function TModuleRegistryTest.Register_(AModule: TMockAppModule): TRegisterOutcome;
var
    Before: longint;
begin
    Before := ModuleCount;
    try
        RegisterAppModule(AModule);
        //  Accepted or silently ignored - the count is what distinguishes them,
        //  and the distinction matters here for a second reason: only a module the
        //  registry did NOT keep may be freed by this fixture.
        if ModuleCount > Before then
            Result := roAccepted
        else
            Result := roIgnored;
    except
        on EModuleRegistration do
            Result := roRefused;
    end;
    if Result <> roAccepted then
        FOwned.Add(AModule);
end;

function TModuleRegistryTest.Refused(AModule: TMockAppModule): boolean;
begin
    Result := Register_(AModule) = roRefused;
end;

procedure TModuleRegistryTest.ARegisteredModuleIsFoundByName;
var
    M: TMockAppModule;
    Found: IAppModule;
begin
    M := NewModule('reg-found');
    AssertFalse('accepted', Refused(M));
    AssertTrue('and findable', FindModule('reg-found', Found));
    //  Identity through the NAME, not through a cast: IAppModule declares no
    //  AsObject, deliberately - the framework's contract with a module names
    //  nothing beyond what it needs - and the name is the identity the registry
    //  actually guarantees.
    AssertEquals('and it is the one registered', 'reg-found', Found.Name);
end;

procedure TModuleRegistryTest.AModuleIsCountedOnce;
var
    Before: longint;
begin
    Before := ModuleCount;
    AssertFalse('accepted', Refused(NewModule('reg-counted')));
    AssertEquals('the count went up by one', Before + 1, ModuleCount);
end;

procedure TModuleRegistryTest.ANamelessModuleIsRefused;
var
    Before: longint;
begin
    //  A module with no name cannot be found, cannot be reported in an error, and
    //  cannot own a resource prefix. Accepting it would install something the
    //  framework can never refer to again.
    Before := ModuleCount;
    AssertTrue('refused', Refused(NewModule('')));
    AssertEquals('and nothing was stored', Before, ModuleCount);
end;

procedure TModuleRegistryTest.ADuplicateNameIsIgnoredNotRefused;
var
    Before: longint;
    Found: IAppModule;
begin
    //  IGNORED, and this is deliberate rather than sloppy: the same module is
    //  registered by the client and again by the compute server, so a second
    //  registration of one name has to be a no-op. The declaration's comment used
    //  to claim it raised - it never has - which is exactly the kind of thing a
    //  test finds and prose does not.
    AssertTrue('the first is accepted',
        Register_(NewModule('reg-dup')) = roAccepted);
    Before := ModuleCount;
    AssertTrue('the second is ignored, not refused',
        Register_(NewModule('reg-dup')) = roIgnored);
    AssertEquals('nothing was added', Before, ModuleCount);
    AssertTrue('and the first is still the one installed',
        FindModule('reg-dup', Found));
    AssertEquals('', 'reg-dup', Found.Name);
end;

procedure TModuleRegistryTest.TwoModulesClaimingOneResourceAreRefused;
var
    First, Second: TMockAppModule;
    Before: longint;
begin
    //  THE ONE THAT WOULD HURT MOST. Both modules install, both look present, and
    //  which one actually answers the route depends on link order - so the loser
    //  is reachable in name only, and the symptom is a resource that does the
    //  wrong thing rather than a resource that is missing.
    First := NewModule('reg-res-a');
    First.AddResource('shared/detect');
    AssertTrue('the first is accepted', Register_(First) = roAccepted);

    Second := NewModule('reg-res-b');
    Second.AddResource('shared/detect');
    Before := ModuleCount;
    AssertTrue('the second is refused', Refused(Second));
    AssertEquals('and nothing was stored', Before, ModuleCount);
end;

procedure TModuleRegistryTest.AnUnknownNameIsNotFound;
var
    Found: IAppModule;
begin
    //  False, not an exception: asking whether a module is present is an ordinary
    //  question, and a build with no module is the ordinary case.
    AssertFalse(FindModule('no-such-module-anywhere', Found));
end;

procedure TModuleRegistryTest.TheLookupIsByExactName;
var
    Found: IAppModule;
begin
    AssertFalse('accepted', Refused(NewModule('reg-exact')));
    AssertFalse('a prefix is not a match', FindModule('reg-exac', Found));
    AssertFalse('nor a longer name', FindModule('reg-exactly', Found));
    AssertTrue('the name itself is', FindModule('reg-exact', Found));
end;

procedure TModuleRegistryTest.AResourceReportsWhatItNeeds;
var
    M: TMockAppModule;
    Info: TModuleResource;
begin
    //  The framework applies each resource's policy without knowing what the
    //  resource does, so these two flags ARE the contract. Getting LongRunning
    //  wrong is invisible until real data is slow enough to hit the timeout.
    M := NewModule('reg-policy');
    M.AddResource('reg-policy/quick', False, False);
    M.AddResource('reg-policy/slow', True, True);
    AssertFalse('accepted', Refused(M));

    AssertTrue('the quick one is declared', FindModuleResource('reg-policy/quick', Info));
    AssertFalse('needs no sidecar', Info.NeedsPythonSidecar);
    AssertFalse('and is not long running', Info.LongRunning);

    AssertTrue('the slow one is declared', FindModuleResource('reg-policy/slow', Info));
    AssertTrue('needs the sidecar', Info.NeedsPythonSidecar);
    AssertTrue('and is long running', Info.LongRunning);
end;

procedure TModuleRegistryTest.AnUnknownResourceIsNotFound;
var
    Info: TModuleResource;
begin
    AssertFalse(FindModuleResource('nothing/claims-this', Info));
end;

procedure TModuleRegistryTest.KnownResourcesNamesWhatCouldHaveBeenAsked;
var
    M: TMockAppModule;
begin
    //  This exists so a refusal can say what the caller could have asked instead.
    //  An empty or incomplete list turns a helpful error back into "not found".
    M := NewModule('reg-known');
    M.AddResource('reg-known/alpha');
    M.AddResource('reg-known/beta');
    AssertFalse('accepted', Refused(M));

    AssertTrue('the first is listed', Pos('reg-known/alpha', KnownModuleResources) > 0);
    AssertTrue('and the second', Pos('reg-known/beta', KnownModuleResources) > 0);
end;

procedure TModuleRegistryTest.RegisteredModulesListsWhatWasRegistered;
var
    M: TMockAppModule;
    All: TAppModuleArray;
    i: longint;
    Seen: boolean;
begin
    M := NewModule('reg-listed');
    AssertFalse('accepted', Refused(M));
    All := RegisteredModules;
    AssertEquals('the array is as long as the count', ModuleCount, Length(All));
    Seen := False;
    for i := 0 to High(All) do
        if All[i].Name = 'reg-listed' then
            Seen := True;
    AssertTrue('and it contains the module just registered', Seen);
end;

{ ------------------- registered with something missing ---------------------- }

{ WHAT A MODULE MUST BRING. These fire at link time, so the reader is whoever
  wrote the module - and a module accepted with something missing is worse than
  one refused: it appears in the list, its menus are built, and the gap shows up
  later as a resource nobody can address. }

procedure TModuleRegistryTest.ANilModuleIsRefused;
var
    Raised: boolean;
begin
    //  A NIL MODULE IS A LINKAGE MISTAKE, typically a registration written
    //  before the module object exists. Accepted, the first thing to walk the
    //  registry dereferences it - and that walk happens while the main window is
    //  being built, so the program fails to start with no clue why.
    Raised := False;
    try
        RegisterAppModule(nil);
    except
        on E: EModuleRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TModuleRegistryTest.AResourceWithNoNameIsRefused;
var
    M: TMockAppModule;
    Raised: boolean;
begin
    //  A RESOURCE IS ADDRESSED BY NAME over REST, so a nameless one cannot be
    //  read or written by anything. It would sit in the module's declaration
    //  looking like a feature.
    M := NewModule('mod-nameless-resource');
    M.AddResource('');
    Raised := False;
    try
        RegisterAppModule(M);
    except
        on E: EModuleRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
    if Raised then
        FOwned.Add(M);
end;

initialization
    //  A unit test: declarations and a mock, no process and no file.
    RegisterTest('unit', TModuleRegistryTest);
end.
