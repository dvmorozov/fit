// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How an analysis pack keeps its own state in a project file without the
framework knowing anything about it.)

THE RULE THIS DEFENDS. A module is a directory plus one registration unit, in a
repository this one has never heard of, and the framework may not name one. So a
project file cannot have a section per module written by the framework - it has
to be a channel a module answers on.

IModuleSession already answers named resources carrying JSON the module defines,
so the project file uses that: one reserved resource name, `project-state`. The
framework asks every module that DECLARES it, stores whatever text comes back
under the module's name, and posts it back verbatim when the project is opened.
It never parses it - parsing would be the framework knowing what a module keeps.

WHY "DECLARES IT" MATTERS. ModuleGet raises when no session answers a resource,
and a module with nothing to keep in a project is an ordinary case, not an error.
The declaration is what tells the two apart - the same way it already decides
whether a resource needs the Python sidecar or is long-running.

A NOTE ON THE MOCK. The registry has no unregister, so a module registered by a
test stays registered for the rest of the run and every problem created afterwards
asks it for a session. That is why the mock declines every resource unless a test
scripts it, and why this fixture unscripts it again in TearDown.

AND A NOTE ON COUNTING. These tests asked how MANY modules answered, which was
true in a build whose only modules are the two mocks below - and false the moment
a real pack declared the resource: the pro build links an analysis pack, which
keeps its own markup here, and every count was one too many. A test that breaks because
the product gained a feature was testing the fixture. They now ask about the
module they registered, by name.
}
unit testcase_project_module_state;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, int_app_module, module_registry, module_project_state,
    fit_service, title_points_set, points_set, fit_project_document, fit_project_session,
    mock_app_module;

type
    TProjectModuleStateTest = class(TTestCase)
    private
        FService: TFitService;
        { Registered once for the whole run, because the registry has no
          unregister - see the unit comment. }
        class var FModule: TMockAppModule;
        class var FSilent: TMockAppModule;
        procedure GivenAProfile;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        { Fit intervals > Auto, split by the model's module (stage 7 of
          fit-performance.md). }
        procedure AModulesProposalReplacesTheIntervals;
        procedure WithoutOneTheFrameworksTypesAreStillSearchedForPeaks;
        procedure AModuleThatDeclaresProjectStateIsAsked;
        procedure ItsDocumentIsStoredUnderItsOwnName;
        procedure AModuleThatDoesNotDeclareItIsNotAsked;
        procedure NoModuleWithAnythingToKeepMeansNoModuleParts;
        procedure TheDocumentIsCarriedAsTextAndNotParsed;
        procedure ARestorePostsItBackToTheModuleThatWroteIt;
        procedure APackThisBuildDoesNotHaveDoesNotStopTheRestore;
    end;

implementation

const
    { Distinctive, because this module outlives the fixture that registered it
      and must not be mistaken for anything else's. }
    ModuleName = 'projstatemock';
    SilentName = 'projstatesilent';

{ What AModule answered, or '' when it was not asked at all. Named rather than
  counted - see the header. }
function StateOf(const AStates: TModuleStateArray;
    const AModule: string): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AStates) do
        if AStates[i].Module = AModule then
            Exit(AStates[i].Content);
end;

{ The section ADoc carries for AModule, or '' when it carries none. }
function DocumentOf(const ADoc: TProjectDocument;
    const AModule: string): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(ADoc.ModuleDocuments) do
        if ADoc.ModuleDocuments[i].Module = AModule then
            Exit(ADoc.ModuleDocuments[i].Content);
end;

procedure TProjectModuleStateTest.SetUp;
begin
    if not Assigned(FModule) then
    begin
        FModule := TMockAppModule.Create;
        FModule.SetName(ModuleName);
        FModule.AddResource(ModuleName + '/' + ProjectStateResource);
        RegisterAppModule(FModule);

        //  ONE THAT KEEPS NOTHING, registered beside it: without this the
        //  "not asked" case would be asserted over an empty registry, which
        //  proves nothing about a build that has modules in it.
        FSilent := TMockAppModule.Create;
        FSilent.SetName(SilentName);
        FSilent.AddResource(SilentName + '/something-else');
        RegisterAppModule(FSilent);
    end;
    FService := TFitService.Create;
end;

procedure TProjectModuleStateTest.TearDown;
begin
    FreeAndNil(FService);
    //  UNSCRIPTED, not freed. The registry still holds both modules and will
    //  ask them for a session whenever any later test creates a problem;
    //  freeing them would leave it holding a dangling reference, and leaving
    //  them scripted would put a module document into every project captured
    //  for the rest of the run.
    if Assigned(FModule) then
        FModule.Session.AnswerResource('', '');
    //  And nothing proposed: a later test creating a problem must not find its
    //  intervals split by a module it never set up.
    if Assigned(FModule) then
        FModule.Session.ProposeIntervals([]);
end;

procedure TProjectModuleStateTest.GivenAProfile;
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 10 do
        P.AddNewPoint(i, 10 + i);
    FService.SetProfilePointsSet(P);
end;

{ A MODEL IS SPLIT BY ITS MODULE. The module whose markup the model is made of
  proposes the intervals, and the proposal REPLACES the ones there - it owns how
  its model divides. Through the service the window's Auto command reaches. }
procedure TProjectModuleStateTest.AModulesProposalReplacesTheIntervals;
var
    B: TPointsSet;
begin
    GivenAProfile;
    FService.AddPointToRFactorBounds(1, 11);
    FModule.Session.ProposeIntervals([2, 5, 6, 9]);
    FService.ComputeCurveBounds;
    B := FService.GetRFactorBounds;
    try
        AssertEquals('the module''s intervals, and only those', 4, B.PointsCount);
        AssertEquals(2.0, B.PointXCoord[0], 0);
        AssertEquals(5.0, B.PointXCoord[1], 0);
        AssertEquals(6.0, B.PointXCoord[2], 0);
        AssertEquals(9.0, B.PointXCoord[3], 0);
    finally
        B.Free;
    end;
end;

{ AND THE FRAMEWORK'S OWN TYPES are searched for peaks when no module proposes,
  exactly as before: a ramp has one stretch above the background fraction. }
procedure TProjectModuleStateTest.WithoutOneTheFrameworksTypesAreStillSearchedForPeaks;
var
    B: TPointsSet;
begin
    GivenAProfile;
    FService.ComputeCurveBounds;
    B := FService.GetRFactorBounds;
    try
        AssertTrue('bounds were found', B.PointsCount >= 2);
    finally
        B.Free;
    end;
end;

procedure TProjectModuleStateTest.AModuleThatDeclaresProjectStateIsAsked;
var
    States: TModuleStateArray;
    Svc: IFitService;
begin
    FModule.Session.AnswerResource(ModuleName + '/' + ProjectStateResource,
        '{"marks":[1,2,3]}');
    GivenAProfile;
    Svc := FService;
    States := Svc.GetModuleProjectStates;
    AssertTrue('the one that declares it was asked',
        StateOf(States, ModuleName) <> '');
end;

procedure TProjectModuleStateTest.ItsDocumentIsStoredUnderItsOwnName;
var
    Doc: TProjectDocument;
begin
    //  Under the module's name, so a project carrying two packs' state keeps
    //  them apart and can hand each back to the pack that wrote it.
    FModule.Session.AnswerResource(ModuleName + '/' + ProjectStateResource,
        '{"marks":[1,2,3]}');
    GivenAProfile;
    Doc := CaptureProject(FService, EmptyProjectClientContext, EmptyProjectDocument);
    AssertEquals('its own document, under its own name', '{"marks":[1,2,3]}',
        DocumentOf(Doc, ModuleName));
    AssertEquals('and it goes to modules/<name>.json',
        'modules/' + ModuleName + '.json', ModulePartName(ModuleName));
end;

procedure TProjectModuleStateTest.AModuleThatDoesNotDeclareItIsNotAsked;
var
    States: TModuleStateArray;
    Svc: IFitService;
begin
    //  AND IT IS NOT AN ERROR. ModuleGet raises when nothing answers, so asking
    //  a module that keeps nothing would turn every save into a failure in any
    //  build with such a module in it.
    FModule.Session.AnswerResource(ModuleName + '/' + ProjectStateResource, '{}');
    GivenAProfile;
    Svc := FService;
    States := Svc.GetModuleProjectStates;
    AssertTrue('the one that declares it was asked',
        StateOf(States, ModuleName) <> '');
    AssertEquals('and the one that does not was not', '',
        StateOf(States, SilentName));
end;

procedure TProjectModuleStateTest.NoModuleWithAnythingToKeepMeansNoModuleParts;
var
    Doc: TProjectDocument;
begin
    //  The ordinary case, and the published framework's own: no module keeps
    //  anything, and a project simply has no module parts.
    GivenAProfile;
    Doc := CaptureProject(FService, EmptyProjectClientContext, EmptyProjectDocument);
    AssertEquals('nothing from either mock', '', DocumentOf(Doc, ModuleName));
    AssertEquals('', '', DocumentOf(Doc, SilentName));
end;

procedure TProjectModuleStateTest.TheDocumentIsCarriedAsTextAndNotParsed;
var
    Doc: TProjectDocument;
    Payload: string;
begin
    //  BYTE FOR BYTE. The framework does not read what a module keeps -
    //  re-encoding it here would BE reading it - so whatever the module wrote
    //  comes back exactly as written, including the parts this build could not
    //  interpret if it tried.
    Payload := '{"waves":[{"leg":"A","x":1.5}],"note":"quoted \\"thing\\""}';
    FModule.Session.AnswerResource(ModuleName + '/' + ProjectStateResource,
        Payload);
    GivenAProfile;
    Doc := CaptureProject(FService, EmptyProjectClientContext, EmptyProjectDocument);
    AssertEquals('unchanged', Payload, DocumentOf(Doc, ModuleName));
end;

procedure TProjectModuleStateTest.ARestorePostsItBackToTheModuleThatWroteIt;
var
    Doc: TProjectDocument;
    Fault: string;
    Applied: boolean;
begin
    //  THE OTHER HALF. A document is worth keeping only if it reaches the
    //  module again, and it must reach the module it came from rather than
    //  whichever one answers first.
    FModule.Session.AnswerResource(ModuleName + '/' + ProjectStateResource,
        '{"marks":[7]}');
    GivenAProfile;
    Doc := CaptureProject(FService, EmptyProjectClientContext, EmptyProjectDocument);

    FreeAndNil(FService);
    FService := TFitService.Create;
    GivenAProfile;
    Applied := ApplyProject(FService, Doc, Fault);
    AssertTrue('applied: ' + Fault, Applied);
    AssertEquals('the module got its own document back', '{"marks":[7]}',
        FModule.Session.PostedPayload);
end;

procedure TProjectModuleStateTest.APackThisBuildDoesNotHaveDoesNotStopTheRestore;
var
    Doc: TProjectDocument;
    Fault: string;
    Applied: boolean;
begin
    //  A PROJECT FROM A RICHER BUILD. Nobody answers that pack's resource, and
    //  ModulePost raises when nobody answers - so before this the whole restore
    //  failed, and a project carrying one analysis pack could not be opened at
    //  all by a build without it.
    //
    //  That is the opposite of what the part-based format is for: the section
    //  stays in the file, a build that does have the pack still finds it, and
    //  everything else opens here meanwhile. Refusing would make one absent
    //  extension cost the user every other thing in their project.
    GivenAProfile;
    Doc := EmptyProjectDocument;
    SetLength(Doc.ModuleDocuments, 1);
    Doc.ModuleDocuments[0].Module := 'a-pack-this-build-has-never-heard-of';
    Doc.ModuleDocuments[0].Content := '{"marks":[1]}';

    Applied := ApplyProject(FService, Doc, Fault);
    AssertTrue('the project still opens: ' + Fault, Applied);
end;

initialization
    //  A unit test: the engine and two mock modules, in process.
    RegisterTest('unit', TProjectModuleStateTest);
end.
