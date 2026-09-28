// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IAppModule that declares whatever a test needs it to declare.)

WHAT IT IS FOR. module_registry enforces the rules that keep a build's modules
unambiguous - no nameless module, no duplicate name, no two modules claiming one
resource - and every one of those rules is about what a module DECLARES. Reaching
them through a real module means having a real module, which the framework
deliberately does not contain; that is why the registry sat at 0 % while being
load-bearing for every module build.

See mock_support for the -SIcorba lifetime rule: the fixture owns this, nils the
interface first, then frees the object.
}
unit mock_app_module;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, int_app_module, mock_module_session, mock_support;

type
    TMockAppModule = class(TMockBase, IAppModule)
    private
        FName: string;
        FResources: TModuleResourceArray;
        { Created on demand and owned here, because the registry keeps this module
          for the life of the process and the framework will ask it for a session
          the next time any problem is created - long after the test that
          registered it has finished. }
        FSession: TMockModuleSession;
    public
        constructor Create; override;
        destructor Destroy; override;

        { The name this module reports. Settable to the empty string on purpose -
          a nameless module is one of the cases the registry has to refuse. }
        procedure SetName(const AName: string);
        { Declares one more resource. Name is taken verbatim, so a test can
          declare a fully-qualified '<module>/<resource>' or a malformed one. }
        procedure AddResource(const AName: string;
            ANeedsPythonSidecar: boolean = False;
            ALongRunning: boolean = False);

        //  IAppModule
        function Name: string;
        function Resources: TModuleResourceArray;
        { An inert session, never nil.

          NIL WAS THE BUG. The registry has no unregister, so a mock registered by
          a test stays registered - and TFitService.CreateModuleSessions asks every
          registered module for a session as soon as a problem is created, then
          dereferences the answer. Returning nil made twenty-seven REST tests fail
          hundreds of tests away from this file. See mock_module_session. }
        function CreateSession(AHost: TObject): IModuleSession;

        { The one session this module hands out, so a test can script what it
          answers and read back what was posted to it. Created on first ask, as
          CreateSession does, so a test need not create a problem first. }
        function Session: TMockModuleSession;
    end;

implementation

constructor TMockAppModule.Create;
begin
    inherited Create;
    FName := 'mock';
    SetLength(FResources, 0);
end;

destructor TMockAppModule.Destroy;
begin
    FSession.Free;
    inherited;
end;

procedure TMockAppModule.SetName(const AName: string);
begin
    FName := AName;
end;

procedure TMockAppModule.AddResource(const AName: string;
    ANeedsPythonSidecar: boolean; ALongRunning: boolean);
var
    n: longint;
begin
    n := Length(FResources);
    SetLength(FResources, n + 1);
    FResources[n].Name := AName;
    FResources[n].NeedsPythonSidecar := ANeedsPythonSidecar;
    FResources[n].LongRunning := ALongRunning;
end;

function TMockAppModule.Name: string;
begin
    FLog.Note('Name');
    Result := FName;
end;

function TMockAppModule.Resources: TModuleResourceArray;
begin
    FLog.Note('Resources');
    Result := FResources;
end;

function TMockAppModule.Session: TMockModuleSession;
begin
    //  ONE session, reused: a real module makes one per problem, but this one
    //  holds no problem state, and the alternative is leaking one per problem
    //  for the rest of the run.
    if not Assigned(FSession) then
    begin
        FSession := TMockModuleSession.Create;
        FSession.SetKind(FName);
    end;
    Result := FSession;
end;

function TMockAppModule.CreateSession(AHost: TObject): IModuleSession;
begin
    FLog.Note('CreateSession');
    Result := Session;
end;

end.
