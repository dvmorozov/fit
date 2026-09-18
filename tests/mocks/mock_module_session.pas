// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IModuleSession that holds nothing and objects to nothing.)

WHY IT HAS TO EXIST, and it is a lesson rather than a convenience. The module
registry is process-global and has no unregister, so a test that registers a mock
module leaves it registered for the rest of the run - and TFitService.
CreateModuleSessions calls CreateSession on EVERY registered module the moment a
problem is created, then dereferences what it got. A mock returning nil therefore
did not fail its own test: it made every later problem creation raise, and
twenty-seven REST tests failed several hundred tests away from the cause.

So a mock module registered into a live registry must behave like a real module
that simply has nothing in it. Every answer below is the "nothing marked yet"
answer a real module gives before the user has touched it:

  * PointSink is nil, which the framework already handles - a module that
    collects no points returns nil by contract;
  * TryGet and TryPost decline, so the caller tries the next module and then
    reports that nothing owns the resource;
  * TryRemoveInstance declines, which is the honest answer for a module that
    placed nothing - claiming a handle would report a deletion as done;
  * ContributesFitReadiness is False - this module is not an alternative way to
    describe the model;
  * CheckFitAllowed and CheckIntervalAllowed stay silent, which means "no
    objection"; raising here would forbid fits the framework should allow;
  * SliceForInterval is nil - nothing in that stretch.

See mock_support for the -SIcorba lifetime rule.
}
unit mock_module_session;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, int_app_module, mock_support;

type
    TMockModuleSession = class(TMockBase, IModuleSession)
    private
        FKind: string;
        FResource: string;
        FJson: string;
        FPosted: string;
    public
        constructor Create; override;
        procedure SetKind(const AKind: string);
        { Makes this session answer AResource with AJson, and nothing else.

          SCRIPTED RATHER THAN ALWAYS-ON, and cleared with an empty resource,
          because the registry has no unregister: a module a test registers is
          asked for a session by every problem created for the rest of the run.
          A session that started answering resources would go on answering them
          in tests written years later. }
        procedure AnswerResource(const AResource, AJson: string);
        { What was last posted to AnswerResource's resource, or '' - so a test
          can see what a restore handed back without asserting inside the mock. }
        function PostedPayload: string;

        //  IModuleSession
        function Kind: string;
        function PointSink: IModulePointSink;
        function TryGet(const AResource: string; out AJson: string): boolean;
        function TryPost(const AResource, APayload: string;
            out AJson: string): boolean;
        procedure Reset;
        function ContributesFitReadiness: boolean;
        procedure CheckFitAllowed;
        procedure CheckIntervalAllowed(ALoX, AHiX: double);
        function SliceForInterval(ALoX, AHiX: double): IModuleTaskState;
        function TryRemoveInstance(const AInstanceId: string;
            out ARemoved: TInstanceHandles): boolean;
    end;

implementation

constructor TMockModuleSession.Create;
begin
    inherited Create;
    FKind := 'mock';
end;

procedure TMockModuleSession.SetKind(const AKind: string);
begin
    FKind := AKind;
end;

function TMockModuleSession.Kind: string;
begin
    FLog.Note('Kind');
    Result := FKind;
end;

function TMockModuleSession.PointSink: IModulePointSink;
begin
    FLog.Note('PointSink');
    //  nil is a documented answer: a module that collects no picked points has no
    //  sink, and SinkNamed skips a nil rather than dereferencing it.
    Result := nil;
end;

procedure TMockModuleSession.AnswerResource(const AResource, AJson: string);
begin
    FResource := AResource;
    FJson := AJson;
    FPosted := '';
end;

function TMockModuleSession.PostedPayload: string;
begin
    Result := FPosted;
end;

function TMockModuleSession.TryGet(const AResource: string;
    out AJson: string): boolean;
begin
    FLog.Note('TryGet', AResource);
    AJson := '';
    //  Declines unless scripted, which is what keeps this inert for every test
    //  that did not ask it to answer anything.
    Result := (FResource <> '') and (AResource = FResource);
    if Result then
        AJson := FJson;
end;

function TMockModuleSession.TryPost(const AResource, APayload: string;
    out AJson: string): boolean;
begin
    FLog.Note('TryPost', AResource);
    AJson := '';
    Result := (FResource <> '') and (AResource = FResource);
    if Result then
        //  RECORDED, not asserted. A mock that failed inside its own callback
        //  would report from whatever thread the code under test happened to
        //  use, and would name the mock rather than the expectation.
        FPosted := APayload;
end;

procedure TMockModuleSession.Reset;
begin
    FLog.Note('Reset');
end;

function TMockModuleSession.ContributesFitReadiness: boolean;
begin
    FLog.Note('ContributesFitReadiness');
    Result := False;
end;

procedure TMockModuleSession.CheckFitAllowed;
begin
    //  SILENCE MEANS NO OBJECTION. Raising here would forbid every fit in every
    //  test that runs after the mock module was registered.
    FLog.Note('CheckFitAllowed');
end;

procedure TMockModuleSession.CheckIntervalAllowed(ALoX, AHiX: double);
begin
    FLog.Note('CheckIntervalAllowed');
end;

function TMockModuleSession.SliceForInterval(ALoX, AHiX: double): IModuleTaskState;
begin
    FLog.Note('SliceForInterval');
    Result := nil;
end;

function TMockModuleSession.TryRemoveInstance(const AInstanceId: string;
    out ARemoved: TInstanceHandles): boolean;
begin
    FLog.Note('TryRemoveInstance', AInstanceId);
    ARemoved := nil;
    //  DECLINES, like TryGet and TryPost and for the same reason: this module
    //  placed nothing, and a session that claimed a handle would report a
    //  deletion the framework then treats as done - in every test that runs
    //  after the mock was registered, because the registry has no unregister.
    Result := False;
end;

end.
