// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(That the engine's verbs are data, and stay well-formed.)

The actions themselves are exercised through the REST surface by
testcase_rest_api. What is asserted here is the property that conversion bought
and that nothing else would notice losing: the set of verbs can be ENUMERATED,
with a description apiece.

That is not decoration. A batch layer drives verbs from that list; an assistant
calling the app through tool-use is given that list to choose from. A verb
registered without a description would reach both as a blank option.
}
unit testcase_action_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    StrUtils,
    fit_server_session, action_registry, fit_rest_api,
    //  The other end of the wire. Named here rather than the reverse, because
    //  this is where the server's own registry can be asked - see the group at
    //  the bottom of the file.
    mock_http_transport;

type
    TActionRegistryTest = class(TTestCase)
    published
        procedure TheVerbsCanBeEnumerated;
        procedure EveryVerbIsNamedAndDescribed;
        procedure TheFittingVerbsAreRegistered;
        procedure AVerbCannotBeRegisteredTwice;
        procedure AVerbNeedsAHandler;
        procedure AnUnknownVerbIsRefusedAndSaysWhatIsAvailable;

        //  THE TWO ENDS OF THE WIRE, checked against each other in one process.
        procedure EveryVerbTheClientSendsIsOneTheServerKnows;
        procedure AndTheClientHasAWayToAskForEachOfThem;
    end;

implementation

procedure Nothing(ASession: TFitSession; const ABody: string;
    out ACode: longint; out AResult, AError: string);
begin
    ACode := 200;
    AResult := '';
    AError := '';
end;

{ The same call the server makes, so a test sees exactly the set it offers. }
procedure EnsureRegistered;
begin
    RegisterBuiltInActions;
end;

procedure TActionRegistryTest.TheVerbsCanBeEnumerated;
begin
    EnsureRegistered;
    //  The whole point of the conversion. Before it, the set of verbs existed
    //  only as branches, so nothing could ask what the engine supports.
    AssertTrue('the engine must offer verbs', ActionCount > 0);
    AssertTrue('and they must be listable', KnownActionNames <> '');
end;

procedure TActionRegistryTest.EveryVerbIsNamedAndDescribed;
var
    Actions: TActionInfoArray;
    i: longint;
begin
    EnsureRegistered;
    Actions := RegisteredActions;
    for i := 0 to High(Actions) do
    begin
        AssertTrue('every verb has a name', Actions[i].Name <> '');
        //  A verb with no description reaches a batch listing or an assistant's
        //  tool list as a blank option - visible only to whoever then picks it.
        AssertTrue(Actions[i].Name + ' must describe itself',
            Actions[i].Description <> '');
        AssertTrue(Actions[i].Name + ' must have a handler',
            Assigned(Actions[i].Handler));
    end;
end;

procedure TActionRegistryTest.TheFittingVerbsAreRegistered;
var
    Info: TActionInfo;
begin
    EnsureRegistered;
    //  A spot-check that the conversion did not drop a verb: these are the ones
    //  the client calls on an ordinary fitting session.
    AssertTrue('minimize-difference', FindAction('minimize-difference', Info));
    //  Declared asynchronous, so a caller knows to poll rather than wait - this
    //  was implicit in the old chain and is now stated.
    AssertTrue('and it is asynchronous', Info.IsAsynchronous);
    AssertTrue('do-all-automatically', FindAction('do-all-automatically', Info));
    AssertTrue('select-profile-interval',
        FindAction('select-profile-interval', Info));
    AssertTrue('stop', FindAction('stop', Info));
    AssertFalse('stopping is immediate, not asynchronous', Info.IsAsynchronous);
end;

procedure TActionRegistryTest.AVerbCannotBeRegisteredTwice;
var
    Info: TActionInfo;
    Raised: boolean;
begin
    EnsureRegistered;
    Info := Default(TActionInfo);
    Info.Name := 'minimize-difference';
    Info.Description := 'an impostor';
    Info.Handler := @Nothing;
    Raised := False;
    try
        RegisterAction(Info);
    except
        on E: EActionRegistration do
            Raised := True;
    end;
    //  Otherwise which handler runs depends on registration order, and the
    //  loser is a verb that appears to exist and does something else.
    AssertTrue('a second claim on a verb must be refused', Raised);
end;

procedure TActionRegistryTest.AVerbNeedsAHandler;
var
    Info: TActionInfo;
    Raised: boolean;
begin
    Info := Default(TActionInfo);
    Info.Name := 'verb-with-no-handler';
    Info.Description := 'does nothing at all';
    Raised := False;
    try
        RegisterAction(Info);
    except
        on E: EActionRegistration do
            Raised := True;
    end;
    //  It would otherwise be listed, offered, called - and dereference nil.
    AssertTrue('a verb with no handler must be refused', Raised);
end;

procedure TActionRegistryTest.AnUnknownVerbIsRefusedAndSaysWhatIsAvailable;
var
    Code: longint;
    Res, Err: string;
begin
    RunAction(nil, 'fly-to-the-moon', '', Code, Res, Err);
    AssertEquals('an unknown verb is not found', 404, Code);
    AssertTrue('the refusal names the verb asked for',
        Pos('fly-to-the-moon', Err) > 0);
    //  "What is wrong" without "what is valid" is half an answer for a typo in
    //  a script, which is where this error is actually read.
    AssertTrue('and says what could have been asked instead',
        Pos('minimize-difference', Err) > 0);
end;

{ ------------------- the client's verbs against the server's ---------------- }

{ THE VERB NAME IS A STRING THAT CROSSES A PROCESS BOUNDARY, and until now
  nothing compared the two ends. The client's side is nine one-line methods, each
  posting a literal; the server's side is this registry. Each end had tests, and
  each end's tests were satisfied by the end alone: the client's asserted that it
  sent the string it sends, and the registry's asserted that the server knows the
  names the server registered.

  A typo in one client literal is therefore invisible to the whole suite, and
  invisible in use too - a menu command that answers "unknown action" once, into
  a message box, for one verb, on a build where the other eight work.

  Checked here rather than in the client's own fixture because this is the side
  that can be enumerated. The client's literals cannot be read out of it; they
  can only be observed by making the call and looking at where it went, which is
  what the transport mock is for. }

procedure TActionRegistryTest.EveryVerbTheClientSendsIsOneTheServerKnows;
var
    Svc: TMockHttpService;
    Sent: TStringList;
    Info: TActionInfo;
    i, p: longint;
    Line, Name: string;
begin
    EnsureRegistered;
    Sent := TStringList.Create;
    Svc := TMockHttpService.Create('http://localhost:1/problems/1');
    try
        //  EVERY ACTION THE CLIENT OFFERS. Listed rather than derived, because
        //  the client's verbs are literals inside its methods and there is
        //  nothing to enumerate - which is the reason this test exists. A verb
        //  added to the SERVER and not called here fails the next test; a client
        //  method added and never listed here is caught by neither, and that is
        //  the honest limit of what can be checked from this side.
        Svc.SmoothProfile;
        Svc.SubtractBackground(True);
        Svc.DoAllAutomatically;
        Svc.MinimizeDifference;
        Svc.MinimizeDifferenceAgain;
        Svc.MinimizeNumberOfCurves;
        Svc.ComputeCurveBounds;
        Svc.ComputeBackgroundPoints;
        Svc.ComputeCurvePositions;
        Svc.SelectAllPointsAsCurvePositions;
        Svc.SelectEntireProfile;
        Svc.SelectProfileInterval(0, 5);
        Svc.CreateCurveList;
        Svc.StopAsyncOper;

        //  The logged URLs, reduced to the verb each names. Anything not under
        //  /actions/ is some other route the call also made - reading the state
        //  back, for instance - and is not this test's subject.
        Sent.Text := Svc.Log.AsText;
        for i := 0 to Sent.Count - 1 do
        begin
            Line := Sent[i];
            p := Pos('/actions/', Line);
            if p = 0 then
                Continue;
            Name := Copy(Line, p + Length('/actions/'), MaxInt);
            //  Whatever the log puts after the URL, and any query.
            Name := Trim(Name);
            p := Pos(' ', Name);
            if p > 0 then
                Name := Copy(Name, 1, p - 1);
            p := Pos('?', Name);
            if p > 0 then
                Name := Copy(Name, 1, p - 1);
            //  The log wraps the arguments in brackets, so a call with no body
            //  ends in one.
            p := Pos(')', Name);
            if p > 0 then
                Name := Copy(Name, 1, p - 1);
            while (Name <> '') and (Name[Length(Name)] = '/') do
                Name := Copy(Name, 1, Length(Name) - 1);

            AssertTrue('the server offers no verb "' + Name +
                '", which the client sends. It offers: ' + KnownActionNames,
                FindAction(Name, Info));
        end;
    finally
        Svc.Free;
        Sent.Free;
    end;
end;

procedure TActionRegistryTest.AndTheClientHasAWayToAskForEachOfThem;
var
    Svc: TMockHttpService;
    Actions: TActionInfoArray;
    Sent: string;
    i: longint;
begin
    //  THE OTHER DIRECTION, which catches the opposite mistake: a verb the
    //  server grew and no client can reach. That one is not a broken command -
    //  it is a feature nobody can use, which shows up as a support question
    //  rather than as a fault, and never as a test failure.
    EnsureRegistered;
    Svc := TMockHttpService.Create('http://localhost:1/problems/1');
    try
        Svc.SmoothProfile;
        Svc.SubtractBackground(True);
        Svc.DoAllAutomatically;
        Svc.MinimizeDifference;
        Svc.MinimizeDifferenceAgain;
        Svc.MinimizeNumberOfCurves;
        Svc.ComputeCurveBounds;
        Svc.ComputeBackgroundPoints;
        Svc.ComputeCurvePositions;
        Svc.SelectAllPointsAsCurvePositions;
        Svc.SelectEntireProfile;
        Svc.SelectProfileInterval(0, 5);
        Svc.CreateCurveList;
        Svc.StopAsyncOper;
        Sent := Svc.Log.AsText;

        Actions := RegisteredActions;
        for i := 0 to High(Actions) do
            AssertTrue('no client call reaches the verb "' + Actions[i].Name +
                '"', Pos('/actions/' + Actions[i].Name, Sent) > 0);
    finally
        Svc.Free;
    end;
end;

initialization
    RegisterTest('unit', TActionRegistryTest);
end.
