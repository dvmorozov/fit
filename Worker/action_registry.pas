// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The verbs a client can ask the engine to perform.)

WHAT THIS REPLACES: a fourteen-branch if-chain over the action name, in the
middle of the router. Adding a verb meant editing it, and - the part that
matters more - NOTHING COULD ASK WHAT THE VERBS ARE. The set existed only as
control flow.

That is the difference this registry buys, and it is not a tidying:

  - a batch or scripting layer needs the list of verbs to drive them;
  - an assistant driving the app through tool-use needs the list, with a
    description per verb, to know what it may call;
  - a module contributing an action needs somewhere to put it that is not this
    file.

All three want the same thing: the verbs as DATA. A handler is a procedure
pointer, so the arithmetic of each action stays exactly where it was, and this
unit gains no knowledge of what any of them do.
}
unit action_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fit_server_session;

type
    EActionRegistration = class(Exception);

    { Runs one action.

      ACode is the HTTP status: 200 unless the handler says otherwise, so the
      ordinary case needs no ceremony. AResult is the reply body, AError the
      message shown to the user when ACode is not 200 - never both. }
    TActionHandler = procedure(ASession: TFitSession; const ABody: string;
        out ACode: longint; out AResult, AError: string);

    TActionInfo = record
        { The name in the URL, e.g. 'minimize-difference'. }
        Name: string;
        { One line, for whatever presents the verb to a person or to a model. }
        Description: string;
        { True when the action starts work that outlives the request, so a
          caller knows to poll rather than wait for a result. }
        IsAsynchronous: boolean;
        Handler: TActionHandler;
    end;

    TActionInfoArray = array of TActionInfo;

{ Registers an action. Raises on a duplicate name or a missing handler: two
  handlers for one verb would be resolved by registration order, and the loser
  would be a verb that appears to exist and does something else. }
procedure RegisterAction(const AInfo: TActionInfo);

{ Everything registered, in registration order. }
function RegisteredActions: TActionInfoArray;
function ActionCount: longint;

{ True when AName is a registered verb; AInfo receives its declaration. }
function FindAction(const AName: string; out AInfo: TActionInfo): boolean;

{ The verb names, comma-separated, for an error that tells the caller what it
  could have asked for instead. }
function KnownActionNames: string;

implementation

var
    Registry: TActionInfoArray;

function ActionCount: longint;
begin
    Result := Length(Registry);
end;

function RegisteredActions: TActionInfoArray;
begin
    Result := Registry;
end;

function FindAction(const AName: string; out AInfo: TActionInfo): boolean;
var
    i: longint;
begin
    Result := False;
    AInfo := Default(TActionInfo);
    for i := 0 to High(Registry) do
        if Registry[i].Name = AName then
        begin
            AInfo := Registry[i];
            Exit(True);
        end;
end;

procedure RegisterAction(const AInfo: TActionInfo);
var
    Existing: TActionInfo;
begin
    if AInfo.Name = '' then
        raise EActionRegistration.Create(
            'an action was registered with no name, so nothing could call it');
    if not Assigned(AInfo.Handler) then
        raise EActionRegistration.Create(AInfo.Name +
            ' was registered without a handler');
    if FindAction(AInfo.Name, Existing) then
        raise EActionRegistration.Create(
            'action "' + AInfo.Name + '" is already registered');

    SetLength(Registry, Length(Registry) + 1);
    Registry[High(Registry)] := AInfo;
end;

function KnownActionNames: string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(Registry) do
    begin
        if Result <> '' then
            Result := Result + ', ';
        Result := Result + Registry[i].Name;
    end;
end;

end.
