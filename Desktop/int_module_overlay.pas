// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Letting a module redraw after the model is recomputed.)

A module's view of a model - markers on the chart, rows in its panel - must be
refreshed whenever the model changes, and only the client knows when that is.
Before this the client called the pack's redraw directly, so the presenter named
the pack and could not compile without it.

A module registers a procedure instead. The client runs them after plotting the
curves; each fetches what it needs through the module channel and draws through
the view contract. A build with no module registers none.
}
unit int_module_overlay;

{$mode objfpc}{$H+}

interface

type
    { Called after the model has been recomputed and the curves plotted. AClient
      is the TFitClient, as TObject so this unit names nothing a module
      defines. }
    TModuleOverlayProc = procedure(AClient: TObject);

{ Idempotent: a hook registered twice would draw its series twice per redraw. }
procedure RegisterModuleOverlay(AProc: TModuleOverlayProc);

{ Runs every hook. Swallows exceptions, and this is the one place that is right:
  it runs on EVERY redraw, so a module whose server is momentarily unreachable
  must not make the chart unusable. What went wrong is the module's to report
  when the user next asks it to act.

  SWALLOWED, BUT NOT UNSEEN: each distinct failure is written to the log once
  (OverlayFailureNote), as a warning. A hook that raises on every redraw
  otherwise left a chart with the module's markers silently missing and nothing
  anywhere to say why; logging every occurrence instead would write a line per
  redraw - hundreds during one fit - and bury the log. Once per hook and
  message keeps the first sight of each fault, and a different fault from the
  same hook is still news. Not shown to the user: the module reports what went
  wrong when the user next asks it to act, and a message box on a redraw would
  interrupt whatever the user was doing. }
procedure DrawModuleOverlays(AClient: TObject);

type
    TOverlayFailures = array of string;

{ What to log for hook number AHook failing with an exception of class
  AClassName saying AMessage - or '' when ASeen already holds that failure,
  which it then records. }
function OverlayFailureNote(var ASeen: TOverlayFailures; AHook: longint;
    const AClassName, AMessage: string): string;

{ How many hooks are registered. Zero in a build with no module, which is the
  ordinary case rather than a fault. }
function ModuleOverlayCount: longint;

implementation

uses
    SysUtils, log;

var
    Hooks: array of TModuleOverlayProc;
    { Every failure already logged, for the life of the process. }
    Seen: TOverlayFailures;

function OverlayFailureNote(var ASeen: TOverlayFailures; AHook: longint;
    const AClassName, AMessage: string): string;
var
    Key: string;
    i: longint;
begin
    Key := IntToStr(AHook) + #0 + AClassName + #0 + AMessage;
    for i := 0 to High(ASeen) do
        if ASeen[i] = Key then
            Exit('');
    SetLength(ASeen, Length(ASeen) + 1);
    ASeen[High(ASeen)] := Key;
    Result := 'module overlay ' + IntToStr(AHook + 1) + ' of ' +
        IntToStr(Length(Hooks)) + ' failed to redraw (' + AClassName + ': ' +
        AMessage + '); the chart is drawn without it. Logged once per ' +
        'failure: it may recur on every redraw.';
end;

function ModuleOverlayCount: longint;
begin
    Result := Length(Hooks);
end;

procedure RegisterModuleOverlay(AProc: TModuleOverlayProc);
var
    i: longint;
begin
    if not Assigned(AProc) then
        Exit;
    for i := 0 to High(Hooks) do
        if Hooks[i] = AProc then
            Exit;
    SetLength(Hooks, Length(Hooks) + 1);
    Hooks[High(Hooks)] := AProc;
end;

procedure DrawModuleOverlays(AClient: TObject);
var
    i: longint;
    Note: string;
begin
    for i := 0 to High(Hooks) do
        try
            Hooks[i](AClient);
        except
            on E: Exception do
            begin
                Note := OverlayFailureNote(Seen, i, E.ClassName, E.Message);
                if Note <> '' then
                    WriteLog(Note, Warning);
            end;
        else
            //  Not an Exception at all: still swallowed, for the same reason,
            //  with nothing in it to log.
            ;
        end;
end;

end.
