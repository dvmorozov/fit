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
  when the user next asks it to act. }
procedure DrawModuleOverlays(AClient: TObject);

{ How many hooks are registered. Zero in a build with no module, which is the
  ordinary case rather than a fault. }
function ModuleOverlayCount: longint;

implementation

var
    Hooks: array of TModuleOverlayProc;

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
begin
    for i := 0 to High(Hooks) do
        try
            Hooks[i](AClient);
        except
        end;
end;

end.
