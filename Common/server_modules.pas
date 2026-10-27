// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place a build says what its modules add to the compute
server alone.)

THE SERVER'S HALF OF THE EXTENSION POINT app_modules. What every host needs
from a module - its curve types, its explanations, its menus, its readers, the
declaration of its per-problem resources - is registered in app_modules, which
the desktop client and the compute server both call. What only the server may
hold - a module's per-problem session (IAppModule's CreateSession), its model
builder, whatever reaches the fitting engine - is registered HERE, which only
fit_server calls.

WHY TWO. The desktop client contains no fitting engine (AGENTS.md,
non-negotiable 1). With one registration both hosts called, a module's front
door named its session, the session named fit_service, and the window linked
the whole engine again - found 2026-10-13 in a module's client, a year after
the framework's own minimizer registration had done the same
(client_without_engine.tests.ps1 now walks every client project).

TO ADD A MODULE'S SERVER HALF, as for app_modules: put your own copy of this
unit in a directory on the SERVER project's search path, ahead of Common,
calling your server front door. A module with nothing server-only needs no
copy: this one registers nothing.
}
unit server_modules;

{$mode objfpc}{$H+}

interface

{ Registers what this build's modules add to the compute server alone. Called
  once by the compute server, after RegisterAppModules. }
procedure RegisterServerModules;

implementation

procedure RegisterServerModules;
begin
    //  The published framework contains no module, and so nothing here.
end;

end.
