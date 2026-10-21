// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the scientific module adds to the compute server alone.)

THE BACKGROUND ARITHMETIC (background_search): proposing a profile's
background points and taking them off, which the engine asks for by name
(background_algorithms). It runs on the server only, so it is named here -
called by the server's composition unit, server_modules - and never by the
module's client front door: the desktop client contains no engine code
(AGENTS.md, non-negotiable 1). Naming the unit is what links it, and linking
it is what registers it, as for a curve type.

THE PEAK SEARCH (peak_search), which the engine asks for by name
(peak_search_registry), for the same reason and in the same way.
}
unit scientific_server_module;

{$mode objfpc}{$H+}

interface

{ Links the module's server-side units, which register themselves. Idempotent. }
procedure RegisterScientificServer;

implementation

uses
    background_search, peak_search;

procedure RegisterScientificServer;
begin
    //  Nothing to call: background_search and peak_search registered
    //  themselves when linked.
end;

end.
