// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place a build says what its modules add to the desktop
client alone: the client application's windows for them.)

THE CLIENT'S HALF OF THE EXTENSION POINT app_modules, as server_modules is
the server's. A module names no window (module-architecture.md); the windows
a module needs are the client application's own - the Free and Pro client's
for the scientific module are in Desktop/ScientificForms - and are linked only
into a client that links the module. A module's module.json names them as its
"forms", and the build writes each product's copy of this unit calling them
(tools/build-lib/product_modules.ps1). Only the desktop client calls it, so
the compute server, which has no window, never links them.

This copy is the bare core's: it registers nothing. IT IS IN Common, NOT
Desktop, although only the client calls it: the compiler searches the
project's own folder (Desktop) before the search path, so a copy there would
shadow every product's - the first build linked no window at all.
}
unit client_modules;

{$mode objfpc}{$H+}

interface

{ Registers what this build's modules add to the desktop client alone. Called
  once by the window, after RegisterAppModules. }
procedure RegisterClientModules;

implementation

procedure RegisterClientModules;
begin
    //  The bare core links no module, and so no window for one.
end;

end.
