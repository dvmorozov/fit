// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Pointing the client at a compute server, and what follows from that.)

THERE IS NO IN-PROCESS ENGINE. Fitting happens in a separate process, possibly on
another machine, and the client is useless without one - so what the window does
when the user names a server is not bookkeeping, it is the difference between an
application that works and one that silently cannot fit.

Three rules lived inside two LCL handlers, where the only way to run them was to
open the dialog:

  * AN EMPTY SETTING MEANS THE DEFAULT, and it means it in two places that had
    their own copies: the URL offered in the dialog, and the URL actually applied.
    A blank kept as blank is a client pointed at nothing, which fails later and
    somewhere else.

  * A FILE LOADED WHILE NO SERVER WAS REACHABLE LIVES ONLY IN THE CLIENT. Naming a
    server that answers has to hand it the profile, or the user has a file on
    screen, a server connected, and a fit that has nothing to work on. This is the
    rule worth the unit: it is invisible, it depends on two facts that arrive from
    different places, and getting it wrong looks like the file not having loaded.

  * NOTHING ANSWERING IS WORTH SAYING AT ONCE, and saying where. A server that is
    not running is the single most likely thing to be wrong with this program, and
    the message is the only place the user learns which address was tried.
}
unit server_connection;

{$mode objfpc}{$H+}

interface

type
    { What to do once a server has been named and probed. }
    TConnectionStep = (
        { Nothing answered. The user is told, with the address that was tried. }
        csTellTheUserNothingAnswered,
        { It answered, and there is a profile in the client that it has never
          seen - it was loaded while no server was reachable. }
        csSendTheProfile,
        { It answered and there is nothing to hand over. }
        csNothingToDo
    );

{ The URL to offer the user, given what is configured.

  Blank means the default rather than blank: the dialog is a chance to correct the
  address, and offering an empty box hides what would be used. }
function ServerUrlToOffer(const AConfigured, ADefault: string): string;

{ The URL to actually use, given what the user typed.

  Trimmed - a URL with a stray space is a URL that resolves to nothing - and
  falling back to the default when that leaves nothing at all. }
function ServerUrlToUse(const ATyped, ADefault: string): string;

{ What follows from probing the server just named.

  AProfileIsOpen is whether the client holds a profile; it is only handed over
  when the server answered, because there is nowhere to send it otherwise. }
function StepAfterProbing(AServerAnswered,
    AProfileIsOpen: boolean): TConnectionStep;

{ What to tell the user when nothing answered, naming the address tried. }
function NoServerAnsweredNotice(const AUrl: string): string;

implementation

uses
    SysUtils;

function ServerUrlToOffer(const AConfigured, ADefault: string): string;
begin
    Result := Trim(AConfigured);
    if Result = '' then
        Result := ADefault;
end;

function ServerUrlToUse(const ATyped, ADefault: string): string;
begin
    //  The same rule as above, and that is the point of both being here: the
    //  address offered and the address used were two copies of it, and a
    //  difference between them would show the user one server and use another.
    Result := ServerUrlToOffer(ATyped, ADefault);
end;

function StepAfterProbing(AServerAnswered,
    AProfileIsOpen: boolean): TConnectionStep;
begin
    if not AServerAnswered then
    begin
        Result := csTellTheUserNothingAnswered;
        Exit;
    end;
    if AProfileIsOpen then
        //  THE RULE THAT IS INVISIBLE WHEN IT IS WRONG. The profile was loaded
        //  with no server to send it to; this is the moment it can be sent.
        Result := csSendTheProfile
    else
        Result := csNothingToDo;
end;

function NoServerAnsweredNotice(const AUrl: string): string;
begin
    Result := 'No server answered at ' + AUrl + '.' + LineEnding +
        //  Names the binary, because "start the server" is not actionable if you
        //  do not know what it is called.
        'Start fit_server there - the application cannot fit without it.';
end;

end.
