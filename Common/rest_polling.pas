// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which REST routes the client polls, as opposed to acts on.)

Both sides of the wire need this answer and must not disagree about it: the
server decides at which tier to log an incoming request, the client at which
tier to log the outgoing call, and a route treated as ordinary by one side and
as a heartbeat by the other produces a log where the two halves of the same
call sit at different levels - so one of them vanishes and the other looks
unanswered.

It lives in Common/ rather than in the REST unit because the client may not
depend on the server: pulling fit_rest_api into the desktop client to ask this
one question would drag the engine in with it, and the client contains no
engine (see AGENTS.md).

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit rest_polling;

{$MODE Delphi}

interface

{ True for a route the client polls for as long as it is open, rather than one
  it calls because the user did something: /problems/<id>/state, /async and
  /rfactor. Accepts either a bare path or a full URL, so a caller holding
  whichever of the two it happens to have need not normalise first. }
function IsPolledRoute(const APath: string): boolean;

implementation

uses
    SysUtils;

{ The last path segment, ignoring any query string and any trailing slash. }
function LastSegment(const APath: string): string;
var
    i: integer;
    P: string;
begin
    P := APath;
    i := Pos('?', P);
    if i > 0 then
        P := Copy(P, 1, i - 1);
    while (P <> '') and (P[Length(P)] = '/') do
        SetLength(P, Length(P) - 1);
    Result := P;
    for i := Length(P) downto 1 do
        if P[i] = '/' then
        begin
            Result := Copy(P, i + 1, MaxInt);
            Break;
        end;
end;

function IsPolledRoute(const APath: string): boolean;
var
    Last: string;
begin
    //  Matching on the last segment alone is enough to separate these three from
    //  every other route, and it is what makes a full URL and a bare path answer
    //  the same. The names are distinct across the whole API - see fit_rest_api.
    Last := LowerCase(LastSegment(APath));
    Result := (Last = 'state') or (Last = 'async') or (Last = 'rfactor');
end;

end.
