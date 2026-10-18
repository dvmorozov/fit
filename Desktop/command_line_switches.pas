// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the client accepts on its command line.)

WHY IT IS ITS OWN UNIT. This rule lived nested inside Fit.lpr's main block,
where nothing could link it and nothing did - and it is a rule that is silent
when it is wrong. An argument the client does not recognise is not refused, it
is passed over: a desktop that hands the program a bare file path, which is what
Explorer's %1 and macOS's Open With both do, opens an empty window and reports
nothing at all. That is why every packaged launcher translates a path into a
switch before starting the client, and why the two halves are now asserted
against each other (tests/testcase_command_line_switches.pas).

THE RULE IS UNCHANGED, deliberately: a switch starts with / or \, its name is
matched as a substring of the argument, and its value is whatever follows the
first equals sign. Command lines that worked before this was moved work after it.
}
unit command_line_switches;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ True when AArgs holds a switch named AName, with its value in AValue.

  The first match wins, and a switch with no equals sign is found with an empty
  value - both are the behaviour this rule has always had. }
function SwitchFound(AArgs: TStrings; const AName: string;
    out AValue: string): boolean;

{ The command line this process was started with, as a list - ParamStr(1) up.

  Separated from the rule above so that the rule can be exercised without a
  process that was started with the arguments it is about. }
procedure CommandLineArgs(ADest: TStrings);

implementation

uses
    StrUtils;

function SwitchFound(AArgs: TStrings; const AName: string;
    out AValue: string): boolean;
const
    { A switch starts with one of these. }
    Token = '/\';
var
    i, j, TokenPos: integer;
    Param: string;
begin
    AValue := '';
    Result := False;
    if not Assigned(AArgs) then
        Exit;
    for i := 0 to AArgs.Count - 1 do
    begin
        Param := AArgs[i];
        //  AN EMPTY ARGUMENT IS POSSIBLE - a shell expanding a variable that is
        //  not set produces one - and reading its first character is how this
        //  was written before it was moved here.
        if Param = '' then
            continue;
        for j := 1 to Length(Token) do
            if (Token[j] = Param[1]) and AnsiContainsStr(Param, AName) then
            begin
                Result := True;
                TokenPos := Pos('=', Param);
                if TokenPos <> 0 then
                    AValue := Copy(Param, TokenPos + 1, Length(Param) - TokenPos);
                Exit;
            end;
    end;
end;

procedure CommandLineArgs(ADest: TStrings);
var
    i: integer;
begin
    for i := 1 to ParamCount do
        ADest.Add(ParamStr(i));
end;

end.
