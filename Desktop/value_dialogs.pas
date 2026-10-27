// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The windows that ask the user for one number, found by name.)

WHY A REGISTRY. The window asked two windows for a number by naming them: the
wavelength and the background fraction. Both belong to the scientific module's
features, and the program created both at start-up, so every product carried
them - one without that module included. They are the Free and Pro client's
(Desktop/ScientificForms), registered here by name from that client's
client_modules; the window asks this registry and names neither.

NOTHING ELSE CHANGED. The interface is exactly what the window used of the
forms - show it, read its value, read what was typed - so every check and
every step of the window's code is the one it had (a restructuring, at the
user's direction, 2026-10-17).
}
unit value_dialogs;

{$mode objfpc}{$H+}

interface

const
    { A number an axis mode reads (axis_mode_registry.apValue) is asked by the
      window under the name its module described it by, so no constant for it
      is here. }
    { The background fraction: Model > Background > Set Background Fraction. }
    BackgroundFractionDialog = 'background-fraction';

type
    IValueDialog = interface
        ['{2F8C61D4-7A3B-4E95-9C0D-5B1E8A47F362}']
        { Shows the window; True when the user confirmed it. }
        function Execute: boolean;
        { The number the window holds - what it shows on opening, and what it
          accepted. }
        function GetValue: double;
        procedure SetValue(AValue: double);
        { What was typed, as typed. }
        function Text: string;
        property Value: double read GetValue write SetValue;
    end;

{ Registers the window asked for by AName. The last registration wins. }
procedure RegisterValueDialog(const AName: string; ADialog: IValueDialog);
{ The window registered as AName, or nil in a build without it. }
function ValueDialog(const AName: string): IValueDialog;

implementation

uses
    Classes;

var
    Names: TStringList;
    Dialogs: array of IValueDialog;

procedure RegisterValueDialog(const AName: string; ADialog: IValueDialog);
var
    i: longint;
begin
    if not Assigned(Names) then
        Names := TStringList.Create;
    i := Names.IndexOf(AName);
    if i < 0 then
    begin
        i := Names.Add(AName);
        SetLength(Dialogs, Names.Count);
    end;
    Dialogs[i] := ADialog;
end;

function ValueDialog(const AName: string): IValueDialog;
var
    i: longint;
begin
    Result := nil;
    if not Assigned(Names) then
        Exit;
    i := Names.IndexOf(AName);
    if i >= 0 then
        Result := Dialogs[i];
end;

finalization
    Names.Free;
end.
