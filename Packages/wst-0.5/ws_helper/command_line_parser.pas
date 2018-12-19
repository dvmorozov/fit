{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit command_line_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

Type

  TComandLineOption = (
    cloInterface, cloProxy, cloImp, cloBinder,
    cloOutPutDirRelative, cloOutPutDirAbsolute
  );
  TComandLineOptions = set of TComandLineOption;

  function ParseCmdLineOptions(out AAppOptions : TComandLineOptions):Integer;
  function GetOptionArg(const AOption : TComandLineOption):string;
  
implementation
uses getopts;

Var
  OptionsArgsMAP : Array[TComandLineOption] of string;

function GetOptionArg(const AOption : TComandLineOption):string;
begin
  Result := OptionsArgsMAP[AOption];
end;

function ParseCmdLineOptions(out AAppOptions : TComandLineOptions):Integer;
var
  c : Char;
begin
  AAppOptions := [];
  c := #0;
  repeat
    c := GetOpt('u:pibo:a:');
    case c of
      'u' :
        begin
          Include(AAppOptions,cloInterface);
          OptionsArgsMAP[cloInterface] := OptArg;
        end;
      'p' : Include(AAppOptions,cloProxy);
      'i' : Include(AAppOptions,cloImp);
      'b' : Include(AAppOptions,cloBinder);
      'o' :
        Begin
          Include(AAppOptions,cloOutPutDirRelative);
          OptionsArgsMAP[cloOutPutDirRelative] := OptArg;
        End;
      'a' :
        Begin
          Include(AAppOptions,cloOutPutDirAbsolute);
          OptionsArgsMAP[cloOutPutDirAbsolute] := OptArg;
        End;
    end;
  until ( c = EndOfOptions );
  Result := OptInd;
end;

end.

