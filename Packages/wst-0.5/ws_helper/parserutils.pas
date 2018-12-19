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

unit parserutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  sNEW_LINE = {$ifndef Unix}#13#10{$else}#10{$endif};

  function IsStrEmpty(Const AStr : String):Boolean;
  function ExtractIdentifier(const AValue : string) : string ;
  
  function IsReservedKeyWord(const AValue : string):Boolean ;
  
implementation
uses StrUtils;

const LANGAGE_TOKEN : array[0..107] of string = (
  'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM',
  'BEGIN', 'BOOLEAN', 'BYTE',
  'CASE', 'CDECL', 'CHAR', 'CLASS', 'COMP', 'CONST', 'CONSTRUCTOR', 'CONTAINS', 'CURRENCY',
  'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOUBLE', 'DOWNTO', 'DYNAMIC',
  'END', 'EXPORT', 'EXPORTS', 'EXTERNAL',
  'FAR', 'FILE', 'FINALLY', 'FOR', 'FORWARD', 'FUNCTION', 'GOTO',
  'ELSE', 'EXCEPT', 'EXTENDED',
  'IF', 'IMPLEMENTATION', 'IMPLEMENTS', 'IN', 'INHERITED', 'INT64', 'INITIALIZATION',
    'INTEGER', 'INTERFACE', 'IS',
  'LABEL', 'LIBRARY', 'LOCAL', 'LONGINT', 'LONGWORD',
  'MOD', 'NEAR', 'NIL', 'NODEFAULT', 'NOT',
  'OBJECT', 'OF', 'OLEVARIANT', 'OR', 'OUT', 'OVERLOAD', 'OVERRIDE',
  'PACKAGE', 'PACKED', 'PASCAL', 'PCHAR', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PUBLISHED',
  'RAISE', 'READ', 'REAL', 'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT', 'REQUIRES', 'RESULT',
  'SAFECALL', 'SET', 'SHL', 'SHORTINT', 'SHR', 'SINGLE', 'SMALLINT', 'STDCALL', 'STORED',
  'THEN', 'TO', 'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES',
  'VAR', 'VARARGS', 'VARIANT', 'VIRTUAL', 'WHILE', 'WIDECHAR', 'WITH', 'WORD', 'WRITE', 'XOR'
);
const WST_RESERVED_TOKEN : array[0..1] of string = ( 'Item', 'Item' );
function IsReservedKeyWord(const AValue : string):Boolean ;
begin
  Result := AnsiMatchText(AValue,LANGAGE_TOKEN) or
            AnsiMatchText(AValue,WST_RESERVED_TOKEN);
end;

function IsStrEmpty(Const AStr : String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function ExtractIdentifier(const AValue : string) : string ;
var
  i, c : Integer;
  s : string;
begin
  Result := '';
  s := Trim(AValue);
  c := Length(s);
  if ( c > 0 ) then begin
    if not ( s[1] in ['A'..'Z', 'a'..'z', '_'] ) then begin
      Result := '_';
    end;
    for i := 1 to c do begin
      if ( s[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] ) then begin
        Result := Result + s[i];
      end else begin
        if ( Length(Result) > 0 ) and ( Result[Length(Result)] <> '_' ) then begin
          Result := Result + '_';
        end;
      end;
    end;
  end;
end;


end.

