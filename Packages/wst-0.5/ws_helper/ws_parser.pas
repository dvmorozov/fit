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

unit ws_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  parserdefs;

Type
  EParserException = class(Exception)
  End;

  { TPascalParser }

  TPascalParser = class
  private
    FStream : TStream;
    FTokenizer : TParser;
    FErrorMessage : String;
    FSymbolTable : TSymbolTable;
    FCurrentSymbol : TAbstractSymbolDefinition;
  private
    Property Tokenizer : TParser Read FTokenizer;
    procedure SetErrorMessage(Const AValue : String);
    procedure CheckCurrentString(Const AString : String);
  private
    procedure BeginParsing();
    procedure EndParsing();
    function GetSourceLine: Integer;
    function ReadUntil(Const AText : String; Const ARaiseException : Boolean):Boolean;
    procedure SkipComment();
    function NextToken(): Char;
  Private
    procedure ParseHeader();
    procedure ParseInterfaceSection();
    procedure ParseUses();
    procedure ParseTypeDeclaration();
    procedure ParseInterfaceType(Const AName : String);
    procedure ParseEnumType(Const AName : String);
    procedure ParseClassType(Const AName : String);
  public
    constructor Create(AStream : TStream; ASymbolTable : TSymbolTable);
    destructor Destroy();override;
    procedure Error(Const AMsg : String);overload;
    procedure Error(Const AMsg : String; Const AArgs : Array of const);overload;
    function Parse():Boolean;
    property SourceLine: Integer read GetSourceLine;
    property ErrorMessage : String read FErrorMessage;
    property SymbolTable : TSymbolTable Read FSymbolTable;
  End;


implementation

Type
  TPascalToken = (
    ptNone,
    ptUnit, ptInterface, ptUses, ptType,
    ptImplementation, ptEnd,
    ptProcedure, ptFunction,
    ptSemicolon, ptComma, ptPeriod, ptEqual, ptColon,
    ptLeftParenthesis, ptRigthParenthesis,
    ptLeftSquareBracket, ptRigthSquareBracket,
    ptConst, ptVar, ptOut,
    ptClass
  );
Const
  PascalTokenStrMAP : Array[TPascalToken] Of String = (
    '',
    'UNIT', 'INTERFACE', 'USES', 'TYPE',
    'IMPLEMENTATION', 'END',
    'PROCEDURE', 'FUNCTION',
    ';', ',', '.', '=', ':',
    '(', ')',
    '[', ']',
    'CONST', 'VAR', 'OUT',
    'CLASS'
  );

function GetPascalTokenStr(Const AToken:TPascalToken):String;
begin
  Result := PascalTokenStrMAP[AToken];
end;

function GetPascalTokenFromStr(Const ATokenStr:String):TPascalToken;
begin
  Result := Succ(ptNone);
  For Result := Result To High(TPascalToken) Do Begin
    If AnsiSameText(ATokenStr,PascalTokenStrMAP[Result]) Then
      Exit;
  End;
  Result := ptNone;
end;

{ TPascalParser }

procedure TPascalParser.SetErrorMessage(const AValue: String);
begin
  FErrorMessage := AValue;
end;

procedure TPascalParser.Error(const AMsg: String);
begin
  Raise EParserException.Create(AMsg);
end;

procedure TPascalParser.Error(const AMsg: String; const AArgs: array of const);
begin
  Raise EParserException.CreateFmt(AMsg,AArgs);
end;

procedure TPascalParser.CheckCurrentString(const AString: String);
begin
  If Not AnsiSameText(Tokenizer.TokenString,AString) Then
    Error('"%s" expected.',[AString]);
end;

procedure TPascalParser.BeginParsing();
begin

end;

procedure TPascalParser.EndParsing();
begin

end;

function TPascalParser.GetSourceLine: Integer;
begin
  Result := Tokenizer.SourceLine;
end;

function TPascalParser.ReadUntil(Const AText : String; Const ARaiseException : Boolean):Boolean;
begin
  While ( Tokenizer.Token <> toEOF ) And ( Not AnsiSameText(Tokenizer.TokenString,AText) ) Do
    Tokenizer.NextToken();
  Result := AnsiSameText(Tokenizer.TokenString,AText);
  If ARaiseException And ( Not Result ) Then
    Error('"%s" not found.',[AText]);
end;

procedure TPascalParser.SkipComment();
const L_C = '{'; R_C = '}';
begin
  While ( FTokenizer.TokenString = L_C ) do begin
    ReadUntil(R_C,False);
    FTokenizer.NextToken();
  end;
end;

function TPascalParser.NextToken(): Char;
begin
  SkipComment();
  Result := FTokenizer.NextToken();
  SkipComment();
end;

procedure TPascalParser.ParseHeader();
begin // Unit UnitName;
  SkipComment();
  Tokenizer.CheckTokenSymbol(GetPascalTokenStr(ptUnit));
  NextToken();
  Tokenizer.CheckToken(toSymbol);
  FSymbolTable.Name := Tokenizer.TokenString;
  NextToken();
  CheckCurrentString(GetPascalTokenStr(ptSemicolon));

  NextToken();
end;

procedure TPascalParser.ParseInterfaceSection();
begin
  ReadUntil(GetPascalTokenStr(ptInterface),True);
  Tokenizer.CheckTokenSymbol(GetPascalTokenStr(ptInterface));
  NextToken();
  Tokenizer.CheckToken(toSymbol);
  If Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptUses)) Then
    ParseUses();
  ReadUntil(GetPascalTokenStr(ptType),True);
  Tokenizer.CheckToken(toSymbol);
  If Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptType)) Then Begin
    NextToken();
    Repeat
      ParseTypeDeclaration();
    Until ( Tokenizer.Token = toEOF ) Or
          Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptImplementation)) Or
          Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptProcedure)) Or
          Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptFunction));
  End;
end;

procedure TPascalParser.ParseUses();
begin
  Tokenizer.CheckTokenSymbol(GetPascalTokenStr(ptUses));
  NextToken();
  Repeat
    Tokenizer.CheckToken(toSymbol); //UnitName
    NextToken();
    If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptSemicolon)) Then
      Break;
    CheckCurrentString(GetPascalTokenStr(ptComma));
    NextToken();
  Until ( Tokenizer.Token = toEOF ) Or AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptSemicolon));
  CheckCurrentString(GetPascalTokenStr(ptSemicolon));
  NextToken();
end;

procedure TPascalParser.ParseTypeDeclaration();
var
  sname : string;
begin
  Tokenizer.CheckToken(toSymbol);
  sname := Tokenizer.TokenString;
  NextToken();
  CheckCurrentString(GetPascalTokenStr(ptEqual));

  NextToken();
  if AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptLeftParenthesis)) then
    self.ParseEnumType(sname)
  else if AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptClass)) then
    self.ParseClassType(sname)
  else begin
    Tokenizer.CheckToken(toSymbol);
    if Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptInterface)) then
      ParseInterfaceType(sname)
    else begin
      ReadUntil(GetPascalTokenStr(ptEnd),True);
      NextToken();// End
      NextToken();// ;
    end;
  end;
end;

procedure TPascalParser.ParseInterfaceType(const AName: String);
Var
  sbl : TInterfaceDefinition;

  procedure ReadIntfHeader();
  begin
    NextToken();
    Repeat
      Tokenizer.CheckToken(toSymbol);
      NextToken();
      If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptRigthParenthesis)) Then Begin
        NextToken();
        Break;
      End;
      NextToken();
      CheckCurrentString(GetPascalTokenStr(ptComma));

      NextToken();
    Until ( Tokenizer.Token = toEOF ) ;
  end;

  procedure ReadProcedure(Const AProc : Boolean);
  Var
    tmpStr,prmName : String;
    tmpTkn : TPascalToken;
    pr : TMethodDefinition;
    prmM : TParameterModifier;
    dataTypeSbl : TTypeDefinition;
    foundSymbol : TAbstractSymbolDefinition;
  begin
    NextToken();
    Tokenizer.CheckToken(toSymbol);
    tmpStr := Tokenizer.TokenString;
    If AProc Then
      pr := sbl.AddMethod(tmpStr,mtProcedure)
    Else
      pr := sbl.AddMethod(tmpStr,mtFunction);
    NextToken();
    If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptLeftParenthesis)) Then Begin
      NextToken();
      If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptRigthParenthesis)) Then
        NextToken()
      Else Begin
        Repeat
          Tokenizer.CheckToken(toSymbol);
          tmpStr := Tokenizer.TokenString;
          tmpTkn := GetPascalTokenFromStr(tmpStr);
          prmM := pmNone;
          If ( tmpTkn = ptConst ) Then Begin
            prmM := pmConst;
          End Else If ( tmpTkn = ptVar ) Then Begin
            prmM := pmVar;
          End Else If ( tmpTkn = ptOut ) Then Begin
            prmM := pmOut;
          End;
          If ( prmM > pmNone ) Then Begin
            NextToken();
            tmpStr := Tokenizer.TokenString;
          End;
          prmName := tmpStr;
          NextToken();
          CheckCurrentString(GetPascalTokenStr(ptColon));

          NextToken();
          Tokenizer.CheckToken(toSymbol);
          tmpStr := Tokenizer.TokenString;
          foundSymbol := FSymbolTable.Find(tmpStr);
          If Assigned(foundSymbol) And ( Not foundSymbol.InheritsFrom(TTypeDefinition) ) Then
            Error('Type symbol expected where "%s" was found.',[foundSymbol.Name]);
          If Assigned(foundSymbol) Then
            dataTypeSbl := foundSymbol As TTypeDefinition
          Else Begin
            dataTypeSbl := TTypeDefinition.Create(tmpStr);
            FSymbolTable.Add(dataTypeSbl);
          End;
          pr.AddParameter(prmName,prmM,dataTypeSbl);
          NextToken();
          tmpStr := Tokenizer.TokenString;
          If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptRigthParenthesis)) Then Begin
            NextToken();
            Break;
          End;
          NextToken();
        Until ( Tokenizer.Token = toEOF ) ;
      End;
    End;
    If Not AProc Then Begin
      CheckCurrentString(GetPascalTokenStr(ptColon));
      NextToken();
      Tokenizer.CheckToken(toSymbol);
      tmpStr := Tokenizer.TokenString;
      dataTypeSbl := FSymbolTable.Find(tmpStr) As TTypeDefinition;
      If Not Assigned(dataTypeSbl) Then Begin
        dataTypeSbl := TTypeDefinition.Create(tmpStr);
        FSymbolTable.Add(dataTypeSbl);
      End;
      pr.AddParameter('result',pmOut,dataTypeSbl);
      NextToken();
    End;
    CheckCurrentString(GetPascalTokenStr(ptSemicolon));
    NextToken();
  end;

  procedure ReadFunction();
  begin
    ReadProcedure(False);
  end;

  procedure ReadGUID();
  begin //['{804A3825-ADA5-4499-87BF-CF5491BFD674}']
    CheckCurrentString(GetPascalTokenStr(ptLeftSquareBracket));
    NextToken();
    FTokenizer.CheckToken(toString);
    sbl.InterfaceGUID := FTokenizer.TokenString;
    NextToken();
    CheckCurrentString(GetPascalTokenStr(ptRigthSquareBracket));
    NextToken();
  end;

begin
  Tokenizer.CheckTokenSymbol(GetPascalTokenStr(ptInterface));
  sbl := TInterfaceDefinition.Create(AName);
  FSymbolTable.Add(sbl);
  FCurrentSymbol := sbl;
  NextToken();
  if AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptLeftParenthesis)) then
    ReadIntfHeader();
  if AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptLeftSquareBracket)) then
    ReadGUID();
  repeat
    if Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptProcedure)) then
      ReadProcedure(True)
    else if Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptFunction)) then
      ReadFunction()
    else if Tokenizer.TokenSymbolIs(GetPascalTokenStr(ptEnd)) then begin
      NextToken();
      NextToken();
      Break;
    end else begin
      Error('"%s", "%s", "%s" expected.',[GetPascalTokenStr(ptProcedure),GetPascalTokenStr(ptFunction),GetPascalTokenStr(ptEnd)]);
    end;
  until ( Tokenizer.Token = toEOF ) ;
end;

procedure TPascalParser.ParseEnumType(const AName: String);
Var
  sbl : TEnumTypeDefinition;
  tmpStr : String;
  sblItem : TEnumItemDefinition;
  tmpInt : Integer;
begin
  sbl := TEnumTypeDefinition.Create(AName);
  FSymbolTable.Add(sbl);
  FCurrentSymbol := sbl;
  CheckCurrentString(GetPascalTokenStr(ptLeftParenthesis));
  NextToken();
  Tokenizer.CheckToken(toSymbol);
  tmpInt := 0;
  Repeat
    tmpStr := Tokenizer.TokenString;
    If ( FSymbolTable.IndexOf(tmpStr) > -1 ) Then
      Error('Duplicated symbol : "%s"',[tmpStr]);
    sblItem := TEnumItemDefinition.Create(tmpStr,sbl,tmpInt);
    FSymbolTable.Add(sblItem);
    sbl.AddItem(sblItem);
    NextToken();
    If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptRigthParenthesis)) Then
      Break;
    If AnsiSameText(Tokenizer.TokenString,GetPascalTokenStr(ptComma)) Then
      NextToken();
    Tokenizer.CheckToken(toSymbol);
    Inc(tmpInt);
  Until ( Tokenizer.Token = toEOF ) ;
  CheckCurrentString(GetPascalTokenStr(ptRigthParenthesis));
  NextToken();
  CheckCurrentString(GetPascalTokenStr(ptSemicolon));
  NextToken();
end;

procedure TPascalParser.ParseClassType(const AName: String);
Var
  sbl : TClassTypeDefinition;
begin
  sbl := TClassTypeDefinition.Create(AName);
  FSymbolTable.Add(sbl);
  FCurrentSymbol := sbl;
  CheckCurrentString(GetPascalTokenStr(ptClass));
  NextToken();
  ReadUntil(GetPascalTokenStr(ptEnd),True);
  CheckCurrentString(GetPascalTokenStr(ptEnd));
  NextToken();
  CheckCurrentString(GetPascalTokenStr(ptSemicolon));
  NextToken();
end;

constructor TPascalParser.Create(AStream : TStream; ASymbolTable : TSymbolTable);
begin
  Assert(Assigned(AStream));
  Assert(Assigned(ASymbolTable));
  FStream := AStream;
  FTokenizer := TParser.Create(FStream);
  FSymbolTable := ASymbolTable;
  FSymbolTable.Add(CreateWstInterfaceSymbolTable());
  FCurrentSymbol := Nil;
end;

destructor TPascalParser.Destroy();
begin
  FTokenizer.Free();
  inherited Destroy();
end;

function TPascalParser.Parse(): Boolean;
begin
  BeginParsing();
  Try
    Try
      SkipComment();
      ParseHeader();
      ParseInterfaceSection();
    Except
      On E : Exception Do Begin
        Result := False;
        SetErrorMessage(E.Message );
      End;
    End;
  Finally
    EndParsing();
  End;
end;

end.

