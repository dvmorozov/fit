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
{$INCLUDE wst_global.inc}
unit metadata_generator;

interface

uses
  Classes, SysUtils,
  pastree, pascal_parser_intf, binary_streamer;

const
  sWST_META = 'WST_METADATA_0.2.2.0';

type

  { TMetadataGenerator }

  TMetadataGenerator = class
  private
    FStream : IDataStore;
    FSymbolTable: TwstPasTreeContainer;

    procedure GenerateHeader();
    procedure GenerateIntfMetadata(AIntf : TPasClassType);
  public
    constructor Create(
      ASymTable   : TwstPasTreeContainer;
      ADstStream  : IDataStore
    );
    procedure Execute();
  end;


implementation

{ TMetadataGenerator }

procedure TMetadataGenerator.GenerateHeader();
var
  c, i, k : LongInt;
  typeList : TList;
  elt : TPasElement;
begin
  FStream.WriteStr(sWST_META);
  FStream.WriteStr(FSymbolTable.CurrentModule.Name);
  k := 0;
  typeList := FSymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to pred(c) do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then
      inc(k);
  end;
  FStream.WriteInt8U(k);
end;

procedure TMetadataGenerator.GenerateIntfMetadata(AIntf: TPasClassType);

  procedure WriteMethod(AMeth : TPasProcedure);

    procedure WriteParam(APrm : TPasArgument);
    begin
      FStream.WriteStr(APrm.Name);
      FStream.WriteStr(APrm.ArgType.Name);
      FStream.WriteEnum(Ord(APrm.Access));
    end;

    procedure WriteResult(ARes : TPasResultElement);
    begin
      FStream.WriteStr(ARes.Name);
      FStream.WriteStr(ARes.ResultType.Name);
      FStream.WriteEnum(Ord(argOut));
    end;

  var
    j, k : LongInt;
    argLst : TList;
  begin
    argLst := AMeth.ProcType.Args;
    k := argLst.Count;
    FStream.WriteStr(AMeth.Name);
    if AMeth.InheritsFrom(TPasFunction) then begin
      FStream.WriteInt8U(k + 1);
    end else begin
      FStream.WriteInt8U(k);
    end;
    for j := 0 to pred(k) do begin
      WriteParam(TPasArgument(argLst[j]));
    end;
    if AMeth.InheritsFrom(TPasFunction) then begin
      WriteResult(TPasFunctionType(AMeth.ProcType).ResultEl);
    end;
  end;
  
var
  i, c : LongInt;
  mbrs : TList;
  elt : TPasElement;
begin
  FStream.WriteStr(AIntf.Name);
  c := GetElementCount(AIntf.Members,TPasProcedure);
  FStream.WriteInt8U(c);
  mbrs := AIntf.Members;
  for i := 0 to pred(mbrs.Count) do begin
    elt := TPasElement(mbrs[i]);
    if elt.InheritsFrom(TPasProcedure) then begin
      WriteMethod(TPasProcedure(elt));
    end;
  end;
end;

constructor TMetadataGenerator.Create(ASymTable: TwstPasTreeContainer;ADstStream: IDataStore);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ADstStream));
  FSymbolTable := ASymTable;
  FStream := ADstStream;
end;

procedure TMetadataGenerator.Execute();
Var
  i,c : Integer;
  intf : TPasClassType;
  typeList : TList;
  elt : TPasElement;
begin
  GenerateHeader();
  typeList := FSymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := Pred(typeList.Count);
  for i := 0 to c do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
      intf := TPasClassType(elt);
      GenerateIntfMetadata(intf);
    end;
  end;
end;

end.

