{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit imp_utils;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Type

  EPropertyManagerException = class(EServiceException)
  End;
  
  { TPublishedPropertyManager }

  TPublishedPropertyManager = class(TInterfacedObject,IPropertyManager)
  Private
    FParent : TObject;
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  Protected
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  Public
    constructor Create(AParent : TObject);
  End;

  function IsStrEmpty(Const AStr:String):Boolean;
  function ExtractOptionName(const ACompleteName : string):string;
  
implementation

function IsStrEmpty(Const AStr:String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function ExtractOptionName(const ACompleteName : string):string;
var
  i, c : Integer;
begin
  Result := '';
  c := Length(ACompleteName);
  for i := c downto 1 do begin
    if ( ACompleteName[i] = '_' ) then
      Break;
    Result := ACompleteName[i] + Result;
  end;
  Result := Trim(Result);
end;

{ TPublishedPropertyManager }

procedure TPublishedPropertyManager.Error(const AMsg: string);
begin
  Raise EPropertyManagerException.Create(AMsg);
end;

procedure TPublishedPropertyManager.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EPropertyManagerException.CreateFmt(AMsg,AArgs);
end;

procedure TPublishedPropertyManager.SetProperty(const AName, AValue: string);
Var
  pinf : PPropInfo;
  int64Val : Int64;
begin
  pinf := GetPropInfo(FParent,AName);
  If Assigned(pinf) And Assigned(pinf^.SetProc) Then Begin
    Case pinf^.PropType^.Kind of
      tkLString{$IFDEF FPC},tkSString,tkAString{$ENDIF},tkWString:
        SetStrProp(FParent,pinf,AValue);
      tkEnumeration :
        SetEnumProp(FParent,pinf,AValue);
      tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
        Begin
          If TryStrToInt64(AValue,int64Val) Then
            SetOrdProp(FParent,AName,int64Val);
        End;
      {$IFDEF FPC}
      tkBool :
        SetOrdProp(FParent,AName,Ord(StrToBool(AValue)));
      {$ENDIF}
    End;
  End;
end;

procedure TPublishedPropertyManager.SetProperties(const APropsStr: string);
Var
  lst : TStringList;
  i : Integer;
begin
  If IsStrEmpty(APropsStr) Then
    Exit;
  lst := TStringList.Create();
  Try
    lst.QuoteChar := #0;
    lst.Delimiter := PROP_LIST_DELIMITER;
    lst.DelimitedText := APropsStr;
    For i := 0 To Pred(lst.Count) Do
      SetProperty(lst.Names[i],lst.ValueFromIndex[i]);
  Finally
    lst.Free();
  End;
end;

function TPublishedPropertyManager.GetProperty(const AName: String): string;
Var
  pinf : PPropInfo;
begin
  Result := '';
  pinf := GetPropInfo(FParent,AName);
  If Assigned(pinf) And Assigned(pinf^.SetProc) Then Begin
    Case pinf^.PropType^.Kind of
      tkLString{$IFDEF FPC},tkSString,tkAString{$ENDIF},tkWString:
        Result := GetStrProp(FParent,pinf);
      tkEnumeration :
        Result := GetEnumProp(FParent,pinf);
      tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
        Result := IntToStr(GetOrdProp(FParent,pinf));
    End;
  End;
end;

function TPublishedPropertyManager.GetPropertyNames(ADest: TStrings): Integer;
Var
  propList : PPropList;
  i, propListLen : Integer;
begin
  ADest.Clear();
  propListLen := GetPropList(PTypeInfo(FParent.ClassInfo),propList);
  Try
    For i := 0 To Pred(propListLen) Do Begin
      If ( propList^[i]^.PropType^.Kind in
           [ tkLString{$IFDEF FPC},tkSString,tkAString{$ENDIF},tkWString,
             tkEnumeration,
             tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF}
           ]
         )
      Then
        ADest.Add(propList^[i]^.Name);
    End;
  Finally
    Freemem(propList,propListLen*SizeOf(Pointer));
  End;
  Result := ADest.Count;
end;

procedure TPublishedPropertyManager.Clear();
begin

end;

procedure TPublishedPropertyManager.Copy(
        ASource: IPropertyManager;
  const AClearBefore: Boolean
);
Var
  lst : TStringList;
  i : Integer;
  s : string;
begin
  If AClearBefore Then
    Clear();
  If Assigned(ASource) Then Begin
    lst := TStringList.Create();
    Try
      ASource.GetPropertyNames(lst);
      For i := 0 To Pred(lst.Count) Do Begin
        s := lst[i];
        SetProperty(s,ASource.GetProperty(s));
      End;
    Finally
      lst.Free();
    End;
  End;
end;

constructor TPublishedPropertyManager.Create(AParent: TObject);
begin
  Assert(Assigned(AParent));
  FParent := AParent;
end;


end.

