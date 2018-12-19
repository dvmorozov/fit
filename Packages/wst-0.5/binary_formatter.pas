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
unit binary_formatter; 

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, service_intf, imp_utils,
  base_binary_formatter;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Const
  sCONTENT_TYPE = 'contenttype';
  sBINARY_CONTENT = 'binary';
  sPROTOCOL_NAME = sBINARY_CONTENT;
  
  sTARGET = 'target';
  
Type

{$M+}
  TBinaryFormatter = class(TBaseBinaryFormatter,IFormatterClient)
  private
    FPropMngr : IPropertyManager;
    FCallProcedureName : string;
    FCallTarget : String;
  protected
  public
    function GetPropertyManager():IPropertyManager;

    procedure BeginCall(
      const AProcName,
            ATarget      : string;
            ACallContext : ICallContext
    );
    procedure EndCall();
    procedure BeginCallRead(ACallContext : ICallContext);

    function GetCallProcedureName():String;
    function GetCallTarget():String;
  End;


  { TBinaryCallMaker }

  TBinaryCallMaker = class(TSimpleFactoryItem,ICallMaker)
  Private
    FPropMngr : IPropertyManager;
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure MakeCall(
      ASerializer : IFormatterClient;
      ATransport  : ITransport
    );
  End;

implementation

function TBinaryFormatter.GetPropertyManager(): IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

procedure TBinaryFormatter.BeginCall(
  const AProcName,
        ATarget      : string;
        ACallContext : ICallContext
);
begin
  FCallProcedureName := AProcName;
  FCallTarget := ATarget;

  BeginObject('Body',Nil);
    BeginObject(FCallTarget,Nil);
      BeginObject(FCallProcedureName,Nil);
end;

procedure TBinaryFormatter.EndCall();
begin
      EndScope();
    EndScope();
  EndScope();
end;

procedure TBinaryFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  s,nme : string;
  e : EBaseRemoteException;
begin
  ClearStack();
  PushStack(GetRootData(),stObject);
  s := 'Body';
  BeginObjectRead(s,nil);
    s := StackTop().GetByIndex(0)^.Name;
    If AnsiSameText(s,'Fault') Then Begin
      BeginObjectRead(s,nil);
      e := EBaseRemoteException.Create('');
      Try
        nme := 'faultcode';
        Get(TypeInfo(string),nme,s);
        e.FaultCode := s;
        nme := 'faultstring';
        Get(TypeInfo(string),nme,s);
        e.FaultString := s;
        e.Message := Format('%s : "%s"',[e.FaultCode,e.FaultString]);
      Except
        FreeAndNil(e);
        Raise;
      End;
      Raise e;
    End;
    FCallTarget := s;
    BeginObjectRead(FCallTarget,nil);
      FCallProcedureName := StackTop().GetByIndex(0)^.Name;
      BeginObjectRead(FCallProcedureName,nil);
end;

function TBinaryFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TBinaryFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

{ TBinaryCallMaker }

constructor TBinaryCallMaker.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TBinaryCallMaker.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TBinaryCallMaker.GetPropertyManager(): IPropertyManager;
begin
  Result:= FPropMngr;
end;

procedure TBinaryCallMaker.MakeCall(
  ASerializer : IFormatterClient;
  ATransport  : ITransport
);
Var
  rqt, rsps : TMemoryStream;
  propMngr : IPropertyManager;
begin
  Assert(Assigned(ASerializer));
  Assert(Assigned(ATransport));
  propMngr := ATransport.GetPropertyManager();
  propMngr.SetProperty(
    sCONTENT_TYPE,
    sBINARY_CONTENT
  );
  propMngr.SetProperty(
    sFORMAT,
    sPROTOCOL_NAME
  );
  rsps := Nil;
  rqt := TMemoryStream.Create();
  Try
    rsps := TMemoryStream.Create();
    ASerializer.SaveToStream(rqt);
    rqt.Position := 0;
    ATransport.SendAndReceive(rqt,rsps);
    rqt.Clear();
    rsps.Position := 0;
    ASerializer.Clear();
    ASerializer.LoadFromStream(rsps);
  Finally
    rsps.Free();
    rqt.Free();
  End;
end;

procedure RegisterBinaryProtocol();
begin
  GetFormaterRegistry().Register(
    sPROTOCOL_NAME,
    TSimpleItemFactory.Create(TBinaryFormatter) as IItemFactory,
    TSimpleItemFactory.Create(TBinaryCallMaker) as IItemFactory
  );
end;

Initialization
  RegisterBinaryProtocol();
end.
