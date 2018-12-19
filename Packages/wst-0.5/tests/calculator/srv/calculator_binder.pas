{
This unit has been produced by ws_helper.
  Input unit name : "calculator".
  This unit name  : "calculator_binder".
  Date            : "12/11/2006 11:22".
}
unit calculator_binder;
{$mode objfpc}{$H+}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, calculator;

type


  TCalculator_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure AddIntHandler(AFormatter:IFormatterResponse);
    procedure DivIntHandler(AFormatter:IFormatterResponse);
    procedure DoAllOperationsHandler(AFormatter:IFormatterResponse);
    procedure DoOperationHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TCalculator_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterCalculatorService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TCalculator_ServiceBinder implementation }
procedure TCalculator_ServiceBinder.AddIntHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : TBinaryArgsResult;
Begin
  callCtx := GetCallContext();
  Pointer(returnVal) := Nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.AddInt(A,B);
  If Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TBinaryArgsResult),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TCalculator_ServiceBinder.DivIntHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : Integer;
Begin
  callCtx := GetCallContext();
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.DivInt(A,B);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(Integer),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TCalculator_ServiceBinder.DoAllOperationsHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  returnVal : TBinaryArgsResultArray;
Begin
  callCtx := GetCallContext();
  Pointer(returnVal) := Nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.DoAllOperations(A,B);
  If Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TBinaryArgsResultArray),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TCalculator_ServiceBinder.DoOperationHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : ICalculator;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  A : Integer;
  B : Integer;
  AOperation : TCalc_Op;
  returnVal : TBinaryArgsResult;
Begin
  callCtx := GetCallContext();
  Pointer(returnVal) := Nil;
  
  strPrmName := 'A';  AFormatter.Get(TypeInfo(Integer),strPrmName,A);
  strPrmName := 'B';  AFormatter.Get(TypeInfo(Integer),strPrmName,B);
  strPrmName := 'AOperation';  AFormatter.Get(TypeInfo(TCalc_Op),strPrmName,AOperation);
  
  tmpObj := Self.GetFactory().CreateInstance() as ICalculator;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.DoOperation(A,B,AOperation);
  If Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TBinaryArgsResult),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;


constructor TCalculator_ServiceBinder.Create();
Begin
  Inherited Create(GetServiceImplementationRegistry().FindFactory('ICalculator'));
  RegisterVerbHandler('AddInt',@AddIntHandler);
  RegisterVerbHandler('DivInt',@DivIntHandler);
  RegisterVerbHandler('DoAllOperations',@DoAllOperationsHandler);
  RegisterVerbHandler('DoOperation',@DoOperationHandler);
End;


{ TCalculator_ServiceBinderFactory }
function TCalculator_ServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TCalculator_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterCalculatorService();
Begin
  GetServerServiceRegistry().Register('ICalculator',TCalculator_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$IF DECLARED(Register_calculator_NameSpace)}
  Register_calculator_NameSpace();
  {$ENDIF}

  {$i calculator.wst}

End.
