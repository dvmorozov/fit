{
This unit has been produced by ws_helper.
  Input unit name : "calculator".
  This unit name  : "calculator_proxy".
  Date            : "12/11/2006 11:21".
}
Unit calculator_proxy;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, calculator;

Type


  TCalculator_Proxy=class(TBaseProxy,ICalculator)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function AddInt(
      Const A : Integer; 
      Const B : Integer
    ):TBinaryArgsResult;
    function DivInt(
      Const A : Integer; 
      Const B : Integer
    ):Integer;
    function DoAllOperations(
      Const A : Integer; 
      Const B : Integer
    ):TBinaryArgsResultArray;
    function DoOperation(
      Const A : Integer; 
      Const B : Integer; 
      Const AOperation : TCalc_Op
    ):TBinaryArgsResult;
  End;

Implementation
uses wst_resources_imp, metadata_repository;

{ TCalculator_Proxy implementation }

class function TCalculator_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(ICalculator);
end;

function TCalculator_Proxy.AddInt(
  Const A : Integer; 
  Const B : Integer
):TBinaryArgsResult;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('AddInt', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(Integer), A);
      locSerializer.Put('B', TypeInfo(Integer), B);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TBinaryArgsResult), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;

function TCalculator_Proxy.DivInt(
  Const A : Integer; 
  Const B : Integer
):Integer;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('DivInt', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(Integer), A);
      locSerializer.Put('B', TypeInfo(Integer), B);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(Integer), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;

function TCalculator_Proxy.DoAllOperations(
  Const A : Integer; 
  Const B : Integer
):TBinaryArgsResultArray;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('DoAllOperations', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(Integer), A);
      locSerializer.Put('B', TypeInfo(Integer), B);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TBinaryArgsResultArray), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;

function TCalculator_Proxy.DoOperation(
  Const A : Integer; 
  Const B : Integer; 
  Const AOperation : TCalc_Op
):TBinaryArgsResult;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('DoOperation', GetTarget(),(Self as ICallContext));
      locSerializer.Put('A', TypeInfo(Integer), A);
      locSerializer.Put('B', TypeInfo(Integer), B);
      locSerializer.Put('AOperation', TypeInfo(TCalc_Op), AOperation);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TBinaryArgsResult), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i calculator.wst}

  {$IF DECLARED(Register_calculator_ServiceMetadata)}
  Register_calculator_ServiceMetadata();
  {$ENDIF}
End.
