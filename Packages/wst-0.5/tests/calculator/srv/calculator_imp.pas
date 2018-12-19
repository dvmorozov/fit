{
This unit has been produced by ws_helper.
  Input unit name : "calculator".
  This unit name  : "calculator_imp".
  Date            : "02/07/2006 16:49".
}
Unit calculator_imp;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, calculator;

Type


  TCalculator_ServiceImp=class(TBaseServiceImplementation,ICalculator)
  Protected
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


  procedure RegisterCalculatorImplementationFactory();

Implementation

{ TCalculator_ServiceImp implementation }
function TCalculator_ServiceImp.AddInt(
  Const A : Integer;
  Const B : Integer
):TBinaryArgsResult;
var
  hdr : TCalcResultHeader;
  h : TCalcHeader;
  cc : ICallContext;
Begin
  hdr := TCalcResultHeader.Create();
  cc := GetCallContext();
  if Assigned(cc) and ( cc.GetHeaderCount([hdIn]) > 0 ) and ( cc.GetHeader(0).InheritsFrom(TCalcHeader) ) then begin
    h := cc.GetHeader(0) as TCalcHeader;
    h.Understood := True;
    hdr.Assign(h);
  end;
  hdr.TimeStamp := DateTimeToStr(Now());
  hdr.SessionID := 'testSession';
  cc.AddHeader(hdr,True);
  hdr := nil;
  Result := TBinaryArgsResult.Create();
  Try
    Result.Arg_OP := '+';
    Result.Arg_OpEnum := coAdd;
    Result.Arg_A := A;
    Result.Arg_B := B;
    Result.Arg_R := A + B;
    Result.Comment := 'Doing an + operation';
  Except
    FreeAndNil(Result);
    Raise;
  End;
End;

function TCalculator_ServiceImp.DivInt(
  Const A : Integer;
  Const B : Integer
):Integer;
Begin
  Result := A div B;
End;

function TCalculator_ServiceImp.DoAllOperations(
  Const A : Integer;
  Const B : Integer
):TBinaryArgsResultArray;
Begin
  Result := TBinaryArgsResultArray.Create();
  Result.SetLength(4);
  With Result[0] do Begin
    Arg_A := A;
    Arg_B := B;
    Arg_OP := '-';
    Arg_OpEnum := coSub;
    Arg_R := Arg_A - Arg_B;
  End;
  With Result[1] do Begin
    Arg_A := A;
    Arg_B := B;
    Arg_OP := '+';
    Arg_OpEnum := coAdd;
    Arg_R := Arg_A + Arg_B;
  End;
  With Result[2] do Begin
    Arg_A := A;
    Arg_B := B;
    Arg_OP := '*';
    Arg_OpEnum := coMul;
    Arg_R := Arg_A * Arg_B;
  End;
  With Result[3] do Begin
    Arg_A := A;
    Arg_B := B;
    Arg_OP := '/';
    Arg_OpEnum := coDiv;
    Arg_R := Arg_A div Arg_B;
  End;
End;

function TCalculator_ServiceImp.DoOperation(
  Const A : Integer;
  Const B : Integer;
  Const AOperation : TCalc_Op
):TBinaryArgsResult;
Begin
  Result := TBinaryArgsResult.Create();
  try
    Result.Arg_A := A;
    Result.Arg_B := B;
    Result.Arg_OP := 'X';
    Result.Arg_OpEnum := AOperation;
    Result.Comment := 'Doing an operation...';

    case AOperation of
      coAdd : Result.Arg_R := Result.Arg_A + Result.Arg_B;
      coSub : Result.Arg_R := Result.Arg_A - Result.Arg_B;
      coMul : Result.Arg_R := Result.Arg_A * Result.Arg_B;
      coDiv : Result.Arg_R := Result.Arg_A div Result.Arg_B;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
End;

procedure RegisterCalculatorImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register(
    'ICalculator',
    TImplementationFactory.Create(TCalculator_ServiceImp) as IServiceImplementationFactory
  ).RegisterExtension(['TLoggerServiceExtension']);
End;

End.
