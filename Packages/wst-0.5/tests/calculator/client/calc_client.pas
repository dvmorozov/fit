program calc_client;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  service_intf,
  base_soap_formatter, soap_formatter, binary_formatter,
  ics_tcp_protocol,
  calculator, calculator_proxy, TypInfo;

Var
  calcObj : ICalculator;
  i, j, k : Integer;
  rk : TBinaryArgsResult;
  vA : TBinaryArgsResultArray;
  f : IFormatterClient;
  s : TStream;
  msgProt : string;
begin
{  vA := TBinaryArgsResultArray.Create();
  vA.SetLength(2);
  f := TBinaryFormatter.Create() as IFormatterClient;
  f.BeginCall('pr','trgt');
    f.Put('xx',TypeInfo(TBinaryArgsResultArray),vA);
  f.EndCall();
  s:= TMemoryStream.Create();
  f.SaveToStream(s);
  f := Nil;
  f := TBinaryFormatter.Create() as IFormatterClient;
  s.Position := 0;
  WriteLn('------------------------------------------');
  WriteLn('------------------------------------------');
  f.LoadFromStream(s);
  ReadLn();
  Exit;}
  //Client
  Write('Enter msg protocol :');
  ReadLn(msgProt);
  If Not( AnsiSameText(msgProt,'SOAP') Or AnsiSameText(msgProt,'binary') ) Then
    msgProt := 'binary';
  Writeln('USED Msg protocol = ',msgProt);
  ICS_RegisterTCP_Transport();

  rk := nil;
  Try
    Try
      calcObj := TCalculator_Proxy.Create(
                   'ICalculator',                       // Target
                   msgProt,//'SOAP',//'binary',                             // Protocol Data
                   'TCP:Address=127.0.0.1;Port=1234;target=ICalculator'    // Transport Data
                 );
      WriteLn('Calculator test.');
      Write('Enter A = '); ReadLn(i);
      Write('Enter B = '); ReadLn(j);
      k := calcObj.DivInt(i,j);
      Write(' A / B = '); WriteLn(k);
      rk := calcObj.AddInt(i,j);
      WriteLn(Format('     ( %d %s %d ) = %d',[rk.Arg_A, rk.Arg_OP, rk.Arg_B, rk.Arg_R]));
      WriteLn('----------------------------------------------');
      vA := calcObj.DoAllOperations(i,j);
      Try
        For i := 0 To Pred(vA.Length) Do
          WriteLn(Format('     ( %d %s %d ) = %d;   OP=%s',[vA[i].Arg_A, vA[i].Arg_OP, vA[i].Arg_B, vA[i].Arg_R,GetEnumName(TypeInfo(TCalc_Op),Ord(vA[i].Arg_OpEnum))]));
      Finally
        vA.Free();
      End;
    Except
      On E : ESOAPException Do Begin
        WriteLn('Oups ( SOAP Exception ) :');
        WriteLn('  Code=',E.FaultCode);
        WriteLn('  String=',E.FaultString);
      End;
      On E : Exception Do Begin
        WriteLn('Oups:');
        WriteLn(E.Message);
      End;
    End;
  Finally
    rk.Free();
  End;
  ReadLn();
end.
