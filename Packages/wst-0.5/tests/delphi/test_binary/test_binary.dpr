program test_binary;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  base_service_intf,
  service_intf,
  base_binary_formatter,
  binary_formatter,
  //ics_tcp_protocol,
  user_service_intf,
  user_service_intf_proxy,
  TypInfo,
  synapse_http_protocol,
  synapse_tcp_protocol;

Var
  calcObj : ICalculator;
  i, j, k : Integer;
  rk : TBinaryArgsResult;
  vA : TBinaryArgsResultArray;
  msgProt, s : string;
begin
  msgProt := 'binary';
  Writeln('USED Msg protocol = ',msgProt);
  //ICS_RegisterTCP_Transport();
  SYNAPSE_RegisterTCP_Transport();

  rk := nil;
  Try
    Try
      calcObj := TCalculator_Proxy.Create(
                   'ICalculator',                       // Target
                   msgProt,//'SOAP',//'binary',                             // Protocol Data
                   'TCP:Address=127.0.0.1;Port=1234;target=ICalculator'    // Transport Data
                 );
      WriteLn('Calculator test.');
      while True do begin
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
        Write('Stop now Y/N:  ');
        Readln(s);
        s := UpperCase(Trim(s));
        if ( Length(s) > 0 ) and ( s[1] = 'Y' ) then
          Break;
      end;
    Except
      On E : Exception Do Begin
        WriteLn('Oups:');
        WriteLn(E.Message);
      End;
    End;
  Finally
    rk.Free();
  End;
  ReadLn;
end.
