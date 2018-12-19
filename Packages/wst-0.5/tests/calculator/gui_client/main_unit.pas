unit main_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, calculator, calculator_proxy;

type

  { Tfmain }

  Tfmain = class(TForm)
    btnExec: TButton;
    btnInit: TButton;
    btnClearLog: TButton;
    edtAddress: TEdit;
    edtFormat: TEdit;
    edtA: TEdit;
    edtB: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmoLog: TMemo;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
    procedure btnInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FObj : ICalculator;
    procedure LogMsg(const AMsg : string);
  public
  end;

var
  fmain: Tfmain;

implementation
uses TypInfo, base_service_intf, soap_formatter, binary_formatter,
     ics_tcp_protocol, ics_http_protocol,
     //synapse_http_protocol,
     library_protocol,
     service_intf;

{ Tfmain }

procedure Tfmain.btnExecClick(Sender: TObject);
Var
  a, b, k : Integer;
  rk : TBinaryArgsResult;
  vA : TBinaryArgsResultArray;
  ch : TCalcHeader;
  rch : TCalcResultHeader;
  hdrs : ICallContext;
begin
  try
    if not Assigned(FObj) then
      FObj := TCalculator_Proxy.Create(
                'ICalculator', // Target
                edtFormat.Text,//'SOAP',//'binary',  // Protocol Data
                edtAddress.Text
                //'http:Address=http://127.0.0.1:8000/services/ICalculator'
                //'TCP:Address=127.0.0.1;Port=1234;target=ICalculator'
              );//'TCP:Address=127.0.0.1;Port=1234;target=ICalculator'

    ch := TCalcHeader.Create();
    ch.mustUnderstand := 0;
    ch.Login := 'azerty';
    ch.Password := 'qwerty';
    ch.WantedPrecision := 121076;
    hdrs := FObj as ICallContext;
    hdrs.AddHeader(ch,true);
    ch := TCalcHeader.Create();
    ch.Login := 'azerty';
    ch.Password := '';
    ch.WantedPrecision := 321654;
    hdrs.AddHeader(ch,true);
    ch := nil;
    rk := Nil;
    vA := Nil;
    a := StrToInt(edtA.Text);
    b := StrToInt(edtB.Text);
    Try
      rk := FObj.AddInt(a,b);
      hdrs.ClearHeaders(hdOut);
      LogMsg(Format('Header Count = %d',[hdrs.GetHeaderCount(AllHeaderDirection)]));
      if ( hdrs.GetHeaderCount(AllHeaderDirection) > 0 ) then begin
        LogMsg(Format('Header(0) Class  = %s',[hdrs.GetHeader(0).ClassName]));
        if hdrs.GetHeader(0).InheritsFrom(TCalcResultHeader) then begin
          rch := hdrs.GetHeader(0) as TCalcResultHeader;
          LogMsg(Format('Header(0) Dir  = %d; TimeStamp = %s; SessionID = %s; Login =%s; Password = %s',[Ord(rch.Direction),rch.TimeStamp,rch.SessionID,rch.Login,rch.Password]));
        end;
      end;
      LogMsg(Format('     ( %d %s %d ) = %d; Comment = %s',[rk.Arg_A, rk.Arg_OP, rk.Arg_B, rk.Arg_R,rk.Comment]));
      

      rk := FObj.DoOperation(a,b,coSub);
      LogMsg(Format('     ( %d %s %d ) = %d; Comment = %s',[rk.Arg_A, rk.Arg_OP, rk.Arg_B, rk.Arg_R,rk.Comment]));

      vA := FObj.DoAllOperations(a,b);
      For k := 0 To Pred(vA.Length) Do
        LogMsg(Format('     ( %d %s %d ) = %d;   OP=%s;  Comment = %s',[vA[k].Arg_A, vA[k].Arg_OP, vA[k].Arg_B, vA[k].Arg_R,GetEnumName(TypeInfo(TCalc_Op),Ord(vA[k].Arg_OpEnum)),vA[k].Comment]));
    Finally
      FreeAndNil(rk);
      FreeAndNil(vA);
    End;
  except
    on e : Exception do
      ShowMessage(e.Message);
  end;
end;

procedure Tfmain.btnClearLogClick(Sender: TObject);
begin
  mmoLog.Clear();
end;

procedure Tfmain.btnInitClick(Sender: TObject);
begin
  FObj := Nil;
end;

procedure Tfmain.FormCreate(Sender: TObject);
begin
  FObj := Nil;
  ICS_RegisterTCP_Transport();
  ICS_RegisterHTTP_Transport();
  LIB_Register_Transport();
  //SYNAPSE_RegisterHTTP_Transport();
end;

procedure Tfmain.LogMsg(const AMsg: string);
begin
  mmoLog.Lines.Add(AMsg);
end;

initialization
  {$I main_unit.lrs}
  
  RegisterStdTypes();
  
end.

