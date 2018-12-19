unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
Var
  s : single;
begin
  If TryStrToFloat(Edit1.Text,s) then begin
    Memo1.Clear();
    Memo1.Lines.Add(FloatToStrF(s,ffExponent,8,5));
    Memo1.Lines.Add(FloatToStrF(s,ffGeneral,8,5));
    Memo1.Lines.Add(FloatToStrF(s,ffFixed,8,5));
    Memo1.Lines.Add(FloatToStrF(s,ffNumber,8,5));
    Memo1.Lines.Add(FloatToStrF(s,ffCurrency,8,5)) ;
    Memo1.Lines.Add(FormatFloat(Edit2.Text,s));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  v : TFloatRec;
  s : single;
  b : string;
begin
  If TryStrToFloat(Edit1.Text,s) then begin
    FillChar(v,SizeOf(v),0);
    FloatToDecimal(v,s,7,1);
    Memo1.Clear();
    Memo1.Lines.Add(Format('%d',[v.Exponent]));
  end;
end;

initialization
  {$I unit1.lrs}

end.

