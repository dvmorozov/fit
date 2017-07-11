unit Unit6; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormHint }

  TFormHint = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormHint: TFormHint;

implementation

{ TFormHint }

procedure TFormHint.Timer1Timer(Sender: TObject);
begin
    Close;
end;

procedure TFormHint.FormShow(Sender: TObject);
begin
    ClientWidth := Label1.Width + 14;
    ClientHeight := Label1.Height + 14;
end;

initialization
  {$I Unit6.lrs}
end.



