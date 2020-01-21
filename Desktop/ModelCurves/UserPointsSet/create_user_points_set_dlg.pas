{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TCreateUserPointsSetDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)

}
unit create_user_points_set_dlg;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
  LCLIntf, Classes, LResources, Forms, StdCtrls, ExtCtrls;

type

  { TCreateUserPointsSetDlg }

  TCreateUserPointsSetDlg = class(TForm)
    Bevel1: TBevel;
    ButtonSin: TButton;
    ButtonTh: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ButtonMul: TButton;
    ButtonArcsin: TButton;
    ButtonArccos: TButton;
    ButtonArctg: TButton;
    Button1: TButton;
    Button2: TButton;
    ButtonCos: TButton;
    Button3: TButton;
    ButtonMinus: TButton;
    ButtonArsh: TButton;
    ButtonArch: TButton;
    ButtonArth: TButton;
    Button0: TButton;
    ButtonBrackets: TButton;
    ButtonDecimal: TButton;
    ButtonPlus: TButton;
    ButtonSch: TButton;
    ButtonTg: TButton;
    ButtonCsch: TButton;
    ButtonLn: TButton;
    ButtonPower: TButton;
    ButtonExp: TButton;
    ButtonAbs: TButton;
    ButtonSqrt: TButton;
    ButtonCtg: TButton;
    ButtonCth: TButton;
    ButtonArcctg: TButton;
    ButtonArcth: TButton;
    Button7: TButton;
    ButtonLog: TButton;
    Button9: TButton;
    Button8: TButton;
    ButtonDiv: TButton;
    ButtonSh: TButton;
    ButtonCh: TButton;
    BtnContinue: TButton;
    EditCurveName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    BtnCancel: TButton;
    EditExpression: TEdit;
    procedure Button0Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ButtonAbsClick(Sender: TObject);
    procedure ButtonArccosClick(Sender: TObject);
    procedure ButtonArcctgClick(Sender: TObject);
    procedure ButtonArchClick(Sender: TObject);
    procedure ButtonArcsinClick(Sender: TObject);
    procedure ButtonArctgClick(Sender: TObject);
    procedure ButtonArcthClick(Sender: TObject);
    procedure ButtonArshClick(Sender: TObject);
    procedure ButtonArthClick(Sender: TObject);
    procedure ButtonBracketsClick(Sender: TObject);
    procedure ButtonChClick(Sender: TObject);
    procedure ButtonCosClick(Sender: TObject);
    procedure ButtonCschClick(Sender: TObject);
    procedure ButtonCtgClick(Sender: TObject);
    procedure ButtonCthClick(Sender: TObject);
    procedure ButtonDecimalClick(Sender: TObject);
    procedure ButtonDivClick(Sender: TObject);
    procedure ButtonExpClick(Sender: TObject);
    procedure ButtonLnClick(Sender: TObject);
    procedure ButtonLogClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
    procedure ButtonMulClick(Sender: TObject);
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonPowerClick(Sender: TObject);
    procedure ButtonSchClick(Sender: TObject);
    procedure ButtonShClick(Sender: TObject);
    procedure ButtonSinClick(Sender: TObject);
    procedure ButtonSqrtClick(Sender: TObject);
    procedure ButtonTgClick(Sender: TObject);
    procedure ButtonThClick(Sender: TObject);
    procedure EditExpressionKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    procedure InsertFunc(FuncName: string);
    procedure InsertSymbol(Symbol: string);
  public
    { public declarations }
  end; 

var
  CreateUserPointsSetDlg: TCreateUserPointsSetDlg;

implementation

{ TCreateUserPointsSetDlg }

procedure TCreateUserPointsSetDlg.InsertFunc(FuncName: string);
var Temp: LongInt;
begin
    with EditExpression do
    begin
        { Selected text is replaced by new. }
        Temp := SelStart;
        SelText := FuncName + '()';
        SelStart := Temp + Length(FuncName) + 1;
    end;
end;

procedure TCreateUserPointsSetDlg.InsertSymbol(Symbol: string);
var Temp: LongInt;
begin
    with EditExpression do
    begin
        Temp := SelStart;
        SelText := Symbol;
        SelStart := Temp + Length(Symbol);
    end;
end;

procedure TCreateUserPointsSetDlg.ButtonSinClick(Sender: TObject);
begin
    InsertFunc('Sin');
end;

procedure TCreateUserPointsSetDlg.ButtonSqrtClick(Sender: TObject);
begin
    InsertFunc('Sqrt');
end;

procedure TCreateUserPointsSetDlg.ButtonTgClick(Sender: TObject);
begin
    InsertFunc('Tg');
end;

procedure TCreateUserPointsSetDlg.ButtonThClick(Sender: TObject);
begin
    InsertFunc('Th');
end;

procedure TCreateUserPointsSetDlg.ButtonCosClick(Sender: TObject);
begin
    InsertFunc('Cos');
end;

procedure TCreateUserPointsSetDlg.ButtonCschClick(Sender: TObject);
begin
    InsertFunc('Csch');
end;

procedure TCreateUserPointsSetDlg.ButtonChClick(Sender: TObject);
begin
    InsertFunc('Ch');
end;

procedure TCreateUserPointsSetDlg.ButtonArcsinClick(Sender: TObject);
begin
    InsertFunc('Arcsin');
end;

procedure TCreateUserPointsSetDlg.ButtonArctgClick(Sender: TObject);
begin
    InsertFunc('Arctg');
end;

procedure TCreateUserPointsSetDlg.ButtonArcthClick(Sender: TObject);
begin
    InsertFunc('Arcth');
end;

procedure TCreateUserPointsSetDlg.ButtonArshClick(Sender: TObject);
begin
    InsertFunc('Arsh');
end;

procedure TCreateUserPointsSetDlg.ButtonArthClick(Sender: TObject);
begin
    InsertFunc('Arth');
end;

procedure TCreateUserPointsSetDlg.ButtonBracketsClick(Sender: TObject);
begin
    InsertFunc('');
end;

procedure TCreateUserPointsSetDlg.ButtonArccosClick(Sender: TObject);
begin
    InsertFunc('Arccos');
end;

procedure TCreateUserPointsSetDlg.ButtonAbsClick(Sender: TObject);
begin
    InsertFunc('Abs');
end;

procedure TCreateUserPointsSetDlg.Button7Click(Sender: TObject);
begin
    InsertSymbol('7');
end;

procedure TCreateUserPointsSetDlg.Button4Click(Sender: TObject);
begin
    InsertSymbol('4');
end;

procedure TCreateUserPointsSetDlg.Button1Click(Sender: TObject);
begin
    InsertSymbol('1');
end;

procedure TCreateUserPointsSetDlg.Button0Click(Sender: TObject);
begin
    InsertSymbol('0');
end;

procedure TCreateUserPointsSetDlg.Button2Click(Sender: TObject);
begin
    InsertSymbol('2');
end;

procedure TCreateUserPointsSetDlg.Button3Click(Sender: TObject);
begin
    InsertSymbol('3');
end;

procedure TCreateUserPointsSetDlg.Button5Click(Sender: TObject);
begin
    InsertSymbol('5');
end;

procedure TCreateUserPointsSetDlg.Button6Click(Sender: TObject);
begin
    InsertSymbol('6');
end;

procedure TCreateUserPointsSetDlg.Button8Click(Sender: TObject);
begin
    InsertSymbol('8');
end;

procedure TCreateUserPointsSetDlg.Button9Click(Sender: TObject);
begin
    InsertSymbol('9');
end;

procedure TCreateUserPointsSetDlg.ButtonArcctgClick(Sender: TObject);
begin
    InsertFunc('Arcctg');
end;

procedure TCreateUserPointsSetDlg.ButtonArchClick(Sender: TObject);
begin
    InsertFunc('Arch');
end;

procedure TCreateUserPointsSetDlg.ButtonCtgClick(Sender: TObject);
begin
    InsertFunc('Ctg');
end;

procedure TCreateUserPointsSetDlg.ButtonCthClick(Sender: TObject);
begin
    InsertFunc('Cth');
end;

procedure TCreateUserPointsSetDlg.ButtonDecimalClick(Sender: TObject);
begin
    InsertSymbol('.');
end;

procedure TCreateUserPointsSetDlg.ButtonDivClick(Sender: TObject);
begin
    InsertSymbol('/');
end;

procedure TCreateUserPointsSetDlg.ButtonExpClick(Sender: TObject);
begin
    InsertFunc('Exp');
end;

procedure TCreateUserPointsSetDlg.ButtonLnClick(Sender: TObject);
begin
    InsertFunc('Ln');
end;

procedure TCreateUserPointsSetDlg.ButtonLogClick(Sender: TObject);
begin
    InsertFunc('Log');
end;

procedure TCreateUserPointsSetDlg.ButtonMinusClick(Sender: TObject);
begin
    InsertSymbol('-');
end;

procedure TCreateUserPointsSetDlg.ButtonMulClick(Sender: TObject);
begin
    InsertSymbol('*');
end;

procedure TCreateUserPointsSetDlg.ButtonPlusClick(Sender: TObject);
begin
    InsertSymbol('+');
end;

procedure TCreateUserPointsSetDlg.ButtonPowerClick(Sender: TObject);
begin
    InsertSymbol('^');
end;

procedure TCreateUserPointsSetDlg.ButtonSchClick(Sender: TObject);
begin
    InsertFunc('Sch');
end;

procedure TCreateUserPointsSetDlg.ButtonShClick(Sender: TObject);
begin
    InsertFunc('Sh');
end;

procedure TCreateUserPointsSetDlg.EditExpressionKeyPress(Sender: TObject;
  var Key: char);
begin
    //  desyatichnyy razdelitel'
    if Key = ',' then Key := '.';
end;

initialization
  {$I create_user_points_set_dlg.lrs}
end.



