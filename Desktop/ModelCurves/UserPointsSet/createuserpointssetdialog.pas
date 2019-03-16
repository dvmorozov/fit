{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TCreateSpecialCurveDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CreateUserPointsSetDialog;

{$MODE Delphi}
//{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, StdCtrls, ExtCtrls
{$IFDEF WINDOWS}
  , Windows
{$ENDIF}
  ;

type

  { TCreateUserPointsSetDialog }

  TCreateUserPointsSetDialog = class(TForm)
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
  CreateUserPointsSetDlg: TCreateUserPointsSetDialog;

implementation

{ TCreateUserPointsSetDialog }

procedure TCreateUserPointsSetDialog.InsertFunc(FuncName: string);
var Str: string;
    Temp: LongInt;
begin
    with EditExpression do
    begin
        (* tak rabotaet...
        SelLength := 0;
        Str := Text;
        Temp := SelStart;
        //  ravenstvo SelStart nulyu oznachaet, chto stroka pustaya
        Insert(FuncName + '()', Str, Temp + 1);
        Text := Str;
        SelStart := Temp + Length(FuncName) + 1;
        *)
        //  ...no tak pravil'nee, potomu chto
        //  vydelennyy tekst zamenyaetsya na novyy
        Temp := SelStart;
        SelText := FuncName + '()';
        SelStart := Temp + Length(FuncName) + 1;
    end;
end;

procedure TCreateUserPointsSetDialog.InsertSymbol(Symbol: string);
var Str: string;
    Temp: LongInt;
begin
    with EditExpression do
    begin
        (*
        SelLength := 0;
        Str := Text;
        Temp := SelStart;
        Insert(Symbol, Str, Temp + 1);
        Text := Str;
        SelStart := Temp + Length(Symbol);
        *)
        Temp := SelStart;
        SelText := Symbol;
        SelStart := Temp + Length(Symbol);
    end;
end;

procedure TCreateUserPointsSetDialog.ButtonSinClick(Sender: TObject);
begin
    InsertFunc('Sin');
end;

procedure TCreateUserPointsSetDialog.ButtonSqrtClick(Sender: TObject);
begin
    InsertFunc('Sqrt');
end;

procedure TCreateUserPointsSetDialog.ButtonTgClick(Sender: TObject);
begin
    InsertFunc('Tg');
end;

procedure TCreateUserPointsSetDialog.ButtonThClick(Sender: TObject);
begin
    InsertFunc('Th');
end;

procedure TCreateUserPointsSetDialog.ButtonCosClick(Sender: TObject);
begin
    InsertFunc('Cos');
end;

procedure TCreateUserPointsSetDialog.ButtonCschClick(Sender: TObject);
begin
    InsertFunc('Csch');
end;

procedure TCreateUserPointsSetDialog.ButtonChClick(Sender: TObject);
begin
    InsertFunc('Ch');
end;

procedure TCreateUserPointsSetDialog.ButtonArcsinClick(Sender: TObject);
begin
    InsertFunc('Arcsin');
end;

procedure TCreateUserPointsSetDialog.ButtonArctgClick(Sender: TObject);
begin
    InsertFunc('Arctg');
end;

procedure TCreateUserPointsSetDialog.ButtonArcthClick(Sender: TObject);
begin
    InsertFunc('Arcth');
end;

procedure TCreateUserPointsSetDialog.ButtonArshClick(Sender: TObject);
begin
    InsertFunc('Arsh');
end;

procedure TCreateUserPointsSetDialog.ButtonArthClick(Sender: TObject);
begin
    InsertFunc('Arth');
end;

procedure TCreateUserPointsSetDialog.ButtonBracketsClick(Sender: TObject);
begin
    InsertFunc('');
end;

procedure TCreateUserPointsSetDialog.ButtonArccosClick(Sender: TObject);
begin
    InsertFunc('Arccos');
end;

procedure TCreateUserPointsSetDialog.ButtonAbsClick(Sender: TObject);
begin
    InsertFunc('Abs');
end;

procedure TCreateUserPointsSetDialog.Button7Click(Sender: TObject);
begin
    InsertSymbol('7');
end;

procedure TCreateUserPointsSetDialog.Button4Click(Sender: TObject);
begin
    InsertSymbol('4');
end;

procedure TCreateUserPointsSetDialog.Button1Click(Sender: TObject);
begin
    InsertSymbol('1');
end;

procedure TCreateUserPointsSetDialog.Button0Click(Sender: TObject);
begin
    InsertSymbol('0');
end;

procedure TCreateUserPointsSetDialog.Button2Click(Sender: TObject);
begin
    InsertSymbol('2');
end;

procedure TCreateUserPointsSetDialog.Button3Click(Sender: TObject);
begin
    InsertSymbol('3');
end;

procedure TCreateUserPointsSetDialog.Button5Click(Sender: TObject);
begin
    InsertSymbol('5');
end;

procedure TCreateUserPointsSetDialog.Button6Click(Sender: TObject);
begin
    InsertSymbol('6');
end;

procedure TCreateUserPointsSetDialog.Button8Click(Sender: TObject);
begin
    InsertSymbol('8');
end;

procedure TCreateUserPointsSetDialog.Button9Click(Sender: TObject);
begin
    InsertSymbol('9');
end;

procedure TCreateUserPointsSetDialog.ButtonArcctgClick(Sender: TObject);
begin
    InsertFunc('Arcctg');
end;

procedure TCreateUserPointsSetDialog.ButtonArchClick(Sender: TObject);
begin
    InsertFunc('Arch');
end;

procedure TCreateUserPointsSetDialog.ButtonCtgClick(Sender: TObject);
begin
    InsertFunc('Ctg');
end;

procedure TCreateUserPointsSetDialog.ButtonCthClick(Sender: TObject);
begin
    InsertFunc('Cth');
end;

procedure TCreateUserPointsSetDialog.ButtonDecimalClick(Sender: TObject);
begin
    InsertSymbol('.');
end;

procedure TCreateUserPointsSetDialog.ButtonDivClick(Sender: TObject);
begin
    InsertSymbol('/');
end;

procedure TCreateUserPointsSetDialog.ButtonExpClick(Sender: TObject);
begin
    InsertFunc('Exp');
end;

procedure TCreateUserPointsSetDialog.ButtonLnClick(Sender: TObject);
begin
    InsertFunc('Ln');
end;

procedure TCreateUserPointsSetDialog.ButtonLogClick(Sender: TObject);
begin
    InsertFunc('Log');
end;

procedure TCreateUserPointsSetDialog.ButtonMinusClick(Sender: TObject);
begin
    InsertSymbol('-');
end;

procedure TCreateUserPointsSetDialog.ButtonMulClick(Sender: TObject);
begin
    InsertSymbol('*');
end;

procedure TCreateUserPointsSetDialog.ButtonPlusClick(Sender: TObject);
begin
    InsertSymbol('+');
end;

procedure TCreateUserPointsSetDialog.ButtonPowerClick(Sender: TObject);
begin
    InsertSymbol('^');
end;

procedure TCreateUserPointsSetDialog.ButtonSchClick(Sender: TObject);
begin
    InsertFunc('Sch');
end;

procedure TCreateUserPointsSetDialog.ButtonShClick(Sender: TObject);
begin
    InsertFunc('Sh');
end;

procedure TCreateUserPointsSetDialog.EditExpressionKeyPress(Sender: TObject;
  var Key: char);
begin
    //  desyatichnyy razdelitel'
    if Key = ',' then Key := '.';
end;

initialization
  {$I createuserpointssetdialog.lrs}
end.



