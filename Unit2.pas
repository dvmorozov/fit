unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls(*, Windows*);

type

  { TCreateSpecialCurveDlg }

  TCreateSpecialCurveDlg = class(TForm)
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
  CreateSpecialCurveDlg: TCreateSpecialCurveDlg;

implementation

{ TCreateSpecialCurveDlg }

procedure TCreateSpecialCurveDlg.InsertFunc(FuncName: string);
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

procedure TCreateSpecialCurveDlg.InsertSymbol(Symbol: string);
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

procedure TCreateSpecialCurveDlg.ButtonSinClick(Sender: TObject);
begin
    InsertFunc('Sin');
end;

procedure TCreateSpecialCurveDlg.ButtonSqrtClick(Sender: TObject);
begin
    InsertFunc('Sqrt');
end;

procedure TCreateSpecialCurveDlg.ButtonTgClick(Sender: TObject);
begin
    InsertFunc('Tg');
end;

procedure TCreateSpecialCurveDlg.ButtonThClick(Sender: TObject);
begin
    InsertFunc('Th');
end;

procedure TCreateSpecialCurveDlg.ButtonCosClick(Sender: TObject);
begin
    InsertFunc('Cos');
end;

procedure TCreateSpecialCurveDlg.ButtonCschClick(Sender: TObject);
begin
    InsertFunc('Csch');
end;

procedure TCreateSpecialCurveDlg.ButtonChClick(Sender: TObject);
begin
    InsertFunc('Ch');
end;

procedure TCreateSpecialCurveDlg.ButtonArcsinClick(Sender: TObject);
begin
    InsertFunc('Arcsin');
end;

procedure TCreateSpecialCurveDlg.ButtonArctgClick(Sender: TObject);
begin
    InsertFunc('Arctg');
end;

procedure TCreateSpecialCurveDlg.ButtonArcthClick(Sender: TObject);
begin
    InsertFunc('Arcth');
end;

procedure TCreateSpecialCurveDlg.ButtonArshClick(Sender: TObject);
begin
    InsertFunc('Arsh');
end;

procedure TCreateSpecialCurveDlg.ButtonArthClick(Sender: TObject);
begin
    InsertFunc('Arth');
end;

procedure TCreateSpecialCurveDlg.ButtonBracketsClick(Sender: TObject);
begin
    InsertFunc('');
end;

procedure TCreateSpecialCurveDlg.ButtonArccosClick(Sender: TObject);
begin
    InsertFunc('Arccos');
end;

procedure TCreateSpecialCurveDlg.ButtonAbsClick(Sender: TObject);
begin
    InsertFunc('Abs');
end;

procedure TCreateSpecialCurveDlg.Button7Click(Sender: TObject);
begin
    InsertSymbol('7');
end;

procedure TCreateSpecialCurveDlg.Button4Click(Sender: TObject);
begin
    InsertSymbol('4');
end;

procedure TCreateSpecialCurveDlg.Button1Click(Sender: TObject);
begin
    InsertSymbol('1');
end;

procedure TCreateSpecialCurveDlg.Button0Click(Sender: TObject);
begin
    InsertSymbol('0');
end;

procedure TCreateSpecialCurveDlg.Button2Click(Sender: TObject);
begin
    InsertSymbol('2');
end;

procedure TCreateSpecialCurveDlg.Button3Click(Sender: TObject);
begin
    InsertSymbol('3');
end;

procedure TCreateSpecialCurveDlg.Button5Click(Sender: TObject);
begin
    InsertSymbol('5');
end;

procedure TCreateSpecialCurveDlg.Button6Click(Sender: TObject);
begin
    InsertSymbol('6');
end;

procedure TCreateSpecialCurveDlg.Button8Click(Sender: TObject);
begin
    InsertSymbol('8');
end;

procedure TCreateSpecialCurveDlg.Button9Click(Sender: TObject);
begin
    InsertSymbol('9');
end;

procedure TCreateSpecialCurveDlg.ButtonArcctgClick(Sender: TObject);
begin
    InsertFunc('Arcctg');
end;

procedure TCreateSpecialCurveDlg.ButtonArchClick(Sender: TObject);
begin
    InsertFunc('Arch');
end;

procedure TCreateSpecialCurveDlg.ButtonCtgClick(Sender: TObject);
begin
    InsertFunc('Ctg');
end;

procedure TCreateSpecialCurveDlg.ButtonCthClick(Sender: TObject);
begin
    InsertFunc('Cth');
end;

procedure TCreateSpecialCurveDlg.ButtonDecimalClick(Sender: TObject);
begin
    InsertSymbol('.');
end;

procedure TCreateSpecialCurveDlg.ButtonDivClick(Sender: TObject);
begin
    InsertSymbol('/');
end;

procedure TCreateSpecialCurveDlg.ButtonExpClick(Sender: TObject);
begin
    InsertFunc('Exp');
end;

procedure TCreateSpecialCurveDlg.ButtonLnClick(Sender: TObject);
begin
    InsertFunc('Ln');
end;

procedure TCreateSpecialCurveDlg.ButtonLogClick(Sender: TObject);
begin
    InsertFunc('Log');
end;

procedure TCreateSpecialCurveDlg.ButtonMinusClick(Sender: TObject);
begin
    InsertSymbol('-');
end;

procedure TCreateSpecialCurveDlg.ButtonMulClick(Sender: TObject);
begin
    InsertSymbol('*');
end;

procedure TCreateSpecialCurveDlg.ButtonPlusClick(Sender: TObject);
begin
    InsertSymbol('+');
end;

procedure TCreateSpecialCurveDlg.ButtonPowerClick(Sender: TObject);
begin
    InsertSymbol('^');
end;

procedure TCreateSpecialCurveDlg.ButtonSchClick(Sender: TObject);
begin
    InsertFunc('Sch');
end;

procedure TCreateSpecialCurveDlg.ButtonShClick(Sender: TObject);
begin
    InsertFunc('Sh');
end;

procedure TCreateSpecialCurveDlg.EditExpressionKeyPress(Sender: TObject;
  var Key: char);
begin
    //  desyatichnyy razdelitel'
    if Key = ',' then Key := '.';
end;

initialization
  {$I Unit2.lrs}
end.



