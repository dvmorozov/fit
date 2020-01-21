unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Windows;

type

  { TCalcExpressionDlg }

  TCalcExpressionDlg = class(TForm)
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
    CalculateBtn: TButton;
    ComboSymbols: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OKBtn: TButton;
    ExpressionEdit: TEdit;
    ResultEdit: TEdit;
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
    procedure CalculateBtnClick(Sender: TObject);
    procedure ComboSymbolsDropDown(Sender: TObject);
    procedure ExpressionEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    procedure InsertFunc(FuncName: string);
    procedure InsertSymbol(Symbol: string);
  public
    { public declarations }
  end; 

var
  CalcExpressionDlg: TCalcExpressionDlg;

implementation

function ParseExpression(Expr: LPCSTR; ExprId: LPINT
    ): LongInt; cdecl; external 'MathExpr' name 'ParseExpression';
function ParseAndCalcExpression(Expr: LPCSTR; ParamList: LPCSTR;
    Result: PDouble): LongInt; cdecl; external 'MathExpr' name 'ParseAndCalcExpression';
function CalcExpression(ExprId: LongInt; ParamList: LPCSTR;
    Result: PDouble): LongInt; cdecl; external 'MathExpr' name 'CalcExpression';
function CalcExpression2(
    ExprId: LongInt; ParamList: array of Double; ParamCount: LongInt;
    Result: PDouble): LongInt; cdecl; external 'MathExpr' name 'CalcExpression2';
function GetSymbols: LPCSTR; cdecl; external 'MathExpr' name 'GetSymbols';
procedure FreeSymbols(Symbols: LPCSTR); cdecl; external 'MathExpr' name 'FreeSymbols';

{ TCalcExpressionDlg }

procedure TCalcExpressionDlg.CalculateBtnClick(Sender: TObject);
var MyResult: Double;
    Result: LongInt;
begin
    Result := ParseAndCalcExpression(PChar(ExpressionEdit.Text), '', @MyResult);
    if Result = 2 then
        ResultEdit.Text := 'Value assigned successfully'
    else
    if Result = 1 then
        ResultEdit.Text := FloatToStr(MyResult)
    else
    if Result = -1 then
        ResultEdit.Text := 'Undefined parameters'
    else
    if Result = 0 then
        ResultEdit.Text := 'Inadmissible expression';
end;

procedure TCalcExpressionDlg.InsertFunc(FuncName: string);
var Str: string;
    Temp: LongInt;
begin
    with ExpressionEdit do
    begin
        SelLength := 0;
        Str := Text;
        Temp := SelStart;
        //  равенство SelStart нулю означает, что строка пустая
        Insert(FuncName + '()', Str, Temp + 1);
        Text := Str;
        SelStart := Temp + Length(FuncName) + 1;
    end;
end;

procedure TCalcExpressionDlg.InsertSymbol(Symbol: string);
var Str: string;
    Temp: LongInt;
begin
    with ExpressionEdit do
    begin
        SelLength := 0;
        Str := Text;
        Temp := SelStart;
        Insert(Symbol, Str, Temp + 1);
        Text := Str;
        SelStart := Temp + Length(Symbol);
    end;
end;

procedure TCalcExpressionDlg.ButtonSinClick(Sender: TObject);
begin
    InsertFunc('Sin');
end;

procedure TCalcExpressionDlg.ButtonSqrtClick(Sender: TObject);
begin
    InsertFunc('Sqrt');
end;

procedure TCalcExpressionDlg.ButtonTgClick(Sender: TObject);
begin
    InsertFunc('Tg');
end;

procedure TCalcExpressionDlg.ButtonThClick(Sender: TObject);
begin
    InsertFunc('Th');
end;

procedure TCalcExpressionDlg.ButtonCosClick(Sender: TObject);
begin
    InsertFunc('Cos');
end;

procedure TCalcExpressionDlg.ButtonCschClick(Sender: TObject);
begin
    InsertFunc('Csch');
end;

procedure TCalcExpressionDlg.ButtonChClick(Sender: TObject);
begin
    InsertFunc('Ch');
end;

procedure TCalcExpressionDlg.ButtonArcsinClick(Sender: TObject);
begin
    InsertFunc('Arcsin');
end;

procedure TCalcExpressionDlg.ButtonArctgClick(Sender: TObject);
begin
    InsertFunc('Arctg');
end;

procedure TCalcExpressionDlg.ButtonArcthClick(Sender: TObject);
begin
    InsertFunc('Arcth');
end;

procedure TCalcExpressionDlg.ButtonArshClick(Sender: TObject);
begin
    InsertFunc('Arsh');
end;

procedure TCalcExpressionDlg.ButtonArthClick(Sender: TObject);
begin
    InsertFunc('Arth');
end;

procedure TCalcExpressionDlg.ButtonBracketsClick(Sender: TObject);
begin
    InsertFunc('');
end;

procedure TCalcExpressionDlg.ButtonArccosClick(Sender: TObject);
begin
    InsertFunc('Arccos');
end;

procedure TCalcExpressionDlg.ButtonAbsClick(Sender: TObject);
begin
    InsertFunc('Abs');
end;

procedure TCalcExpressionDlg.Button7Click(Sender: TObject);
begin
    InsertSymbol('7');
end;

procedure TCalcExpressionDlg.Button4Click(Sender: TObject);
begin
    InsertSymbol('4');
end;

procedure TCalcExpressionDlg.Button1Click(Sender: TObject);
begin
    InsertSymbol('1');
end;

procedure TCalcExpressionDlg.Button0Click(Sender: TObject);
begin
    InsertSymbol('0');
end;

procedure TCalcExpressionDlg.Button2Click(Sender: TObject);
begin
    InsertSymbol('2');
end;

procedure TCalcExpressionDlg.Button3Click(Sender: TObject);
begin
    InsertSymbol('3');
end;

procedure TCalcExpressionDlg.Button5Click(Sender: TObject);
begin
    InsertSymbol('5');
end;

procedure TCalcExpressionDlg.Button6Click(Sender: TObject);
begin
    InsertSymbol('6');
end;

procedure TCalcExpressionDlg.Button8Click(Sender: TObject);
begin
    InsertSymbol('8');
end;

procedure TCalcExpressionDlg.Button9Click(Sender: TObject);
begin
    InsertSymbol('9');
end;

procedure TCalcExpressionDlg.ButtonArcctgClick(Sender: TObject);
begin
    InsertFunc('Arcctg');
end;

procedure TCalcExpressionDlg.ButtonArchClick(Sender: TObject);
begin
    InsertFunc('Arch');
end;

procedure TCalcExpressionDlg.ButtonCtgClick(Sender: TObject);
begin
    InsertFunc('Ctg');
end;

procedure TCalcExpressionDlg.ButtonCthClick(Sender: TObject);
begin
    InsertFunc('Cth');
end;

procedure TCalcExpressionDlg.ButtonDecimalClick(Sender: TObject);
begin
    InsertSymbol('.');
end;

procedure TCalcExpressionDlg.ButtonDivClick(Sender: TObject);
begin
    InsertSymbol('/');
end;

procedure TCalcExpressionDlg.ButtonExpClick(Sender: TObject);
begin
    InsertFunc('Exp');
end;

procedure TCalcExpressionDlg.ButtonLnClick(Sender: TObject);
begin
    InsertFunc('Ln');
end;

procedure TCalcExpressionDlg.ButtonLogClick(Sender: TObject);
begin
    InsertFunc('Log');
end;

procedure TCalcExpressionDlg.ButtonMinusClick(Sender: TObject);
begin
    InsertSymbol('-');
end;

procedure TCalcExpressionDlg.ButtonMulClick(Sender: TObject);
begin
    InsertSymbol('*');
end;

procedure TCalcExpressionDlg.ButtonPlusClick(Sender: TObject);
begin
    InsertSymbol('+');
end;

procedure TCalcExpressionDlg.ButtonPowerClick(Sender: TObject);
begin
    InsertSymbol('^');
end;

procedure TCalcExpressionDlg.ButtonSchClick(Sender: TObject);
begin
    InsertFunc('Sch');
end;

procedure TCalcExpressionDlg.ButtonShClick(Sender: TObject);
begin
    InsertFunc('Sh');
end;

procedure TCalcExpressionDlg.ComboSymbolsDropDown(Sender: TObject);
var Symbols, Saved: LPCSTR;
begin
    ComboSymbols.Items.Clear;
    Symbols := GetSymbols;
    Saved := Symbols;
    while Assigned(Symbols) and (Length(Symbols) <> 0) do
    begin
        ComboSymbols.Items.Add(Symbols);
        Symbols := Symbols + Length(Symbols) + 1;
    end;
    if Assigned(Symbols) then FreeSymbols(Saved);
end;

procedure TCalcExpressionDlg.ExpressionEditKeyPress(Sender: TObject;
  var Key: char);
begin
    //  десятичный разделитель
    if Key = ',' then Key := '.';
end;

initialization
  {$I Unit2.lrs}
end.

