// SPDX-License-Identifier: GPL-3.0-or-later
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
    Classes, ExtCtrls, Forms, LCLIntf, LResources, StdCtrls,
    //  What a keypad press does to the text and the caret. This unit reads the
    //  control and writes the answer back; it decides nothing itself.
    formula_editing;

type

    { TCreateUserPointsSetDlg }

    TCreateUserPointsSetDlg = class(TForm)
        BevelButtons:      TBevel;
        ButtonSin:   TButton;
        ButtonTh:    TButton;
        ButtonDigit4:     TButton;
        ButtonDigit5:     TButton;
        ButtonDigit6:     TButton;
        ButtonMul:   TButton;
        ButtonArcsin: TButton;
        ButtonArccos: TButton;
        ButtonArctg: TButton;
        ButtonDigit1:     TButton;
        ButtonDigit2:     TButton;
        ButtonCos:   TButton;
        ButtonDigit3:     TButton;
        ButtonMinus: TButton;
        ButtonArsh:  TButton;
        ButtonArch:  TButton;
        ButtonArth:  TButton;
        ButtonDigit0:     TButton;
        ButtonBrackets: TButton;
        ButtonDecimal: TButton;
        ButtonPlus:  TButton;
        ButtonSch:   TButton;
        ButtonTg:    TButton;
        ButtonCsch:  TButton;
        ButtonLn:    TButton;
        ButtonPower: TButton;
        ButtonExp:   TButton;
        ButtonAbs:   TButton;
        ButtonSqrt:  TButton;
        ButtonCtg:   TButton;
        ButtonCth:   TButton;
        ButtonArcctg: TButton;
        ButtonArcth: TButton;
        ButtonDigit7:     TButton;
        ButtonLog:   TButton;
        ButtonDigit9:     TButton;
        ButtonDigit8:     TButton;
        ButtonDiv:   TButton;
        ButtonSh:    TButton;
        ButtonCh:    TButton;
        ButtonContinue: TButton;
        EditCurveName: TEdit;
        LabelExpression:      TLabel;
        LabelCurveName:      TLabel;
        ButtonCancel:   TButton;
        EditExpression: TEdit;
        procedure ButtonDigit0Click(Sender: TObject);
        procedure ButtonDigit1Click(Sender: TObject);
        procedure ButtonDigit2Click(Sender: TObject);
        procedure ButtonDigit3Click(Sender: TObject);
        procedure ButtonDigit4Click(Sender: TObject);
        procedure ButtonDigit5Click(Sender: TObject);
        procedure ButtonDigit6Click(Sender: TObject);
        procedure ButtonDigit7Click(Sender: TObject);
        procedure ButtonDigit8Click(Sender: TObject);
        procedure ButtonDigit9Click(Sender: TObject);
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
        { The edit box as three values, and the answer written back onto it. }
        function CurrentEdit: TEditState;
        procedure ApplyEdit(const AResult: TEditState);
        procedure InsertFunc(FuncName: string);
        procedure InsertSymbol(Symbol: string);
    public
        { public declarations }
    end;

var
    CreateUserPointsSetDlg: TCreateUserPointsSetDlg;

implementation

{ TCreateUserPointsSetDlg }

{ WHAT THE INSERTION DOES is in formula_editing, where a caret landing one
  character out is a test failure rather than a formula the user silently built
  wrong. These two read the control, ask, and write the answer back. }
procedure TCreateUserPointsSetDlg.ApplyEdit(const AResult: TEditState);
begin
    //  The WHOLE text, rather than assigning SelText and moving the caret after
    //  it: the computed result is the single source of truth for both, and
    //  splitting it would put half the rule back in here. The control has no
    //  OnChange handler, so nothing else runs; what is lost is the widget's own
    //  undo of a keypad press, which was never a designed behaviour - the old
    //  code moved the caret by hand straight afterwards anyway.
    EditExpression.Text := AResult.Text;
    EditExpression.SelStart := AResult.SelStart;
    EditExpression.SelLength := 0;
end;

function TCreateUserPointsSetDlg.CurrentEdit: TEditState;
begin
    Result := EditState(EditExpression.Text, EditExpression.SelStart,
        EditExpression.SelLength);
end;

procedure TCreateUserPointsSetDlg.InsertFunc(FuncName: string);
begin
    ApplyEdit(formula_editing.InsertFunction(CurrentEdit, FuncName));
end;

procedure TCreateUserPointsSetDlg.InsertSymbol(Symbol: string);
begin
    ApplyEdit(formula_editing.InsertSymbol(CurrentEdit, Symbol));
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

procedure TCreateUserPointsSetDlg.ButtonDigit7Click(Sender: TObject);
begin
    InsertSymbol('7');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit4Click(Sender: TObject);
begin
    InsertSymbol('4');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit1Click(Sender: TObject);
begin
    InsertSymbol('1');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit0Click(Sender: TObject);
begin
    InsertSymbol('0');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit2Click(Sender: TObject);
begin
    InsertSymbol('2');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit3Click(Sender: TObject);
begin
    InsertSymbol('3');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit5Click(Sender: TObject);
begin
    InsertSymbol('5');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit6Click(Sender: TObject);
begin
    InsertSymbol('6');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit8Click(Sender: TObject);
begin
    InsertSymbol('8');
end;

procedure TCreateUserPointsSetDlg.ButtonDigit9Click(Sender: TObject);
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
    Key := TypedCharacter(Key);
end;

initialization
  {$I create_user_points_set_dlg.lrs}
end.
