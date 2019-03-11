{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TSpecialCurvePropDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit SpecialCurvePropDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, Settings, PointsSet, CurvePointsSet
{$IFDEF WINDOWS}
  , Windows
{$ENDIF}
  ;

type

  { TSpecialCurvePropDialog }

  TSpecialCurvePropDialog = class(TForm)
    Bevel1: TBevel;
    BtnApply: TButton;
    BtnDone: TButton;
    BtnBacktrack: TButton;
    CheckListFixed: TCheckListBox;
    ComboArgument: TComboBox;
    ComboPosition: TComboBox;
    EditExpression: TEdit;
    EditInitValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BtnCancel: TButton;
    Label5: TLabel;
    procedure BtnApplyClick(Sender: TObject);
    procedure CheckListFixedClick(Sender: TObject);
    procedure CheckListFixedClickCheck(Sender: TObject);
    procedure ComboArgumentChange(Sender: TObject);
    procedure ComboPositionChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    
  private
    ClickCheck: Boolean;

    procedure FillComboArgument;
    procedure FillComboPosition;
    procedure FillCheckListFixed;
    procedure UpdateCheck;
    
  public
    ct: Curve_type;     //  vneshnyaya ssylka
  end; 

var
  SpecialCurvePropDlg: TSpecialCurvePropDialog;

implementation

uses Unit4;

{ TSpecialCurvePropDialog }

procedure TSpecialCurvePropDialog.ComboPositionChange(Sender: TObject);
var P: TSpecialCurveParameter;
    i: LongInt;
begin
    for i := 0 to ct.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(ct.Params.Items[i]);
        if (P.Type_ = InvariablePosition) or
           (P.Type_ = VariablePosition) then P.Type_ := Variable;
    end;

    P := TSpecialCurveParameter(
        ComboPosition.Items.Objects[ComboPosition.ItemIndex]);
    P.Type_ := InvariablePosition;

    FillCheckListFixed;
end;

procedure TSpecialCurvePropDialog.ComboArgumentChange(Sender: TObject);
var P: TSpecialCurveParameter;
    i: LongInt;
begin
    for i := 0 to ct.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(ct.Params.Items[i]);
        if P.Type_ = Argument then P.Type_ := Variable;
    end;
    
    P := TSpecialCurveParameter(
        ComboArgument.Items.Objects[ComboArgument.ItemIndex]);
    P.Type_ := Argument;
    
    FillComboPosition;
    FillCheckListFixed;
end;

procedure TSpecialCurvePropDialog.UpdateCheck;
var P: TSpecialCurveParameter;
begin
    P := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    if CheckListFixed.Checked[CheckListFixed.ItemIndex] then
    begin
        if P.Type_ = VariablePosition then P.Type_ := InvariablePosition
        else P.Type_ := Shared;
    end
    else
    begin
        if P.Type_ = InvariablePosition then P.Type_ := VariablePosition
        else P.Type_ := Variable;
    end;
end;

procedure TSpecialCurvePropDialog.CheckListFixedClick(Sender: TObject);
var P: TSpecialCurveParameter;
begin
    P := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    EditInitValue.Text := FloatToStr(P.Value);

    if ClickCheck then begin UpdateCheck; ClickCheck := False; end;
end;

procedure TSpecialCurvePropDialog.CheckListFixedClickCheck(Sender: TObject);
var P: TSpecialCurveParameter;
begin
    if CheckListFixed.ItemIndex <> -1 then UpdateCheck
    else ClickCheck := True;
end;

procedure TSpecialCurvePropDialog.BtnApplyClick(Sender: TObject);
var P: TSpecialCurveParameter;
begin
    if CheckListFixed.ItemIndex <> -1 then
    begin
        P := TSpecialCurveParameter(
            CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
        try
            P.Value := StringToValue(EditInitValue.Text);
        except
{$ifdef windows}
            ShowBalloon(EditInitValue.Handle,
                ImproperRealValueInput,
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$else}
            MessageDlg(ImproperRealValueInput, mtError, [mbOk], 0);
{$endif}
            ActiveControl := EditInitValue;
        end;
    end;
end;

procedure TSpecialCurvePropDialog.FormActivate(Sender: TObject);
begin
    Assert(Assigned(ct));
    EditExpression.Text := ct.Expression;
    FillComboArgument;
    FillComboPosition;
    FillCheckListFixed;
end;

procedure TSpecialCurvePropDialog.FillComboArgument;
var i: LongInt;
    P: TSpecialCurveParameter;
    //  !!! nel'zya isp. indeks potomu, chto real'nyy indeks dannogo
    //  elementa menyaetsya v protsesse dobavleniya drugih elementov !!!
    ArgName: string;
begin
    ComboArgument.Items.Clear;
    for i := 0 to ct.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(ct.Params.Items[i]);
        //  !!! svyazku po indeksu delat' nel'zya,
        //  poskol'ku spisok sortirovan !!!
        ComboArgument.Items.AddObject(P.Name, P);
        if P.Type_ = Argument then ArgName := P.Name;
    end;
    ComboArgument.ItemIndex := ComboArgument.Items.IndexOf(ArgName);
end;

procedure TSpecialCurvePropDialog.FillComboPosition;
var i: LongInt;
    P: TSpecialCurveParameter;
    PosName: string;
begin
    ComboPosition.Items.Clear;
    for i := 0 to ct.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(ct.Params.Items[i]);
        if (P.Type_ = Shared) or (P.Type_ = Variable) or
           (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) then
        begin
            ComboPosition.Items.AddObject(P.Name, P);
            
            if (P.Type_ = InvariablePosition) or
               (P.Type_ = VariablePosition) then PosName := P.Name;
        end;
    end;
    ComboPosition.ItemIndex := ComboPosition.Items.IndexOf(PosName);
end;

//  vyvodit vse parametry, krome argumenta
procedure TSpecialCurvePropDialog.FillCheckListFixed;
var i, Index: LongInt;
    P: TSpecialCurveParameter;
begin
    CheckListFixed.Items.Clear;
    for i := 0 to ct.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(ct.Params.Items[i]);
        if (P.Type_ = Shared) or (P.Type_ = Variable) or
           (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) then
        begin
            Index := CheckListFixed.Items.Add(P.Name);
            if (P.Type_ = Shared) or (P.Type_ = InvariablePosition) then
                CheckListFixed.Checked[Index] := True;
            CheckListFixed.Items.Objects[Index] := P;
        end;
    end;
end;

initialization
  {$I specialcurvepropdialog.lrs}
end.



