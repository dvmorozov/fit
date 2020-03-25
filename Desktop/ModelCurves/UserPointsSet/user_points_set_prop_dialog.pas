{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TUserPointsSetPropDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit user_points_set_prop_dialog;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    app_settings, CheckLst, ExtCtrls, Forms, LResources, special_curve_parameter,
    StdCtrls, SysUtils
{$IFNDEF _WINDOWS}
    , Dialogs
{$ENDIF}
    ;

type
    { TUserPointsSetPropDlg }

    TUserPointsSetPropDlg = class(TForm)
        Bevel1:    TBevel;
        BtnApply:  TButton;
        BtnDone:   TButton;
        BtnBacktrack: TButton;
        CheckListFixed: TCheckListBox;
        ComboArgument: TComboBox;
        ComboPosition: TComboBox;
        EditExpression: TEdit;
        EditInitValue: TEdit;
        Label1:    TLabel;
        Label2:    TLabel;
        Label3:    TLabel;
        Label4:    TLabel;
        BtnCancel: TButton;
        Label5:    TLabel;
        procedure BtnApplyClick(Sender: TObject);
        procedure CheckListFixedClick(Sender: TObject);
        procedure CheckListFixedClickCheck(Sender: TObject);
        procedure ComboArgumentChange(Sender: TObject);
        procedure ComboPositionChange(Sender: TObject);
        procedure FormActivate(Sender: TObject);

    private
        FClickCheck: boolean;

        procedure FillComboArgument;
        procedure FillComboPosition;
        procedure FillCheckListFixed;
        procedure UpdateCheck;

    public
        FCurveType: Curve_type;
    end;

var
    UserPointsSetPropDlg: TUserPointsSetPropDlg;

implementation

uses input_max_rfactor_dialog;

{ TUserPointsSetPropDlg }

procedure TUserPointsSetPropDlg.ComboPositionChange(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
    i: longint;
begin
    for i := 0 to FCurveType.Parameters.Count - 1 do
    begin
        Parameter := FCurveType.Parameters[i];
        if (Parameter.Type_ = InvariablePosition) or
            (Parameter.Type_ = VariablePosition) then
            Parameter.Type_ := Variable;
    end;

    Parameter := TSpecialCurveParameter(
        ComboPosition.Items.Objects[ComboPosition.ItemIndex]);
    Parameter.Type_ := InvariablePosition;

    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.ComboArgumentChange(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
    i: longint;
begin
    for i := 0 to FCurveType.Params.Count - 1 do
    begin
        Parameter := FCurveType.Parameters[i];
        if Parameter.Type_ = Argument then
            Parameter.Type_ := Variable;
    end;

    Parameter := TSpecialCurveParameter(
        ComboArgument.Items.Objects[ComboArgument.ItemIndex]);
    Parameter.Type_ := Argument;

    FillComboPosition;
    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.UpdateCheck;
var
    Parameter: TSpecialCurveParameter;
begin
    Parameter := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    if CheckListFixed.Checked[CheckListFixed.ItemIndex] then
    begin
        if Parameter.Type_ = VariablePosition then
            Parameter.Type_ := InvariablePosition
        else
            Parameter.Type_ := Shared;
    end
    else
    if Parameter.Type_ = InvariablePosition then
        Parameter.Type_ := VariablePosition
    else
        Parameter.Type_ := Variable;
end;

procedure TUserPointsSetPropDlg.CheckListFixedClick(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
begin
    Parameter := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    EditInitValue.Text := FloatToStr(Parameter.Value);

    if FClickCheck then
    begin
        UpdateCheck;
        FClickCheck := False;
    end;
end;

procedure TUserPointsSetPropDlg.CheckListFixedClickCheck(Sender: TObject);
begin
    if CheckListFixed.ItemIndex <> -1 then
        UpdateCheck
    else
        FClickCheck := True;
end;

procedure TUserPointsSetPropDlg.BtnApplyClick(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
begin
    if CheckListFixed.ItemIndex <> -1 then
    begin
        Parameter := TSpecialCurveParameter(
            CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
        try
            Parameter.Value := StringToValue(EditInitValue.Text);
        except
{$IFDEF _WINDOWS}
            ShowBalloon(EditInitValue.Handle,
                ImproperRealValueInput,
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(string(ImproperRealValueInput), mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := EditInitValue;
        end;
    end;
end;

procedure TUserPointsSetPropDlg.FormActivate(Sender: TObject);
begin
    Assert(Assigned(FCurveType));
    EditExpression.Text := FCurveType.Expression;
    FillComboArgument;
    FillComboPosition;
    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.FillComboArgument;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
    //  !!! nel'zya isp. indeks potomu, chto real'nyy indeks dannogo
    //  elementa menyaetsya v protsesse dobavleniya drugih elementov !!!
    ArgName: string;
begin
    ComboArgument.Items.Clear;
    for i := 0 to FCurveType.Params.Count - 1 do
    begin
        Parameter := FCurveType.Parameters[i];
        //  !!! svyazku po indeksu delat' nel'zya,
        //  poskol'ku spisok sortirovan !!!
        ComboArgument.Items.AddObject(Parameter.Name, Parameter);
        if Parameter.Type_ = Argument then
            ArgName := Parameter.Name;
    end;
    ComboArgument.ItemIndex := ComboArgument.Items.IndexOf(ArgName);
end;

procedure TUserPointsSetPropDlg.FillComboPosition;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
    PosName: string;
begin
    ComboPosition.Items.Clear;
    for i := 0 to FCurveType.Params.Count - 1 do
    begin
        Parameter := FCurveType.Parameters[i];
        if (Parameter.Type_ = Shared) or (Parameter.Type_ = Variable) or
            (Parameter.Type_ = InvariablePosition) or
            (Parameter.Type_ = VariablePosition) then
        begin
            ComboPosition.Items.AddObject(Parameter.Name, Parameter);

            if (Parameter.Type_ = InvariablePosition) or
                (Parameter.Type_ = VariablePosition) then
                PosName := Parameter.Name;
        end;
    end;
    ComboPosition.ItemIndex := ComboPosition.Items.IndexOf(PosName);
end;

//  vyvodit vse parametry, krome argumenta
procedure TUserPointsSetPropDlg.FillCheckListFixed;
var
    i, Index:  longint;
    Parameter: TSpecialCurveParameter;
begin
    CheckListFixed.Items.Clear;
    for i := 0 to FCurveType.Params.Count - 1 do
    begin
        Parameter := FCurveType.Parameters[i];
        if (Parameter.Type_ = Shared) or (Parameter.Type_ = Variable) or
            (Parameter.Type_ = InvariablePosition) or
            (Parameter.Type_ = VariablePosition) then
        begin
            Index := CheckListFixed.Items.Add(Parameter.Name);
            if (Parameter.Type_ = Shared) or (Parameter.Type_ = InvariablePosition) then
                CheckListFixed.Checked[Index]   := True;
            CheckListFixed.Items.Objects[Index] := Parameter;
        end;
    end;
end;

initialization
  {$I user_points_set_prop_dialog.lrs}
end.
