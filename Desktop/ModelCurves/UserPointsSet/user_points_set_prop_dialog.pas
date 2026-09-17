// SPDX-License-Identifier: GPL-3.0-or-later
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
    StdCtrls, SysUtils,
    //  Which parameter holds which role, and what fixing one makes it. The rule
    //  is there; this dialog is the way the user states it.
    parameter_roles,
    //  The order the list box reports a tick and a selection in, which is not
    //  the order they happen in.
    deferred_tick
{$IFNDEF _WINDOWS}
    , Dialogs
{$ENDIF}
    ;

type
    { TUserPointsSetPropDlg }

    TUserPointsSetPropDlg = class(TForm)
        BevelButtons:    TBevel;
        ButtonApply:  TButton;
        ButtonDone:   TButton;
        ButtonBacktrack: TButton;
        CheckListFixed: TCheckListBox;
        ComboArgument: TComboBox;
        ComboPosition: TComboBox;
        ComboAmplitude: TComboBox;
        ComboWidth: TComboBox;
        LabelAmplitude: TLabel;
        LabelWidth: TLabel;
        EditExpression: TEdit;
        EditInitValue: TEdit;
        LabelFixedParameters:    TLabel;
        LabelArgument:    TLabel;
        LabelInitialValue:    TLabel;
        LabelExpression:    TLabel;
        ButtonCancel: TButton;
        LabelPosition:    TLabel;
        procedure BtnApplyClick(Sender: TObject);
        procedure CheckListFixedClick(Sender: TObject);
        procedure CheckListFixedClickCheck(Sender: TObject);
        procedure ComboArgumentChange(Sender: TObject);
        procedure ComboPositionChange(Sender: TObject);
        procedure ComboAmplitudeChange(Sender: TObject);
        procedure ComboWidthChange(Sender: TObject);
        procedure FormActivate(Sender: TObject);

    private
        { A tick that arrived before its row did. The RULE is in deferred_tick,
          where it can be tested; this holds the state and nothing else. }
        FTick: TDeferredTick;

        { Puts a list of choices into a combo and selects the marked one. }
        procedure ShowChoices(ACombo: TComboBox;
            const AChoices: TParameterChoices);
        procedure FillComboArgument;
        procedure FillComboPosition;
        procedure FillComboRole(ACombo: TComboBox; ARole: TParameterType);
        procedure FillCheckListFixed;
        procedure UpdateCheck;
        { The parameter a role combo has selected, or nil. }
        function SelectedParameter(ACombo: TComboBox): TSpecialCurveParameter;

    public
        FCurveType: Curve_type;
    end;

var
    UserPointsSetPropDlg: TUserPointsSetPropDlg;

implementation

uses set_maximum_rfactor_dialog, typed_number, checks;

{ TUserPointsSetPropDlg }

{ WHO HOLDS WHICH ROLE is decided in parameter_roles, where "at most one" is an
  invariant a test can assert. These four handlers read the combo and hand the
  answer over; each used to walk the parameter list clearing the role itself,
  which was one invariant written out four times. }
procedure TUserPointsSetPropDlg.ComboPositionChange(Sender: TObject);
begin
    AssignRole(FCurveType.Parameters, prPosition,
        SelectedParameter(ComboPosition));
    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.ComboArgumentChange(Sender: TObject);
begin
    AssignRole(FCurveType.Parameters, prArgument,
        SelectedParameter(ComboArgument));
    FillComboPosition;
    FillCheckListFixed;
end;

{ WHICH PARAMETERS A LIST OFFERS is decided in parameter_roles, where each
  filter is a rule a test can state. These two put the answer into a widget and
  do nothing else: an item per row, the parameter attached to it - never matched
  up by index, because the combo sorts and the two orders do not correspond -
  and the marked row selected. }
procedure TUserPointsSetPropDlg.ShowChoices(ACombo: TComboBox;
    const AChoices: TParameterChoices);
var
    i: longint;
begin
    ACombo.Items.Clear;
    for i := 0 to High(AChoices) do
        ACombo.Items.AddObject(AChoices[i].Name, AChoices[i].Parameter);
    //  By NAME, because the combo may be sorted; -1 when nothing is marked,
    //  which is the honest answer when no parameter holds the role.
    if MarkedIndex(AChoices) >= 0 then
        ACombo.ItemIndex :=
            ACombo.Items.IndexOf(AChoices[MarkedIndex(AChoices)].Name)
    else
        ACombo.ItemIndex := -1;
end;

procedure TUserPointsSetPropDlg.FillComboRole(ACombo: TComboBox;
    ARole: TParameterType);
begin
    if ARole = Amplitude then
        ShowChoices(ACombo, RoleChoices(FCurveType.Parameters, prAmplitude))
    else
        ShowChoices(ACombo, RoleChoices(FCurveType.Parameters, prWidth));
end;

procedure TUserPointsSetPropDlg.ComboAmplitudeChange(Sender: TObject);
begin
    AssignRole(FCurveType.Parameters, prAmplitude,
        SelectedParameter(ComboAmplitude));
    //  The other role's combo is refilled because the parameter that just took
    //  this one is no longer free to take that one.
    FillComboRole(ComboWidth, special_curve_parameter.Width);
    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.ComboWidthChange(Sender: TObject);
begin
    AssignRole(FCurveType.Parameters, prWidth,
        SelectedParameter(ComboWidth));
    FillComboRole(ComboAmplitude, Amplitude);
    FillCheckListFixed;
end;

{ The parameter a role combo has selected, or nil - which is what its "(none)"
  item carries, and what an empty selection means. }
function TUserPointsSetPropDlg.SelectedParameter(
    ACombo: TComboBox): TSpecialCurveParameter;
begin
    Result := nil;
    if ACombo.ItemIndex >= 0 then
        Result := TSpecialCurveParameter(
            ACombo.Items.Objects[ACombo.ItemIndex]);
end;

procedure TUserPointsSetPropDlg.UpdateCheck;
var
    Parameter: TSpecialCurveParameter;
begin
    Parameter := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    Parameter.Type_ := TypeAfterFixing(Parameter.Type_,
        CheckListFixed.Checked[CheckListFixed.ItemIndex]);
end;

procedure TUserPointsSetPropDlg.CheckListFixedClick(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
begin
    Parameter := TSpecialCurveParameter(
        CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
    EditInitValue.Text := FloatToStr(Parameter.Value);

    if FTick.Clicked then
        UpdateCheck;
end;

procedure TUserPointsSetPropDlg.CheckListFixedClickCheck(Sender: TObject);
begin
    if FTick.Ticked(CheckListFixed.ItemIndex <> -1) then
        UpdateCheck;
end;

procedure TUserPointsSetPropDlg.BtnApplyClick(Sender: TObject);
var
    Parameter: TSpecialCurveParameter;
    Value: double;
begin
    if CheckListFixed.ItemIndex <> -1 then
    begin
        Parameter := TSpecialCurveParameter(
            CheckListFixed.Items.Objects[CheckListFixed.ItemIndex]);
        //  READ FIRST, ASSIGNED ONLY IF IT READ. The local parser this
        //  replaced raised on a typo, so the assignment was skipped by an
        //  exception - which also left the process-wide decimal separator
        //  swapped, because it was mid-swap when it raised. See findings.md.
        if not TryTypedNumber(EditInitValue.Text, Value) then
        begin
{$IFDEF _WINDOWS}
            ShowBalloon(EditInitValue.Handle,
                ImproperRealValueInput,
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(string(ImproperRealValueInput), mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := EditInitValue;
        end
        else
            Parameter.Value := Value;
    end;
end;

procedure TUserPointsSetPropDlg.FormActivate(Sender: TObject);
begin
    CheckAssigned(FCurveType, 'the curve type this dialog was opened to edit');
    //  The dialog is reused, so a tick left owing by a previous visit must not
    //  be carried out against this visit's list.
    FTick := NoDeferredTick;
    EditExpression.Text := FCurveType.Expression;
    FillComboArgument;
    FillComboPosition;
    FillComboRole(ComboAmplitude, Amplitude);
    FillComboRole(ComboWidth, special_curve_parameter.Width);
    FillCheckListFixed;
end;

procedure TUserPointsSetPropDlg.FillComboArgument;
begin
    ShowChoices(ComboArgument, ArgumentChoices(FCurveType.Parameters));
end;

procedure TUserPointsSetPropDlg.FillComboPosition;
begin
    ShowChoices(ComboPosition, PositionChoices(FCurveType.Parameters));
end;

procedure TUserPointsSetPropDlg.FillCheckListFixed;
var
    i, Index: longint;
    Choices: TParameterChoices;
begin
    Choices := FixedChoices(FCurveType.Parameters);
    CheckListFixed.Items.Clear;
    for i := 0 to High(Choices) do
    begin
        Index := CheckListFixed.Items.Add(Choices[i].Name);
        //  The tick BEFORE the object, as it was: setting Checked on an item
        //  whose object is already attached fires the click-check handler,
        //  which reads the object and writes a type back.
        if Choices[i].Marked then
            CheckListFixed.Checked[Index] := True;
        CheckListFixed.Items.Objects[Index] := Choices[i].Parameter;
    end;
end;

initialization
  {$I user_points_set_prop_dialog.lrs}
end.
