// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TInputMaxRFactorDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit set_maximum_rfactor_dialog;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Controls, ExtCtrls, Forms, LResources, StdCtrls, SysUtils
{$IFDEF _WINDOWS}
    , CommCtrl, Windows
{$ELSE}
    , Dialogs
{$ENDIF}
    ;

type

    { TSetMaximumRFactorDlg }

    TSetMaximumRFactorDlg = class(TForm)
        ButtonOK:     TButton;
        ButtonCancel: TButton;
        BevelButtons:    TBevel;
        EditMaxRFactor: TEdit;
        LabelMaxRFactor:    TLabel;
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormActivate(Sender: TObject);
    private

    public
        FValue: double;
    end;

var
    SetMaximumRFactorDlg: TSetMaximumRFactorDlg;

const
    ImproperRealValueInput: WideString = 'Please enter a valid number (for example, 1.5).';
{$IFDEF _WINDOWS}
const
    Error: WideString = 'Error';

{$ENDIF}

{$IFDEF _WINDOWS}
procedure ShowBalloon(Hwnd: HWND; Msg: WideString; Title: WideString);

type
    BalloonException = class(Exception)
    public
        Handle: HWND;
    end;

{$ENDIF}

implementation

uses
    typed_number;

{$IFDEF _WINDOWS}
   //  pri isp. PostMessage d.b. global'noy, t.k.
   //  struktura obrabatyvaetsya vne tela protsedury
var
    EBT: _tagEDITBALLOONTIP;

{$hints off}
procedure ShowBalloon(Hwnd: HWND; Msg: WideString; Title: WideString);
//var FH: TFormHint;
begin
    EBT.cbStruct := SizeOf(EBT);
    EBT.pszText  := PWideChar(Msg);
    EBT.pszTitle := PWideChar(Title);
    EBT.ttiIcon  := 0;
    //  Uses SendMessage.
    Edit_ShowBalloonTip(Hwnd, LPARAM(Addr(EBT)));
    //  SendMessage sometimes caused something like a stack overflow, and
    //  PostMessage deadlocked while an exception was allowed to escape the event
    //  handler.
    //  WITH SendMessage the balloon closes too early.
    //PostMessage(Hwnd, EM_SHOWBALLOONTIP, 0, LPARAM(Addr(EBT)));
    (*
    FH := TFormHint.Create(nil);
    FH.LabelMaxRFactor.Caption := Msg;
    FH.Caption := Title;
    //  pri isp. Show okno srazu ischezaet
    FH.ShowModal;
    *)
end;

{$hints on}
{$ENDIF}

procedure TSetMaximumRFactorDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := True;
    //  posle uspeshnogo zakrytiya okna d. b.
    //  garantirovano, chto znachenie korrektno
    if ModalResult = mrOk then
        //  READ, NOT PARSED HERE. The rule belongs to typed_number, which reads
        //  with a full stop whatever the locale and never touches the
        //  process-wide separator - see findings.md for what the local copy
        //  this replaced did when a typo made it raise mid-swap.
        if not TryTypedNumber(EditMaxRFactor.Text, FValue) then
        begin
{$IFDEF _WINDOWS}
            ShowBalloon(EditMaxRFactor.Handle,
                ImproperRealValueInput, '');
{$ELSE}
            MessageDlg(string(ImproperRealValueInput), mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := EditMaxRFactor;
            CanClose      := False;
        end
        else
            //  The box is in per cent; the model holds a fraction.
            FValue := FValue / 100;
end;

procedure TSetMaximumRFactorDlg.FormActivate(Sender: TObject);
begin
    EditMaxRFactor.Text := FloatToStr(FValue * 100);
    ActiveControl := EditMaxRFactor;
end;

initialization
    {$i set_maximum_rfactor_dialog.lrs}
end.
