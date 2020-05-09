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
        OKBtn:     TButton;
        CancelBtn: TButton;
        Bevel1:    TBevel;
        RFactorValueEdit: TEdit;
        Label1:    TLabel;
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormActivate(Sender: TObject);
    private

    public
        FValue: double;
    end;

var
    SetMaximumRFactorDlg: TSetMaximumRFactorDlg;

const
    ImproperRealValueInput: WideString = 'Improper real value input.';
{$IFDEF _WINDOWS}
const
    Error: WideString = 'Error';

{$ENDIF}

function StringToValue(Str: string): double;
{$IFDEF _WINDOWS}
procedure ShowBalloon(Hwnd: HWND; Msg: WideString; Title: WideString);

type
    BalloonException = class(Exception)
    public
        Handle: HWND;
    end;

{$ENDIF}

implementation

{$warnings off}
   //  vypolnyaet podgotovku stroki k preobrazovaniyu v chislo;
   //  esli dazhe posle podgotovki preobrazovat' stroku v chislo
   //  nevozmozhno, to voznikaet isklyuchenie
function StringToValue(Str: string): double;
var
    SavedDecimalSeparator: char;
    Index:     longint;
    LastFound: boolean;
begin
    SavedDecimalSeparator := DecimalSeparator;
    //  vse zapyatye zamenyayutsya na tochki
    while True do
    begin
        Index := Pos(',', Str);
        if Index = 0 then
            Break;
        Str[Index] := '.';
    end;
    //  udalyayutsya vse tochki, krome posledney
    Index     := Length(Str);
    LastFound := False;
    while Index > 0 do
    begin
        if Str[Index] = '.' then
            if not LastFound then
                LastFound := True
            else
                Delete(Str, Index, 1);
        Dec(Index);
    end;

    DecimalSeparator := '.';
    try
        Result := StrToFloat(Str);
    finally
        DecimalSeparator := SavedDecimalSeparator;
    end;
end;

{$warnings on}

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
    //  ispol'zuet SendMessage
    Edit_ShowBalloonTip(Hwnd, LPARAM(Addr(EBT)));
    //  SendMessage inogda vyzyvaet chto-to vrode perepolneniya steka,
    //  PostMessage - deadlock poka dopuskalsya vyhod isklyucheniya za
    //  granitsy obrabotchika sobytiya
    //  !!! pri isp. SendMessage balloon zakryvaetsya slishkom rano !!!
    //PostMessage(Hwnd, EM_SHOWBALLOONTIP, 0, LPARAM(Addr(EBT)));
    (*
    FH := TFormHint.Create(nil);
    FH.Label1.Caption := Msg;
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
        try
            FValue := StringToValue(RFactorValueEdit.Text) / 100;
        except
{$IFDEF _WINDOWS}
            ShowBalloon(RFactorValueEdit.Handle,
                ImproperRealValueInput, '');
{$ELSE}
            MessageDlg(string(ImproperRealValueInput), mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := RFactorValueEdit;
            CanClose      := False;
        end; {if ModalResult = mrOk then...}
end;

procedure TSetMaximumRFactorDlg.FormActivate(Sender: TObject);
begin
    RFactorValueEdit.Text := FloatToStr(FValue * 100);
    ActiveControl := RFactorValueEdit;
end;

initialization
    {$i set_maximum_rfactor_dialog.lrs}
end.
