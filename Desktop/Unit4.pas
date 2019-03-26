{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TInputMaxRFactorDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit Unit4;

{$MODE Delphi}

interface

uses SysUtils, Forms, Controls, StdCtrls,
  ExtCtrls, LResources
{$ifdef windows}
  ,Windows, CommCtrl
{$endif}
  ;

type

  { TInputMaxRFactorDlg }

  TInputMaxRFactorDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    RFactorValueEdit: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private

  public
    Value: Double;
  end;

var
    InputMaxRFactorDlg: TInputMaxRFactorDlg;
  
const
    ImproperRealValueInput: WideString = 'Improper real value input.';
{$ifdef windows}
const
    Error: WideString = 'Error';
{$endif}
  
function StringToValue(Str: string): Double;
{$ifdef windows}
procedure ShowBalloon(Hwnd: HWND; Msg: WideString; Title: WideString);

type
    BalloonException = class(Exception)
    public
        Handle: HWND;
    end;
{$endif}

implementation

//uses Unit6;

//  vypolnyaet podgotovku stroki k preobrazovaniyu v chislo;
//  esli dazhe posle podgotovki preobrazovat' stroku v chislo
//  nevozmozhno, to voznikaet isklyuchenie
function StringToValue(Str: string): Double;
var SavedDecimalSeparator : Char;
    Index: LongInt;
    LastFound: Boolean;
begin
    SavedDecimalSeparator := DecimalSeparator;
    //  vse zapyatye zamenyayutsya na tochki
    while True do
    begin
        Index := Pos(',', Str);
        if Index = 0 then Break;
        Str[Index] := '.';
    end;
    //  udalyayutsya vse tochki, krome posledney
    Index := Length(Str); LastFound := False;
    while Index > 0 do
    begin
        if Str[Index] = '.' then
        begin
            if not LastFound then LastFound := True
            else Delete(Str, Index, 1);
        end;
        Dec(Index);
    end;
    
    DecimalSeparator := '.';
    try
        Result := StrToFloat(Str);
    finally
        DecimalSeparator := SavedDecimalSeparator;
    end;
end;

{$ifdef windows}
//  pri isp. PostMessage d.b. global'noy, t.k.
//  struktura obrabatyvaetsya vne tela protsedury
var EBT: _tagEDITBALLOONTIP;
    
procedure ShowBalloon(Hwnd: HWND; Msg: WideString; Title: WideString);
//var FH: TFormHint;
begin
    EBT.cbStruct := SizeOf(EBT);
    EBT.pszText := PWideChar(Msg);
    EBT.pszTitle := PWideChar(Title);
    EBT.ttiIcon := 0;
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
{$endif}

procedure TInputMaxRFactorDlg.FormCloseQuery(Sender: TObject;
    var CanClose: Boolean);
begin
    CanClose := True;
    //  posle uspeshnogo zakrytiya okna d. b.
    //  garantirovano, chto znachenie korrektno
    if ModalResult = mrOk then
    begin
        try
            Value := StringToValue(RFactorValueEdit.Text) / 100;
        except
{$ifdef windows}
            ShowBalloon(RFactorValueEdit.Handle,
                ImproperRealValueInput,
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$else}
            MessageDlg(ImproperRealValueInput, mtError, [mbOk], 0);
{$endif}
            ActiveControl := RFactorValueEdit;
            CanClose := False;
        end;
    end;{if ModalResult = mrOk then...}
end;

procedure TInputMaxRFactorDlg.FormActivate(Sender: TObject);
begin
    RFactorValueEdit.Text := FloatToStr(Value * 100);
    ActiveControl := RFactorValueEdit;
end;

initialization
    {$i Unit4.lrs}
end.


