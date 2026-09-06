// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A window for a module's UI to talk to, with no window in it.)

`IUiHost` IS THE SEAM A MODULE'S UI WAS WRITTEN AGAINST. A module never names an
LCL type: it asks for a hint, a message, a confirmation, a line of input, a
picking mode, a menu state, a panel. All ten of those go through this interface,
so a module's whole user-facing half can be driven without a form - which is what
this stands in for.

WHAT IT ANSWERS IS SCRIPTED, and that is the point. The interesting cases in a
module's UI are the ones a real window makes hard to reach: the user cancelled
the input box, the user said no to the confirmation, the text they typed is not a
number. Each is one property here.

WHAT IT RECORDS IS EVERYTHING ELSE - which menu entries were enabled and
checked, what the hint said, which picking mode was started and how many picks
it declared, what went into the panel. A module that forgets to untick its own
entry when a mode ends is a tick that says the mode is on when it is off, and the
next click on it reads as "leave" instead of "enter": that is a defect nothing
but a recording host can see.

A PLAIN TObject, per the rule in mock_support: everything compiles -SIcorba, so
interfaces carry no reference counting and the fixture owns this outright - nil
the interface first, then free the object.
}
unit mock_ui_host;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_ui_host, module_view_types, mock_support;

type
    TMockUiHost = class(TInterfacedObject, IUiHost)
    private
        FLog: TCallLog;
        FHints: TStringList;
        FMessages: TStringList;
        FEnabled: TStringList;
        FChecked: TStringList;
        FPanelRows: longint;
        FPanelId: string;
        FPickSet: string;
        FPickMenuId: string;
        FPickHint: string;
        FPicksPerGesture: longint;
        FConfirmAnswer: boolean;
        FTextAnswer: string;
        FTextAccepted: boolean;
        FLastPrompt: string;
        FLastConfirmText: string;
    public
        constructor Create;
        destructor Destroy; override;

        //  IUiHost
        procedure ShowHint(const AText: string);
        procedure ShowMessage(const ATitle, AText: string;
            AKind: TUiMessageKind);
        function Confirm(const ATitle, AText: string): boolean;
        function AskText(const ATitle, APrompt: string;
            var AValue: string): boolean;
        procedure BeginPointPicking(const APointSet, AMenuId: string;
            APicksPerGesture: longint; const AHint: string);
        procedure SetMenuEnabled(const AId: string; AEnabled: boolean);
        procedure SetMenuChecked(const AId: string; AChecked: boolean);
        procedure ShowModulePanel(const APanelId: string;
            const ARows: TOutline);

        { A corba interface carries no cast to TObject - see mock_support. }
        function AsObject: TObject;

        { --- what the module was told --- }

        { What the user will answer the next confirmation. Defaults to True,
          because a module that asks is usually asking to proceed and the
          interesting case is the refusal, which a test sets deliberately. }
        property ConfirmAnswer: boolean
            read FConfirmAnswer write FConfirmAnswer;
        { What the user will type, and whether they accept the box at all.
          Cancelling is the case a real dialog makes hardest to reach. }
        property TextAnswer: string read FTextAnswer write FTextAnswer;
        property TextAccepted: boolean
            read FTextAccepted write FTextAccepted;

        { --- what the module did --- }

        property Log: TCallLog read FLog;
        { The most recent hint, or '' - what the user reads while picking. }
        function LastHint: string;
        { True when a message of any kind was shown. }
        function ShowedAMessage: boolean;
        { The most recent message, title and text joined. }
        function LastMessage: string;
        { The prompt of the most recent input box. }
        property LastPrompt: string read FLastPrompt;
        { The TEXT of the most recent confirmation, not its title. A
          confirmation that names what it is about to do is a different thing
          from one that asks "are you sure?", and only the text says which. }
        property LastConfirmText: string read FLastConfirmText;
        { How many hints were shown. "Said nothing" and "said the same thing
          again" are different module decisions, and the most recent hint alone
          cannot tell them apart. }
        function HintCount: longint;

        { The state the module last asked for on one of its entries. Answers
          True/False only when the module said so; UnknownMenuState when it
          never mentioned that entry, which is a different thing from having
          set it False. }
        function MenuEnabledIsKnown(const AId: string): boolean;
        function MenuEnabled(const AId: string): boolean;
        function MenuCheckedIsKnown(const AId: string): boolean;
        function MenuChecked(const AId: string): boolean;

        { The picking mode the module started, if it did. }
        property PickSet: string read FPickSet;
        property PickMenuId: string read FPickMenuId;
        property PickHint: string read FPickHint;
        property PicksPerGesture: longint read FPicksPerGesture;

        { The panel the module filled, and how many rows went into it. -1 when
          it never filled one: "asked to clear" and "never asked" are different
          module decisions and only one of them is a defect. }
        property PanelId: string read FPanelId;
        property PanelRows: longint read FPanelRows;
    end;

implementation

constructor TMockUiHost.Create;
begin
    inherited Create;
    FLog := TCallLog.Create;
    FHints := TStringList.Create;
    FMessages := TStringList.Create;
    FEnabled := TStringList.Create;
    FChecked := TStringList.Create;
    FPanelRows := -1;
    FPicksPerGesture := -1;
    //  The defaults a module meets when a test says nothing: the user agrees,
    //  and accepts the input box with whatever TextAnswer holds.
    FConfirmAnswer := True;
    FTextAccepted := True;
end;

destructor TMockUiHost.Destroy;
begin
    FChecked.Free;
    FEnabled.Free;
    FMessages.Free;
    FHints.Free;
    FLog.Free;
    inherited;
end;

function TMockUiHost.AsObject: TObject;
begin
    Result := Self;
end;

{ ---- IUiHost --------------------------------------------------------------- }

procedure TMockUiHost.ShowHint(const AText: string);
begin
    FHints.Add(AText);
    FLog.Note('ShowHint', AText);
end;

procedure TMockUiHost.ShowMessage(const ATitle, AText: string;
    AKind: TUiMessageKind);
begin
    FMessages.Add(ATitle + ': ' + AText);
    FLog.Note('ShowMessage', ATitle);
end;

function TMockUiHost.Confirm(const ATitle, AText: string): boolean;
begin
    FLastConfirmText := AText;
    FLog.Note('Confirm', ATitle);
    Result := FConfirmAnswer;
end;

function TMockUiHost.AskText(const ATitle, APrompt: string;
    var AValue: string): boolean;
begin
    FLastPrompt := APrompt;
    FLog.Note('AskText', APrompt);
    Result := FTextAccepted;
    //  ONLY ON ACCEPT. A cancelled box must leave the caller's variable alone,
    //  or a module that ignores the False would act on a value the user did not
    //  give it.
    if Result then
        AValue := FTextAnswer;
end;

procedure TMockUiHost.BeginPointPicking(const APointSet, AMenuId: string;
    APicksPerGesture: longint; const AHint: string);
begin
    FPickSet := APointSet;
    FPickMenuId := AMenuId;
    FPicksPerGesture := APicksPerGesture;
    FPickHint := AHint;
    FLog.Note('BeginPointPicking', APointSet);
end;

procedure TMockUiHost.SetMenuEnabled(const AId: string; AEnabled: boolean);
begin
    FEnabled.Values[AId] := BoolToStr(AEnabled, True);
    FLog.Note('SetMenuEnabled', AId + '=' + BoolToStr(AEnabled, True));
end;

procedure TMockUiHost.SetMenuChecked(const AId: string; AChecked: boolean);
begin
    FChecked.Values[AId] := BoolToStr(AChecked, True);
    FLog.Note('SetMenuChecked', AId + '=' + BoolToStr(AChecked, True));
end;

procedure TMockUiHost.ShowModulePanel(const APanelId: string;
    const ARows: TOutline);
begin
    FPanelId := APanelId;
    FPanelRows := Length(ARows);
    FLog.Note('ShowModulePanel', APanelId);
end;

{ ---- what the module did --------------------------------------------------- }

function TMockUiHost.LastHint: string;
begin
    Result := '';
    if FHints.Count > 0 then
        Result := FHints[FHints.Count - 1];
end;

function TMockUiHost.HintCount: longint;
begin
    Result := FHints.Count;
end;

function TMockUiHost.ShowedAMessage: boolean;
begin
    Result := FMessages.Count > 0;
end;

function TMockUiHost.LastMessage: string;
begin
    Result := '';
    if FMessages.Count > 0 then
        Result := FMessages[FMessages.Count - 1];
end;

function TMockUiHost.MenuEnabledIsKnown(const AId: string): boolean;
begin
    Result := FEnabled.IndexOfName(AId) >= 0;
end;

function TMockUiHost.MenuEnabled(const AId: string): boolean;
begin
    Result := StrToBoolDef(FEnabled.Values[AId], False);
end;

function TMockUiHost.MenuCheckedIsKnown(const AId: string): boolean;
begin
    Result := FChecked.IndexOfName(AId) >= 0;
end;

function TMockUiHost.MenuChecked(const AId: string): boolean;
begin
    Result := StrToBoolDef(FChecked.Values[AId], False);
end;

end.
