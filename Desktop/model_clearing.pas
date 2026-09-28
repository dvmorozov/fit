// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Clearing the whole model: asked first, and only then done.)

WHY A UNIT AND NOT THE MENU HANDLER. The decision that matters here - nothing is
removed unless the user said yes - would be unreachable by any test inside
TFormMain. So the window hands its IUiHost and its client to this function and
does nothing else; the tests hand it a recording host and a mocked client, and
drive the very call the menu makes.

THE QUESTION NAMES THE COUNT and says it cannot be undone. "Are you sure?" is
answered by reflex; "Remove all 12 curves" is read. There is no undo in this
application, so saying so is the honest thing rather than a scare.
}
unit model_clearing;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_ui_host, fit_client, checks;

const
    CLEAR_MODEL_TITLE = 'Clear Model';

{ The confirmation text for removing ACurveCount curves. }
function ClearModelQuestion(ACurveCount: longint): string;

{ What is said when ARemaining curves are still in the model after clearing.
  AReason is the server's own refusal, when it gave one. }
function ClearModelLeftoverMessage(ARemaining: longint;
    const AReason: string): string;

{ Asks AHost whether every curve may be removed, and removes them if so.

  Answers how many curves are actually GONE - counted from the model before and
  after, not from requests sent - or -1 when the user declined. An EMPTY model is
  not asked about and answers 0: the command is greyed for one, and a question
  about removing nothing is noise.

  SMOOTH, NOT FORCED. A curve the engine keeps - one it rebuilds from the fit
  interval rather than from a pick, or one an analysis pack declines to give up
  - stays where it is. Nothing is switched behind the user's back to make it go
  (the curve type, the intervals), nothing raises, and the user is told ONCE,
  with how many stayed and why. A clean clearing says nothing more. }
function ClearModelWithConsent(const AHost: IUiHost;
    AClient: TFitClient): longint;

implementation

function ClearModelLeftoverMessage(ARemaining: longint;
    const AReason: string): string;
var
    Noun: string;
begin
    if ARemaining = 1 then
        Noun := '1 curve stays'
    else
        Noun := Format('%d curves stay', [ARemaining]);
    Result := Noun + ' in the model.' + LineEnding;
    if AReason <> '' then
        Result := Result + AReason
    else
        //  No refusal, yet the curve is back: the engine rebuilt it, which is
        //  what a curve type placed per fit interval rather than by a pick does.
        Result := Result + 'The current curve type places one curve per fit ' +
            'interval rather than from a pick, so the model is rebuilt with it. ' +
            'Choose another curve type to change that.';
end;

function ClearModelQuestion(ACurveCount: longint): string;
var
    Noun: string;
begin
    if ACurveCount = 1 then
        Noun := 'curve'
    else
        Noun := 'curves';
    Result := Format('Remove all %d %s from the model? Their picks go with ' +
        'them. This cannot be undone.', [ACurveCount, Noun]);
end;

function ClearModelWithConsent(const AHost: IUiHost;
    AClient: TFitClient): longint;
var
    Count, Remaining: longint;
    Reason: string;
begin
    CheckAssigned(AClient, 'the fit client whose model is cleared');
    CheckThat(Assigned(AClient.FitService), 'the fit service is missing');

    Count := AClient.FitService.GetCurveCount;
    if Count = 0 then
        Exit(0);

    if not AHost.Confirm(CLEAR_MODEL_TITLE, ClearModelQuestion(Count)) then
        Exit(-1);

    AClient.ClearModel(Reason);

    //  ASKED, NOT ASSUMED. Whether a removal stuck is the engine's to say.
    Remaining := AClient.FitService.GetCurveCount;
    Result := Count - Remaining;
    if Remaining > 0 then
        AHost.ShowMessage(CLEAR_MODEL_TITLE,
            ClearModelLeftoverMessage(Remaining, Reason), umInfo);
end;

end.
