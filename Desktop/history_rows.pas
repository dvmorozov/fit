// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the History tab says about each model: its two lines, its
details, and the words of every refusal and question about it.)

WHY THIS IS NOT IN THE WINDOW. Each of these is a small decision that goes wrong
quietly - a model nobody fitted shown with an R-factor of -1, a time in UTC read
as local, a greyed command with nothing saying why - and none of them can be
tested inside an LCL class. The window draws the strings; this unit decides them.

TIMES ARE SHOWN IN LOCAL TIME and stored in UTC. The offset is a parameter rather
than a call to the clock, so a test can say what a time in another zone reads as
- and so the rule is checkable at all.
}
unit history_rows;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, DateUtils, model_history;

const
    HistoryTabCaption = 'History';
    HistoryTabHint = 'Every model a fit reached, and the one each started from';
    HistoryEmptyText = 'No models yet. Each fit records the model it ' +
        'reaches here, so you can come back to it.';
    HistoryListHint = 'Click a model to see it described below. ' +
        'Double-click it, or press Enter, to make it the current model.';

{ The first line of a row: the user's name for it if any, the R-factor and the
  number of curves - what tells one model from another at a glance. }
function HistoryRowHeadline(const AEntry: THistoryEntry): string;
{ The second line: when, and what made it. ALocalMinusUtc is the local
  offset in minutes; ANowLocal decides whether the date is worth showing. }
function HistoryRowByline(const AEntry: THistoryEntry;
    ALocalMinusUtc: longint; ANowLocal: TDateTime): string;
{ The marks before a row: the current model and the best one. }
function HistoryRowMarks(AIsCurrent, AIsBest: boolean): string;

type
    { A curve type's name, from its id as the model records it; '' when it is
      not known. A PARAMETER, not a lookup: names live in the curve-type
      registry, which this unit has no business linking - the window hands
      over the one it has. }
    THistoryTypeNamer = function(const ATypeId: string): string;

{ Everything known about the entry at AIndex, a line per fact, for the pane
  under the list. ANamer, when given, names the curve type the model is built
  from. }
function HistoryDetails(AHistory: TModelHistory; AIndex: longint;
    ALocalMinusUtc: longint; ANamer: THistoryTypeNamer = nil): string;

{ Why Make Current is offered or not, for the command's hint. }
function HistoryMakeCurrentHint(AHistory: TModelHistory;
    AIndex: longint): string;

{ The question Delete asks, naming the model it would remove. }
function HistoryDeleteQuestion(AHistory: TModelHistory;
    AIndex: longint; ALocalMinusUtc: longint): string;

{ A time this program wrote (ISO 8601 UTC) as local time; False when AUtc is
  not one. }
function TryLocalTimeOf(const AUtc: string; ALocalMinusUtc: longint;
    out ALocal: TDateTime): boolean;

implementation

uses
    rfactor_text;

function RFactorWords(ARFactor: double): string;
begin
    //  -1 IS "NO FIT HAS RUN", and printed as a number it reads as an
    //  impossibly good one.
    if ARFactor < 0 then
        Result := 'Not fitted'
    else
        Result := 'R ' + RFactorText(ARFactor);
end;

function CurvesWords(ACount: longint): string;
begin
    if ACount = 1 then
        Result := '1 curve'
    else
        Result := IntToStr(ACount) + ' curves';
end;

function HistoryRowHeadline(const AEntry: THistoryEntry): string;
begin
    Result := RFactorWords(AEntry.Model.RFactor) + ' · ' +
        CurvesWords(Length(AEntry.Model.Curves));
    if AEntry.Name <> '' then
        Result := AEntry.Name + ' · ' + Result;
end;

function TryLocalTimeOf(const AUtc: string; ALocalMinusUtc: longint;
    out ALocal: TDateTime): boolean;
var
    Y, Mo, D, H, Mi, S: longint;
begin
    ALocal := 0;
    //  EXACTLY the form this program writes - yyyy-mm-ddThh:nn:ssZ - and
    //  nothing looser: a time read wrongly is worse than one shown as written.
    Result := (Length(AUtc) = 20) and (AUtc[5] = '-') and (AUtc[8] = '-') and
        (AUtc[11] = 'T') and (AUtc[14] = ':') and (AUtc[17] = ':') and
        (AUtc[20] = 'Z') and
        TryStrToInt(Copy(AUtc, 1, 4), Y) and TryStrToInt(Copy(AUtc, 6, 2), Mo) and
        TryStrToInt(Copy(AUtc, 9, 2), D) and TryStrToInt(Copy(AUtc, 12, 2), H) and
        TryStrToInt(Copy(AUtc, 15, 2), Mi) and
        TryStrToInt(Copy(AUtc, 18, 2), S) and
        TryEncodeDateTime(Y, Mo, D, H, Mi, S, 0, ALocal);
    if Result then
        ALocal := IncMinute(ALocal, ALocalMinusUtc);
end;

function WhenWords(const AUtc: string; ALocalMinusUtc: longint;
    ANowLocal: TDateTime): string;
var
    T: TDateTime;
begin
    if not TryLocalTimeOf(AUtc, ALocalMinusUtc, T) then
        Exit(AUtc);
    //  THE TIME ALONE ON THE DAY IT WAS RECORDED: a session's runs are
    //  minutes apart, and a date on every row is noise until it is not today.
    if DateOf(T) = DateOf(ANowLocal) then
        Result := FormatDateTime('hh:nn', T)
    else
        Result := FormatDateTime('yyyy-mm-dd hh:nn', T);
end;

function RunWords(const AEntry: THistoryEntry): string;
begin
    Result := HistoryRunKindName(AEntry.RunKind);
    if AEntry.Stopped then
        Result := Result + ', stopped';
end;

function HistoryRowByline(const AEntry: THistoryEntry;
    ALocalMinusUtc: longint; ANowLocal: TDateTime): string;
begin
    Result := WhenWords(AEntry.CreatedUtc, ALocalMinusUtc, ANowLocal) + ' · ' +
        RunWords(AEntry);
end;

function HistoryRowMarks(AIsCurrent, AIsBest: boolean): string;
begin
    Result := '';
    if AIsCurrent then
        Result := Result + '● ';
    if AIsBest then
        Result := Result + '★ ';
end;

function HistoryDetails(AHistory: TModelHistory; AIndex: longint;
    ALocalMinusUtc: longint; ANamer: THistoryTypeNamer): string;
var
    E: THistoryEntry;
    Parent: longint;
    T: TDateTime;
    Lines: string;

    procedure Line(const AText: string);
    begin
        if Lines <> '' then
            Lines := Lines + LineEnding;
        Lines := Lines + AText;
    end;

begin
    Result := '';
    if (AIndex < 0) or (AIndex >= AHistory.Count) then
        Exit;
    E := AHistory[AIndex];
    Lines := '';
    if E.Name <> '' then
        Line(E.Name);
    if TryLocalTimeOf(E.CreatedUtc, ALocalMinusUtc, T) then
        Line('Recorded ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', T) +
            ', when ' + RunWords(E) + ' ended.')
    else
        Line('Recorded ' + E.CreatedUtc + ', when ' + RunWords(E) + ' ended.');
    if E.Model.RFactor < 0 then
        Line('Not fitted: nothing has measured this model.')
    else
        Line('R-factor ' + RFactorText(E.Model.RFactor) + '.');
    if AHistory.BestIndex = AIndex then
        Line('The lowest R-factor among the models fitted to the same data ' +
            'with the same objective.');
    Line(CurvesWords(Length(E.Model.Curves)) + ', from ' +
        IntToStr(Length(E.Model.Positions.X)) + ' picked positions.');
    if Assigned(ANamer) and (ANamer(E.Model.Settings.CurveTypeId) <> '') then
        Line('Curve type: ' + ANamer(E.Model.Settings.CurveTypeId) + '.');
    if E.Model.Statistics.Valid then
        Line(Format('R² %.4f, reduced χ² %.4g, AIC %.4g, BIC %.4g.',
            [E.Model.Statistics.RSquared, E.Model.Statistics.ReducedChiSquare,
            E.Model.Statistics.AIC, E.Model.Statistics.BIC]));
    Parent := AHistory.IndexOf(E.ParentId);
    if Parent < 0 then
        Line('The first model recorded on this line of work.')
    else
        Line('Fitted from the model ' + HistoryRowHeadline(AHistory[Parent]) +
            '.');
    if E.Id = AHistory.CurrentId then
        Line('This is the current model.');
    if not AHistory.IsRestorable(AIndex) then
        Line('The profile it was fitted to is missing from the project, so ' +
            'it cannot be made current.');
    Result := Lines;
end;

function HistoryMakeCurrentHint(AHistory: TModelHistory;
    AIndex: longint): string;
begin
    if (AIndex < 0) or (AIndex >= AHistory.Count) then
        Result := 'Select a model in the history to make it the current one.'
    else if AHistory[AIndex].Id = AHistory.CurrentId then
        Result := 'This is already the current model.'
    else if not AHistory.IsRestorable(AIndex) then
        Result := 'The profile this model was fitted to is missing from the ' +
            'project, so it cannot be put back.'
    else
        Result := 'Make the selected model the current one: the chart, the ' +
            'tables and the next fit start from it.';
end;

function HistoryDeleteQuestion(AHistory: TModelHistory;
    AIndex: longint; ALocalMinusUtc: longint): string;
var
    i: longint;
    HasChildren: boolean;
    T: TDateTime;
    When: string;
begin
    Result := '';
    if (AIndex < 0) or (AIndex >= AHistory.Count) then
        Exit;
    if TryLocalTimeOf(AHistory[AIndex].CreatedUtc, ALocalMinusUtc, T) then
        When := FormatDateTime('yyyy-mm-dd hh:nn', T)
    else
        When := AHistory[AIndex].CreatedUtc;
    Result := 'Delete the model recorded at ' + When + ' (' +
        HistoryRowHeadline(AHistory[AIndex]) + ') from the history? ' +
        'The current model is not changed.';
    HasChildren := False;
    for i := 0 to AHistory.Count - 1 do
        if AHistory[i].ParentId = AHistory[AIndex].Id then
            HasChildren := True;
    if HasChildren then
        Result := Result + ' The models fitted from it stay, and are shown ' +
            'as fitted from the one before it.';
end;

end.
