// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which explanation the Explain pane shows.)

WHY A UNIT OF ITS OWN. The pane follows what the user is looking at, and three
things compete for that: the selected Model row, the selected curve type, and -
while a menu is open - the entry under the pointer. The events arrive in any
order, and every wrong answer is quiet: the pane keeps showing the previous
thing, or goes blank while something explainable is in front of the user. A
decision like that inside an LCL form cannot be tested, so it is a pure function
of the events here, and the form only reports events and draws the answer.

THE RULE.
  * A hovered entry wins while its menu is open, if its topic resolves.
  * Otherwise the more recently chosen of row and curve type, if it resolves,
    and failing that the other one.
  * Otherwise nothing.

A HOVER NEEDS AN OPEN MENU. A widget set can report a hover late, after the menu
has gone; showing an entry no longer on screen would describe nothing visible.
And opening a menu forgets the previous hover, for the same reason.

RESOLVES IS ASKED, NOT REMEMBERED. A topic that was explainable when chosen can
stop being so - the model is rebuilt and the row's pattern is gone - so whether
each candidate resolves is asked at the moment of showing, and an unresolvable
one falls back rather than blanking the pane.

WHERE THE HOVER CANNOT BE SEEN. Cocoa never reports a pointer resting on a menu
item (TMenuItem.IntfDoSelect is not called there), so on macOS only the row and
curve-type halves of this rule ever fire - which is why a module's rows carry
topics explaining everything its context menu offers over them.
}
unit explanation_focus;

{$mode objfpc}{$H+}

interface

type
    { Which of the two selections was made last. }
    TLastSelected = (lsNone, lsRow, lsCurveType);

    TExplanationFocus = record
        RowTopic: string;
        CurveTypeTopic: string;
        HoverTopic: string;
        MenuOpen: boolean;
        LastSelected: TLastSelected;
        { A topic reached by following a link, or asked for by a module. }
        LinkTopic: string;
    end;

    { Whether a topic can be explained right now. A plain function, so the
      form's argument is a named routine whose signature the compiler checks. }
    TTopicResolves = function(const ATopic: string): boolean;

function FocusStart: TExplanationFocus;
{ A row was selected; '' when the selection was cleared or the row names nothing. }
function FocusRowSelected(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
function FocusCurveTypeSelected(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
function FocusMenuOpened(const AFocus: TExplanationFocus): TExplanationFocus;
{ The pointer rests on an entry; '' for one with no topic. }
function FocusMenuHovered(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
function FocusMenuClosed(const AFocus: TExplanationFocus): TExplanationFocus;
{ A link to ATopic was followed, or a module asked for it to be shown. }
function FocusLinkFollowed(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
{ The topic to show, or '' when nothing in focus resolves. }
function FocusShownTopic(const AFocus: TExplanationFocus;
    AResolves: TTopicResolves): string;

implementation

function FocusStart: TExplanationFocus;
begin
    Result := Default(TExplanationFocus);
end;

function FocusRowSelected(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
begin
    Result := AFocus;
    Result.RowTopic := ATopic;
    Result.LastSelected := lsRow;
    //  A new focus replaces a followed link: the user moved on.
    Result.LinkTopic := '';
end;

function FocusCurveTypeSelected(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
begin
    Result := AFocus;
    Result.CurveTypeTopic := ATopic;
    Result.LastSelected := lsCurveType;
    Result.LinkTopic := '';
end;

function FocusMenuOpened(const AFocus: TExplanationFocus): TExplanationFocus;
begin
    Result := AFocus;
    Result.MenuOpen := True;
    Result.HoverTopic := '';
    Result.LinkTopic := '';
end;

function FocusMenuHovered(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
begin
    Result := AFocus;
    if Result.MenuOpen then
        Result.HoverTopic := ATopic;
end;

function FocusMenuClosed(const AFocus: TExplanationFocus): TExplanationFocus;
begin
    Result := AFocus;
    Result.MenuOpen := False;
    Result.HoverTopic := '';
end;

function FocusLinkFollowed(const AFocus: TExplanationFocus;
    const ATopic: string): TExplanationFocus;
begin
    Result := AFocus;
    Result.LinkTopic := ATopic;
end;

function FocusShownTopic(const AFocus: TExplanationFocus;
    AResolves: TTopicResolves): string;

    function Resolves(const ATopic: string): boolean;
    begin
        Result := (ATopic <> '') and Assigned(AResolves) and AResolves(ATopic);
    end;

var
    First: string;
begin
    //  WHAT WAS ASKED FOR OUTRANKS WHAT IS POINTED AT. A followed link, or a
    //  module showing the rule that just refused a click, is an explicit
    //  request - and the refused entry may still be under the pointer.
    if Resolves(AFocus.LinkTopic) then
        Exit(AFocus.LinkTopic);
    if AFocus.MenuOpen and Resolves(AFocus.HoverTopic) then
        Exit(AFocus.HoverTopic);

    //  ONLY WHAT WAS CHOSEN LAST, and nothing standing in for it. With no row
    //  selected in the Model panel - an empty model, a cleared selection - the
    //  pane is empty: a curve type chosen earlier, or the one the model happens
    //  to use, is not what the user is looking at.
    if AFocus.LastSelected = lsCurveType then
        First := AFocus.CurveTypeTopic
    else
        First := AFocus.RowTopic;

    if Resolves(First) then
        Result := First
    else
        Result := '';
end;

end.
