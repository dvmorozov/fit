// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for which explanation the Explain pane shows.

  WHY THESE EXIST. The pane follows whatever the user is looking at - a Model
  row, a curve type, a menu entry under the pointer - and those events arrive in
  any order: a hover while a row is selected, a menu closed after a hover, a row
  whose topic vanished when the model was rebuilt. Wrong answers here are all
  quiet: the pane shows the previous thing, or goes blank while something
  explainable is in front of the user. So the rule is a pure function of the
  events, and beside the examples it is walked EXHAUSTIVELY over every event
  order up to four long, asserting the two properties that matter: what is shown
  always resolves, and the pane is blank only when nothing in focus does. }
unit testcase_explanation_focus;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation_focus;

type
    TExplanationFocusTest = class(TTestCase)
    published
        procedure NothingFocusedShowsNothing;
        procedure ASelectedRowIsShown;
        procedure ASelectedCurveTypeIsShown;
        procedure TheMoreRecentOfRowAndCurveTypeWins;
        procedure OpeningTheMenuDoesNotChangeWhatIsShown;
        procedure AHoveredEntryWinsWhileTheMenuIsOpen;
        procedure HoveringAnEntryWithNoTopicShowsTheRowRatherThanNothing;
        procedure HoveringAnEntryWhoseTopicDoesNotResolveShowsTheRow;
        procedure ClosingTheMenuRestoresTheRow;
        procedure AHoverOutsideAnOpenMenuIsIgnored;
        procedure ReopeningTheMenuForgetsTheLastHover;
        procedure ARowWhoseTopicNoLongerResolvesShowsNothing;
        procedure DeselectingTheRowShowsNothing;
        procedure ACurveTypeIsShownOnlyWhenItWasTheLastThingChosen;
        procedure AFollowedLinkIsShown;
        procedure AFollowedLinkWinsOverAHoverToo;
        procedure AFollowedLinkThatDoesNotResolveKeepsWhatWasShown;
        procedure SelectingARowReplacesAFollowedLink;
        procedure SelectingACurveTypeReplacesAFollowedLink;
        procedure OpeningAMenuReplacesAFollowedLink;
        procedure EveryEventOrderShowsOnlyWhatResolvesAndIsBlankOnlyWhenNothingApplies;
    end;

implementation

function OnlyKnown(const ATopic: string): boolean;
begin
    //  'gone/...' stands for a topic that was explainable once and is not now.
    Result := (ATopic <> '') and (Copy(ATopic, 1, 5) <> 'gone/');
end;

procedure TExplanationFocusTest.NothingFocusedShowsNothing;
begin
    AssertEquals('', FocusShownTopic(FocusStart, @OnlyKnown));
end;

procedure TExplanationFocusTest.ASelectedRowIsShown;
begin
    AssertEquals('row/1', FocusShownTopic(
        FocusRowSelected(FocusStart, 'row/1'), @OnlyKnown));
end;

procedure TExplanationFocusTest.ASelectedCurveTypeIsShown;
begin
    AssertEquals('type/g', FocusShownTopic(
        FocusCurveTypeSelected(FocusStart, 'type/g'), @OnlyKnown));
end;

procedure TExplanationFocusTest.TheMoreRecentOfRowAndCurveTypeWins;
var
    F: TExplanationFocus;
begin
    F := FocusCurveTypeSelected(FocusRowSelected(FocusStart, 'row/1'), 'type/g');
    AssertEquals('type chosen last', 'type/g', FocusShownTopic(F, @OnlyKnown));
    F := FocusRowSelected(F, 'row/2');
    AssertEquals('row chosen last', 'row/2', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.OpeningTheMenuDoesNotChangeWhatIsShown;
var
    F: TExplanationFocus;
begin
    F := FocusRowSelected(FocusStart, 'row/1');
    AssertEquals('row/1', FocusShownTopic(FocusMenuOpened(F), @OnlyKnown));
end;

procedure TExplanationFocusTest.AHoveredEntryWinsWhileTheMenuIsOpen;
var
    F: TExplanationFocus;
begin
    F := FocusMenuOpened(FocusRowSelected(FocusStart, 'row/1'));
    F := FocusMenuHovered(F, 'entry/p4');
    AssertEquals('entry/p4', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.HoveringAnEntryWithNoTopicShowsTheRowRatherThanNothing;
var
    F: TExplanationFocus;
begin
    F := FocusMenuOpened(FocusRowSelected(FocusStart, 'row/1'));
    F := FocusMenuHovered(F, 'entry/p4');
    F := FocusMenuHovered(F, '');
    AssertEquals('a separator or a plain entry', 'row/1',
        FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.HoveringAnEntryWhoseTopicDoesNotResolveShowsTheRow;
var
    F: TExplanationFocus;
begin
    F := FocusMenuOpened(FocusRowSelected(FocusStart, 'row/1'));
    F := FocusMenuHovered(F, 'gone/entry');
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.ClosingTheMenuRestoresTheRow;
var
    F: TExplanationFocus;
begin
    F := FocusMenuOpened(FocusRowSelected(FocusStart, 'row/1'));
    F := FocusMenuClosed(FocusMenuHovered(F, 'entry/p4'));
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.AHoverOutsideAnOpenMenuIsIgnored;
var
    F: TExplanationFocus;
begin
    //  A widget set can report a hover late, after the menu has gone; showing
    //  an entry that is no longer on screen would describe nothing visible.
    F := FocusMenuHovered(FocusRowSelected(FocusStart, 'row/1'), 'entry/p4');
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.ReopeningTheMenuForgetsTheLastHover;
var
    F: TExplanationFocus;
begin
    F := FocusMenuOpened(FocusRowSelected(FocusStart, 'row/1'));
    F := FocusMenuClosed(FocusMenuHovered(F, 'entry/p4'));
    F := FocusMenuOpened(F);
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.ARowWhoseTopicNoLongerResolvesShowsNothing;
var
    F: TExplanationFocus;
begin
    //  NOTHING STANDS IN FOR THE ROW. A curve type chosen earlier is not what
    //  the user is looking at now, and showing it would explain something the
    //  model may not even hold.
    F := FocusRowSelected(FocusCurveTypeSelected(FocusStart, 'type/g'),
        'gone/row');
    AssertEquals('', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.DeselectingTheRowShowsNothing;
var
    F: TExplanationFocus;
begin
    //  Nothing selected in the Model panel - an empty model, or a selection
    //  cleared - means nothing to explain, whatever type was chosen before.
    F := FocusRowSelected(FocusCurveTypeSelected(FocusStart, 'type/g'), 'row/1');
    F := FocusRowSelected(F, '');
    AssertEquals('', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.ACurveTypeIsShownOnlyWhenItWasTheLastThingChosen;
var
    F: TExplanationFocus;
begin
    //  A click on a type in the Tools list is a choice, so it is explained - and
    //  once a row is chosen after it, the type is not what the pane shows.
    F := FocusCurveTypeSelected(FocusStart, 'type/g');
    AssertEquals('clicked', 'type/g', FocusShownTopic(F, @OnlyKnown));
    F := FocusRowSelected(F, '');
    AssertEquals('then an empty selection', '', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.EveryEventOrderShowsOnlyWhatResolvesAndIsBlankOnlyWhenNothingApplies;
const
    EventCount = 11;
var
    Sequence: array[0..3] of longint;
    Length_, i, Total: longint;
    F: TExplanationFocus;
    Shown: string;
    AnyResolves: boolean;

    function Apply(const AFocus: TExplanationFocus;
        AEvent: longint): TExplanationFocus;
    begin
        case AEvent of
            0: Result := FocusRowSelected(AFocus, 'row/1');
            1: Result := FocusRowSelected(AFocus, 'gone/row');
            2: Result := FocusRowSelected(AFocus, '');
            3: Result := FocusCurveTypeSelected(AFocus, 'type/g');
            4: Result := FocusCurveTypeSelected(AFocus, 'gone/type');
            5: Result := FocusMenuOpened(AFocus);
            6: Result := FocusMenuHovered(AFocus, 'entry/p4');
            7: Result := FocusMenuHovered(AFocus, 'gone/entry');
            8: Result := FocusMenuClosed(AFocus);
            9: Result := FocusLinkFollowed(AFocus, 'link/rule');
            else Result := FocusLinkFollowed(AFocus, 'gone/link');
        end;
    end;

    procedure Check(ALength: longint);
    var
        k: longint;
        Description, Chosen: string;
    begin
        F := FocusStart;
        Description := '';
        for k := 0 to ALength - 1 do
        begin
            F := Apply(F, Sequence[k]);
            Description := Description + IntToStr(Sequence[k]) + ' ';
        end;
        Shown := FocusShownTopic(F, @OnlyKnown);
        //  What may be shown: a followed link, a hovered entry while the menu
        //  is open, or the one thing chosen last - never the other of the row
        //  and the curve type standing in for it.
        if F.LastSelected = lsCurveType then
            Chosen := F.CurveTypeTopic
        else
            Chosen := F.RowTopic;
        AnyResolves := OnlyKnown(Chosen) or
            (F.MenuOpen and OnlyKnown(F.HoverTopic)) or OnlyKnown(F.LinkTopic);
        if Shown <> '' then
            AssertTrue('shown but unresolvable after ' + Description,
                OnlyKnown(Shown));
        AssertEquals('blank exactly when nothing resolves, after ' +
            Description, not AnyResolves, Shown = '');
        Inc(Total);
    end;

    procedure Walk(ADepth, ALength: longint);
    var
        e: longint;
    begin
        if ADepth = ALength then
        begin
            Check(ALength);
            Exit;
        end;
        for e := 0 to EventCount - 1 do
        begin
            Sequence[ADepth] := e;
            Walk(ADepth + 1, ALength);
        end;
    end;

begin
    Total := 0;
    for Length_ := 0 to 4 do
        Walk(0, Length_);
    //  1 + 11 + 121 + 1331 + 14641: a walk that silently checked nothing would
    //  otherwise pass.
    i := 1 + 11 + 121 + 1331 + 14641;
    AssertEquals('every order was checked', i, Total);
end;

procedure TExplanationFocusTest.AFollowedLinkIsShown;
var
    F: TExplanationFocus;
begin
    F := FocusLinkFollowed(FocusRowSelected(FocusStart, 'row/1'), 'link/rule');
    AssertEquals('link/rule', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.AFollowedLinkWinsOverAHoverToo;
var
    F: TExplanationFocus;
begin
    //  A module that refuses a click asks for the rule to be shown while the
    //  pointer may still rest on the entry; what it asked for is the answer.
    F := FocusMenuHovered(FocusMenuOpened(FocusStart), 'entry/p4');
    F := FocusLinkFollowed(F, 'link/rule');
    AssertEquals('link/rule', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.AFollowedLinkThatDoesNotResolveKeepsWhatWasShown;
var
    F: TExplanationFocus;
begin
    F := FocusLinkFollowed(FocusRowSelected(FocusStart, 'row/1'), 'gone/link');
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.SelectingARowReplacesAFollowedLink;
var
    F: TExplanationFocus;
begin
    F := FocusLinkFollowed(FocusStart, 'link/rule');
    F := FocusRowSelected(F, 'row/2');
    AssertEquals('row/2', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.SelectingACurveTypeReplacesAFollowedLink;
var
    F: TExplanationFocus;
begin
    F := FocusLinkFollowed(FocusStart, 'link/rule');
    F := FocusCurveTypeSelected(F, 'type/g');
    AssertEquals('type/g', FocusShownTopic(F, @OnlyKnown));
end;

procedure TExplanationFocusTest.OpeningAMenuReplacesAFollowedLink;
var
    F: TExplanationFocus;
begin
    F := FocusLinkFollowed(FocusRowSelected(FocusStart, 'row/1'), 'link/rule');
    F := FocusMenuOpened(F);
    AssertEquals('row/1', FocusShownTopic(F, @OnlyKnown));
end;


initialization
    RegisterTest('unit', TExplanationFocusTest);
end.
