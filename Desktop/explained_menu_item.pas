// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A menu entry that says which explanation it stands for when the
pointer rests on it.)

WHY A SUBCLASS. The LCL tells an application that the pointer rests on a menu
entry in exactly one way: TMenuItem.IntfDoSelect, which the widget set calls and
which does nothing but copy the entry's hint into Application.Hint. That names
no entry, so the window could not tell WHICH entry is under the pointer - only
what its hint says. IntfDoSelect is virtual, so an entry that knows its own topic
can report it.

WHERE IT FIRES, verified in the Lazarus sources rather than assumed:
  * Qt5/Qt6: TQtMenu.SlotHovered, hooked to QAction's hovered signal.
  * win32:   the WM_MENUSELECT handler.
  * Cocoa:   NOWHERE - no menu delegate reports highlighting.
So on macOS the pane cannot follow the pointer through a menu at all, which is
why a module's rows carry topics explaining everything its context menu offers
over them, and why a prohibited entry's caption carries its reason as well.

The hint still reaches the status bar: the inherited behaviour runs first.

NOT UNIT-TESTED, AND LISTED AS A UI WRAPPER. TMenuItem's constructor reaches the
widget set, which the test suite never initialises, so constructing one faults
at address 0 (testcase_ui_menus records the same obstacle). A test was written
and could not run. What is left here is one forwarding call; which topic the pane
then shows is explanation_focus's decision, and that is tested exhaustively.
}
unit explained_menu_item;

{$mode objfpc}{$H+}

interface

uses
    Classes, Menus;

type
    { Told the topic of the entry under the pointer; '' for an entry with none. }
    TMenuTopicEvent = procedure(const ATopic: string) of object;

    TExplainedMenuItem = class(TMenuItem)
    private
        FTopic: string;
        FOnTopicHovered: TMenuTopicEvent;
    public
        procedure IntfDoSelect; override;
        { The explanation this entry stands for, or ''. }
        property Topic: string read FTopic write FTopic;
        property OnTopicHovered: TMenuTopicEvent read FOnTopicHovered
            write FOnTopicHovered;
    end;

implementation

procedure TExplainedMenuItem.IntfDoSelect;
begin
    inherited IntfDoSelect;
    //  AN ENTRY WITH NO TOPIC IS REPORTED TOO, as ''. Hovering a separator or a
    //  plain entry after an explained one must not leave the pane describing
    //  the entry the pointer has left.
    if Assigned(FOnTopicHovered) then
        FOnTopicHovered(FTopic);
end;

end.
