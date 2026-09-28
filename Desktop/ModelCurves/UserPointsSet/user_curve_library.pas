// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user's own curve types, as a library that can be asked questions.)

THE SAME SEARCH, WRITTEN OUT THREE TIMES, in three menu handlers. Each walked
FSettings.Curve_types comparing PtrInt(ct) against the clicked item's Tag; each
carried its own copy of the reason that comparison is by identity; and none of
them could be reached by a test, because reaching them meant clicking a menu item
in a window. One of the three has a subtly different loop shape from the other
two, which is what three copies of a search always come to.

WHAT IS HERE, and why each is a rule rather than plumbing:

  * WHICH CURVE A MENU ITEM STANDS FOR. Compared by the object's own address, so a
    list that changed since the menu was built fails to match instead of selecting
    whichever curve now sits at that position. That is a deliberate choice with a
    failure mode - the wrong curve silently selected - and it is worth a test.

  * WHICH CURVE IS "THE LAST ONE". Asked after the definition dialogs report
    having created one, to find what they made. The rule is the LAST entry that is
    not the placeholder, and it is not the same thing as the last entry.

  * WHAT DELETING ONE MEANS FOR THE SELECTION. Deleting the curve currently being
    fitted leaves the model naming a type that exists nowhere in the menu; the
    formula lives on the compute server, which knows nothing about the deletion,
    so unless it is told the next fit quietly produces more curves of the type
    just deleted. The user then sees "User Defined" curves and no "User Defined"
    anything to explain them.

  * WHAT THE USER IS TOLD in that case and when a saved curve has no formula.
    Both messages name the menu path back, because a message that only says what
    went wrong leaves the user with a menu that appears broken.

WHAT IS NOT HERE: the deletion itself, the file removal, and the menu rebuild.
Those are the window's, and they are what remains in the handlers.
}
unit user_curve_library;

{$mode objfpc}{$H+}

interface

uses
    Contnrs, app_settings;

const
    { The placeholder a very old version of this program wrote into an otherwise
      empty settings file, because reading an empty list back raised SIGSEGV
      inside the XML library. Nothing creates it any more - the code that did is
      commented out in the form - but a settings file written by that version
      still holds one, so it is still filtered out of "the last curve". Named
      here so the two places that care spell it the same way. }
    DUMMY_CURVE_NAME = 'Dummy';

{ Which stored curve type a menu item stands for, or nil.

  BY IDENTITY, not by position: the tag holds the object's own address. A menu
  built against a list that has since changed then matches nothing rather than
  matching whatever now sits where the curve used to be. Nil is a normal answer
  and the callers test for it. }
function CurveWithTag(AList: TComponentList; ATag: PtrInt): Curve_type;

{ The most recently created stored curve, or nil if there is none.

  THE LAST ONE THAT IS NOT THE PLACEHOLDER. Asked right after the definition
  dialogs report success, to find out what they made - so answering with the
  placeholder would select a curve whose formula is "1.0+1.0". }
function LastCreatedCurve(AList: TComponentList): Curve_type;

{ What the selection should be after ADeleted is removed.

  Nil when the deleted curve was the selected one - a selection pointing at a
  freed object is worse than none - and otherwise unchanged. }
function SelectionAfterDeleting(ASelected, ADeleted: Curve_type): Curve_type;

{ Whether deleting ADeleted also takes the model's curve type away with it. }
function DeletingLeavesTheModelWithoutACurveType(ASelected,
    ADeleted: Curve_type): boolean;

{ What to tell the user who deleted the curve type being fitted.

  Says what happened, what to do, and what the curves already on the chart are -
  they were built from the deleted formula and stay until the next fit, which
  looks like the deletion not having worked. }
function DeletedFittedCurveNotice(const AName: string): string;

{ What to tell the user who selected a stored curve that has no formula.

  A curve saved without one is an entry that cannot become a curve; selecting it
  used to fail an assertion inside the optimiser instead of being explained here.
  The menu path is named because the only thing to do about it is delete it. }
function UnusableCurveNotice(const AGroup, ADeleteEntry: string): string;

implementation

uses
    SysUtils;

function CurveWithTag(AList: TComponentList; ATag: PtrInt): Curve_type;
var
    i: longint;
    ct: Curve_type;
begin
    Result := nil;
    if AList = nil then
        Exit;
    //  A tag of zero is no curve: it is what an item that was never given one
    //  carries, and PtrInt(nil) can never be a live object either.
    if ATag = 0 then
        Exit;
    for i := 0 to AList.Count - 1 do
    begin
        ct := Curve_type(AList.Items[i]);
        if PtrInt(ct) = ATag then
        begin
            Result := ct;
            Exit;
        end;
    end;
end;

function LastCreatedCurve(AList: TComponentList): Curve_type;
var
    i: longint;
    ct: Curve_type;
begin
    Result := nil;
    if AList = nil then
        Exit;
    //  Walked forwards and overwritten rather than walked backwards, so that
    //  "last" means last in creation order even where the placeholder sits at
    //  the end.
    for i := 0 to AList.Count - 1 do
    begin
        ct := Curve_type(AList.Items[i]);
        if ct.Name <> DUMMY_CURVE_NAME then
            Result := ct;
    end;
end;

function SelectionAfterDeleting(ASelected, ADeleted: Curve_type): Curve_type;
begin
    if ASelected = ADeleted then
        Result := nil
    else
        Result := ASelected;
end;

function DeletingLeavesTheModelWithoutACurveType(ASelected,
    ADeleted: Curve_type): boolean;
begin
    //  Only when the deleted one was the one being fitted. Deleting any other
    //  leaves the model exactly as it was.
    Result := (ADeleted <> nil) and (ASelected = ADeleted);
end;

function DeletedFittedCurveNotice(const AName: string): string;
begin
    Result := 'The curve type "' + AName + '" was the one being fitted, so ' +
        'the model has no curve type now.' + LineEnding + LineEnding +
        'Select one in Model \ Curve Type before fitting again. Curves ' +
        'already on the chart were built from the deleted formula and stay ' +
        'until the next fit.';
end;

function UnusableCurveNotice(const AGroup, ADeleteEntry: string): string;
begin
    Result := 'This curve type has no formula and cannot be used. Delete it ' +
        '(Model \ Curve Type \ ' + AGroup + ' \ ' + ADeleteEntry +
        ') and create it again.';
end;

end.
