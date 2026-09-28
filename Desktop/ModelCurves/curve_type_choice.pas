// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What choosing a curve type from the menu means.)

ONE MENU CLICK, FOUR DECISIONS, ALL OF THEM INSIDE A WHILE-TRUE LOOP INSIDE AN
LCL EVENT HANDLER (TFormMain.ActionSelCurveExecute):

  1. WHICH type was clicked. The menu carries a tag, the registry knows which
     type each tag belongs to, and the loop compared them.

  2. WHETHER THE TYPE MAY BE SELECTED AT ALL. A type with parameters the user has
     to supply - the user-defined curve is the one that exists - offers a dialog.
     If the dialog is cancelled the type may still be selectable, but only if it
     has defaults to fall back on; with neither, choosing it must do NOTHING, and
     the user has to be told why or the menu simply appears not to work.

  3. WHAT THE MENU NOW SHOWS AS SELECTED. Configuring a user-defined type CREATES
     a curve, and it is that curve, not the item that was clicked, which is
     active afterwards. Every other type is a curve in itself and leaves no user
     curve selected - a stale one there is a tick beside a curve the fit is not
     using.

  4. WHETHER THE AXIS HAS TO FOLLOW. A peak is fitted against a scattering angle,
     a wave pattern against a plain position, so the abscissa belongs to the
     type.

Only the third and fourth were reachable by a test, and only indirectly. What is
left in the form after this unit is the parts that are genuinely the window's: it
opens the dialog, shows the message, and re-selects on the server.

WHY THE OUTCOME IS A VALUE AND NOT A BOOLEAN. The old code expressed decision 2
as a nested if/else with an empty branch and a Break in the middle of a loop,
which is why the "cancelled, but there are defaults" case reads as an accident.
There are three outcomes and they are different actions: select it, apply the
defaults and select it, or refuse and explain. Naming them is what makes the
missing fourth combination - no parameters at all, so nothing to cancel -
obviously not a case rather than accidentally handled.
}
unit curve_type_choice;

{$mode objfpc}{$H+}

interface

uses
    named_points_set, int_curve_factory, int_curve_type_iterator;

type
    { What choosing a type should lead to, once the type has had its chance to
      ask the user for what it needs. }
    TCurveSetupOutcome = (
        { Nothing was needed, or the user supplied it: select the type. }
        csoSelect,
        { The user backed out, but the type can stand on its default values.
          They must be applied BEFORE the type is selected: selecting it is what
          builds a curve, and a curve built from unset parameters is the defect
          this ordering exists to prevent. }
        csoApplyDefaultsThenSelect,
        { The user backed out and there is nothing to fall back on. The type is
          not selected and the user is told - see CURVE_SETUP_WAS_CANCELLED. }
        csoRefuse
    );

{ Which registered curve type a menu item stands for, or nil.

  NIL IS A REAL ANSWER, not a "cannot happen": the menu is rebuilt whenever the
  registry changes, and a click can be delivered against the strip as it was
  before the rebuild. The loop this replaces ran to the end of the registry and
  then simply did nothing, which is the same outcome reached without saying so. }
function CurveClassForMenuTag(AIterator: ICurveTypeIterator;
    ATag: integer): TCurveClass;

{ What to do about a type whose parameters the user was offered.

  AHasParameters is the type's own answer to whether it has any; ADialogAccepted
  is what the user did with the dialog, and is not consulted unless there was one;
  AHasDefaults says whether backing out still leaves the type usable. }
function CurveSetupOutcome(AHasParameters, ADialogAccepted,
    AHasDefaults: boolean): TCurveSetupOutcome;

{ Whether choosing this type leaves a user-defined curve selected.

  The user-defined type is the only one whose configuration produces a curve, so
  it is the only one that does. Anything else must clear the selection rather
  than leave the previous type's curve ticked. }
function SelectionLeavesUserCurve(const ATypeId,
    AUserCurveTypeId: TCurveTypeId): boolean;

{ Why nothing happened, for the user who chose a type and got no curve.

  The text names the menu entry to come back through, because a message that says
  only "cancelled" leaves the user with a menu that appears not to work. The
  caller supplies the entry's caption, which is the window's to know. }
function CurveSetupWasCancelled(const AMenuCaption: string): string;

implementation

uses
    SysUtils;

function CurveClassForMenuTag(AIterator: ICurveTypeIterator;
    ATag: integer): TCurveClass;
var
    Cls: TCurveClass;
begin
    Result := nil;
    if AIterator = nil then
        Exit;

    //  Walked rather than indexed, and the tag is asked of the registry for each
    //  type: a new curve type is then picked up with no list to maintain here.
    AIterator.FirstCurveType;
    while True do
    begin
        Cls := AIterator.GetCurrentCurveClass;
        if Cls <> nil then
            if AIterator.GetCurveTypeTag(Cls.GetCurveTypeId) = ATag then
            begin
                Result := Cls;
                Exit;
            end;
        if AIterator.EndCurveType then
            Exit;
        AIterator.NextCurveType;
    end;
end;

function CurveSetupOutcome(AHasParameters, ADialogAccepted,
    AHasDefaults: boolean): TCurveSetupOutcome;
begin
    //  Nothing to configure: the commonest case by far, and it asks the user
    //  nothing at all.
    if not AHasParameters then
    begin
        Result := csoSelect;
        Exit;
    end;

    //  The user filled the dialog in.
    if ADialogAccepted then
    begin
        Result := csoSelect;
        Exit;
    end;

    //  Backed out. Usable only if the type says it can supply its own values.
    if AHasDefaults then
        Result := csoApplyDefaultsThenSelect
    else
        Result := csoRefuse;
end;

function SelectionLeavesUserCurve(const ATypeId,
    AUserCurveTypeId: TCurveTypeId): boolean;
begin
    Result := IsEqualGUID(ATypeId, AUserCurveTypeId);
end;

function CurveSetupWasCancelled(const AMenuCaption: string): string;
begin
    Result := 'Setup was cancelled, so the user-defined curve type was not ' +
        'created and has not been selected.' + LineEnding + LineEnding;
    if AMenuCaption <> '' then
        Result := Result + 'To create one, choose "' + AMenuCaption +
            '" again, then enter a name and a formula in terms of x'
    else
        //  Without a caption the message still has to say what to do; the entry
        //  is described rather than named.
        Result := Result + 'To create one, choose the new user-defined curve ' +
            'entry again, then enter a name and a formula in terms of x';
    Result := Result + ' - for example:' + LineEnding +
        '    A*exp(-sqr((x-x0)/w))';
end;

end.
