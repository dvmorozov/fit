// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What choosing a curve type from the menu means.)

THE CASE THAT MATTERS IS THE ONE THE USER MEETS AND CANNOT EXPLAIN: they pick
"user-defined curve", change their mind in the dialog, and the menu does nothing.
Whether that is correct depends on something invisible - whether the type can
supply its own default values - and the old code expressed the whole rule as a
nested if/else with an empty branch and a Break in the middle of a while-true
loop, inside an LCL action handler. Nothing could reach it.

The other claim worth pinning is the tick: configuring a user-defined type CREATES
a curve, so afterwards it is that curve which is selected and not the menu item
that was clicked - while any other type must CLEAR that selection. A stale tick
there is a curve the user believes is being fitted and which is not.
}
unit testcase_curve_type_choice;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    named_points_set, int_curve_factory, int_curve_type_iterator,
    curve_types_singleton, curve_type_choice;

type
    { An iterator over nothing, for the answers that must hold when the registry
      cannot help: a tag from a menu strip that has since been rebuilt. }
    TEmptyIterator = class(TObject, ICurveTypeIterator)
    public
        procedure FirstCurveType;
        procedure NextCurveType;
        function EndCurveType: boolean;
        function GetCurveTypeName: string;
        function GetCurveTypeId: TCurveTypeId;
        function GetCurveTypeTag(CurveTypeId: TCurveTypeId): integer;
        function GetCurrentCurveClass: TCurveClass;
    end;

    TCurveTypeChoiceTest = class(TTestCase)
    private
        FEmptyObj: TEmptyIterator;
        FEmpty: ICurveTypeIterator;
        { The real registry, so the tag mapping under test is the one the menu
          is built from rather than a restatement of it. }
        function Registry: ICurveTypeIterator;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EveryRegisteredTypeIsFoundByItsOwnTag;
        procedure AnUnknownTagFindsNothing;
        procedure NoIteratorFindsNothing;
        procedure AnEmptyRegistryFindsNothing;
        procedure ATypeWithNothingToConfigureIsSelected;
        procedure AConfirmedDialogSelectsTheType;
        procedure CancellingFallsBackToDefaultsWhenThereAreSome;
        procedure CancellingWithNoDefaultsRefusesTheType;
        procedure ADialogAnswerIsIgnoredWhereThereIsNoDialog;
        procedure OnlyTheUserDefinedTypeLeavesAUserCurveSelected;
        procedure TheRefusalNamesTheMenuEntryToComeBackThrough;
        procedure TheRefusalStillSaysWhatToDoWithoutACaption;
    end;

implementation

{ ---- the empty iterator ---------------------------------------------------- }

procedure TEmptyIterator.FirstCurveType;
begin
end;

procedure TEmptyIterator.NextCurveType;
begin
end;

function TEmptyIterator.EndCurveType: boolean;
begin
    Result := True;
end;

function TEmptyIterator.GetCurveTypeName: string;
begin
    Result := '';
end;

function TEmptyIterator.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
end;

function TEmptyIterator.GetCurveTypeTag(CurveTypeId: TCurveTypeId): integer;
begin
    Result := 0;
end;

function TEmptyIterator.GetCurrentCurveClass: TCurveClass;
begin
    Result := nil;
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TCurveTypeChoiceTest.SetUp;
begin
    FEmptyObj := TEmptyIterator.Create;
    FEmpty := FEmptyObj;
end;

procedure TCurveTypeChoiceTest.TearDown;
begin
    //  The interface goes first: corba interfaces carry no refcount, so a live
    //  reference over a freed object is a use-after-free that happens to work.
    FEmpty := nil;
    FreeAndNil(FEmptyObj);
end;

function TCurveTypeChoiceTest.Registry: ICurveTypeIterator;
begin
    Result := TCurveTypesSingleton.CreateCurveTypeIterator;
end;

{ ---- which type was clicked ------------------------------------------------ }

procedure TCurveTypeChoiceTest.EveryRegisteredTypeIsFoundByItsOwnTag;
var
    It: ICurveTypeIterator;
    Cls: TCurveClass;
    Found: TCurveClass;
    Seen: longint;
begin
    //  A SWEEP OVER WHATEVER IS REGISTERED, so this holds for the pack's types
    //  as well as the framework's - the menu is built from the same walk, and a
    //  type whose tag does not lead back to it is an entry that does nothing.
    It := Registry;
    Seen := 0;
    It.FirstCurveType;
    while True do
    begin
        Cls := It.GetCurrentCurveClass;
        AssertNotNull('a registered type with no class', Cls);
        Found := CurveClassForMenuTag(Registry,
            It.GetCurveTypeTag(Cls.GetCurveTypeId));
        AssertTrue('the tag of ' + Cls.ClassName + ' leads back to it',
            Found = Cls);
        Inc(Seen);
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    end;
    AssertTrue('the registry is not empty, or this proves nothing', Seen > 0);
end;

procedure TCurveTypeChoiceTest.AnUnknownTagFindsNothing;
begin
    //  Tags are hashes of the type id, so this is not a boundary - it is simply
    //  a tag no type claims. The menu can deliver one: a click lands on the
    //  strip as it was before a rebuild.
    AssertTrue('nothing claims it',
        CurveClassForMenuTag(Registry, -12345) = nil);
end;

procedure TCurveTypeChoiceTest.NoIteratorFindsNothing;
begin
    //  Answered rather than faulted, because the caller's next line tests the
    //  result for nil anyway.
    AssertTrue('no registry, no type', CurveClassForMenuTag(nil, 0) = nil);
end;

procedure TCurveTypeChoiceTest.AnEmptyRegistryFindsNothing;
begin
    //  An iterator that is done before it starts must not be walked past its
    //  end: the loop asks for the current class first, so this is the case that
    //  would read one type too many.
    AssertTrue('nothing to find', CurveClassForMenuTag(FEmpty, 0) = nil);
end;

{ ---- may the type be selected --------------------------------------------- }

procedure TCurveTypeChoiceTest.ATypeWithNothingToConfigureIsSelected;
begin
    //  Every built-in type. The dialog and defaults answers are whatever they
    //  happen to be - with no parameters there is nothing to ask about.
    AssertTrue('selected', CurveSetupOutcome(False, False, False) = csoSelect);
    AssertTrue('selected', CurveSetupOutcome(False, True, True) = csoSelect);
end;

procedure TCurveTypeChoiceTest.AConfirmedDialogSelectsTheType;
begin
    AssertTrue('the user supplied the values',
        CurveSetupOutcome(True, True, False) = csoSelect);
end;

procedure TCurveTypeChoiceTest.CancellingFallsBackToDefaultsWhenThereAreSome;
begin
    //  NOT csoSelect: the defaults have to be applied first, because selecting
    //  the type is what builds a curve and a curve built from unset parameters
    //  is the defect the distinction exists to prevent.
    AssertTrue('defaults, then select',
        CurveSetupOutcome(True, False, True) = csoApplyDefaultsThenSelect);
end;

procedure TCurveTypeChoiceTest.CancellingWithNoDefaultsRefusesTheType;
begin
    //  THE CASE THE USER MEETS. Nothing is selected, and the caller owes them a
    //  message - without one the menu simply appears broken.
    AssertTrue('refused', CurveSetupOutcome(True, False, False) = csoRefuse);
end;

procedure TCurveTypeChoiceTest.ADialogAnswerIsIgnoredWhereThereIsNoDialog;
begin
    //  A type with no parameters is never asked, so a stale "cancelled" from a
    //  previous dialog cannot refuse it. This is the combination the nested
    //  if/else handled by accident rather than by intent.
    AssertTrue('no parameters, no dialog, no refusal',
        CurveSetupOutcome(False, False, False) <> csoRefuse);
end;

{ ---- what stays ticked ----------------------------------------------------- }

procedure TCurveTypeChoiceTest.OnlyTheUserDefinedTypeLeavesAUserCurveSelected;
var
    UserId, OtherId: TCurveTypeId;
begin
    UserId := StringToGUID('{6A1C8B62-4C3E-4E1A-9F2D-0B7A5C1D3E48}');
    OtherId := StringToGUID('{11111111-2222-3333-4444-555555555555}');
    AssertTrue('the user-defined type keeps its curve',
        SelectionLeavesUserCurve(UserId, UserId));
    //  AND THE OTHER WAY ROUND IS THE BUG WORTH PINNING: any other type must
    //  clear the selection, or the menu shows a tick beside a curve the fit is
    //  not using.
    AssertTrue('anything else clears it',
        not SelectionLeavesUserCurve(OtherId, UserId));
end;

{ ---- what the user is told ------------------------------------------------- }

procedure TCurveTypeChoiceTest.TheRefusalNamesTheMenuEntryToComeBackThrough;
var
    Msg: string;
begin
    Msg := CurveSetupWasCancelled('New user curve');
    AssertTrue('it says nothing was created',
        Pos('not created', Msg) > 0);
    AssertTrue('it names the way back', Pos('New user curve', Msg) > 0);
    //  An example, because "a formula in terms of x" is not enough to act on if
    //  you have never seen one.
    AssertTrue('and shows what a formula looks like', Pos('exp', Msg) > 0);
end;

procedure TCurveTypeChoiceTest.TheRefusalStillSaysWhatToDoWithoutACaption;
var
    Msg: string;
begin
    //  The caption comes from a menu item the caller looks up, and that lookup
    //  can fail - the strip is rebuilt around this very action. The message must
    //  not degrade into naming an empty string.
    Msg := CurveSetupWasCancelled('');
    AssertTrue('no empty quotes', Pos('""', Msg) = 0);
    AssertTrue('still says what to choose', Pos('choose', Msg) > 0);
end;

initialization
    RegisterTest('unit', TCurveTypeChoiceTest);
end.
