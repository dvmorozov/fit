// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The saved formulas' gestures under Model > Curve Type > User, in
memory: the entries, choosing, deleting, and which one is in use.)

The same gestures testcase_user_formula_menu drives over a folder, kept here
as UNIT tests over a menu made with no folder (TUserFormulaMenu's seam), so
the unit half of the suite - the half coverage measures - reaches the logic
the window used to hold. The folder tests stay for the files themselves.
}
unit testcase_user_formula_gestures;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_ui_host, mock_ui_host,
    saved_formula, user_formula_menu, user_points_set, gauss_points_set;

type
    TUserFormulaGesturesTest = class(TTestCase)
    private
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FMenu: TUserFormulaMenu;
        function Saved(const AName, AExpression: string): Curve_type;
        function IdOf(const APrefix: string; ACurve: Curve_type): string;
        function IndexOf(const ACaption: string; AKind: TUiMenuKind): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheGroupIsTheUsersOwn;
        procedure NothingSavedIsNoEntry;
        procedure EachFormulaIsAnEntryUnderASeparatorAndADeleteEntry;
        procedure ChoosingOneSelectsItsFormulaAndTicksIt;
        procedure AFormulaWithoutAnExpressionIsRefusedInWords;
        procedure AStaleEntryMatchesNothing;
        procedure DeletingTheOneInUseForgetsItAndSaysWhyLater;
        procedure DeletingAnotherKeepsTheOneInUse;
        procedure ChoosingAFormulaTypeMakesTheLastSavedCurrent;
        procedure ChoosingAPeakLeavesNoneCurrent;
        procedure AFormulaTheDialogsRemoveIsGone;
        procedure AnUpdatedFormulaStaysListed;
    end;

implementation

procedure TUserFormulaGesturesTest.SetUp;
begin
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    //  No folder: the formulas are kept in memory only.
    FMenu := TUserFormulaMenu.Create('');
end;

procedure TUserFormulaGesturesTest.TearDown;
begin
    FreeAndNil(FMenu);
    FHost := nil;
    FreeAndNil(FHostObj);
end;

function TUserFormulaGesturesTest.Saved(const AName,
    AExpression: string): Curve_type;
begin
    Result := Curve_type.Create(nil);
    Result.Name := AName;
    Result.Expression := AExpression;
    FMenu.Add(Result);
end;

function TUserFormulaGesturesTest.IdOf(const APrefix: string;
    ACurve: Curve_type): string;
begin
    Result := APrefix + IntToStr(PtrInt(ACurve));
end;

function TUserFormulaGesturesTest.IndexOf(const ACaption: string;
    AKind: TUiMenuKind): longint;
var
    Items: TUiMenuDeclArray;
    i: longint;
begin
    Items := FMenu.MenuItems;
    for i := 0 to High(Items) do
        if (Items[i].Caption = ACaption) and (Items[i].Kind = AKind) then
            Exit(i);
    Result := -1;
end;

procedure TUserFormulaGesturesTest.TheGroupIsTheUsersOwn;
begin
    AssertEquals('User', FMenu.Group);
end;

procedure TUserFormulaGesturesTest.NothingSavedIsNoEntry;
begin
    AssertEquals(0, Length(FMenu.MenuItems));
    AssertEquals('none in use', '', FMenu.SelectedFormula);
end;

procedure TUserFormulaGesturesTest.EachFormulaIsAnEntryUnderASeparatorAndADeleteEntry;
var
    Items: TUiMenuDeclArray;
begin
    Saved('Bump', 'A*exp(-x*x)');
    Items := FMenu.MenuItems;
    AssertEquals('a separator first', Ord(mkSeparator), Ord(Items[0].Kind));
    AssertTrue('an entry that chooses it', IndexOf('Bump', mkToggle) >= 0);
    AssertTrue('the delete submenu', IndexOf(DeleteUserCurveCaption, mkSubmenu) >= 0);
    AssertTrue('an entry under it that deletes it', IndexOf('Bump', mkCommand) >= 0);
    AssertEquals('under the submenu',
        Items[IndexOf(DeleteUserCurveCaption, mkSubmenu)].Id,
        Items[IndexOf('Bump', mkCommand)].Parent);
end;

procedure TUserFormulaGesturesTest.ChoosingOneSelectsItsFormulaAndTicksIt;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    AssertEquals('the host builds the model from it', 'A*exp(-x*x)',
        FHostObj.Formula);
    AssertTrue('a copy of its parameters, the host''s from here',
        Assigned(FHostObj.FormulaParams) and
        (FHostObj.FormulaParams <> Bump.Parameters));
    AssertEquals('A*exp(-x*x)', FMenu.SelectedFormula);
    AssertTrue('the menu is rebuilt', FHostObj.Log.Saw('RefreshCurveTypeMenu'));
    AssertTrue('and its entry ticked',
        FMenu.MenuItems[IndexOf('Bump', mkToggle)].Checked);
end;

procedure TUserFormulaGesturesTest.AFormulaWithoutAnExpressionIsRefusedInWords;
var
    Empty: Curve_type;
begin
    //  Listed only through the list itself: a saved file without one is
    //  dropped when read (testcase_user_formula_menu).
    Saved('Bump', 'A*exp(-x*x)');
    Empty := Curve_type.Create(nil);
    Empty.Name := 'Empty';
    FMenu.Curves.Add(Empty);
    FMenu.Command(IdOf('select:', Empty), FHost);
    AssertFalse('nothing selected', FHostObj.Log.Saw('SelectFormula'));
    AssertTrue('said', FHostObj.Log.Saw('ShowMessage'));
end;

procedure TUserFormulaGesturesTest.AStaleEntryMatchesNothing;
begin
    Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command('select:12345', FHost);
    FMenu.Command('delete:12345', FHost);
    FMenu.Command('select:not-a-number', FHost);
    FMenu.Command('something-else', FHost);
    AssertFalse(FHostObj.Log.Saw('SelectFormula'));
    AssertEquals('nothing deleted', 1, FMenu.Curves.Count);
end;

procedure TUserFormulaGesturesTest.DeletingTheOneInUseForgetsItAndSaysWhyLater;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.Command(IdOf('delete:', Bump), FHost);
    AssertEquals('gone', 0, FMenu.Curves.Count);
    AssertTrue('the engine is told', FHostObj.Log.Saw('ForgetFormula'));
    AssertTrue('and the user, once the click is over',
        FHostObj.Log.Saw('ShowMessageLater'));
    AssertEquals('none in use', '', FMenu.SelectedFormula);
end;

procedure TUserFormulaGesturesTest.DeletingAnotherKeepsTheOneInUse;
var
    Bump, Step: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    Step := Saved('Step', 'A*(x>0)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.Command(IdOf('delete:', Step), FHost);
    AssertFalse(FHostObj.Log.Saw('ForgetFormula'));
    AssertEquals('A*exp(-x*x)', FMenu.SelectedFormula);
end;

procedure TUserFormulaGesturesTest.ChoosingAFormulaTypeMakesTheLastSavedCurrent;
begin
    Saved('Bump', 'A*exp(-x*x)');
    Saved('Step', 'A*(x>0)');
    FMenu.CurveTypeSelected(TUserPointsSet.GetCurveTypeId);
    AssertEquals('A*(x>0)', FMenu.SelectedFormula);
    AssertTrue(FMenu.Selected <> nil);
end;

procedure TUserFormulaGesturesTest.ChoosingAPeakLeavesNoneCurrent;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.CurveTypeSelected(TGaussPointsSet.GetCurveTypeId);
    AssertEquals('', FMenu.SelectedFormula);
end;

procedure TUserFormulaGesturesTest.AFormulaTheDialogsRemoveIsGone;
var
    Bump: Curve_type;
begin
    //  What the definition dialogs do when the user rejects the roles of a
    //  formula they had just saved.
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.Remove(Bump);
    AssertEquals(0, FMenu.Curves.Count);
    AssertEquals('', FMenu.SelectedFormula);
end;

procedure TUserFormulaGesturesTest.AnUpdatedFormulaStaysListed;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    Bump.Expression := 'A*exp(-x*x/2)';
    FMenu.Update(Bump);
    AssertEquals(1, FMenu.Curves.Count);
    AssertEquals('A*exp(-x*x/2)', Curve_type(FMenu.Curves[0]).Expression);
end;

initialization
    RegisterTest('unit', TUserFormulaGesturesTest);
end.
