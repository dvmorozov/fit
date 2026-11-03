// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The formulas a user saved, as Model > Curve Type > User lists them:
read and written one file each, chosen, deleted, and which one is in use.)

THE WINDOW DID ALL OF THIS UNTIL 2026-10-17, where no test could reach it:
reading the files, building the entries, and what a click on one meant. It is
the scientific module's now (user_formula_menu), declared through the window's
seam (curve_type_menu_extension) and acting through the host it is handed -
so each gesture is driven here against a mock host, over a folder of its own.
}
unit testcase_user_formula_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_ui_host, mock_ui_host,
    saved_formula, user_formula_menu, curve_types_singleton,
    persistent_curve_parameters, user_points_set;

type
    TUserFormulaMenuTest = class(TTestCase)
    private
        FDir: string;
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FMenu: TUserFormulaMenu;
        function Saved(const AName, AExpression: string): Curve_type;
        function IdOf(const APrefix: string; ACurve: Curve_type): string;
        function EntryCaptioned(const ACaption: string;
            AKind: TUiMenuKind): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure NoFormulaSavedAddsNoEntry;
        procedure EachFormulaIsAnEntryAndADeleteEntry;
        procedure ASavedFormulaIsReadBackFromItsFile;
        procedure AFormulaWithNoExpressionIsDroppedWithItsFile;
        procedure ChoosingOneSelectsItsFormulaAndTicksIt;
        procedure AnEntryFromAListThatChangedMatchesNothing;
        procedure DeletingTheOneInUseForgetsItAndSaysWhy;
        procedure DeletingAnotherKeepsTheOneInUse;
        procedure ChoosingTheFormulaTypeMakesTheLastCreatedCurrent;
        procedure ChoosingAnyOtherTypeLeavesNoneCurrent;
    end;

implementation

procedure TUserFormulaMenuTest.SetUp;
begin
    FDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
        'fit-formulas-' + IntToStr(Random(MaxInt));
    ForceDirectories(FDir);
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    FMenu := TUserFormulaMenu.Create(FDir);
end;

procedure TUserFormulaMenuTest.TearDown;
var
    R: TSearchRec;
begin
    FreeAndNil(FMenu);
    FHost := nil;
    FreeAndNil(FHostObj);
    if FindFirst(IncludeTrailingPathDelimiter(FDir) + '*', faAnyFile, R) = 0 then
    begin
        repeat
            if (R.Attr and faDirectory) = 0 then
                DeleteFile(IncludeTrailingPathDelimiter(FDir) + R.Name);
        until FindNext(R) <> 0;
        FindClose(R);
    end;
    RemoveDir(FDir);
end;

function TUserFormulaMenuTest.Saved(const AName, AExpression: string): Curve_type;
begin
    Result := Curve_type.Create(nil);
    Result.Name := AName;
    Result.Expression := AExpression;
    FMenu.Add(Result);
    //  Distinct file names: they are stamped from the clock.
    Sleep(5);
end;

function TUserFormulaMenuTest.IdOf(const APrefix: string;
    ACurve: Curve_type): string;
begin
    Result := APrefix + IntToStr(PtrInt(ACurve));
end;

function TUserFormulaMenuTest.EntryCaptioned(const ACaption: string;
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

procedure TUserFormulaMenuTest.NoFormulaSavedAddsNoEntry;
begin
    AssertEquals(0, Length(FMenu.MenuItems));
    AssertEquals('the group it adds to', 'User', FMenu.Group);
end;

procedure TUserFormulaMenuTest.EachFormulaIsAnEntryAndADeleteEntry;
var
    Items: TUiMenuDeclArray;
    i, Choose, Delete_, Submenu: longint;
begin
    Saved('Bump', 'A*exp(-x*x)');
    Items := FMenu.MenuItems;
    Choose := -1;
    Delete_ := -1;
    Submenu := -1;
    for i := 0 to High(Items) do
        if Items[i].Kind = mkSubmenu then
            Submenu := i
        else if (Items[i].Caption = 'Bump') and (Items[i].Parent = '') then
            Choose := i
        else if Items[i].Caption = 'Bump' then
            Delete_ := i;
    AssertTrue('an entry that chooses it', Choose >= 0);
    AssertTrue('ticked only while in use', not Items[Choose].Checked);
    AssertTrue('the delete submenu', Submenu >= 0);
    AssertEquals(DeleteUserCurveCaption, Items[Submenu].Caption);
    AssertTrue('an entry under it that deletes it', Delete_ >= 0);
    AssertEquals(Items[Submenu].Id, Items[Delete_].Parent);
end;

procedure TUserFormulaMenuTest.ASavedFormulaIsReadBackFromItsFile;
var
    Again: TUserFormulaMenu;
begin
    Saved('Bump', 'A*exp(-x*x)');
    Again := TUserFormulaMenu.Create(FDir);
    try
        AssertEquals('one file, one formula', 1, Again.Curves.Count);
        AssertEquals('Bump', Curve_type(Again.Curves[0]).Name);
        AssertEquals('A*exp(-x*x)', Curve_type(Again.Curves[0]).Expression);
    finally
        Again.Free;
    end;
end;

procedure TUserFormulaMenuTest.AFormulaWithNoExpressionIsDroppedWithItsFile;
var
    Again: TUserFormulaMenu;
    R: TSearchRec;
    Files: longint;
begin
    //  Written by an older build: a formula that can never be selected.
    Saved('Empty', '');
    Again := TUserFormulaMenu.Create(FDir);
    try
        AssertEquals('not listed', 0, Again.Curves.Count);
    finally
        Again.Free;
    end;
    Files := 0;
    if FindFirst(IncludeTrailingPathDelimiter(FDir) + '*.cpr', faAnyFile, R) = 0 then
    begin
        repeat
            Inc(Files);
        until FindNext(R) <> 0;
        FindClose(R);
    end;
    AssertEquals('and its stale file is gone', 0, Files);
end;

procedure TUserFormulaMenuTest.ChoosingOneSelectsItsFormulaAndTicksIt;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    AssertEquals('the host builds the model from it', 'A*exp(-x*x)',
        FHostObj.Formula);
    AssertTrue('with its parameters, a copy the host owns',
        Assigned(FHostObj.FormulaParams) and
        (FHostObj.FormulaParams <> Bump.Parameters));
    AssertEquals('the formula in use', 'A*exp(-x*x)', FMenu.SelectedFormula);
    AssertTrue('the menu is rebuilt so the tick moves',
        FHostObj.Log.Saw('RefreshCurveTypeMenu'));
    AssertTrue('and its entry is ticked',
        FMenu.MenuItems[EntryCaptioned('Bump', mkToggle)].Checked);
end;

procedure TUserFormulaMenuTest.AnEntryFromAListThatChangedMatchesNothing;
begin
    Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command('select:12345', FHost);
    AssertFalse(FHostObj.Log.Saw('SelectFormula'));
    FMenu.Command('delete:12345', FHost);
    AssertEquals('nothing deleted', 1, FMenu.Curves.Count);
end;

procedure TUserFormulaMenuTest.DeletingTheOneInUseForgetsItAndSaysWhy;
var
    Bump: Curve_type;
    FileName: string;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FileName := Bump.FFileName;
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.Command(IdOf('delete:', Bump), FHost);
    AssertEquals('gone from the list', 0, FMenu.Curves.Count);
    AssertFalse('and its file', FileExists(FileName));
    AssertTrue('the engine is told the formula is gone',
        FHostObj.Log.Saw('ForgetFormula'));
    AssertTrue('and the user why, once the click is over',
        FHostObj.Log.Saw('ShowMessageLater'));
    AssertEquals('none in use', '', FMenu.SelectedFormula);
end;

procedure TUserFormulaMenuTest.DeletingAnotherKeepsTheOneInUse;
var
    Bump, Step: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    Step := Saved('Step', 'A*(x>0)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.Command(IdOf('delete:', Step), FHost);
    AssertFalse('nothing to forget', FHostObj.Log.Saw('ForgetFormula'));
    AssertEquals('A*exp(-x*x)', FMenu.SelectedFormula);
end;

procedure TUserFormulaMenuTest.ChoosingTheFormulaTypeMakesTheLastCreatedCurrent;
begin
    Saved('Bump', 'A*exp(-x*x)');
    Saved('Step', 'A*(x>0)');
    //  New User Curve ran its dialogs, which saved the second; selecting the
    //  formula type is what the window reports next.
    FMenu.CurveTypeSelected(TUserPointsSet.GetCurveTypeId);
    AssertEquals('A*(x>0)', FMenu.SelectedFormula);
end;

procedure TUserFormulaMenuTest.ChoosingAnyOtherTypeLeavesNoneCurrent;
var
    Bump: Curve_type;
begin
    Bump := Saved('Bump', 'A*exp(-x*x)');
    FMenu.Command(IdOf('select:', Bump), FHost);
    FMenu.CurveTypeSelected(StringToGUID('{00000000-0000-0000-0000-0000000000AB}'));
    AssertEquals('', FMenu.SelectedFormula);
end;

initialization
    RegisterTest('integration', TUserFormulaMenuTest);
end.
