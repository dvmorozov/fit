// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The registry of entries a module adds to its own Model > Curve Type
group, and the formula a project records from them.)
}
unit testcase_curve_type_menu_extension;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_ui_host, named_points_set,
    curve_type_menu_extension;

type
    { An extension reporting a fixed formula, for a group no build shows - so
      registering it changes no window the suite builds. }
    TFixedExtension = class(TObject, ICurveTypeMenuExtension)
    private
        FFormula: string;
    public
        constructor Create(const AFormula: string);
        function Group: string;
        function MenuItems: TUiMenuDeclArray;
        procedure Command(const AId: string; AHost: IUiHost);
        procedure CurveTypeSelected(const ATypeId: TCurveTypeId);
        function SelectedFormula: string;
    end;

    TCurveTypeMenuExtensionTest = class(TTestCase)
    published
        procedure ANilExtensionIsRefused;
        procedure TheSameExtensionTwiceIsOnce;
        procedure TheFirstCurrentFormulaIsTheProjects;
        procedure NoCurrentFormulaIsNone;
    end;

implementation

var
    //  Registered once and kept: the registry has no way back, as the
    //  program's own registrations never leave.
    Registered: TFixedExtension;

constructor TFixedExtension.Create(const AFormula: string);
begin
    inherited Create;
    FFormula := AFormula;
end;

function TFixedExtension.Group: string;
begin
    Result := 'No build shows this group';
end;

function TFixedExtension.MenuItems: TUiMenuDeclArray;
begin
    Result := nil;
end;

procedure TFixedExtension.Command(const AId: string; AHost: IUiHost);
begin
end;

procedure TFixedExtension.CurveTypeSelected(const ATypeId: TCurveTypeId);
begin
end;

function TFixedExtension.SelectedFormula: string;
begin
    Result := FFormula;
end;

procedure TCurveTypeMenuExtensionTest.ANilExtensionIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterCurveTypeMenuExtension(nil);
    except
        on Exception do Raised := True;
    end;
    AssertTrue(Raised);
end;

procedure TCurveTypeMenuExtensionTest.TheSameExtensionTwiceIsOnce;
var
    Before, i, Seen: longint;
    All: TCurveTypeMenuExtensions;
begin
    if Registered = nil then
        Registered := TFixedExtension.Create('');
    RegisterCurveTypeMenuExtension(Registered);
    Before := Length(RegisteredCurveTypeMenuExtensions);
    RegisterCurveTypeMenuExtension(Registered);
    All := RegisteredCurveTypeMenuExtensions;
    AssertEquals(Before, Length(All));
    Seen := 0;
    for i := 0 to High(All) do
        if All[i] = ICurveTypeMenuExtension(Registered) then
            Inc(Seen);
    AssertEquals('listed once', 1, Seen);
end;

procedure TCurveTypeMenuExtensionTest.TheFirstCurrentFormulaIsTheProjects;
var
    None, First, Second: TFixedExtension;
    Exts: TCurveTypeMenuExtensions;
begin
    None := TFixedExtension.Create('');
    First := TFixedExtension.Create('A*x');
    Second := TFixedExtension.Create('B*x');
    try
        Exts := nil;
        SetLength(Exts, 3);
        Exts[0] := None;
        Exts[1] := First;
        Exts[2] := Second;
        AssertEquals('A*x', SelectedFormulaOf(Exts));
    finally
        Exts := nil;
        Second.Free;
        First.Free;
        None.Free;
    end;
end;

procedure TCurveTypeMenuExtensionTest.NoCurrentFormulaIsNone;
var
    None: TFixedExtension;
    Exts: TCurveTypeMenuExtensions;
begin
    AssertEquals('no extension', '', SelectedFormulaOf(nil));
    None := TFixedExtension.Create('');
    try
        Exts := nil;
        SetLength(Exts, 1);
        Exts[0] := None;
        AssertEquals('none current', '', SelectedFormulaOf(Exts));
    finally
        Exts := nil;
        None.Free;
    end;
end;

initialization
    RegisterTest('unit', TCurveTypeMenuExtensionTest);
finalization
    //  The registry still holds the interface; nothing reads it this late.
    Registered.Free;
end.
