// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class-adapter for ICurveTypeStorage.)

Copyright (C) Dmitry Morozov
}
unit curve_type_storage_adapter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    saved_formula, int_curve_type_storage, SysUtils;

type
    { Class-adapter implementing basic operation for 
      storing parameters of custom curve type.
      Implemented as singleton. }
    {$warnings off}
    TCurveTypeStorageAdapter = class(TInterfacedObject, ICurveTypeStorage)
    private
        constructor Init;

    public
        class function Create: ICurveTypeStorage;

        procedure AddCurveType(CurveType: Curve_type);
        procedure UpdateCurveType(CurveType: Curve_type);
        procedure DeleteCurveType(CurveType: Curve_type);
    end;

    {$warnings on}

implementation

uses
    //  The module's list of the user's formulas, which the window kept until
    //  2026-10-17 (this adapter reached into the window to change it).
    user_formula_menu;

var
    CurveTypeStorageAdapter: TCurveTypeStorageAdapter;

constructor TCurveTypeStorageAdapter.Init;
begin
    inherited;
end;

class function TCurveTypeStorageAdapter.Create: ICurveTypeStorage;
begin
    Result := ICurveTypeStorage(CurveTypeStorageAdapter);
end;

procedure TCurveTypeStorageAdapter.AddCurveType(CurveType: Curve_type);
begin
    //  Kept and written; the menu lists it the next time it is built, which
    //  selecting the new formula's type does.
    UserFormulas.Add(CurveType);
end;

procedure TCurveTypeStorageAdapter.UpdateCurveType(CurveType: Curve_type);
begin
    UserFormulas.Update(CurveType);
end;

procedure TCurveTypeStorageAdapter.DeleteCurveType(CurveType: Curve_type);
begin
    UserFormulas.Remove(CurveType);
end;

initialization
    CurveTypeStorageAdapter := TCurveTypeStorageAdapter.Init;

finalization
    CurveTypeStorageAdapter.Free;

end.
