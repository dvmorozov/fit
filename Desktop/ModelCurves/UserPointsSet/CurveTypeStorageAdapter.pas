{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class-adapter for ICurveTypeStorage.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CurveTypeStorageAdapter;

{$MODE Delphi}

interface

uses SysUtils, Settings, CBRCComponent, IntCurveTypeStorage;

type
    { Class-adapter implementing basic operation for 
      storing parameters of custom curve type.
      Implemented as singleton. }
    {$warnings off}
    TCurveTypeStorageAdapter = class(TCBRCComponent, ICurveTypeStorage)
    private
        class var FCurveTypeStorageAdapter: TCurveTypeStorageAdapter;
        constructor Init;

    public
        class function Create: TCurveTypeStorageAdapter;

        procedure AddCurveType(CurveType: Curve_type);
        procedure UpdateCurveType(CurveType: Curve_type);
        procedure DeleteCurveType(CurveType: Curve_type);
    end;
    {$warnings on}

implementation

uses mainform;

constructor TCurveTypeStorageAdapter.Init;
begin
    inherited Create(nil);
end;

class function TCurveTypeStorageAdapter.Create: TCurveTypeStorageAdapter;
begin
    if FCurveTypeStorageAdapter = nil then
      FCurveTypeStorageAdapter := TCurveTypeStorageAdapter.Init;
    Result := FCurveTypeStorageAdapter;
end;

procedure TCurveTypeStorageAdapter.AddCurveType(CurveType: Curve_type);
begin
    //  Saving curve parameters.
    FormMain.Settings.Curve_types.Add(CurveType);
    FormMain.WriteCurve(CurveType);

    //FormMain.DeleteDummyCurve;
    //  Adds new menu item.
    FormMain.AddCurveMenuItem(CurveType);
end;

procedure TCurveTypeStorageAdapter.UpdateCurveType(CurveType: Curve_type);
begin
    DeleteFile(PChar(CurveType.FileName));
    FormMain.WriteCurve(CurveType);
end;

procedure TCurveTypeStorageAdapter.DeleteCurveType(CurveType: Curve_type);
begin
    FormMain.DeleteCurve(CurveType);
end;

end.


