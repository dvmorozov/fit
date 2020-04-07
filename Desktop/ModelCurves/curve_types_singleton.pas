{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit curve_types_singleton;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, crc, int_curve_factory, int_curve_type_iterator,
    int_curve_type_selector, named_points_set, SysUtils;

type
    { ENotImplementd isn't supported by Lazarus 0.9.24. It is used
      for building server part using wst-0.5. }
    ENotImplemented = class(Exception);

    { Class-singleton containing information about curve types.
      Should be inherited from TCompoenent to }
{$warnings off}
{$hints off}
    TCurveTypesSingleton = class(TInterfacedObject,
        ICurveFactory, ICurveTypeIterator, ICurveTypeSelector)
    private
        FCurveTypes: TList;
        { Current curve type used in iteration. }
        FCurrentCurveType: TCurveType;
        { Curve type selected by user. }
        FSelectedCurveType: TCurveType;

        constructor Init;

    public
        class function CreateCurveFactory: ICurveFactory;
        class function CreateCurveTypeIterator: ICurveTypeIterator;
        class function CreateCurveTypeSelector: ICurveTypeSelector;

        { Implementation of ICurveFactory. }
        { TODO: create implementation based on TFitTask.GetPatternCurve: TCurvePointsSet. }
        //function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet; virtual; abstract;
        procedure RegisterCurveType(CurveClass: TCurveClass);

        { Implementation of ICurveTypeIterator. }
        procedure FirstCurveType;
        procedure NextCurveType;
        function EndCurveType: boolean;
        function GetCurveTypeName: string;
        function GetCurveTypeId: TCurveTypeId;
        function GetCurveTypeTag(CurveTypeId: TCurveTypeId): integer;

        { Implementation of ICurveTypeSelector. }
        procedure SelectCurveType(TypeId: TCurveTypeId);
        { Returns value of FCurrentCurveType. The value should be checked on Nil. }
        function GetSelectedCurveType: TCurveTypeId;
        function GetSelectedExtremumMode: TExtremumMode;
    end;

implementation

{ Class members aren't supported by Lazarus 0.9.24, global variable is used instead. }
var
    CurveTypesSingleton: TCurveTypesSingleton;

const
    CurveTypeMustBeSelected: string = 'Curve type must be previously selected.';
    NoItemsInTheList: string = 'No more items in the list.';

constructor TCurveTypesSingleton.Init;
begin
    inherited;
    FCurveTypes := TList.Create;
end;

class function TCurveTypesSingleton.CreateCurveFactory: ICurveFactory;
begin
    Result := ICurveFactory(CurveTypesSingleton);
end;

class function TCurveTypesSingleton.CreateCurveTypeIterator: ICurveTypeIterator;
begin
    Result := ICurveTypeIterator(CurveTypesSingleton);
end;

class function TCurveTypesSingleton.CreateCurveTypeSelector: ICurveTypeSelector;
begin
    Result := ICurveTypeSelector(CurveTypesSingleton);
end;

function SortAlphabetically(Item1, Item2: Pointer): integer;
begin
    if TCurveType(Item1).FName < TCurveType(Item2).FName then
        Result := -1
    else
    if TCurveType(Item1).FName > TCurveType(Item2).FName then
        Result := 1
    else
        Result := 0;
end;

procedure TCurveTypesSingleton.RegisterCurveType(CurveClass: TCurveClass);
var
    CurveType: TCurveType;
begin
    CurveType := TCurveType.Create;
    CurveType.FClass := CurveClass;
    CurveType.FExtremumMode := CurveClass.GetExtremumMode;
    CurveType.FTypeId := CurveClass.GetCurveTypeId;
    CurveType.FName := CurveClass.GetCurveTypeName;

    FCurveTypes.Add(CurveType);
    FCurveTypes.Sort(@SortAlphabetically);
    { The first type is selected by default.
      https://github.com/dvmorozov/fit/issues/126 }
    if FCurveTypes.Count <> 0 then
        FSelectedCurveType := FCurveTypes.Items[0]
    else
        FSelectedCurveType := nil;
end;

procedure TCurveTypesSingleton.FirstCurveType;
begin
    if FCurveTypes.Count <> 0 then
        FCurrentCurveType := FCurveTypes.First
    else
        FCurrentCurveType := nil;
end;

procedure TCurveTypesSingleton.NextCurveType;
var
    ItemIndex: integer;
begin
    if FCurrentCurveType <> nil then
    begin
        ItemIndex := FCurveTypes.IndexOf(FCurrentCurveType);
        if ItemIndex < FCurveTypes.Count - 1 then
            FCurrentCurveType := FCurveTypes[ItemIndex + 1]
        else
            raise EListError.Create(NoItemsInTheList);
    end
    else
        raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.EndCurveType: boolean;
begin
    if FCurrentCurveType <> nil then
    begin
        if FCurveTypes.IndexOf(FCurrentCurveType) = FCurveTypes.Count - 1 then
            Result := True
        else
            Result := False;
    end
    else
    if FCurveTypes.Count = 0 then
        Result := True
    else
        Result := False;
end;

function TCurveTypesSingleton.GetCurveTypeName: string;
begin
    if FCurrentCurveType <> nil then
        Result := FCurrentCurveType.FName
    else
        raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeId: TCurveTypeId;
begin
    if FCurrentCurveType <> nil then
        Result := FCurrentCurveType.FTypeId
    else
        raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeTag(CurveTypeId: TCurveTypeId): integer;
begin
    { crc32 is used for compatibility with Lazarus 0.9.24. }
    Result := crc32(0, @CurveTypeId, SizeOf(CurveTypeId));
end;

procedure TCurveTypesSingleton.SelectCurveType(TypeId: TCurveTypeId);
begin
    FirstCurveType;
    while True do
    begin
        if IsEqualGUID(FCurrentCurveType.FTypeId, TypeId) then
        begin
            FSelectedCurveType := FCurrentCurveType;
            Break;
        end;
        if EndCurveType then
            Break
        else
            NextCurveType;
    end;
end;

function TCurveTypesSingleton.GetSelectedCurveType: TCurveTypeId;
begin
    if FSelectedCurveType <> nil then
        Result := FSelectedCurveType.FTypeId
    else
        { In this case returned GUID should be different from GUID
          of any registered type. }
        Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
end;

function TCurveTypesSingleton.GetSelectedExtremumMode: TExtremumMode;
begin
    if FSelectedCurveType <> nil then
        Result := FSelectedCurveType.FExtremumMode
    else
        Result := OnlyMaximums;
end;

{$hints on}
{$warnings on}
initialization
    CurveTypesSingleton := TCurveTypesSingleton.Init;

finalization
    CurveTypesSingleton.Free;

end.
