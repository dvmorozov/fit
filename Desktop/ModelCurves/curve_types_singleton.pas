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

uses Classes, SysUtils, named_points_set, int_points_set,
  int_curve_factory, int_curve_type_selector, int_curve_type_iterator, crc;

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

        destructor Destroy; override;

        { Implementation of ICurveFactory. }
        { TODO: create implementation based on TFitTask.GetPatternSpecimen: TCurvePointsSet. }
        //function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet; virtual; abstract;
        procedure RegisterCurveType(CurveClass: TCurveClass);

        { Implementation of ICurveTypeIterator. }
        procedure FirstCurveType;
        procedure NextCurveType;
        function EndCurveType: Boolean;
        function GetCurveTypeName: string;
        function GetCurveTypeId: TCurveTypeId;
        function GetCurveTypeTag(CurveTypeId: TCurveTypeId): Integer;

        { Implementation of ICurveTypeSelector. }
        procedure SelectCurveType(TypeId: TCurveTypeId);
        { Returns value of FCurrentCurveType. The value should be checked on Nil. }
        function GetSelectedCurveType: TCurveTypeId;
    end;

implementation

{ Class members aren't supported by Lazarus 0.9.24, global variable is used instead. }
var CurveTypesSingleton: TCurveTypesSingleton;

const
    CurveTypeMustBeSelected: string = 'Curve type must be previously selected.';
    NoItemsInTheList: string = 'No more items in the list.';

constructor TCurveTypesSingleton.Init;
begin
    inherited;
    FCurveTypes := TList.Create;
end;

destructor TCurveTypesSingleton.Destroy;
begin
    inherited;
end;

class function TCurveTypesSingleton.CreateCurveFactory: ICurveFactory;
begin
    Result := CurveTypesSingleton as ICurveFactory;
end;

class function TCurveTypesSingleton.CreateCurveTypeIterator: ICurveTypeIterator;
begin
    Result := CurveTypesSingleton as ICurveTypeIterator;
end;

class function TCurveTypesSingleton.CreateCurveTypeSelector: ICurveTypeSelector;
begin
    Result := CurveTypesSingleton as ICurveTypeSelector;
end;

function SortAlphabetically(Item1, Item2: Pointer): Integer;
begin
    if TCurveType(Item1).Name < TCurveType(Item2).Name then
        Result := -1
    else
        if TCurveType(Item1).Name > TCurveType(Item2).Name then
            Result := 1
        else
            Result := 0;
end;

procedure TCurveTypesSingleton.RegisterCurveType(CurveClass: TCurveClass);
var CurveType: TCurveType;
    Curve: TNamedPointsSet;
begin
    CurveType := TCurveType.Create;
    CurveType.Class_ := CurveClass;
    CurveType.ExtremumMode := CurveClass.GetExtremumMode;
    { Instantiates curve object to call its methods. }
    Curve := CurveClass.Create(nil);
    try
        CurveType.Name := Curve.GetCurveTypeName;
        CurveType.TypeId := Curve.GetCurveTypeId;
    finally
        Curve.Free;
    end;
    FCurveTypes.Add(CurveType);
    FCurveTypes.Sort(@SortAlphabetically);
    { The first type is selected by default.
      https://github.com/dvmorozov/fit/issues/126 }
    if FCurveTypes.Count <> 0 then
    begin
        FSelectedCurveType := FCurveTypes.Items[0];
    end
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
var ItemIndex: Integer;
begin
    if FCurrentCurveType <> nil then
    begin
        ItemIndex := FCurveTypes.IndexOf(FCurrentCurveType);
        if ItemIndex < FCurveTypes.Count - 1 then
        begin
            FCurrentCurveType := FCurveTypes[ItemIndex + 1];
        end
        else
            raise EListError.Create(NoItemsInTheList);
    end
    else
        raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.EndCurveType: Boolean;
begin
    if FCurrentCurveType <> nil then
    begin
        if FCurveTypes.IndexOf(FCurrentCurveType) = FCurveTypes.Count - 1 then
            Result := True
        else
            Result := False;
    end
    else
    begin
        if FCurveTypes.Count = 0 then
            Result := True
        else
            Result := False;
    end;
end;

function TCurveTypesSingleton.GetCurveTypeName: string;
begin
    if FCurrentCurveType <> nil then
    begin
        Result := FCurrentCurveType.Name;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeId: TCurveTypeId;
begin
    if FCurrentCurveType <> nil then
    begin
        Result := FCurrentCurveType.TypeId;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeTag(CurveTypeId: TCurveTypeId): Integer;
begin
    { crc32 is used for compatibility with Lazarus 0.9.24. }
    Result := crc32(0, @CurveTypeId, SizeOf(CurveTypeId));
end;

procedure TCurveTypesSingleton.SelectCurveType(TypeId: TCurveTypeId);
begin
    FirstCurveType;
    while True do
    begin
        if IsEqualGUID(FCurrentCurveType.TypeId, TypeId) then
        begin
            FSelectedCurveType := FCurrentCurveType;
            Break;
        end;
        if EndCurveType then Break
        else
            NextCurveType;
    end;
end;

function TCurveTypesSingleton.GetSelectedCurveType: TCurveTypeId;
begin
    if FSelectedCurveType <> nil then
        Result := FSelectedCurveType.TypeId
    else
        { In this case returned GUID should be different from GUID
          of any registered type. }
        Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
end;

{$hints on}
{$warnings on}
initialization
    CurveTypesSingleton := TCurveTypesSingleton.Init;

finalization
    CurveTypesSingleton.Free;

end.


