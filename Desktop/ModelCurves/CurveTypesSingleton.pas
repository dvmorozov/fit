{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CurveTypesSingleton;

{$MODE Delphi}

interface

uses Classes, SysUtils, NamedPointsSet, CBRCComponent,
  IntCurveFactory, IntCurveTypeSelector, IntCurveTypeIterator, crc;

type
    { Class-reference type for base curve type. }
    TCurveClass = class of TNamedPointsSet;

    { Class containing information about curve types. }
    TCurveType = class
    public
        CurveTypeName: string;
        CurveClass: TCurveClass;
        CurveTypeId: TCurveTypeId;
        CurveTypeTag: Integer;
    end;

    { Class-singleton containing information about curve types. }
    TCurveTypesSingleton = class(TCBRCComponent,
        ICurveFactory, ICurveTypeIterator, ICurveTypeSelector)
    private
        FCurveTypes: TList;
        { Current curve type used in iteration. }
        FCurrentCurveType: TCurveType;
        { Curve type selected by user. }
        FSelectedCurveType: TCurveType;

        class var FCurveTypesSingleton: TCurveTypesSingleton;
        constructor Init;

    public
        class function Create: TCurveTypesSingleton;
        procedure RegisterCurveType(CurveClass: TCurveClass);
        { Implementation of ICurveFactory. }
        function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
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

const
  CurveTypeMustBeSelected: string = 'Curve type must be previously selected.';
  NoItemsInTheList: string = 'No more items in the list.';

constructor TCurveTypesSingleton.Init;
begin
    inherited Create(nil);
    FCurveTypes := TList.Create;
end;

class function TCurveTypesSingleton.Create: TCurveTypesSingleton;
begin
    if FCurveTypesSingleton = nil then
      FCurveTypesSingleton := TCurveTypesSingleton.Init;
    Result := FCurveTypesSingleton;
end;

function TCurveTypesSingleton.CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.CreatePointsSet not implemented.');
end;

procedure TCurveTypesSingleton.RegisterCurveType(CurveClass: TCurveClass);
var CurveType: TCurveType;
    Curve: TNamedPointsSet;
begin
    CurveType := TCurveType.Create;
    CurveType.CurveClass := CurveClass;
    //  Instantiates curve object to call its methods.
    Curve := CurveClass.Create(nil);
    try
        CurveType.CurveTypeName := Curve.GetCurveTypeName;
        CurveType.CurveTypeId := Curve.GetCurveTypeId;
    finally
      Curve.Free;
    end;
    FCurveTypes.Add(CurveType);
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
        Result := FCurrentCurveType.CurveTypeName;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeId: TCurveTypeId;
begin
    if FCurrentCurveType <> nil then
    begin
        Result := FCurrentCurveType.CurveTypeId;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetCurveTypeTag(CurveTypeId: TCurveTypeId): Integer;
begin
    Result := crc64(0, @CurveTypeId, SizeOf(CurveTypeId));
end;

procedure TCurveTypesSingleton.SelectCurveType(TypeId: TCurveTypeId);
begin
    FirstCurveType;
    while True do
    begin
        if IsEqualGUID(FCurrentCurveType.CurveTypeId, TypeId) then
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
        Result := FSelectedCurveType.CurveTypeId
    else
        //  In this case returned GUID should be different from GUID
        //  of any registered type.
        Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
end;

end.


