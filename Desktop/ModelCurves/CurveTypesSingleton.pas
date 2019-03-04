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
    end;

    { Class-singleton containing information about curve types. }
    TCurveTypesSingleton = class(TCBRCComponent,
        ICurveFactory, ICurveTypeIterator, ICurveTypeSelector)
    private
        FCurveTypes: TList;
        FSelectedType: TCurveType;

        class var FCurveTypesSingleton: TCurveTypesSingleton;
        constructor Init;

    public
        class function Create: TCurveTypesSingleton;
        procedure RegisterCurveType(CurveClass: TCurveClass);
        { Implementation of ICurveFactory. }
        function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
        { Implementation of ICurveTypeIterator. }
        procedure FirstType;
        procedure NextType;
        function EndType: Boolean;
        function GetTypeName: string;
        function GetTypeId: TCurveTypeId;
        function GetTypeTag: Integer;
        { Implementation of ICurveTypeSelector. }
        procedure SelectType(TypeId: TCurveTypeId);
        function GetSelectedType: TCurveTypeId;
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
        CurveType.CurveTypeName := Curve.GetTypeName;
        CurveType.CurveTypeId := Curve.GetCurveTypeId;
    finally
      Curve.Free;
    end;
    FCurveTypes.Add(CurveType);
end;

procedure TCurveTypesSingleton.FirstType;
begin
    if FCurveTypes.Count <> 0 then
        FSelectedType := FCurveTypes.First
    else
        FSelectedType := nil;
end;

procedure TCurveTypesSingleton.NextType;
var ItemIndex: Integer;
begin
    if FSelectedType <> nil then
    begin
        ItemIndex := FCurveTypes.IndexOf(FSelectedType);
        if ItemIndex < FCurveTypes.Count - 1 then
        begin
            FSelectedType := FCurveTypes[ItemIndex + 1];
        end
        else
            raise EListError.Create(NoItemsInTheList);
    end
    else
        raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.EndType: Boolean;
begin
    if FSelectedType <> nil then
    begin
        if FCurveTypes.IndexOf(FSelectedType) = FCurveTypes.Count - 1 then
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

function TCurveTypesSingleton.GetTypeName: string;
begin
    if FSelectedType <> nil then
    begin
        Result := FSelectedType.CurveTypeName;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetTypeId: TCurveTypeId;
begin
    if FSelectedType <> nil then
    begin
        Result := FSelectedType.CurveTypeId;
    end
        else raise EListError.Create(CurveTypeMustBeSelected);
end;

function TCurveTypesSingleton.GetTypeTag: Integer;
var CurveTypeId: TCurveTypeId;
begin
    CurveTypeId := GetTypeId;
    Result := crc64(0, @CurveTypeId, SizeOf(CurveTypeId));
end;

procedure TCurveTypesSingleton.SelectType(TypeId: TCurveTypeId);
begin
    FirstType;
    while not EndType do
    begin
        if IsEqualGUID(FSelectedType.CurveTypeId, TypeId) then Break;
        NextType;
    end;
end;

function TCurveTypesSingleton.GetSelectedType: TCurveTypeId;
begin
    Result := GetTypeId;
end;

end.


