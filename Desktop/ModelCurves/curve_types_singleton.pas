// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

Copyright (C) Dmitry Morozov
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
        function GetCurrentCurveClass: TCurveClass;

        { Implementation of ICurveTypeSelector. }
        procedure SelectCurveType(TypeId: TCurveTypeId);
        { Returns value of FCurrentCurveType. The value should be checked on Nil. }
        function GetSelectedCurveType: TCurveTypeId;
        function GetSelectedExtremumMode: TExtremumMode;
    end;

{ The class registered under ATypeId, or nil when nothing is registered under
  it. Lets the engine instantiate a curve type generically instead of from a
  hardcoded list, so a self-registered type actually works end to end. }
function FindCurveClassById(const ATypeId: TCurveTypeId): TCurveClass;
{ The type a new problem starts on: the first registered type in the order the
  registry keeps (alphabetical), which is also where the process-wide selection
  starts. Raises, as the iterator does, when nothing is registered. }
function DefaultCurveTypeId: TCurveTypeId;

{ The one registered class called AName, or nil when no type or more than one
  has that name - two types sharing a name cannot say which one a curve is, and
  answering for the wrong one is worse than answering for none. A curve's type
  is read back from its title this way (model_outline.CurveTypeNameOfTitle). }
function FindCurveClassByName(const AName: string): TCurveClass;

{ ATypeId as a log line names it: its name and its id, or the id and that nothing
  is registered under it. What every "curve type:" line in the logs writes, so
  a line can be read without looking an id up. }
function CurveTypeText(const ATypeId: TCurveTypeId): string;

{ Whether curve type AClass has a formula. With no type selected, a curve is
  taken to have one: that is what every engine can fit, and offering less
  before a type is chosen would grey entries for no reason anyone could name. }
function CurveIsAnalytic(AClass: TCurveClass): boolean;

{ Whether the amplitude of curve type AClass is free to grow. With no type
  selected it is not. }
function CurveAmplitudeIsFree(AClass: TCurveClass): boolean;

{ Whether curve type AId is registered in this build. Of the shape
  curve_type_choice.TCurveTypeQuery, for the restore at start-up. }
function CurveTypeIsRegistered(const AId: TGuid): boolean;

implementation

{ Class members aren't supported by Lazarus 0.9.24, global variable is used instead. }
var
    CurveTypesSingleton: TCurveTypesSingleton;

function CurveTypeText(const ATypeId: TCurveTypeId): string;
var
    CurveClass: TCurveClass;
begin
    CurveClass := FindCurveClassById(ATypeId);
    if Assigned(CurveClass) then
        Result := CurveClass.GetCurveTypeName + ' ' + GUIDToString(ATypeId)
    else
        Result := GUIDToString(ATypeId) + ' (not registered in this build)';
end;

function DefaultCurveTypeId: TCurveTypeId;
var
    Iter: ICurveTypeIterator;
begin
    //  The FIRST REGISTERED type, never the selection: the selection is
    //  process-wide and moves with whatever chose a type last - another
    //  problem in the same server, or the desktop menu in the same process.
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    Result := Iter.GetCurveTypeId;
end;

function FindCurveClassById(const ATypeId: TCurveTypeId): TCurveClass;
var
    Iter: ICurveTypeIterator;
begin
    Result := nil;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        if IsEqualGUID(Iter.GetCurveTypeId, ATypeId) then
        begin
            Result := Iter.GetCurrentCurveClass;
            Exit;
        end;
        //  EndCurveType means "the current item IS the last one", not "past the
        //  end", so the item must be examined BEFORE the test - otherwise the
        //  last registered type is silently skipped.
        if Iter.EndCurveType then
            Break
        else
            Iter.NextCurveType;
    end;
end;

function FindCurveClassByName(const AName: string): TCurveClass;
var
    Iter: ICurveTypeIterator;
    Cls: TCurveClass;
    Matches: longint;
begin
    Result := nil;
    if Trim(AName) = '' then
        Exit;
    Matches := 0;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        Cls := Iter.GetCurrentCurveClass;
        if Assigned(Cls) and (Cls.GetCurveTypeName = AName) then
        begin
            Result := Cls;
            Inc(Matches);
        end;
        //  Examined BEFORE the test, for FindCurveClassById's reason.
        if Iter.EndCurveType then
            Break;
        Iter.NextCurveType;
    end;
    if Matches <> 1 then
        Result := nil;
end;

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

function TCurveTypesSingleton.GetCurrentCurveClass: TCurveClass;
begin
    if FCurrentCurveType <> nil then
        Result := FCurrentCurveType.FClass
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

function CurveIsAnalytic(AClass: TCurveClass): boolean;
begin
    Result := (not Assigned(AClass)) or AClass.IsAnalytic;
end;

function CurveAmplitudeIsFree(AClass: TCurveClass): boolean;
begin
    Result := Assigned(AClass) and AClass.AmplitudeIsUnbounded;
end;

{$hints on}
{$warnings on}
function CurveTypeIsRegistered(const AId: TGuid): boolean;
begin
    Result := Assigned(FindCurveClassById(AId));
end;

initialization
    CurveTypesSingleton := TCurveTypesSingleton.Init;

finalization
    CurveTypesSingleton.Free;

end.
