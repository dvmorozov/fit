{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of component list implementing ISelfChecked.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}

unit component_list;

{$MODE Delphi}

interface

uses
    CBRCComponent, Classes, LCLIntf, SysUtils, Tools;

type
    ISelfChecked = interface
        ['{E7E7008A-EE1C-4828-B1D6-A53806820A66}']
        procedure IsReady;
        function MyNameIs: string;
    end;

const
    SelfCheckedGUID: TGUID = '{E7E7008A-EE1C-4828-B1D6-A53806820A66}';

type
    //  по-умолчанию сам освобождает хранимые компоненты
    TComponentList = class(TCBRCComponent, ISelfChecked)
    protected
        List:  TList;
        State: longint;

        function GetCount: integer;
        function GetItem(index: integer): TComponent;
        function GetCapacity: integer;

        procedure SetItem(index: integer; Item: TComponent);
        procedure SetCapacity(ACapacity: integer);

        procedure ReadList(Reader: TReader);
        procedure WriteList(Writer: TWriter);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        //  !!! с XML-потоками не работает !!!
        procedure DefineProperties(Filer: TFiler); override;

        procedure IsReady; virtual;
        function MyNameIs: string; virtual;

        procedure Sort(Compare: TListSortCompare);
        procedure Pack;
        function GetState: longint;
        procedure SetState(AState: longint);

        procedure Clear;
        procedure ClearAll;
        function Add(Item: TComponent): integer; virtual;
        procedure Delete(Index: integer); virtual;
        procedure Insert(Index: integer; Item: TComponent); virtual;
        function Extract(Item: Pointer): Pointer;
        function Remove(Item: Pointer): integer;
        function IndexOf(Item: Pointer): integer;

        property Items[index: integer]: TComponent read GetItem write SetItem;
        property Capacity: integer read GetCapacity write SetCapacity;
        property Count: integer read GetCount;
    end;

const
    cfActive: longint  = 1;
    cfPassive: longint = 2;

type
    TSelfCleanList = class(TList)
    public
        procedure ClearAll; virtual;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Common', [TComponentList]);
end;

constructor TComponentList.Create;
begin
    inherited Create(AOwner);
    List := TList.Create;
    SetState(cfActive);
end;

destructor TComponentList.Destroy;
begin
    Clear;
    UtilizeObject(List);
    inherited Destroy;
end;

procedure TComponentList.DefineProperties(Filer: TFiler);
begin
    Filer.DefineProperty('List', ReadList, WriteList, True);
end;

procedure TComponentList.ReadList(Reader: TReader);
var
    i, CompCount: longint;
begin
    CompCount := Reader.ReadInteger;
    for i := 1 to CompCount do
        Add(Reader.ReadComponent(nil));
end;

procedure TComponentList.WriteList(Writer: TWriter);
var
    i: longint;
begin
    Writer.WriteInteger(Count);
    for i := 0 to Count - 1 do
        Writer.WriteComponent(Items[i]);
end;

procedure TComponentList.Clear;
begin
    if State = cfActive then
        ClearAll;
    List.Clear;
end;

function TComponentList.GetCount;
begin
    GetCount := List.Count;
end;

function TComponentList.GetItem;
begin
    Result := List.Items[Index];
end;

procedure TComponentList.SetItem;
begin
    List.Items[Index] := Item;
end;

function TComponentList.GetCapacity: integer;
begin
    GetCapacity := List.Capacity;
end;

procedure TComponentList.SetCapacity(ACapacity: integer);
begin
    List.Capacity := ACapacity;
end;

function TComponentList.Add;
begin
    Add := List.Add(Item);
end;

procedure TComponentList.Sort(Compare: TListSortCompare);
begin
    List.Sort(Compare);
end;

procedure TComponentList.Delete(Index: integer);
var
    TC: TComponent;
begin
    if State = cfActive then
    begin
        TC := Items[Index];
        UtilizeObject(TC);
    end;
    List.Delete(Index);
end;

function TComponentList.Extract(Item: Pointer): Pointer;
begin
    Result := List.Extract(Item);
end;

function TComponentList.Remove(Item: Pointer): integer;
begin
    Result := IndexOf(Item);
    Delete(Result);
end;

procedure TComponentList.ClearAll;
begin
    while Count <> 0 do
        Delete(0);
end;

function TComponentList.IndexOf(Item: Pointer): integer;
begin
    Result := List.IndexOf(Item);
end;

procedure TComponentList.SetState;
begin
    State := AState;
end;

function TComponentList.GetState: longint;
begin
    Result := State;
end;

procedure TComponentList.Insert(Index: integer; Item: TComponent);
begin
    List.Insert(Index, Item);
end;

procedure TComponentList.Pack;
begin
    List.Pack;
end;

procedure TSelfCleanList.ClearAll;
var
    i:    longint;
    Item: Pointer;
begin
    for i := 0 to Count - 1 do
    begin
        Item := Items[i];
        if Assigned(Item) then
            with TObject(Item) do
                try
                    UtilizeObject(TObject(Item));
                    Items[i] := nil;
                except
                    Items[i] := nil
                end;
    end;
    Clear;
end;

procedure TComponentList.IsReady;
var
    i:   longint;
    ISC: ISelfChecked;
begin
    for i := 0 to Count - 1 do
        if Items[i].GetInterface(SelfCheckedGUID, ISC) then
            ISC.IsReady;
end;

function TComponentList.MyNameIs: string;
begin
    Result := '';
end;

initialization
    RegisterClass(TComponentList);
end.
