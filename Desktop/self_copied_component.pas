{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of interface and class for component which can copy itself.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit self_copied_component;

interface

uses
    Classes, Contnrs, SysUtils;

type
    { The interface of component which can copy itself. }
    ISelfCopied = interface
        { Returns only copy of the object itself without copies of associated objects. }
        function GetCopy: TObject;
        procedure CopyParameters(Dest: TObject);
    end;

type
    { The component implementing self copying interface. }
    TSelfCopiedComponent = class(TComponent, ISelfCopied)
    public
        function GetCopy: TObject; virtual;
        procedure CopyParameters(Dest: TObject); virtual;
    end;

    ESelfCopiedCompList = class(Exception);

    { List of self copied components. By default is always active, so copy of 
      list is also active. Caller should make the list inactive by itself if 
      necessary. }
    TSelfCopiedCompList = class(TComponentList, ISelfCopied)
    public
        function GetCopy: TObject; virtual;
        { Returns copy of list which owns its items. }
        function GetSharedCopy: TObject; virtual;
        procedure CopyParameters(Dest: TObject); virtual;

        procedure Insert(Index: integer; Item: TComponent); virtual;
        function Add(Item: TComponent): integer; virtual;
        procedure Delete(Index: integer); virtual;
    end;

implementation

const
    InvalidDestinationType: string = 'Invalid destination type...';

function TSelfCopiedCompList.GetCopy: TObject;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create;
    CopyParameters(Result);
end;

function TSelfCopiedCompList.GetSharedCopy: TObject;
var
    i: longint;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create;
    for i := 0 to Count - 1 do
        TSelfCopiedCompList(Result).Add(TComponent(Items[i]));
end;

procedure TSelfCopiedCompList.CopyParameters(Dest: TObject);
var
    i:   longint;
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create(InvalidDestinationType);

    if Count <> 0 then
        if Count <> TSelfCopiedCompList(Dest).Count then
        begin
            TSelfCopiedCompList(Dest).Clear;
            for i := 0 to Count - 1 do
                TSelfCopiedCompList(Dest).Add(
                    TComponent(TSelfCopiedComponent(Items[i]).GetCopy))
        end
        else
        begin
            for i := 0 to Count - 1 do
                TSelfCopiedComponent(Items[i]).CopyParameters(
                    TSelfCopiedCompList(Dest).Items[i]);
        end;
end;

procedure TSelfCopiedCompList.Insert(Index: integer; Item: TComponent);
begin
    inherited;
end;

function TSelfCopiedCompList.Add(Item: TComponent): integer;
begin
    Result := inherited;
end;

procedure TSelfCopiedCompList.Delete(Index: integer);
begin
    inherited;
end;

function TSelfCopiedComponent.GetCopy: TObject;
begin
    Result := NewInstance;
    try
        TSelfCopiedComponent(Result).Create(nil);
        CopyParameters(Result);
    except
        Result.Free;
        raise;
    end;
end;

procedure TSelfCopiedComponent.CopyParameters(Dest: TObject);
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create(InvalidDestinationType);
end;

end.
