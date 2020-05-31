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

    { List of self copied components. By default it keeps ownership of items
      and destroys them in corresponding methods. Ownership is transferrred
      to copy of list as well. }
    TSelfCopiedCompList = class(TComponentList, ISelfCopied)
    public
        destructor Destroy; override;
        function GetCopy: TObject; virtual;
        { Returns copy of list which owns its items. }
        function GetSharedCopy: TObject; virtual;
        procedure CopyParameters(Dest: TObject); virtual;
        { Redefines methods as virtual to be used in descendant classes. }
        procedure Insert(Index: integer; Item: TComponent); virtual;
        function Add(Item: TComponent): integer; virtual;
        procedure Delete(Index: integer); virtual;
        function Remove(AComponent: TComponent): Integer; virtual;
    end;

implementation

destructor TSelfCopiedCompList.Destroy;
begin
    { Must free objects itself because neither TComponentList nor TObjectList
      do that. }
    if OwnsObjects then
    begin
        while Count > 0 do
            Delete(0);
    end;
    inherited;
end;

function TSelfCopiedCompList.GetCopy: TObject;
begin
    { Constructs object of the same type. }
    Result := NewInstance;

    if not Assigned(Result) then
        raise EOutOfMemory.Create(
            'Out of memory in TSelfCopiedCompList.GetCopy.');

    TSelfCopiedCompList(Result).Create(OwnsObjects);
    CopyParameters(Result);
end;

function TSelfCopiedCompList.GetSharedCopy: TObject;
var
    i: longint;
begin
    { Constructs object of the same type. }
    Result := NewInstance;

    if not Assigned(Result) then
        raise EOutOfMemory.Create(
            'Out of memory in TSelfCopiedCompList.GetSharedCopy.');

    TSelfCopiedCompList(Result).Create(false);
    for i := 0 to Count - 1 do
        TSelfCopiedCompList(Result).Add(TComponent(Items[i]));
end;

procedure TSelfCopiedCompList.CopyParameters(Dest: TObject);
var
    i:   longint;
begin
    Assert(Dest.ClassType = Self.ClassType);

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
    Assert((Index >= 0) and (Index < Count));
    { The object is freed by inherited Notify method. }
    inherited;
end;

function TSelfCopiedCompList.Remove(AComponent: TComponent): Integer;
begin
    { The object is freed by inherited Notify method. }
    Result := inherited;
end;

function TSelfCopiedComponent.GetCopy: TObject;
begin
    { Constructs object of the same type. }
    Result := NewInstance;

    if not Assigned(Result) then
        raise EOutOfMemory.Create(
            'Out of memory in TSelfCopiedComponent.GetCopy.');

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
    Assert(Dest.ClassType = Self.ClassType);
end;

end.
