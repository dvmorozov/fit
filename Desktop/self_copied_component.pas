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
    Classes, SelfCheckedComponentList, SysUtils, CBRCComponent;

type
    { The interface of component which can copy itself. }
    ISelfCopied = interface
        ['{DF1ABB41-F255-11D4-968F-C7AD39AA7469}']
        { Returns only copy of the object itself without copies of associated objects. }
        function GetCopy: TObject;
        procedure CopyParameters(const Dest: TObject);
    end;

const
    SelfCopiedGUID: TGUID = '{DF1ABB41-F255-11D4-968F-C7AD39AA7469}';

type
    { The component implementing self copying interface. }
    TSelfCopiedComponent = class(TCBRCComponent, ISelfCopied)
    public
        function GetCopy: TObject; virtual;
        procedure CopyParameters(const Dest: TObject); virtual;
    end;

    ESelfCopiedCompList = class(Exception);

    { List of self copied components. By default is always active, so copy of 
      list is also active. Caller should make the list inactive by itself if 
      necessary. }
    TSelfCopiedCompList = class(TSelfCheckedComponentList, ISelfCopied)
    public
        function GetCopy: TObject; virtual;
        { Returns copy of list which owns its items. }
        function GetSharedCopy: TObject; virtual;
        procedure CopyParameters(const Dest: TObject); virtual;

        procedure Insert(Index: integer; Item: TComponent); override;
        function Add(Item: TComponent): integer; override;
        procedure Delete(Index: integer); override;
    end;

implementation

const
    InvalidItemType: string = 'Invalid item type...';
    InvalidDestinationType: string = 'Invalid destination type...';

function TSelfCopiedCompList.GetCopy: TObject;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    CopyParameters(Result);
end;

function TSelfCopiedCompList.GetSharedCopy: TObject;
var
    i: longint;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    for i := 0 to Count - 1 do
        TSelfCopiedCompList(Result).Add(TComponent(Items[i]));
end;

procedure TSelfCopiedCompList.CopyParameters(const Dest: TObject);
var
    i:   longint;
    ISC: ISelfCopied;
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create(InvalidDestinationType);

    if Count <> 0 then
        if Count <> TSelfCopiedCompList(Dest).Count then
        begin
            TSelfCopiedCompList(Dest).Clear;
            for i := 0 to Count - 1 do
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    TSelfCopiedCompList(Dest).Add(TComponent(ISC.GetCopy))
                else
                    raise ESelfCopiedCompList.Create(InvalidItemType);
        end
        else
            for i := 0 to Count - 1 do
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    ISC.CopyParameters(TSelfCopiedCompList(Dest).Items[i])
                else
                    raise ESelfCopiedCompList.Create(InvalidItemType);
end;

procedure TSelfCopiedCompList.Insert(Index: integer; Item: TComponent);
var
    ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then
    begin
        inherited;
        ISC._AddRef;
    end
    else
        raise ESelfCopiedCompList.Create(InvalidItemType);
end;

function TSelfCopiedCompList.Add(Item: TComponent): integer;
var
    ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then
    begin
        Result := inherited;
        ISC._AddRef;
    end
    else
        raise ESelfCopiedCompList.Create(InvalidItemType);
end;

procedure TSelfCopiedCompList.Delete(Index: integer);
var
    ISC: ISelfCopied;
begin
    if Items[Index].GetInterface(SelfCopiedGUID, ISC) then
    begin
        ISC._Release;
        inherited;
    end
    else
        raise ESelfCopiedCompList.Create(InvalidItemType);
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

procedure TSelfCopiedComponent.CopyParameters(const Dest: TObject);
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create(InvalidDestinationType);
end;

end.
