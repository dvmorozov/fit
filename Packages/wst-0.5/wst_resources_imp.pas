{$INCLUDE wst_global.inc}
unit wst_resources_imp;

interface

uses
  Classes, SysUtils;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  EWSTResourceException = class(Exception)
  end;
  
  TWSTResourceManager = Class(TPersistent)
  Public
    Procedure Assign(Source : TPersistent); override;
    Function  HasResource(Const AName : String) : Boolean; virtual; abstract;
    Function  ResourceAsString(Const AName : String) : String; virtual; abstract;
    Procedure AddResource(Const AName,AValue : String); overload;virtual; Abstract;
    Procedure AddResource(const Name: AnsiString; Values: array of string);overload;
    Procedure GetResourceList(List : TStrings); virtual; abstract;
  end;
  
  Function GetWSTResourceManager(Force : Boolean = True) : TWSTResourceManager;
  Function SetWSTResourceManager(AValue : TWSTResourceManager) : TWSTResourceManager;
  
implementation

ResourceString
  SErrNoSuchResource = 'No such resource in resource list: "%s"';

Type
  { Default implementation of resource manager }

  TWSTResourceItem = Class(TCollectionItem)
  Private
    FName : String;
    FValue : String;
  Public
    Property Name : String Read FName Write FName;
    Property Value : String Read FValue Write FValue;
  end;

  TWSTResourceItems = Class(TCollection)
  Private
    Function GetResource(AIndex : Integer) : TWSTResourceItem;
    Procedure SetResource(AIndex : Integer; AValue : TWSTResourceItem);
  Public
    Function IndexOfResource(AName : String) : Integer;
    Function FindResource(AName : String) : TWSTResourceItem;
    Function ResourceByName(AName : String) : TWSTResourceItem;
    Property Resources[Index : Integer] : TWSTResourceItem Read GetResource Write SetResource; default;
  end;

  TCollectionResourceManager = Class(TWSTResourceManager)
    FResources : TWSTResourceItems;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Function  HasResource(Const AName : String) : Boolean; override;
    Function  ResourceAsString(Const AName : String) : String; override;
    Procedure AddResource(Const AName,AValue : String); override;
    Procedure GetResourceList(List : TStrings); override;
  end;


Function TWSTResourceItems.GetResource(AIndex : Integer) : TWSTResourceItem;

begin
  Result:=TWSTResourceItem(Items[AIndex]);
end;

Procedure TWSTResourceItems.SetResource(AIndex : Integer; AValue : TWSTResourceItem);

begin
  Items[AIndex]:=AValue;
end;

Function TWSTResourceItems.IndexOfResource(AName : String) : Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetResource(Result).Name,AName)<>0) do
    Dec(Result);
end;

Function TWSTResourceItems.FindResource(AName : String) : TWSTResourceItem;

Var
  I : Integer;

begin
  I:=IndexOfResource(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetResource(I);
end;

Function TWSTResourceItems.ResourceByName(AName : String) : TWSTResourceItem;

begin
  Result:=FindResource(AName);
  If (Result=Nil) then
    Raise EWSTResourceException.CreateFmt(SErrNoSuchResource,[ANAme]);
end;

Procedure TWSTResourceManager.Assign(Source : TPersistent);

Var
  I : integer;
  L : TStringList;
  S : String;
  R : TWSTResourceManager;

begin
  If Source is TWSTResourceManager then
    begin
    L:=TStringList.Create;
    try
      R:=TWSTResourceManager(Source);
      R.GetResourceList(L);
      For I:=0 to L.Count-1 do
        begin
        S:=R.ResourceAsString(L[i]);
        Self.AddResource(L[i],S);
        end;
    finally
      L.free;
    end;
    end;
end;

procedure TWSTResourceManager.AddResource(const Name: AnsiString; Values: array of string);

var
  i,L,TLen, p: integer;
  S : String;

begin
  L:=High(Values)-Low(Values)+1;
  If (L=1) then
    S:=Values[0]
  else if (L>1) then
    begin
    TLen:=0;
    for I:=Low(Values) to High(Values) do
      TLen:=TLen+Length(Values[i]);
    SetLength(S,TLen);
    p:=1;
    for i:=Low(Values) to High(Values) do
      begin
      L:=length(Values[i]);
      if (L>0) then
        begin
        Move(Values[i][1],S[p],l);
        inc(p,l);
        end;
      end;
    end;
  If (S<>'') then
    AddResource(Name,S);
end;


Function  TCollectionResourceManager.HasResource(Const AName : String) : Boolean;

begin
  Result:=FResources.IndexOfResource(AName)<>-1;
end;

Function  TCollectionResourceManager.ResourceAsString(Const AName : String) : String;

begin
  Result:=FResources.ResourceByName(AName).Value;
end;

Procedure TCollectionResourceManager.AddResource(Const AName,AValue : String);

Var
  R : TWSTResourceItem;

begin
  R:=FResources.Add as TWSTResourceItem;
  R.Name:=AName;
  R.Value:=AValue;
end;

Procedure TCollectionResourceManager.GetResourceList(List : TStrings);

Var
  I : Integer;

begin
  For I:=0 to FResources.Count-1 do
    List.Add(FResources[i].Name);
end;

Constructor TCollectionResourceManager.Create;

begin
  FResources:=TWSTResourceItems.Create(TWSTResourceItem);
end;

Destructor TCollectionResourceManager.Destroy;

begin
  FResources.Free;
  Inherited;
end;

Var
  ResMGR : TWSTResourceManager;

Function GetWSTResourceManager(Force : Boolean = True) : TWSTResourceManager;

begin
  Result:=ResMGR;
  If (ResMgr=Nil) and Force then
    ResMGr:=TCollectionResourceManager.Create;
  Result:=ResMGR;
end;

Function SetWSTResourceManager(AValue : TWSTResourceManager) : TWSTResourceManager;

begin
  // Copy resources if needed.
  If Assigned(ResMGR) and Assigned(AValue) then
    AValue.Assign(ResMGR);
  FreeAndNil(ResMGR);
  ResMGR:=AValue;
end;

end.

