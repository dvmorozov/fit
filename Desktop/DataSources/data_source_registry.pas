// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which places this build can fetch data from, and what fetches them.)

THIS IS data_loader_registry'S SHAPE, DELIBERATELY. Same record of declared
facts, same class-of, same three questions - register, find, list - and the same
refusals in words. Two registries a module contributes through should not be two
things to learn, and whoever adds the third can copy either.

WHY THE FRAMEWORK NEEDS ONE AT ALL. The wizard offers what is registered and
nothing else, so a source is reachable exactly when a build contains it. There is
no table anywhere of "which source needs which step": each source declares its
facts and one rule derives the rest (see data_source.pas).

A MODULE REGISTERS ITS SOURCES FROM ITS FRONT DOOR, beside its curve types, its
menu and its explanations - and the framework names none of them. The one thing
this file's public build ships is the framework's own field-neutral set, in
data_source_registration.

NO ExpectDataSources, AND THAT IS NOT AN OVERSIGHT. Curve types need that check
because they register from their unit's initialization section, which runs only
if the unit was linked - so "did it link?" is a real question with a silent wrong
answer. A source is registered by an explicit call in the front door, and that
call is what links it: if it is missing, the build fails to compile rather than
starting up short of a source.
}
unit data_source_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    TDataSourceClasses = array of TDataSourceClass;

    { Raised when a build's source set is self-contradictory - no class, no id,
      or two sources under one id. A registration fault is a programming error
      in this build, not a user error, so it stops start-up rather than being
      logged and carried past. }
    EDataSourceRegistration = class(Exception);

{ Registers a source. Raises when the class is nil, when its declared id is
  empty, and when another class has already claimed that id - which would
  otherwise be resolved by registration order, i.e. by the order of two uses
  clauses, and the loser would be a source that looks installed and can never be
  chosen.

  AN IDENTICAL REGISTRATION IS A NO-OP, for the reason data_loader_registry now
  says: a module's front door registers it, and a front door may be called
  twice. }
procedure RegisterDataSource(ASourceClass: TDataSourceClass);

{ The source registered under AId, or nil when nothing claims it. Nil rather
  than an exception: a stale id in a remembered import is an ordinary outcome -
  the build may simply not contain that module any more - and the caller says so
  in its own words. }
function FindDataSourceClass(const AId: string): TDataSourceClass;

{ Everything registered, in registration order. What the wizard lists, and what
  the completeness walk walks. }
function RegisteredDataSources: TDataSourceClasses;

{ How many sources are registered. }
function DataSourceCount: longint;

{ The categories in use, in the order they were first registered. Derived, never
  declared: a category exists because a source uses it. }
function DataSourceCategories: TStringList;

implementation

var
    Registry: TDataSourceClasses;

function DataSourceCount: longint;
begin
    Result := Length(Registry);
end;

function RegisteredDataSources: TDataSourceClasses;
begin
    Result := Registry;
end;

function IndexOfId(const AId: string): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(Registry) do
        if SameText(Registry[i].Info.Id, AId) then
            Exit(i);
end;

function FindDataSourceClass(const AId: string): TDataSourceClass;
var
    Index: longint;
begin
    Result := nil;
    Index := IndexOfId(Trim(AId));
    if Index >= 0 then
        Result := Registry[Index];
end;

procedure RegisterDataSource(ASourceClass: TDataSourceClass);
var
    Existing: longint;
    Id: string;
begin
    if not Assigned(ASourceClass) then
        raise EDataSourceRegistration.Create(
            'a data source was registered with no class');

    Id := Trim(ASourceClass.Info.Id);
    if Id = '' then
        raise EDataSourceRegistration.Create(ASourceClass.ClassName +
            ' was registered without an id, so nothing could ever name it - ' +
            'not the wizard, and not a project remembering where its data ' +
            'came from');

    Existing := IndexOfId(Id);
    if Existing >= 0 then
    begin
        //  The same class arriving again is a front door called twice.
        if Registry[Existing] = ASourceClass then
            Exit;
        raise EDataSourceRegistration.Create('the data source id "' + Id +
            '" is claimed by both ' + Registry[Existing].ClassName + ' and ' +
            ASourceClass.ClassName);
    end;

    SetLength(Registry, Length(Registry) + 1);
    Registry[High(Registry)] := ASourceClass;
end;

function DataSourceCategories: TStringList;
var
    i: longint;
    Category: string;
begin
    Result := TStringList.Create;
    for i := 0 to High(Registry) do
    begin
        Category := Registry[i].Info.Category;
        if Result.IndexOf(Category) < 0 then
            Result.Add(Category);
    end;
end;

end.
