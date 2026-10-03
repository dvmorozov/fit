// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which ways of showing a coordinate this build offers, and what each
one is.)

WHAT THIS ENDS. The ways of showing the argument were six closed integers
(XCM_*), a `case` that turned each into an axis, three menu entries written into
the form file and three more created in code - and the value coordinate had no
modes at all, only the word "Intensity" written into a label. So the only field
that could name its own coordinates was the one the framework was written for,
and a price series was captioned in scattering angle.

A MODE IS DATA PLUS A FACTORY. Each one declares an id (what a settings file and
a project remember), a caption (what the menu offers), an explanation topic,
which coordinates it can show, and what it reads before it can draw; its one
method builds the axis. Nothing anywhere lists which mode goes with which
coordinate or field: the menu, the persistence, the defaulting and the checks
are all derived from what is registered (non-negotiable 5).

A MODULE REGISTERS ITS MODES FROM ITS FRONT DOOR, beside its curve types and its
loaders, and the framework names none of them. Its curve types and loaders then
name the modes they prefer by id (axis_choice), which is how a field says what
its coordinates are.

THE REGISTRY'S SHAPE is data_source_registry's - register, find, list, and the
same refusals in words - so a contributor who has met one has met this one.

NO "AUTOMATIC" ENTRY IN HERE. The default is not a way of showing a coordinate;
it is the rule for choosing one of these, and it lives in axis_choice.
}
unit axis_mode_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, coordinate_axis;

type
    { What a mode reads besides the coordinate itself. }
    TAxisParameter = (
        apNone,
        { The radiation wavelength, a problem setting (A2 in module-architecture:
          it is still the framework's, and this entry leaves with it). }
        apWaveLength,
        { A definition the user gives: a name, a unit and a pair of formulas. }
        apDefinition);

    { A user's own axis for one coordinate. }
    TAxisDefinition = record
        Name: string;
        UnitName: string;
        Forward: string;    //  shown = f(x), x the stored value
        Inverse: string;    //  stored = g(x), x the shown value
    end;

    { Everything a mode may build its axis from. }
    TAxisContext = record
        WaveLength: double;
        Definition: TAxisDefinition;
        { What the coordinate IS - the name and unit its own quantity goes by,
          as the automatic rule found it. A mode that transforms a quantity
          rather than naming one (a logarithm) keeps the quantity's name. }
        QuantityName: string;
        QuantityUnit: string;
        { The date each bar was recorded on, for an argument that counts bars
          of a dated series; empty otherwise, and always for the value. }
        ArgumentDates: TAxisDates;
    end;

    { What a mode says about itself. }
    TAxisModeInfo = record
        { What settings and projects remember. Never shown, never changed once
          published: a renamed id is a remembered choice silently lost. }
        Id: string;
        { The menu entry, the same for every coordinate unless the mode says
          otherwise (TAxisMode.CaptionFor). }
        Caption: string;
        { The explanation behind the entry. }
        Topic: string;
        { Which coordinates it can show. }
        Dimensions: TAxisDimensions;
        { What it reads, and whether it cannot draw without it. }
        Parameter: TAxisParameter;
        ParameterRequired: boolean;
    end;

    TAxisMode = class
    public
        class function Info: TAxisModeInfo; virtual; abstract;
        { The menu entry for ADimension. Info.Caption unless overridden - a
          custom axis is a Custom Position on one side and a Custom Value on
          the other. }
        class function CaptionFor(ADimension: TAxisDimension): string; virtual;
        { The axis for ADimension. The caller owns the result. }
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; virtual; abstract;
    end;
    TAxisModeClass = class of TAxisMode;
    TAxisModeClasses = array of TAxisModeClass;

    { Raised when a build's mode set is self-contradictory. A registration
      fault is a programming error in this build, not a user error, so it stops
      start-up rather than being logged and carried past. }
    EAxisModeRegistration = class(Exception);

{ Registers a mode. Raises when the class is nil, when it declares no id or no
  coordinate, or when another class has already claimed the id - which would
  otherwise be settled by the order of two uses clauses, and the loser would be
  an entry that looks installed and can never be chosen.

  AN IDENTICAL REGISTRATION IS A NO-OP: a front door may be called twice. }
procedure RegisterAxisMode(AMode: TAxisModeClass);

{ The mode registered under AId, or nil. Nil rather than an exception: a stale
  id in a remembered choice is an ordinary outcome - the build may no longer
  contain the module that registered it - and the caller falls back. }
function FindAxisMode(const AId: string): TAxisModeClass;

{ Everything registered, in registration order. }
function RegisteredAxisModes: TAxisModeClasses;

{ The modes offered for ADimension, in registration order - what that
  coordinate's menu lists after its automatic entry. }
function AxisModesFor(ADimension: TAxisDimension): TAxisModeClasses;

{ Whether AId names a registered mode that can show ADimension. }
function AxisModeShows(const AId: string; ADimension: TAxisDimension): boolean;

type
    TTopicResolves = function(const ATopic: string): boolean;

{ Everything wrong with the registered modes, one sentence each; empty when the
  build is sound. The walk, not a list: the next mode registered without an
  explanation - here or in a module nobody here has heard of - fails by name. }
function AxisModeFindings(AResolves: TTopicResolves): TStringArray;

{ The same over a GIVEN set, so a test can see what an incomplete mode is
  reported as without registering it for every later test in the process. }
function AxisModeFindingsFor(const AModes: TAxisModeClasses;
    AResolves: TTopicResolves): TStringArray;

{ The coordinate in words, for menus and findings: 'argument' or 'value'. }
function DimensionWord(ADimension: TAxisDimension): string;

implementation

var
    Registry: TAxisModeClasses;

class function TAxisMode.CaptionFor(ADimension: TAxisDimension): string;
begin
    Result := Info.Caption;
end;

function DimensionWord(ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := 'argument';
        adValue:    Result := 'value';
    end;
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

procedure RegisterAxisMode(AMode: TAxisModeClass);
var
    Existing: longint;
    Id: string;
begin
    if not Assigned(AMode) then
        raise EAxisModeRegistration.Create(
            'an axis mode was registered with no class');
    Id := Trim(AMode.Info.Id);
    if Id = '' then
        raise EAxisModeRegistration.Create(AMode.ClassName +
            ' was registered without an id, so no setting or project could ' +
            'remember it');
    if AMode.Info.Dimensions = [] then
        raise EAxisModeRegistration.Create(AMode.ClassName +
            ' was registered for no coordinate, so no menu would offer it');

    Existing := IndexOfId(Id);
    if Existing >= 0 then
    begin
        if Registry[Existing] = AMode then
            Exit;
        raise EAxisModeRegistration.Create('the axis mode id "' + Id +
            '" is claimed by both ' + Registry[Existing].ClassName + ' and ' +
            AMode.ClassName);
    end;

    SetLength(Registry, Length(Registry) + 1);
    Registry[High(Registry)] := AMode;
end;

function FindAxisMode(const AId: string): TAxisModeClass;
var
    Index: longint;
begin
    Result := nil;
    if Trim(AId) = '' then
        Exit;
    Index := IndexOfId(Trim(AId));
    if Index >= 0 then
        Result := Registry[Index];
end;

function RegisteredAxisModes: TAxisModeClasses;
begin
    Result := Registry;
end;

function AxisModesFor(ADimension: TAxisDimension): TAxisModeClasses;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(Registry) do
        if ADimension in Registry[i].Info.Dimensions then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Registry[i];
        end;
end;

function AxisModeShows(const AId: string; ADimension: TAxisDimension): boolean;
var
    Mode: TAxisModeClass;
begin
    Mode := FindAxisMode(AId);
    Result := Assigned(Mode) and (ADimension in Mode.Info.Dimensions);
end;

procedure Add(var AList: TStringArray; const AText: string);
begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AText;
end;

function AxisModeFindingsFor(const AModes: TAxisModeClasses;
    AResolves: TTopicResolves): TStringArray;
var
    Info: TAxisModeInfo;
    Name_: string;
    D: TAxisDimension;
    i: longint;
begin
    Result := nil;
    for i := 0 to High(AModes) do
    begin
        Info := AModes[i].Info;
        Name_ := AModes[i].ClassName;
        if Trim(Info.Id) = '' then
            Add(Result, Name_ + ' declares no id');
        if Info.Dimensions = [] then
            Add(Result, Name_ + ' shows no coordinate');
        for D := Low(TAxisDimension) to High(TAxisDimension) do
            if (D in Info.Dimensions) and (Trim(AModes[i].CaptionFor(D)) = '') then
                Add(Result, Name_ + ' has no caption for the ' +
                    DimensionWord(D) + ', so its menu entry would be blank');
        if Trim(Info.Topic) = '' then
            Add(Result, Name_ + ' declares no explanation topic')
        else if Assigned(AResolves) and not AResolves(Info.Topic) then
            Add(Result, Name_ + ' names the topic "' + Info.Topic +
                '", which resolves to no explanation');
        if Info.ParameterRequired and (Info.Parameter = apNone) then
            Add(Result, Name_ + ' requires a parameter without saying which, ' +
                'so nothing could ever supply it');
    end;
end;

function AxisModeFindings(AResolves: TTopicResolves): TStringArray;
begin
    Result := AxisModeFindingsFor(Registry, AResolves);
end;

end.
