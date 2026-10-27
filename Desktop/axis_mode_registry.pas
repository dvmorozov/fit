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

A NUMBER A MODE READS IS A MODULE'S TOO. A mode may draw through a number the
problem holds - a constant of the measurement that turns the stored coordinate
into the one shown. The mode names it (TAxisModeInfo.ParameterName); the module
that registers the mode describes it here (RegisterAxisParameter), keeps it for
each problem through problem_value_store under the same name, and offers the
command that sets it through its own menu (int_ui_host); the window asks for it
through the value dialog registered under that name (value_dialogs) when a mode
cannot be drawn without it. So the framework reads, asks for and remembers a
number it has no word for. The wavelength the diffraction angles divide by was
the first such number, and was the framework's own until 2026-10-26:
TAxisParameter had an entry for it, and the window a menu entry, a dialog call
and the id of the mode that entry switched to.
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
        { A number the problem holds, named by TAxisModeInfo.ParameterName and
          described by the module that registered the mode
          (RegisterAxisParameter). }
        apValue,
        { A definition the user gives: a name, a unit and a pair of formulas. }
        apDefinition);

    { One number a mode may read, by the name its module gave it. }
    TAxisValue = record
        Name: string;
        Value: double;
    end;
    { Every number known, at most one per name. Zero is "none known". }
    TAxisValues = array of TAxisValue;

    { What a module says about a number its modes read. }
    TAxisParameterInfo = record
        { The name the mode reads it by, the problem keeps it under
          (problem_value_store) and its dialog is registered as
          (value_dialogs). Never changed once published, for the reason a mode
          id is not. }
        Name: string;
        { What it is called where it is asked for and refused: a noun that
          reads in the middle of a sentence once lower-cased ("Wavelength"). }
        Caption: string;
        { A value it may take, written as typed: what a refusal offers as an
          example. }
        Example: string;
    end;
    TAxisParameterInfos = array of TAxisParameterInfo;

    { A user's own axis for one coordinate. }
    TAxisDefinition = record
        Name: string;
        UnitName: string;
        Forward: string;    //  shown = f(x), x the stored value
        Inverse: string;    //  stored = g(x), x the shown value
    end;

    { Everything a mode may build its axis from. }
    TAxisContext = record
        { The numbers the problem holds that modes read (apValue), by name. }
        Values: TAxisValues;
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
        { For apValue, the name of the number it reads (TAxisParameterInfo). }
        ParameterName: string;
    end;

    TAxisMode = class
    public
        class function Info: TAxisModeInfo; virtual; abstract;
        { The menu entry for ADimension. Info.Caption unless overridden - a
          custom axis is a Custom Position on one side and a Custom Value on
          the other. }
        class function CaptionFor(ADimension: TAxisDimension): string; virtual;
        { The explanation behind the entry for ADimension. Info.Topic unless
          overridden - Linear over the R-factor is not the data's value axis,
          and must not be explained as one. }
        class function TopicFor(ADimension: TAxisDimension): string; virtual;
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

{ What an axis shows, in words, for menus and findings: 'argument', 'value'
  or 'R-factor'. }
function DimensionWord(ADimension: TAxisDimension): string;

{ Describes a number a mode reads. Raises for no name or no caption, and when
  another description already claims the name - two modules reading one name
  would each find the other's number. An identical registration is a no-op. }
procedure RegisterAxisParameter(const AInfo: TAxisParameterInfo);

{ The description registered under AName; False when there is none. }
function FindAxisParameter(const AName: string;
    out AInfo: TAxisParameterInfo): boolean;

{ Every number described, in registration order. }
function RegisteredAxisParameters: TAxisParameterInfos;

{ The number AValues holds under AName, or 0 - "none known" - when it holds
  none. }
function AxisValueOf(const AValues: TAxisValues; const AName: string): double;

{ AValues with AName set to AValue, replacing what it held under that name. }
procedure PutAxisValue(var AValues: TAxisValues; const AName: string;
    AValue: double);

{ WHETHER A TYPED VALUE OF AParameter CAN BE USED, and what to say if it cannot.

  POSITIVE, AND NEVER ZERO: zero is what "none known" means to the client and to
  the problem's store, so accepting it would silently do nothing at all rather
  than failing - this software's choice, and one the first such number (a
  wavelength, which a mode divides by) needs anyway.

  REFUSED, NOT RAISED, in words naming the number: False with a message to
  show; True with an empty one and the value out. }
function AxisValueFromText(const AParameter: TAxisParameterInfo;
    const AText: string; out AValue: double; out ARefusal: string): boolean;

implementation

uses
    typed_number;

var
    Registry: TAxisModeClasses;
    Parameters: TAxisParameterInfos;

class function TAxisMode.CaptionFor(ADimension: TAxisDimension): string;
begin
    Result := Info.Caption;
end;

class function TAxisMode.TopicFor(ADimension: TAxisDimension): string;
begin
    Result := Info.Topic;
end;

function DimensionWord(ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := 'argument';
        adValue:    Result := 'value';
        adLoss:     Result := 'R-factor';
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
    Param: TAxisParameterInfo;
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
        else if Assigned(AResolves) then
            //  PER COORDINATE: one entry may mean a different thing on each.
            for D := Low(TAxisDimension) to High(TAxisDimension) do
                if (D in Info.Dimensions) and
                    not AResolves(AModes[i].TopicFor(D)) then
                    Add(Result, Name_ + ' names the topic "' +
                        AModes[i].TopicFor(D) + '" for the ' + DimensionWord(D) +
                        ', which resolves to no explanation');
        if Info.ParameterRequired and (Info.Parameter = apNone) then
            Add(Result, Name_ + ' requires a parameter without saying which, ' +
                'so nothing could ever supply it');
        if (Info.Parameter = apValue) and (Trim(Info.ParameterName) = '') then
            Add(Result, Name_ + ' reads a number without naming it, so nothing ' +
                'could ever supply it')
        else if (Info.Parameter = apValue) and
            not FindAxisParameter(Info.ParameterName, Param) then
            Add(Result, Name_ + ' reads the number "' + Info.ParameterName +
                '", which no module describes (RegisterAxisParameter), so the ' +
                'window could not ask for it');
    end;
end;

function AxisModeFindings(AResolves: TTopicResolves): TStringArray;
begin
    Result := AxisModeFindingsFor(Registry, AResolves);
end;

function IndexOfParameter(const AName: string): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(Parameters) do
        if SameText(Parameters[i].Name, Trim(AName)) then
            Exit(i);
end;

procedure RegisterAxisParameter(const AInfo: TAxisParameterInfo);
var
    Existing: longint;
begin
    if Trim(AInfo.Name) = '' then
        raise EAxisModeRegistration.Create(
            'a number an axis reads was described without a name');
    if Trim(AInfo.Caption) = '' then
        raise EAxisModeRegistration.Create('the number "' + AInfo.Name +
            '" an axis reads was described without a caption, so the window ' +
            'could not say what it asks for');
    Existing := IndexOfParameter(AInfo.Name);
    if Existing >= 0 then
    begin
        if (Parameters[Existing].Caption = AInfo.Caption) and
            (Parameters[Existing].Example = AInfo.Example) then
            Exit;
        raise EAxisModeRegistration.Create('the number "' + AInfo.Name +
            '" an axis reads is described twice, as "' +
            Parameters[Existing].Caption + '" and as "' + AInfo.Caption + '"');
    end;
    SetLength(Parameters, Length(Parameters) + 1);
    Parameters[High(Parameters)] := AInfo;
    Parameters[High(Parameters)].Name := Trim(AInfo.Name);
end;

function FindAxisParameter(const AName: string;
    out AInfo: TAxisParameterInfo): boolean;
var
    Index: longint;
begin
    AInfo := Default(TAxisParameterInfo);
    Index := IndexOfParameter(AName);
    Result := Index >= 0;
    if Result then
        AInfo := Parameters[Index];
end;

function RegisteredAxisParameters: TAxisParameterInfos;
begin
    Result := Parameters;
end;

function AxisValueOf(const AValues: TAxisValues; const AName: string): double;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(AValues) do
        if SameText(AValues[i].Name, AName) then
            Exit(AValues[i].Value);
end;

procedure PutAxisValue(var AValues: TAxisValues; const AName: string;
    AValue: double);
var
    i: longint;
begin
    for i := 0 to High(AValues) do
        if SameText(AValues[i].Name, AName) then
        begin
            AValues[i].Value := AValue;
            Exit;
        end;
    SetLength(AValues, Length(AValues) + 1);
    AValues[High(AValues)].Name := AName;
    AValues[High(AValues)].Value := AValue;
end;

function AxisValueFromText(const AParameter: TAxisParameterInfo;
    const AText: string; out AValue: double; out ARefusal: string): boolean;
var
    Noun: string;
begin
    ARefusal := '';
    AValue := 0;
    Noun := LowerCase(AParameter.Caption);
    //  REFUSED, NOT RAISED. The code this came from swapped the process-wide
    //  decimal separator around a StrToFloat that raises on a typo, so the
    //  separator was never put back and the exception reached the top-level
    //  handler - which logs at Fatal and STOPS THE SERVER POLL. A typo in a text
    //  box disconnected the user from the compute server. See findings.md.
    if not TryTypedNumber(AText, AValue) then
    begin
        //  Names the separator, because a comma is what a great many keyboards
        //  and locales produce and the refusal is otherwise a mystery.
        ARefusal := 'The ' + Noun + ' must be a number, written with a full ' +
            'stop';
        if Trim(AParameter.Example) <> '' then
            ARefusal := ARefusal + ' - for example ' + Trim(AParameter.Example);
        ARefusal := ARefusal + '.';
        AValue := 0;
        Exit(False);
    end;
    if AValue <= 0 then
    begin
        ARefusal := 'The ' + Noun + ' must be greater than zero.';
        AValue := 0;
        Exit(False);
    end;
    Result := True;
end;

end.
