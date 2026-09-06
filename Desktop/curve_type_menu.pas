// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the curve-type menu is laid out: which group each type goes in,
and in what order the groups appear.)

THE MENU IS THE MODEL LIBRARY. Every curve type a build can fit is in it, and
where each one sits is how a user finds it. Four decisions govern that, and all
four were inside a method that creates `TMenuItem`s:

  * WHICH GROUP a type belongs to - its own if it declares one, Standard if not,
    and the user's own group for the entry that creates user curves;
  * WHAT ORDER the groups appear in - the everyday list first, whatever a curve
    pack contributes between, the user's own curves last. REGISTRATION ORDER
    DECIDES NOTHING, which is the whole point: a module registering earlier or
    later must not move the menu about under the user;
  * WHETHER an entry can be ticked. The entry that CREATES a user curve names no
    curve one can pick, so it is never ticked - the curve it creates carries the
    tick instead;
  * WHICH entry is ticked now.

None of it could be tested: building the menu needs a window, and the framework
ships no curve pack, so the grouping was only ever exercised with one group in
it.
}
unit curve_type_menu;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, named_points_set;

const
    { The group a curve type that declares none belongs to. }
    StandardCurveGroup = 'Standard';
    { The group the user's own curve types live in. }
    UserCurveGroup = 'User';

type
    { What one registered curve type contributes to the menu. Gathered from the
      registry by the caller, so that nothing here has to iterate it. }
    TCurveTypeInfo = record
        Id: TCurveTypeId;
        { What the type calls itself. }
        Name: string;
        { The group it declares, or '' for none. }
        Group: string;
        { The registry's handle for it, which comes back on a click. }
        Tag: longint;
        { True for the one entry that CREATES a user curve rather than naming
          one. }
        IsUserCurveFactory: boolean;
    end;

    TCurveTypeInfos = array of TCurveTypeInfo;

    { One entry of the menu, decided. }
    TCurveMenuEntry = record
        Group: string;
        Caption: string;
        { False only for the entry that creates a user curve. }
        Checkable: boolean;
        Checked: boolean;
        Tag: longint;
    end;

    TCurveMenuEntries = array of TCurveMenuEntry;

{ The entries, in the order the types were given.

  ACreateCaption is what the user-curve factory is captioned as - it is an
  action, not a curve, and the wording belongs to the window. }
function CurveMenuEntries(const ATypes: TCurveTypeInfos;
    const ASelected: TCurveTypeId;
    const ACreateCaption: string): TCurveMenuEntries;

{ Which group an entry belongs to, given what the type declares. }
function CurveMenuGroupOf(const AType: TCurveTypeInfo): string;

{ Whether a stored user curve can be selected at all.

  A USER CURVE IS A FORMULA. One saved without it - by an older version, or by a
  session interrupted between naming the curve and giving it an expression - is
  an entry in the menu that cannot become a curve. Selecting it used to reach the
  engine and fail an assertion there, which names a source line in the optimiser
  rather than the menu item the user clicked.

  So the menu asks first, and the refusal says what to do about it. Whitespace
  counts as nothing: a formula of spaces evaluates to the same nothing, and the
  user cannot see the difference between the two. }
function UserCurveIsUsable(const AExpression: string): boolean;

type
    { One row of the curve types shown as a FLAT LIST rather than as a menu.

      A list box has no submenus, so the grouping a menu expresses by nesting
      has to be expressed by rows: a header row per group, then its types. The
      same entries either way - this is a second projection of what
      CurveMenuEntries already decided, not a second decision. }
    TCurveListRow = record
        Caption: string;
        { A group name. Drawn differently, and never selectable: it names no
          curve, so choosing it would ask the engine to fit a heading. }
        IsHeader: boolean;
        Selected: boolean;
        { The registry's handle, which comes back on a click. Meaningless on a
          header. }
        Tag: longint;
    end;

    TCurveListRows = array of TCurveListRow;

{ The entries flattened into one sequence, group headers included, in the order
  CurveMenuGroupOrder gives. }
function CurveTypeListRows(const AEntries: TCurveMenuEntries): TCurveListRows;

{ The nearest selectable row at or after AIndex, or -1 when there is none.

  WHAT IT IS FOR: a click lands on whatever row is under the pointer, and a
  header is a row. Without this the user clicks a group name and either nothing
  happens or - worse - the previous selection is silently kept while the
  highlight moves, so the list and the model disagree about which type is
  chosen. Searching FORWARD from the click, because a header is followed by the
  types it heads: the row the user was reaching for is the next one down. }
function NextSelectableRow(const ARows: TCurveListRows;
    AIndex: longint): longint;

{ Which row carries the selected type, or -1. }
function SelectedCurveRow(const ARows: TCurveListRows): longint;

{ The groups, in the order they should appear in the menu. Each named once.

  Standard first, User last, everything else between in the order it was first
  seen - so a curve pack's own group keeps a stable place without the framework
  having to know its name. }
procedure CurveMenuGroupOrder(const AEntries: TCurveMenuEntries;
    ADest: TStrings);

implementation

function UserCurveIsUsable(const AExpression: string): boolean;
begin
    Result := Trim(AExpression) <> '';
end;

function CurveTypeListRows(const AEntries: TCurveMenuEntries): TCurveListRows;
var
    Order: TStringList;
    i, j, N: longint;

    procedure AddRow(const ACaption: string; AIsHeader, ASelected: boolean;
        ATag: longint);
    begin
        SetLength(Result, N + 1);
        Result[N].Caption := ACaption;
        Result[N].IsHeader := AIsHeader;
        Result[N].Selected := ASelected;
        Result[N].Tag := ATag;
        Inc(N);
    end;

begin
    Result := nil;
    N := 0;
    Order := TStringList.Create;
    try
        //  THE SAME ORDER THE MENU USES, from the same function. Two orders for
        //  one library is two things to keep in step, and the user would see
        //  them disagree.
        CurveMenuGroupOrder(AEntries, Order);
        for i := 0 to Order.Count - 1 do
        begin
            AddRow(Order[i], True, False, 0);
            for j := 0 to High(AEntries) do
                if AEntries[j].Group = Order[i] then
                    AddRow(AEntries[j].Caption, False, AEntries[j].Checked,
                        AEntries[j].Tag);
        end;
    finally
        Order.Free;
    end;
end;

function NextSelectableRow(const ARows: TCurveListRows;
    AIndex: longint): longint;
var
    i: longint;
begin
    Result := -1;
    if AIndex < 0 then
        //  A list box with nothing selected reports -1. Start at the top rather
        //  than answering "nothing", so the first real row is reachable.
        AIndex := 0;
    for i := AIndex to High(ARows) do
        if not ARows[i].IsHeader then
            Exit(i);
end;

function SelectedCurveRow(const ARows: TCurveListRows): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(ARows) do
        if (not ARows[i].IsHeader) and ARows[i].Selected then
            Exit(i);
end;

function CurveMenuGroupOf(const AType: TCurveTypeInfo): string;
begin
    if AType.IsUserCurveFactory then
        //  The entry that creates user curves heads the group they live in, so
        //  everything about user curves is in one place.
        Result := UserCurveGroup
    else if Trim(AType.Group) = '' then
        //  Which is every type the framework itself ships.
        Result := StandardCurveGroup
    else
        Result := AType.Group;
end;

function CurveMenuEntries(const ATypes: TCurveTypeInfos;
    const ASelected: TCurveTypeId;
    const ACreateCaption: string): TCurveMenuEntries;
var
    i: longint;
begin
    SetLength(Result, Length(ATypes));
    for i := 0 to High(ATypes) do
    begin
        Result[i].Group := CurveMenuGroupOf(ATypes[i]);
        Result[i].Tag := ATypes[i].Tag;
        if ATypes[i].IsUserCurveFactory then
        begin
            //  Captioned as the action it performs, and never ticked: it names
            //  no curve one can pick, and the curve it creates carries the tick.
            Result[i].Caption := ACreateCaption;
            Result[i].Checkable := False;
            Result[i].Checked := False;
        end
        else
        begin
            Result[i].Caption := ATypes[i].Name;
            //  EVERY type, not only the selected one. Which type is selected is
            //  a tick that MOVES, so each entry has to be the kind of widget
            //  that can carry one from the moment it is created.
            Result[i].Checkable := True;
            Result[i].Checked := IsEqualGUID(ASelected, ATypes[i].Id);
        end;
    end;
end;

procedure CurveMenuGroupOrder(const AEntries: TCurveMenuEntries;
    ADest: TStrings);
var
    i: longint;
    Seen: TStringList;

    procedure Take(const AGroup: string);
    begin
        if AGroup = '' then
            Exit;
        if Seen.IndexOf(AGroup) >= 0 then
            Exit;
        Seen.Add(AGroup);
        ADest.Add(AGroup);
    end;

begin
    ADest.Clear;
    Seen := TStringList.Create;
    try
        //  The everyday list first - and only if something is in it, so a build
        //  whose every type declares a group of its own does not show an empty
        //  Standard.
        for i := 0 to High(AEntries) do
            if AEntries[i].Group = StandardCurveGroup then
            begin
                Take(StandardCurveGroup);
                Break;
            end;

        //  Then whatever a curve pack contributed, in the order first seen.
        for i := 0 to High(AEntries) do
            if (AEntries[i].Group <> StandardCurveGroup) and
                (AEntries[i].Group <> UserCurveGroup) then
                Take(AEntries[i].Group);

        //  The user's own last.
        for i := 0 to High(AEntries) do
            if AEntries[i].Group = UserCurveGroup then
            begin
                Take(UserCurveGroup);
                Break;
            end;
    finally
        Seen.Free;
    end;
end;

end.
