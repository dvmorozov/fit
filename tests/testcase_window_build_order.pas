// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The main window is built in an order where every step comes after
what it needs.)

The window runs window_build_order's steps in the order that unit states; this
holds the order to what each step says it needs, so a step moved before its
prerequisites fails here by name instead of inside FormCreate, before there is
a window to say so.
}
unit testcase_window_build_order;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, window_build_order;

type
    TWindowBuildOrderTest = class(TTestCase)
    published
        procedure EveryStepComesAfterWhatItNeeds;
        procedure AStepMovedBeforeWhatItNeedsIsNamed;
        procedure AStepLeftOutOrRunTwiceIsNamed;
        procedure TheCommandTableComesAfterEverythingItResolves;
    end;

implementation

procedure FailAll(const AFindings: TStringArray);
var
    i: longint;
begin
    for i := 0 to High(AFindings) do
        raise EAssertionFailedError.Create(AFindings[i]);
end;

procedure TWindowBuildOrderTest.EveryStepComesAfterWhatItNeeds;
begin
    FailAll(WindowBuildFindings(WindowBuildOrder));
end;

function Swapped(const AOrder: TWindowBuildOrder;
    A, B: TWindowBuildStep): TWindowBuildOrder;
var
    i: longint;
begin
    Result := Copy(AOrder);
    for i := 0 to High(Result) do
        if Result[i] = A then
            Result[i] := B
        else if Result[i] = B then
            Result[i] := A;
end;

procedure TWindowBuildOrderTest.AStepMovedBeforeWhatItNeedsIsNamed;
var
    Findings: TStringArray;
begin
    //  The command table resolves Clear Model's action, so it cannot come
    //  first.
    Findings := WindowBuildFindings(
        Swapped(WindowBuildOrder, wbCodedActions, wbCommandTable));
    AssertTrue('refused', Length(Findings) > 0);
    AssertTrue(Findings[0],
        Pos('wbCommandTable runs before wbCodedActions', Findings[0]) > 0);
end;

procedure TWindowBuildOrderTest.AStepLeftOutOrRunTwiceIsNamed;
var
    Order: TWindowBuildOrder;
    Findings: TStringArray;
    Text: string;
    i: longint;
begin
    Order := WindowBuildOrder;
    //  The layout's step replaced by a second run of the theme's.
    for i := 0 to High(Order) do
        if Order[i] = wbLayout then
            Order[i] := wbTheme;
    Findings := WindowBuildFindings(Order);
    Text := '';
    for i := 0 to High(Findings) do
        Text := Text + Findings[i] + '; ';
    AssertTrue(Text, Pos('wbTheme is run twice', Text) > 0);
    AssertTrue(Text, Pos('wbLayout is never run', Text) > 0);
end;

procedure TWindowBuildOrderTest.TheCommandTableComesAfterEverythingItResolves;
begin
    //  The names it resolves are the axis entries', the modules' and the coded
    //  actions' - the ordering trap the name registers once fell into.
    AssertTrue(wbAxisMenus in WindowBuildNeeds(wbCommandTable));
    AssertTrue(wbModuleMenus in WindowBuildNeeds(wbCommandTable));
    AssertTrue(wbCodedActions in WindowBuildNeeds(wbCommandTable));
    AssertTrue(wbNames in WindowBuildNeeds(wbAxisMenus));
end;

initialization
    RegisterTest('unit', TWindowBuildOrderTest);
end.
