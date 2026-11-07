// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The sizes and the starting view the parameters table is drawn with.)

THE TABLE IS REBUILT CONSTANTLY: every fit cycle, every pick, every change of
curve type makes a new curve list, and the grid keeps its own place across them
(Desktop/curve_list_grid.pas). So the list remembers nothing the user did to the
grid - the members that saved sizes, the cursor and the scroll position had no
caller once the wrapper's Release, which could never be called, was deleted -
and what is pinned here is what every list answers: a width for every column and
a height for every row at the display's density, a refusal for a column that is
not there, and the first cell a newly shown table starts on.
}
unit testcase_curve_list_memory;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    amplitude_curve_parameter, curve_list, persistent_curve_parameters,
    persistent_curve_parameter_container, special_curve_parameter;

type
    TCurveListMemoryTest = class(TTestCase)
    private
        FList: TCurveListBase;
        FSavedPPI: longint;
        { A curve with three named parameters, so the table has columns. }
        function AddCurve(const AName: string): longint;
        procedure GivenTwoCurves;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The widths.
        procedure EveryColumnHasAWidth;
        procedure AColumnOutsideTheTableIsRefused;
        procedure ANegativeColumnIsRefusedToo;

        //  The heights.
        procedure EveryRowHasAHeight;

        //  Following the display's density.
        procedure ADefaultWidthFollowsTheDisplaysDensity;
        procedure ADefaultHeightDoesToo;
        procedure AColumnsWidthIsTheSeedAtTheDisplaysDensity;

        //  Where a newly shown table starts.
        procedure AnUntouchedGridStartsOnTheFirstDataCell;
    end;

implementation

const
    { A scaled display, and an awkward one: 144 is 1.5x, so a default computed
      by rounding rather than by scaling would be caught. }
    SCALED_PPI = 144;

procedure TCurveListMemoryTest.SetUp;
begin
    FList := TCurveListBase.Create;
    FSavedPPI := CurveListPixelsPerInch;
end;

procedure TCurveListMemoryTest.TearDown;
begin
    //  RESTORED FIRST. It is a unit-level variable standing in for the screen,
    //  so a test that left it scaled would silently rescale every default in
    //  every test that ran afterwards.
    CurveListPixelsPerInch := FSavedPPI;
    FreeAndNil(FList);
end;

function TCurveListMemoryTest.AddCurve(const AName: string): longint;
var
    Params: Curve_parameters;
    P: TSpecialCurveParameter;
    i: longint;
    Names: array[0..2] of string;
begin
    Names[0] := 'A';
    Names[1] := 'sigma';
    Names[2] := AName;
    Params := Curve_parameters(FList.CreateNewObject);
    for i := 0 to 2 do
    begin
        P := TAmplitudeCurveParameter.Create;
        P.Name := Names[i];
        P.Type_ := Variable;
        P.Value := i + 1;
        P.Error := -1;
        TPersistentCurveParameterContainer(Params.Params.Add).Parameter := P;
    end;
    Result := FList.Add(Params);
end;

procedure TCurveListMemoryTest.GivenTwoCurves;
begin
    AddCurve('x0');
    AddCurve('x0');
end;

{ ---- the widths ------------------------------------------------------------ }

procedure TCurveListMemoryTest.EveryColumnHasAWidth;
var
    i: longint;
begin
    //  A list that has never been shown still answers, and a column that
    //  appeared when a curve was added has a width - so the grid never draws a
    //  column of nothing.
    GivenTwoCurves;
    for i := 0 to FList.GetColCount - 1 do
        AssertTrue(Format('column %d has a width', [i]),
            FList.GetColWidth(i) > 0);
end;

procedure TCurveListMemoryTest.AColumnOutsideTheTableIsRefused;
var
    Raised: boolean;
begin
    //  THE GRID ASKS ABOUT THE COLUMN IT IS DRAWING, and during a rebuild it
    //  can ask about one that has just gone - a mistake in the caller that a
    //  width quietly answered would hide.
    GivenTwoCurves;
    Raised := False;
    try
        FList.GetColWidth(FList.GetColCount + 5);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TCurveListMemoryTest.ANegativeColumnIsRefusedToo;
var
    Raised: boolean;
begin
    //  -1 is what "no column" looks like everywhere in this program, and it
    //  reaches here whenever a redraw outruns a selection change.
    GivenTwoCurves;
    Raised := False;
    try
        FList.GetColWidth(-1);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

{ ---- the heights ----------------------------------------------------------- }

procedure TCurveListMemoryTest.EveryRowHasAHeight;
begin
    GivenTwoCurves;
    AssertTrue('a height', FList.GetRowHeight(0) > 0);
end;

{ ---- following the display's density --------------------------------------- }

procedure TCurveListMemoryTest.ADefaultWidthFollowsTheDisplaysDensity;
var
    At96, At144: longint;
begin
    //  QUOTED AT 96 DPI AND SCALED FROM THERE. Written as device pixels, the
    //  parameters table came up with columns a third of the width of their
    //  contents on a scaled display - which is the defect the scaling was added
    //  for, and it is invisible to anybody developing at 96.
    GivenTwoCurves;
    CurveListPixelsPerInch := 96;
    At96 := FList.GetColWidthByDefault(0);
    CurveListPixelsPerInch := SCALED_PPI;
    At144 := FList.GetColWidthByDefault(0);
    AssertTrue(Format('one and a half times as wide (%d vs %d)', [At144, At96]),
        At144 > At96);
end;

procedure TCurveListMemoryTest.ADefaultHeightDoesToo;
begin
    //  Both directions, because a row that did not grow with the font clips its
    //  own text - and the text does grow, because the LCL scales that for free.
    CurveListPixelsPerInch := 96;
    AssertEquals('20 pixels at 96', 20, FList.GetRowHeightByDefault(0));
    CurveListPixelsPerInch := SCALED_PPI;
    AssertEquals('and 30 at 144', 30, FList.GetRowHeightByDefault(0));
end;

procedure TCurveListMemoryTest.AColumnsWidthIsTheSeedAtTheDisplaysDensity;
begin
    //  WHAT THE GRID DRAWS A COLUMN AT when it does not size it to contents:
    //  the seed, at the density in force when it asks.
    GivenTwoCurves;
    CurveListPixelsPerInch := SCALED_PPI;
    AssertEquals('one and a half times 64', 96, FList.GetColWidth(1));
    AssertEquals('and the height, one and a half times 20', 30,
        FList.GetRowHeight(1));
end;

{ ---- where a newly shown table starts -------------------------------------- }

procedure TCurveListMemoryTest.AnUntouchedGridStartsOnTheFirstDataCell;
var
    L, T, R, B: longint;
begin
    //  NOT THE ORIGIN - the first cell the user could actually edit, past the
    //  header row and the name column. The grid applies these to the first
    //  list it shows, so a list that reported 0,0 would open with the
    //  cursor parked on a column heading and the first keystroke would go
    //  nowhere.
    //
    //  The selection starts as that one cell rather than empty, for the same
    //  reason: a grid with no selection has nothing for Copy to copy.
    AssertEquals('the cursor is past the name column',
        FList.GetFixedCols, FList.GetCol);
    AssertEquals('and past the header row',
        FList.GetFixedRows, FList.GetRow);
    AssertEquals('the view is scrolled to it',
        FList.GetFixedCols, FList.GetLeftCol);
    AssertEquals('in both directions',
        FList.GetFixedRows, FList.GetTopRow);

    FList.GetSelectionRect(L, T, R, B);
    AssertEquals('the selection is that cell: left',
        FList.GetFixedCols, L);
    AssertEquals('top', FList.GetFixedRows, T);
    AssertEquals('and it is one cell wide', L, R);
    AssertEquals('and one cell tall', T, B);
end;

initialization
    //  A unit test: a curve list in memory. No grid and no screen - the
    //  display's density is a unit-level variable standing in for one.
    RegisterTest('unit', TCurveListMemoryTest);
end.
