// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the parameters table remembers when it is rebuilt, and how its
saved sizes follow rows and columns coming and going.)

THE TABLE IS REBUILT CONSTANTLY. It is a view of a model the engine owns, and
every fit cycle, every pick, every change of curve type throws the grid away and
draws it again. Anything the user did to the grid itself - dragged a column
wider, scrolled down, selected a block of cells, put the cursor somewhere - lives
in this list rather than in the widget, because the widget does not survive.

WHAT GOING WRONG LOOKS LIKE. The user drags a column to fit their numbers, a fit
cycle completes, and the column is back where it was. Or the table jumps to the
top while they are reading row forty. Or - worse and quieter - a curve is deleted
and every saved width after it stays with the column it used to describe, so the
columns are progressively mis-sized in a way that looks like the grid drawing
badly rather than like an array that was not shifted.

None of it raises. None of it is logged. It is felt as an application that
fidgets, and that is exactly the kind of defect nobody files a report about.

THE SIZES ARE ALSO SEEDED LAZILY, the first time one is asked for, so a list that
has never been shown still answers and a column added later still has a width.
That laziness is worth pinning: a seed that ran too early would quote every width
in the pixels of whatever display was current when the list was built.
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
        //  The widths, and when they are seeded.
        procedure EveryColumnHasAWidthBeforeAnybodySetsOne;
        procedure AWidthTheUserSetIsRemembered;
        procedure SettingOneWidthLeavesItsNeighbourAlone;
        procedure AColumnOutsideTheTableIsRefused;
        procedure ANegativeColumnIsRefusedToo;

        //  Whether the widths are the user's or the defaults.
        procedure WidthsAreAutomaticUntilOneIsSet;
        procedure SettingOneMakesThemAllTheUsers;
        procedure HeightsAreTrackedSeparatelyFromWidths;

        //  The heights.
        procedure EveryRowHasAHeight;
        procedure AHeightTheUserSetIsRemembered;

        //  Following the display's density.
        procedure ADefaultWidthFollowsTheDisplaysDensity;
        procedure ADefaultHeightDoesToo;
        procedure ASavedWidthIsNotRescaled;

        //  Where the user was looking.
        procedure TheCursorIsRemembered;
        procedure TheScrollPositionIsRemembered;
        procedure TheSelectedBlockIsRemembered;
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

{ ---- the widths, and when they are seeded ---------------------------------- }

procedure TCurveListMemoryTest.EveryColumnHasAWidthBeforeAnybodySetsOne;
var
    i: longint;
begin
    //  SEEDED LAZILY, on the first ask. A list that has never been shown still
    //  answers, and a column that appeared when a curve was added still has a
    //  width - so the grid never draws a column of nothing.
    GivenTwoCurves;
    for i := 0 to FList.GetColCount - 1 do
        AssertTrue(Format('column %d has a width', [i]),
            FList.GetColWidth(i) > 0);
end;

procedure TCurveListMemoryTest.AWidthTheUserSetIsRemembered;
begin
    //  THE WHOLE POINT OF SAVING THEM. The user drags a column to fit their
    //  numbers and the width has to outlive the next rebuild of the table -
    //  which happens on every fit cycle, several times a second.
    GivenTwoCurves;
    FList.SaveColWidth(0, 137);
    AssertEquals('the width came back', 137, FList.GetColWidth(0));
end;

procedure TCurveListMemoryTest.SettingOneWidthLeavesItsNeighbourAlone;
var
    Other: longint;
begin
    //  A width written to the wrong slot moves a column the user did not touch,
    //  and they cannot tell which of their drags did it.
    GivenTwoCurves;
    Other := FList.GetColWidth(1);
    FList.SaveColWidth(0, 137);
    AssertEquals('the neighbour is untouched', Other, FList.GetColWidth(1));
end;

procedure TCurveListMemoryTest.AColumnOutsideTheTableIsRefused;
var
    Raised: boolean;
begin
    //  THE GRID ASKS ABOUT THE COLUMN IT IS DRAWING, and during a rebuild it
    //  can ask about one that has just gone. Reading past the array would hand
    //  back whatever is next in memory as a width - a column a few million
    //  pixels wide, or one of zero.
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

{ ---- whether the widths are the user's or the defaults --------------------- }

procedure TCurveListMemoryTest.WidthsAreAutomaticUntilOneIsSet;
begin
    //  THIS FLAG DECIDES WHETHER THE SETTINGS FILE CARRIES WIDTHS AT ALL. Set
    //  too early, every table is written out at its defaults - and those
    //  defaults then stop following the display's density, because a saved
    //  width is taken as the user's and is never rescaled.
    GivenTwoCurves;
    AssertTrue('nothing saved yet', FList.AutoWidths);
end;

procedure TCurveListMemoryTest.SettingOneMakesThemAllTheUsers;
begin
    //  ONE FLAG FOR THE WHOLE TABLE, not one per column: the user who sized one
    //  column has taken the table over, and the rest keeping their automatic
    //  behaviour would have them jump the next time the font changed.
    GivenTwoCurves;
    FList.SaveColWidth(0, 137);
    AssertFalse('now they are the user''s', FList.AutoWidths);
end;

procedure TCurveListMemoryTest.HeightsAreTrackedSeparatelyFromWidths;
begin
    //  Two flags, because the two are set by different gestures. Sharing one
    //  would have sizing a column freeze the row heights at whatever density
    //  was current.
    GivenTwoCurves;
    FList.SaveColWidth(0, 137);
    AssertTrue('heights are still automatic', FList.AutoHeights);
end;

{ ---- the heights ----------------------------------------------------------- }

procedure TCurveListMemoryTest.EveryRowHasAHeight;
begin
    GivenTwoCurves;
    AssertTrue('a height', FList.GetRowHeight(0) > 0);
end;

procedure TCurveListMemoryTest.AHeightTheUserSetIsRemembered;
begin
    GivenTwoCurves;
    FList.SaveRowHeight(0, 42);
    AssertEquals(42, FList.GetRowHeight(0));
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

procedure TCurveListMemoryTest.ASavedWidthIsNotRescaled;
begin
    //  A WIDTH THE USER CHOSE IS IN REAL PIXELS ALREADY. Scaling it again on a
    //  scaled display would multiply their choice by the density every time the
    //  table was rebuilt, and the column would walk off the edge of the window.
    GivenTwoCurves;
    FList.SaveColWidth(0, 137);
    CurveListPixelsPerInch := SCALED_PPI;
    AssertEquals('exactly what was saved', 137, FList.GetColWidth(0));
end;

{ ---- where the user was looking -------------------------------------------- }

procedure TCurveListMemoryTest.TheCursorIsRemembered;
begin
    //  The cell the user is editing. Lost on a rebuild, the caret jumps out of
    //  the cell mid-keystroke and the rest of the number is typed nowhere.
    GivenTwoCurves;
    FList.SaveCol(2);
    FList.SaveRow(1);
    AssertEquals('the column', 2, FList.SavedCol);
    AssertEquals('and the row', 1, FList.SavedRow);
end;

procedure TCurveListMemoryTest.TheScrollPositionIsRemembered;
begin
    //  Which cell is at the top left. Without it the table jumps to the top on
    //  every fit cycle while the user is reading row forty.
    GivenTwoCurves;
    FList.SaveLeftCol(1);
    FList.SaveTopRow(1);
    AssertEquals('the left column', 1, FList.SavedLeftCol);
    AssertEquals('and the top row', 1, FList.SavedTopRow);
end;

procedure TCurveListMemoryTest.TheSelectedBlockIsRemembered;
var
    L, T, R, B: longint;
begin
    //  ALL FOUR EDGES, which is why they are set and read as a group: three
    //  restored and one forgotten is a selection that grows or shrinks by
    //  itself between one redraw and the next.
    GivenTwoCurves;
    FList.SaveSelectionRect(1, 2, 3, 4);
    FList.GetSelectionRect(L, T, R, B);
    AssertEquals('left', 1, L);
    AssertEquals('top', 2, T);
    AssertEquals('right', 3, R);
    AssertEquals('bottom', 4, B);
end;

procedure TCurveListMemoryTest.AnUntouchedGridStartsOnTheFirstDataCell;
var
    L, T, R, B: longint;
begin
    //  NOT THE ORIGIN - the first cell the user could actually edit, past the
    //  header row and the name column. The grid applies these on every rebuild
    //  including the first, so a list that reported 0,0 would open with the
    //  cursor parked on a column heading and the first keystroke would go
    //  nowhere.
    //
    //  The selection starts as that one cell rather than empty, for the same
    //  reason: a grid with no selection has nothing for Copy to copy.
    AssertEquals('the cursor is past the name column',
        FList.GetFixedCols, FList.SavedCol);
    AssertEquals('and past the header row',
        FList.GetFixedRows, FList.SavedRow);
    AssertEquals('the view is scrolled to it',
        FList.GetFixedCols, FList.SavedLeftCol);
    AssertEquals('in both directions',
        FList.GetFixedRows, FList.SavedTopRow);

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
