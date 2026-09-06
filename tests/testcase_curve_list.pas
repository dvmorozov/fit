// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the parameter table shows, and what it accepts back.)

WHY THESE TESTS DID NOT EXIST BEFORE. Every rule checked here used to live inside
a method that took a TStringGrid, and an LCL grid cannot be driven headlessly -
sizing one with no parent raises "Canvas does not allow drawing" before a single
cell is written, whichever way the row height and column width are pinned first
(docs/contributing/findings.md records the investigation). So the rules could
only be checked by reading them, and the one that mattered most - a wrong number
under a plausible column heading - had already reached a user that way.

Server/curve_list.pas is the model those rules were moved into, and this is what
makes the move worth having rather than merely tidy.
}
unit testcase_curve_list;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    amplitude_curve_parameter, curve_list, persistent_curve_parameters,
    persistent_curve_parameter_container, special_curve_parameter;

type
    TCurveListTest = class(TTestCase)
    private
        FList: TCurveListBase;
        { Appends a curve carrying the named parameters, each of the given type and
          value. Returns its row index. }
        function AddCurve(const ANames: array of string;
            const ATypes: array of TParameterType;
            const AValues: array of double): longint;
        procedure SetParamError(const ARow: longint; const AName: string;
            const AError: double);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The columns
        procedure ColumnsAreTheUnionOfParameterNames;
        procedure ColumnOrderIsFirstSeen;
        procedure TheArgumentIsNotAColumn;
        procedure ColumnNamesAreCaseInsensitive;
        procedure AnEmptyListHasNoColumns;

        //  What a cell reads
        procedure ACellReadsItsParameterValue;
        procedure AParameterTheCurveLacksReadsBlankNotZero;
        procedure AnEstimatedUncertaintyIsAppended;
        procedure AnAbsentUncertaintyIsNotAppended;
        procedure ACellOutsideTheDataIsBlank;

        //  What a cell accepts back
        procedure AValidNumberIsApplied;
        procedure ANonNumberIsRefusedAndChangesNothing;
        procedure AColumnTheCurveLacksIsAcceptedAndIgnored;
        procedure ACellOutsideTheDataIsRefused;

        //  Which columns may be edited
        procedure AFittedColumnIsEditable;
        procedure AColumnEveryCurveComputesIsNotEditable;
        procedure AColumnFittedByAnyCurveIsEditable;
        procedure TheArgumentColumnIsNotEditable;
        procedure AColumnOutsideTheDataIsNotEditable;

        //  What kind of parameter a column holds
        procedure ColumnParameterTypeReportsTheType;
        procedure ColumnParameterTypeRefusesAColumnTheCurveLacks;
        procedure ColumnParameterTypeRefusesAnOutOfRangeCell;

        //  Geometry the table is drawn from
        procedure RowCountFollowsTheList;
        procedure ColumnCountFollowsTheParameterNames;

        //  The seed sizes, and the display scaling that was unreachable
        procedure SeedSizesScaleWithTheDisplay;
        procedure SavedWidthsSurviveAndSuppressAutoSizing;
        procedure SavedSelectionRoundTrips;

        //  The saved row heights must follow the rows they belong to
        procedure DeletingARowDeletesItsSavedHeight;
        procedure InsertingARowInsertsAHeightAtThatPosition;
        procedure EmptyingTheListLeavesOneHeightForTheBlankRow;

        //  The array primitives the saved sizes sit on
        procedure DeletingAnItemShiftsTheRestDown;
        procedure InsertingAnItemShiftsTheRestUp;
        procedure AnOutOfRangeItemIndexIsRefused;
    end;

implementation

const
    { A scaled display, and a deliberately awkward one: 144 is 1.5x, so a seed
      that rounded instead of dividing would be caught. }
    SCALED_PPI = 144;

function TCurveListTest.AddCurve(const ANames: array of string;
    const ATypes: array of TParameterType;
    const AValues: array of double): longint;
var
    Params: Curve_parameters;
    P: TSpecialCurveParameter;
    i: longint;
begin
    Params := Curve_parameters(FList.CreateNewObject);
    for i := 0 to High(ANames) do
    begin
        //  A concrete subclass: TSpecialCurveParameter itself is the contract, and
        //  the amplitude one is the plainest thing that satisfies it. The container
        //  takes ownership and frees the placeholder it was constructed with.
        P := TAmplitudeCurveParameter.Create;
        P.Name := ANames[i];
        P.Type_ := ATypes[i];
        P.Value := AValues[i];
        //  Negative means "no backend estimated one", which is what the native
        //  engine leaves behind.
        P.Error := -1;
        TPersistentCurveParameterContainer(Params.Params.Add).Parameter := P;
    end;
    Result := FList.Add(Params);
end;

procedure TCurveListTest.SetParamError(const ARow: longint;
    const AName: string; const AError: double);
var
    Params: Curve_parameters;
begin
    Params := Curve_parameters(FList.Items[ARow]);
    Params.FindByName(AName).Error := AError;
end;

procedure TCurveListTest.SetUp;
begin
    FList := TCurveListBase.Create;
    CurveListPixelsPerInch := 96;
end;

procedure TCurveListTest.TearDown;
begin
    FList.Free;
    FList := nil;
    //  Restored, because it is process-wide and a later test reading it would
    //  otherwise inherit whatever the last one set.
    CurveListPixelsPerInch := 96;
end;

{ ---- the columns ------------------------------------------------------------ }

procedure TCurveListTest.ColumnsAreTheUnionOfParameterNames;
var
    Names: TStringList;
begin
    //  THE CASE THAT REACHED A USER. Two curve types in one model differ in the
    //  parameters they carry; sizing the table from the first curve showed the
    //  second curve's values under the first curve's headings.
    AddCurve(['A', 'B'], [Variable, Variable], [1, 2]);
    AddCurve(['B', 'C', 'D'], [Variable, Variable, Variable], [3, 4, 5]);
    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        AssertEquals('every parameter of every curve has a column', 4, Names.Count);
        AssertTrue('A is a column', Names.IndexOf('A') >= 0);
        AssertTrue('D is a column', Names.IndexOf('D') >= 0);
    finally
        Names.Free;
    end;
end;

procedure TCurveListTest.ColumnOrderIsFirstSeen;
var
    Names: TStringList;
begin
    //  Not sorted: the columns a user already knows keep their places when a
    //  second curve type joins the model and contributes its own at the end.
    AddCurve(['Zebra', 'Apple'], [Variable, Variable], [1, 2]);
    AddCurve(['Mango'], [Variable], [3]);
    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        AssertEquals('first curve, first parameter', 0, Names.IndexOf('Zebra'));
        AssertEquals('first curve, second parameter', 1, Names.IndexOf('Apple'));
        AssertEquals('the newcomer goes last', 2, Names.IndexOf('Mango'));
    finally
        Names.Free;
    end;
end;

procedure TCurveListTest.TheArgumentIsNotAColumn;
var
    Names: TStringList;
begin
    //  The argument is the x the curve is evaluated over, not a property of it.
    AddCurve(['x', 'A'], [Argument, Variable], [0, 1]);
    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        AssertEquals('only the non-argument parameter', 1, Names.Count);
        AssertEquals('and it is the right one', 'A', Names[0]);
    finally
        Names.Free;
    end;
end;

procedure TCurveListTest.ColumnNamesAreCaseInsensitive;
var
    Names: TStringList;
begin
    //  Matching Curve_parameters, which resolves names case-insensitively. Two
    //  spellings of one parameter must not become two columns.
    AddCurve(['Sigma'], [Variable], [1]);
    AddCurve(['sigma'], [Variable], [2]);
    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        AssertEquals('one column, not two', 1, Names.Count);
    finally
        Names.Free;
    end;
end;

procedure TCurveListTest.AnEmptyListHasNoColumns;
var
    Names: TStringList;
begin
    Names := TStringList.Create;
    try
        Names.Add('left over from a previous model');
        FList.CollectColumnNames(Names);
        AssertEquals('the list is cleared, not appended to', 0, Names.Count);
    finally
        Names.Free;
    end;
end;

{ ---- what a cell reads ------------------------------------------------------ }

procedure TCurveListTest.ACellReadsItsParameterValue;
begin
    AddCurve(['A'], [Variable], [12.5]);
    AssertEquals('the value, fixed to four places', '12.5000',
        FList.RowCellText(0, 0));
end;

procedure TCurveListTest.AParameterTheCurveLacksReadsBlankNotZero;
begin
    //  BLANK, not '0'. A zero reads as a value the curve holds, and this is the
    //  failure this project treats as the expensive one: a plausible number under
    //  a heading that does not apply to that row.
    AddCurve(['A', 'B'], [Variable, Variable], [1, 2]);
    AddCurve(['A'], [Variable], [3]);
    AssertEquals('the second curve has no B', '', FList.RowCellText(1, 1));
end;

procedure TCurveListTest.AnEstimatedUncertaintyIsAppended;
var
    Text: string;
begin
    AddCurve(['A'], [Variable], [10]);
    SetParamError(0, 'A', 0.25);
    Text := FList.RowCellText(0, 0);
    AssertTrue('the value is still there: ' + Text, Pos('10.0000', Text) = 1);
    AssertTrue('and the uncertainty follows it: ' + Text, Pos('0.25', Text) > 1);
end;

procedure TCurveListTest.AnAbsentUncertaintyIsNotAppended;
begin
    //  The native engine leaves Error < 0 to mean "none". Reporting 0 would claim
    //  the fit was certain.
    AddCurve(['A'], [Variable], [10]);
    AssertEquals('no uncertainty, no suffix', '10.0000', FList.RowCellText(0, 0));
end;

procedure TCurveListTest.ACellOutsideTheDataIsBlank;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertEquals('row past the end', '', FList.RowCellText(5, 0));
    AssertEquals('column past the end', '', FList.RowCellText(0, 5));
    AssertEquals('negative row', '', FList.RowCellText(-1, 0));
end;

{ ---- what a cell accepts back ----------------------------------------------- }

procedure TCurveListTest.AValidNumberIsApplied;
var
    Params: Curve_parameters;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertTrue('accepted', FList.ApplyRowCellText(0, 0, '7.5'));
    Params := Curve_parameters(FList.Items[0]);
    AssertEquals('and applied', 7.5, Params.FindByName('A').Value, 1e-9);
end;

procedure TCurveListTest.ANonNumberIsRefusedAndChangesNothing;
var
    Params: Curve_parameters;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertFalse('refused', FList.ApplyRowCellText(0, 0, 'not a number'));
    Params := Curve_parameters(FList.Items[0]);
    AssertEquals('and the parameter is untouched', 1.0,
        Params.FindByName('A').Value, 1e-9);
end;

procedure TCurveListTest.AColumnTheCurveLacksIsAcceptedAndIgnored;
begin
    //  TRUE, not False, and the distinction matters: the cell is blank by
    //  construction, so there is nothing to read back. Reporting a conversion
    //  failure here made a correct model look invalid.
    AddCurve(['A', 'B'], [Variable, Variable], [1, 2]);
    AddCurve(['A'], [Variable], [3]);
    AssertTrue('a blank cell for an absent parameter is not a failure',
        FList.ApplyRowCellText(1, 1, ''));
end;

procedure TCurveListTest.ACellOutsideTheDataIsRefused;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertFalse('row past the end', FList.ApplyRowCellText(5, 0, '1'));
    AssertFalse('column past the end', FList.ApplyRowCellText(0, 5, '1'));
end;

{ ---- which columns may be edited -------------------------------------------- }

procedure TCurveListTest.AFittedColumnIsEditable;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertTrue('a fitted parameter can be typed into', FList.ColumnIsEditable(0));
end;

procedure TCurveListTest.AColumnEveryCurveComputesIsNotEditable;
begin
    AddCurve(['A'], [Calculated], [1]);
    AddCurve(['A'], [Calculated], [2]);
    AssertFalse('nothing to type into a computed column',
        FList.ColumnIsEditable(0));
end;

procedure TCurveListTest.AColumnFittedByAnyCurveIsEditable;
begin
    //  A column's option is one setting for the whole column, but the parameter
    //  may be computed in one curve type and fitted in another. Presenting a
    //  fitted value as computed would show it as something the fit never touched,
    //  which is the worse of the two mistakes.
    AddCurve(['A'], [Calculated], [1]);
    AddCurve(['A'], [Variable], [2]);
    AssertTrue('one fitted curve is enough', FList.ColumnIsEditable(0));
end;

procedure TCurveListTest.TheArgumentColumnIsNotEditable;
begin
    //  It has no column at all, so asking about index 0 asks about the parameter
    //  that follows it.
    AddCurve(['x', 'A'], [Argument, Calculated], [0, 1]);
    AssertFalse('the only column is the calculated one',
        FList.ColumnIsEditable(0));
end;

procedure TCurveListTest.AColumnOutsideTheDataIsNotEditable;
begin
    AssertFalse('an empty list has nothing to edit', FList.ColumnIsEditable(0));
    AddCurve(['A'], [Variable], [1]);
    AssertFalse('past the last column', FList.ColumnIsEditable(9));
    AssertFalse('negative', FList.ColumnIsEditable(-1));
end;

{ ---- what kind of parameter a column holds ---------------------------------- }

procedure TCurveListTest.ColumnParameterTypeReportsTheType;
var
    Kind: TParameterType;
begin
    AddCurve(['A', 'B'], [Variable, Calculated], [1, 2]);
    AssertTrue('the fitted one is described', FList.ColumnParameterType(0, 0, Kind));
    AssertTrue('and it is fitted', Kind = Variable);
    AssertTrue('the computed one is described', FList.ColumnParameterType(0, 1, Kind));
    AssertTrue('and it is computed', Kind = Calculated);
end;

procedure TCurveListTest.ColumnParameterTypeRefusesAColumnTheCurveLacks;
var
    Kind: TParameterType;
begin
    //  False, so a caller colouring cells leaves a blank one alone rather than
    //  colouring it as something.
    AddCurve(['A', 'B'], [Variable, Variable], [1, 2]);
    AddCurve(['A'], [Variable], [3]);
    AssertFalse('the second curve has no B to describe',
        FList.ColumnParameterType(1, 1, Kind));
end;

procedure TCurveListTest.ColumnParameterTypeRefusesAnOutOfRangeCell;
var
    Kind: TParameterType;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertFalse('row past the end', FList.ColumnParameterType(5, 0, Kind));
    AssertFalse('column past the end', FList.ColumnParameterType(0, 5, Kind));
    AssertFalse('negative column', FList.ColumnParameterType(0, -1, Kind));
end;

{ ---- geometry --------------------------------------------------------------- }

procedure TCurveListTest.RowCountFollowsTheList;
begin
    //  AN EMPTY LIST STILL REPORTS ONE DATA ROW, which is not an off-by-one: the
    //  table must offer a blank row to type the first curve into, and
    //  SetRowContents has a branch that fills exactly that row with blanks. So the
    //  count is max(Count, 1) plus the heading, and it stops growing by one once
    //  there is real data.
    AssertEquals('an empty list still offers a row to type into',
        1 + FList.GetFixedRows, FList.GetRowCount);

    AddCurve(['A'], [Variable], [1]);
    AssertEquals('one curve still one row', 1 + FList.GetFixedRows,
        FList.GetRowCount);

    AddCurve(['A'], [Variable], [2]);
    AssertEquals('two curves, two rows', 2 + FList.GetFixedRows,
        FList.GetRowCount);
end;

procedure TCurveListTest.ColumnCountFollowsTheParameterNames;
begin
    AddCurve(['A', 'B'], [Variable, Variable], [1, 2]);
    AssertEquals('a column per name, plus the fixed one',
        FList.GetFixedCols + 2, FList.GetColCount);
    AddCurve(['C'], [Variable], [3]);
    AssertEquals('the third name adds a column',
        FList.GetFixedCols + 3, FList.GetColCount);
end;

{ ---- the seed sizes -------------------------------------------------------- }

procedure TCurveListTest.SeedSizesScaleWithTheDisplay;
var
    Unscaled, Scaled: longint;
begin
    //  THE ARITHMETIC THAT WAS UNREACHABLE. These seeds read Screen.PixelsPerInch
    //  directly, which is what required Forms in the data path; they are also the
    //  thing that, left unscaled, cut a scaled grid's columns back down to a
    //  quarter of the width the text needed. Now the value is injectable, so the
    //  scaling can simply be checked.
    AddCurve(['A'], [Variable], [1]);

    CurveListPixelsPerInch := 96;
    Unscaled := FList.GetColWidthByDefault(0);
    AssertEquals('64 pixels at 96 ppi', 64, Unscaled);

    CurveListPixelsPerInch := SCALED_PPI;
    Scaled := FList.GetColWidthByDefault(0);
    AssertEquals('and 1.5x of that at 144 ppi', 96, Scaled);

    CurveListPixelsPerInch := 96;
    AssertEquals('20 pixels of row at 96 ppi', 20, FList.GetRowHeightByDefault(0));
    CurveListPixelsPerInch := SCALED_PPI;
    AssertEquals('and 30 at 144 ppi', 30, FList.GetRowHeightByDefault(0));
end;

procedure TCurveListTest.SavedWidthsSurviveAndSuppressAutoSizing;
begin
    AddCurve(['A'], [Variable], [1]);
    AssertTrue('nothing saved yet, so sizes may be worked out',
        FList.AutoWidths);
    FList.SaveColWidth(0, 123);
    AssertEquals('the saved width comes back', 123, FList.GetColWidth(0));
    AssertFalse('and auto-sizing stops overriding the user',
        FList.AutoWidths);
end;

procedure TCurveListTest.SavedSelectionRoundTrips;
var
    L, T, R, B: longint;
begin
    //  Four numbers rather than the widget set's rectangle, which is what let the
    //  model stop depending on Grids.
    FList.SaveSelectionRect(2, 3, 4, 5);
    FList.GetSelectionRect(L, T, R, B);
    AssertEquals('left', 2, L);
    AssertEquals('top', 3, T);
    AssertEquals('right', 4, R);
    AssertEquals('bottom', 5, B);
end;

{ ---- the saved row heights follow their rows ------------------------------- }

procedure TCurveListTest.DeletingARowDeletesItsSavedHeight;
begin
    //  The heights are a parallel array, so a row removed without its height
    //  leaves every row below it wearing its neighbour's size. InitRowHeights is
    //  what arms the bookkeeping - before that there is no array to keep in step.
    AddCurve(['A'], [Variable], [1]);
    AddCurve(['A'], [Variable], [2]);
    AddCurve(['A'], [Variable], [3]);
    FList.InitRowHeights;
    FList.SaveRowHeight(0, 10);
    FList.SaveRowHeight(1, 20);
    FList.SaveRowHeight(2, 30);

    FList.Delete(1);

    AssertEquals('two curves left', 2, FList.Count);
    AssertEquals('the first row keeps its height', 10, FList.GetRowHeight(0));
    AssertEquals('and the third has moved up into the second',
        30, FList.GetRowHeight(1));
end;

procedure TCurveListTest.InsertingARowInsertsAHeightAtThatPosition;
begin
    AddCurve(['A'], [Variable], [1]);
    AddCurve(['A'], [Variable], [2]);
    FList.InitRowHeights;
    FList.SaveRowHeight(0, 10);
    FList.SaveRowHeight(1, 20);

    FList.Insert(0, FList.CreateNewObject);

    AssertEquals('three curves', 3, FList.Count);
    //  The newcomer takes the seed height; the two already there keep theirs.
    AssertEquals('what was first is now second', 10, FList.GetRowHeight(1));
    AssertEquals('what was second is now third', 20, FList.GetRowHeight(2));
end;

procedure TCurveListTest.EmptyingTheListLeavesOneHeightForTheBlankRow;
begin
    //  The table always shows one row, so deleting the last curve must leave a
    //  height for it rather than an empty array the next GetRowHeight indexes.
    AddCurve(['A'], [Variable], [1]);
    FList.InitRowHeights;
    FList.Delete(0);
    AssertEquals('the list is empty', 0, FList.Count);
    AssertTrue('and the blank row still has a height',
        FList.GetRowHeight(0) > 0);
end;

{ ---- row and column operations --------------------------------------------- }

{ ---- the array primitives -------------------------------------------------- }

procedure TCurveListTest.DeletingAnItemShiftsTheRestDown;
var
    Arr: TLongArray;
begin
    Arr := nil;
    AddItemLongArr(Arr, 10);
    AddItemLongArr(Arr, 20);
    AddItemLongArr(Arr, 30);
    DeleteItemLongArr(Arr, 1);
    AssertEquals('one shorter', 2, Length(Arr));
    AssertEquals('first untouched', 10, Arr[0]);
    AssertEquals('the last moved down', 30, Arr[1]);
end;

procedure TCurveListTest.InsertingAnItemShiftsTheRestUp;
var
    Arr: TLongArray;
begin
    Arr := nil;
    AddItemLongArr(Arr, 10);
    AddItemLongArr(Arr, 20);
    InsertItemLongArr(Arr, 0, 5);
    AssertEquals('one longer', 3, Length(Arr));
    AssertEquals('the newcomer is first', 5, Arr[0]);
    AssertEquals('and the rest moved up', 10, Arr[1]);
    AssertEquals('all of it', 20, Arr[2]);
end;

procedure TCurveListTest.AnOutOfRangeItemIndexIsRefused;
var
    Arr: TLongArray;
    Raised: boolean;
begin
    Arr := nil;
    AddItemLongArr(Arr, 10);
    Raised := False;
    try
        DeleteItemLongArr(Arr, 5);
    except
        on ECurveList do Raised := True;
    end;
    AssertTrue('an index past the end is an error, not a silent no-op', Raised);
end;

initialization
    //  A unit test: plain objects over plain values, no grid, no process, no file.
    RegisterTest('unit', TCurveListTest);
end.
