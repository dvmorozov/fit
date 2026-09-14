// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Puts a curve list into a grid, and reads it back out. The only unit
that knows both a TCurveListBase and a TStringGrid.)

WHY THIS IS A WRAPPER AND NOT A DESCENDANT. The grid concerns used to sit at the
ROOT of the curve-list hierarchy, so TMSCRCurveList - which appears in the
signatures of int_fit_service - inherited `uses Grids, Controls, Forms,
Graphics`. That put the LCL on the dependency path of the fitting engine, the
REST surface and the headless compute server, and it is why some fifteen thousand
lines could only be compiled by lazbuild with a widget set and so could never be
reached by the suite where coverage is measured. Server/curve_list.pas is the
model that came out of it.

Composition rather than a leaf subclass, for one concrete reason: a subclass has
to answer "what does GetCopy return for a grid-bound list?", and every answer is
wrong - either the copy silently loses the grid behaviour, or the engine's own
copies acquire a widget-set dependency through the type. A wrapper owns no data,
so nothing copies it.

WHAT IT DELIBERATELY DOES NOT DO. It does not implement IGridDataSource. That
interface was declared on the old root class and NOTHING in this application ever
handed a data source to a grid - TDataGrid.SetGridDataSource has no caller here
at all, only in fitgrids' own examples - so forty interface methods were
maintained for a consumer that did not exist. Assign pushes values into the grid
directly, which is what it always actually did.

NOTHING READS THE GRID BACK, and that is the grid's own answer rather than an
omission here. Assign sets Options := StaticOptions, which carries no goEditing;
the .lfm gives GridParameters AutoEdit = False and no goEditing either; and
TNumericGrid's double-click-to-edit path is compiled out under Lazarus. So the
parameters table is a display of the model, and a read-back loop over it would
be a loop that never runs - which is what the two methods deleted from here in
fact were, dead since before they were moved out of table_components. The
model's side of it, TCurveListBase.ApplyRowCellText, is kept: it is the inverse
of RecalcParamValue's axis transform, and an inverse that exists only in one
direction states the contract by halves.

THE DECISIONS ARE NOT HERE. What a cell reads, when it is blank, when an
uncertainty is appended, whether an edited string is acceptable - all of that is
in TCurveListBase, where a test can reach it. An LCL grid cannot be driven
headlessly (sizing one with no parent raises "Canvas does not allow drawing"
before a cell is written), so any rule left in this file is a rule no test can
check. The loops below are loops over the model's answers; keep them that way.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit curve_list_grid;

{$MODE Delphi}

interface

uses
    Classes, Grids, NumericGrid, SysUtils, checks, curve_list;

type
    { Displays a curve list in a grid. Holds no data: the list is the model and
      the caller owns it. }
    TCurveListGrid = class(TObject)
    private
        FList: TCurveListBase;

        procedure SetCaption(Grid: TStringGrid);
        procedure SetColOptions(Grid: TStringGrid);
        procedure SetColFunc(Grid: TStringGrid);
        procedure SetDataToGrid(Grid: TStringGrid);
        procedure SetRowContents(Grid: TStringGrid; RowNum: longint);
        procedure SetColWidths(Grid: TStringGrid);
        procedure GetColWidths(Grid: TStringGrid);
        procedure SetRowHeights(Grid: TStringGrid);
        procedure GetRowHeights(Grid: TStringGrid);

    public
        { Shows AList in AGrid. AList is borrowed, not owned. }
        procedure Assign(Grid: TStringGrid; AList: TCurveListBase);
        { Saves the view state - widths, heights, cursor, selection - back into
          the list, so reattaching the same list restores what the user arranged. }
        procedure Release(Grid: TStringGrid);

        { The list currently shown, or nil. }
        property List: TCurveListBase read FList;
    end;

implementation

procedure TCurveListGrid.Assign(Grid: TStringGrid; AList: TCurveListBase);
var
    L, T, R, B: longint;
begin
    CheckAssigned(Grid, 'the grid');
    CheckAssigned(AList, 'the curve list');
    FList := AList;

    //  SIZED THROUGH THE PLAIN TStringGrid REFERENCE, which is safe now and
    //  was not. The grid classes used to re-declare ColCount and RowCount with
    //  virtual setters, and those setters were what resized TNumericGrid's
    //  per-column options array and TColorStringGrid's per-cell colour matrix.
    //  A re-declared property binds by the reference's compile-time type, so
    //  assigning through this parameter skipped both, left the arrays at their
    //  old length, and made SetColOptions below raise 'Invalid option index' -
    //  which, running first in TFitViewer.PlotCurves, stopped the chart drawing
    //  its curves after every fit. Both arrays now follow the grid from its own
    //  virtual SizeChanged, so no caller can miss them; see the note on
    //  TClipboardGrid in NumericGrid.pas.
    with Grid do
    begin
        RowCount := FList.GetRowCount;
        ColCount := FList.GetColCount;

        FixedCols := FList.GetFixedCols;
        FixedRows := FList.GetFixedRows;

        LeftCol := FList.GetLeftCol;
        TopRow  := FList.GetTopRow;
        Col     := FList.GetCol;
        Row     := FList.GetRow;

        FList.GetSelectionRect(L, T, R, B);
        Selection  := Rect(L, T, R, B);
        EditorMode := False;

        Options := StaticOptions;
    end;

    SetCaption(Grid);
    SetColOptions(Grid);
    SetColFunc(Grid);

    //  Row numbering before the contents: the contents loop writes into cells the
    //  numbering has already claimed, and doing it the other way round overwrote
    //  the first column of every row with a row number.
    if Grid is TColorStringGrid then
        with Grid as TColorStringGrid do
            EnumerateRows;

    if Grid is TIDAGrid then
        with Grid as TIDAGrid do
            Changeable := False;

    if Grid is TDataGrid then
        with Grid as TDataGrid do
            ShowTable;

    SetDataToGrid(Grid);

    SetColWidths(Grid);
    SetRowHeights(Grid);
end;

procedure TCurveListGrid.Release(Grid: TStringGrid);
begin
    CheckAssigned(Grid, 'the grid');
    CheckAssigned(FList, 'the curve list');

    GetRowHeights(Grid);
    GetColWidths(Grid);

    with Grid do
    begin
        FList.SaveLeftCol(LeftCol);
        FList.SaveTopRow(TopRow);
        FList.SaveCol(Col);
        FList.SaveRow(Row);

        FList.SaveSelectionRect(Selection.Left, Selection.Top,
            Selection.Right, Selection.Bottom);
    end;

    FList.MarkSettingsSaved;
end;

procedure TCurveListGrid.SetCaption(Grid: TStringGrid);
var
    Names: TStringList;
    i: longint;
begin
    CheckThat(Grid.FixedRows >= 1,
        'the parameters grid must have its heading row before rows are filled');

    if FList.Count = 0 then
        Exit;

    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        CheckThat(Grid.ColCount - Grid.FixedCols = Names.Count,
            'the grid must have one column per parameter name the curves carry');
        for i := 0 to Names.Count - 1 do
            Grid.Cells[Grid.FixedCols + i, 0] := Names[i];
    finally
        Names.Free;
    end;
end;

{ A column's option is one setting for the whole column, but the parameter it
  stands for may be calculated in one curve type and fitted in another. Disabled
  only when EVERY curve that has this parameter computes it: treating a fitted
  value as computed would present it as something the fit did not touch.

  Which curves those are is the model's answer - ColumnIsEditable - so that the
  rule can be tested. This loop only applies it. }
procedure TCurveListGrid.SetColOptions(Grid: TStringGrid);
var
    Names: TStringList;
    i: longint;
begin
    if not (Grid is TNumericGrid) then
        Exit;
    if FList.Count = 0 then
        Exit;

    Names := TStringList.Create;
    try
        FList.CollectColumnNames(Names);
        CheckThat(Grid.ColCount - Grid.FixedCols = Names.Count,
            'the grid must have one column per parameter name the curves carry');

        for i := 0 to Names.Count - 1 do
            if FList.ColumnIsEditable(i) then
                TNumericGrid(Grid).ColOptions[Grid.FixedCols + i] := coReal
            else
                TNumericGrid(Grid).ColOptions[Grid.FixedCols + i] := coDisabled;
    finally
        Names.Free;
    end;
end;

procedure TCurveListGrid.SetColFunc(Grid: TStringGrid);
var
    i: longint;
begin
    //  Clears the function objects the fixed row carries, so a previous model's
    //  column functions do not stay attached to this one's headings.
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to ColCount - 1 do
                Objects[i, 0] := nil;
end;

procedure TCurveListGrid.SetDataToGrid(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := FixedRows to RowCount - 1 do
            SetRowContents(Grid, i);
end;

procedure TCurveListGrid.SetRowContents(Grid: TStringGrid; RowNum: longint);
var
    Names: TStringList;
    i: longint;
begin
    with Grid do
    begin
        Cells[0, RowNum] := IntToStr(RowNum);

        if (RowNum < FixedRows) or (RowNum - FixedRows >= FList.Count) then
        begin
            //  Empty row is initialized.
            for i := FixedCols to ColCount - 1 do
                Cells[i, RowNum] := '';
            Exit;
        end;

        Names := TStringList.Create;
        try
            FList.CollectColumnNames(Names);
            CheckThat(ColCount - FixedCols = Names.Count,
                'the grid must have one column per parameter name the curves carry');
            //  Assigned unconditionally, including the empty string the model
            //  returns for a parameter this curve's type does not have - the cell
            //  may still carry what the previous model left there.
            for i := 0 to Names.Count - 1 do
                Cells[FixedCols + i, RowNum] :=
                    FList.RowCellText(RowNum - FixedRows, i);
        finally
            Names.Free;
        end;
    end;
end;

procedure TCurveListGrid.SetColWidths(Grid: TStringGrid);
var
    i: longint;
begin
    if (Grid is TIDAGrid) and FList.AutoWidths then
        with Grid as TIDAGrid do
            AutoColWidths
    else
        with Grid do
            for i := 0 to ColCount - 1 do
                ColWidths[i] := FList.GetColWidth(i);
end;

procedure TCurveListGrid.GetColWidths(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := 0 to ColCount - 1 do
            FList.SaveColWidth(i, ColWidths[i]);
end;

procedure TCurveListGrid.SetRowHeights(Grid: TStringGrid);
var
    i: longint;
begin
    if (Grid is TIDAGrid) and FList.AutoHeights then
        with Grid as TIDAGrid do
            AutoRowHeights
    else
        with Grid do
            for i := 0 to RowCount - 1 do
                RowHeights[i] := FList.GetRowHeight(i);
end;

procedure TCurveListGrid.GetRowHeights(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := 0 to RowCount - 1 do
            FList.SaveRowHeight(i, RowHeights[i]);
end;

end.
