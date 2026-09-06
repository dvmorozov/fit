// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The list of curve parameter sets, and every rule the parameter table
displays - with no reference to a grid, a control or a widget set.)

WHY THIS UNIT EXISTS. It is the LCL-free half of what used to be
Server/data_classes.pas plus Desktop/table_components.pas. The old shape put the
grid concerns at the ROOT of the chain:

    TSelfCopiedCompList -> TTableCompList(IGridDataSource) -> TRowCompList
                        -> TCurveList -> TMSCRCurveList

so every descendant inherited `uses Grids, Controls, Forms, Graphics`, and
TMSCRCurveList appears in the signatures of int_fit_service - which means the
whole fitting engine, the REST surface and the compute server linked the LCL to
hold a list of numbers. Roughly fifteen thousand lines could only be compiled by
lazbuild with a widget set, and therefore could not be reached by the plain-FPC
suite where coverage is measured.

The grid concerns now sit in Desktop/curve_list_grid.pas, which OWNS a list
rather than descending from one. Composition rather than a new leaf class,
because the inheritance version had to answer "what does GetCopy return for a
grid-bound list?" and there is no good answer; the wrapper has no state anyone
copies.

WHAT THAT REVEALED, and it is worth knowing before adding to this file: the
IGridDataSource interface TTableCompList declared was VESTIGIAL. Nothing in this
application ever called TDataGrid.SetGridDataSource - the only caller anywhere is
fitgrids' own examples - so the grid never held the data source, and forty
interface methods were implemented for a caller that did not exist. GridAssign
pushes values into the grid directly instead. So the interface is not
re-implemented here; the members it required survive only where something calls
them.

TColCompList and TIconicCompList are gone. Neither has ever had a descendant in
any of the four repositories, and nothing constructed one.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit curve_list;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, checks, persistent_curve_parameters,
    self_copied_component, special_curve_parameter;

type
    { A growable array of longints, and the three operations the saved column
      widths and row heights need.

      DECLARED HERE rather than imported from Packages/utils/vectors.pas, which
      is where it used to come from. That unit is 690 lines of 3-D vector and
      matrix mathematics and this was its only consumer in the entire tree - so
      holding a list of column widths linked all of it, put 281 measured lines
      of unrelated code into the denominator of every suite that touched a
      curve list, and made the parameter table depend on geometry code it has
      nothing to do with. Four declarations are cheaper than that dependency. }
    TLongArray = array of longint;

procedure DeleteItemLongArr(var Arr: TLongArray; const Index: longint);
procedure InsertItemLongArr(var Arr: TLongArray; const Index, Item: longint);
procedure AddItemLongArr(var Arr: TLongArray; const Item: longint);

var
    { The display's pixels-per-inch, as the parameter table's data source sees it.

      96 is the unscaled default and the right answer for a headless build. The
      desktop client assigns Screen.PixelsPerInch once at start-up - see
      Desktop/Fit.lpr - because this used to read Screen directly, which is what
      required Forms here and put the LCL on the engine's dependency path.

      A VARIABLE rather than a virtual method on purpose: the two callers are
      seed sizes for a column and a row, the value is the same for every list in
      the process, and a test that wants to check the scaling arithmetic can now
      simply set it. Before, that arithmetic was unreachable. }
    CurveListPixelsPerInch: longint = 96;

type
    ECurveList = class(Exception);
    { Kept as an alias: the old name appears in module code and in a raise or two,
      and renaming an exception class is a behaviour change for anything that
      catches it by name. }
    ETableCompList = ECurveList;
    ERowCompList = ECurveList;

    { The set of curve parameter sets that make up the calculated profile, plus
      the rules the parameter table is drawn from.

      Every member here answers a question about the DATA - how many columns
      there are, what a cell should read, whether a string is a valid value for
      one, which parameters a column stands for. Nothing here knows that a grid
      exists. What draws it is Desktop/curve_list_grid.pas.

      If the number of columns returned by GetColCount changes, the wrapper's
      caption and column-option loops follow it automatically: they are loops
      over CollectColumnNames rather than over a hard-coded count. That was the
      point of collecting the names in the first place. }
    TCurveListBase = class(TSelfCopiedCompList)
    protected
        FCaption: string;

        { Arrays are deleted from the destructor. Therefore deleting
          array items should be disabled during deleting the whole object. }
        FSavedColWidths:     TLongArray;
        FSavedRowHeights:    TLongArray;
        { Indicates that array is already initialized. }
        FAreColWidthsReady:  boolean;
        FAreRowHeightsReady: boolean;

        { Saved table properties. }
        FSavedCol, FSavedRow, FSavedLeftCol, FSavedTopRow: longint;
        { The saved selection, as four numbers rather than a TGridRect - that type
          comes from Grids. The wrapper converts. }
        FSavedSelLeft, FSavedSelTop, FSavedSelRight, FSavedSelBottom: longint;

        { Indicates that grid parameters were saved. Set by the wrapper's Release. }
        FSettingsSaved: boolean;
        FHeightsSaved, FWidthsSaved: boolean;

        { Indicates that the object is destroyed. }
        FDestroying: boolean;

        { Checks that column (row) index is valid. Otherwise throws an exception. }
        procedure CheckColIndex(const Index: longint);
        procedure CheckRowIndex(const Index: longint);

        { Generic method for conversion values to user friendly representation. }
        function RecalcParamValue(P: TSpecialCurveParameter): double; virtual;
        { Generic method for reverse conversion of value from user to internal
          representation. }
        procedure ReverseCalcParamValue(P: TSpecialCurveParameter;
            NewValue: double); virtual;

    public
        constructor Create;
        destructor Destroy; override;

        function Add(Item: TComponent): integer; override;
        procedure Delete(Index: integer); override;
        procedure Insert(Index: integer; Item: TComponent); override;

        function CreateNewObject: TComponent; virtual;

        { THE TABLE'S COLUMNS, as parameter NAMES in first-seen order over EVERY
          curve - not the parameters of the first one.

          A column means a parameter, and which parameter it means has to be the
          same for every row, so the only workable identity is the NAME. This
          used to be positional and sized from Items[0], which quietly assumed
          every curve in the model carries the same parameters in the same order.
          A model of one curve type does, and that is why it held for years.

          It stops being true the moment two types coexist, which a module may
          produce and this framework must therefore support: two types from one
          module differ in COUNT (a corrective pattern has 15 parameters where a
          motive one has 19) and, even at equal counts, in NAME - a motive
          pattern's k5 is a diagonal's c5. The count mismatch fired an internal
          check and took the client down. The name mismatch was worse: the counts
          agreed, so nothing complained, and the table showed one curve's c5 under
          the other curve's k5 heading. A wrong number under a plausible label is
          exactly the failure this project treats as the expensive one.

          The argument is excluded, as it always was - it is the x the curve is
          evaluated over, not a property of the curve.

          Names are compared case-insensitively, matching Curve_parameters. }
        procedure CollectColumnNames(ANames: TStringList);

        { What KIND of parameter a data column holds - fitted, fixed, computed -
          for the curve in ARow.

          ARow and ACol are both zero-based within the DATA - the caller
          subtracts the table's fixed rows and columns. False when there is no
          such curve or column, AND ALSO when this curve simply has no parameter
          of that column's name - the cell is blank for it, and a caller that
          colours cells must leave it alone rather than colour it as something. }
        function ColumnParameterType(const ARow, ACol: longint;
            out AType: TParameterType): boolean;

        { WHAT ONE DATA CELL READS, extracted from the loop that used to write it
          straight into a grid.

          ARow is zero-based within the data; ACol indexes the names
          CollectColumnNames returns. Two rules live here and nowhere else:

            * BLANK, not '0', when this curve's type has no parameter of that
              column's name. A 0 would read as a value the curve holds. The old
              code cleared the cell rather than skipping it, because the cell may
              still carry what the previous model left there - and returning ''
              here keeps that, since the caller assigns unconditionally.
            * the uncertainty is appended as ' +- <error>' only when a backend
              estimated one. The native engine leaves Error < 0 to mean "none",
              so a >= 0 test is the whole condition.

          These were unreachable by any test while they lived inside a method
          that took a TStringGrid: an LCL grid cannot be sized headlessly - it
          raises "Canvas does not allow drawing" before a cell is written - so
          the rules could only be checked by reading them. }
        function RowCellText(const ARow, ACol: longint): string;

        { THE REVERSE, for reading an edited cell back into the model. Returns
          False when AText is not a number, leaving the parameter untouched;
          returns True and does nothing at all when this curve has no parameter of
          that column's name.

          That second case is why the answer is not simply "did it parse": a
          column this curve's type does not have is BLANK by construction, so
          there is nothing to read back, and reporting a conversion failure for a
          cell the user was never offered is how a correct model came to look
          invalid. }
        function ApplyRowCellText(const ARow, ACol: longint;
            const AText: string): boolean;

        { WHETHER A COLUMN MAY BE EDITED AT ALL, extracted from the loop that used
          to set the grid's column options.

          A column's option is one setting for the whole column, but the parameter
          it stands for may be calculated in one curve type and fitted in another.
          Editable when ANY curve that has this parameter fits it: treating a
          fitted value as computed would present it as something the fit never
          touched, which is the worse of the two mistakes.

          ACol indexes the names CollectColumnNames returns. False for a column
          out of range, and False for an empty list - there is nothing to edit.

          The bug the extracted version makes unrepeatable rather than merely
          fixed: the two branches indexed ColOptions differently, one by the
          parameter's own index and one by the column's, so as soon as a curve
          carried its argument anywhere but first, the calculated and the real
          columns were disabled in each other's places. }
        function ColumnIsEditable(const ACol: longint): boolean;

        { The saved column widths and row heights. }
        procedure DeleteAllColWidthItems;
        procedure AddColWidthItem;

        procedure DeleteAllRowHeightItems;
        procedure DeleteRowHeightItem(const Index: longint);
        procedure InsertRowHeightItem(const Index: longint);
        procedure AddRowHeightItem;

        function GetColWidthByDefault(const Index: longint): longint; virtual;
        function GetRowHeightByDefault(const Index: longint): longint; virtual;

        procedure InitColWidths;
        procedure InitRowHeights;

        { Total number of columns and rows, including the fixed ones. }
        function GetColCount: longint; virtual;
        function GetRowCount: longint; virtual;
        function GetInfoCols: longint; virtual;
        function GetInfoRows: longint; virtual;
        function GetFixedCols: longint; virtual;
        function GetFixedRows: longint; virtual;

        function GetColWidth(const Col: longint): longint;
        procedure SaveColWidth(const Col, Width: longint);
        function GetRowHeight(const Row: longint): longint;
        procedure SaveRowHeight(const Row, Height: longint);
        function AutoWidths: boolean;
        function AutoHeights: boolean;

        { The saved selection, as four numbers. The wrapper converts to and from
          the widget set's own rectangle type. }
        procedure GetSelectionRect(out ALeft, ATop, ARight, ABottom: longint);
        procedure SaveSelectionRect(const ALeft, ATop, ARight, ABottom: longint);

        function GetCol: longint;
        procedure SaveCol(const Col: longint);
        function GetRow: longint;
        procedure SaveRow(const Row: longint);
        function GetLeftCol: longint;
        procedure SaveLeftCol(const LeftCol: longint);
        function GetTopRow: longint;
        procedure SaveTopRow(const TopRow: longint);

        { Set by the wrapper when it saves the view state back. }
        procedure MarkSettingsSaved;

        property SavedCol: longint read GetCol write SaveCol;
        property SavedRow: longint read GetRow write SaveRow;
        property SavedLeftCol: longint read GetLeftCol write SaveLeftCol;
        property SavedTopRow: longint read GetTopRow write SaveTopRow;

        property Caption: string read FCaption write FCaption;
    end;

    { The name the rest of the tree used. Kept so the module pack and the tests
      need no edit for a rename that carries no meaning. }
    TCurveList = TCurveListBase;

implementation

{ The three array operations, moved verbatim from Packages/utils/vectors.pas
  along with the index check they share. See TLongArray for why they were moved
  rather than imported. }
procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: longint);
begin
    if (Index < MinIndex) or (Index > MaxIndex) then
        raise ECurveList.Create('Invalid item index (' + IntToStr(Index) + ')...');
end;

procedure DeleteItemLongArr(var Arr: TLongArray; const Index: longint);
var
    i: longint;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    for i := Index + 1 to Length(Arr) - 1 do
        Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

procedure InsertItemLongArr(var Arr: TLongArray; const Index, Item: longint);
var
    i: longint;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do
        Arr[i + 1] := Arr[i];
    Arr[Index]     := Item;
end;

procedure AddItemLongArr(var Arr: TLongArray; const Item: longint);
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Item;
end;

constructor TCurveListBase.Create;
begin
    inherited;

    FSavedCol     := GetFixedCols;
    FSavedRow     := GetFixedRows;
    FSavedLeftCol := FSavedCol;
    FSavedTopRow  := FSavedRow;
    SaveSelectionRect(FSavedCol, FSavedRow, FSavedCol, FSavedRow);
end;

destructor TCurveListBase.Destroy;
begin
    FDestroying := True;
    Finalize(FSavedColWidths);
    Finalize(FSavedRowHeights);
    inherited;
end;

function TCurveListBase.Add(Item: TComponent): integer;
var
    Flag: boolean;
begin
    Flag   := Count = 0;
    Result := inherited Add(Item);
    //  список уже не пуст !!!
    if FAreRowHeightsReady and not Flag then
        AddRowHeightItem;
    //  должна вызываться последней, чтобы проверка
    //  индексов дала правильные результаты; для
    //  пустого объекта Add вызывается дважды,
    //  поэтому первый раз нужно пропустить
end;

procedure TCurveListBase.Delete(Index: integer);
begin
    if (not FDestroying) and FAreRowHeightsReady then
        DeleteRowHeightItem(Index);
    //  должна вызываться первой, чтобы проверка
    //  индексов дала правильные результаты
    inherited;  //  число элементов в списке изменилось
    if (not FDestroying) and (Count = 0) and FAreRowHeightsReady then
        AddRowHeightItem;
    //  если удалены все данные нужно добавить один
    //  элемент на пустую строку
end;

procedure TCurveListBase.Insert(Index: integer; Item: TComponent);
var
    Flag: boolean;
begin
    Flag := Count = 0;
    inherited;
    if FAreRowHeightsReady and not Flag then
        InsertRowHeightItem(Index);
    //  должна вызываться последней, чтобы проверка
    //  индексов дала правильные результаты
end;

function TCurveListBase.GetColCount: longint;
begin
    Result := GetInfoCols + GetFixedCols;
end;

function TCurveListBase.GetRowCount: longint;
begin
    Result := GetInfoRows + GetFixedRows;
end;

function TCurveListBase.GetFixedCols: longint;
begin
    Result := 1;
end;

function TCurveListBase.GetFixedRows: longint;
begin
    Result := 1;
end;

function TCurveListBase.GetInfoRows: longint;
begin
    if Count <> 0 then
        Result := Count
    else
        Result := 1;
end;

procedure TCurveListBase.CheckColIndex(const Index: integer);
begin
    if (Index < 0) or (Index >= GetColCount) then
        raise ETableCompList.Create('Invalid column index...');
end;

procedure TCurveListBase.CheckRowIndex(const Index: integer);
begin
    if (Index < 0) or (Index >= GetRowCount) then
        raise ETableCompList.Create('Invalid row index...');
end;

{ The seed size of a column and a row, in the pixels of the display.

  64 and 20 are what these used to return outright, and they are what a cell
  needs at 96 dpi. The grid gets them AFTER the LCL has scaled it - TDataGrid
  fills its widths from here on every data change - so on a scaled display they
  did not merely fail to grow, they cut the scaled defaults back down: a column
  too narrow for the number in it and a row too short for its own text.

  Screen, not a control: this is a data source with no control to ask, and the
  value is a starting width the user is free to drag. What must not happen is
  that it arrives at a quarter of the size the text needs. Screen is the right
  place to ask because ui_dpi has already corrected it - see that unit for why
  the widget set's own answer cannot be trusted on a scaled Linux desktop. }
function TCurveListBase.GetColWidthByDefault(const Index: integer): longint;
begin
    CheckColIndex(Index);
    Result := 64 * CurveListPixelsPerInch div 96;
end;

function TCurveListBase.GetRowHeightByDefault(const Index: integer): longint;
begin
    CheckRowIndex(Index);
    Result := 20 * CurveListPixelsPerInch div 96;
end;

procedure TCurveListBase.AddColWidthItem;
begin
    CheckColIndex(Length(FSavedColWidths)(* - 1 + 1*));
    AddItemLongArr(FSavedColWidths,
        GetColWidthByDefault(Length(FSavedColWidths)(* - 1 + 1*)));
    //  последний элемент имеет индекс Length - 1,
    //  а проверять нужно индекс на 1 больше
end;

procedure TCurveListBase.AddRowHeightItem;
begin
    CheckRowIndex(Length(FSavedRowHeights)(* - 1 + 1*));
    AddItemLongArr(FSavedRowHeights,
        GetRowHeightByDefault(Length(FSavedRowHeights)(* - 1 + 1*)));
    //  последний элемент имеет индекс Length - 1,
    //  а проверять нужно индекс на 1 больше
end;

procedure TCurveListBase.DeleteRowHeightItem(const Index: integer);
begin
    CheckRowIndex(Index);
    DeleteItemLongArr(FSavedRowHeights, Index);
end;

procedure TCurveListBase.InsertRowHeightItem(const Index: integer);
begin
    CheckRowIndex(Index);
    InsertItemLongArr(FSavedRowHeights, Index, GetRowHeightByDefault(Index));
end;

procedure TCurveListBase.DeleteAllColWidthItems;
begin
    Finalize(FSavedColWidths);
end;

procedure TCurveListBase.DeleteAllRowHeightItems;
begin
    Finalize(FSavedRowHeights);
end;

procedure TCurveListBase.InitColWidths;
var
    i: longint;
begin
    if not FAreColWidthsReady then
    begin
        DeleteAllColWidthItems;
        for i := 1 to GetColCount do
            AddColWidthItem;
        FAreColWidthsReady := True;
    end;
end;

procedure TCurveListBase.InitRowHeights;
var
    i: longint;
begin
    if not FAreRowHeightsReady then
    begin
        DeleteAllRowHeightItems;
        for i := 1 to GetRowCount do
            AddRowHeightItem;
        FAreRowHeightsReady := True;
    end;
end;

function TCurveListBase.GetColWidth(const Col: integer): longint;
begin
    CheckColIndex(Col);
    InitColWidths;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число колонок неизвестно
    //  на этапе создания объекта
    Result := FSavedColWidths[Col];
end;

procedure TCurveListBase.SaveColWidth(const Col, Width: integer);
begin
    CheckColIndex(Col);
    InitColWidths;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число колонок неизвестно
    //  на этапе создания объекта
    FSavedColWidths[Col] := Width;
    FWidthsSaved := True;
end;

function TCurveListBase.GetRowHeight(const Row: integer): longint;
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число строк неизвестно
    //  на этапе создания объекта
    Result := FSavedRowHeights[Row];
end;

procedure TCurveListBase.SaveRowHeight(const Row, Height: integer);
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  первоначальная инициализация массива
    //  сделана здесь потому, что не всегда
    //  удобно делать инициализацию в конструкторе,
    //  например, когда число строк неизвестно
    //  на этапе создания объекта
    FSavedRowHeights[Row] := Height;
    FHeightsSaved := True;
end;

function TCurveListBase.AutoWidths: boolean;
begin
    Result := not FWidthsSaved;
end;

function TCurveListBase.AutoHeights: boolean;
begin
    Result := not FHeightsSaved;
end;

function TCurveListBase.GetCol: longint;
begin
    Result := FSavedCol;
end;

procedure TCurveListBase.SaveCol(const Col: integer);
begin
    FSavedCol := Col;
end;

function TCurveListBase.GetRow: longint;
begin
    Result := FSavedRow;
end;

procedure TCurveListBase.SaveRow(const Row: integer);
begin
    FSavedRow := Row;
end;

function TCurveListBase.GetLeftCol: longint;
begin
    Result := FSavedLeftCol;
end;

procedure TCurveListBase.SaveLeftCol(const LeftCol: integer);
begin
    FSavedLeftCol := LeftCol;
end;

function TCurveListBase.GetTopRow: longint;
begin
    Result := FSavedTopRow;
end;

procedure TCurveListBase.SaveTopRow(const TopRow: integer);
begin
    FSavedTopRow := TopRow;
end;

procedure TCurveListBase.CollectColumnNames(ANames: TStringList);
var
    i, j: longint;
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
begin
    CheckAssigned(ANames, 'the column name list');

    ANames.Clear;
    ANames.CaseSensitive := False;
    //  Sorted stays FALSE: the order is first-seen, so the columns a user
    //  already knows keep their places when a second curve type joins the model
    //  and contributes its own parameters at the end.
    for i := 0 to Count - 1 do
    begin
        CurveParameters := Curve_parameters(Items[i]);
        for j := 0 to CurveParameters.Params.Count - 1 do
        begin
            Parameter := CurveParameters[j];
            if Parameter.Type_ = Argument then
                Continue;
            if ANames.IndexOf(Parameter.Name) = -1 then
                ANames.Add(Parameter.Name);
        end;
    end;
end;

function TCurveListBase.ColumnParameterType(const ARow, ACol: longint;
    out AType: TParameterType): boolean;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    Names: TStringList;
begin
    AType := Calculated;
    Result := False;
    if (ARow < 0) or (ARow >= Count) or (ACol < 0) then
        Exit;

    Names := TStringList.Create;
    try
        CollectColumnNames(Names);
        if ACol >= Names.Count then
            Exit;
        CurveParameters := Curve_parameters(Items[ARow]);
        Parameter := CurveParameters.FindByName(Names[ACol]);
        //  Absent is an ordinary answer, not an error: this curve is of a type
        //  that has no such parameter, its cell is blank, and False tells the
        //  caller there is nothing here to describe.
        if not Assigned(Parameter) then
            Exit;
        AType := Parameter.Type_;
        Result := True;
    finally
        Names.Free;
    end;
end;

{ One column per parameter NAME across every curve - see CollectColumnNames for
  why the first curve cannot speak for the rest. }
function TCurveListBase.GetInfoCols: longint;
var
    Names: TStringList;
begin
    if Count = 0 then
    begin
        Result := GetFixedCols + 1;
        Exit;
    end;

    Names := TStringList.Create;
    try
        CollectColumnNames(Names);
        Result := Names.Count;
    finally
        Names.Free;
    end;
end;

function TCurveListBase.CreateNewObject: TComponent;
begin
    Result := Curve_parameters.Create(nil);
end;

function TCurveListBase.RecalcParamValue(P: TSpecialCurveParameter): double;
begin
    CheckAssigned(P, 'the parameter whose value the column shows');
    Result := P.Value;
end;

procedure TCurveListBase.ReverseCalcParamValue(P: TSpecialCurveParameter;
    NewValue: double);
begin
    CheckAssigned(P, 'the parameter whose value the column shows');
    P.Value := NewValue;
end;

{ WHAT ONE DATA CELL READS. See the declaration for the two rules this is the
  only home of. }
function TCurveListBase.RowCellText(const ARow, ACol: longint): string;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    Names: TStringList;
begin
    Result := '';
    if (ARow < 0) or (ARow >= Count) or (ACol < 0) then
        Exit;

    Names := TStringList.Create;
    try
        CollectColumnNames(Names);
        if ACol >= Names.Count then
            Exit;
        CurveParameters := Curve_parameters(Items[ARow]);
        Parameter := CurveParameters.FindByName(Names[ACol]);
        //  BLANK, not zero: this curve's type has no such parameter, and a 0
        //  would read as a value it holds.
        if not Assigned(Parameter) then
            Exit;

        Result := FloatToStrF(RecalcParamValue(Parameter), ffFixed, 8, 4);
        //  Show the uncertainty only when a backend estimated one - the native
        //  engine leaves it < 0 to mean "none".
        if Parameter.Error >= 0 then
            Result := Result + ' ' + #$C2#$B1 + ' ' +
                FloatToStrF(Parameter.Error, ffGeneral, 4, 4);
    finally
        Names.Free;
    end;
end;

{ THE REVERSE. See the declaration for why "this curve has no such parameter" is
  True rather than False. }
function TCurveListBase.ApplyRowCellText(const ARow, ACol: longint;
    const AText: string): boolean;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    Names: TStringList;
    Value: double;
begin
    Result := False;
    if (ARow < 0) or (ARow >= Count) or (ACol < 0) then
        Exit;

    Names := TStringList.Create;
    try
        CollectColumnNames(Names);
        if ACol >= Names.Count then
            Exit;
        CurveParameters := Curve_parameters(Items[ARow]);
        Parameter := CurveParameters.FindByName(Names[ACol]);
        //  A column this curve's type does not have. Its cell is blank by
        //  construction, so there is nothing to read back - and trying would
        //  report a conversion failure for a cell the user was never offered.
        if not Assigned(Parameter) then
        begin
            Result := True;
            Exit;
        end;

        //  TryStrToFloat rather than StrToFloat in a try..except: the old code
        //  swallowed the exception to return False, and an exception used as a
        //  branch is both slower and harder to read than the test it stands for.
        if not TryStrToFloat(AText, Value) then
            Exit;
        ReverseCalcParamValue(Parameter, Value);
        Result := True;
    finally
        Names.Free;
    end;
end;

{ WHETHER A COLUMN MAY BE EDITED. See the declaration for the rule and for the
  indexing bug the extraction makes unrepeatable. }
function TCurveListBase.ColumnIsEditable(const ACol: longint): boolean;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    Names: TStringList;
    j: longint;
begin
    Result := False;
    if (ACol < 0) or (Count = 0) then
        Exit;

    Names := TStringList.Create;
    try
        CollectColumnNames(Names);
        if ACol >= Names.Count then
            Exit;

        for j := 0 to Count - 1 do
        begin
            CurveParameters := Curve_parameters(Items[j]);
            Parameter := CurveParameters.FindByName(Names[ACol]);
            //  This curve's type has no such parameter, so it has no opinion on
            //  whether the column is editable.
            if not Assigned(Parameter) then
                Continue;
            if not (Parameter.Type_ in [Argument, Calculated]) then
            begin
                Result := True;
                Exit;
            end;
        end;
    finally
        Names.Free;
    end;
end;

procedure TCurveListBase.GetSelectionRect(out ALeft, ATop, ARight, ABottom: longint);
begin
    ALeft   := FSavedSelLeft;
    ATop    := FSavedSelTop;
    ARight  := FSavedSelRight;
    ABottom := FSavedSelBottom;
end;

procedure TCurveListBase.SaveSelectionRect(
    const ALeft, ATop, ARight, ABottom: longint);
begin
    FSavedSelLeft   := ALeft;
    FSavedSelTop    := ATop;
    FSavedSelRight  := ARight;
    FSavedSelBottom := ABottom;
end;

procedure TCurveListBase.MarkSettingsSaved;
begin
    FSettingsSaved := True;
end;

initialization
    RegisterClass(Curve_parameters);
{$warnings off}
    DecimalSeparator := '.';
{$warnings on}
end.
