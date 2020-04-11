{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of visual component which can display data in grid.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit table_components;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, Controls, Graphics, Grids, NumericGrid, self_copied_component,
    SysUtils, vectors;

type
    ETableCompList = class(Exception);

    { Component list which can display component properties in grid.
      Class implements functions of saving/reading table properties,
      but does not bind grid positions with list items. If number of
      columns returned by GetColCount is changed then corresponding
      changes in SetCaption, SetColOptions, SetColFunc, SetRowContents,
      GetRowContents should be done. }
    TTableCompList = class(TSelfCopiedCompList, IGridDataSource)
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
        FSavedSelection: TGridRect;

        { Indicates that grid parameters were saved. It is set up in GridRelease. }
        FSettingsSaved: boolean;
        FHeightsSaved, FWidthsSaved: boolean;

        { Indicates that the object is destroyed. }
        FDestroying: boolean;

        { Checks that column (row) index is valid. Otherwise throws an exception. }
        procedure CheckColIndex(const Index: longint);
        procedure CheckRowIndex(const Index: longint);

    public
        constructor Create;
        destructor Destroy; override;

        ////////////////////////////////////////////////////////////////////////
        //  ������ ������� ���������� ��������� ��� ������������� � ��������
        //  ������������, � ����� ��� ����, ����� ����� ���� �������� �
        //  ����������� ������� ��� ����� ���������� ������� �� ���������

        //  ������ ������� ���������� �������������, ��� �������� ������
        //  �������� ��������, � ����� ��������� (��� ���������� � ���������
        //  �������)
        ////////////////////////////////////////////////////////////////////////

        procedure GridAssign(Grid: TStringGrid); virtual;
        //  ��������� ���������� �������; ����� ������ ���������������
        //  � ����� �� ���������� ��������� ������ ���� � ������� ��������
        procedure GridRelease(Grid: TStringGrid); virtual;
        //  ���������� ��������� ���������� �������
        procedure SetDataToGrid(Grid: TStringGrid); virtual; abstract;
        function GetDataFromGrid(Grid: TStringGrid): boolean; virtual; abstract;

        procedure SetCaption(Grid: TStringGrid); virtual; abstract;
        procedure SetColOptions(Grid: TStringGrid); virtual; abstract;
        //  ������������� ����� ������� � Grid'�
        procedure SetColFunc(Grid: TStringGrid); virtual;
        //  ������������� ������� ��� �������

        procedure SetColWidths(Grid: TStringGrid);
        //  ������������� ������ ������� �������
        procedure GetColWidths(Grid: TStringGrid);
        //  ��������� �� ���������� �����
        //  �������� ������ ������� �������
        procedure SetRowHeights(Grid: TStringGrid);
        //  ������������� ������ ����� �������
        procedure GetRowHeights(Grid: TStringGrid);
        //  ��������� �� ���������� �����
        //  �������� ������ ����� �������

        procedure InitColWidths;    //  �������������� ������������� �������
        procedure InitRowHeights;

        ////////////////////////////////////////////////////////////////////////
        //  ������ ������ ����������

        //  ������ ������� ���������� �������������, ��� ��������
        //  �������� �����, � �������� ������ ��������� (��� �����
        //  ���������� � ��������� �������)
        ////////////////////////////////////////////////////////////////////////

        function IsDataSourceEmpty: boolean; virtual; abstract;

        function ValueToString(const ACol, ARow: longint): string;
            virtual; abstract;
        procedure BeforeStringToValue(const ACol, ARow: longint;
            const AString: string); virtual; abstract;
        procedure StringToValue(const ACol, ARow: longint;
            const AString: string); virtual; abstract;
        procedure SetValueByDefault(const ACol, ARow: longint); virtual; abstract;
        //  ������������� ���������� �������� "�� ���������"
        //  ��� ������ ������; ������������ ��� ��������� ��������
        //  ������� ������
        function GetCellColor(
        //  ���������� True, ���� ������ ���� ����������
        //  ���� Color, � ��������� ������ - False
        //  (������� ��������� ���� "�� ���������")
            const ACol, ARow: longint; var Color: TColor): boolean; virtual;
        { Returns default empty mask. }
        function GetCellEditMask(const ACol, ARow: longint): string; virtual;
        function GetCellEnabledCharSet(const ACol, ARow: longint): TCharSet;
            virtual; abstract;
        function IsCellDisabled(
        //  ���������� ������� ���������� �����
        //  � ������ - True - ���� ��������
            const ACol, ARow: longint): boolean; virtual;

        function IsDataValid(const ACol, ARow: longint;
        //  ��������� "������" �������� ������ ��� �����������
        //  ����������; ������, ���� ���������� ������/�������
        //  ����� ������������ �������� ���������� ����������;
        //  ������ ���������� True - ��������� ��� ����������
        //  ������ ��������
            const AString: string): boolean; virtual; abstract;

        //  ��� ������� �������� ����������� ���������� ��������
        //  ���������� False; ������ - ������� ������ ��������� ��
        function MayIDoInsertRows(StartRow, RowsCount: longint): boolean; virtual;
        function MayIDoDeleteRows(StartRow, RowsCount: longint): boolean; virtual;
        function MayIDoAddRow: boolean; virtual;

        function MayIDoInsertColumns(StartCol, ColsCount: longint): boolean; virtual;
        function MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean; virtual;
        function MayIDoAddColumn: boolean; virtual;

        function MayIDoDeleteAllData: boolean; virtual;
        function MayIDoClearSelectedArea: boolean; virtual;
        function MayIDoClearAllCells: boolean; virtual;

        //  ������� ���������� ��������� ����������� ��������
        //  ������ (������) ������� (��������)
        procedure DeleteAllColWidthItems;
        procedure DeleteColWidthItem(const Index: longint);
        procedure InsertColWidthItem(const Index: longint);
        procedure AddColWidthItem;

        procedure DeleteAllRowHeightItems;
        procedure DeleteRowHeightItem(const Index: longint);
        procedure InsertRowHeightItem(const Index: longint);
        procedure AddRowHeightItem;

        function GetColWidthByDefault(const Index: longint): longint;
            virtual;
        function GetRowHeightByDefault(const Index: longint): longint;
            virtual;

        //  ��� ������ ��������, �������, ������� ��������
        //  ���������� � ���������� � ������������� ���������
        //  ������ ��������; ������ - ������� ������ ���������
        //  ��� ������ ��� ���������� ��������� ��������
        procedure RowsDeleted(const StartRow, RowsCount: longint); virtual;
        procedure RowsInserted(const StartRow, RowsCount: longint); virtual;
        procedure RowAdded; virtual;

        procedure ColumnsDeleted(const StartCol, ColsCount: longint);
            virtual;
        procedure ColumnsInserted(const StartCol, ColsCount: longint);
            virtual;
        procedure ColumnAdded; virtual;

        procedure AllDataDeleted; virtual;

        function GetColCount: longint; virtual;
        //  ������ ����� �������, ������� Fixed
        function GetRowCount: longint; virtual;
        //  ������ ����� �����, ������� Fixed
        function GetInfoCols: longint; virtual; abstract;
        //  ����� ������� �������������� ����� �������
        function GetInfoRows: longint; virtual; abstract;
        //  ����� ����� �������������� ����� �������
        function GetFixedCols: longint; virtual;
        //  ����� ������������� ������� (�� ��������� = 1)
        function GetFixedRows: longint; virtual;
        //  ����� ������������� ����� (�� ��������� = 1)
        function GetColNumFixed: boolean; virtual;  //  (�� ��������� = False)
        function GetRowNumFixed: boolean; virtual;  //  (�� ��������� = False)

        function GetColWidth(const Col: longint): longint;
        procedure SaveColWidth(const Col, Width: longint);
        function GetRowHeight(const Row: longint): longint;
        procedure SaveRowHeight(const Row, Height: longint);
        function AutoWidths: boolean;
        function AutoHeights: boolean;

        function GetSelection: TGridRect;
        procedure SaveSelection(const Selection: TGridRect);
        function GetCol: longint;       //  ����� ������� ��������� �������
        procedure SaveCol(const Col: longint);
        function GetRow: longint;       //  ����� ������� ��������� ������
        procedure SaveRow(const Row: longint);
        function GetLeftCol: longint;
        procedure SaveLeftCol(const LeftCol: longint);
        function GetTopRow: longint;
        procedure SaveTopRow(const TopRow: longint);

        property SavedCol: longint read GetCol write SaveCol;
        property SavedRow: longint read GetRow write SaveRow;
        property SavedLeftCol: longint read GetLeftCol write SaveLeftCol;
        property SavedTopRow: longint read GetTopRow write SaveTopRow;
        property SavedSelection: TGridRect read GetSelection write SaveSelection;

        property Caption: string read FCaption write FCaption;
    end;

    ERowCompList = class(Exception);
    EColCompList = class(Exception);

    TRowCompList = class(TTableCompList)
        //  ������ �����������, ������ �� �������
        //  ������������ ����� ������ �������; �����
        //  �������� �������������� �������������, �������
        //  ��� ���������� ����������� ����� ��������
        //  ����� ������� ��������������� ���������
    protected
        function CreateNewObject: TComponent; virtual; abstract;
        //  ������� ����� ���������, ������� �����
        //  ������������ ������ �������

    public
        //  ��� ���������� ������ ������� ����������, ����� �����
        //  ����� � �������������� ����� ������� ���� ����� �����
        //  ��������� � ������
        function GetDataFromGrid(Grid: TStringGrid): boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): integer; override;
        procedure Delete(Index: integer); override;
        procedure Insert(Index: integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: integer;
        //  ��������� ������������ �������� � � ������ ������
        //  �������� ����������; ���� ������ ������ ���������
        //  ������ - ������; !!! ����� ����������� ��������
        //  � ������� - ����������� !!!
            const AString: string); override;

        function MayIDoInsertRows(StartRow, RowsCount: longint): boolean; override;
        function MayIDoDeleteRows(StartRow, RowsCount: longint): boolean; override;
        function MayIDoAddRow: boolean; override;

        function MayIDoDeleteAllData: boolean; override;
        function MayIDoClearAllCells: boolean; override;
        function MayIDoClearSelectedArea: boolean; override;

        procedure SetRowContents(
        //  ��������� ������ ������� � ������� RowNum
            Grid: TStringGrid; RowNum: longint); virtual; abstract;
        function GetRowContents(
        //  ��������� ���������� ������ � ������� RowNum
            Grid: TStringGrid; RowNum: longint): boolean; virtual; abstract;

        procedure RowsDeleted(const StartRow, RowsCount: longint); override;
        procedure RowsInserted(const StartRow, RowsCount: longint); override;
        procedure RowAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: boolean; override;

        function GetInfoRows: longint; override;
        //  ���������� ��������� ����� ����� = Fixed + 1
        //  ��� ����������� ����������� �����
        function GetColNumFixed: boolean; override;
        //  ����� ������� �����������
    end;

    TColCompList = class(TTableCompList)
        //  ������ �����������, ������ �� �������
        //  ������������ ����� ������� �������; �����
        //  ����� �������������� �������������, �������
        //  ��� ���������� ����������� ����� �����
        //  ����� ������� ��������������� ���������
    protected
        function CreateNewObject: TComponent; virtual; abstract;
        //  ������� ����� ���������, ������� �����
        //  ������������ ������� �������

    public
        //  ��� ���������� ������ ������� ����������, ����� �����
        //  ������� � �������������� ����� ������� ���� ����� �����
        //  ��������� � ������
        function GetDataFromGrid(Grid: TStringGrid): boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): integer; override;
        procedure Delete(Index: integer); override;
        procedure Insert(Index: integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: longint;
        //  ��������� ������������ �������� � � ������ ������
        //  �������� ����������; ���� ������ ������ ���������
        //  ������ - �������; !!! ����� ����������� ��������
        //  � ������� - ����������� !!!
            const AString: string); override;

        function MayIDoInsertColumns(StartCol, ColsCount: longint): boolean; override;
        function MayIDoDeleteColumns(StartCol, ColsCount: longint): boolean; override;
        function MayIDoAddColumn: boolean; override;

        function MayIDoDeleteAllData: boolean; override;
        function MayIDoClearSelectedArea: boolean; override;
        function MayIDoClearAllCells: boolean; override;

        procedure SetColContents(
        //  ��������� ������� ������� � ������� ColNum
            Grid: TStringGrid; ColNum: longint); virtual; abstract;
        function GetColContents(
        //  ��������� ���������� ������� � ������� ColNum
            Grid: TStringGrid; ColNum: longint): boolean; virtual; abstract;

        procedure ColumnsDeleted(const StartCol, ColsCount: longint);
            override;
        procedure ColumnsInserted(const StartCol, ColsCount: longint);
            override;
        procedure ColumnAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: boolean; override;

        function GetInfoCols: longint; override;
        //  ���������� ��������� ����� ������� = Fixed + 1
        //  ��� ����������� ����������� �����
        function GetRowNumFixed: boolean; override;
        //  ����� ����� ������ �����������
    end;

    TIconicCompList = class(TTableCompList)
    protected
        FImageList: TImageList;
    public
    end;

implementation

constructor TTableCompList.Create;
begin
    inherited;

    FSavedCol     := GetFixedCols;
    FSavedRow     := GetFixedRows;
    FSavedLeftCol := FSavedCol;
    FSavedTopRow  := FSavedRow;
    with FSavedSelection do
    begin
        Left   := FSavedCol;
        Top    := FSavedRow;
        Right  := FSavedCol;
        Bottom := FSavedRow;
    end;
end;

destructor TTableCompList.Destroy;
begin
    FDestroying := True;
    Finalize(FSavedColWidths);
    Finalize(FSavedRowHeights);
    inherited;
end;

procedure TTableCompList.GridAssign(Grid: TStringGrid);
(*var i, j: LongInt;*)
begin
    with Grid do
    begin
        if Grid is TColorStringGrid then
            with Grid as TColorStringGrid do
            begin
                //  ��� �����, ����� ������������� �������
                //  ���������������� ��������
                RowCount := GetRowCount;
                ColCount := GetColCount;
            end
        else
        begin
            RowCount := GetRowCount;
            ColCount := GetColCount;
        end;

        //  ���� ����� �������, �� ������ ������� �����������
        //  for j := 0 to RowCount - 1 do
        //      for i := 0 to ColCount - 1 do Cells[i, j] := '';

        FixedCols := GetFixedCols;
        FixedRows := GetFixedRows;

        LeftCol := GetLeftCol;
        TopRow  := GetTopRow;
        Col     := GetCol;
        Row     := GetRow;

        Selection  := GetSelection;
        EditorMode := False;

        Options := StaticOptions;
        //  "�� ���������", ����� ������ ��������������� � �����
        //  �� ���������� ��������� ���� � ������� ��������
    end;    //  with Grid do...

    //  ��� �������� � �������� ������ ����
    //  �� ��������� ������/������ �����
    SetCaption(Grid);
    SetColOptions(Grid);
    SetColFunc(Grid);

    //  ������������� ����� ����� ������, ��� �������� ������
    //  ��� �� �������� ������ � SetRowContents; ��� �����
    //  ����������� ��������� ������ ������� ��������� ����
    //  ���������� ������, ���� ��� ��� ������������ ��������,
    //  ������� ����� ������������ ������ ������� ��� ���������
    if Grid is TColorStringGrid then
        with Grid as TColorStringGrid do
            EnumerateRows;

    if Grid is TIDAGrid then
        with Grid as TIDAGrid do
            Changeable := False;
    //  �� ��������� ���� ������ � ������ ��������

    if Grid is TDataGrid then
        with Grid as TDataGrid do
            ShowTable;

    SetDataToGrid(Grid);    //  ������ ��� ������ ����������� ��
    //  ��������� ������ � ������ �����

    SetColWidths(Grid);
    SetRowHeights(Grid);
end;

procedure TTableCompList.SetColWidths(Grid: TStringGrid);
var
    i: longint;
begin
    if (Grid is TIDAGrid) and (not FWidthsSaved) then
        with Grid as TIDAGrid do
            AutoColWidths
    else
        with Grid do
            for i := 0 to ColCount - 1 do
                ColWidths[i] := GetColWidth(i);
end;

procedure TTableCompList.GetColWidths(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := 0 to ColCount - 1 do
            SaveColWidth(i, ColWidths[i]);
end;

procedure TTableCompList.GridRelease(Grid: TStringGrid);
begin
    GetRowHeights(Grid);
    GetColWidths(Grid);

    with Grid do
    begin
        SaveLeftCol(LeftCol);
        SaveTopRow(TopRow);
        SaveCol(Col);
        SaveRow(Row);

        SaveSelection(Selection);
    end;

    FSettingsSaved := True;
end;

{ TRowCompList }

procedure TRowCompList.AllDataDeleted;
begin
    if not (Count = 0) then
        Clear;
end;

function TRowCompList.GetColNumFixed: boolean;
begin
    Result := True;
end;

function TRowCompList.GetDataFromGrid(Grid: TStringGrid): boolean;
var
    i: longint;
begin
    //  !!! �� ������ ���� ������� ������, ���������
    //  GetRowContents �� ������� ����� ������� !!!
    Result := True;
    with Grid do
        for i := FixedRows to RowCount - 1 do
            if not GetRowContents(Grid, i) then
                Result := False;
end;

function TRowCompList.GetInfoRows: longint;
begin
    if Count <> 0 then
        Result := Count
    else
        Result := 1;
end;

function TRowCompList.IsDataSourceEmpty: boolean;
begin
    Result := Count = 0;
end;

function TRowCompList.MayIDoAddRow: boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearAllCells: boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearSelectedArea: boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoDeleteAllData: boolean;
begin
    Result := True;
end;

{$hints off}
function TRowCompList.MayIDoDeleteRows(StartRow, RowsCount: integer): boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoInsertRows(StartRow, RowsCount: integer): boolean;
begin
    Result := True;
end;

{$hints on}

procedure TRowCompList.RowAdded;
begin
    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� ������
    Add(CreateNewObject);
end;

procedure TRowCompList.RowsDeleted(const StartRow, RowsCount: integer);
var
    i: longint;
    First, Last: longint;
begin
    //  �������� ��������� ��� ������ ������ �� ������
    //  �������� ����������, ��� ��� ������������, �
    //  ��������, ����� ���������� ������� ������ �������
    //  ��� �������� ������� ��� �������� ������ -
    //  ������ ������ �� ����� ������
    if not (Count = 0) then
    begin
        Last  := StartRow - GetFixedRows + RowsCount - 1;
        First := StartRow - GetFixedRows;
        if (First < 0) or (Last > Count - 1) then
            raise ERowCompList.Create('Invalid deleting parameters...');
        i := 0;
        while i < RowsCount do
        begin
            Delete(First);
            Inc(i);
        end;
    end;
end;

procedure TRowCompList.RowsInserted(const StartRow, RowsCount: integer);
var
    i:     longint;
    First: longint;
begin
    First := StartRow - GetFixedRows;
    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� ������ - ������� �����
    //  ��������, � ��� ����� ���������

    if (First < 0) or (First > Count - 1) then
        raise ERowCompList.Create('Invalid insertion parameters...');

    for i := 1 to RowsCount do
        Insert(First, CreateNewObject);
end;

{$hints off}
procedure TRowCompList.BeforeStringToValue(const ACol, ARow: integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� ������
end;

{$hints on}

function TRowCompList.Add(Item: TComponent): integer;
var
    Flag: boolean;
begin
    Flag   := Count = 0;
    Result := inherited Add(Item);  //  ������ ��� �� ���� !!!
    if FAreRowHeightsReady and not Flag then
        AddRowHeightItem;
    //  ������ ���������� ���������, ����� ��������
    //  �������� ���� ���������� ����������; ���
    //  ������� ������� Add ���������� ������,
    //  ������� ������ ��� ����� ����������
end;

procedure TRowCompList.Delete(Index: integer);
begin
    if (not FDestroying) and FAreRowHeightsReady then
        DeleteRowHeightItem(Index);
    //  ������ ���������� ������, ����� ��������
    //  �������� ���� ���������� ����������
    inherited;  //  ����� ��������� � ������ ����������
    if (not FDestroying) and (Count = 0) and FAreRowHeightsReady then
        AddRowHeightItem;
    //  ���� ������� ��� ������ ����� �������� ����
    //  ������� �� ������ ������
end;

procedure TRowCompList.Insert(Index: integer; Item: TComponent);
var
    Flag: boolean;
begin
    Flag := Count = 0;
    inherited;
    if FAreRowHeightsReady and not Flag then
        InsertRowHeightItem(Index);
    //  ������ ���������� ���������, ����� ��������
    //  �������� ���� ���������� ����������
end;

procedure TRowCompList.SetDataToGrid(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := FixedRows to RowCount - 1 do
            SetRowContents(Grid, i);
end;

{ TColCompList }

function TColCompList.Add(Item: TComponent): integer;
var
    Flag: boolean;
begin
    Flag   := Count = 0;
    Result := inherited Add(Item);  //  ������ ��� �� ���� !!!
    if FAreColWidthsReady and not Flag then
        AddColWidthItem;
    //  ������ ���������� ���������, ����� ��������
    //  �������� ���� ���������� ����������; ���
    //  ������� ������� Add ���������� ������,
    //  ������� ������ ��� ����� ����������
end;

procedure TColCompList.AllDataDeleted;
begin
    if not (Count = 0) then
        Clear;
end;

procedure TColCompList.ColumnAdded;
begin
    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� �������
    Add(CreateNewObject);
end;

procedure TColCompList.ColumnsDeleted(const StartCol, ColsCount: integer);
var
    i: longint;
    First, Last: longint;
begin
    //  �������� ��������� ��� ������ ������ �� ������
    //  �������� ����������, ��� ��� ������������, �
    //  ��������, ����� ���������� ������� ������ ������
    //  ��� ������� ������� ��� �������� ������ -
    //  ������ ������ �� ����� ������
    if not (Count = 0) then
    begin
        Last  := StartCol - GetFixedCols + ColsCount - 1;
        First := StartCol - GetFixedCols;
        if (First < 0) or (Last > Self.Count - 1) then
            raise EColCompList.Create('Invalid deleting parameters...');
        i := 0;
        while i < ColsCount do
        begin
            Delete(First);
            Inc(i);
        end;
    end;
end;

procedure TColCompList.ColumnsInserted(const StartCol, ColsCount: integer);
var
    i:     longint;
    First: longint;
begin
    First := StartCol - GetFixedCols;
    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� ������� - ������� �����
    //  ��������, � ��� ����� ���������

    if (First < 0) or (First > Self.Count - 1) then
        raise EColCompList.Create('Invalid insertion parameters...');

    for i := 1 to ColsCount do
        Insert(First, CreateNewObject);
end;

procedure TColCompList.Delete(Index: integer);
begin
    if (not FDestroying) and FAreColWidthsReady then
        DeleteColWidthItem(Index);
    //  ������ ���������� ������, ����� ��������
    //  �������� ���� ���������� ����������
    inherited;  //  ����� ��������� � ������ ����������
    if (not FDestroying) and (Count = 0) and FAreColWidthsReady then
        AddColWidthItem;
    //  ���� ������� ��� ������ ����� �������� ����
    //  ������� �� ������ �������
end;

function TColCompList.GetDataFromGrid(Grid: TStringGrid): boolean;
var
    i: longint;
begin
    Result := True;
    with Grid do
        for i := FixedCols to ColCount - 1 do
            if not GetColContents(Grid, i) then
                Result := False;
end;

function TColCompList.GetInfoCols: longint;
begin
    if Count <> 0 then
        Result := Count
    else
        Result := 1;
end;

function TColCompList.GetRowNumFixed: boolean;
begin
    Result := True;
end;

procedure TColCompList.Insert(Index: integer; Item: TComponent);
var
    Flag: boolean;
begin
    Flag := Count = 0;
    inherited;
    if FAreColWidthsReady and not Flag then
        InsertColWidthItem(Index);
    //  ������ ���������� ���������, ����� ��������
    //  �������� ���� ���������� ����������
end;

function TColCompList.IsDataSourceEmpty: boolean;
begin
    Result := Count = 0;
end;

function TColCompList.MayIDoAddColumn: boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearAllCells: boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearSelectedArea: boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoDeleteAllData: boolean;
begin
    Result := True;
end;

{$hints off}
function TColCompList.MayIDoDeleteColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoInsertColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := True;
end;

{$hints on}

procedure TColCompList.SetDataToGrid(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := FixedCols to ColCount - 1 do
            SetColContents(Grid, i);
end;

{$hints off}
procedure TColCompList.BeforeStringToValue(const ACol, ARow: integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if Count = 0 then
        Add(CreateNewObject);
    //  � ������ ������ ����������� �������
end;

function TTableCompList.GetCellColor(const ACol, ARow: integer;
    var Color: TColor): boolean;
begin
    Color  := clDefault;
    Result := False;
end;

function TTableCompList.GetCellEditMask(const ACol, ARow: longint): string;
begin
    Result := '';
end;

function TTableCompList.IsCellDisabled(const ACol, ARow: integer): boolean;
begin
    Result := False;
end;

{$hints on}

procedure TTableCompList.AllDataDeleted;
begin
    raise ETableCompList.Create('All data deleting is impossible...');
end;

procedure TTableCompList.ColumnAdded;
begin
    raise ETableCompList.Create('Columns adding is impossible...');
end;

{$hints off}
procedure TTableCompList.ColumnsDeleted(const StartCol, ColsCount: integer);
begin
    raise ETableCompList.Create('Columns deleting is impossible...');
end;

procedure TTableCompList.ColumnsInserted(const StartCol, ColsCount: integer);
begin
    raise ETableCompList.Create('Columns insertion is impossible...');
end;

{$hints on}

procedure TTableCompList.RowAdded;
begin
    raise ETableCompList.Create('Row adding is impossible...');
end;

{$hints off}
procedure TTableCompList.RowsDeleted(const StartRow, RowsCount: integer);
begin
    raise ETableCompList.Create('Row deleting is impossible...');
end;

procedure TTableCompList.RowsInserted(const StartRow, RowsCount: integer);
begin
    raise ETableCompList.Create('Row insertion is impossible...');
end;

{$hints on}

function TTableCompList.MayIDoAddColumn: boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoAddRow: boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearAllCells: boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearSelectedArea: boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteAllData: boolean;
begin
    Result := False;
end;

{$hints off}
function TTableCompList.MayIDoDeleteColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteRows(StartRow, RowsCount: integer): boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertColumns(StartCol, ColsCount: integer): boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertRows(StartRow, RowsCount: integer): boolean;
begin
    Result := False;
end;

{$hints on}

function TTableCompList.GetColNumFixed: boolean;
begin
    Result := False;
end;

function TTableCompList.GetFixedCols: longint;
begin
    Result := 1;
end;

function TTableCompList.GetFixedRows: longint;
begin
    Result := 1;
end;

function TTableCompList.GetRowNumFixed: boolean;
begin
    Result := False;
end;

function TTableCompList.GetColCount: longint;
begin
    Result := GetInfoCols + GetFixedCols;
end;

function TTableCompList.GetRowCount: longint;
begin
    Result := GetInfoRows + GetFixedRows;
end;

function TTableCompList.GetCol: longint;
begin
    Result := FSavedCol;
end;

function TTableCompList.GetColWidth(const Col: integer): longint;
begin
    CheckColIndex(Col);
    InitColWidths;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ������� ����������
    //  �� ����� �������� �������
    Result := FSavedColWidths[Col];
end;

function TTableCompList.GetLeftCol: longint;
begin
    Result := FSavedLeftCol;
end;

function TTableCompList.GetRow: longint;
begin
    Result := FSavedRow;
end;

function TTableCompList.GetRowHeight(const Row: integer): longint;
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ����� ����������
    //  �� ����� �������� �������
    Result := FSavedRowHeights[Row];
end;

function TTableCompList.GetSelection: TGridRect;
begin
    Result := FSavedSelection;
end;

function TTableCompList.GetTopRow: longint;
begin
    Result := FSavedTopRow;
end;

procedure TTableCompList.SaveCol(const Col: integer);
begin
    FSavedCol := Col;
end;

procedure TTableCompList.SaveColWidth(const Col, Width: integer);
begin
    CheckColIndex(Col);
    InitColWidths;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ������� ����������
    //  �� ����� �������� �������
    FSavedColWidths[Col] := Width;
    FWidthsSaved := True;
end;

procedure TTableCompList.SaveLeftCol(const LeftCol: integer);
begin
    FSavedLeftCol := LeftCol;
end;

procedure TTableCompList.SaveRow(const Row: integer);
begin
    FSavedRow := Row;
end;

procedure TTableCompList.SaveRowHeight(const Row, Height: integer);
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ����� ����������
    //  �� ����� �������� �������
    FSavedRowHeights[Row] := Height;
    FHeightsSaved := True;
end;

procedure TTableCompList.SaveSelection(const Selection: TGridRect);
begin
    FSavedSelection := Selection;
end;

procedure TTableCompList.SaveTopRow(const TopRow: integer);
begin
    FSavedTopRow := TopRow;
end;

procedure TTableCompList.AddColWidthItem;
begin
    CheckColIndex(Length(FSavedColWidths)(* - 1 + 1*));
    AddItemLongArr(FSavedColWidths,
        GetColWidthByDefault(Length(FSavedColWidths)(* - 1 + 1*)));
    //  ��������� ������� ����� ������ Length - 1,
    //  � ��������� ����� ������ �� 1 ������
end;

procedure TTableCompList.AddRowHeightItem;
begin
    CheckRowIndex(Length(FSavedRowHeights)(* - 1 + 1*));
    AddItemLongArr(FSavedRowHeights,
        GetRowHeightByDefault(Length(FSavedRowHeights)(* - 1 + 1*)));
    //  ��������� ������� ����� ������ Length - 1,
    //  � ��������� ����� ������ �� 1 ������
end;

procedure TTableCompList.DeleteColWidthItem(const Index: integer);
begin
    CheckColIndex(Index);
    DeleteItemLongArr(FSavedColWidths, Index);
end;

procedure TTableCompList.DeleteRowHeightItem(const Index: integer);
begin
    CheckRowIndex(Index);
    DeleteItemLongArr(FSavedRowHeights, Index);
end;

procedure TTableCompList.InsertColWidthItem(const Index: integer);
begin
    CheckColIndex(Index);
    InsertItemLongArr(FSavedColWidths, Index, GetColWidthByDefault(Index));
end;

procedure TTableCompList.InsertRowHeightItem(const Index: integer);
begin
    CheckRowIndex(Index);
    InsertItemLongArr(FSavedRowHeights, Index, GetRowHeightByDefault(Index));
end;

procedure TTableCompList.CheckColIndex(const Index: integer);
begin
    if (Index < 0) or (Index >= GetColCount) then
        raise ETableCompList.Create('Invalid column index...');
end;

procedure TTableCompList.CheckRowIndex(const Index: integer);
begin
    if (Index < 0) or (Index >= GetRowCount) then
        raise ETableCompList.Create('Invalid row index...');
end;

procedure TTableCompList.DeleteAllColWidthItems;
begin
    Finalize(FSavedColWidths);
end;

procedure TTableCompList.DeleteAllRowHeightItems;
begin
    Finalize(FSavedRowHeights);
end;

function TTableCompList.GetColWidthByDefault(const Index: integer): longint;
begin
    CheckColIndex(Index);
    Result := 64;
end;

function TTableCompList.GetRowHeightByDefault(const Index: integer): longint;
begin
    CheckRowIndex(Index);
    Result := 20;
end;

procedure TTableCompList.GetRowHeights(Grid: TStringGrid);
var
    i: longint;
begin
    with Grid do
        for i := 0 to RowCount - 1 do
            SaveRowHeight(i, RowHeights[i]);
end;

procedure TTableCompList.SetRowHeights(Grid: TStringGrid);
var
    i: longint;
begin
    if (Grid is TIDAGrid) and (not FHeightsSaved) then
        with Grid as TIDAGrid do
            AutoRowHeights
    else
        with Grid do
            for i := 0 to RowCount - 1 do
                RowHeights[i] := GetRowHeight(i);
end;

procedure TTableCompList.InitColWidths;
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

procedure TTableCompList.InitRowHeights;
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

function TTableCompList.AutoHeights: boolean;
begin
    Result := not FHeightsSaved;
end;

function TTableCompList.AutoWidths: boolean;
begin
    Result := not FWidthsSaved;
end;

procedure TTableCompList.SetColFunc(Grid: TStringGrid);
var
    i: longint;
begin
    //  ��������� �� �-� ������������� � Fixed �������
    //  "�����" �������
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to ColCount - 1 do
                Objects[i, 0] := nil;
end;

end.
