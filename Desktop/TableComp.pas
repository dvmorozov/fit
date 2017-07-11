//      ������� ����� ������ �������������� ���������, ����������� ��
//      ���� ������� ���������; ��������� �������� �������������� ���������,
//      ����������� ������ � ������ ��������� ��� ����������� ���������������
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit TableComp;

{$MODE Delphi}

interface

uses
    Classes, Grids, Controls, Graphics, Tools,
    SysUtils, NumericGrid, MyExceptions, SelfCopied;

type
    ETableCompList = class(Exception);

    TTableCompList = class(TSelfCopiedCompList, IGridDataSource)
        //  ����������� ������ �����������, ������� ����� ����������
        //  �������� ����������� � �������; ����� ��������� �������
        //  ����������/������ ������� �������, �� ����� �� ���������
        //  ������������ ������ � ������� � ���������� ������

        //  !!! ��� ��������� ���������� �������, ������������ GetColCount
        //  ����� ����������� �������� �������������� ��������� � SetCaption,
        //  SetColOptions, SetColFunc, SetRowContents, GetRowContents !!!
    protected
        FCaption: string;

        SavedColWidths: TLongArray;
        SavedRowHeights: TLongArray;
            //  ������� ��������� � �����������, ������� ��������
            //  ��������� ������� � ������ �������� ����� ������
            //  ������ ���� ���������
        AreColWidthsReady: Boolean;     //  ������� ����, ��� ������ ���
        AreRowHeightsReady: Boolean;    //  ���������������

            //  ����������� �������� �������
        FSavedCol, FSavedRow, FSavedLeftCol, FSavedTopRow: LongInt;
        FSavedSelection: TGridRect;

        SettingsSaved: Boolean;
            //  ������� ����, ��� ��������� ������� ����
            //  ��������� - ��������������� � GridRelease
        HeightsSaved, WidthsSaved: Boolean;

        Destroying: Boolean;
            //  ������ ��������� � ��������� ��������

        //  ��������� ������������ �������� ������� ������� (������),
        //  � ������ �������������� ������������ ����������
        procedure CheckColIndex(const Index: LongInt);
        procedure CheckRowIndex(const Index: LongInt);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        ////////////////////////////////////////////////////////////////////////
        //  ������ ������� ���������� ��������� ��� ������������� � ��������
        //  ������������, � ����� ��� ����, ����� ����� ���� �������� �
        //  ����������� ������� ��� ����� ���������� ������� �� ���������
        //
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
        function GetDataFromGrid(Grid: TStringGrid): Boolean; virtual; abstract;

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

        procedure SetRowContents(
            //  ��������� ������ ������� � ������� RowNum
            Grid: TStringGrid; RowNum: LongInt); virtual; abstract;
        function GetRowContents(
            //  ��������� ���������� ������ � ������� RowNum
            Grid: TStringGrid; RowNum: LongInt): Boolean; virtual; abstract;
        procedure SetColContents(
            //  ��������� ������� ������� � ������� ColNum
            Grid: TStringGrid; ColNum: LongInt); virtual; abstract;
        function GetColContents(
            //  ��������� ���������� ������� � ������� ColNum
            Grid: TStringGrid; ColNum: LongInt): Boolean; virtual; abstract;

        procedure InitColWidths;    //  �������������� ������������� �������
        procedure InitRowHeights;

        ////////////////////////////////////////////////////////////////////////
        //  ������ ������ ����������
        //
        //  ������ ������� ���������� �������������, ��� ��������
        //  �������� �����, � �������� ������ ��������� (��� �����
        //  ���������� � ��������� �������)
        ////////////////////////////////////////////////////////////////////////

        function IsDataSourceEmpty: Boolean; virtual; abstract;

        function ValueToString(const ACol, ARow: LongInt
            ): string; virtual; abstract;
        procedure BeforeStringToValue(const ACol, ARow: LongInt;
            const AString: string); virtual; abstract;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); virtual; abstract;
        procedure SetValueByDefault(const ACol, ARow: LongInt); virtual; abstract;
            //  ������������� ���������� �������� "�� ���������"
            //  ��� ������ ������; ������������ ��� ��������� ��������
            //  ������� ������
        function GetCellColor(
            //  ���������� True, ���� ������ ���� ����������
            //  ���� Color, � ��������� ������ - False
            //  (������� ��������� ���� "�� ���������")
            const ACol, ARow: LongInt;
            var Color: TColor): Boolean; virtual;
        function GetCellEditMask(
            const ACol, ARow: LongInt): string; virtual; abstract;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; virtual; abstract;
        function IsCellDisabled(
            //  ���������� ������� ���������� �����
            //  � ������ - True - ���� ��������
            const ACol, ARow: LongInt): Boolean; virtual;

        function IsDataValid(const ACol, ARow: LongInt;
            //  ��������� "������" �������� ������ ��� �����������
            //  ����������; ������, ���� ���������� ������/�������
            //  ����� ������������ �������� ���������� ����������;
            //  ������ ���������� True - ��������� ��� ����������
            //  ������ ��������
            const AString: string): Boolean; virtual; abstract;

        //  ��� ������� �������� ����������� ���������� ��������
        //  ���������� False; ������ - ������� ������ ��������� ��
        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; virtual;
        function MayIDoAddRow: Boolean; virtual;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; virtual;
        function MayIDoAddColumn: Boolean; virtual;

        function MayIDoDeleteAllData: Boolean; virtual;
        function MayIDoClearSelectedArea: Boolean; virtual;
        function MayIDoClearAllCells: Boolean; virtual;

        //  ������� ���������� ��������� ����������� ��������
        //  ������ (������) ������� (��������)
        procedure DeleteAllColWidthItems;
        procedure DeleteColWidthItem(const Index: LongInt);
        procedure InsertColWidthItem(const Index: LongInt);
        procedure AddColWidthItem;

        procedure DeleteAllRowHeightItems;        
        procedure DeleteRowHeightItem(const Index: LongInt);
        procedure InsertRowHeightItem(const Index: LongInt);
        procedure AddRowHeightItem;

        function GetColWidthByDefault(
            const Index: LongInt): LongInt; virtual;
        function GetRowHeightByDefault(
            const Index: LongInt): LongInt; virtual;

        //  ��� ������ ��������, �������, ������� ��������
        //  ���������� � ���������� � ������������� ���������
        //  ������ ��������; ������ - ������� ������ ���������
        //  ��� ������ ��� ���������� ��������� ��������
        procedure RowsDeleted(
            const StartRow, RowsCount: LongInt); virtual;
        procedure RowsInserted(
            const StartRow, RowsCount: LongInt); virtual;
        procedure RowAdded; virtual;

        procedure ColumnsDeleted(
            const StartCol, ColsCount: LongInt); virtual;
        procedure ColumnsInserted(
            const StartCol, ColsCount: LongInt); virtual;
        procedure ColumnAdded; virtual;

        procedure AllDataDeleted; virtual;

        function GetColCount: LongInt; virtual;
            //  ������ ����� �������, ������� Fixed
        function GetRowCount: LongInt; virtual;
            //  ������ ����� �����, ������� Fixed
        function GetInfoCols: LongInt; virtual; abstract;
            //  ����� ������� �������������� ����� �������
        function GetInfoRows: LongInt; virtual; abstract;
            //  ����� ����� �������������� ����� �������
        function GetFixedCols: LongInt; virtual;
            //  ����� ������������� ������� (�� ��������� = 1)
        function GetFixedRows: LongInt; virtual;
            //  ����� ������������� ����� (�� ��������� = 1)
        function GetColNumFixed: Boolean; virtual;  //  (�� ��������� = False)
        function GetRowNumFixed: Boolean; virtual;  //  (�� ��������� = False)

        function GetColWidth(const Col: LongInt): LongInt;
        procedure SaveColWidth(const Col, Width: LongInt);
        function GetRowHeight(const Row: LongInt): LongInt;
        procedure SaveRowHeight(const Row, Height: LongInt);
        function AutoWidths: Boolean;
        function AutoHeights: Boolean;

        function GetSelection: TGridRect;
        procedure SaveSelection(const Selection: TGridRect);
        function GetCol: LongInt;       //  ����� ������� ��������� �������
        procedure SaveCol(const Col: LongInt);
        function GetRow: LongInt;       //  ����� ������� ��������� ������
        procedure SaveRow(const Row: LongInt);
        function GetLeftCol: LongInt;
        procedure SaveLeftCol(const LeftCol: LongInt);
        function GetTopRow: LongInt;
        procedure SaveTopRow(const TopRow: LongInt);

        property SavedCol: LongInt
            read GetCol                 write SaveCol;
        property SavedRow: LongInt
            read GetRow                 write SaveRow;
        property SavedLeftCol: LongInt
            read GetLeftCol             write SaveLeftCol;
        property SavedTopRow: LongInt
            read GetTopRow              write SaveTopRow;
        property SavedSelection: TGridRect
            read GetSelection           write SaveSelection;

        property Caption: string
            read FCaption               write FCaption;
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
        function GetDataFromGrid(Grid: TStringGrid): Boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): Integer; override;
        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: Integer;
            //  ��������� ������������ �������� � � ������ ������
            //  �������� ����������; ���� ������ ������ ���������
            //  ������ - ������; !!! ����� ����������� ��������
            //  � ������� - ����������� !!!
            const AString: string); override;

        function MayIDoInsertRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoDeleteRows(StartRow, RowsCount: LongInt): Boolean; override;
        function MayIDoAddRow: Boolean; override;

        function MayIDoDeleteAllData: Boolean; override;
        function MayIDoClearAllCells: Boolean; override;
        function MayIDoClearSelectedArea: Boolean; override;

        procedure RowsDeleted(
            const StartRow, RowsCount: LongInt); override;
        procedure RowsInserted(
            const StartRow, RowsCount: LongInt); override;
        procedure RowAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: Boolean; override;

        function GetInfoRows: LongInt; override;
            //  ���������� ��������� ����� ����� = Fixed + 1
            //  ��� ����������� ����������� �����
        function GetColNumFixed: Boolean; override;
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
        function GetDataFromGrid(Grid: TStringGrid): Boolean; override;
        procedure SetDataToGrid(Grid: TStringGrid); override;

        function Add(Item: TComponent): Integer; override;
        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;

        procedure BeforeStringToValue(const ACol, ARow: LongInt;
            //  ��������� ������������ �������� � � ������ ������
            //  �������� ����������; ���� ������ ������ ���������
            //  ������ - �������; !!! ����� ����������� ��������
            //  � ������� - ����������� !!!
            const AString: string); override;

        function MayIDoInsertColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoDeleteColumns(StartCol, ColsCount: LongInt): Boolean; override;
        function MayIDoAddColumn: Boolean; override;

        function MayIDoDeleteAllData: Boolean; override;
        function MayIDoClearSelectedArea: Boolean; override;
        function MayIDoClearAllCells: Boolean; override;

        procedure ColumnsDeleted(
            const StartCol, ColsCount: LongInt); override;
        procedure ColumnsInserted(
            const StartCol, ColsCount: LongInt); override;
        procedure ColumnAdded; override;

        procedure AllDataDeleted; override;
        function IsDataSourceEmpty: Boolean; override;

        function GetInfoCols: LongInt; override;
            //  ���������� ��������� ����� ������� = Fixed + 1
            //  ��� ����������� ����������� �����
        function GetRowNumFixed: Boolean; override;
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
    inherited Create(AOwner);

    FSavedCol := GetFixedCols;
    FSavedRow := GetFixedRows;
    FSavedLeftCol := FSavedCol;
    FSavedTopRow := FSavedRow;
    with FSavedSelection do
    begin
        Left := FSavedCol;
        Top := FSavedRow;
        Right := FSavedCol;
        Bottom := FSavedRow;        
    end;
end;

destructor TTableCompList.Destroy;
begin
    Destroying := True;
    Finalize(SavedColWidths);
    Finalize(SavedrowHeights);
    inherited Destroy;
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
        TopRow := GetTopRow;
        Col := GetCol;
        Row := GetRow;

        Selection := GetSelection;
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
        with Grid as TColorStringGrid do EnumerateRows;

    if Grid is TIDAGrid then
        with Grid as TIDAGrid do Changeable := False;
    //  �� ��������� ���� ������ � ������ ��������

    if Grid is TDataGrid then
        with Grid as TDataGrid do ShowTable;

    SetDataToGrid(Grid);    //  ������ ��� ������ ����������� ��
                            //  ��������� ������ � ������ �����

    SetColWidths(Grid);
    SetRowHeights(Grid);
end;

procedure TTableCompList.SetColWidths(Grid: TStringGrid);
var i: LongInt;
begin
    if (Grid is TIDAGrid) and (not WidthsSaved) then
        with Grid as TIDAGrid do AutoColWidths
    else
        with Grid do
            for i := 0 to ColCount - 1 do ColWidths[i] := GetColWidth(i);
end;

procedure TTableCompList.GetColWidths(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := 0 to ColCount - 1 do SaveColWidth(i, ColWidths[i]);
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

    SettingsSaved := True;
end;

{ TRowCompList }

procedure TRowCompList.AllDataDeleted;
begin
    if not (Count = 0) then Clear;
end;

function TRowCompList.GetColNumFixed: Boolean;
begin
    Result := True;
end;

function TRowCompList.GetDataFromGrid(Grid: TStringGrid): Boolean;
var i: LongInt;
begin
    //  !!! �� ������ ���� ������� ������, ���������
    //  GetRowContents �� ������� ����� ������� !!!
    Result := True;
    with Grid do
        for i := FixedRows to RowCount - 1 do
            if not GetRowContents(Grid, i) then Result := False;
end;

function TRowCompList.GetInfoRows: LongInt;
begin
    if Count <> 0 then Result := Count
    else Result := 1;
end;

function TRowCompList.IsDataSourceEmpty: Boolean;
begin
    Result := Count = 0;
end;

function TRowCompList.MayIDoAddRow: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearAllCells: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoDeleteRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := True;
end;

function TRowCompList.MayIDoInsertRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := True;
end;

procedure TRowCompList.RowAdded;
begin
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� ������
    Add(CreateNewObject);
end;

procedure TRowCompList.RowsDeleted(const StartRow, RowsCount: Integer);
var i: LongInt;
    First, Last: LongInt;
begin
    //  �������� ��������� ��� ������ ������ �� ������
    //  �������� ����������, ��� ��� ������������, �
    //  ��������, ����� ���������� ������� ������ �������
    //  ��� �������� ������� ��� �������� ������ -
    //  ������ ������ �� ����� ������
    if not (Count = 0) then
    begin
        Last := StartRow - GetFixedRows + RowsCount - 1;
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

procedure TRowCompList.RowsInserted(const StartRow, RowsCount: Integer);
var i: LongInt;
    First: LongInt;
begin
    First := StartRow - GetFixedRows;
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� ������ - ������� �����
        //  ��������, � ��� ����� ���������

    if (First < 0) or (First > Count - 1) then
        raise ERowCompList.Create('Invalid insertion parameters...');

    for i := 1 to RowsCount do Insert(First, CreateNewObject);
end;

procedure TRowCompList.BeforeStringToValue(const ACol, ARow: Integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� ������
end;

function TRowCompList.Add(Item: TComponent): Integer;
var Flag: Boolean;
begin
    Flag := Count = 0;
    Result := inherited Add(Item);  //  ������ ��� �� ���� !!!
    if AreRowHeightsReady and not Flag then AddRowHeightItem;
        //  ������ ���������� ���������, ����� ��������
        //  �������� ���� ���������� ����������; ���
        //  ������� ������� Add ���������� ������,
        //  ������� ������ ��� ����� ����������
end;

procedure TRowCompList.Delete(Index: Integer);
begin
    if (not Destroying) and AreRowHeightsReady then
        DeleteRowHeightItem(Index);
        //  ������ ���������� ������, ����� ��������
        //  �������� ���� ���������� ����������
    inherited;  //  ����� ��������� � ������ ����������
    if (not Destroying) and
        (Count = 0) and AreRowHeightsReady then AddRowHeightItem;
        //  ���� ������� ��� ������ ����� �������� ����
        //  ������� �� ������ ������
end;

procedure TRowCompList.Insert(Index: Integer; Item: TComponent);
var Flag: Boolean;
begin
    Flag := Count = 0;
    inherited;
    if AreRowHeightsReady and not Flag then InsertRowHeightItem(Index);
        //  ������ ���������� ���������, ����� ��������
        //  �������� ���� ���������� ����������
end;

procedure TRowCompList.SetDataToGrid(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := FixedRows to RowCount - 1 do SetRowContents(Grid, i);
end;

{ TColCompList }

function TColCompList.Add(Item: TComponent): Integer;
var Flag: Boolean;
begin
    Flag := Count = 0;
    Result := inherited Add(Item);  //  ������ ��� �� ���� !!!
    if AreColWidthsReady and not Flag then AddColWidthItem;
        //  ������ ���������� ���������, ����� ��������
        //  �������� ���� ���������� ����������; ���
        //  ������� ������� Add ���������� ������,
        //  ������� ������ ��� ����� ����������
end;

procedure TColCompList.AllDataDeleted;
begin
    if not (Count = 0) then Clear;
end;

procedure TColCompList.ColumnAdded;
begin
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� �������
    Add(CreateNewObject);
end;

procedure TColCompList.ColumnsDeleted(const StartCol, ColsCount: Integer);
var i: LongInt;
    First, Last: LongInt;
begin
    //  �������� ��������� ��� ������ ������ �� ������
    //  �������� ����������, ��� ��� ������������, �
    //  ��������, ����� ���������� ������� ������ ������
    //  ��� ������� ������� ��� �������� ������ -
    //  ������ ������ �� ����� ������
    if not (Count = 0) then
    begin
        Last := StartCol - GetFixedCols + ColsCount - 1;
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

procedure TColCompList.ColumnsInserted(const StartCol, ColsCount: Integer);
var i: LongInt;
    First: LongInt;
begin
    First := StartCol - GetFixedCols;
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� ������� - ������� �����
        //  ��������, � ��� ����� ���������

    if (First < 0) or (First > Self.Count - 1) then
        raise EColCompList.Create('Invalid insertion parameters...');

    for i := 1 to ColsCount do Insert(First, CreateNewObject);
end;

procedure TColCompList.Delete(Index: Integer);
begin
    if (not Destroying) and AreColWidthsReady then
        DeleteColWidthItem(Index);
            //  ������ ���������� ������, ����� ��������
            //  �������� ���� ���������� ����������
    inherited;  //  ����� ��������� � ������ ����������
    if (not Destroying) and
        (Count = 0) and AreColWidthsReady then AddColWidthItem;
        //  ���� ������� ��� ������ ����� �������� ����
        //  ������� �� ������ �������
end;

function TColCompList.GetDataFromGrid(Grid: TStringGrid): Boolean;
var i: LongInt;
begin
    Result := True;
    with Grid do
        for i := FixedCols to ColCount - 1 do
            if not GetColContents(Grid, i) then Result := False;
end;

function TColCompList.GetInfoCols: LongInt;
begin
    if Count <> 0 then Result := Count
    else Result := 1;
end;

function TColCompList.GetRowNumFixed: Boolean;
begin
    Result := True;
end;

procedure TColCompList.Insert(Index: Integer; Item: TComponent);
var Flag: Boolean;
begin
    Flag := Count = 0;
    inherited;
    if AreColWidthsReady and not Flag then InsertColWidthItem(Index);
        //  ������ ���������� ���������, ����� ��������
        //  �������� ���� ���������� ����������
end;

function TColCompList.IsDataSourceEmpty: Boolean;
begin
    Result := Count = 0;
end;

function TColCompList.MayIDoAddColumn: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearAllCells: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := True;
end;

function TColCompList.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := True;
end;

procedure TColCompList.SetDataToGrid(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := FixedCols to ColCount - 1 do SetColContents(Grid, i);
end;

procedure TColCompList.BeforeStringToValue(const ACol, ARow: Integer;
    const AString: string);
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    
    if Count = 0 then Add(CreateNewObject);
        //  � ������ ������ ����������� �������
end;

function TTableCompList.GetCellColor(const ACol, ARow: Integer;
    var Color: TColor): Boolean;
begin
    Result := False;
end;

function TTableCompList.IsCellDisabled(const ACol, ARow: Integer): Boolean;
begin
    Result := False;
end;

procedure TTableCompList.AllDataDeleted;
begin
    raise ETableCompList.Create('All data deleting is impossible...');
end;

procedure TTableCompList.ColumnAdded;
begin
    raise ETableCompList.Create('Columns adding is impossible...');
end;

procedure TTableCompList.ColumnsDeleted(const StartCol,
    ColsCount: Integer);
begin
    raise ETableCompList.Create('Columns deleting is impossible...');
end;

procedure TTableCompList.ColumnsInserted(const StartCol,
    ColsCount: Integer);
begin
    raise ETableCompList.Create('Columns insertion is impossible...');
end;

procedure TTableCompList.RowAdded;
begin
    raise ETableCompList.Create('Row adding is impossible...');
end;

procedure TTableCompList.RowsDeleted(const StartRow, RowsCount: Integer);
begin
    raise ETableCompList.Create('Row deleting is impossible...');
end;

procedure TTableCompList.RowsInserted(const StartRow, RowsCount: Integer);
begin
    raise ETableCompList.Create('Row insertion is impossible...');
end;

function TTableCompList.MayIDoAddColumn: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoAddRow: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearAllCells: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoClearSelectedArea: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteAllData: Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoDeleteRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertColumns(StartCol,
    ColsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.MayIDoInsertRows(StartRow,
    RowsCount: Integer): Boolean;
begin
    Result := False;
end;

function TTableCompList.GetColNumFixed: Boolean;
begin
    Result := False;
end;

function TTableCompList.GetFixedCols: LongInt;
begin
    Result := 1;
end;

function TTableCompList.GetFixedRows: LongInt;
begin
    Result := 1;
end;

function TTableCompList.GetRowNumFixed: Boolean;
begin
    Result := False;
end;

function TTableCompList.GetColCount: LongInt;
begin
    Result := GetInfoCols + GetFixedCols;
end;

function TTableCompList.GetRowCount: LongInt;
begin
    Result := GetInfoRows + GetFixedRows;
end;

function TTableCompList.GetCol: LongInt;
begin
    Result := FSavedCol;
end;

function TTableCompList.GetColWidth(const Col: Integer): LongInt;
begin
    CheckColIndex(Col);
    InitColWidths;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ������� ����������
    //  �� ����� �������� �������
    Result := SavedColWidths[Col];
end;

function TTableCompList.GetLeftCol: LongInt;
begin
    Result := FSavedLeftCol;
end;

function TTableCompList.GetRow: LongInt;
begin
    Result := FSavedRow;
end;

function TTableCompList.GetRowHeight(const Row: Integer): LongInt;
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ����� ����������
    //  �� ����� �������� �������
    Result := SavedRowHeights[Row];
end;

function TTableCompList.GetSelection: TGridRect;
begin
    Result := FSavedSelection;
end;

function TTableCompList.GetTopRow: LongInt;
begin
    Result := FSavedTopRow;
end;

procedure TTableCompList.SaveCol(const Col: Integer);
begin
    FSavedCol := Col;
end;

procedure TTableCompList.SaveColWidth(const Col, Width: Integer);
begin
    CheckColIndex(Col);
    InitColWidths;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ������� ����������
    //  �� ����� �������� �������
    SavedColWidths[Col] := Width;
    WidthsSaved := True;
end;

procedure TTableCompList.SaveLeftCol(const LeftCol: Integer);
begin
    FSavedLeftCol := LeftCol;
end;

procedure TTableCompList.SaveRow(const Row: Integer);
begin
    FSavedRow := Row;
end;

procedure TTableCompList.SaveRowHeight(const Row, Height: Integer);
begin
    CheckRowIndex(Row);
    InitRowHeights;
    //  �������������� ������������� �������
    //  ������� ����� ������, ��� �� ������
    //  ������ ������ ������������� � ������������,
    //  ��������, ����� ����� ����� ����������
    //  �� ����� �������� �������
    SavedRowHeights[Row] := Height;
    HeightsSaved := True;
end;

procedure TTableCompList.SaveSelection(const Selection: TGridRect);
begin
    FSavedSelection := Selection;
end;

procedure TTableCompList.SaveTopRow(const TopRow: Integer);
begin
    FSavedTopRow := TopRow;
end;

procedure TTableCompList.AddColWidthItem;
begin
    CheckColIndex(Length(SavedColWidths)(* - 1 + 1*));
    AddItemLongArr(SavedColWidths,
        GetColWidthByDefault(Length(SavedColWidths)(* - 1 + 1*)));
        //  ��������� ������� ����� ������ Length - 1,
        //  � ��������� ����� ������ �� 1 ������
end;

procedure TTableCompList.AddRowHeightItem;
begin
    CheckRowIndex(Length(SavedRowHeights)(* - 1 + 1*));
    AddItemLongArr(SavedRowHeights,
        GetRowHeightByDefault(Length(SavedRowHeights)(* - 1 + 1*)));
        //  ��������� ������� ����� ������ Length - 1,
        //  � ��������� ����� ������ �� 1 ������
end;

procedure TTableCompList.DeleteColWidthItem(const Index: Integer);
begin
    CheckColIndex(Index);
    DeleteItemLongArr(SavedColWidths, Index);
end;

procedure TTableCompList.DeleteRowHeightItem(const Index: Integer);
begin
    CheckRowIndex(Index);
    DeleteItemLongArr(SavedRowHeights, Index);
end;

procedure TTableCompList.InsertColWidthItem(const Index: Integer);
begin
    CheckColIndex(Index);
    InsertItemLongArr(SavedColWidths, Index, GetColWidthByDefault(Index));
end;

procedure TTableCompList.InsertRowHeightItem(const Index: Integer);
begin
    CheckRowIndex(Index);
    InsertItemLongArr(SavedRowHeights, Index, GetRowHeightByDefault(Index));
end;

procedure TTableCompList.CheckColIndex(const Index: Integer);
begin
    if (Index < 0) or (Index >= GetColCount) then
        raise ETableCompList.Create('Invalid column index...');
end;

procedure TTableCompList.CheckRowIndex(const Index: Integer);
begin
    if (Index < 0) or (Index >= GetRowCount) then
        raise ETableCompList.Create('Invalid row index...');
end;

procedure TTableCompList.DeleteAllColWidthItems;
begin
    Finalize(SavedColWidths);
end;

procedure TTableCompList.DeleteAllRowHeightItems;
begin
    Finalize(SavedRowHeights);
end;

function TTableCompList.GetColWidthByDefault(
    const Index: Integer): LongInt;
begin
    CheckColIndex(Index);
    Result := 64;
end;

function TTableCompList.GetRowHeightByDefault(
    const Index: Integer): LongInt;
begin
    CheckRowIndex(Index);
    Result := 20;
end;

procedure TTableCompList.GetRowHeights(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
        for i := 0 to RowCount - 1 do SaveRowHeight(i, RowHeights[i]);
end;

procedure TTableCompList.SetRowHeights(Grid: TStringGrid);
var i: LongInt;
begin
    if (Grid is TIDAGrid) and (not HeightsSaved) then
        with Grid as TIDAGrid do AutoRowHeights
    else
        with Grid do
            for i := 0 to RowCount - 1 do RowHeights[i] := GetRowHeight(i);
end;

procedure TTableCompList.InitColWidths;
var i: LongInt;
begin
    if not AreColWidthsReady then
    begin
        DeleteAllColWidthItems;
        for i := 1 to GetColCount do AddColWidthItem;
        AreColWidthsReady := True;
    end;
end;

procedure TTableCompList.InitRowHeights;
var i: LongInt;
begin
    if not AreRowHeightsReady then
    begin
        DeleteAllRowHeightItems;
        for i := 1 to GetRowCount do AddRowHeightItem;
        AreRowHeightsReady := True;
    end;
end;

function TTableCompList.AutoHeights: Boolean;
begin
    Result := not HeightsSaved;
end;

function TTableCompList.AutoWidths: Boolean;
begin
    Result := not WidthsSaved;
end;

procedure TTableCompList.SetColFunc(Grid: TStringGrid);
var i: LongInt;
begin
    //  ��������� �� �-� ������������� � Fixed �������
    //  "�����" �������
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to ColCount - 1 do Objects[i, 0] := nil;
end;

initialization
    RegisterClass(TRowCompList);
    RegisterClass(TColCompList);
    RegisterClass(TTableCompList);
end.
