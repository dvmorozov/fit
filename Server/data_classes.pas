{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of container classes.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit data_classes;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, Grids, NumericGrid, persistent_curve_parameters,
    special_curve_parameter, SysUtils, table_components;

type
    { Set of pattern instances forming the calculated profile in sum. }
    TCurveList = class(TRowCompList)
    protected
        function CreateNewObject: TComponent; override;
        { Generic method for conversion values to user friendly representation. }
        function RecalcParamValue(P: TSpecialCurveParameter): double; virtual;
        { Generic method for reverse conversion of value from user to internal representation. }
        procedure ReverseCalcParamValue(P: TSpecialCurveParameter;
            NewValue: double); virtual;

    public
        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;
        function GetRowContents(Grid: TStringGrid; RowNum: longint): boolean;
            override;
        procedure SetRowContents(Grid: TStringGrid; RowNum: longint); override;

        function ValueToString(const ACol, ARow: longint): string; override;
        procedure StringToValue(const ACol, ARow: longint; const AString: string);
            override;
        { Sets valid default value for the cell with given coordinates. Is used in cell initialization. }
        procedure SetValueByDefault(const ACol, ARow: longint); override;
        { Performs "soft" data validation without throwing an exception. However if coordinates of row
          or column have inadmissible values exception is thrown. Always returns true. Override to 
          perform suitable validation. }
        function IsDataValid(const ACol, ARow: longint; const AString: string): boolean;
            override;
        function GetCellEnabledCharSet(const ACol, ARow: longint): TCharSet; override;

        function GetInfoCols: longint; override;
    end;

implementation

procedure TCurveList.SetCaption;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    i, Index:  longint;
begin
    Assert(Assigned(Grid));
    Assert(Grid.FixedRows >= 1);

    if Count <> 0 then
    begin
        Index := 0;
        //  If the set of curver parameters is empty
        //  then it's impossible to set up headers.
        CurveParameters := Curve_parameters(Items[0]);
        //  !!! dolzhny obrabatyvat'sya vse parametry, krome argumenta !!!
        Assert(Grid.ColCount - Grid.FixedCols = CurveParameters.Params.Count - 1);
        for i := 0 to CurveParameters.Params.Count - 1 do
        begin
            Parameter := CurveParameters[i];
            if Parameter.Type_ <> Argument then
            begin
                Grid.Cells[Grid.FixedCols + Index, 0] := Parameter.Name;
                Inc(Index);
            end;
        end;
    end;
end;

procedure TCurveList.SetColOptions(Grid: TStringGrid);
var
    CurveParameters: Curve_parameters;
    Parameters: TSpecialCurveParameter;
    i, Index: longint;
begin
    Assert(Assigned(Grid));

    if Grid is TNumericGrid then
        with TNumericGrid(Grid) do
            if Count <> 0 then
            begin
                CurveParameters := Curve_parameters(Items[0]);
                //  All parameters must be processed except the argument.
                Assert(Grid.ColCount - Grid.FixedCols =
                    CurveParameters.Params.Count - 1);
                Index := 0;
                for i := 0 to CurveParameters.Params.Count - 1 do
                begin
                    Parameters := CurveParameters[i];
                    case Parameters.Type_ of
                        Argument: ;
                        Calculated:
                        begin
                            ColOptions[Grid.FixedCols + Index] := coDisabled;
                            Inc(Index);
                        end
                        else
                        begin
                            ColOptions[Grid.FixedCols + i] := coReal;
                            Inc(Index);
                        end;
                    end;
                end;
            end;
end;

procedure TCurveList.SetRowContents(Grid: TStringGrid; RowNum: longint);
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    i, Index:  longint;
begin
    Assert(Assigned(Grid));

    with Grid do
        if (RowNum >= FixedRows) and (RowNum - FixedRows < Count) then
        begin
            Cells[0, RowNum] := IntToStr(RowNum);

            CurveParameters := Curve_parameters(Items[RowNum - FixedRows]);
            //  All parameters must be processed except the argument.
            Assert(Grid.ColCount - Grid.FixedCols = CurveParameters.Params.Count - 1);
            Index := 0;
            for i := 0 to CurveParameters.Params.Count - 1 do
            begin
                Parameter := CurveParameters[i];
                if Parameter.Type_ <> Argument then
                begin
                    Cells[FixedCols + Index, RowNum] :=
                        FloatToStrF(RecalcParamValue(Parameter), ffFixed, 8, 4);
                    Inc(Index);
                end;
            end;
        end
        else
        begin
            //  Empty row is initialized.
            Cells[0, RowNum] := IntToStr(RowNum);
            for i := FixedCols to ColCount - 1 do
                Cells[i, RowNum] := '';
        end;
end;

function TCurveList.GetRowContents;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    i, Index:  longint;
begin
    Result := True;
    with Grid do
    begin
        Assert((RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count));

        CurveParameters := Curve_parameters(Items[RowNum - FixedRows]);
        //  All parameters must be processed except the argument.
        Assert(Grid.ColCount - Grid.FixedCols = CurveParameters.Params.Count - 1);
        Index := 0;
        for i := 0 to CurveParameters.Count - 1 do
        begin
            Parameter := CurveParameters[i];
            if Parameter.Type_ <> Argument then
            begin
                try
                    ReverseCalcParamValue(Parameter,
                        StrToFloat(Cells[FixedCols + Index, RowNum]));
                except
                    //  Non-fatal error in string conversion.
                    Result := False;
                end;
                Inc(Index);
            end;
        end;
    end;
end;

function TCurveList.CreateNewObject: TComponent;
begin
    Result := Curve_parameters.Create(nil);
end;

function TCurveList.RecalcParamValue(P: TSpecialCurveParameter): double;
begin
    Assert(Assigned(P));
    Result := P.Value;
end;

procedure TCurveList.ReverseCalcParamValue(P: TSpecialCurveParameter;
    NewValue: double);
begin
    Assert(Assigned(P));
    P.Value := NewValue;
end;

function TCurveList.GetInfoCols: longint;
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    i: longint;
begin
    if Count <> 0 then
    begin
        Result := 0;

        CurveParameters := Curve_parameters(Items[0]);
        for i := 0 to CurveParameters.Params.Count - 1 do
        begin
            Parameter := CurveParameters[i];
            if Parameter.Type_ <> Argument then
                Inc(Result);
        end;
    end
    else
        Result := GetFixedCols + 1;
end;

function TCurveList.ValueToString(const ACol, ARow: integer): string;
var
    CurveParameters: Curve_parameters;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        { Column headers are filled. }
        if ARow > 0 then
            Result := '';
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        { Fixed row cells are filled by sequential numbers. }
        if ACol > 0 then
            Result := ''
        else
            Result := IntToStr(ARow - (GetFixedRows - 1));
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
        begin
            CurveParameters := Curve_parameters(Items[ARow - GetFixedRows]);
            with CurveParameters do
                case ACol - GetFixedCols of
                    0: Result :=
                            FloatToStrF(ValuesByName['Intensity'], ffGeneral, 8, 4);
                    1: Result := FloatToStrF(ValuesByName['StartPos'], ffGeneral, 6, 4);
                    2: Result := FloatToStrF(ValuesByName['PeakPos'], ffGeneral, 6, 4);
                    3: Result :=
                            FloatToStrF(ValuesByName['FinishPos'], ffGeneral, 6, 4);
                    4: Result :=
                            FloatToStrF(ValuesByName['IntCorrFactor'], ffGeneral, 6, 4);
                    5: Result := FloatToStrF(ValuesByName['Sigma'], ffGeneral, 6, 4);
                end;
        end
        else
            { If the list is empty then the method must return empty string
              to allow the grid filling empty row for which there isn't real
              data object in the list. }
            case ACol - GetFixedCols of
                0, 1, 2, 3, 5: Result := '0';
                4: Result := '1';
            end{ Information area of the table is filled. };
end;

function TCurveList.IsDataValid(const ACol, ARow: integer;
    const AString: string): boolean;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        try
            StrToFloat(AString)
        except
            Result := False;
            Exit
        end;
    end
    else
        Result := True;
end;

procedure TCurveList.SetValueByDefault(const ACol, ARow: integer);
var
    CurveParameters: Curve_parameters;
begin
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
        begin
            CurveParameters := Curve_parameters(Items[ARow - GetFixedRows]);
            with CurveParameters do
                case ACol - GetFixedCols of
                    0: ValuesByName['Intensity'] := 0;
                    1: ValuesByName['StartPos']  := 0;
                    2: ValuesByName['PeakPos']   := 0;
                    3: ValuesByName['FinishPos'] := 0;
                    4: ValuesByName['IntCorrFactor'] := 1;
                    5: ValuesByName['Sigma']     := 1;
                end;
        end;
end;

procedure TCurveList.StringToValue(const ACol, ARow: integer; const AString: string);
var
    CurveParameters: Curve_parameters;
begin
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Assert(IsDataValid(ACol, ARow, AString));

        CurveParameters := Curve_parameters(Items[ARow - GetFixedRows]);
        with CurveParameters do
            case ACol - GetFixedCols of
                0: ValuesByName['Intensity'] := StrToFloat(AString);
                1: ValuesByName['StartPos']  := StrToFloat(AString);
                2: ValuesByName['PeakPos']   := StrToFloat(AString);
                3: ValuesByName['FinishPos'] := StrToFloat(AString);
                4: ValuesByName['IntCorrFactor'] := StrToFloat(AString);
                5: ValuesByName['Sigma']     := StrToFloat(AString);
            end;
    end;
end;

{$hints off}
function TCurveList.GetCellEnabledCharSet(const ACol, ARow: integer): TCharSet;
begin
    Result := POS_REAL_SET;
end;

{$hints on}

initialization
    RegisterClass(Curve_parameters);
    RegisterClass(TCurveList);
{$warnings off}
    DecimalSeparator := '.';
{$warnings on}
end.
