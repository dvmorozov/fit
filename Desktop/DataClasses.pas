//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit DataClasses;

{$MODE Delphi}

interface

uses
    Classes, SelfCheckedComponentList, Grids, SysUtils, NumericGrid, SimpMath,
    Math3d, Tools, TableComp, DownhillSimplexContainer, SelfCopied,
    ObjSavingStringList, Graphics, DataLoader;

type
    //  nabor ekzemplyarov patternov sozdayuschih raschetnyy profil'
    TSpecimenList = class(TRowCompList)
    protected
        function CreateNewObject: TComponent; override;
        //  bazovaya f-ya dlya perescheta znacheniy parametrov vyvodimyh pol'zovatelyu
        function RecalcParamValue(P: TSpecialCurveParameter): Double; virtual;
        //  bazovaya f-ya dlya obratnogo perescheta znacheniy parametrov vvodimyh pol'zovatelem
        procedure ReverseCalcParamValue(
            P: TSpecialCurveParameter; NewValue: Double); virtual;

    public
        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;
        function GetRowContents(
            Grid: TStringGrid; RowNum: LongInt): Boolean; override;
        procedure SetRowContents(Grid: TStringGrid; RowNum: LongInt); override;

        ////////////////////////////////////////////////////////////////////////
        //  funktsii novogo interfeysa
        ////////////////////////////////////////////////////////////////////////
        function ValueToString(const ACol, ARow: LongInt
            ): string; override;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); override;
        procedure SetValueByDefault(const ACol, ARow: LongInt); override;
            //  ustanavlivaet pravil'noe znachenie "po umolchaniyu"
            //  dlya dannoy yacheyki; ispol'zuetsya pri obrabotke operatsii
            //  ochistki yacheyki
        function IsDataValid(const ACol, ARow: LongInt;
            //  vypolnyaet "myagkuyu" proverku dannyh bez vozbuzhdeniya
            //  isklyucheniya; odnako, esli koordinaty stroki/kolonki
            //  imeyut nedorustimye znacheniya isklyuchenie vyzyvaetsya;
            //  vsegda vozvraschaet True - perekryt' dlya realizatsii
            //  nuzhnyh proverok
            const AString: string): Boolean; override;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; override;

        function GetInfoCols: LongInt; override;
    end;

implementation

procedure TSpecimenList.SetCaption;
var CP: Curve_parameters;
    P: TSpecialCurveParameter;
    i, Index: LongInt;
begin
    Assert(Assigned(Grid));
    Assert(Grid.FixedRows >= 1);
    
    if Count <> 0 then
    begin
        Index := 0;
        //  esli nabor parametrov krivyh pust nel'zya
        //  ustanovit' zagolovki parametrov
        CP := Curve_parameters(Items[0]);
        //  !!! dolzhny obrabatyvat'sya vse parametry, krome argumenta !!!
        Assert(Grid.ColCount - Grid.FixedCols = CP.Params.Count - 1);
        for i := 0 to CP.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(CP.Params.Items[i]);
            if P.Type_ <> Argument then
            begin
                Grid.Cells[Grid.FixedCols + Index, 0] := P.Name;
                Inc(Index);
            end;
        end;
    end;
end;

procedure TSpecimenList.SetColOptions(Grid: TStringGrid);
var CP: Curve_parameters;
    P: TSpecialCurveParameter;
    i, Index: LongInt;
begin
    Assert(Assigned(Grid));
    
    if Grid is TNumericGrid then
        with TNumericGrid(Grid) do
        begin
            if Count <> 0 then
            begin
                CP := Curve_parameters(Items[0]);
                //  !!! dolzhny obrabatyvat'sya vse parametry, krome argumenta !!!
                Assert(Grid.ColCount - Grid.FixedCols = CP.Params.Count - 1);
                Index := 0;
                for i := 0 to CP.Params.Count - 1 do
                begin
                    P := TSpecialCurveParameter(CP.Params.Items[i]);
                    case P.Type_ of
                        Argument: begin end;
                        Calculated: begin
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
end;

procedure TSpecimenList.SetRowContents(Grid: TStringGrid; RowNum: LongInt);
var CP: Curve_parameters;
    P: TSpecialCurveParameter;
    i, Index: LongInt;
begin
    Assert(Assigned(Grid));
    
    with Grid do
        if (RowNum >= FixedRows) and (RowNum - FixedRows < Count) then
        begin
            Cells[0, RowNum] := IntToStr(RowNum);
            
            CP := Curve_parameters(Items[RowNum - FixedRows]);
            //  !!! dolzhny obrabatyvat'sya vse parametry, krome argumenta !!!
            Assert(Grid.ColCount - Grid.FixedCols = CP.Params.Count - 1);
            Index := 0;
            for i := 0 to CP.Params.Count - 1 do
            begin
                P := TSpecialCurveParameter(CP.Params.Items[i]);
                if P.Type_ <> Argument then
                begin
                    Cells[FixedCols + Index, RowNum] :=
                        FloatToStrF(RecalcParamValue(P), ffFixed, 8, 4);
                    Inc(Index);
                end;
            end;
        end
        else
        begin
            //  initsializiruetsya pustaya stroka
            Cells[0, RowNum] := IntToStr(RowNum);
            for i := FixedCols to ColCount - 1 do Cells[i, RowNum] := '';
        end;
end;

function TSpecimenList.GetRowContents;
var CP: Curve_parameters;
    P: TSpecialCurveParameter;
    i, Index: LongInt;
begin
    Result := True;
    with Grid do
    begin
        Assert((RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count));
        
        CP := Curve_parameters(Items[RowNum - FixedRows]);
        //  !!! dolzhny obrabatyvat'sya vse parametry, krome argumenta !!!
        Assert(Grid.ColCount - Grid.FixedCols = CP.Params.Count - 1);
        Index := 0;
        for i := 0 to CP.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(CP.Params.Items[i]);
            if P.Type_ <> Argument then
            begin
                try
                    ReverseCalcParamValue(P,
                        StrToFloat(Cells[FixedCols + Index, RowNum]));
                except
                    //  nefatal'naya oshibka pol'zovatelya,
                    //  vyyavlennaya pri preobrazovanii stroki
                    Result := False;
                end;
                Inc(Index);
            end;
        end;
    end;
end;

function TSpecimenList.CreateNewObject: TComponent;
begin
    Result := Curve_parameters.Create(nil);
end;

function TSpecimenList.RecalcParamValue(P: TSpecialCurveParameter): Double;
begin
    Assert(Assigned(P));
    Result := P.Value;
end;

procedure TSpecimenList.ReverseCalcParamValue(
    P: TSpecialCurveParameter; NewValue: Double);
begin
    Assert(Assigned(P));
    P.Value := NewValue;
end;

function TSpecimenList.GetInfoCols: LongInt;
var CP: Curve_parameters;
    P: TSpecialCurveParameter;
    i: LongInt;
begin
    if Count <> 0 then
    begin
        Result := 0;
        
        CP := Curve_parameters(Items[0]);
        for i := 0 to CP.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(CP.Params.Items[i]);
            if P.Type_ <> Argument then Inc(Result);
        end;
    end
    else Result := GetFixedCols + 1;
end;

function TSpecimenList.ValueToString(const ACol, ARow: Integer): string;
var TN: Curve_parameters;
begin
    Assert(False);    //???

    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        //  zapolnyaetsya verhnyaya chast' tablitsy - zagolovki kolonok
        if ARow > 0 then Result := ''
        else
        begin
        
        end;
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        //  zapolnyayutsya fiksirovannye polya strok - nomera
        if ACol > 0 then Result := ''
        else Result := IntToStr(ARow - (GetFixedRows - 1));
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        //  zapolnyaetsya informatsionnaya chast' tablitsy
        if Count <> 0 then
            //  esli spisok pust f-ya dolzhna vernut' pustuyu stroku,
            //  chtoby obespechit' vozmozhnost' setke zapolnit' pustuyu
            //  stroku, dlya kotoroy real'no net ob'ekta spiska
        begin
            (* ???
            TN := Curve_parameters(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Result := FloatToStrF(Intensity, ffGeneral, 8, 4);
                1 : Result := FloatToStrF(StartPos, ffGeneral, 6, 4);
                2 : Result := FloatToStrF(PeakPos, ffGeneral, 6, 4);
                3 : Result := FloatToStrF(FinishPos, ffGeneral, 6, 4);
                4 : Result := FloatToStrF(IntCorrFactor, ffGeneral, 6, 4);
                5 : Result := FloatToStrF(Sigma, ffGeneral, 6, 4);
            end;
            *)
        end else    //  stroki, ekvivalentnye znacheniyam "po umolchaniyu"
            case ACol - GetFixedCols of
                0, 1, 2, 3, 5 : Result := '0';
                4 : Result := '1';
            end;
    end;
end;

function TSpecimenList.IsDataValid(const ACol, ARow: Integer;
    const AString: string): Boolean;
begin
    Assert(False);  //  ???
    
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        //  case ACol - GetFixedCols of
            //  0, 1, 2, 3, 4 :
                try StrToFloat(AString) except Result := False; Exit end;
        //  end;
    end else Result := True;
end;

procedure TSpecimenList.SetValueByDefault(const ACol, ARow: Integer);
var TN: Curve_parameters;
begin
    Assert(False);  //  ???
    
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
            //  esli spisok pust f-ya ne dolzhna nichego delat',
            //  chtoby obespechit' vozmozhnost' setke ochistit' pustuyu
            //  stroku, dlya kotoroy real'no net ob'ekta spiska
        begin
            TN := Curve_parameters(Items[ARow - GetFixedRows]);
            (* ???
            with TN do case ACol - GetFixedCols of
                0 : Intensity := 0;
                1 : StartPos := 0;
                2 : PeakPos := 0;
                3 : FinishPos := 0;
                4 : IntCorrFactor := 1;
                5 : Sigma := 1;
            end;
            *)
        end;
end;

procedure TSpecimenList.StringToValue(const ACol, ARow: Integer;
    const AString: string);
var TN: Curve_parameters;
begin
    Assert(False);  //  ???
    
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Assert(IsDataValid(ACol, ARow, AString));

        TN := Curve_parameters(Items[ARow - GetFixedRows]);
        (* ???
        with TN do case ACol - GetFixedCols of
            0 : Intensity := StrToFloat(AString);
            1 : StartPos := StrToFloat(AString);
            2 : PeakPos := StrToFloat(AString);
            3 : FinishPos := StrToFloat(AString);
            4 : IntCorrFactor := StrToFloat(AString);
            5 : Sigma := StrToFloat(AString);
        end;
        *)
    end;
end;

function TSpecimenList.GetCellEnabledCharSet(const ACol,
    ARow: Integer): TCharSet;
begin
    Result := POS_REAL_SET;
end;

initialization
    RegisterClass(Curve_parameters);
    RegisterClass(TSpecimenList);
    DecimalSeparator := '.';
end.



