// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Minimal, dependency-free loader for OHLC price CSV files.)

Wave analysis needs real market data, and the app's CSV loader was a stub that
only raised ENotImplemented. This is a small native loader so the pack is usable
standalone and with zero external dependencies; the general pandas-backed import
(Excel, HDF5, JCAMP-DX, real dialect handling) remains Stage 4's job and will
supersede this. csv_file_loader is left untouched as the home for that work -
only the injector's .CSV routing changes, which removes nothing, since that path
previously just threw.

THE ABSCISSA. Storage is the raw argument as loaded (D5), so the loader must
choose a number for x. Two modes:

  xmBarIndex    0, 1, 2, ... - the DEFAULT, and what wave analysis actually
                wants: it removes weekend and holiday gaps, so a five-day week
                is five equal steps rather than a run with holes. Pattern
                durations then mean "bars", which is how counts are read.

  xmDateSerial  TDateTime as a float, for genuinely chronological display. Keeps
                calendar gaps, which is correct for a time axis and wrong for
                measuring wave duration.

Log-price needs nothing here: it is the existing Stage 1 expression axis with
ln(x), applied as a display transform.

BEING EXPLICIT RATHER THAN CLEVER. The loader reports which column it took as
the value and which x mode it used. A loader that silently guesses a column
mapping is a classic source of "the data looks wrong" confusion, and the guess
is invisible in the result.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit ohlc_csv_loader;

{$mode delphi}

interface

uses
    Classes, data_loader, DateUtils, log, Math, SysUtils;

type
    { How a row's x coordinate is derived. See the unit header for why bar index
      is the default. }
    TOhlcXMode = (xmBarIndex, xmDateSerial);

    { What the date column's layout was determined to be.

      Most files are decidable by ARITHMETIC and never need to trouble the user:
      a first component above 12 can only be a day, a second component above 12
      can only be a month. Only a column where every row is <= 12 in both
      positions is genuinely undecidable. }
    TDateLayout = (
        dlNone,           //  no date column, or none needed (bar-index mode)
        dlIso,            //  yyyy-mm-dd - unambiguous, and what feeds emit
        dlDayFirst,       //  dd/mm/yyyy, proved by some row with first > 12
        dlMonthFirst,     //  mm/dd/yyyy, proved by some row with second > 12
        dlAmbiguous,      //  every row <= 12 both ways: genuinely undecidable
        dlContradictory   //  some row proves each - the column is not uniform
        );

    { Which price column becomes y. }
    TOhlcValueColumn = (vcClose, vcOpen, vcHigh, vcLow);

    TOHLCFileLoader = class(TDataLoader)
    private
        FXMode: TOhlcXMode;
        FValueColumn: TOhlcValueColumn;
        { Column indices resolved from the header; -1 when absent. }
        FIdxDate, FIdxOpen, FIdxHigh, FIdxLow, FIdxClose: integer;
        FDelimiter: char;
        FSkipped: integer;
        FDateLayout: TDateLayout;

        function DetectDelimiter(const ALine: string): char;
        procedure Split(const ALine: string; AFields: TStringList);
        { Fills the column indices from a header row. False when the row does not
          look like a header at all. }
        function MapHeader(AFields: TStringList): boolean;
        { Assumes the conventional date,open,high,low,close[,volume] order, used
          only when there is no header. }
        procedure AssumeOhlcvOrder;
        function IndexOfValueColumn: integer;
        function TryParseNumber(const AText: string; out AValue: double): boolean;
        { Splits a date field into its three numeric components. False when the
          field is not three numbers at all. }
        function SplitDateParts(const AText: string;
            out A, B, C: integer; out AIsoOrder: boolean): boolean;
        { Decides the layout of the whole date column, so one verdict covers the
          file rather than each row guessing for itself. }
        function DetectDateLayout(AData: TStrings; AFields: TStringList;
            const AHeaderLine: string; AHasHeader: boolean): TDateLayout;
        function TryParseDate(const AText: string; out AValue: TDateTime): boolean;

    protected
        procedure ParseLines(ALines: TStrings); override;

    public
        constructor Create(AOwner: TComponent); override;

        property XMode: TOhlcXMode read FXMode write FXMode;
        property ValueColumn: TOhlcValueColumn read FValueColumn write FValueColumn;
        { Rows that could not be parsed and were skipped (blank lines, footers,
          partial last bars). Exposed so a caller can report a file that is
          mostly junk instead of silently loading three points from it. }
        property SkippedRows: integer read FSkipped;
    end;

var
    { Default for newly created loaders. The injector builds the loader without
      arguments, so the choice has to live somewhere reachable; a settings entry
      drives it from the UI later. }
    DefaultOhlcXMode: TOhlcXMode = xmBarIndex;
    DefaultOhlcValueColumn: TOhlcValueColumn = vcClose;

implementation

uses
    checks;

constructor TOHLCFileLoader.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FXMode := DefaultOhlcXMode;
    FValueColumn := DefaultOhlcValueColumn;
end;

function TOHLCFileLoader.DetectDelimiter(const ALine: string): char;
var
    Commas, Semis, Tabs: integer;
    i: integer;
begin
    Commas := 0; Semis := 0; Tabs := 0;
    for i := 1 to Length(ALine) do
        case ALine[i] of
            ',':  Inc(Commas);
            ';':  Inc(Semis);
            #9:   Inc(Tabs);
        end;

    //  Semicolon wins ties because a semicolon-delimited file is the European
    //  convention that also uses the comma as a DECIMAL separator - guessing
    //  comma there would split every number in half.
    if (Semis > 0) and (Semis >= Commas) then
        Result := ';'
    else if Tabs > Commas then
        Result := #9
    else
        Result := ',';
end;

procedure TOHLCFileLoader.Split(const ALine: string; AFields: TStringList);
var
    i, Start: integer;
begin
    AFields.Clear;
    Start := 1;
    for i := 1 to Length(ALine) do
        if ALine[i] = FDelimiter then
        begin
            AFields.Add(Trim(Copy(ALine, Start, i - Start)));
            Start := i + 1;
        end;
    AFields.Add(Trim(Copy(ALine, Start, MaxInt)));
end;

function TOHLCFileLoader.MapHeader(AFields: TStringList): boolean;
var
    i: integer;
    Name_: string;
begin
    FIdxDate := -1; FIdxOpen := -1; FIdxHigh := -1; FIdxLow := -1;
    FIdxClose := -1;

    for i := 0 to AFields.Count - 1 do
    begin
        Name_ := LowerCase(AFields[i]);
        //  'adj close' is preferred over 'close' when both are present: it is
        //  the split/dividend-adjusted series, and an unadjusted one has
        //  artificial jumps that would read as wave boundaries.
        if (Name_ = 'adj close') or (Name_ = 'adj_close') or
           (Name_ = 'adjclose') then
            FIdxClose := i
        else if (Name_ = 'close') and (FIdxClose < 0) then
            FIdxClose := i
        else if Name_ = 'open' then FIdxOpen := i
        else if Name_ = 'high' then FIdxHigh := i
        else if Name_ = 'low'  then FIdxLow := i
        else if (Name_ = 'date') or (Name_ = 'time') or (Name_ = 'timestamp')
             or (Name_ = 'datetime') or (Name_ = 'observation_date') then
            FIdxDate := i;
    end;

    //  A TWO-COLUMN series - a date and one value - is how central banks and
    //  statistical releases publish (FRED's "observation_date,DEXUSEU", for
    //  instance). There is no price column to name, but the shape is
    //  unambiguous, so take the second column as the value rather than refusing
    //  a whole class of real data.
    if (FIdxClose < 0) and (FIdxOpen < 0) and (FIdxHigh < 0) and (FIdxLow < 0)
       and (AFields.Count = 2) and (FIdxDate = 0) then
        FIdxClose := 1;

    //  A header is only a header if it named a price column; otherwise this is
    //  a data row and the caller falls back to the conventional order.
    Result := (FIdxClose >= 0) or (FIdxOpen >= 0) or (FIdxHigh >= 0) or
              (FIdxLow >= 0);
end;

procedure TOHLCFileLoader.AssumeOhlcvOrder;
begin
    FIdxDate := 0; FIdxOpen := 1; FIdxHigh := 2; FIdxLow := 3; FIdxClose := 4;
end;

function TOHLCFileLoader.IndexOfValueColumn: integer;
begin
    case FValueColumn of
        vcOpen: Result := FIdxOpen;
        vcHigh: Result := FIdxHigh;
        vcLow:  Result := FIdxLow;
        else    Result := FIdxClose;
    end;
end;

function TOHLCFileLoader.TryParseNumber(const AText: string;
    out AValue: double): boolean;
var
    Fmt: TFormatSettings;
    S: string;
    i: integer;
begin
    AValue := 0;
    S := Trim(AText);
    if S = '' then
    begin
        Result := False;
        Exit;
    end;

    //  A field with no digit at all is not a number, whatever StrToFloat makes
    //  of it. FRED marks a missing observation with a bare '.', which parses as
    //  ZERO with '.' as the decimal separator - and a spurious zero in a price
    //  series is far worse than a skipped row: it looks like a crash to zero and
    //  would drag any fit through it.
    Result := False;
    for i := 1 to Length(S) do
        if (S[i] >= '0') and (S[i] <= '9') then
        begin
            Result := True;
            Break;
        end;
    if not Result then
        Exit;

    //  Try '.' as the decimal separator first, which is what price feeds emit,
    //  then ',' for the European convention. Explicit format settings rather
    //  than the machine's locale, so the same file loads identically anywhere.
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Fmt.ThousandSeparator := #0;
    Result := TryStrToFloat(S, AValue, Fmt);
    if not Result then
    begin
        Fmt.DecimalSeparator := ',';
        Result := TryStrToFloat(S, AValue, Fmt);
    end;
end;

function TOHLCFileLoader.SplitDateParts(const AText: string;
    out A, B, C: integer; out AIsoOrder: boolean): boolean;
var
    S: string;
    i: integer;
    Parts: TStringList;
begin
    Result := False;
    A := 0; B := 0; C := 0; AIsoOrder := False;

    S := Trim(AText);
    if Length(S) < 8 then
        Exit;

    for i := 1 to Length(S) do
        if (S[i] = '/') or (S[i] = '.') then
            S[i] := '-';
    //  Drop any time component.
    i := Pos(' ', S);
    if i > 0 then S := Copy(S, 1, i - 1);
    i := Pos('T', S);
    if i > 0 then S := Copy(S, 1, i - 1);

    Parts := TStringList.Create;
    try
        Parts.Delimiter := '-';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := S;
        if Parts.Count <> 3 then
            Exit;
        try
            A := StrToInt(Parts[0]);
            B := StrToInt(Parts[1]);
            C := StrToInt(Parts[2]);
        except
            Exit;
        end;
        //  A four-digit leading component means ISO, which needs no guessing.
        AIsoOrder := Length(Parts[0]) = 4;
        Result := True;
    finally
        Parts.Free;
    end;
end;

function TOHLCFileLoader.DetectDateLayout(AData: TStrings; AFields: TStringList;
    const AHeaderLine: string; AHasHeader: boolean): TDateLayout;
var
    i, A, B, C: integer;
    Iso, SawDayFirst, SawMonthFirst, SawAny: boolean;
begin
    Result := dlNone;
    if FIdxDate < 0 then
        Exit;

    SawDayFirst := False;
    SawMonthFirst := False;
    SawAny := False;

    for i := 0 to AData.Count - 1 do
    begin
        if AHasHeader and (AData[i] = AHeaderLine) then Continue;
        if Trim(AData[i]) = '' then Continue;

        Split(AData[i], AFields);
        if AFields.Count <= FIdxDate then Continue;
        if not SplitDateParts(AFields[FIdxDate], A, B, C, Iso) then Continue;

        SawAny := True;
        if Iso then
        begin
            //  One ISO row settles it: the column is ISO throughout.
            Result := dlIso;
            Exit;
        end;

        //  Arithmetic, not guesswork: >12 in a position can only be a day.
        if A > 12 then SawDayFirst := True;
        if B > 12 then SawMonthFirst := True;
    end;

    if not SawAny then
        Result := dlNone
    else if SawDayFirst and SawMonthFirst then
        Result := dlContradictory
    else if SawDayFirst then
        Result := dlDayFirst
    else if SawMonthFirst then
        Result := dlMonthFirst
    else
        //  Every row is <= 12 both ways. Nothing in the file can decide this.
        Result := dlAmbiguous;
end;

function TOHLCFileLoader.TryParseDate(const AText: string;
    out AValue: TDateTime): boolean;
var
    A, B, C: integer;
    Iso: boolean;
    Y, M, D: word;
begin
    AValue := 0;
    Result := False;
    if not SplitDateParts(AText, A, B, C, Iso) then
        Exit;

    if Iso then
    begin
        Y := A; M := B; D := C;
    end
    else
        case FDateLayout of
            dlDayFirst:   begin D := A; M := B; Y := C; end;
            dlMonthFirst: begin M := A; D := B; Y := C; end;
            else
                //  Never guess. LoadDataSetActually refuses the file before
                //  reaching here when the layout could not be determined.
                Exit;
        end;

    Result := TryEncodeDate(Y, M, D, AValue);
end;

procedure TOHLCFileLoader.ParseLines(ALines: TStrings);
var
    Fields: TStringList;
    i, ValueIdx, BarIndex: integer;
    Line: string;
    Y, X: double;
    Stamp: TDateTime;
    HasHeader: boolean;
begin
    CheckAssigned(ALines, 'the lines of the CSV file being parsed');
    CheckAssigned(FPointsSet, 'the points set the CSV file is parsed into');

    FPointsSet.Clear;
    FSkipped := 0;

    Fields := TStringList.Create;
    try
        if ALines.Count = 0 then
            raise EInvalidFileFormat.Create('The CSV file is empty.');

        //  Delimiter from the first non-blank line - the header when there is
        //  one, which is the most delimiter-rich line in the file.
        Line := '';
        for i := 0 to ALines.Count - 1 do
            if Trim(ALines[i]) <> '' then
            begin
                Line := ALines[i];
                Break;
            end;
        FDelimiter := DetectDelimiter(Line);

        Split(Line, Fields);
        HasHeader := MapHeader(Fields);
        if not HasHeader then
        begin
            if Fields.Count < 5 then
                raise EInvalidFileFormat.Create(
                    'This CSV has no recognisable header and fewer than five ' +
                    'columns, so its price column cannot be identified. Add a ' +
                    'header row naming at least a "close" column.');
            AssumeOhlcvOrder;
        end;

        //  Decide the date column's layout ONCE for the whole file, and refuse
        //  outright rather than guess. Only needed when x comes from dates.
        FDateLayout := dlNone;
        if FXMode = xmDateSerial then
        begin
            FDateLayout := DetectDateLayout(ALines, Fields, Line, HasHeader);

            if FDateLayout = dlAmbiguous then
                //  BLOCK, with the reason and the two ways out. Guessing would
                //  silently reorder the series - e.g. 01/02 and 02/01 swap - and
                //  a wrong x axis is not visibly wrong, it just makes every
                //  subsequent wave count nonsense.
                raise EInvalidFileFormat.Create(
                    'The dates in this file are ambiguous: every value fits ' +
                    'both day/month/year and month/day/year, so their order ' +
                    'cannot be determined from the file itself. Loading was ' +
                    'stopped rather than risk reordering the series. Either ' +
                    'convert the date column to ISO format (yyyy-mm-dd), or ' +
                    'load with the bar-index abscissa, which does not use the ' +
                    'dates at all.');

            if FDateLayout = dlContradictory then
                raise EInvalidFileFormat.Create(
                    'The date column is not consistent: some rows can only be ' +
                    'day/month/year and others only month/day/year. Fix the ' +
                    'file, or load with the bar-index abscissa, which does not ' +
                    'use the dates.');

            if FDateLayout = dlNone then
                raise EInvalidFileFormat.Create(
                    'The chronological abscissa was requested but this file ' +
                    'has no usable date column. Load with the bar-index ' +
                    'abscissa instead.');
        end;

        ValueIdx := IndexOfValueColumn;
        if ValueIdx < 0 then
            raise EInvalidFileFormat.Create(
                'The requested price column is not present in this CSV.');

        //  Uses the plain log unit, not a module's: this is a general data
        //  loader that a wave pack happens to need, not part of one -
        //  coupling it to the pack would also drag the pack into every project
        //  that merely loads a file.
        WriteLog(Format(
            'OHLC load: delimiter "%s", %s, value column index %d, x mode %s',
            [FDelimiter,
             BoolToStr(HasHeader, 'header row', 'no header (assumed OHLCV order)'),
             ValueIdx,
             BoolToStr(FXMode = xmBarIndex, 'bar index', 'date serial')]),
            log.Notification);   //  qualified: TComponent.Notification shadows it

        BarIndex := 0;
        for i := 0 to ALines.Count - 1 do
        begin
            //  Skip the header row itself.
            if HasHeader and (ALines[i] = Line) then
                Continue;

            if Trim(ALines[i]) = '' then
                Continue;

            Split(ALines[i], Fields);
            if Fields.Count <= ValueIdx then
            begin
                //  Short rows are normal at the end of a feed export; skipping
                //  beats aborting a load of thousands of good bars.
                Inc(FSkipped);
                Continue;
            end;

            if not TryParseNumber(Fields[ValueIdx], Y) then
            begin
                Inc(FSkipped);
                Continue;
            end;

            if FXMode = xmDateSerial then
            begin
                if (FIdxDate < 0) or (Fields.Count <= FIdxDate) or
                   (not TryParseDate(Fields[FIdxDate], Stamp)) then
                begin
                    Inc(FSkipped);
                    Continue;
                end;
                X := Stamp;
            end
            else
                X := BarIndex;

            FPointsSet.AddNewPoint(X, Y);
            Inc(BarIndex);
        end;

        if FPointsSet.PointsCount = 0 then
            raise EInvalidFileFormat.CreateFmt(
                'No usable rows were found in this CSV (%d skipped).',
                [FSkipped]);

        if FSkipped > 0 then
            WriteLog(Format(
                'OHLC load: %d row(s) skipped, %d point(s) loaded',
                [FSkipped, FPointsSet.PointsCount]), log.Warning);
    finally
        Fields.Free;
    end;
end;

end.
