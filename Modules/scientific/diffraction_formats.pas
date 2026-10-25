// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The files diffractometers and refinement programs write a powder
pattern in: PANalytical .xrdml, Bruker .uxd, GSAS .gsa/.fxye and a powder CIF.)

WHY THESE, AND WHY HERE. They are the formats a powder pattern is published in
beside plain text - found in Zenodo's datasets (2026-10-20: records holding
.xrdml, .uxd, .xye, .chi and .cif files) - and they are a FIELD'S formats, so
they are this module's: a build without the diffraction shapes has no reason
to read a diffractometer's file, and the framework names no field.

EACH READS THE PATTERN AND NOTHING ELSE: the argument (2 Theta, in degrees) and
the intensity. Wavelengths, slits, sample names and esds are not kept; the
explanation of each format says so.

WHAT IS LEFT OUT, AND WHY:
  * Bruker .raw and .brml, Rigaku .raw: binary or zipped formats whose
    layouts are not published, or vary by version - a reader written without
    the specification reads the wrong numbers silently. A Rigaku file named
    .raw is what a search for .raw actually turned up (Zenodo 14246011).
  * a GSAS file binned other than CONST (time of flight, log bins): its
    argument is not an angle.

EVERY RULE IS A FUNCTION OVER TEXT, reachable from a test with string literals;
the loaders only read the file and hand its text over.
}
unit diffraction_formats;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_loader;

type
    { One pattern: an argument and an intensity per point. }
    TPattern = record
        X: array of double;
        Y: array of double;
    end;

    { A reader of one of these formats: the pattern it finds becomes the
      profile, a repeated angle keeping its first point as the two-column
      reader does. }
    TPatternLoader = class(TDataLoader)
    protected
        function Pattern(ALines: TStrings): TPattern; virtual; abstract;
        procedure ParseLines(ALines: TStrings); override;
    end;

    { PANalytical's XML: the first scan's 2 Theta and intensities. }
    TXrdmlLoader = class(TPatternLoader)
    protected
        function Pattern(ALines: TStrings): TPattern; override;
    end;

    { Bruker's text export. }
    TUxdLoader = class(TPatternLoader)
    protected
        function Pattern(ALines: TStrings): TPattern; override;
    end;

    { GSAS's powder data file, constant-width bins. }
    TGsasLoader = class(TPatternLoader)
    protected
        function Pattern(ALines: TStrings): TPattern; override;
    end;

    { A CIF holding a powder pattern (pdCIF). }
    TPowderCifLoader = class(TPatternLoader)
    protected
        function Pattern(ALines: TStrings): TPattern; override;
    end;

    EPatternFormat = class(EInvalidFileFormat);

{ The pattern in an .xrdml document. }
function XrdmlPattern(const AText: string): TPattern;

{ The pattern in a .uxd file's lines. }
function UxdPattern(ALines: TStrings): TPattern;

{ The pattern in a GSAS file's lines: its first bank. }
function GsasPattern(ALines: TStrings): TPattern;

{ The pattern in a CIF's lines. }
function PowderCifPattern(ALines: TStrings): TPattern;

{ The value of a CIF number: '1234(56)' is 1234, '?' and '.' are none. }
function TryCifNumber(const AToken: string; out AValue: double): boolean;

{ Every whitespace-separated token of a CIF's lines, quoted strings and
  semicolon-delimited text fields each one token, comments dropped. }
function CifTokens(ALines: TStrings): TStringList;

implementation

uses
    DOM, XMLRead, StrUtils, Math;

function Settings: TFormatSettings;
begin
    Result := DefaultFormatSettings;
    Result.DecimalSeparator := '.';
    Result.ThousandSeparator := #0;
end;

function TryNumber(const AText: string; out AValue: double): boolean;
begin
    Result := TryStrToFloat(Trim(AText), AValue, Settings);
end;

procedure Add(var APattern: TPattern; AX, AY: double);
begin
    SetLength(APattern.X, Length(APattern.X) + 1);
    SetLength(APattern.Y, Length(APattern.Y) + 1);
    APattern.X[High(APattern.X)] := AX;
    APattern.Y[High(APattern.Y)] := AY;
end;

{ Points spread evenly from AFrom to ATo, one per value. }
procedure Evenly(var APattern: TPattern; AFrom, ATo: double;
    const AValues: array of double);
var
    i: longint;
    Step: double;
begin
    if Length(AValues) = 0 then
        Exit;
    Step := 0;
    if Length(AValues) > 1 then
        Step := (ATo - AFrom) / (Length(AValues) - 1);
    for i := 0 to High(AValues) do
        Add(APattern, AFrom + i * Step, AValues[i]);
end;

{ Points from AFrom by AStep. }
procedure Stepped(var APattern: TPattern; AFrom, AStep: double;
    const AValues: array of double);
var
    i: longint;
begin
    for i := 0 to High(AValues) do
        Add(APattern, AFrom + i * AStep, AValues[i]);
end;

{ The numbers of a whitespace-separated text. }
function Numbers(const AText: string): TStringArray;
begin
    Result := AText.Split([' ', #9, #10, #13], TStringSplitOptions.ExcludeEmpty);
end;

function NumbersOf(const AText: string): specialize TArray<double>;
var
    Parts: TStringArray;
    i: longint;
    V: double;
begin
    Result := nil;
    Parts := Numbers(AText);
    SetLength(Result, Length(Parts));
    for i := 0 to High(Parts) do
    begin
        if not TryNumber(Parts[i], V) then
            raise EPatternFormat.Create('"' + Parts[i] + '" is not a number.');
        Result[i] := V;
    end;
end;

{ --------------------------------- xrdml ------------------------------------- }

function LocalName(ANode: TDOMNode): string;
var
    P: longint;
begin
    Result := ANode.NodeName;
    P := Pos(':', Result);
    if P > 0 then
        Result := Copy(Result, P + 1, MaxInt);
end;

function FirstNamed(ANode: TDOMNode; const AName: string): TDOMNode;
var
    Child: TDOMNode;
begin
    Result := nil;
    if ANode = nil then
        Exit;
    Child := ANode.FirstChild;
    while Child <> nil do
    begin
        if (Child.NodeType = ELEMENT_NODE) and (LocalName(Child) = AName) then
            Exit(Child);
        Result := FirstNamed(Child, AName);
        if Result <> nil then
            Exit;
        Child := Child.NextSibling;
    end;
end;

function ChildNamed(ANode: TDOMNode; const AName: string): TDOMNode;
begin
    Result := nil;
    if ANode = nil then
        Exit;
    Result := ANode.FirstChild;
    while (Result <> nil) and not ((Result.NodeType = ELEMENT_NODE) and
        (LocalName(Result) = AName)) do
        Result := Result.NextSibling;
end;

function Attribute(ANode: TDOMNode; const AName: string): string;
begin
    Result := '';
    if (ANode <> nil) and (ANode is TDOMElement) then
        Result := TDOMElement(ANode).GetAttribute(AName);
end;

function XrdmlPattern(const AText: string): TPattern;
var
    Doc: TXMLDocument;
    Stream: TStringStream;
    Points, Node, Positions, Values: TDOMNode;
    Axis, Wanted: string;
    Counts, Listed: specialize TArray<double>;
    From, To_: double;
    i: longint;
begin
    Result := Default(TPattern);
    Stream := TStringStream.Create(AText);
    try
        try
            ReadXMLFile(Doc, Stream);
        except
            on E: Exception do
                raise EPatternFormat.Create('The .xrdml file is not well-formed ' +
                    'XML (' + E.Message + '), so nothing could be read from it.');
        end;
    finally
        Stream.Free;
    end;
    try
        //  THE FIRST SCAN: a measurement of several scans is several patterns,
        //  and joining them would draw one that runs backwards.
        Points := FirstNamed(Doc.DocumentElement, 'dataPoints');
        if Points = nil then
            raise EPatternFormat.Create('The .xrdml file holds no scan data ' +
                '(no dataPoints).');
        //  THE ANGLE, whatever axis the goniometer scanned: an Omega-2Theta
        //  scan lists Omega too, and Omega is not what is plotted.
        Wanted := '2Theta';
        Positions := nil;
        Node := Points.FirstChild;
        while Node <> nil do
        begin
            if (Node.NodeType = ELEMENT_NODE) and (LocalName(Node) = 'positions') then
            begin
                Axis := Attribute(Node, 'axis');
                if Axis = Wanted then
                begin
                    Positions := Node;
                    Break;
                end;
            end;
            Node := Node.NextSibling;
        end;
        if Positions = nil then
            raise EPatternFormat.Create('The .xrdml file''s scan has no 2Theta ' +
                'positions, so its argument is not a diffraction angle.');
        Values := ChildNamed(Points, 'intensities');
        if Values = nil then
            Values := ChildNamed(Points, 'counts');
        if Values = nil then
            raise EPatternFormat.Create('The .xrdml file''s scan holds no ' +
                'intensities.');
        Counts := NumbersOf(Values.TextContent);
        if ChildNamed(Positions, 'listPositions') <> nil then
        begin
            Listed := NumbersOf(ChildNamed(Positions, 'listPositions').TextContent);
            if Length(Listed) <> Length(Counts) then
                raise EPatternFormat.Create(Format('The .xrdml file lists %d ' +
                    'positions and %d intensities.', [Length(Listed),
                    Length(Counts)]));
            for i := 0 to High(Counts) do
                Add(Result, Listed[i], Counts[i]);
        end
        else
        begin
            if not TryNumber(ChildNamed(Positions, 'startPosition').TextContent,
                From) or not TryNumber(
                ChildNamed(Positions, 'endPosition').TextContent, To_) then
                raise EPatternFormat.Create('The .xrdml file''s 2Theta has no ' +
                    'start and end position.');
            Evenly(Result, From, To_, Counts);
        end;
    finally
        Doc.Free;
    end;
end;

{ ---------------------------------- uxd -------------------------------------- }

function UxdPattern(ALines: TStrings): TPattern;
var
    i: longint;
    Line, Key, Value: string;
    P: longint;
    Start, Step, V, Y: double;
    HasStart, HasStep, Pairs: boolean;
    Counts: array of double;
    Parts: TStringArray;
    j: longint;
begin
    Result := Default(TPattern);
    HasStart := False;
    HasStep := False;
    Pairs := False;
    Start := 0;
    Step := 0;
    Counts := nil;
    for i := 0 to ALines.Count - 1 do
    begin
        Line := Trim(ALines[i]);
        //  ';' starts a comment.
        if (Line = '') or (Line[1] = ';') then
            Continue;
        if Line[1] = '_' then
        begin
            P := Pos('=', Line);
            if P > 0 then
            begin
                Key := UpperCase(Trim(Copy(Line, 1, P - 1)));
                Value := Trim(Copy(Line, P + 1, MaxInt));
                if ((Key = '_START') or (Key = '_2THETA')) and TryNumber(Value, V) then
                begin
                    //  THE FIRST RANGE ONLY: a second range is a second pattern.
                    if HasStart and ((Length(Counts) > 0) or
                        (Length(Result.X) > 0)) then
                        Break;
                    Start := V;
                    HasStart := True;
                end
                else if (Key = '_STEPSIZE') and TryNumber(Value, V) then
                begin
                    Step := V;
                    HasStep := True;
                end;
            end
            else if (UpperCase(Line) = '_2THETACOUNTS') or
                (UpperCase(Line) = '_2THETACPS') then
                Pairs := True;
            Continue;
        end;
        Parts := Numbers(Line);
        if Pairs then
        begin
            if (Length(Parts) >= 2) and TryNumber(Parts[0], V) and
                TryNumber(Parts[1], Y) then
                Add(Result, V, Y);
        end
        else
            for j := 0 to High(Parts) do
                if TryNumber(Parts[j], V) then
                begin
                    SetLength(Counts, Length(Counts) + 1);
                    Counts[High(Counts)] := V;
                end;
    end;
    if Pairs then
        Exit;
    if Length(Counts) = 0 then
        Exit;
    if not (HasStart and HasStep) then
        raise EPatternFormat.Create('The .uxd file lists counts without the ' +
            '_START (or _2THETA) and _STEPSIZE they are measured at.');
    Stepped(Result, Start, Step, Counts);
end;

{ ---------------------------------- gsas ------------------------------------- }

function GsasPattern(ALines: TStrings): TPattern;
var
    i, j, Bank, Channels: longint;
    Line, Kind, Bin, Text: string;
    Parts: TStringArray;
    Start, Step, V: double;
    Values: array of double;
begin
    Result := Default(TPattern);
    Bank := -1;
    for i := 0 to ALines.Count - 1 do
        if StartsText('BANK', TrimLeft(ALines[i])) then
        begin
            Bank := i;
            Break;
        end;
    if Bank < 0 then
        raise EPatternFormat.Create('The GSAS file has no BANK line, so where ' +
            'its data starts is not known.');
    //  BANK n nchan nrec BINTYP bcoef1 bcoef2 bcoef3 bcoef4 TYPE
    Parts := Numbers(ALines[Bank]);
    if Length(Parts) < 5 then
        raise EPatternFormat.Create('The GSAS BANK line is too short: ' +
            Trim(ALines[Bank]));
    Channels := StrToIntDef(Parts[2], 0);
    Bin := UpperCase(Parts[4]);
    Kind := UpperCase(Parts[High(Parts)]);
    if not (Kind = 'FXYE') and not ((Kind = 'STD') or (Kind = 'ESD')) then
    begin
        //  A bank line without a type ends at its coefficients: STD is the
        //  default.
        if TryNumber(Kind, V) then
            Kind := 'STD'
        else
            raise EPatternFormat.Create('The GSAS file''s data are of type ' +
                Kind + ', which this reader does not read (STD, ESD and FXYE ' +
                'are read).');
    end;
    if Kind = 'FXYE' then
    begin
        //  x in centidegrees, y, esd - one point per line.
        for i := Bank + 1 to ALines.Count - 1 do
        begin
            Parts := Numbers(ALines[i]);
            if (Length(Parts) >= 2) and TryNumber(Parts[0], Start) and
                TryNumber(Parts[1], V) then
                Add(Result, Start / 100, V)
            else if StartsText('BANK', TrimLeft(ALines[i])) then
                Break;
        end;
        Exit;
    end;
    if Bin <> 'CONST' then
        raise EPatternFormat.Create('The GSAS file is binned ' + Bin + ', not ' +
            'CONST: its argument is not a diffraction angle in even steps.');
    if (Length(Parts) < 7) or not TryNumber(Parts[5], Start) or
        not TryNumber(Parts[6], Step) then
        raise EPatternFormat.Create('The GSAS BANK line gives no start and step.');
    //  FIXED-WIDTH FIELDS: STD is ten of eight characters, a two-digit counter
    //  count then the intensity; ESD is five pairs of intensity and esd.
    Values := nil;
    for i := Bank + 1 to ALines.Count - 1 do
    begin
        Line := ALines[i];
        if StartsText('BANK', TrimLeft(Line)) then
            Break;
        for j := 0 to 9 do
        begin
            if j * 8 >= Length(Line) then
                Break;
            //  An esd is not an intensity.
            if (Kind = 'ESD') and Odd(j) then
                Continue;
            if Kind = 'STD' then
                Text := Copy(Line, j * 8 + 3, 6)
            else
                Text := Copy(Line, j * 8 + 1, 8);
            if not TryNumber(Text, V) then
                Continue;
            SetLength(Values, Length(Values) + 1);
            Values[High(Values)] := V;
            if (Channels > 0) and (Length(Values) >= Channels) then
                Break;
        end;
        if (Channels > 0) and (Length(Values) >= Channels) then
            Break;
    end;
    Stepped(Result, Start / 100, Step / 100, Values);
end;

{ ----------------------------------- cif ------------------------------------- }

function TryCifNumber(const AToken: string; out AValue: double): boolean;
var
    Text: string;
    P: longint;
begin
    AValue := NaN;
    Text := Trim(AToken);
    if (Text = '') or (Text = '?') or (Text = '.') then
        Exit(False);
    P := Pos('(', Text);
    if P > 0 then
        Text := Copy(Text, 1, P - 1);
    Result := TryNumber(Text, AValue);
end;

function CifTokens(ALines: TStrings): TStringList;
var
    i, j, Len: longint;
    Line, Token, Field: string;
    Quote: char;
begin
    Result := TStringList.Create;
    i := 0;
    while i < ALines.Count do
    begin
        Line := ALines[i];
        //  A TEXT FIELD: from a line starting with ';' to the next such line.
        if (Line <> '') and (Line[1] = ';') then
        begin
            Field := Copy(Line, 2, MaxInt);
            Inc(i);
            while (i < ALines.Count) and not ((ALines[i] <> '') and
                (ALines[i][1] = ';')) do
            begin
                Field := Field + LineEnding + ALines[i];
                Inc(i);
            end;
            Result.Add(Field);
            Inc(i);
            Continue;
        end;
        j := 1;
        Len := Length(Line);
        while j <= Len do
        begin
            if Line[j] in [' ', #9] then
            begin
                Inc(j);
                Continue;
            end;
            if Line[j] = '#' then
                Break;
            if Line[j] in ['''', '"'] then
            begin
                Quote := Line[j];
                Inc(j);
                Token := '';
                while (j <= Len) and not ((Line[j] = Quote) and
                    ((j = Len) or (Line[j + 1] in [' ', #9]))) do
                begin
                    Token := Token + Line[j];
                    Inc(j);
                end;
                Inc(j);
                Result.Add(Token);
                Continue;
            end;
            Token := '';
            while (j <= Len) and not (Line[j] in [' ', #9]) do
            begin
                Token := Token + Line[j];
                Inc(j);
            end;
            Result.Add(Token);
        end;
        Inc(i);
    end;
end;

const
    { Where a pattern's argument is, in the order preferred: the angle as it
      was corrected for, then as measured. }
    CifArgumentTags: array[0..2] of string = ('_pd_proc_2theta_corrected',
        '_pd_meas_2theta_scan', '_pd_meas_angle_2theta');
    { Its intensity: processed before measured, totals before nets. }
    CifValueTags: array[0..5] of string = ('_pd_proc_intensity_total',
        '_pd_meas_intensity_total', '_pd_meas_counts_total',
        '_pd_proc_intensity_net', '_pd_meas_counts_net',
        '_pd_calc_intensity_total');

function IsTag(const AToken: string): boolean;
begin
    Result := (AToken <> '') and (AToken[1] = '_');
end;

function IsReserved(const AToken: string): boolean;
begin
    Result := SameText(AToken, 'loop_') or StartsText('data_', AToken) or
        StartsText('save_', AToken) or SameText(AToken, 'stop_') or
        SameText(AToken, 'global_');
end;

function PowderCifPattern(ALines: TStrings): TPattern;
var
    Tokens, Items, Tags, Angles: TStringList;
    i, Width, Row, XColumn, YColumn, k: longint;
    X, Y, From, Step: double;
    Values: array of double;
begin
    Result := Default(TPattern);
    Values := nil;
    Tokens := CifTokens(ALines);
    Items := TStringList.Create;
    Tags := TStringList.Create;
    Angles := TStringList.Create;
    try
        i := 0;
        while i < Tokens.Count do
        begin
            if SameText(Tokens[i], 'loop_') then
            begin
                //  A LOOP: its tags, then its values row by row.
                Tags.Clear;
                Inc(i);
                while (i < Tokens.Count) and IsTag(Tokens[i]) do
                begin
                    Tags.Add(LowerCase(Tokens[i]));
                    Inc(i);
                end;
                Width := Max(Tags.Count, 1);
                XColumn := -1;
                YColumn := -1;
                for k := 0 to High(CifArgumentTags) do
                    if (XColumn < 0) and (Tags.IndexOf(CifArgumentTags[k]) >= 0) then
                        XColumn := Tags.IndexOf(CifArgumentTags[k]);
                for k := 0 to High(CifValueTags) do
                    if (YColumn < 0) and (Tags.IndexOf(CifValueTags[k]) >= 0) then
                        YColumn := Tags.IndexOf(CifValueTags[k]);
                Row := 0;
                while (i < Tokens.Count) and not IsTag(Tokens[i]) and
                    not IsReserved(Tokens[i]) do
                begin
                    if (YColumn >= 0) and (Row mod Width = YColumn) then
                    begin
                        if not TryCifNumber(Tokens[i], Y) then
                            Y := NaN;
                        SetLength(Values, Length(Values) + 1);
                        Values[High(Values)] := Y;
                    end;
                    if (YColumn >= 0) and (XColumn >= 0) and
                        (Row mod Width = XColumn) then
                        Angles.Add(Tokens[i]);
                    Inc(Row);
                    Inc(i);
                end;
                //  THE FIRST PATTERN ONLY: a later loop of intensities is
                //  another one - a calculated pattern, another bank.
                if Length(Values) > 0 then
                    Break;
                Continue;
            end;
            //  A single item: _tag value.
            if IsTag(Tokens[i]) and (i + 1 < Tokens.Count) then
            begin
                Items.Values[LowerCase(Tokens[i])] := Tokens[i + 1];
                Inc(i, 2);
                Continue;
            end;
            Inc(i);
        end;

        if Angles.Count > 0 then
        begin
            for k := 0 to Min(High(Values), Angles.Count - 1) do
                if not IsNan(Values[k]) and TryCifNumber(Angles[k], X) then
                    Add(Result, X, Values[k]);
        end
        else if Length(Values) > 0 then
        begin
            //  COUNTS WITHOUT ANGLES: the range they were measured over gives
            //  them - its items may come after the loop, so they are read now.
            while i < Tokens.Count do
            begin
                if IsTag(Tokens[i]) and (i + 1 < Tokens.Count) then
                    Items.Values[LowerCase(Tokens[i])] := Tokens[i + 1];
                Inc(i);
            end;
            if not (TryCifNumber(Items.Values['_pd_meas_2theta_range_min'], From) or
                TryCifNumber(Items.Values['_pd_proc_2theta_range_min'], From)) or
                not (TryCifNumber(Items.Values['_pd_meas_2theta_range_inc'], Step) or
                TryCifNumber(Items.Values['_pd_proc_2theta_range_inc'], Step)) then
                raise EPatternFormat.Create('The CIF lists intensities with no ' +
                    'angles: neither a 2theta column nor a 2theta range.');
            for k := 0 to High(Values) do
                if not IsNan(Values[k]) then
                    Add(Result, From + k * Step, Values[k]);
        end;
    finally
        Angles.Free;
        Tags.Free;
        Items.Free;
        Tokens.Free;
    end;
    if Length(Result.X) = 0 then
        raise EPatternFormat.Create('The CIF holds no powder pattern: no loop ' +
            'of _pd_ intensities. It may describe a crystal structure rather ' +
            'than a measurement.');
end;

{ --------------------------------- loaders ----------------------------------- }

procedure TPatternLoader.ParseLines(ALines: TStrings);
var
    Found: TPattern;
    i: longint;
begin
    FPointsSet.Clear;
    Found := Pattern(ALines);
    for i := 0 to High(Found.X) do
        if FPointsSet.IndexOfValueX(Found.X[i]) = -1 then
            FPointsSet.AddNewPoint(Found.X[i], Found.Y[i]);
end;

function TXrdmlLoader.Pattern(ALines: TStrings): TPattern;
begin
    Result := XrdmlPattern(ALines.Text);
end;

function TUxdLoader.Pattern(ALines: TStrings): TPattern;
begin
    Result := UxdPattern(ALines);
end;

function TGsasLoader.Pattern(ALines: TStrings): TPattern;
begin
    Result := GsasPattern(ALines);
end;

function TPowderCifLoader.Pattern(ALines: TStrings): TPattern;
begin
    Result := PowderCifPattern(ALines);
end;

end.
