// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reading a JCAMP-DX spectrum as an ordinary profile.)

WHY A MODULE OWNS THIS FORMAT. JCAMP-DX is a spectroscopy interchange format,
and a field's formats belong to the module that covers that field - the
framework claims none. It arrives through the same registry every loader does,
so a JCAMP file opened from the File menu, one downloaded from a data source and
one already on disk all take one path.

WHAT IS READ AND WHAT IS NOT. A spectrum is X against Y, which is what this
program fits, so:

  ##XYDATA=(X++(Y..Y))   the ordinary form: each line begins with its X and
                         carries the Y values that follow it;
  ##XYPOINTS=(XY..XY)    and ##PEAK TABLE=(XY..XY), where each pair is written
                         out - a mass spectrum, usually.

A LINK BLOCK holding several spectra (##NTUPLES, ##BLOCKS) reads only its first
spectrum, and says so rather than silently concatenating them into one curve
whose x values go backwards in the middle.

THE SCALING IS THE PART THAT GOES WRONG QUIETLY. Y values are integers scaled by
##YFACTOR, and X by ##XFACTOR, with the step given by ##DELTAX or derived from
##FIRSTX, ##LASTX and ##NPOINTS. Ignoring the factors gives a spectrum of the
right shape and the wrong magnitude - a plausible picture, fitted to nonsense -
so a file that gives neither a step nor the three values it can be derived from
is refused in words instead.
}
unit jcamp_dx_loader;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_loader;

type
    { What a file's header said. Public so the header rules can be tested
      without a spectrum behind them. }
    TJcampHeader = record
        Title: string;
        DataType: string;
        XUnits: string;
        YUnits: string;
        XFactor: double;
        YFactor: double;
        DeltaX: double;
        FirstX: double;
        LastX: double;
        NPoints: longint;
        { Which form the data is written in, as the ##XYDATA line said. }
        IsXYPairs: boolean;
        { Whether a data section was seen at all. }
        HasData: boolean;
    end;

    TJcampDxLoader = class(TDataLoader)
    protected
        procedure ParseLines(ALines: TStrings); override;
    end;

const
    JcampExtensions = '.JDX;.DX;.JCAMP';
    JcampFormatName = 'JCAMP-DX spectrum';

{ A header line split into its label and its value, or False when the line is
  not one. The label is normalised the way the standard says to compare them:
  case, spaces and hyphens are not significant, so '##DATA TYPE' and
  '##DATATYPE' are one label. }
function JcampLabel(const ALine: string; out ALabel, AValue: string): boolean;

{ What this header means for the X step, and why it cannot be decided when it
  cannot. AReason is empty exactly when the step is usable. }
function JcampStep(const AHeader: TJcampHeader; out AStep: double;
    out AReason: string): boolean;

{ Whether the file says the spectrum could not be delivered at all. A service
  that answers "not found" with a well-formed empty file is the quiet failure
  this catches - it would otherwise read as a spectrum of no points. }
function JcampIsMissingSpectrum(const AHeader: TJcampHeader): boolean;

implementation

uses
    checks, jcamp_asdf;

function JcampLabel(const ALine: string; out ALabel, AValue: string): boolean;
var
    Text, Raw: string;
    i, Stop: longint;
begin
    ALabel := '';
    AValue := '';
    Text := Trim(ALine);
    Result := Copy(Text, 1, 2) = '##';
    if not Result then
        Exit;
    Delete(Text, 1, 2);
    Stop := Pos('=', Text);
    if Stop = 0 then
    begin
        Result := False;
        Exit;
    end;
    Raw := Copy(Text, 1, Stop - 1);
    AValue := Trim(Copy(Text, Stop + 1, MaxInt));
    for i := 1 to Length(Raw) do
        if not (Raw[i] in [' ', '-', '/', '_']) then
            ALabel := ALabel + UpCase(Raw[i]);
end;

function JcampIsMissingSpectrum(const AHeader: TJcampHeader): boolean;
begin
    //  What NIST answers for a spectrum it does not hold: a title saying so and
    //  nothing else. Without this the wizard would show a file with no points
    //  and blame the reader.
    Result := (not AHeader.HasData) and
        (Pos('not found', LowerCase(AHeader.Title)) > 0);
end;

function JcampStep(const AHeader: TJcampHeader; out AStep: double;
    out AReason: string): boolean;
begin
    AStep := 0;
    AReason := '';
    if AHeader.DeltaX <> 0 then
        AStep := AHeader.DeltaX
    else if (AHeader.NPoints > 1) and (AHeader.LastX <> AHeader.FirstX) then
        //  Derived, as the standard says it may be: the step is the span over
        //  the gaps between points, which is one fewer than the points.
        AStep := (AHeader.LastX - AHeader.FirstX) / (AHeader.NPoints - 1)
    else
    begin
        AReason := 'This JCAMP-DX file gives neither ##DELTAX nor the ' +
            '##FIRSTX, ##LASTX and ##NPOINTS a step could be worked out from, ' +
            'so where its points sit on the x axis is not stated.';
        Exit(False);
    end;
    Result := True;
end;

procedure TJcampDxLoader.ParseLines(ALines: TStrings);
var
    Header: TJcampHeader;
    Label_, Value, Line: string;
    i, k, Blocks: longint;
    InData: boolean;
    Values: TJcampValues;
    EndedInDifference, PreviousEndedInDifference: boolean;
    Step, X, Y: double;
    Reason: string;
    First: boolean;
begin
    CheckAssigned(ALines, 'the lines of the JCAMP-DX file being parsed');
    CheckAssigned(FPointsSet, 'the points set the JCAMP-DX file is parsed into');

    FPointsSet.Clear;
    Header := Default(TJcampHeader);
    Header.XFactor := 1;
    Header.YFactor := 1;
    InData := False;
    Blocks := 0;
    PreviousEndedInDifference := False;
    Step := 0;

    for i := 0 to ALines.Count - 1 do
    begin
        Line := ALines[i];
        if JcampLabel(Line, Label_, Value) then
        begin
            if Label_ = 'TITLE' then
            begin
                Inc(Blocks);
                //  A LINK BLOCK holds several spectra one after another. Only
                //  the first is read: concatenating them would make one curve
                //  whose x runs forwards, jumps back and runs forwards again.
                if Blocks > 1 then
                    Break;
                Header.Title := Value;
            end
            else if Label_ = 'DATATYPE' then
                Header.DataType := Value
            else if Label_ = 'XUNITS' then
                Header.XUnits := Value
            else if Label_ = 'YUNITS' then
                Header.YUnits := Value
            else if Label_ = 'XFACTOR' then
                Header.XFactor := StrToFloatDef(Value, 1)
            else if Label_ = 'YFACTOR' then
                Header.YFactor := StrToFloatDef(Value, 1)
            else if Label_ = 'DELTAX' then
                Header.DeltaX := StrToFloatDef(Value, 0)
            else if Label_ = 'FIRSTX' then
                Header.FirstX := StrToFloatDef(Value, 0)
            else if Label_ = 'LASTX' then
                Header.LastX := StrToFloatDef(Value, 0)
            else if Label_ = 'NPOINTS' then
                Header.NPoints := StrToIntDef(Value, 0)
            else if (Label_ = 'XYDATA') or (Label_ = 'XYPOINTS') or
                (Label_ = 'PEAKTABLE') then
            begin
                Header.HasData := True;
                Header.IsXYPairs := Pos('XY..XY', UpperCase(Value)) > 0;
                InData := True;
                if not Header.IsXYPairs then
                    if not JcampStep(Header, Step, Reason) then
                        raise EInvalidFileFormat.Create(Reason);
            end
            else if Label_ = 'END' then
                InData := False;
            Continue;
        end;

        if not InData then
            Continue;
        if Trim(Line) = '' then
            Continue;

        Values := DecodeJcampLine(Line, EndedInDifference);
        if Length(Values) = 0 then
            Continue;

        if Header.IsXYPairs then
        begin
            //  x,y x,y ... - the pairs are written out, so the file says where
            //  every point is and nothing is derived.
            k := 0;
            while k + 1 <= High(Values) do
            begin
                FPointsSet.AddNewPoint(Values[k] * Header.XFactor,
                    Values[k + 1] * Header.YFactor);
                Inc(k, 2);
            end;
        end
        else
        begin
            X := Values[0] * Header.XFactor;
            First := True;
            for k := 1 to High(Values) do
            begin
                //  A DIF-CODED FILE REPEATS the last value of a line as the
                //  first value of the next, as a check. Counting it twice
                //  shifts every later point by one step - a spectrum that looks
                //  right and is wrong by a channel.
                if First and PreviousEndedInDifference then
                begin
                    First := False;
                    Continue;
                end;
                First := False;
                Y := Values[k] * Header.YFactor;
                if FPointsSet.IndexOfValueX(X) = -1 then
                    FPointsSet.AddNewPoint(X, Y);
                X := X + Step;
            end;
        end;
        PreviousEndedInDifference := EndedInDifference;
    end;

    if JcampIsMissingSpectrum(Header) then
        raise EInvalidFileFormat.Create('This is not a spectrum: the service ' +
            'answered "' + Header.Title + '". It may hold no spectrum of that ' +
            'kind for this substance.');
    if not Header.HasData then
        raise EInvalidFileFormat.Create('This JCAMP-DX file holds no data ' +
            'section (##XYDATA or ##PEAK TABLE), so there is nothing in it to ' +
            'fit.');
end;

end.
