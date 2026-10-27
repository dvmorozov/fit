// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The model's chart as a picture: what the line under it says, and
which sizes are refused.)

WHY A LINE UNDER THE CHART. A chart that leaves the program leaves its project
behind, and with it the record of where the data came from - which is what a
reader needs, and what a data source's terms usually ask to be cited. So when
the project knows its origin (a data source recorded it), the picture says it.

WHY A SCALE. The window's chart is drawn for a screen: a one-pixel line, a
marker a few pixels across, lettering of the desktop's size. Painted onto a
picture three times the default size, all of that keeps its pixel size and
shrinks to a third of the picture - unreadable once the picture is shown at the
width of a page. TAChart's own answer is a drawer that multiplies every pen
width, font size, marker size and margin it is handed (TScaledCanvasDrawer,
which its printing demo uses to go from screen to printer resolution), so the
scale is the one number that drawer takes. 1 draws exactly what the window
draws, which is what every picture was before the choice existed; 'auto' takes
the factor by which the picture is larger than the default size, so a picture at
three times 1600x900 reads like the default one enlarged. Rejected: changing
the chart's own fonts and pens for the export and putting them back - it is the
same multiplication done by hand over every axis, mark and series, and the next
element added to the chart would be missed.

WHY LAYERS CAN BE LEFT OUT. A fit interval defaults to the whole profile, so its
hatching covers the whole plot, and the difference runs along the bottom of it:
both are working views, and on a published picture they bury the model. The
choice names what to leave out rather than listing what to keep, so a series
added to the chart later is drawn unless someone decides otherwise. A series is
recognised by the colour role it is drawn in (series_style), which is the one
thing every framework series declares about what it is.

The drawing itself is the window's (TFormMain.SaveChartImage, through
IUiHost); these are its decisions, where a test reaches them.
}
unit chart_image;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_project_document, series_style;

const
    { The size a chart is exported at unless a size is asked for. }
    DefaultChartImageWidth = 1600;
    DefaultChartImageHeight = 900;
    { Below this nothing is legible; above it the picture is larger than any
      screen it will be looked at on and slow to draw. }
    MinChartImageSide = 200;
    MaxChartImageSide = 8000;
    { Below half, a one-pixel line rounds to nothing; ten times the screen is
      already more than the largest picture allowed needs. }
    MinChartImageScale: double = 0.5;
    MaxChartImageScale: double = 10;
    { The scale answered to have lines and text grow with the picture. }
    ChartImageScaleAuto = 'auto';

type
    { What a picture of the chart is drawn with. }
    TChartImageOptions = record
        Width, Height: integer;
        { How many times larger than in the window lines, markers, lettering
          and margins are drawn; 1 is exactly the window's drawing. }
        Scale: double;
        { Whether the fit intervals' bounds and hatching are drawn. }
        DrawFitIntervals: boolean;
        { Whether the difference between the data and the model is drawn, with
          the stretches marked as not noise, which are a verdict on it. }
        DrawDifference: boolean;
    end;

    { The same choices as a person or a script states them: text, '' for
      "leave the default". One reading for both, so a dialog and a script can
      never disagree about what an answer means. }
    TChartImageAnswers = record
        { WIDTHxHEIGHT. }
        Size: string;
        { A number, or ChartImageScaleAuto. }
        Scale: string;
        { yes or no. }
        FitIntervals: string;
        Difference: string;
    end;

{ The line under the picture: where the data came from and when, or '' when
  the project does not know (a file opened from this computer). }
function ChartImageCaption(const AProvenance: TProjectProvenance): string;

{ Why AWidth x AHeight cannot be drawn, or '' when it can. }
function ChartImageSizeRefusal(AWidth, AHeight: integer): string;

{ Reads 'WIDTHxHEIGHT' (1600x900). False for anything else. }
function TryParseChartImageSize(const AText: string;
    out AWidth, AHeight: integer): boolean;

{ Reads yes, no, true or false, in any case. False for anything else. }
function TryReadChartImageYesNo(const AText: string; out AYes: boolean): boolean;
{ AYes as the answer ReadChartImageAnswers reads back. }
function ChartImageYesNo(AYes: boolean): string;

{ Today's picture: the default size, drawn as the window draws, every layer. }
function DefaultChartImageOptions: TChartImageOptions;
{ The defaults as text, which is what a dialog starts from. }
function DefaultChartImageAnswers: TChartImageAnswers;
{ How many times larger than the default size AWidth x AHeight is - by the
  side that grew less, so nothing is drawn larger than the picture has room
  for. }
function ChartImageAutoScale(AWidth, AHeight: integer): double;
{ Reads AAnswers into AOptions: '' when every answer can be drawn, otherwise
  what is wrong with the first that cannot, and AOptions holds the defaults. }
function ReadChartImageAnswers(const AAnswers: TChartImageAnswers;
    out AOptions: TChartImageOptions): string;
{ Whether a series drawn in the colour ASource names belongs on a picture made
  with AOptions. }
function ChartImageDrawsSeries(const ASource: TSeriesColorSource;
    const AOptions: TChartImageOptions): boolean;

implementation

function ChartImageCaption(const AProvenance: TProjectProvenance): string;
var
    Day: string;
begin
    Result := '';
    if Trim(AProvenance.OriginSourceTitle) = '' then
        Exit;
    Result := 'Source: ' + Trim(AProvenance.OriginSourceTitle);
    if Trim(AProvenance.OriginQuery) <> '' then
        Result := Result + ', ' + Trim(AProvenance.OriginQuery);
    //  The day is what a citation needs; the time of day is noise.
    Day := Copy(Trim(AProvenance.OriginRetrievedAt), 1, 10);
    if Day <> '' then
        Result := Result + ', retrieved ' + Day;
end;

function ChartImageSizeRefusal(AWidth, AHeight: integer): string;
begin
    Result := '';
    if (AWidth < MinChartImageSide) or (AHeight < MinChartImageSide) or
        (AWidth > MaxChartImageSide) or (AHeight > MaxChartImageSide) then
        Result := Format('A chart image is %d to %d pixels a side; %d x %d is ' +
            'not.', [MinChartImageSide, MaxChartImageSide, AWidth, AHeight]);
end;

function TryParseChartImageSize(const AText: string;
    out AWidth, AHeight: integer): boolean;
var
    X: integer;
    Text: string;
begin
    Text := LowerCase(Trim(AText));
    X := Pos('x', Text);
    Result := (X > 1) and TryStrToInt(Copy(Text, 1, X - 1), AWidth) and
        TryStrToInt(Copy(Text, X + 1, MaxInt), AHeight);
    if not Result then
    begin
        AWidth := 0;
        AHeight := 0;
    end;
end;

function TryReadChartImageYesNo(const AText: string; out AYes: boolean): boolean;
var
    Text: string;
begin
    Text := LowerCase(Trim(AText));
    AYes := (Text = 'yes') or (Text = 'true');
    Result := AYes or (Text = 'no') or (Text = 'false');
end;

function ChartImageYesNo(AYes: boolean): string;
begin
    if AYes then
        Result := 'yes'
    else
        Result := 'no';
end;

function DefaultChartImageOptions: TChartImageOptions;
begin
    Result.Width := DefaultChartImageWidth;
    Result.Height := DefaultChartImageHeight;
    Result.Scale := 1;
    Result.DrawFitIntervals := True;
    Result.DrawDifference := True;
end;

function DefaultChartImageAnswers: TChartImageAnswers;
begin
    Result.Size := IntToStr(DefaultChartImageWidth) + 'x' +
        IntToStr(DefaultChartImageHeight);
    Result.Scale := '1';
    Result.FitIntervals := ChartImageYesNo(True);
    Result.Difference := ChartImageYesNo(True);
end;

function ChartImageAutoScale(AWidth, AHeight: integer): double;
var
    ByWidth, ByHeight: double;
begin
    ByWidth := AWidth / DefaultChartImageWidth;
    ByHeight := AHeight / DefaultChartImageHeight;
    if ByWidth < ByHeight then
        Result := ByWidth
    else
        Result := ByHeight;
end;

{ A number written with a point or a comma, whatever the system's own decimal
  separator is: a script is written once and run on machines set up for any
  language. }
function PointDecimals: TFormatSettings;
begin
    Result := DefaultFormatSettings;
    Result.DecimalSeparator := '.';
    Result.ThousandSeparator := #0;
end;

function TryReadScale(const AText: string; out AScale: double): boolean;
begin
    Result := TryStrToFloat(StringReplace(Trim(AText), ',', '.', []), AScale,
        PointDecimals);
end;

function ReadChartImageAnswers(const AAnswers: TChartImageAnswers;
    out AOptions: TChartImageOptions): string;
var
    O: TChartImageOptions;
    Scale: string;
begin
    AOptions := DefaultChartImageOptions;
    O := AOptions;
    Result := '';
    if Trim(AAnswers.Size) <> '' then
    begin
        if not TryParseChartImageSize(AAnswers.Size, O.Width, O.Height) then
            Exit(Format('"%s" is not a size: write WIDTHxHEIGHT, such as 1200x630',
                [AAnswers.Size]));
        Result := ChartImageSizeRefusal(O.Width, O.Height);
        if Result <> '' then
            Exit;
    end;
    Scale := Trim(AAnswers.Scale);
    if SameText(Scale, ChartImageScaleAuto) then
        O.Scale := ChartImageAutoScale(O.Width, O.Height)
    else if (Scale <> '') and not TryReadScale(Scale, O.Scale) then
        O.Scale := -1;
    if (O.Scale < MinChartImageScale) or (O.Scale > MaxChartImageScale) then
        Exit(Format('"%s" is not a scale this picture can be drawn at: write a ' +
            'number from %s to %s - 3 draws lines and text three times their ' +
            'size on screen - or %s to have them grow with the picture',
            [AAnswers.Scale, FloatToStr(MinChartImageScale, PointDecimals),
            FloatToStr(MaxChartImageScale, PointDecimals), ChartImageScaleAuto]));
    if (Trim(AAnswers.FitIntervals) <> '') and
        not TryReadChartImageYesNo(AAnswers.FitIntervals, O.DrawFitIntervals) then
        Exit(Format('Whether to draw the fit intervals is yes or no, not "%s"',
            [AAnswers.FitIntervals]));
    if (Trim(AAnswers.Difference) <> '') and
        not TryReadChartImageYesNo(AAnswers.Difference, O.DrawDifference) then
        Exit(Format('Whether to draw the difference is yes or no, not "%s"',
            [AAnswers.Difference]));
    AOptions := O;
end;

function ChartImageDrawsSeries(const ASource: TSeriesColorSource;
    const AOptions: TChartImageOptions): boolean;
begin
    Result := True;
    if ASource.Kind <> cskRole then
        Exit;
    case ASource.Role of
        crIntervalBound:
            Result := AOptions.DrawFitIntervals;
        //  Where the difference is not noise is a verdict on the difference,
        //  hatched over the plot like the intervals: without the difference it
        //  would mark stretches of something the picture does not show.
        crResidual, crNonRandom:
            Result := AOptions.DrawDifference;
    end;
end;

end.
