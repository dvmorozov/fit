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

The drawing itself is the window's (TFormMain.SaveChartImage, through
IUiHost); these are its decisions, where a test reaches them.
}
unit chart_image;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_project_document;

const
    { The size a chart is exported at unless a size is asked for. }
    DefaultChartImageWidth = 1600;
    DefaultChartImageHeight = 900;
    { Below this nothing is legible; above it the picture is larger than any
      screen it will be looked at on and slow to draw. }
    MinChartImageSide = 200;
    MaxChartImageSide = 8000;

{ The line under the picture: where the data came from and when, or '' when
  the project does not know (a file opened from this computer). }
function ChartImageCaption(const AProvenance: TProjectProvenance): string;

{ Why AWidth x AHeight cannot be drawn, or '' when it can. }
function ChartImageSizeRefusal(AWidth, AHeight: integer): string;

{ Reads 'WIDTHxHEIGHT' (1600x900). False for anything else. }
function TryParseChartImageSize(const AText: string;
    out AWidth, AHeight: integer): boolean;

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

end.
