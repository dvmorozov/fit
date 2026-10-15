// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A canvas that draws nothing and remembers what it was asked to draw.)

The chart tests run on the nogui widget set, which has no bitmaps and no device
contexts, so nothing drawn there can be read back as pixels. What CAN be read is
the sequence of calls the chart makes: every stroke, rectangle and piece of text,
with the pen or font it was drawn in, in the order drawn. The chart is drawn onto
this canvas through TAChart's own drawer (TCanvasDrawer), so the calls recorded
are the ones the screen would receive.

ONE THING OF THE DRAWER'S IS REPLACED, and only because it cannot run here: its
clipping. TCanvasDrawer clips a bitmap it keeps for transparency as well as the
canvas, and a bitmap needs a device context the nogui widget set does not have -
"Canvas does not allow drawing". TRecordingDrawer takes over the three clipping
calls and records the clip rectangle instead; everything else is TAChart's.

A STROKE IS RECORDED THE WAY QT6 DRAWS IT. The LCL's Qt6 LineTo pulls the end of a
line back one pixel on each axis before handing it to Qt - the GDI convention -
and Qt draws a line of no length as nothing. Covers answers for those shortened
lines, which is what made a dense curve vanish on a 200% desktop
(testcase_chart_strokes); a polyline is drawn whole.

Not an LCL-free unit and not meant to be: it IS the widget-set boundary, stood in
for. It owns nothing but its own lists.
}
unit recording_canvas;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, Math, Graphics, FPCanvas, TADrawUtils, TADrawerCanvas;

type
    { One straight stroke, as drawn: its ends, and the pen. Shortened says the
      stroke came from a LineTo, whose last pixel Qt6 leaves off. }
    TRecordedStroke = record
        X1, Y1, X2, Y2: integer;
        Color: TColor;
        Width: integer;
        Shortened: boolean;
    end;

    TRecordedRect = record
        Rect: TRect;
        PenColor: TColor;
        { pmXor for a rectangle drawn to be erased by drawing it again - which
          Qt, with no XOR mode, draws as nothing at all. }
        PenMode: TPenMode;
        BrushStyle: TBrushStyle;
        BrushColor: TColor;
    end;

    TRecordedText = record
        X, Y: integer;
        Text: string;
        Color: TColor;
    end;

    TRecordingCanvas = class(TCanvas)
    private
        FStrokes: array of TRecordedStroke;
        FRects: array of TRecordedRect;
        FTexts: array of TRecordedText;
        procedure AddStroke(AX1, AY1, AX2, AY2: integer; AShortened: boolean);
    protected
        procedure DoMoveTo(X, Y: integer); override;
        procedure DoLineTo(X, Y: integer); override;
        { A single pixel: TAChart finishes a polyline with its end pixel, which
          the LCL's Polyline leaves off. }
        procedure SetPixel(X, Y: integer; Value: TColor); override;
    public
        { Nothing here has a handle, and nothing needs one. }
        procedure RequiredState(ReqState: TCanvasState); override;
        procedure Polyline(Points: PPoint; NumPts: integer); override;
        procedure Rectangle(X1, Y1, X2, Y2: integer); override;
        procedure TextOut(X, Y: integer; const Text: string); override;
        { What TAChart writes its text with: TextOut ignores the text style. }
        procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
            const Style: TTextStyle); override;
        function TextExtent(const Text: string): TSize; override;

        { Forgets everything drawn so far. }
        procedure Reset;

        function StrokeCount: integer;
        function Stroke(AIndex: integer): TRecordedStroke;
        function RectCount: integer;
        function Rect(AIndex: integer): TRecordedRect;
        function TextCount: integer;
        function Text(AIndex: integer): TRecordedText;

        { Whether a stroke in AColor covers pixel column AColumn, as Qt6 draws. }
        function Covers(AColor: TColor; AColumn: longint): boolean;
        { The index of the first stroke in AColor, or -1: the order the chart drew
          its series in. }
        function FirstStrokeIn(AColor: TColor): integer;
        { Whether any text written reads exactly AText. }
        function Wrote(const AText: string): boolean;
        { The position AText was first written at; False when it never was. }
        function WhereWritten(const AText: string; out AAt: TPoint): boolean;
    end;

    { TAChart's canvas drawer, with clipping that touches only the canvas
      (see the unit header). Re-implements IChartDrawer so the three calls
      reach these methods rather than TCanvasDrawer's own. }
    TRecordingDrawer = class(TCanvasDrawer, IChartDrawer)
    public
        { TCanvasDrawer's own three are strict private, and re-implementing the
          interface needs every member visible: these do what they do for a
          TPen, TBrush and TFont, which is all a chart hands them. }
        procedure SetBrush(ABrush: TFPCustomBrush);
        procedure SetFont(AFont: TFPCustomFont);
        procedure SetPen(APen: TFPCustomPen);
        procedure ClippingStart; overload;
        procedure ClippingStart(const AClipRect: TRect); overload;
        procedure ClippingStop;
    end;

implementation

procedure TRecordingDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
    GetCanvas.Brush.Assign(ABrush);
end;

procedure TRecordingDrawer.SetFont(AFont: TFPCustomFont);
begin
    GetCanvas.Font.Assign(AFont);
end;

procedure TRecordingDrawer.SetPen(APen: TFPCustomPen);
begin
    if APen <> nil then
        GetCanvas.Pen.Assign(APen);
    //  As TCanvasDrawer does: a tool drawing in XOR mode gets an XOR pen.
    if FXor then
        GetCanvas.Pen.Mode := pmXor
    else
        GetCanvas.Pen.Mode := pmCopy;
end;

procedure TRecordingDrawer.ClippingStart;
begin
end;

procedure TRecordingDrawer.ClippingStart(const AClipRect: TRect);
begin
end;

procedure TRecordingDrawer.ClippingStop;
begin
end;

procedure TRecordingCanvas.RequiredState(ReqState: TCanvasState);
begin
end;

procedure TRecordingCanvas.AddStroke(AX1, AY1, AX2, AY2: integer;
    AShortened: boolean);
var
    n: integer;
begin
    n := Length(FStrokes);
    SetLength(FStrokes, n + 1);
    FStrokes[n].X1 := AX1;
    FStrokes[n].Y1 := AY1;
    FStrokes[n].X2 := AX2;
    FStrokes[n].Y2 := AY2;
    FStrokes[n].Color := Pen.Color;
    FStrokes[n].Width := Pen.Width;
    FStrokes[n].Shortened := AShortened;
end;

procedure TRecordingCanvas.DoMoveTo(X, Y: integer);
begin
end;

procedure TRecordingCanvas.DoLineTo(X, Y: integer);
begin
    AddStroke(PenPos.X, PenPos.Y, X, Y, True);
end;

procedure TRecordingCanvas.SetPixel(X, Y: integer; Value: TColor);
var
    Saved: TColor;
begin
    Saved := Pen.Color;
    Pen.Color := Value;
    AddStroke(X, Y, X, Y, False);
    Pen.Color := Saved;
end;

procedure TRecordingCanvas.Polyline(Points: PPoint; NumPts: integer);
var
    i: integer;
begin
    for i := 1 to NumPts - 1 do
        AddStroke(Points[i - 1].X, Points[i - 1].Y, Points[i].X, Points[i].Y, False);
end;

procedure TRecordingCanvas.Rectangle(X1, Y1, X2, Y2: integer);
var
    n: integer;
begin
    n := Length(FRects);
    SetLength(FRects, n + 1);
    FRects[n].Rect := Types.Rect(X1, Y1, X2, Y2);
    FRects[n].PenColor := Pen.Color;
    FRects[n].PenMode := Pen.Mode;
    FRects[n].BrushStyle := Brush.Style;
    FRects[n].BrushColor := Brush.Color;
end;

procedure TRecordingCanvas.TextOut(X, Y: integer; const Text: string);
var
    n: integer;
begin
    n := Length(FTexts);
    SetLength(FTexts, n + 1);
    FTexts[n].X := X;
    FTexts[n].Y := Y;
    FTexts[n].Text := Text;
    FTexts[n].Color := Font.Color;
end;

procedure TRecordingCanvas.TextRect(ARect: TRect; X, Y: integer;
    const Text: string; const Style: TTextStyle);
begin
    TextOut(X, Y, Text);
end;

function TRecordingCanvas.TextExtent(const Text: string): TSize;
begin
    Result.cx := 6 * Length(Text);
    Result.cy := 12;
end;

procedure TRecordingCanvas.Reset;
begin
    FStrokes := nil;
    FRects := nil;
    FTexts := nil;
end;

function TRecordingCanvas.StrokeCount: integer;
begin
    Result := Length(FStrokes);
end;

function TRecordingCanvas.Stroke(AIndex: integer): TRecordedStroke;
begin
    Result := FStrokes[AIndex];
end;

function TRecordingCanvas.RectCount: integer;
begin
    Result := Length(FRects);
end;

function TRecordingCanvas.Rect(AIndex: integer): TRecordedRect;
begin
    Result := FRects[AIndex];
end;

function TRecordingCanvas.TextCount: integer;
begin
    Result := Length(FTexts);
end;

function TRecordingCanvas.Text(AIndex: integer): TRecordedText;
begin
    Result := FTexts[AIndex];
end;

{ The LCL's Qt6 LineTo, as far as a column can tell: the end is pulled back one
  pixel towards the start on each axis (TQtDeviceContext.GetLineLastPixelPos),
  and a line left with no length is not drawn at all. A polyline segment keeps
  its end. }
function TRecordingCanvas.Covers(AColor: TColor; AColumn: longint): boolean;
var
    i, X2, Y2: integer;
    S: TRecordedStroke;
begin
    Result := False;
    for i := 0 to High(FStrokes) do
    begin
        S := FStrokes[i];
        if S.Color <> AColor then
            Continue;
        X2 := S.X2;
        Y2 := S.Y2;
        if S.Shortened then
        begin
            X2 := S.X2 - Sign(S.X2 - S.X1);
            Y2 := S.Y2 - Sign(S.Y2 - S.Y1);
            if (X2 = S.X1) and (Y2 = S.Y1) then
                Continue;
        end;
        if (AColumn >= Min(S.X1, X2)) and (AColumn <= Max(S.X1, X2)) then
            Exit(True);
    end;
end;

function TRecordingCanvas.FirstStrokeIn(AColor: TColor): integer;
var
    i: integer;
begin
    for i := 0 to High(FStrokes) do
        if FStrokes[i].Color = AColor then
            Exit(i);
    Result := -1;
end;

function TRecordingCanvas.Wrote(const AText: string): boolean;
var
    At: TPoint;
begin
    Result := WhereWritten(AText, At);
end;

function TRecordingCanvas.WhereWritten(const AText: string; out AAt: TPoint): boolean;
var
    i: integer;
begin
    for i := 0 to High(FTexts) do
        if FTexts[i].Text = AText then
        begin
            AAt := Point(FTexts[i].X, FTexts[i].Y);
            Exit(True);
        end;
    AAt := Point(0, 0);
    Result := False;
end;

end.
