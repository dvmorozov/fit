// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The application's icon, scaled for a dialog to show.)

WHY NOT A STRETCHED TIcon. The About box first assigned the application icon to
a TImage with Stretch on. The LCL draws a stretched icon by stretching its
colours and its mask apart, and on Windows the result was a picture with thin
light stripes through it wherever the two scalings disagreed by a column. So
the icon is scaled here instead, once, into a picture of exactly the size it
will be drawn at, and the image draws it unstretched.

TRANSPARENCY IS SCALED WITH THE COLOUR. Every output pixel averages the source
pixels it covers WEIGHTED BY THEIR ALPHA: an icon's transparent pixels usually
carry black in their colour channels, and a plain average would ring every
shape with a dark fringe.

THE FRAME SCALED FROM is the smallest one at least as large as the target, so
the icon is only ever made smaller; failing that, the largest there is.
}
unit app_logo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FPImage, IntfGraphics, GraphType;

{ An empty 32-bit picture with an alpha channel, every pixel transparent. }
function NewLogoImage(AWidth, AHeight: longint): TLazIntfImage;

{ ASrc scaled to AWidth x AHeight by averaging the source area each output pixel
  covers, weighted by alpha. The caller owns the result. }
function SmoothlyScaled(ASrc: TLazIntfImage;
    AWidth, AHeight: longint): TLazIntfImage;

{ Of frames whose sizes are ASizes, the index of the smallest one no smaller
  than ATarget, or of the largest when none is; -1 when there are none. }
function LogoFrameFor(const ASizes: array of longint; ATarget: longint): longint;

implementation

function NewLogoImage(AWidth, AHeight: longint): TLazIntfImage;
var
    Raw: TRawImage;
begin
    //  Described here rather than asked of the widget set, so the picture is the
    //  same in the application and in a test process that has no display.
    Raw.Init;
    Raw.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    Raw.CreateData(True);
    Result := TLazIntfImage.Create(0, 0);
    Result.SetRawImage(Raw);
    Result.FillPixels(colTransparent);
end;

function SmoothlyScaled(ASrc: TLazIntfImage;
    AWidth, AHeight: longint): TLazIntfImage;
const
    //  Samples per output pixel along each axis. Enough to average a source
    //  area up to four pixels wide - a 256 frame drawn at 64 - without gaps.
    Sub = 4;
var
    x, y, sx, sy, px, py: longint;
    ScaleX, ScaleY, A, R, G, B, W: double;
    C: TFPColor;
begin
    Result := NewLogoImage(AWidth, AHeight);
    if (ASrc.Width = 0) or (ASrc.Height = 0) then
        Exit;
    ScaleX := ASrc.Width / AWidth;
    ScaleY := ASrc.Height / AHeight;
    for y := 0 to AHeight - 1 do
        for x := 0 to AWidth - 1 do
        begin
            A := 0;
            R := 0;
            G := 0;
            B := 0;
            for sy := 0 to Sub - 1 do
                for sx := 0 to Sub - 1 do
                begin
                    px := Trunc((x + (sx + 0.5) / Sub) * ScaleX);
                    py := Trunc((y + (sy + 0.5) / Sub) * ScaleY);
                    if px >= ASrc.Width then
                        px := ASrc.Width - 1;
                    if py >= ASrc.Height then
                        py := ASrc.Height - 1;
                    C := ASrc.Colors[px, py];
                    W := C.Alpha;
                    A := A + W;
                    R := R + C.Red * W;
                    G := G + C.Green * W;
                    B := B + C.Blue * W;
                end;
            if A > 0 then
            begin
                C.Red := Round(R / A);
                C.Green := Round(G / A);
                C.Blue := Round(B / A);
                C.Alpha := Round(A / (Sub * Sub));
            end
            else
                C := colTransparent;
            Result.Colors[x, y] := C;
        end;
end;

function LogoFrameFor(const ASizes: array of longint; ATarget: longint): longint;
var
    i, Largest: longint;
begin
    Result := -1;
    Largest := -1;
    for i := 0 to High(ASizes) do
    begin
        if (ASizes[i] >= ATarget) and
            ((Result < 0) or (ASizes[i] < ASizes[Result])) then
            Result := i;
        if (Largest < 0) or (ASizes[i] > ASizes[Largest]) then
            Largest := i;
    end;
    if Result < 0 then
        Result := Largest;
end;

end.
