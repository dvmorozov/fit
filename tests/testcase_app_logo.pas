// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for how the About box draws the application's icon.

  WHY THESE EXIST. The About box showed the icon with thin light stripes
  through it: the LCL, stretching a TIcon to the image's size, scaled the
  picture and its mask separately and let the background through between the
  columns. The icon is now scaled here, with its transparency carried through
  the scaling, and drawn at exactly the size it was scaled to. What these check
  is what the eye caught: nothing opaque turns see-through on the way, and the
  frame scaled from is one big enough not to be blown up. }
unit testcase_app_logo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, FPImage, IntfGraphics, app_logo;

type
    TAppLogoTest = class(TTestCase)
    private
        function Solid(ASize: longint; const AColor: TFPColor): TLazIntfImage;
    published
        procedure TheScaledImageHasTheSizeAskedFor;
        procedure AnOpaqueImageStaysOpaqueEverywhere;
        procedure AColourSurvivesTheScaling;
        procedure TransparencyIsCarriedThrough;
        procedure ATransparentPixelDoesNotDarkenItsNeighbour;
        procedure TheFrameChosenIsTheSmallestNotSmallerThanTheTarget;
        procedure WithNoFrameThatLargeTheLargestIsChosen;
    end;

implementation

function TAppLogoTest.Solid(ASize: longint; const AColor: TFPColor): TLazIntfImage;
var
    x, y: longint;
begin
    Result := NewLogoImage(ASize, ASize);
    for y := 0 to ASize - 1 do
        for x := 0 to ASize - 1 do
            Result.Colors[x, y] := AColor;
end;

procedure TAppLogoTest.TheScaledImageHasTheSizeAskedFor;
var
    Src, Dst: TLazIntfImage;
begin
    Src := Solid(256, colRed);
    Dst := SmoothlyScaled(Src, 192, 150);
    try
        AssertEquals(192, Dst.Width);
        AssertEquals(150, Dst.Height);
    finally
        Dst.Free;
        Src.Free;
    end;
end;

procedure TAppLogoTest.AnOpaqueImageStaysOpaqueEverywhere;
var
    Src, Dst: TLazIntfImage;
    x, y, Size: longint;
begin
    //  Several ratios, down and up: the stripes came and went with the ratio.
    for Size in [48, 96, 128, 150, 192, 300] do
    begin
        Src := Solid(256, colBlue);
        Dst := SmoothlyScaled(Src, Size, Size);
        try
            for y := 0 to Size - 1 do
                for x := 0 to Size - 1 do
                    AssertEquals(Format('alpha at %d,%d scaled to %d', [x, y, Size]),
                        alphaOpaque, Dst.Colors[x, y].Alpha);
        finally
            Dst.Free;
            Src.Free;
        end;
    end;
end;

procedure TAppLogoTest.AColourSurvivesTheScaling;
var
    Src, Dst: TLazIntfImage;
    C: TFPColor;
begin
    C := FPColor($8000, $4000, $C000, alphaOpaque);
    Src := Solid(256, C);
    Dst := SmoothlyScaled(Src, 100, 100);
    try
        AssertEquals(C.Red shr 8, Dst.Colors[50, 50].Red shr 8);
        AssertEquals(C.Green shr 8, Dst.Colors[50, 50].Green shr 8);
        AssertEquals(C.Blue shr 8, Dst.Colors[50, 50].Blue shr 8);
    finally
        Dst.Free;
        Src.Free;
    end;
end;

procedure TAppLogoTest.TransparencyIsCarriedThrough;
var
    Src, Dst: TLazIntfImage;
begin
    Src := Solid(256, colTransparent);
    Dst := SmoothlyScaled(Src, 128, 128);
    try
        AssertEquals(alphaTransparent, Dst.Colors[64, 64].Alpha);
    finally
        Dst.Free;
        Src.Free;
    end;
end;

procedure TAppLogoTest.ATransparentPixelDoesNotDarkenItsNeighbour;
var
    Src, Dst: TLazIntfImage;
    x, y: longint;
    Edge: TFPColor;
begin
    //  Left half white and opaque, right half transparent - whose colour
    //  channels are black, as an icon's usually are. Averaged without regard
    //  to alpha, the edge turns grey: a dark fringe round every shape.
    Src := NewLogoImage(4, 4);
    for y := 0 to 3 do
        for x := 0 to 3 do
            if x < 2 then
                Src.Colors[x, y] := colWhite
            else
                Src.Colors[x, y] := FPColor(0, 0, 0, alphaTransparent);
    //  One output pixel over both halves: the edge itself.
    Dst := SmoothlyScaled(Src, 1, 1);
    try
        Edge := Dst.Colors[0, 0];
        AssertTrue('the edge is partly see-through', Edge.Alpha < alphaOpaque);
        AssertTrue('and still white where it shows: red ' + IntToStr(Edge.Red),
            Edge.Red > $F000);
    finally
        Dst.Free;
        Src.Free;
    end;
end;

procedure TAppLogoTest.TheFrameChosenIsTheSmallestNotSmallerThanTheTarget;
begin
    AssertEquals(6, LogoFrameFor([16, 24, 32, 48, 64, 128, 256], 150));
    AssertEquals(5, LogoFrameFor([16, 24, 32, 48, 64, 128, 256], 128));
    AssertEquals(4, LogoFrameFor([16, 24, 32, 48, 64, 128, 256], 64));
    AssertEquals(1, LogoFrameFor([256, 16], 16));
end;

procedure TAppLogoTest.WithNoFrameThatLargeTheLargestIsChosen;
begin
    AssertEquals(2, LogoFrameFor([16, 32, 64], 200));
    AssertEquals(-1, LogoFrameFor([], 64));
end;

initialization
    RegisterTest('unit', TAppLogoTest);
end.
