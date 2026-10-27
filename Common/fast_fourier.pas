// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The discrete Fourier transform, fast.)

Radix two, in place: Cooley & Tukey (1965), Math. Comp. 19:297, in its
iterative form - the input put in bit-reversed order, then log2 N passes of
butterflies. The twiddle factors are computed once per transform, a table of
N / 2, rather than once per butterfly: by a sine and a cosine per butterfly a
transform of 65 536 points made half a million calls to each.

Written here rather than taken from a library because the codebase links none
for numerics and needs one transform, for one use: correlating the difference
with the wavelets of residual_wavelet when the direct sum grows as the square
of the series.
}
unit fast_fourier;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math;

{ The discrete Fourier transform of ARe + i AIm, in place:
  X_k = sum_n x_n exp(-2 pi i k n / N). AInverse gives the inverse, divided by
  N, so a transform and its inverse give back what went in. The length must be
  a power of two; any other raises. }
procedure FourierTransform(var ARe, AIm: array of double; AInverse: boolean);

{ The smallest power of two not below AN (one for AN below one). }
function PowerOfTwoHolding(AN: longint): longint;

implementation

uses
    checks;

function PowerOfTwoHolding(AN: longint): longint;
begin
    Result := 1;
    while Result < AN do
        Result := Result * 2;
end;

procedure FourierTransform(var ARe, AIm: array of double; AInverse: boolean);
var
    n, i, j, k, Half, Size, Step: longint;
    CosT, SinT: array of double;
    Sign, TRe, TIm, WRe, WIm: double;
begin
    n := Length(ARe);
    CheckThat(Length(AIm) = n, 'the real and imaginary parts of one series');
    if (n = 0) or (PowerOfTwoHolding(n) <> n) then
        raise EArgumentException.CreateFmt(
            'a Fourier transform of %d points: the length must be a power of two',
            [n]);
    //  Into bit-reversed order.
    j := 0;
    for i := 0 to n - 2 do
    begin
        if i < j then
        begin
            TRe := ARe[i]; ARe[i] := ARe[j]; ARe[j] := TRe;
            TIm := AIm[i]; AIm[i] := AIm[j]; AIm[j] := TIm;
        end;
        k := n shr 1;
        while (k >= 1) and (j >= k) do
        begin
            j := j - k;
            k := k shr 1;
        end;
        j := j + k;
    end;
    //  exp(-+2 pi i m / n) for m below n / 2, once.
    Sign := IfThen(AInverse, 1, -1);
    SetLength(CosT, n div 2);
    SetLength(SinT, n div 2);
    for i := 0 to n div 2 - 1 do
    begin
        CosT[i] := Cos(2 * Pi * i / n);
        SinT[i] := Sign * Sin(2 * Pi * i / n);
    end;
    Size := 2;
    while Size <= n do
    begin
        Half := Size div 2;
        Step := n div Size;
        i := 0;
        while i < n do
        begin
            for k := 0 to Half - 1 do
            begin
                WRe := CosT[k * Step];
                WIm := SinT[k * Step];
                TRe := WRe * ARe[i + k + Half] - WIm * AIm[i + k + Half];
                TIm := WRe * AIm[i + k + Half] + WIm * ARe[i + k + Half];
                ARe[i + k + Half] := ARe[i + k] - TRe;
                AIm[i + k + Half] := AIm[i + k] - TIm;
                ARe[i + k] := ARe[i + k] + TRe;
                AIm[i + k] := AIm[i + k] + TIm;
            end;
            i := i + Size;
        end;
        Size := Size * 2;
    end;
    if AInverse then
        for i := 0 to n - 1 do
        begin
            ARe[i] := ARe[i] / n;
            AIm[i] := AIm[i] / n;
        end;
end;

end.
