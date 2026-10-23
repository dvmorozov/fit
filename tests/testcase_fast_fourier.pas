// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The discrete Fourier transform, fast.)

Checked against its definition, X_k = sum_n x_n exp(-2 pi i k n / N), summed
term by term here, and against itself: the inverse gives back what went in.
}
unit testcase_fast_fourier;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, Types, fpcunit, testregistry, fast_fourier;

type
    TFastFourierTest = class(TTestCase)
    published
        procedure ItIsTheDiscreteTransformByDefinition;
        procedure TheInverseGivesBackWhatWentIn;
        procedure ALengthNotAPowerOfTwoIsRefused;
        procedure ThePowerOfTwoHoldingALengthIsTheSmallest;
    end;

implementation

{ A fixed sequence, so every run sees the same numbers. }
function Series(AN: longint; ASeed: longint): TDoubleDynArray;
var
    i: longint;
begin
    SetLength(Result, AN);
    for i := 0 to AN - 1 do
        Result[i] := Sin(ASeed + i * 0.7) * 3 + Cos(i * i * 0.013);
end;

procedure TFastFourierTest.ItIsTheDiscreteTransformByDefinition;
const
    N = 32;
var
    Re, Im, X, Y: TDoubleDynArray;
    k, j: longint;
    SRe, SIm, A: double;
begin
    X := Series(N, 1);
    Y := Series(N, 2);
    Re := Copy(X);
    Im := Copy(Y);
    FourierTransform(Re, Im, False);
    for k := 0 to N - 1 do
    begin
        SRe := 0;
        SIm := 0;
        for j := 0 to N - 1 do
        begin
            A := -2 * Pi * k * j / N;
            SRe := SRe + X[j] * Cos(A) - Y[j] * Sin(A);
            SIm := SIm + X[j] * Sin(A) + Y[j] * Cos(A);
        end;
        AssertEquals(Format('real part of X_%d', [k]), SRe, Re[k], 1e-9);
        AssertEquals(Format('imaginary part of X_%d', [k]), SIm, Im[k], 1e-9);
    end;
end;

procedure TFastFourierTest.TheInverseGivesBackWhatWentIn;
const
    N = 1024;
var
    Re, Im, X, Y: TDoubleDynArray;
    i: longint;
begin
    X := Series(N, 3);
    Y := Series(N, 4);
    Re := Copy(X);
    Im := Copy(Y);
    FourierTransform(Re, Im, False);
    FourierTransform(Re, Im, True);
    for i := 0 to N - 1 do
    begin
        AssertEquals('real', X[i], Re[i], 1e-10);
        AssertEquals('imaginary', Y[i], Im[i], 1e-10);
    end;
end;

procedure TFastFourierTest.ALengthNotAPowerOfTwoIsRefused;
var
    Re, Im: TDoubleDynArray;
    Raised: boolean;
begin
    SetLength(Re, 12);
    SetLength(Im, 12);
    Raised := False;
    try
        FourierTransform(Re, Im, False);
    except
        Raised := True;
    end;
    AssertTrue('twelve points are refused', Raised);
end;

procedure TFastFourierTest.ThePowerOfTwoHoldingALengthIsTheSmallest;
begin
    AssertEquals(1, PowerOfTwoHolding(1));
    AssertEquals(64, PowerOfTwoHolding(64));
    AssertEquals(128, PowerOfTwoHolding(65));
    AssertEquals(65536, PowerOfTwoHolding(49631));
end;

initialization
    RegisterTest('unit', TFastFourierTest);
end.
