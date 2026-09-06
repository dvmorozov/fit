// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Special functions for the native curve engine: the error function and
the Faddeeva/Voigt profile.)

These are the Pascal side of the "parity rule" (docs/contributing/adding-a-curve-model.md):
the native engine must be able to compute every function a lineshape uses, and each
is pinned to the Python sidecar's scipy.special reference by golden-oracle tests.
Kept dependency-free (only the FPC Math unit) and accurate to ~1e-12 over the range
that matters for fitting.

@author(Dmitry Morozov dvmorozov@hotmail.com)
}
unit special_functions;

{$mode objfpc}{$H+}

interface

{ Error function, matching scipy.special.erf to ~1e-14. }
function Erf(x: double): double;
{ Complementary error function erfc(x) = 1 - erf(x), accurate for large x too
  (where 1 - erf(x) would lose all precision), matching scipy.special.erfc. }
function Erfc(x: double): double;
{ Scaled complementary error function erfcx(x) = exp(x^2)*erfc(x), matching
  scipy.special.erfcx. It stays O(1) for large positive x (where exp(x^2) and
  erfc(x) would separately over/underflow), which is what makes the exponentially
  modified Gaussian evaluable down to tau -> 0. }
function Erfcx(x: double): double;
{ The Voigt profile V(u; sigma, gamma) - a Gaussian (std sigma) convolved with a
  Lorentzian (HWHM gamma), area-normalised - matching scipy.special.voigt_profile.
  It is Re[w(z)]/(sigma*sqrt(2*pi)) with z = (u + i*gamma)/(sigma*sqrt(2)), where w
  is the Faddeeva function; evaluated by Weideman's rational approximation (N = 32),
  accurate to ~1e-13. Reduces to a Gaussian as gamma -> 0 and to a Lorentzian as
  sigma -> 0. }
function VoigtProfile(u, sigma, gamma: double): double;
{ Area-normalised exponentially modified Gaussian value at offset u = x - x0, for
  Gaussian width sigma and relaxation time tau (matching scipy's exponnorm). Uses
  the erfcx form on the rising side (z >= 0) and the exp*erfc form on the falling
  side (z < 0) - each branch is overflow-free where it applies, so the whole curve
  is finite for every parameter (the naive single-formula versions are not). }
function EmgProfile(u, sigma, tau: double): double;

implementation

const
    { 2/sqrt(pi) and 1/sqrt(pi). }
    TWO_OVER_SQRT_PI = 1.1283791670955126;
    ONE_OVER_SQRT_PI = 0.5641895835477563;

{ erf via the all-positive series (Abramowitz & Stegun 7.1.6):
    erf(x) = (2/sqrt(pi)) exp(-x^2) * sum over n>=0 of 2^n x^(2n+1) / (1*3*...*(2n+1)).
  Every term is positive, so there is no cancellation; used for |x| <= 4 where it
  converges in a few dozen terms. }
function ErfSeries(x: double): double;
var
    term, sum, x2: double;
    n: integer;
begin
    x2   := x * x;
    term := x;
    sum  := x;
    n    := 0;
    repeat
        Inc(n);
        term := term * (2 * x2) / (2 * n + 1);
        sum  := sum + term;
    until (Abs(term) <= 1e-18 * Abs(sum)) or (n > 200);
    Result := TWO_OVER_SQRT_PI * Exp(-x2) * sum;
end;

{ The value of the erfc continued fraction (Lentz) for x > 0:
    cf(x) = x + (1/2)/(x + 1/(x + (3/2)/(x + 2/(x + ...)))),  n-th numerator = n/2.
  Then erfc(x) = exp(-x^2)/(sqrt(pi)*cf) and erfcx(x) = 1/(sqrt(pi)*cf) - the latter
  with no exp, so it stays finite for large x. }
function ErfcContinuedFraction(x: double): double;
const
    TINY = 1e-300;
var
    f, c, d, delta, a: double;
    n: integer;
begin
    f := x;
    if f = 0 then
        f := TINY;
    c := f;
    d := 0;
    n := 0;
    repeat
        Inc(n);
        a := n / 2;
        d := x + a * d;
        if d = 0 then
            d := TINY;
        c := x + a / c;
        if c = 0 then
            c := TINY;
        d := 1 / d;
        delta := c * d;
        f := f * delta;
    until (Abs(delta - 1) < 1e-16) or (n > 300);
    Result := f;
end;

function ErfcCF(x: double): double;
begin
    Result := ONE_OVER_SQRT_PI * Exp(-x * x) / ErfcContinuedFraction(x);
end;

function Erf(x: double): double;
begin
    if Abs(x) <= 4 then
        Result := ErfSeries(x)
    else if x > 0 then
        Result := 1 - ErfcCF(x)
    else
        Result := ErfcCF(-x) - 1;
end;

function Erfc(x: double): double;
begin
    //  Below 1.5, erfc is O(1) so 1 - erf is accurate; above it erfc is small and
    //  1 - erf would cancel, so use the continued fraction (accurate for large x).
    if Abs(x) <= 1.5 then
        Result := 1 - ErfSeries(x)
    else if x > 0 then
        Result := ErfcCF(x)
    else
        Result := 2 - ErfcCF(-x);
end;

function Erfcx(x: double): double;
begin
    if x >= 1.5 then
        //  No exp here: 1/(sqrt(pi)*cf) is exp(x^2)*erfc(x) directly, so it stays
        //  finite for large x.
        Result := ONE_OVER_SQRT_PI / ErfcContinuedFraction(x)
    else if x >= -1.5 then
        Result := Exp(x * x) * (1 - ErfSeries(x))
    else
        //  erfcx(x) = 2 exp(x^2) - erfcx(-x); grows for very negative x (inherent).
        Result := 2 * Exp(x * x) - ONE_OVER_SQRT_PI / ErfcContinuedFraction(-x);
end;

const
    { Weideman's Faddeeva approximation, N = 32: L and the polynomial coefficients
      in highest-power-first order (generated once from the FFT of the reference
      function; see the commit that introduced this). Accurate to ~1e-13 over the
      upper half plane. }
    WEIDEMAN_L: double = 4.756828460010884;
    WEIDEMAN_A: array[0..31] of double = (
        -1.3031797863050087e-12,  3.7408812931653625e-12,  8.03036789996389e-12,
        -2.154363207783877e-11,  -5.5442359481664624e-11,  1.1658251093523774e-10,
         4.153743091833453e-10,  -5.231020481196329e-10,   -3.208015091723369e-09,
         8.124889456846652e-10,   2.3797556779897417e-08,   2.2930439065099966e-08,
        -1.4813078915120977e-07, -4.1840763702169776e-07,   4.2558331375750085e-07,
         4.40153173157855e-06,    6.821031944001985e-06,    -2.140961920171075e-05,
        -0.00013075449254615346, -0.0002453298027002143,    0.0003925913607007031,
         0.004519541105349217,    0.019006155784845408,     0.05730440352983722,
         0.14060716226893769,     0.2954445107150873,       0.5460139720639341,
         0.9019254893647999,      1.345544169234545,        1.8256696296324815,
         2.2635372999002676,      2.5722534081245696);

{ Real part of the Faddeeva function w(z) for Im(z) >= 0, via Weideman's method.
  Complex arithmetic is done by hand on (re, im) pairs. }
function FaddeevaReal(zr, zi: double): double;
var
    dr, di, nr, ni, Zr_, Zi_, pr, pi_, tr, den, sr, si: double;
    k: integer;
begin
    //  denom = L - i*z = (L + zi) - i*zr ;  num = L + i*z = (L - zi) + i*zr
    dr := WEIDEMAN_L + zi;   di := -zr;
    nr := WEIDEMAN_L - zi;   ni := zr;

    //  Z = num / denom
    den  := dr * dr + di * di;
    Zr_  := (nr * dr + ni * di) / den;
    Zi_  := (ni * dr - nr * di) / den;

    //  p = polyval(A, Z) by Horner (A[0] is the highest power).
    pr := WEIDEMAN_A[0];  pi_ := 0;
    for k := 1 to High(WEIDEMAN_A) do
    begin
        //  p := p*Z + A[k]
        tr  := pr * Zr_ - pi_ * Zi_;
        pi_ := pr * Zi_ + pi_ * Zr_;
        pr  := tr + WEIDEMAN_A[k];
    end;

    //  w = 2*p/denom^2 + (1/sqrt(pi))/denom ; return Re(w).
    //  denom^2:
    sr := dr * dr - di * di;
    si := 2 * dr * di;
    //  2*p/denom^2:
    den := sr * sr + si * si;
    tr  := 2 * (pr * sr + pi_ * si) / den;      //  Re(2p/denom^2)
    //  (1/sqrt(pi))/denom  -> Re = ONE_OVER_SQRT_PI * dr / (dr^2+di^2)
    Result := tr + ONE_OVER_SQRT_PI * dr / (dr * dr + di * di);
end;

function VoigtProfile(u, sigma, gamma: double): double;
var
    s, zr, zi: double;
begin
    s  := sigma * Sqrt(2.0);
    zr := u / s;
    zi := gamma / s;
    Result := FaddeevaReal(zr, zi) / (sigma * Sqrt(2.0 * Pi));
end;

function EmgProfile(u, sigma, tau: double): double;
var
    z: double;
begin
    z := (sigma / tau - u / sigma) / Sqrt(2.0);
    if z >= 0 then
        //  Rising side/peak: both factors are bounded.
        Result := (1 / (2 * tau)) * Exp(-u * u / (2 * sigma * sigma)) * Erfcx(z)
    else
        //  Falling side: here sigma^2/2tau^2 - u/tau < 0, so no overflow.
        Result := (1 / (2 * tau)) * Exp(sigma * sigma / (2 * tau * tau) - u / tau) *
            Erfc(z);
end;

end.
