// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_lineshapes;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testregistry, SimpMath;
type
  { Characterization of the XRD/neutron peak-shape math (area-normalized A). }
  TLineShapeTest = class(TTestCase)
  published
    procedure Gaussian;
    procedure Lorentzian;
    procedure PseudoVoigt;
  end;
implementation

procedure TLineShapeTest.Gaussian;
begin
  // Peak = A / (sigma * sqrt(2*pi)); A=1, sigma=2 -> 0.1994711
  AssertEquals('gauss peak', 0.1994711, GaussPoint(1.0, 2.0, 10.0, 10.0), 1e-7);
  // At x0 + sigma -> peak * exp(-0.5)
  AssertEquals('gauss at +sigma', 0.1209854, GaussPoint(1.0, 2.0, 10.0, 12.0), 1e-7);
  // Symmetric about x0
  AssertEquals('gauss symmetry', GaussPoint(1.0, 2.0, 10.0, 7.3),
    GaussPoint(1.0, 2.0, 10.0, 12.7), 1e-12);
end;

procedure TLineShapeTest.Lorentzian;
begin
  // FWHM = sigma; peak = 2A/(pi*sigma); A=1, sigma=2 -> 1/pi
  AssertEquals('lorentz peak', 0.3183099, LorentzPoint(1.0, 2.0, 10.0, 10.0), 1e-7);
  // At x0 + sigma/2 (half-width) value is half the peak (confirms FWHM=sigma)
  AssertEquals('lorentz half-max', 0.1591549, LorentzPoint(1.0, 2.0, 10.0, 11.0), 1e-7);
end;

procedure TLineShapeTest.PseudoVoigt;
begin
  // eta = 1 -> pure Lorentzian component; peak = 2A/(pi*sigma)
  AssertEquals('pv eta=1 peak == lorentz', 0.3183099,
    PseudoVoigtPoint(1.0, 2.0, 1.0, 10.0, 10.0), 1e-7);
  // eta = 0 -> Gaussian (FWHM=sigma) component; peak = A*2*sqrt(ln2)/(sigma*sqrt(pi))
  AssertEquals('pv eta=0 peak', 0.4697186,
    PseudoVoigtPoint(1.0, 2.0, 0.0, 10.0, 10.0), 1e-7);
end;

initialization
  RegisterTest('unit', TLineShapeTest);
end.
