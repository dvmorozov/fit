// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_argument_axis;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, argument_axis;
type
  TArgumentAxisTest = class(TTestCase)
  published
    procedure Identity;
    procedure DiffractionTwoTheta;
    procedure DiffractionTheta;
    procedure DiffractionSinThetaOverLambda;
    procedure ExpressionAxisLinear;
    procedure ExpressionAxisLogarithmic;

    //  WHAT A TYPED WAVELENGTH HAS TO BE. The sin(theta)/lambda axis divides by
    //  it, and both refusals used to live in an LCL menu handler with a
    //  MessageDlg each, reachable only by typing into the box.
    procedure APlainWavelengthIsAccepted;
    procedure ANonNumberIsRefusedWithTheSeparatorNamed;
    procedure ZeroIsRefusedBecauseItAlreadyMeansNotSet;
    procedure ANegativeWavelengthIsRefused;
    procedure ARefusedValueComesBackAsZeroRatherThanHalfParsed;
    procedure AnAcceptedValueCarriesNoRefusal;
  end;
implementation

procedure TArgumentAxisTest.Identity;
var ax: TIdentityAxis;
begin
  ax := TIdentityAxis.Create;
  try
    AssertEquals('to-display identity', 17.3, ax.ToDisplay(17.3), 1e-12);
    AssertEquals('from-display identity', 17.3, ax.FromDisplay(17.3), 1e-12);
    AssertEquals('name', 'Position', ax.DisplayName);
  finally ax.Free; end;
end;

procedure TArgumentAxisTest.DiffractionTwoTheta;
var ax: TDiffractionAngleAxis;
begin
  ax := TDiffractionAngleAxis.Create(dmTwoTheta, 1.5);
  try
    AssertEquals('2theta to-display', 30.0, ax.ToDisplay(30.0), 1e-12);
    AssertEquals('2theta from-display', 30.0, ax.FromDisplay(30.0), 1e-12);
  finally ax.Free; end;
end;

procedure TArgumentAxisTest.DiffractionTheta;
var ax: TDiffractionAngleAxis;
begin
  ax := TDiffractionAngleAxis.Create(dmTheta, 1.5);
  try
    AssertEquals('theta = 2theta/2', 15.0, ax.ToDisplay(30.0), 1e-12);
    AssertEquals('inverse theta', 30.0, ax.FromDisplay(15.0), 1e-12);
  finally ax.Free; end;
end;

procedure TArgumentAxisTest.DiffractionSinThetaOverLambda;
var ax: TDiffractionAngleAxis; d: double;
begin
  ax := TDiffractionAngleAxis.Create(dmSinThetaOverLambda, 1.5);
  try
    // 2theta=30 -> sin(15deg)/1.5 = 0.2588190/1.5 = 0.1725460
    d := ax.ToDisplay(30.0);
    AssertEquals('sin(theta)/lambda', 0.1725460, d, 1e-6);
    // round-trips back to the raw 2theta
    AssertEquals('round-trip to 2theta', 30.0, ax.FromDisplay(d), 1e-9);
  finally ax.Free; end;
end;

procedure TArgumentAxisTest.ExpressionAxisLinear;
var ax: TExpressionAxis;
begin
  //  display = 2*x + 1 ; inverse = (x - 1)/2
  ax := TExpressionAxis.Create('Scaled', 'u', '2*x+1', '(x-1)/2');
  try
    AssertEquals('name', 'Scaled', ax.DisplayName);
    AssertEquals('unit', 'u', ax.UnitName);
    AssertEquals('to-display 2*3+1', 7.0, ax.ToDisplay(3.0), 1e-9);
    AssertEquals('inverse of 7', 3.0, ax.FromDisplay(7.0), 1e-9);
  finally ax.Free; end;
end;

procedure TArgumentAxisTest.ExpressionAxisLogarithmic;
var ax: TExpressionAxis; d: double;
begin
  //  display = ln(x) ; inverse = exp(x)
  ax := TExpressionAxis.Create('Log', '', 'ln(x)', 'exp(x)');
  try
    d := ax.ToDisplay(10.0);
    AssertEquals('ln(10)', 2.302585, d, 1e-6);
    AssertEquals('round-trip via exp', 10.0, ax.FromDisplay(d), 1e-9);
  finally ax.Free; end;
end;

{ ---- what a typed wavelength has to be ------------------------------------- }

procedure TArgumentAxisTest.APlainWavelengthIsAccepted;
var V: double; R: string;
begin
    AssertTrue('accepted', WavelengthFromText('1.5406', V, R));
    AssertEquals('the value', 1.5406, V, 1e-12);
end;

procedure TArgumentAxisTest.ANonNumberIsRefusedWithTheSeparatorNamed;
var V: double; R: string;
begin
    //  A comma is what a great many keyboards and locales produce, so the
    //  refusal is a mystery unless it says which separator is wanted.
    AssertFalse('refused', WavelengthFromText('1,5406', V, R));
    AssertTrue('the separator is named', Pos('full stop', R) > 0);
    AssertTrue('an example is given', Pos('1.5406', R) > 0);
end;

procedure TArgumentAxisTest.ZeroIsRefusedBecauseItAlreadyMeansNotSet;
var V: double; R: string;
begin
    //  THE ONE THAT WOULD FAIL SILENTLY. Zero is what the client already reads
    //  as "no wavelength", so accepting it would do nothing at all - and the
    //  axis it is for divides by it.
    AssertFalse('refused', WavelengthFromText('0', V, R));
    AssertTrue('and says why', Pos('greater than zero', R) > 0);
end;

procedure TArgumentAxisTest.ANegativeWavelengthIsRefused;
var V: double; R: string;
begin
    AssertFalse('refused', WavelengthFromText('-1.5', V, R));
end;

procedure TArgumentAxisTest.ARefusedValueComesBackAsZeroRatherThanHalfParsed;
var V: double; R: string;
begin
    //  The caller shows the message and returns; if it ever did read the value,
    //  it must not be a partially converted one.
    V := 99;
    AssertFalse('refused', WavelengthFromText('rubbish', V, R));
    AssertEquals('cleared', 0, V, 1e-12);
end;

procedure TArgumentAxisTest.AnAcceptedValueCarriesNoRefusal;
var V: double; R: string;
begin
    //  So that a caller which shows R unconditionally shows nothing.
    R := 'left over';
    AssertTrue('accepted', WavelengthFromText('2', V, R));
    AssertEquals('no message', '', R);
end;

initialization
  RegisterTest('unit', TArgumentAxisTest);
end.
