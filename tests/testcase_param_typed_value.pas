// SPDX-License-Identifier: GPL-3.0-or-later
{ A curve parameter holds ONE value, and that value carries its own type.

  This replaces a design that had gone wrong: a second string field was bolted on
  beside the double, so a parameter had two values that could disagree, no single
  source of truth, and a parse-the-string guess on load to decide which one
  applied. The guess is the part that actually broke - it works for a GUID, which
  never looks numeric, and fails for a wave label, which is literally "3".

  So the case that matters most here is TEXT THAT LOOKS LIKE A NUMBER. Everything
  else is scaffolding around it. }
unit testcase_param_typed_value;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Variants, Math, fpcunit, testregistry,
  amplitude_curve_parameter, calculated_curve_parameter,
  persistent_curve_parameter_container, persistent_curve_parameters,
  special_curve_parameter;
type
  TParamTypedValueTest = class(TTestCase)
  private
    { A parameter round-tripped through the persistence container, exactly as
      TXMLConfig streams it: value out, value in, kind in. }
    function RoundTrip(const AValue: Variant): Variant;
    { The same, but with NO kind recorded - an older file. }
    function RoundTripWithoutKind(const AValue: Variant): Variant;
  published
    procedure ANumericParameterIsNumeric;
    procedure ATextParameterIsNotNumeric;
    procedure ANonNumericValueReadsAsZeroThroughTheNumericAccessor;
    procedure TheNumericAccessorStillWritesThroughTheSameMember;
    procedure CopyingCarriesTheTypeNotJustTheNumber;

    procedure ANumberSurvivesPersistence;
    procedure AGuidSurvivesPersistence;
    procedure TextThatLooksLikeANumberSurvivesPersistence;
    procedure NegativeAndDecimalTextSurvivePersistence;
    procedure AFileWithoutAKindStillLoadsAsANumber;

    procedure ByNameAccessorsReachTheSameSingleMember;
  end;

implementation

function TParamTypedValueTest.RoundTrip(const AValue: Variant): Variant;
var
  Src, Dst: TCalculatedCurveParameter;
  CSrc, CDst: TPersistentCurveParameterContainer;
  Coll: TCollection;
  Text: string;
  Kind: longint;
begin
  Coll := TCollection.Create(TPersistentCurveParameterContainer);
  try
    Src := TCalculatedCurveParameter.Create;
    Dst := TCalculatedCurveParameter.Create;
    CSrc := TPersistentCurveParameterContainer(Coll.Add);
    CDst := TPersistentCurveParameterContainer(Coll.Add);
    CSrc.Parameter := Src;
    CDst.Parameter := Dst;

    Src.TypedValue := AValue;
    //  Exactly the two published properties TXMLConfig writes and reads.
    Text := CSrc.Value_;
    Kind := CSrc.Kind;

    CDst.Value_ := Text;
    CDst.Kind := Kind;
    Result := Dst.TypedValue;
  finally
    Coll.Free;
  end;
end;

function TParamTypedValueTest.RoundTripWithoutKind(const AValue: Variant): Variant;
var
  Src, Dst: TCalculatedCurveParameter;
  CSrc, CDst: TPersistentCurveParameterContainer;
  Coll: TCollection;
begin
  Coll := TCollection.Create(TPersistentCurveParameterContainer);
  try
    Src := TCalculatedCurveParameter.Create;
    Dst := TCalculatedCurveParameter.Create;
    CSrc := TPersistentCurveParameterContainer(Coll.Add);
    CDst := TPersistentCurveParameterContainer(Coll.Add);
    CSrc.Parameter := Src;
    CDst.Parameter := Dst;

    Src.TypedValue := AValue;
    //  Kind deliberately not assigned: a file written before it existed.
    CDst.Value_ := CSrc.Value_;
    Result := Dst.TypedValue;
  finally
    Coll.Free;
  end;
end;

{ ------------------------------------------------------------------- in memory }

procedure TParamTypedValueTest.ANumericParameterIsNumeric;
var P: TSpecialCurveParameter;
begin
  P := TAmplitudeCurveParameter.Create;
  try
    P.Value := 12.5;
    AssertTrue('a number reports as numeric', P.IsNumeric);
    AssertEquals('and reads back', 12.5, P.Value, 1e-12);
  finally
    P.Free;
  end;
end;

procedure TParamTypedValueTest.ATextParameterIsNotNumeric;
var P: TSpecialCurveParameter;
begin
  P := TCalculatedCurveParameter.Create;
  try
    P.TypedValue := '{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}';
    AssertFalse('text does not report as numeric', P.IsNumeric);
    AssertEquals('and reads back whole',
      '{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}', VarToStr(P.TypedValue));
  finally
    P.Free;
  end;
end;

{ The optimiser reads Value on every iteration. A non-numeric parameter is always
  Calculated - never varied - so it is never used as a quantity; returning 0 beats
  raising a conversion error in the middle of a fit. }
procedure TParamTypedValueTest.ANonNumericValueReadsAsZeroThroughTheNumericAccessor;
var P: TSpecialCurveParameter;
begin
  P := TCalculatedCurveParameter.Create;
  try
    P.TypedValue := 'not a number';
    AssertEquals('reads as zero, does not raise', 0.0, P.Value, 1e-12);
  finally
    P.Free;
  end;
end;

{ One member: writing through the numeric accessor must REPLACE the text, not sit
  beside it. That is the whole point - two slots is what allowed them to disagree. }
procedure TParamTypedValueTest.TheNumericAccessorStillWritesThroughTheSameMember;
var P: TSpecialCurveParameter;
begin
  P := TCalculatedCurveParameter.Create;
  try
    P.TypedValue := 'text';
    AssertFalse('text first', P.IsNumeric);

    P.Value := 7;
    AssertTrue('writing a number makes it numeric', P.IsNumeric);
    AssertEquals('and the text is gone, not shadowed', '7',
      VarToStr(P.TypedValue));
  finally
    P.Free;
  end;
end;

procedure TParamTypedValueTest.CopyingCarriesTheTypeNotJustTheNumber;
var Src, Dst: TSpecialCurveParameter;
begin
  Src := TCalculatedCurveParameter.Create;
  Dst := TCalculatedCurveParameter.Create;
  try
    Src.TypedValue := '{ABC}';
    Src.CopyTo(Dst);
    //  Flattening a copy to a number is how identity would silently vanish
    //  whenever the service handed curves to the client.
    AssertFalse('the copy is still text', Dst.IsNumeric);
    AssertEquals('and identical', '{ABC}', VarToStr(Dst.TypedValue));
  finally
    Src.Free;
    Dst.Free;
  end;
end;

{ ----------------------------------------------------------------- persistence }

procedure TParamTypedValueTest.ANumberSurvivesPersistence;
var V: Variant;
begin
  V := RoundTrip(3.25);
  AssertTrue('still numeric', VarIsNumeric(V));
  AssertEquals('and unchanged', 3.25, double(V), 1e-12);
end;

procedure TParamTypedValueTest.AGuidSurvivesPersistence;
var V: Variant;
begin
  V := RoundTrip('{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}');
  AssertFalse('still text', VarIsNumeric(V));
  AssertEquals('and unchanged',
    '{D4FF48CA-FD58-4EF1-BC9E-7D316B4AE4EB}', VarToStr(V));
end;

{ THE CASE THE OLD DESIGN COULD NOT HANDLE.

  The previous container decided on load by "does this parse as a number?". A
  GUID never does, so identity happened to work. A wave label is "3", which does -
  so it would load back as the number 3 with the text silently lost. That is why
  the kind has to be recorded rather than inferred. }
procedure TParamTypedValueTest.TextThatLooksLikeANumberSurvivesPersistence;
var V: Variant;
begin
  V := RoundTrip('3');
  AssertFalse('"3" stored as text must NOT come back as a number',
    VarIsNumeric(V));
  AssertEquals('it is still the label "3"', '3', VarToStr(V));
end;

procedure TParamTypedValueTest.NegativeAndDecimalTextSurvivePersistence;
var V: Variant;
begin
  //  Other shapes that parse as numbers, in case anyone "optimises" the check
  //  into a digits-only test.
  V := RoundTrip('-1');
  AssertFalse('"-1" stays text', VarIsNumeric(V));
  AssertEquals('"-1"', '-1', VarToStr(V));

  V := RoundTrip('1.5');
  AssertFalse('"1.5" stays text', VarIsNumeric(V));
  AssertEquals('"1.5"', '1.5', VarToStr(V));
end;

{ A settings/model file written before the kind existed carries only the text.
  It must load exactly as it always did - numerically - or every existing saved
  model changes meaning (D2). }
procedure TParamTypedValueTest.AFileWithoutAKindStillLoadsAsANumber;
var V: Variant;
begin
  V := RoundTripWithoutKind(42.5);
  AssertTrue('an older file still loads numerically', VarIsNumeric(V));
  AssertEquals('unchanged', 42.5, double(V), 1e-12);
end;

{ ---------------------------------------------------------------- by-name path }

procedure TParamTypedValueTest.ByNameAccessorsReachTheSameSingleMember;
var
  CP: Curve_parameters;
  P: TSpecialCurveParameter;
  Container: TPersistentCurveParameterContainer;
begin
  CP := Curve_parameters.Create(nil);
  try
    CP.Params.Clear;
    P := TCalculatedCurveParameter.Create;
    P.Name := 'meta';
    Container := TPersistentCurveParameterContainer(CP.Params.Add);
    Container.Parameter := P;

    CP.TypedByName['meta'] := 'abc';
    AssertEquals('typed accessor reads it back', 'abc',
      VarToStr(CP.TypedByName['meta']));
    //  The numeric accessor sees the SAME member - 0 because the value is not a
    //  number, not because there is a separate empty numeric slot.
    AssertEquals('numeric accessor reads the same member', 0.0,
      CP.ValuesByName['meta'], 1e-12);

    CP.ValuesByName['meta'] := 5;
    AssertEquals('and writing numerically replaces it', '5',
      VarToStr(CP.TypedByName['meta']));
  finally
    CP.Free;
  end;
end;

initialization
  RegisterTest('unit', TParamTypedValueTest);
end.
