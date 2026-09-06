// SPDX-License-Identifier: GPL-3.0-or-later
{ Pins the answer Curve_parameters.IndexOfFirstVarying gives when there is no
  first varying parameter to give.

  THE CASE THIS EXISTS FOR. The fit asks "which shared parameter do I vary
  first?" at the start of every run, and for an ordinary fit the answer is
  "none" - there are no shared parameters at all. That answer used to come from
  a loop with a Break whose loop variable was read after the loop, which the
  language does not define, and which an empty list never assigns at all. The
  leftover was a number failing the callers' `index < Count` guard on x86-64, so
  the fit ran and every test passed; on Apple Silicon and on Windows it was a
  large NEGATIVE number, which PASSES that guard, and every single test that
  fitted anything died with "List index (-1431655779) out of bounds".

  So the answer is not "whatever is left over" but Count, and these tests say so
  for the three ways of having no first varying parameter. They do not reproduce
  the old defect - nothing portable can, since its symptom was whatever the
  machine happened to leave behind - but they stop the ambiguity coming back in
  a rewrite, and they run on every platform rather than only the unlucky ones. }
unit testcase_first_varying_parameter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  persistent_curve_parameters, persistent_curve_parameter_container,
  special_curve_parameter, amplitude_curve_parameter;

type
  TFirstVaryingParameterTest = class(TTestCase)
  private
    FParams: Curve_parameters;
    procedure Add(const AName: string; ADisabled: boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AnEmptyListHasNoFirstVaryingParameter;
    procedure AllDisabledHasNoFirstVaryingParameter;
    procedure TheFirstEnabledOneIsFound;
    procedure DisabledOnesBeforeItAreSkipped;
    procedure TheAnswerIsAlwaysAUsableIndexOrTheCount;
  end;

implementation

procedure TFirstVaryingParameterTest.SetUp;
begin
  FParams := Curve_parameters.Create(nil);
  //  Create seeds one placeholder parameter; these tests state their own list.
  FParams.Params.Clear;
end;

procedure TFirstVaryingParameterTest.TearDown;
begin
  FreeAndNil(FParams);
end;

procedure TFirstVaryingParameterTest.Add(const AName: string; ADisabled: boolean);
var
  Prm: TSpecialCurveParameter;
  Cont: TPersistentCurveParameterContainer;
begin
  Prm := TAmplitudeCurveParameter.Create;
  Prm.Name := AName;
  Prm.Type_ := Variable;
  Prm.Value := 1;
  Prm.VariationDisabled := ADisabled;
  Cont := TPersistentCurveParameterContainer(FParams.Params.Add);
  Cont.Parameter := Prm;
end;

{ The ordinary fit: no shared parameters at all. The loop body never runs, so
  this is the case that used to answer with a variable nothing had assigned. }
procedure TFirstVaryingParameterTest.AnEmptyListHasNoFirstVaryingParameter;
begin
  AssertEquals('an empty list answers with its count, not a leftover',
    0, FParams.IndexOfFirstVarying);
end;

{ The other way of having no answer: parameters exist, none may vary. Here the
  loop runs to completion without breaking, which the language leaves just as
  undefined as never running at all. }
procedure TFirstVaryingParameterTest.AllDisabledHasNoFirstVaryingParameter;
begin
  Add('A', True);
  Add('B', True);
  Add('C', True);
  AssertEquals('all disabled answers with the count',
    3, FParams.IndexOfFirstVarying);
end;

procedure TFirstVaryingParameterTest.TheFirstEnabledOneIsFound;
begin
  Add('A', False);
  Add('B', False);
  AssertEquals('the first one may vary', 0, FParams.IndexOfFirstVarying);
end;

procedure TFirstVaryingParameterTest.DisabledOnesBeforeItAreSkipped;
begin
  Add('A', True);
  Add('B', True);
  Add('C', False);
  Add('D', False);
  AssertEquals('the first one that may vary is found past the disabled ones',
    2, FParams.IndexOfFirstVarying);
end;

{ The property the callers actually depend on, stated directly: the answer is
  never negative. Every guard downstream is written as `index < Count`, which a
  negative number passes - so "not negative" is the whole of what kept the fit
  off the wrong end of the collection. }
procedure TFirstVaryingParameterTest.TheAnswerIsAlwaysAUsableIndexOrTheCount;
var
  i: integer;
begin
  for i := 0 to 3 do
  begin
    AssertTrue(Format('with %d disabled parameters the answer is not negative', [i]),
      FParams.IndexOfFirstVarying >= 0);
    AssertTrue(Format('with %d disabled parameters the answer is at most the count', [i]),
      FParams.IndexOfFirstVarying <= FParams.Count);
    Add('P' + IntToStr(i), True);
  end;
end;

initialization
  RegisterTest('unit', TFirstVaryingParameterTest);
end.
