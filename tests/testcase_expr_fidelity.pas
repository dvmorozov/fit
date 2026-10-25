// SPDX-License-Identifier: GPL-3.0-or-later
{ Pascal side of the expression-translation fidelity guarantee.

  For every case in expr_fidelity_cases.txt this asserts, per row:
    * ExpressionToNumpy(pascal) equals the recorded numpy string - the transpiler
      still produces exactly what the Python side is checked against;
    * the native engine (native_math_expr.ParseAndCalcExpression) evaluates the
      Pascal expression to the recorded value - the reference value really is the
      native semantics, so a change in fpexprpars is caught here.

  test_fit_backend.py checks the third leg: numpy evaluates the numpy string to the
  same value. Together the three legs prove native(pascal) == numpy(translate(pascal))
  for the whole covered surface (operator precedence/associativity, unary minus,
  ln/log base, sqr, pass-through functions, case-insensitivity, the pi constant, and
  identifiers that merely start with a function name). }
unit testcase_expr_fidelity;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, native_math_expr;

type
  TExprFidelityTest = class(TTestCase)
  published
    procedure TranslationPreservesNativeValue;
  end;

implementation

function FixturePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'expr_fidelity_cases.txt';
end;

{ The #0-separated, double-#0-terminated 'name=value' list ParseAndCalcExpression
  expects, from a 'a=1,b=2' string (empty when no parameters). }
function ParamList(const AParams: string): string;
var parts: TStringArray; i: integer;
begin
  Result := '';
  if AParams <> '' then
  begin
    parts := AParams.Split(',');
    for i := 0 to High(parts) do
      Result := Result + parts[i] + #0;
  end;
  Result := Result + #0;
end;

procedure TExprFidelityTest.TranslationPreservesNativeValue;
var
  Lines: TStringList;
  Line, Pascal, Numpy, Params: string;
  Fields: TStringArray;
  Expected, Native: double;
  Code, i, Checked: longint;
begin
  AssertTrue('fixture exists: ' + FixturePath, FileExists(FixturePath));
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FixturePath);
    Checked := 0;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if (Line = '') or (Line[1] = '#') then
        Continue;

      Fields := Line.Split([' ;; ']);
      AssertEquals('row has 4 fields: ' + Line, 4, Length(Fields));
      Pascal   := Trim(Fields[0]);
      Numpy    := Trim(Fields[1]);
      Params   := Trim(Fields[2]);
      Expected := StrToFloat(Trim(Fields[3]));

      //  Leg 1: the transpiler still emits exactly the numpy string under test.
      AssertEquals('transpile ' + Pascal, Numpy, ExpressionToNumpy(Pascal));

      //  Leg 2: the native engine really evaluates the Pascal form to Expected.
      Native := 0;
      Code := ParseAndCalcExpression(PChar(Pascal), PChar(ParamList(Params)), @Native);
      AssertEquals('native evaluable: ' + Pascal, 1, Code);
      AssertEquals('native value: ' + Pascal, Expected, Native, 1e-6);

      Inc(Checked);
    end;
    //  Guard against a silently empty/for-mangled fixture passing vacuously.
    AssertTrue('fixture had cases', Checked >= 20);
  finally
    Lines.Free;
  end;
end;

initialization
  RegisterTest('integration', TExprFidelityTest);
end.
