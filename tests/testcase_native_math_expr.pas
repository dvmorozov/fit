// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the cross-platform user-curve expression engine (native_math_expr),
  the drop-in replacement for the legacy Windows-only 'MathExpr' library. }
unit testcase_native_math_expr;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, native_math_expr;
type
  TNativeMathExprTest = class(TTestCase)
  private
    function Symbols: TStringList;
  published
    procedure DiscoversParametersWithoutValues;
    procedure DiscoversParametersDespitePlaceholderMathError;
    procedure EvaluatesWithParameters;
    procedure EvaluatesBuiltinFunction;
    procedure RejectsInvalidExpression;
    procedure ZeroDenominatorIsFiniteAndDoesNotRaise;
    procedure EvaluatesFiniteAtAcceptsGoodStartingValues;
    procedure EvaluatesFiniteAtRejectsDegenerateStartingValues;
    procedure TranspilesPowerAndFunctionsToNumpy;
    procedure TranspileLeavesIdentifiersAndAbscissaAlone;

    //  The keypad's sixteen. The values are checked against numpy over
    //  expr_fidelity_cases.txt; what is checked here is that each name is
    //  KNOWN - a formula the keypad can build must evaluate, and before these
    //  were registered sixteen of the twenty-three buttons produced one that
    //  did not.
    procedure EveryKeypadFunctionEvaluates;
    procedure TheKeypadNamesAreCaseInsensitiveLikeTheRestOfTheEngine;
    procedure TheReciprocalsAreTheReciprocalsAndNotTheirInverses;
    procedure TranspilesTheKeypadNamesToNumpy;

    //  A parameter that collides with a function name.
    procedure AParameterNamedAfterAFunctionIsRefused;
    procedure AndIsRefusedRatherThanRaising;
    procedure AParameterMerelyStartingWithOneIsFine;

    //  RETRYING A BAD FORMULA. The engine caches on the last expression, and
    //  the retry path used to accept on the second attempt what it had refused
    //  on the first - see the tests for what that cost.
    procedure AMalformedFormulaIsRefusedHoweverOftenItIsRetried;
    procedure AndStillAfterAGoodOneInBetween;
    procedure AGoodFormulaAfterABadOneIsStillAccepted;
  end;

implementation

{ Reads the #0-separated, double-#0-terminated buffer from GetSymbols into a
  TStringList (mirrors how the server walks the list). }
function TNativeMathExprTest.Symbols: TStringList;
var
  buf, p: PChar;
  s: string;
begin
  Result := TStringList.Create;
  buf := GetSymbols;
  try
    p := buf;
    while (p <> nil) and (StrLen(p) <> 0) do
    begin
      s := StrPas(p);
      Result.Add(s);
      p := p + Length(s) + 1;
    end;
  finally
    FreeSymbols(buf);
  end;
end;

procedure TNativeMathExprTest.DiscoversParametersWithoutValues;
var
  r: double;
  code: longint;
  syms: TStringList;
begin
  //  No parameter values supplied -> valid but "has parameters" (-1),
  //  and the identifiers are reported in order of first appearance.
  code := ParseAndCalcExpression('A*x+B', '', @r);
  AssertEquals('return code for undefined params', -1, code);
  syms := Symbols;
  try
    AssertEquals('symbol count', 3, syms.Count);
    AssertEquals('sym0', 'A', syms[0]);
    AssertEquals('sym1', 'x', syms[1]);
    AssertEquals('sym2', 'B', syms[2]);
  finally
    syms.Free;
  end;
end;

procedure TNativeMathExprTest.DiscoversParametersDespitePlaceholderMathError;
var
  r: double;
begin
  //  Discovery evaluates with placeholder zeros; the bell curve divides by w,
  //  so w=0 causes a math error - but the formula is still valid, so parameter
  //  discovery must succeed (-1), not report a syntax error (0). Regression for
  //  the "default formula doesn't work" bug.
  AssertEquals('bell curve discovers as valid-with-params', -1,
    ParseAndCalcExpression('A*exp(-((x-x0)/w)^2)', '', @r));
end;

procedure TNativeMathExprTest.EvaluatesWithParameters;
var
  r: double;
  code: longint;
  params: string;
begin
  //  Parameters as 'name=value'#0 ... terminated by an extra #0.
  params := 'A=2'#0 + 'x=3'#0 + 'B=1'#0 + #0;
  code := ParseAndCalcExpression('A*x+B', PChar(params), @r);
  AssertEquals('fully evaluated code', 1, code);
  AssertEquals('A*x+B = 2*3+1', 7.0, r, 1e-9);
end;

procedure TNativeMathExprTest.EvaluatesBuiltinFunction;
var
  r: double;
  code: longint;
begin
  //  exp() is a built-in, not a parameter -> no undefined identifiers.
  code := ParseAndCalcExpression('exp(0)', '', @r);
  AssertEquals('builtin-only code', 1, code);
  AssertEquals('exp(0) = 1', 1.0, r, 1e-9);
end;

procedure TNativeMathExprTest.RejectsInvalidExpression;
var
  r: double;
begin
  AssertEquals('syntax error -> 0', 0, ParseAndCalcExpression('A*(', '', @r));
end;

procedure TNativeMathExprTest.ZeroDenominatorIsFiniteAndDoesNotRaise;
var
  r: double;
  params: string;
begin
  //  A fit probes bad parameter regions (here w=0, x<>x0 -> division by zero).
  //  Evaluation must not raise (that aborted the fit) and must return a finite
  //  value so the optimizer can move away. Regression for the "fit hangs" bug.
  params := 'A=2'#0 + 'x0=0'#0 + 'w=0'#0 + 'x=116'#0 + #0;
  r := 123;
  ParseAndCalcExpression('A*exp(-((x-x0)/w)^2)', PChar(params), @r);
  AssertFalse('result must be finite (not NaN/Inf)', IsNan(r) or IsInfinite(r));
end;

procedure TNativeMathExprTest.EvaluatesFiniteAtAcceptsGoodStartingValues;
var
  params: string;
begin
  //  The bell curve with a non-zero width evaluates to a finite number at its
  //  starting values -> the creation-time guard accepts it.
  params := 'A=1'#0 + 'x0=0'#0 + 'SIGMA=0.25'#0 + 'x=1'#0 + #0;
  AssertTrue('finite at good starting values',
    EvaluatesFiniteAt('A*exp(-((x-x0)/SIGMA)^2)', PChar(params)));
end;

procedure TNativeMathExprTest.EvaluatesFiniteAtRejectsDegenerateStartingValues;
var
  params: string;
begin
  //  A width left at 0 divides by zero at the starting values -> non-finite, so
  //  the guard reports False and the curve is rejected at creation rather than
  //  fitting from a degenerate shape.
  params := 'A=1'#0 + 'x0=0'#0 + 'w=0'#0 + 'x=1'#0 + #0;
  AssertFalse('non-finite at degenerate starting values',
    EvaluatesFiniteAt('A*exp(-((x-x0)/w)^2)', PChar(params)));
end;

procedure TNativeMathExprTest.TranspilesPowerAndFunctionsToNumpy;
begin
  //  '^' -> '**', and the functions whose numpy name/base differs are remapped.
  AssertEquals('power operator',
    'A*exp(-((x-x0)/w)**2)', ExpressionToNumpy('A*exp(-((x-x0)/w)^2)'));
  AssertEquals('ln -> log (natural)', 'log(x)', ExpressionToNumpy('ln(x)'));
  AssertEquals('log -> log10 (base 10)', 'log10(x)', ExpressionToNumpy('log(x)'));
  AssertEquals('sqr -> square', 'square(x0-x)', ExpressionToNumpy('sqr(x0-x)'));
  AssertEquals('sqrt passes through', 'sqrt(A)', ExpressionToNumpy('sqrt(A)'));
end;

procedure TNativeMathExprTest.TranspileLeavesIdentifiersAndAbscissaAlone;
begin
  //  Identifiers that merely start with a function name, and the abscissa, are
  //  not touched - only name tokens immediately followed by '(' are functions.
  AssertEquals('identifier untouched',
    'expA*x0', ExpressionToNumpy('expA*x0'));
  AssertEquals('abscissa untouched', 'A*x', ExpressionToNumpy('A*x'));
end;


{ ---------------------------- the keypad's sixteen -------------------------- }

procedure TNativeMathExprTest.EveryKeypadFunctionEvaluates;
var
  r: double;
  i: longint;
  params: string;
  Names: array[0..15] of string = (
    'tg', 'ctg', 'sh', 'ch', 'th', 'cth', 'sch', 'csch',
    'arcsin', 'arccos', 'arctg', 'arcctg', 'arsh', 'arch', 'arth', 'arcth');
begin
  //  ONE ARGUMENT INSIDE EVERY DOMAIN AT ONCE: 1.5 is fine for arch (>= 1) and
  //  arcth (|x| > 1), and outside it for arcsin, arccos and arth - so the two
  //  groups are checked with their own values rather than one value that would
  //  quietly produce NaN for half of them and still return code 1.
  for i := 0 to High(Names) do
  begin
    if (Names[i] = 'arcsin') or (Names[i] = 'arccos') or (Names[i] = 'arth') then
      params := 'x=0.5'#0 + #0
    else if (Names[i] = 'arch') or (Names[i] = 'arcth') then
      params := 'x=1.5'#0 + #0
    else
      params := 'x=0.75'#0 + #0;
    AssertEquals(Names[i] + '(x) evaluates', 1,
      ParseAndCalcExpression(PChar(Names[i] + '(x)'), PChar(params), @r));
  end;
end;

procedure TNativeMathExprTest.TheKeypadNamesAreCaseInsensitiveLikeTheRestOfTheEngine;
var
  r: double;
  params: string;
begin
  //  The buttons insert them capitalised - Tg, Arctg, Sh - so the engine has to
  //  accept that spelling or every button is broken however well the lowercase
  //  name is registered.
  params := 'x=0.5'#0 + #0;
  AssertEquals('Tg', 1, ParseAndCalcExpression('Tg(x)', PChar(params), @r));
  AssertEquals('ARCTG', 1, ParseAndCalcExpression('ARCTG(x)', PChar(params), @r));
  AssertEquals('Sh', 1, ParseAndCalcExpression('Sh(x)', PChar(params), @r));
end;

procedure TNativeMathExprTest.TheReciprocalsAreTheReciprocalsAndNotTheirInverses;
var
  r: double;
  params: string;
begin
  //  ctg is 1/tan, not arctan, and cth is 1/tanh, not arctanh. Both mistakes
  //  return a plausible number for a plausible argument, which is why they are
  //  worth an identity rather than a value: a reciprocal times its function is
  //  one, and an inverse times its function is not.
  params := 'x=0.9'#0 + #0;
  AssertEquals('ctg(x)*tg(x)', 1,
    ParseAndCalcExpression('ctg(x)*tg(x)', PChar(params), @r));
  AssertEquals('is one', 1.0, r, 1e-9);
  AssertEquals('cth(x)*th(x)', 1,
    ParseAndCalcExpression('cth(x)*th(x)', PChar(params), @r));
  AssertEquals('is one', 1.0, r, 1e-9);
  AssertEquals('sch(x)*ch(x)', 1,
    ParseAndCalcExpression('sch(x)*ch(x)', PChar(params), @r));
  AssertEquals('is one', 1.0, r, 1e-9);
  AssertEquals('csch(x)*sh(x)', 1,
    ParseAndCalcExpression('csch(x)*sh(x)', PChar(params), @r));
  AssertEquals('is one', 1.0, r, 1e-9);
end;

procedure TNativeMathExprTest.TranspilesTheKeypadNamesToNumpy;
begin
  //  Ten are renamed to numpy's spelling; the six numpy has no name for keep
  //  this engine's, and the sidecar provides them under it. Both directions
  //  matter - an unmapped name passes through UNCHANGED, capitals and all, and
  //  Python is case-sensitive where fpexprpars is not.
  AssertEquals('tg -> tan', 'tan(x)', ExpressionToNumpy('tg(x)'));
  AssertEquals('Tg -> tan', 'tan(x)', ExpressionToNumpy('Tg(x)'));
  AssertEquals('arctg -> arctan', 'arctan(x)', ExpressionToNumpy('arctg(x)'));
  AssertEquals('sh -> sinh', 'sinh(x)', ExpressionToNumpy('sh(x)'));
  AssertEquals('arsh -> arcsinh', 'arcsinh(x)', ExpressionToNumpy('arsh(x)'));
  AssertEquals('arth -> arctanh', 'arctanh(x)', ExpressionToNumpy('arth(x)'));
  //  Kept, because numpy has no name of its own for them.
  AssertEquals('ctg stays ctg', 'ctg(x)', ExpressionToNumpy('ctg(x)'));
  AssertEquals('Ctg is lowered', 'ctg(x)', ExpressionToNumpy('Ctg(x)'));
  AssertEquals('arcctg stays', 'arcctg(x)', ExpressionToNumpy('arcctg(x)'));
end;

{ ------------------- a parameter named after a function --------------------- }

procedure TNativeMathExprTest.AParameterNamedAfterAFunctionIsRefused;
var
  r: double;
begin
  //  A NAME THE ENGINE OWNS CANNOT ALSO BE A PARAMETER. This was reachable
  //  before the keypad's names were registered - a parameter called sin or exp
  //  does it - and became easier to reach with short names like th and ch in
  //  the table. The formula is refused as unusable, which is the same 0 the
  //  caller gets for any formula it cannot evaluate.
  AssertEquals('a formula whose parameter is a function name', 0,
    ParseAndCalcExpression('A*th', '', @r));
end;

procedure TNativeMathExprTest.AndIsRefusedRatherThanRaising;
var
  r: double;
  params: string;
  Raised: boolean;
begin
  //  THE POINT OF THE REFUSAL. Without it the seeding loop asks the parser for
  //  the identifier's value and finds a FUNCTION, and setting a value on one
  //  raises - from inside the loop that runs on every one of the millions of
  //  evaluations a fit makes, with nothing on that path to catch it.
  params := 'th=2'#0 + 'A=3'#0 + #0;
  Raised := False;
  try
    ParseAndCalcExpression('A*th', PChar(params), @r);
  except
    on Exception do
      Raised := True;
  end;
  AssertFalse('nothing escaped', Raised);
end;

procedure TNativeMathExprTest.AParameterMerelyStartingWithOneIsFine;
var
  r: double;
  params: string;
begin
  //  theta is not th, and only a name token immediately followed by '(' is a
  //  function call. A guard that matched on a prefix would refuse most of the
  //  parameter names anybody would type.
  params := 'theta=2'#0 + 'A=3'#0 + #0;
  AssertEquals('evaluated', 1,
    ParseAndCalcExpression('A*theta', PChar(params), @r));
  AssertEquals('3*2', 6.0, r, 1e-9);
end;

{ ------------------------ retrying a malformed formula ---------------------- }

procedure TNativeMathExprTest.AMalformedFormulaIsRefusedHoweverOftenItIsRetried;
var
  r: double;
  i: integer;
begin
  //  PRESSING OK TWICE IS ONE BUTTON PRESS AWAY, and it used to change the
  //  answer. Assigning Expression records the text even when building the tree
  //  from it raises, and the setter does nothing when the text is unchanged -
  //  so the retry re-registered the identifiers, assigned the same text, was
  //  told there was nothing to do, and declared the formula valid with no tree
  //  behind it. The user was shown "could not be understood" once and then
  //  either a bogus curve or a complaint about starting values.
  //
  //  Three attempts, because the first fix that comes to mind - clearing a flag
  //  - makes the second attempt right and leaves the third wrong.
  for i := 1 to 3 do
    AssertEquals(Format('attempt %d is still refused', [i]),
      0, ParseAndCalcExpression('A*exp(-sqr(', '', @r));
end;

procedure TNativeMathExprTest.AndStillAfterAGoodOneInBetween;
var
  r: double;
begin
  //  A DIFFERENT EXPRESSION IN BETWEEN is what a user actually does: type
  //  something wrong, correct it, then paste the wrong one back. The cache key
  //  changes twice, so this exercises the invalidation rather than the flag.
  AssertEquals('refused', 0, ParseAndCalcExpression('A*exp(-sqr(', '', @r));
  AssertTrue('the good one parses',
    ParseAndCalcExpression('A*x', '', @r) <> 0);
  AssertEquals('and the bad one is still refused', 0,
    ParseAndCalcExpression('A*exp(-sqr(', '', @r));
end;

procedure TNativeMathExprTest.AGoodFormulaAfterABadOneIsStillAccepted;
var
  r: double;
begin
  //  THE OTHER DIRECTION, which the fix could have broken: whatever the failed
  //  attempt leaves behind must not make the NEXT formula unparseable, or a
  //  user who mistypes once could never enter a formula again without
  //  restarting.
  AssertEquals('refused', 0, ParseAndCalcExpression('A*exp(-sqr(', '', @r));
  AssertTrue('the correction is accepted',
    ParseAndCalcExpression('A*exp(-sqr((x-x0)/w))', '', @r) <> 0);
end;

initialization
  RegisterTest('unit', TNativeMathExprTest);
end.
