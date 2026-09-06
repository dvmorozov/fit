// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL.

@abstract(Cross-platform expression engine for user-defined curves.)

A drop-in replacement for the legacy Windows-only 'MathExpr' shared library.
It replicates that library's tiny C API - ParseAndCalcExpression / GetSymbols /
FreeSymbols - on top of FPC's built-in TFPExpressionParser (fcl-base), so
user-defined curve expressions work on every platform with no external
dependency (route 2 of the argument/curve expression plan).

Contract (as expected by the existing callers):
 * ParseAndCalcExpression(Expr, ParamList, Result):
     - ParamList is a list of 'name=value' entries, each terminated by #0, the
       whole list terminated by an empty entry (an extra #0).
     - Returns 1 when the expression is valid and every identifier it uses was
       supplied a value; -1 when valid but some identifier was left undefined
       (used to discover the parameters of a freshly typed expression); 0 on a
       syntax/evaluation error.
 * GetSymbols returns the identifiers found in the last parse as a #0-separated,
   double-#0-terminated buffer (caller frees it with FreeSymbols).
}
unit native_math_expr;

{$mode objfpc}{$H+}

interface

{ Signatures match the legacy external 'MathExpr' routines so call sites are
  unchanged apart from swapping the external declaration for this unit. }
function ParseAndCalcExpression(Expr: PChar; ParamList: PChar;
    ResultValue: PDouble): longint;
{ Validation helper: evaluates Expr with the supplied 'name=value' ParamList and
  reports whether the result is a finite number. Unlike ParseAndCalcExpression
  (which maps a non-finite result to 0 so a running fit never aborts), this keeps
  the raw finiteness so callers can reject a curve that cannot be evaluated at its
  starting values (e.g. a width that starts at 0 producing a division by zero).
  Returns True only when the formula is valid, every identifier was supplied a
  value, and the result is finite. }
function EvaluatesFiniteAt(Expr: PChar; ParamList: PChar): boolean;
function GetSymbols: PChar;
procedure FreeSymbols(Symbols: PChar);
{ Translates a user expression from this engine's syntax (fpexprpars) to the
  numpy syntax the Python compute backend evaluates, so a user curve fits
  under the Python minimizer too. Token-aware (function names vs identifiers):
  the '^' power operator becomes '**', and the functions whose numpy name or base
  differs are remapped (ln->log, log->log10, sqr->square); other names and the
  abscissa 'x' pass through unchanged.

  Correctness is pinned by a three-legged differential test over
  tests/expr_fidelity_cases.txt: testcase_expr_fidelity.pas checks this function's
  output and the native value of each case, test_fit_backend.py checks numpy
  evaluates the translated string to that same value. There is no way to make both
  pass with a translation that changes a value. }
function ExpressionToNumpy(const Expr: string): string;

implementation

uses
    SysUtils, Classes, Math, fpexprpars, special_functions;

{ fpexprpars callbacks for the special functions the parser does not provide.
  They mirror the sidecar's scipy.special entries (the parity rule). }
procedure ExprErf(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Erf(ArgToFloat(Args[0]));
end;

procedure ExprErfc(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Erfc(ArgToFloat(Args[0]));
end;

procedure ExprErfcx(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Erfcx(ArgToFloat(Args[0]));
end;

{ voigt(u, sigma, gamma) = area-normalised Voigt profile, matching
  scipy.special.voigt_profile in the sidecar. }
procedure ExprVoigt(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := VoigtProfile(ArgToFloat(Args[0]), ArgToFloat(Args[1]),
        ArgToFloat(Args[2]));
end;

{ emg(u, sigma, tau) = area-normalised exponentially modified Gaussian, mirroring
  the sidecar's emg (built on scipy). }
procedure ExprEmg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := EmgProfile(ArgToFloat(Args[0]), ArgToFloat(Args[1]),
        ArgToFloat(Args[2]));
end;

{ THE KEYPAD'S OWN VOCABULARY.

  The user-defined curve dialog draws a keypad of twenty-three function buttons,
  and each inserts its own name. Sixteen of those names meant nothing to this
  engine - fpexprpars knows cos, sin, arctan, abs, sqr, sqrt, exp, ln, log,
  frac, int, round and trunc, and nothing else - so pressing a labelled key on a
  keypad the program itself drew produced a formula that would not evaluate,
  reported as an invalid expression with nothing pointing at the button. Tg and
  Arctg were among them: the plain tangent, which this engine spells tan.

  They are registered here rather than relabelled on the keypad because these
  are the names the notation uses in this field, and because six of them - the
  reciprocals and the two inverse cotangents - have no single name on either
  side and would otherwise have to be written out by hand in every formula.

  EACH IS MIRRORED IN THE SIDECAR, and the mirror is what makes the choice of
  definition load-bearing rather than cosmetic: ctg is 1/tan on both sides
  rather than FPC's cotan, arcctg is pi/2 - arctan on both, and arcth is
  arctanh(1/x) on both. Worker/py/lineshapes.py carries the same six, and
  tests/expr_fidelity_cases.txt evaluates all sixteen through both. }
procedure ExprTg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Tan(ArgToFloat(Args[0]));
end;

procedure ExprCtg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    //  1/tan rather than FPC's cotan, because 1/np.tan is what the sidecar can
    //  write and the two must not differ in the last bits.
    Result.ResFloat := 1.0 / Tan(ArgToFloat(Args[0]));
end;

procedure ExprSh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Sinh(ArgToFloat(Args[0]));
end;

procedure ExprCh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Cosh(ArgToFloat(Args[0]));
end;

procedure ExprTh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := Tanh(ArgToFloat(Args[0]));
end;

procedure ExprCth(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := 1.0 / Tanh(ArgToFloat(Args[0]));
end;

procedure ExprSch(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := 1.0 / Cosh(ArgToFloat(Args[0]));
end;

procedure ExprCsch(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := 1.0 / Sinh(ArgToFloat(Args[0]));
end;

procedure ExprArcsin(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcSin(ArgToFloat(Args[0]));
end;

procedure ExprArccos(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcCos(ArgToFloat(Args[0]));
end;

procedure ExprArctg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcTan(ArgToFloat(Args[0]));
end;

procedure ExprArcctg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    //  The principal branch, range (0, pi) - continuous through zero, which the
    //  arctan(1/x) form is not.
    Result.ResFloat := Pi / 2 - ArcTan(ArgToFloat(Args[0]));
end;

procedure ExprArsh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcSinh(ArgToFloat(Args[0]));
end;

procedure ExprArch(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcCosh(ArgToFloat(Args[0]));
end;

procedure ExprArth(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    Result.ResFloat := ArcTanh(ArgToFloat(Args[0]));
end;

procedure ExprArcth(var Result: TFPExpressionResult; const Args: TExprParameterArray);
begin
    //  Inverse hyperbolic cotangent, defined for |x| > 1.
    Result.ResFloat := ArcTanh(1.0 / ArgToFloat(Args[0]));
end;

var
    GParser:  TFPExpressionParser = nil;
    GSymbols: TStringList = nil;   //  identifiers found in the last parse
    GDotFmt:  TFormatSettings;
    GBuiltinsRegistered: boolean = false;
    //  Cache so a fit (millions of evaluations of one unchanged formula) does not
    //  re-scan / re-register / re-parse on every call - only values are updated.
    GLastExpr: string = #1;   //  sentinel: never equals a real expression
    GLastExprValid: boolean = false;

procedure EnsureEngine;
begin
    if not GBuiltinsRegistered then
    begin
        //  The global builtins manager may already be populated; only register
        //  the standard set when it is empty to avoid duplicate-identifier errors.
        if BuiltinIdentifiers.IndexOfIdentifier('pi') < 0 then
            RegisterStdBuiltins(BuiltinIdentifiers);
        GBuiltinsRegistered := True;
    end;
    if GParser = nil then
    begin
        GParser := TFPExpressionParser.Create(nil);
        GParser.BuiltIns := AllBuiltIns;
        //  Special functions fpexprpars lacks, mirrored from scipy in the sidecar.
        GParser.Identifiers.AddFunction('erf', 'F', 'F', @ExprErf);
        GParser.Identifiers.AddFunction('erfc', 'F', 'F', @ExprErfc);
        GParser.Identifiers.AddFunction('erfcx', 'F', 'F', @ExprErfcx);
        GParser.Identifiers.AddFunction('voigt', 'F', 'FFF', @ExprVoigt);
        GParser.Identifiers.AddFunction('emg', 'F', 'FFF', @ExprEmg);
        //  The keypad's sixteen. See the block comment above them; every one
        //  has a twin in the sidecar's _SYMBOLS.
        GParser.Identifiers.AddFunction('tg', 'F', 'F', @ExprTg);
        GParser.Identifiers.AddFunction('ctg', 'F', 'F', @ExprCtg);
        GParser.Identifiers.AddFunction('sh', 'F', 'F', @ExprSh);
        GParser.Identifiers.AddFunction('ch', 'F', 'F', @ExprCh);
        GParser.Identifiers.AddFunction('th', 'F', 'F', @ExprTh);
        GParser.Identifiers.AddFunction('cth', 'F', 'F', @ExprCth);
        GParser.Identifiers.AddFunction('sch', 'F', 'F', @ExprSch);
        GParser.Identifiers.AddFunction('csch', 'F', 'F', @ExprCsch);
        GParser.Identifiers.AddFunction('arcsin', 'F', 'F', @ExprArcsin);
        GParser.Identifiers.AddFunction('arccos', 'F', 'F', @ExprArccos);
        GParser.Identifiers.AddFunction('arctg', 'F', 'F', @ExprArctg);
        GParser.Identifiers.AddFunction('arcctg', 'F', 'F', @ExprArcctg);
        GParser.Identifiers.AddFunction('arsh', 'F', 'F', @ExprArsh);
        GParser.Identifiers.AddFunction('arch', 'F', 'F', @ExprArch);
        GParser.Identifiers.AddFunction('arth', 'F', 'F', @ExprArth);
        GParser.Identifiers.AddFunction('arcth', 'F', 'F', @ExprArcth);
    end;
    if GSymbols = nil then
        GSymbols := TStringList.Create;
end;

function IsIdentStart(C: char): boolean; inline;
begin
    Result := (C in ['A'..'Z', 'a'..'z', '_']);
end;

function IsIdentChar(C: char): boolean; inline;
begin
    Result := (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
end;

{ Collects the identifiers used as variables in Expr (in order of first
  appearance): name tokens that are not immediately followed by '(' (those are
  function calls) and that are not built-in identifiers. }
procedure ScanIdentifiers(const Expr: string; Names: TStringList);
var
    i, n, j, Start: integer;
    Tok: string;
begin
    Names.Clear;
    n := Length(Expr);
    i := 1;
    while i <= n do
    begin
        if IsIdentStart(Expr[i]) then
        begin
            Start := i;
            while (i <= n) and IsIdentChar(Expr[i]) do
                Inc(i);
            Tok := Copy(Expr, Start, i - Start);
            //  Is a '(' next (ignoring spaces)?  Then it is a function name.
            j := i;
            while (j <= n) and (Expr[j] in [' ', #9]) do
                Inc(j);
            if (j <= n) and (Expr[j] = '(') then
                Continue;
            if BuiltinIdentifiers.FindIdentifier(Tok) <> nil then
                Continue;
            if Names.IndexOf(Tok) < 0 then
                Names.Add(Tok);
        end
        else
            Inc(i);
    end;
end;

{ The numpy spelling for a function token from this engine, or '' when the
  name is not a recognized function (then the caller passes it through unchanged).
  Case-insensitive, matching fpexprpars' identifier handling. }
function NumpyFuncName(const Tok: string): string;
var
    L: string;
begin
    L := LowerCase(Tok);
    if L = 'ln' then Result := 'log'          //  fpexprpars ln = natural log
    else if L = 'log' then Result := 'log10'  //  fpexprpars log = base 10
    else if L = 'sqr' then Result := 'square'
    else if L = 'exp' then Result := 'exp'
    else if L = 'sqrt' then Result := 'sqrt'
    else if L = 'abs' then Result := 'abs'
    else if L = 'sin' then Result := 'sin'
    else if L = 'cos' then Result := 'cos'
    else if L = 'tan' then Result := 'tan'
    else if L = 'arctan' then Result := 'arctan'
    else if L = 'erf' then Result := 'erf'      //  same name in scipy.special
    else if L = 'erfc' then Result := 'erfc'
    else if L = 'erfcx' then Result := 'erfcx'
    else if L = 'voigt' then Result := 'voigt'
    else if L = 'emg' then Result := 'emg'
    //  The keypad's sixteen. Ten have a numpy name and are renamed to it; the
    //  six with none keep this engine's spelling, which the sidecar provides
    //  under that same name. Every one is listed rather than left to fall
    //  through as '' - an unmapped name passes through UNCHANGED, so Tg typed
    //  with a capital T would reach Python as Tg, and Python is case-sensitive
    //  where fpexprpars is not.
    else if L = 'tg' then Result := 'tan'
    else if L = 'arctg' then Result := 'arctan'
    else if L = 'sh' then Result := 'sinh'
    else if L = 'ch' then Result := 'cosh'
    else if L = 'th' then Result := 'tanh'
    else if L = 'arcsin' then Result := 'arcsin'
    else if L = 'arccos' then Result := 'arccos'
    else if L = 'arsh' then Result := 'arcsinh'
    else if L = 'arch' then Result := 'arccosh'
    else if L = 'arth' then Result := 'arctanh'
    else if L = 'ctg' then Result := 'ctg'
    else if L = 'cth' then Result := 'cth'
    else if L = 'sch' then Result := 'sch'
    else if L = 'csch' then Result := 'csch'
    else if L = 'arcctg' then Result := 'arcctg'
    else if L = 'arcth' then Result := 'arcth'
    else Result := '';
end;

function ExpressionToNumpy(const Expr: string): string;
var
    i, n, j, Start: integer;
    Tok, Mapped: string;
begin
    Result := '';
    n := Length(Expr);
    i := 1;
    while i <= n do
    begin
        if IsIdentStart(Expr[i]) then
        begin
            Start := i;
            while (i <= n) and IsIdentChar(Expr[i]) do
                Inc(i);
            Tok := Copy(Expr, Start, i - Start);
            //  A '(' next (ignoring spaces) marks a function call; only then remap
            //  the name. Bare identifiers (parameters, the abscissa x) pass through.
            j := i;
            while (j <= n) and (Expr[j] in [' ', #9]) do
                Inc(j);
            if (j <= n) and (Expr[j] = '(') then
            begin
                Mapped := NumpyFuncName(Tok);
                if Mapped <> '' then
                    Tok := Mapped;
            end;
            Result := Result + Tok;
        end
        else if Expr[i] = '^' then
        begin
            Result := Result + '**';
            Inc(i);
        end
        else
        begin
            Result := Result + Expr[i];
            Inc(i);
        end;
    end;
end;

function ParseAndCalcExpression(Expr: PChar; ParamList: PChar;
    ResultValue: PDouble): longint;
var
    ExprStr, nm, vv: string;
    P: PChar;
    Entry: string;
    i, eq: integer;
    d: double;
    ProvidedCount: integer;
    AllProvided: boolean;
    Def: TFPExprIdentifierDef;
    OldMask: TFPUExceptionMask;
    Idx: integer;
    Clash: boolean;
begin
    EnsureEngine;
    Result  := 0;
    ExprStr := StrPas(Expr);

    //  Re-scan, re-register and re-parse only when the formula changes. During a
    //  fit the same formula is evaluated millions of times, so this must be cheap.
    if ExprStr <> GLastExpr then
    begin
        GLastExpr := ExprStr;
        GLastExprValid := False;
        ScanIdentifiers(ExprStr, GSymbols);
        //  A NAME THIS ENGINE ALREADY OWNS CANNOT ALSO BE A PARAMETER, and the
        //  formula is refused rather than half-bound. IdentifierByName below
        //  would otherwise hand back the FUNCTION definition and setting AsFloat
        //  on it raises - from inside the loop that seeds parameter values,
        //  which runs on every one of the millions of evaluations a fit makes,
        //  and which nothing here catches.
        //
        //  This has been reachable since the engine was written (a parameter
        //  called sin, exp or pi does it) and became easier to reach when the
        //  keypad's short names - tg, sh, ch, th - were registered. Refusing
        //  gives the caller the same 0 it gives any unusable formula, which the
        //  user meets as "the expression is not valid".
        Clash := False;
        for i := 0 to GSymbols.Count - 1 do
        begin
            Idx := GParser.Identifiers.IndexOfIdentifier(GSymbols[i]);
            if Idx < 0 then
                GParser.Identifiers.AddFloatVariable(GSymbols[i], 0)
            else if GParser.Identifiers[Idx].IdentifierType <> itVariable then
                Clash := True;
        end;
        //  SetExpression builds + type-checks the tree (no maths); a failure here
        //  is a genuine syntax/type error in the formula.
        try
            if Clash then
                GLastExpr := #1   //  never valid; re-examined if it is retyped
            else
            begin
                GParser.Expression := ExprStr;
                GLastExprValid := True;
            end;
        except
            GLastExpr := #1;   //  force a fresh attempt next time
            //  AND CLEAR THE UNDERLYING PARSER, which is the half that was
            //  missing. Assigning Expression records the text even when
            //  building the tree from it raises, and the property setter does
            //  nothing when the text has not changed - so the "fresh attempt"
            //  above re-registered the identifiers, assigned the SAME text,
            //  was told there was nothing to do, and declared the formula
            //  valid with no tree behind it.
            //
            //  WHAT THAT COST THE USER: a malformed formula was reported as
            //  malformed once and then accepted. Press OK a second time on the
            //  same text and the discovery ran on whatever the scan had found,
            //  producing either a curve that cannot be evaluated - reported as
            //  "the formula cannot be evaluated at its starting values", which
            //  sends them to change numbers that were never the problem - or
            //  nothing at all. Deterministic, and reachable by pressing one
            //  button twice.
            //
            //  '0' rather than '': a constant always parses, so the next real
            //  attempt is a genuine change and is really re-parsed.
            try
                GParser.Expression := '0';
            except
                //  Nothing left to do about it here; the sentinel above
                //  already guarantees another attempt.
            end;
        end;
    end;

    if not GLastExprValid then
        Exit;   //  Result = 0: invalid formula

    //  Default every parameter to 0, then apply the supplied values. Counting how
    //  many of the formula's identifiers were supplied tells full (1) from
    //  parameter-discovery (-1) calls.
    for i := 0 to GSymbols.Count - 1 do
        GParser.Identifiers.IdentifierByName(GSymbols[i]).AsFloat := 0;

    ProvidedCount := 0;
    if ParamList <> nil then
    begin
        P := ParamList;
        while P^ <> #0 do
        begin
            Entry := StrPas(P);
            Inc(P, Length(Entry) + 1);
            eq := Pos('=', Entry);
            if eq > 0 then
            begin
                nm := Copy(Entry, 1, eq - 1);
                vv := Copy(Entry, eq + 1, MaxInt);
                Def := GParser.Identifiers.FindIdentifier(nm);
                //  A variable, not a function of the same name: the scan above
                //  refuses such a formula, so this can only be a caller passing
                //  a value for a name that is not a parameter at all.
                if (Def <> nil) and (Def.IdentifierType = itVariable) then
                begin
                    if not TryStrToFloat(vv, d, GDotFmt) then
                        if not TryStrToFloat(vv, d) then
                            d := 0;
                    Def.AsFloat := d;
                    Inc(ProvidedCount);
                end;
            end;
        end;
    end;
    AllProvided := ProvidedCount >= GSymbols.Count;

    //  Evaluate with floating-point exceptions masked, so bad parameter values
    //  (e.g. division by a zero w in A*exp(-((x-x0)/w)^2)) yield Inf/NaN instead
    //  of raising - a fit probes such regions millions of times and must not
    //  abort or pay exception cost. A non-finite result is reported as 0 so the
    //  optimizer simply sees a poor fit there and moves on.
    OldMask := GetExceptionMask;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    try
        try
            d := GParser.AsFloat;
            if IsNan(d) or IsInfinite(d) then
                d := 0;
            ResultValue^ := d;
            if AllProvided then
                Result := 1
            else
                Result := -1;
        except
            //  Non-arithmetic evaluation failure (rare, expression already parsed).
            ResultValue^ := 0;
            if AllProvided then
                Result := 0
            else
                Result := -1;
        end;
    finally
        SetExceptionMask(OldMask);
    end;
end;

function EvaluatesFiniteAt(Expr: PChar; ParamList: PChar): boolean;
var
    code: longint;
    d: double;
    OldMask: TFPUExceptionMask;
begin
    Result := False;
    //  Runs the full evaluation (valid formula + every identifier supplied).
    code := ParseAndCalcExpression(Expr, ParamList, @d);
    if code <> 1 then
        Exit;
    //  ParseAndCalcExpression reports a non-finite result as 0; recompute with the
    //  parser's identifiers still holding the same parameter values to inspect the
    //  raw finiteness (exceptions masked so a bad value yields Inf/NaN, not a raise).
    OldMask := GetExceptionMask;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    try
        try
            d := GParser.AsFloat;
            Result := not (IsNan(d) or IsInfinite(d));
        except
            Result := False;
        end;
    finally
        SetExceptionMask(OldMask);
    end;
end;

function GetSymbols: PChar;
var
    total, off, i, len: integer;
    s: string;
begin
    EnsureEngine;
    total := 1;   //  trailing terminator
    for i := 0 to GSymbols.Count - 1 do
        total := total + Length(GSymbols[i]) + 1;
    Result := GetMem(total);
    off := 0;
    for i := 0 to GSymbols.Count - 1 do
    begin
        s := GSymbols[i];
        len := Length(s);
        if len > 0 then
            Move(s[1], Result[off], len);
        Inc(off, len);
        Result[off] := #0;
        Inc(off);
    end;
    Result[off] := #0;   //  double-#0 terminates the list
end;

procedure FreeSymbols(Symbols: PChar);
begin
    if Symbols <> nil then
        FreeMem(Symbols);
end;

initialization
    GDotFmt := DefaultFormatSettings;
    GDotFmt.DecimalSeparator := '.';
    GDotFmt.ThousandSeparator := #0;

finalization
    GParser.Free;
    GSymbols.Free;

end.
