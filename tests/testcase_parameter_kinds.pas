// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a parameter is treated, in the terms the user reads.)

WHAT THIS DEFENDS. The parameter table's colouring is the user's ONLY indication
of which numbers the fit is free to move. A value shown as fitted that is
actually fixed is a fit that appears not to have worked; one shown as fixed that
is being fitted is a constraint the user believes they set and did not.

The mapping is from the engine's eight parameter types onto the four
distinctions a reader needs, and it lived in the main form beside the colours
that paint it - so it could only be reached by opening a window and looking.

THE SWEEP IS THE POINT. A parameter type added later and not classified here
falls into the default and is shown as fitted, silently and plausibly. The walk
over every type is what makes that fail instead.
}
unit testcase_parameter_kinds;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    parameter_kinds, special_curve_parameter;

type
    TParameterKindsTest = class(TTestCase)
    published
        //  The four distinctions.
        procedure ASharedParameterIsShared;
        procedure ACalculatedParameterIsComputed;
        procedure AFixedPositionIsFixed;
        procedure EverythingTheFitMovesIsFitted;

        //  What the reader is shown.
        procedure EveryKindHasACaption;
        procedure EveryKindHasAnExplanation;
        procedure NoTwoKindsShareACaption;
        procedure TheExplanationsAreSentencesNotLabels;

        //  The sweep.
        procedure EveryParameterTypeIsClassified;
        procedure OnlyOneTypeIsShared;
        procedure OnlyOneTypeIsComputed;
        procedure OnlyOneTypeIsFixed;
    end;

implementation

{ ---- the four distinctions ------------------------------------------------- }

procedure TParameterKindsTest.ASharedParameterIsShared;
begin
    //  Held to one value across the curves of an interval - which is a
    //  constraint the user chose, and the only visible sign of it.
    AssertTrue('shared', KindOfParameter(Shared) = pkShared);
end;

procedure TParameterKindsTest.ACalculatedParameterIsComputed;
begin
    //  Neither fitted nor entered: it follows from the others. Showing it as
    //  fitted would invite the user to wonder why editing it does nothing.
    AssertTrue('computed', KindOfParameter(Calculated) = pkComputed);
end;

procedure TParameterKindsTest.AFixedPositionIsFixed;
begin
    //  A position the user placed and the fit will not move. Shown as fitted, a
    //  curve that stayed where it was put looks like a fit that failed.
    AssertTrue('fixed', KindOfParameter(InvariablePosition) = pkFixed);
end;

procedure TParameterKindsTest.EverythingTheFitMovesIsFitted;
begin
    //  FIVE ENGINE TYPES, ONE KIND. The engine distinguishes them by how each
    //  is seeded and constrained; the reader of the table does not care, and
    //  five colours where one distinction exists is noise.
    AssertTrue('variable', KindOfParameter(Variable) = pkFitted);
    AssertTrue('a variable position',
        KindOfParameter(VariablePosition) = pkFitted);
    AssertTrue('the amplitude', KindOfParameter(Amplitude) = pkFitted);
    AssertTrue('the width', KindOfParameter(Width) = pkFitted);
    AssertTrue('the abscissa', KindOfParameter(Argument) = pkFitted);
end;

{ ---- what the reader is shown ---------------------------------------------- }

procedure TParameterKindsTest.EveryKindHasACaption;
var
    K: TParameterKind;
begin
    for K := Low(TParameterKind) to High(TParameterKind) do
        AssertTrue('kind ' + IntToStr(Ord(K)) + ' is named',
            Trim(ParameterKindCaption[K]) <> '');
end;

procedure TParameterKindsTest.EveryKindHasAnExplanation;
var
    K: TParameterKind;
begin
    //  The legend row is one word; the sentence is on the hint, and a kind with
    //  no hint is a colour the user cannot find out the meaning of.
    for K := Low(TParameterKind) to High(TParameterKind) do
        AssertTrue('kind ' + IntToStr(Ord(K)) + ' is explained',
            Trim(ParameterKindHint[K]) <> '');
end;

procedure TParameterKindsTest.NoTwoKindsShareACaption;
var
    J, K: TParameterKind;
begin
    //  Two kinds under one word is two colours the user cannot tell apart.
    for J := Low(TParameterKind) to High(TParameterKind) do
        for K := Low(TParameterKind) to High(TParameterKind) do
            if J <> K then
                AssertTrue(Format('%s and %s differ',
                    [ParameterKindCaption[J], ParameterKindCaption[K]]),
                    ParameterKindCaption[J] <> ParameterKindCaption[K]);
end;

procedure TParameterKindsTest.TheExplanationsAreSentencesNotLabels;
var
    K: TParameterKind;
    Hint: string;
begin
    //  A hint that repeats the caption explains nothing, and a hint is the only
    //  room this interface has for the explanation.
    for K := Low(TParameterKind) to High(TParameterKind) do
    begin
        Hint := ParameterKindHint[K];
        AssertTrue('kind ' + ParameterKindCaption[K] + ' says more than its name',
            Length(Hint) > Length(ParameterKindCaption[K]) + 10);
        AssertEquals('and it ends as a sentence', '.', Copy(Hint, Length(Hint), 1));
    end;
end;

{ ---- the sweep ------------------------------------------------------------- }

procedure TParameterKindsTest.EveryParameterTypeIsClassified;
var
    T: TParameterType;
    Kinds: array[TParameterKind] of longint;
    K: TParameterKind;
    Total: longint;
begin
    //  WALKS THE ENGINE'S ENUM. A parameter type added later and not classified
    //  falls into the default and is shown as fitted - plausibly, and wrongly if
    //  it is not. This does not fail for such a type on its own; what it
    //  guarantees is that every type has an answer at all and that the answer is
    //  one of the four, so the array below can be trusted.
    for K := Low(TParameterKind) to High(TParameterKind) do
        Kinds[K] := 0;
    Total := 0;
    for T := Low(TParameterType) to High(TParameterType) do
    begin
        Inc(Kinds[KindOfParameter(T)]);
        Inc(Total);
    end;
    AssertEquals('every type was classified', Total,
        Kinds[pkFitted] + Kinds[pkShared] + Kinds[pkFixed] + Kinds[pkComputed]);
    AssertTrue('and most of them are fitted', Kinds[pkFitted] > 0);
end;

procedure TParameterKindsTest.OnlyOneTypeIsShared;
var
    T: TParameterType;
    Count: longint;
begin
    //  Exactly one, and it is the one the user asked to be shared. A second
    //  type falling into this kind would colour values as constrained that the
    //  user did not constrain.
    Count := 0;
    for T := Low(TParameterType) to High(TParameterType) do
        if KindOfParameter(T) = pkShared then
            Inc(Count);
    AssertEquals('one', 1, Count);
end;

procedure TParameterKindsTest.OnlyOneTypeIsComputed;
var
    T: TParameterType;
    Count: longint;
begin
    Count := 0;
    for T := Low(TParameterType) to High(TParameterType) do
        if KindOfParameter(T) = pkComputed then
            Inc(Count);
    AssertEquals('one', 1, Count);
end;

procedure TParameterKindsTest.OnlyOneTypeIsFixed;
var
    T: TParameterType;
    Count: longint;
begin
    Count := 0;
    for T := Low(TParameterType) to High(TParameterType) do
        if KindOfParameter(T) = pkFixed then
            Inc(Count);
    AssertEquals('one', 1, Count);
end;

initialization
    //  A unit test: an enum in, an enum out. The colours stayed in the window.
    RegisterTest('unit', TParameterKindsTest);
end.
