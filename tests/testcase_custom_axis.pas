// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The user-defined argument axis: what it starts as, and when it is
usable.)

TWO FORMULAS, BOTH NEEDED. The chart converts in both directions - an abscissa
becomes a position on screen, and a click becomes an abscissa - so an axis with
only f(x) can be drawn and not clicked on, which is a chart the user cannot pick
in. The refusal is the only thing standing between that and a mode that half
works.

THE SEEDING IS THE INSTRUCTION. The dialog opens empty on a first use, and two
empty boxes say nothing about what belongs in them. f(x)=x, g(x)=x shows the
shape of a valid answer and defines the axis the user already has, so accepting
it unchanged does nothing surprising.

All of it was inside a menu click handler, between a dialog and a message box.
}
unit testcase_custom_axis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, custom_axis;

type
    TCustomAxisTest = class(TTestCase)
    private
        function Axis(const AName, AUnits, AForward,
            AInverse: string): TCustomAxisDefinition;
    published
        //  What a first use starts from.
        procedure TheDefaultIsTheIdentity;
        procedure TheDefaultIsNamed;
        procedure TheDefaultCarriesNoUnit;
        procedure TheDefaultIsItselfUsable;

        //  Whether it has been filled in.
        procedure AnAxisWithNoForwardFormulaIsUnset;
        procedure AnAxisWithOnlyWhitespaceIsUnset;
        procedure AnAxisWithAFormulaIsSet;
        procedure ANamelessAxisWithAFormulaIsStillSet;

        //  Whether it can be used.
        procedure BothFormulasMakeAUsableAxis;
        procedure AMissingForwardFormulaIsRefused;
        procedure AMissingInverseFormulaIsRefused;
        procedure WhitespaceIsNotAFormula;
        procedure ANameIsNotRequired;
        procedure AUnitIsNotRequired;

        //  What the user is told.
        procedure EveryProblemHasAMessage;
        procedure ThereIsNoMessageWhenThereIsNoProblem;
        procedure TheMessageNamesBothFormulas;
        procedure TheMessageShowsAnExample;

        //  Tidying what the dialog hands back.
        procedure SurroundingSpaceIsTrimmedFromEveryField;
        procedure TrimmingDoesNotAlterAFormula;
    end;

implementation

function TCustomAxisTest.Axis(const AName, AUnits, AForward,
    AInverse: string): TCustomAxisDefinition;
begin
    Result.Name := AName;
    Result.Units := AUnits;
    Result.Forward_ := AForward;
    Result.Inverse := AInverse;
end;

{ ---- what a first use starts from ------------------------------------------ }

procedure TCustomAxisTest.TheDefaultIsTheIdentity;
begin
    //  THE INSTRUCTION, and the safe answer at once: it shows what a formula
    //  looks like, and the axis it defines is the one the user already has.
    AssertEquals('f(x) = x', 'x', DefaultCustomAxis.Forward_);
    AssertEquals('g(x) = x', 'x', DefaultCustomAxis.Inverse);
end;

procedure TCustomAxisTest.TheDefaultIsNamed;
begin
    //  The name labels the chart's axis, and a blank label is a chart whose
    //  abscissa says nothing.
    AssertTrue('named', Trim(DefaultCustomAxis.Name) <> '');
end;

procedure TCustomAxisTest.TheDefaultCarriesNoUnit;
begin
    //  The identity axis is in whatever the data is in. Inventing a unit would
    //  label the chart with something untrue.
    AssertEquals('none', '', DefaultCustomAxis.Units);
end;

procedure TCustomAxisTest.TheDefaultIsItselfUsable;
begin
    //  A user who opens the dialog and presses OK must get a working axis. A
    //  seed that its own validation rejects would be a dead end.
    AssertTrue('no problem',
        CustomAxisProblem(DefaultCustomAxis) = capNone);
end;

{ ---- whether it has been filled in ----------------------------------------- }

procedure TCustomAxisTest.AnAxisWithNoForwardFormulaIsUnset;
begin
    AssertTrue('unset', CustomAxisIsUnset(Axis('', '', '', '')));
end;

procedure TCustomAxisTest.AnAxisWithOnlyWhitespaceIsUnset;
begin
    //  A settings file that round-tripped through something that padded it.
    AssertTrue('unset', CustomAxisIsUnset(Axis('Custom', '', '   ', '  ')));
end;

procedure TCustomAxisTest.AnAxisWithAFormulaIsSet;
begin
    //  Reopening the dialog on a defined axis must show what is there, not
    //  overwrite it with the seed.
    AssertFalse('set', CustomAxisIsUnset(Axis('Energy', 'eV', '1239.84/x',
        '1239.84/x')));
end;

procedure TCustomAxisTest.ANamelessAxisWithAFormulaIsStillSet;
begin
    //  THE FORWARD FORMULA IS THE TEST. A name or a unit left blank is a
    //  choice, and re-seeding over a formula the user wrote would lose it.
    AssertFalse('set', CustomAxisIsUnset(Axis('', '', 'ln(x)', 'exp(x)')));
end;

{ ---- whether it can be used ------------------------------------------------ }

procedure TCustomAxisTest.BothFormulasMakeAUsableAxis;
begin
    AssertTrue('usable',
        CustomAxisProblem(Axis('Energy', 'eV', 'ln(x)', 'exp(x)')) = capNone);
end;

procedure TCustomAxisTest.AMissingForwardFormulaIsRefused;
begin
    //  Nothing could be displayed at all.
    AssertTrue('refused',
        CustomAxisProblem(Axis('Energy', 'eV', '', 'exp(x)')) = capNoForward);
end;

procedure TCustomAxisTest.AMissingInverseFormulaIsRefused;
begin
    //  THE ONE A USER WILL GET WRONG. An axis with only f(x) draws perfectly
    //  well and cannot be clicked in - the chart has no way to turn a position
    //  back into an abscissa - so the mode half works, which is worse than not
    //  working.
    AssertTrue('refused',
        CustomAxisProblem(Axis('Energy', 'eV', 'ln(x)', '')) = capNoInverse);
end;

procedure TCustomAxisTest.WhitespaceIsNotAFormula;
begin
    AssertTrue('refused',
        CustomAxisProblem(Axis('E', '', '  ', 'exp(x)')) = capNoForward);
    AssertTrue('and the other way',
        CustomAxisProblem(Axis('E', '', 'ln(x)', '   ')) = capNoInverse);
end;

procedure TCustomAxisTest.ANameIsNotRequired;
begin
    //  Refusing an unnamed axis would block a definition that works.
    AssertTrue('accepted',
        CustomAxisProblem(Axis('', '', 'ln(x)', 'exp(x)')) = capNone);
end;

procedure TCustomAxisTest.AUnitIsNotRequired;
begin
    //  Not every axis has one - a ratio, an index, a count.
    AssertTrue('accepted',
        CustomAxisProblem(Axis('Ratio', '', 'x/2', 'x*2')) = capNone);
end;

{ ---- what the user is told ------------------------------------------------- }

procedure TCustomAxisTest.EveryProblemHasAMessage;
var
    P: TCustomAxisProblem;
begin
    //  WALKS THE ENUM. A refusal with nothing to say is a dialog that closes
    //  and does nothing, which the user reads as the program being broken.
    for P := Low(TCustomAxisProblem) to High(TCustomAxisProblem) do
        if P <> capNone then
            AssertTrue(Format('problem %d says something', [Ord(P)]),
                Trim(CustomAxisProblemMessage(P)) <> '');
end;

procedure TCustomAxisTest.ThereIsNoMessageWhenThereIsNoProblem;
begin
    AssertEquals('nothing to say', '', CustomAxisProblemMessage(capNone));
end;

procedure TCustomAxisTest.TheMessageNamesBothFormulas;
begin
    //  ONE MESSAGE FOR BOTH FAILURES, and it names both: a user who left one
    //  blank has very likely not understood that two are wanted, and telling
    //  them only about the one they missed does not explain why.
    AssertTrue('f(x)',
        Pos('f(x)', CustomAxisProblemMessage(capNoInverse)) > 0);
    AssertTrue('and g(x)',
        Pos('g(x)', CustomAxisProblemMessage(capNoInverse)) > 0);
end;

procedure TCustomAxisTest.TheMessageShowsAnExample;
begin
    //  What a formula looks like is the thing the user does not know, and a
    //  message that only restates the requirement leaves them where they were.
    AssertTrue('an example',
        Pos('e.g.', CustomAxisProblemMessage(capNoForward)) > 0);
end;

{ ---- tidying what the dialog hands back ------------------------------------ }

procedure TCustomAxisTest.SurroundingSpaceIsTrimmedFromEveryField;
var
    D: TCustomAxisDefinition;
begin
    //  A formula with a leading space is what a paste produces, and a parser
    //  will not tolerate it.
    D := TrimmedCustomAxis(Axis('  Energy ', ' eV ', ' ln(x) ', ' exp(x) '));
    AssertEquals('the name', 'Energy', D.Name);
    AssertEquals('the unit', 'eV', D.Units);
    AssertEquals('f(x)', 'ln(x)', D.Forward_);
    AssertEquals('g(x)', 'exp(x)', D.Inverse);
end;

procedure TCustomAxisTest.TrimmingDoesNotAlterAFormula;
begin
    //  Only the ends. A formula with spaces inside it is still that formula,
    //  and squeezing them would change what it means to a parser that cares.
    AssertEquals('inner spaces kept', 'ln( x ) + 1',
        TrimmedCustomAxis(Axis('', '', ' ln( x ) + 1 ', 'x')).Forward_);
end;

initialization
    //  A unit test: a record in, an answer out. No dialog and no message box.
    RegisterTest('unit', TCustomAxisTest);
end.
