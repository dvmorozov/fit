// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A component name made from data, that the widget set will accept.)

WHY THESE TESTS EXIST. Setting a TComponent.Name that is not a valid identifier,
or that another component of the same owner already holds, RAISES. Both are
reachable from data the user supplies - a curve type beginning with a digit, two
user curves given the same name - so the failure would be an exception at
start-up on somebody else's model, in a build loop, with nothing on screen yet
to say which name did it.

Every test here is one shape of that. The one that would have cost the most is
the leading digit: the framework ships a type called "2 br. Pseudo-Voigt".
}
unit testcase_ui_names;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, ui_names;

type
    TUiNamesTest = class(TTestCase)
    private
        FNames: TWidgetNames;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The identifier a source makes.
        procedure APlainWordIsItself;
        procedure SpacesAndPunctuationAreDropped;
        procedure ADigitInTheMiddleSurvives;
        procedure ALeadingDigitSurvivesBehindThePrefix;
        procedure UnderscoresAreKept;
        procedure NonAsciiIsDropped;
        procedure ASourceOfNothingUsableStillGetsAName;
        procedure AnEmptySourceStillGetsAName;

        //  What the widget set will accept.
        procedure EveryAnswerIsAValidIdentifier;
        procedure AnswersAreValidEvenWithNoPrefix;

        //  Uniqueness is a property of the set.
        procedure TheSameSourceTwiceGivesTwoNames;
        procedure AndThreeTimesGivesThree;
        procedure DifferentSourcesThatReduceToOneIdentifierStillDiffer;
        procedure UniquenessIgnoresCaseTheWayPascalDoes;
        procedure ADifferentPrefixIsADifferentName;
        procedure ClearingLetsTheNamesBeIssuedAgain;

        //  The framework's own data, which is where this has to work.
        procedure TheShippedCurveTypeNamesAllProduceValidNames;
        procedure AndTheyAreAllDistinct;
    end;

implementation

const
    { The curve type names this build ships, verbatim. The awkward ones are the
      point: a leading digit, a dot, a hyphen and several spaces. }
    ShippedTypes: array[0..12] of string = (
        'Gaussian', 'Lorentzian', 'Pseudo-Voigt', 'Asym. Pseudo-Voigt',
        '2 br. Pseudo-Voigt', 'Voigt', 'Doniach-Sunjic',
        'Exponentially Modified Gaussian', 'Skewed Gaussian', 'Moffat',
        'Pearson VII', 'Step (erf)', 'User Defined');

procedure TUiNamesTest.SetUp;
begin
    FNames := TWidgetNames.Create;
end;

procedure TUiNamesTest.TearDown;
begin
    FNames.Free;
    FNames := nil;
end;

{ ---- the identifier a source makes ---- }

procedure TUiNamesTest.APlainWordIsItself;
begin
    AssertEquals('Gaussian', IdentifierFrom('Gaussian'));
end;

procedure TUiNamesTest.SpacesAndPunctuationAreDropped;
begin
    //  Dropped rather than replaced, so the answer still reads as the name it
    //  came from.
    AssertEquals('AsymPseudoVoigt', IdentifierFrom('Asym. Pseudo-Voigt'));
    AssertEquals('Steperf', IdentifierFrom('Step (erf)'));
end;

procedure TUiNamesTest.ADigitInTheMiddleSurvives;
begin
    AssertEquals('PearsonVII7', IdentifierFrom('Pearson VII 7'));
end;

procedure TUiNamesTest.ALeadingDigitSurvivesBehindThePrefix;
begin
    //  THE ONE THAT WOULD HAVE COST THE MOST. The 2 is what distinguishes this
    //  type from the plain Pseudo-Voigt, so it is kept - and it is legal
    //  because the prefix goes in front of it.
    AssertEquals('2brPseudoVoigt', IdentifierFrom('2 br. Pseudo-Voigt'));
    AssertEquals('MenuCurveType2brPseudoVoigt',
        FNames.NameFor('MenuCurveType', '2 br. Pseudo-Voigt'));
end;

procedure TUiNamesTest.UnderscoresAreKept;
begin
    AssertEquals('peak_bounds', IdentifierFrom('peak_bounds'));
end;

procedure TUiNamesTest.NonAsciiIsDropped;
begin
    //  A name is not a caption: it does not have to carry the user's own
    //  alphabet to be useful in a debugger, and an identifier cannot.
    AssertEquals('Theta', IdentifierFrom('Theta' + #$CE + #$B8));
end;

procedure TUiNamesTest.ASourceOfNothingUsableStillGetsAName;
begin
    //  A dull name beats an exception in a build loop.
    AssertEquals('Item', IdentifierFrom('...'));
end;

procedure TUiNamesTest.AnEmptySourceStillGetsAName;
begin
    AssertEquals('Item', IdentifierFrom(''));
end;

{ ---- what the widget set will accept ---- }

procedure TUiNamesTest.EveryAnswerIsAValidIdentifier;
begin
    AssertTrue('plain', IsValidIdent(FNames.NameFor('Button', 'Pick')));
    AssertTrue('punctuation',
        IsValidIdent(FNames.NameFor('Button', 'Step (erf)')));
    AssertTrue('leading digit',
        IsValidIdent(FNames.NameFor('Button', '2 br.')));
    AssertTrue('nothing usable', IsValidIdent(FNames.NameFor('Button', '!!')));
    AssertTrue('empty', IsValidIdent(FNames.NameFor('Button', '')));
end;

procedure TUiNamesTest.AnswersAreValidEvenWithNoPrefix;
begin
    //  With no prefix a leading digit would be first, which no identifier may
    //  start with. The caller here is a build loop, so this is guarded rather
    //  than forbidden of it.
    AssertTrue('no prefix, leading digit',
        IsValidIdent(FNames.NameFor('', '2 br. Pseudo-Voigt')));
end;

{ ---- uniqueness ---- }

procedure TUiNamesTest.TheSameSourceTwiceGivesTwoNames;
var
    A, B: string;
begin
    //  Two user curves may be given the same name, and nothing stops them. Two
    //  components of one owner with one name is an exception.
    A := FNames.NameFor('MenuCurveType', 'My curve');
    B := FNames.NameFor('MenuCurveType', 'My curve');
    AssertEquals('MenuCurveTypeMycurve', A);
    AssertTrue('the second differs', A <> B);
    AssertTrue('and is still valid', IsValidIdent(B));
end;

procedure TUiNamesTest.AndThreeTimesGivesThree;
var
    A, B, C: string;
begin
    A := FNames.NameFor('X', 'same');
    B := FNames.NameFor('X', 'same');
    C := FNames.NameFor('X', 'same');
    AssertTrue('all three differ',
        (A <> B) and (B <> C) and (A <> C));
end;

procedure TUiNamesTest.DifferentSourcesThatReduceToOneIdentifierStillDiffer;
var
    A, B: string;
begin
    //  "A B" and "AB" reduce to the same characters. The register is what
    //  notices; the reduction cannot.
    A := FNames.NameFor('X', 'A B');
    B := FNames.NameFor('X', 'AB');
    AssertTrue('still two names', A <> B);
end;

procedure TUiNamesTest.UniquenessIgnoresCaseTheWayPascalDoes;
var
    A, B: string;
begin
    //  Pascal compares identifiers case-insensitively and so does the widget
    //  set, so 'Curve' and 'curve' would collide where it matters.
    A := FNames.NameFor('X', 'Curve');
    B := FNames.NameFor('X', 'curve');
    AssertTrue('treated as a collision', A <> B);
end;

procedure TUiNamesTest.ADifferentPrefixIsADifferentName;
var
    A, B: string;
begin
    A := FNames.NameFor('Button', 'Pick');
    B := FNames.NameFor('Label', 'Pick');
    AssertEquals('ButtonPick', A);
    AssertEquals('LabelPick', B);
end;

procedure TUiNamesTest.ClearingLetsTheNamesBeIssuedAgain;
var
    A, B: string;
begin
    //  A caller that rebuilds a set of widgets from scratch: last time's names
    //  went with last time's widgets, so holding them would suffix every name
    //  a little further on every rebuild.
    A := FNames.NameFor('X', 'same');
    FNames.Clear;
    B := FNames.NameFor('X', 'same');
    AssertEquals('the same name again', A, B);
end;

{ ---- the framework's own data ---- }

procedure TUiNamesTest.TheShippedCurveTypeNamesAllProduceValidNames;
var
    i: longint;
begin
    for i := Low(ShippedTypes) to High(ShippedTypes) do
        AssertTrue(ShippedTypes[i],
            IsValidIdent(FNames.NameFor('MenuCurveType', ShippedTypes[i])));
end;

procedure TUiNamesTest.AndTheyAreAllDistinct;
var
    i: longint;
    Seen: TStringList;
    Name_: string;
begin
    Seen := TStringList.Create;
    try
        Seen.CaseSensitive := False;
        for i := Low(ShippedTypes) to High(ShippedTypes) do
        begin
            Name_ := FNames.NameFor('MenuCurveType', ShippedTypes[i]);
            AssertTrue(ShippedTypes[i] + ' is distinct',
                Seen.IndexOf(Name_) < 0);
            Seen.Add(Name_);
        end;
    finally
        Seen.Free;
    end;
end;

initialization
    //  A unit test: strings in, strings out. The widget set is what would
    //  refuse the answer, and it is exactly what cannot be reached from here -
    //  which is why the rule it enforces is asserted directly.
    RegisterTest('unit', TUiNamesTest);
end.
