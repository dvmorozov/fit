// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Every built-in shape says what it is, in the suite that needs no Lazarus.)

WHY A SECOND TEST OF THE SAME PROMISE. testcase_curve_type_explanations walks the
registry, which is the test that catches the NEXT type arriving without an
explanation - but registering the curve types links user_points_set, which
needs the LCL, so that test builds only in the nogui suite. The plain-FPC suite
is the one a visitor without Lazarus runs, and it builds every built-in shape;
without this, what those shapes say about themselves would be tested only where
it cannot be built without the LCL.

So this names the shapes the plain suite links and asks each class directly.
It checks the CONTENT a class owes - summary, body, limitation, source - not the
topic and title, which the provider stamps and the registry test checks.
}
unit testcase_curve_type_explanation_content;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, named_points_set,
    gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
    asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
    voigt_points_set, doniach_sunjic_points_set, emg_points_set,
    skewed_gaussian_points_set, moffat_points_set, pearson7_points_set,
    step_points_set;

type
    TCurveTypeExplanationContentTest = class(TTestCase)
    published
        procedure EveryBuiltInShapeSaysWhatItIs;
        procedure EveryBuiltInShapeSaysWhatItDoesNotCover;
        procedure EveryPublishedShapeNamesWhereToReadMore;
        procedure AShapeThisSoftwareDefinedClaimsNoSource;
        procedure ATypeThatSaysNothingIsLeftUnexplained;
        procedure AShapeThatIsAPublishedProfileQuotesNoInventedWords;
    end;

implementation

function Shapes: specialize TArray<TNamedPointsSetClass>;
begin
    Result := [TGaussPointsSet, TLorentzPointsSet, TPseudoVoigtPointsSet,
        TAsymPseudoVoigtPointsSet, T2BranchesPseudoVoigtPointsSet,
        TVoigtPointsSet, TDoniachSunjicPointsSet, TEmgPointsSet,
        TSkewedGaussianPointsSet, TMoffatPointsSet, TPearson7PointsSet,
        TStepPointsSet];
end;

procedure TCurveTypeExplanationContentTest.EveryBuiltInShapeSaysWhatItIs;
var
    Cls: TNamedPointsSetClass;
    E: TExplanation;
begin
    for Cls in Shapes do
    begin
        E := Cls.Explanation;
        AssertTrue(Cls.GetCurveTypeName + ' has a one-sentence summary',
            Trim(E.Summary) <> '');
        AssertTrue(Cls.GetCurveTypeName + ' explains itself in a body',
            Length(E.Body) > 0);
    end;
end;

procedure TCurveTypeExplanationContentTest.EveryBuiltInShapeSaysWhatItDoesNotCover;
var
    Cls: TNamedPointsSetClass;
begin
    //  A learner can infer what a shape is for; what it quietly gets wrong is
    //  the part they cannot.
    for Cls in Shapes do
        AssertTrue(Cls.GetCurveTypeName + ' states a limitation',
            Length(Cls.Explanation.Limitations) > 0);
end;

procedure TCurveTypeExplanationContentTest.EveryPublishedShapeNamesWhereToReadMore;
var
    Cls: TNamedPointsSetClass;
    E: TExplanation;
    i: longint;
begin
    //  A shape the literature defines names where it is defined.
    for Cls in Shapes do
    begin
        E := Cls.Explanation;
        if E.Standing <> esCanonical then
            Continue;
        AssertTrue(Cls.GetCurveTypeName + ' names a source',
            Length(E.References) > 0);
        for i := 0 to High(E.References) do
            AssertTrue(Cls.GetCurveTypeName + ': a source names its work',
                Trim(E.References[i].Work) <> '');
    end;
end;

procedure TCurveTypeExplanationContentTest.AShapeThisSoftwareDefinedClaimsNoSource;
var
    Cls: TNamedPointsSetClass;
    E: TExplanation;
    Found: boolean;
begin
    //  THE OTHER HALF OF HONESTY. A variant this software defined - the two
    //  asymmetric pseudo-Voigts - must say so, and must not borrow a paper that
    //  describes a different shape to look authoritative. Asserted rather than
    //  assumed: at least one such shape exists, or this test checks nothing.
    Found := False;
    for Cls in Shapes do
    begin
        E := Cls.Explanation;
        if E.Standing <> esModelChoice then
            Continue;
        Found := True;
        AssertEquals(Cls.GetCurveTypeName + ' quotes no source', '', Trim(E.Quote));
        AssertTrue(Cls.GetCurveTypeName + ' says what it does not cover',
            Length(E.Limitations) > 0);
    end;
    AssertTrue('a shape this software defined is among the built-ins', Found);
end;

procedure TCurveTypeExplanationContentTest.AShapeThatIsAPublishedProfileQuotesNoInventedWords;
var
    Cls: TNamedPointsSetClass;
    E: TExplanation;
begin
    //  Canonical means a source states it; a canonical shape with no quote
    //  would be claiming authority it does not show.
    for Cls in Shapes do
    begin
        E := Cls.Explanation;
        if E.Standing = esCanonical then
            AssertTrue(Cls.GetCurveTypeName + ' is canonical, so it quotes its source',
                Trim(E.Quote) <> '');
    end;
end;


procedure TCurveTypeExplanationContentTest.ATypeThatSaysNothingIsLeftUnexplained;
var
    E: TExplanation;
begin
    //  THE DEFAULT IS SILENCE, ON PURPOSE. A curve type that does not override
    //  Explanation gets nothing to say, so the completeness checks fail it by
    //  name instead of shipping a type the application cannot explain. Asked of
    //  the base class, which every built-in shape overrides.
    E := TNamedPointsSet.Explanation;
    AssertEquals('no summary', '', E.Summary);
    AssertEquals('no body', 0, Length(E.Body));
    AssertEquals('no source', 0, Length(E.References));
end;

initialization
    RegisterTest('unit', TCurveTypeExplanationContentTest);
end.
