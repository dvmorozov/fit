// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(SimpMath's vector, affine and numeric helpers.)

WHY THE AFFINE FUNCTIONS ARE TESTED IN THE ORTHONORMAL CASE. Most of this unit is
crystallography machinery: scalar and cross products in a basis with arbitrary
lengths and inter-axial angles, mutual (reciprocal) vectors, cell volumes, and
changes of basis. Checking those against hand-computed values in a general
triclinic basis would mean re-deriving the mathematics in the test, and a test
that re-derives its subject checks only that two derivations match.

So the tests pin them where the answer is independently known: with A=B=C=1 and
all angles pi/2, every affine function must reduce EXACTLY to its Cartesian
counterpart. That is a real constraint - it fails if the D determinant, the
reciprocal-vector normalisation or the volume factor is wrong - and it needs no
second derivation. A few identities that hold in any basis (a vector's own unit
vector has modulus one; a cross product is orthogonal to both operands) are
checked in a skewed basis as well.

TWO DEFECTS ARE CHARACTERISED HERE RATHER THAN FIXED, and both are marked: this
unit belongs to a sibling repository, so a test that documents what it currently
does is the honest thing to leave behind. See ArcSinIsWrongAtMinusOne and
ArcSinHasNoDomainGuard.
}
unit testcase_simpmath;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, SimpMath;

type
    { Enough of a handle on the five array fillers to drive them from one loop.
      They differ in arity, so this names the shape rather than the signature
      and each case supplies its own closure over the fixed parameters. }
    TShapeFill = procedure(APoints: TwoDimArray);

    TSimpMathVectorTest = class(TTestCase)
    private
        function V(const x, y, z: double): TDoubleVector3;
        procedure AssertVector(const AMessage: string;
            const AExpected, AActual: TDoubleVector3);
    published
        //  Cartesian
        procedure TheScalarProductIsTheDotProduct;
        procedure OrthogonalVectorsHaveZeroScalarProduct;
        procedure TheModulusIsThePythagoreanLength;
        procedure AUnitVectorHasModulusOne;
        procedure TheZeroVectorNormalisesToZeroRatherThanRaising;
        procedure ScalingMultipliesEveryComponent;
        procedure SubtractionIsComponentwise;

        //  Affine, reduced to the orthonormal case
        procedure AnOrthonormalCellHasVolumeOne;
        procedure ARectangularCellHasVolumeEqualToItsSides;
        procedure TheAffineScalarProductReducesToTheCartesianOne;
        procedure TheAffineModulusReducesToTheCartesianOne;
        procedure TheAffineCrossProductReducesToTheCartesianOne;
        procedure TheAngleBetweenPerpendicularAxesIsNinetyDegrees;
        procedure TheAngleBetweenAVectorAndItselfIsZero;
        procedure MutualVectorsOfAnOrthonormalBasisAreTheBasis;
        procedure TheAffineUnitVectorHasAffineModulusOne;
        procedure ANormalisedScalarProductIsACosine;

        //  Affine, in a skewed basis - identities that hold in any basis
        procedure ACrossProductIsOrthogonalToBothOperandsInASkewedBasis;
        procedure AUnitVectorHasModulusOneInASkewedBasis;
        procedure SettingAModulusGivesThatModulus;

        //  Change of basis
        procedure AVectorInItsOwnBasisHasUnitCoordinates;
        procedure DoublingTheBasisHalvesTheCoordinates;
        procedure MutualVectorsInANewBasisAreReciprocalToIt;
        procedure CartesianAndAffineConversionsAreInverses;
    end;

    TSimpMathNumericTest = class(TTestCase)
    published
        procedure SphericalAndCartesianRoundTrip;
        procedure TheSphericalPoleIsOnTheZAxis;
        procedure PhiWrapsAtPi;
        procedure ThetaClampsToItsRange;
        procedure AValueIsWrappedIntoTheInterval;
        procedure IntervalMembershipIncludesItsBounds;
        procedure TheDecimalDegreeOfANumber;
        procedure PowersOfTenAreBuiltByRepeatedMultiplication;
        procedure SignTreatsZeroAsPositive;
        procedure SortUpOrdersThreeValues;
        procedure ArcCosMatchesTheLibrary;
        procedure ArcSinMatchesTheLibraryInsideItsRange;
        procedure ArcSinIsWrongAtMinusOne;
        procedure ArcSinHasNoDomainGuard;
        procedure LagrangePassesThroughItsPoints;

        //  The lineshapes, as single points. Every curve type in the menu is
        //  one of these formulas, and what they are seeded from depends on
        //  properties nothing had asserted - that the amplitude IS the peak,
        //  that a pseudo-Voigt reduces to its two ends, that a tail vanishes.
        procedure AGaussianPeaksAtItsCentre;
        procedure AGaussianIsSymmetric;
        procedure AGaussianFallsToNothingFarAway;
        procedure AWiderGaussianIsLowerAwayFromItsCentre;
        procedure ALorentzianPeaksAtItsCentreToo;
        procedure ALorentzianHasTheHeavierTail;
        procedure APseudoVoigtInterpolatesBetweenTheTwo;
        procedure APseudoVoigtSitsBetweenItsEnds;
        procedure AnAsymmetricShapeIsAsymmetric;
        procedure AnAsymmetricShapeWithNoAsymmetryIsSymmetric;
        procedure TheTwoBranchShapeUsesEachBranchOnItsOwnSide;
        procedure EveryShapeIsNonNegativeForAPositiveAmplitude;

        //  The quadratic a background is built from.
        procedure TheQuadraticIsItsConstantAtItsOwnCentre;
        procedure TheQuadraticIsSymmetricWhenItsLinearTermIsZero;
        procedure TheLinearTermTiltsTheQuadratic;

        //  Stepping an angle.
        procedure IncreasingPhiWrapsRatherThanRunningOn;
        procedure DecreasingPhiWrapsTheOtherWay;
        procedure SteppingThetaStaysInsideItsRange;
        procedure LagrangeInterpolatesAStraightLineExactly;
        procedure LagrangeRefusesAnUnassignedArray;
        procedure TheSecondOrderPolynomialIsCentredOnX0;
    end;

    { The five lineshapes as ARRAY FILLERS, which is how the program actually
      calls them - every model curve's DoCalc hands its points to one of these
      and reads column 2 back. The point functions above pin the formulas; what
      is pinned here is the loop around them, and the loop is where a lineshape
      can go wrong without the formula being wrong at all: AsymPseudoVoigt
      passes (.., x0, PointsArray[i][1], DeltaSigma) and TwoBranchesPseudoVoigt
      passes (.., x0, PointsArray[i][1]), so in both the abscissa sits between
      arguments of its own type and a transposition would still compile and
      still produce a plausible curve. Each test therefore compares the filled
      column against the point function called directly. }
    TSimpMathShapeArrayTest = class(TTestCase)
    private
        FPoints: TwoDimArray;
        { Abscissae spread either side of x0 = 10, so both branches of the two
          asymmetric shapes are exercised by every one of these tests. }
        procedure GivenAbscissae;
        procedure CheckColumnTwo(const AMessage: string;
            const AExpected: array of double);
        procedure CheckRefusesNil(const AMessage: string; AFill: TShapeFill;
            APoints: TwoDimArray);
    published
        procedure GaussFillsEveryPointFromItsOwnFormula;
        procedure LorentzFillsEveryPointFromItsOwnFormula;
        procedure PseudoVoigtFillsEveryPointFromItsOwnFormula;
        procedure AsymPseudoVoigtFillsEveryPointFromItsOwnFormula;
        procedure TwoBranchesFillsEveryPointFromItsOwnFormula;
        procedure FillingLeavesTheAbscissaeAlone;
        procedure EveryShapeRefusesAnUnassignedArray;
        procedure AnEmptyArrayIsRefusedAsIfItWereUnassigned;
    end;

implementation

const
    { An orthonormal basis: unit lengths, right angles. Every affine function must
      reduce to its Cartesian counterpart here. }
    ORTHO_A = 1.0; ORTHO_B = 1.0; ORTHO_C = 1.0;
    RIGHT = pi / 2;
    EPS = 1e-9;
    { A deliberately skewed basis - unequal sides, no right angle among them - for
      the identities that must hold whatever the basis is. }
    SKEW_A = 2.0; SKEW_B = 3.0; SKEW_C = 5.0;
    SKEW_ALPHA = 1.2; SKEW_BETA = 1.4; SKEW_GAMMA = 1.0;

function TSimpMathVectorTest.V(const x, y, z: double): TDoubleVector3;
begin
    Result[1] := x;
    Result[2] := y;
    Result[3] := z;
end;

procedure TSimpMathVectorTest.AssertVector(const AMessage: string;
    const AExpected, AActual: TDoubleVector3);
begin
    AssertEquals(AMessage + ' [1]', AExpected[1], AActual[1], EPS);
    AssertEquals(AMessage + ' [2]', AExpected[2], AActual[2], EPS);
    AssertEquals(AMessage + ' [3]', AExpected[3], AActual[3], EPS);
end;

{ ---- Cartesian ------------------------------------------------------------- }

procedure TSimpMathVectorTest.TheScalarProductIsTheDotProduct;
begin
    AssertEquals(1*4 + 2*5 + 3*6, GetScalarMul(V(1, 2, 3), V(4, 5, 6)), EPS);
end;

procedure TSimpMathVectorTest.OrthogonalVectorsHaveZeroScalarProduct;
begin
    AssertEquals(0.0, GetScalarMul(V(1, 0, 0), V(0, 1, 0)), EPS);
    AssertEquals(0.0, GetScalarMul(V(1, 1, 0), V(1, -1, 0)), EPS);
end;

procedure TSimpMathVectorTest.TheModulusIsThePythagoreanLength;
begin
    AssertEquals(5.0, GetVectModule(V(3, 4, 0)), EPS);
    AssertEquals(0.0, GetVectModule(V(0, 0, 0)), EPS);
end;

procedure TSimpMathVectorTest.AUnitVectorHasModulusOne;
var
    U: TDoubleVector3;
begin
    GetUnitVect(V(3, 4, 0), U);
    AssertEquals('modulus', 1.0, GetVectModule(U), EPS);
    AssertVector('direction preserved', V(0.6, 0.8, 0), U);
end;

procedure TSimpMathVectorTest.TheZeroVectorNormalisesToZeroRatherThanRaising;
var
    U: TDoubleVector3;
begin
    //  There is no unit vector for the zero vector, and dividing by its modulus
    //  would be a division by zero. Answering with the zero vector is a choice
    //  rather than an oversight - the guard is explicit in the body - so it is
    //  worth pinning: a caller relying on it must not be broken by a "fix".
    GetUnitVect(V(0, 0, 0), U);
    AssertVector('zero, not a division by zero', V(0, 0, 0), U);
end;

procedure TSimpMathVectorTest.ScalingMultipliesEveryComponent;
begin
    AssertVector('scaled', V(2, 4, 6), MulVectByValue(V(1, 2, 3), 2));
    AssertVector('negated', V(-1, -2, -3), MulVectByValue(V(1, 2, 3), -1));
    AssertVector('zeroed', V(0, 0, 0), MulVectByValue(V(1, 2, 3), 0));
end;

procedure TSimpMathVectorTest.SubtractionIsComponentwise;
begin
    AssertVector('difference', V(3, 3, 3), GetSubVect(V(5, 7, 9), V(2, 4, 6)));
    AssertVector('a vector minus itself', V(0, 0, 0),
        GetSubVect(V(1, 2, 3), V(1, 2, 3)));
end;

{ ---- affine, reduced to the orthonormal case ------------------------------- }

procedure TSimpMathVectorTest.AnOrthonormalCellHasVolumeOne;
begin
    //  V = A*B*C*Sqrt(D), and D must be exactly 1 when every angle is a right
    //  angle. A sign slip in the D determinant shows up here first.
    AssertEquals(1.0,
        GetVolume(ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.ARectangularCellHasVolumeEqualToItsSides;
begin
    AssertEquals(2 * 3 * 5.0, GetVolume(2, 3, 5, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.TheAffineScalarProductReducesToTheCartesianOne;
var
    P, Q: TDoubleVector3;
begin
    P := V(1, 2, 3);
    Q := V(4, -5, 6);
    AssertEquals(GetScalarMul(P, Q),
        GetScalarMulA(P, Q, ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.TheAffineModulusReducesToTheCartesianOne;
var
    P: TDoubleVector3;
begin
    P := V(3, 4, 12);
    AssertEquals(GetVectModule(P),
        GetVectModuleA(P, ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.TheAffineCrossProductReducesToTheCartesianOne;
var
    R: TDoubleVector3;
begin
    //  e1 x e2 = e3 in a right-handed orthonormal basis. This exercises the whole
    //  reciprocal-vector path - GetMutualVectors, the volume factor and the
    //  determinant - against an answer that needs no derivation.
    R := GetVectorMulA(V(1, 0, 0), V(0, 1, 0),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT);
    AssertVector('e1 x e2 = e3', V(0, 0, 1), R);

    R := GetVectorMulA(V(0, 1, 0), V(0, 0, 1),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT);
    AssertVector('e2 x e3 = e1', V(1, 0, 0), R);
end;

procedure TSimpMathVectorTest.TheAngleBetweenPerpendicularAxesIsNinetyDegrees;
begin
    AssertEquals(RIGHT, GetAngle(V(1, 0, 0), V(0, 1, 0),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.TheAngleBetweenAVectorAndItselfIsZero;
begin
    AssertEquals(0.0, GetAngle(V(1, 2, 3), V(1, 2, 3),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.MutualVectorsOfAnOrthonormalBasisAreTheBasis;
var
    M1, M2, M3: TDoubleVector3;
begin
    //  The reciprocal basis of an orthonormal basis is itself. Anything else here
    //  means the 1/V normalisation or the sign correction is wrong.
    M1 := V(0, 0, 0); M2 := V(0, 0, 0); M3 := V(0, 0, 0);
    GetMutualVectors(ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT, M1, M2, M3);
    AssertVector('first', V(1, 0, 0), M1);
    AssertVector('second', V(0, 1, 0), M2);
    AssertVector('third', V(0, 0, 1), M3);
end;

procedure TSimpMathVectorTest.TheAffineUnitVectorHasAffineModulusOne;
var
    U: TDoubleVector3;
begin
    U := V(0, 0, 0);
    GetUnitVectA(V(3, 4, 0), ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT, U);
    AssertEquals(1.0,
        GetVectModuleA(U, ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT), EPS);
end;

procedure TSimpMathVectorTest.ANormalisedScalarProductIsACosine;
var
    C: double;
begin
    //  The normalised product of two unit-length orthogonal axes is cos(90) = 0,
    //  and of a vector with itself, cos(0) = 1.
    C := GetScalarMulAN(V(1, 0, 0), V(0, 1, 0),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT);
    AssertEquals('perpendicular', 0.0, C, EPS);
    C := GetScalarMulAN(V(1, 2, 3), V(1, 2, 3),
        ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT);
    AssertEquals('parallel', 1.0, C, EPS);
end;

{ ---- affine, in a skewed basis --------------------------------------------- }

procedure TSimpMathVectorTest.ACrossProductIsOrthogonalToBothOperandsInASkewedBasis;
var
    P, Q, R: TDoubleVector3;
begin
    //  Holds in ANY basis, so it tests the machinery where the orthonormal
    //  reduction cannot: if the volume factor or the reciprocal normalisation is
    //  wrong in the general case, the result stops being perpendicular.
    P := V(1, 0, 0);
    Q := V(0, 1, 0);
    R := GetVectorMulA(P, Q, SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA);
    AssertEquals('perpendicular to the first', 0.0,
        GetScalarMulA(R, P, SKEW_A, SKEW_B, SKEW_C,
            SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA), 1e-6);
    AssertEquals('perpendicular to the second', 0.0,
        GetScalarMulA(R, Q, SKEW_A, SKEW_B, SKEW_C,
            SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA), 1e-6);
end;

procedure TSimpMathVectorTest.AUnitVectorHasModulusOneInASkewedBasis;
var
    U: TDoubleVector3;
begin
    U := V(0, 0, 0);
    GetUnitVectA(V(1, 2, 3), SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA, U);
    AssertEquals(1.0, GetVectModuleA(U, SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA), EPS);
end;

procedure TSimpMathVectorTest.SettingAModulusGivesThatModulus;
var
    P: TDoubleVector3;
begin
    P := V(1, 2, 3);
    SetVectModule(P, SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA, 7.0);
    AssertEquals('the requested modulus', 7.0,
        GetVectModuleA(P, SKEW_A, SKEW_B, SKEW_C,
            SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA), 1e-6);
end;

{ ---- change of basis ------------------------------------------------------- }

procedure TSimpMathVectorTest.AVectorInItsOwnBasisHasUnitCoordinates;
var
    R: TDoubleVector3;
begin
    //  Expressed in the basis it is a member of, e1 is (1,0,0) - whatever the
    //  original basis was.
    R := GetVectInNewBasis(ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT,
        V(1, 0, 0), V(0, 1, 0), V(0, 0, 1), V(1, 0, 0));
    AssertVector('e1 in the basis containing it', V(1, 0, 0), R);
end;

procedure TSimpMathVectorTest.DoublingTheBasisHalvesTheCoordinates;
var
    R: TDoubleVector3;
begin
    //  A basis twice as long describes the same vector with half the coordinates.
    //  This is the check that catches an inverted transform, which the identity
    //  basis above cannot.
    R := GetVectInNewBasis(ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT,
        V(2, 0, 0), V(0, 2, 0), V(0, 0, 2), V(2, 4, 6));
    AssertVector('halved, not doubled', V(1, 2, 3), R);
end;

procedure TSimpMathVectorTest.MutualVectorsInANewBasisAreReciprocalToIt;
var
    M1, M2, M3: TDoubleVector3;
begin
    //  The defining property: a reciprocal vector has unit product with its own
    //  basis vector and zero with the others.
    M1 := V(0, 0, 0); M2 := V(0, 0, 0); M3 := V(0, 0, 0);
    GetMutualVectorsInNewBasis(ORTHO_A, ORTHO_B, ORTHO_C, RIGHT, RIGHT, RIGHT,
        V(2, 0, 0), V(0, 2, 0), V(0, 0, 2), M1, M2, M3);
    AssertEquals('with its own', 1.0,
        GetScalarMulA(M1, V(2, 0, 0), ORTHO_A, ORTHO_B, ORTHO_C,
            RIGHT, RIGHT, RIGHT), 1e-6);
    AssertEquals('with another', 0.0,
        GetScalarMulA(M1, V(0, 2, 0), ORTHO_A, ORTHO_B, ORTHO_C,
            RIGHT, RIGHT, RIGHT), 1e-6);
end;

procedure TSimpMathVectorTest.CartesianAndAffineConversionsAreInverses;
var
    P: TDoubleVector3;
begin
    //  The two conversions are documented as inverses; running them back to back
    //  is what proves it, and in a SKEWED basis, where they actually differ.
    P := V(1, 2, 3);
    ConvertDekartToAphine(SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA, P);
    ConvertAphineToDekart(SKEW_A, SKEW_B, SKEW_C,
        SKEW_ALPHA, SKEW_BETA, SKEW_GAMMA, P);
    AssertVector('round trip', V(1, 2, 3), P);
end;

{ ---- numeric helpers ------------------------------------------------------- }

procedure TSimpMathNumericTest.SphericalAndCartesianRoundTrip;
var
    x, y, z, Theta, Phi, R: double;
begin
    ConvertSphericalToDekart(0.7, 1.1, 2.5, x, y, z);
    ConvertDekartToSpherical(x, y, z, Theta, Phi, R);
    AssertEquals('theta', 0.7, Theta, 1e-9);
    AssertEquals('phi', 1.1, Phi, 1e-9);
    AssertEquals('radius', 2.5, R, 1e-9);
end;

procedure TSimpMathNumericTest.TheSphericalPoleIsOnTheZAxis;
var
    x, y, z: double;
begin
    //  Theta = 0 is the pole: the whole radius goes into z, whatever phi says.
    ConvertSphericalToDekart(0, 1.1, 2.5, x, y, z);
    AssertEquals('x', 0.0, x, 1e-9);
    AssertEquals('y', 0.0, y, 1e-9);
    AssertEquals('z is the radius', 2.5, z, 1e-9);
end;

procedure TSimpMathNumericTest.PhiWrapsAtPi;
var
    Phi: double;
begin
    //  Phi is an azimuth on (-pi, pi], so incrementing past pi comes back at the
    //  bottom rather than growing without bound.
    Phi := 3.0;
    IncPhi(0.5, Phi);
    AssertTrue('wrapped below pi: ' + FloatToStr(Phi), Phi < 0);
    AssertEquals('and by the right amount', -pi + (3.5 - pi), Phi, 1e-9);

    Phi := -3.0;
    DecPhi(0.5, Phi);
    AssertTrue('wrapped above -pi: ' + FloatToStr(Phi), Phi > 0);
end;

procedure TSimpMathNumericTest.ThetaClampsToItsRange;
var
    Theta: double;
begin
    //  Theta is a polar angle on [0, pi] and CLAMPS rather than wrapping - going
    //  past the pole is not the same as coming round the other side.
    Theta := 3.0;
    IncTheta(0.5, Theta);
    AssertEquals('clamped at pi', pi, Theta, 1e-12);

    Theta := 0.2;
    DecTheta(0.5, Theta);
    AssertEquals('clamped at zero', 0.0, Theta, 1e-12);
end;

procedure TSimpMathNumericTest.AValueIsWrappedIntoTheInterval;
var
    Value: double;
begin
    Value := 5.0;
    PutValueIntoInterval(0, 4, Value);
    AssertTrue('inside now: ' + FloatToStr(Value), IsValueIntoInterval(0, 4, Value));
    AssertEquals('one past the top comes in at the bottom', 1.0, Value, 1e-9);

    Value := -1.0;
    PutValueIntoInterval(0, 4, Value);
    AssertTrue('inside now', IsValueIntoInterval(0, 4, Value));
    AssertEquals('one below the bottom comes in at the top', 3.0, Value, 1e-9);

    Value := 2.0;
    PutValueIntoInterval(0, 4, Value);
    AssertEquals('a value already inside is untouched', 2.0, Value, 1e-12);
end;

procedure TSimpMathNumericTest.IntervalMembershipIncludesItsBounds;
begin
    AssertTrue('the lower bound is in', IsValueIntoInterval(0, 1, 0));
    AssertTrue('the upper bound is in', IsValueIntoInterval(0, 1, 1));
    AssertTrue('the middle is in', IsValueIntoInterval(0, 1, 0.5));
    AssertFalse('below is out', IsValueIntoInterval(0, 1, -0.0001));
    AssertFalse('above is out', IsValueIntoInterval(0, 1, 1.0001));
end;

procedure TSimpMathNumericTest.TheDecimalDegreeOfANumber;
begin
    AssertEquals('single digits are order 0', 0, GetNumberDegree(1));
    AssertEquals('', 0, GetNumberDegree(9.5));
    AssertEquals('tens are order 1', 1, GetNumberDegree(10));
    AssertEquals('', 2, GetNumberDegree(345));
    AssertEquals('a fraction is negative', -1, GetNumberDegree(0.5));
    AssertEquals('', -2, GetNumberDegree(0.05));
    //  CHARACTERISATION: zero answers 1, not 0. There is no decimal order of
    //  zero, and the body returns 1 as an early exit rather than as a result.
    AssertEquals('zero answers 1, which is a sentinel not an order',
        1, GetNumberDegree(0));
end;

procedure TSimpMathNumericTest.PowersOfTenAreBuiltByRepeatedMultiplication;
begin
    AssertEquals('ten to the nought', 1.0, GetPowerOf10(0), 1e-12);
    AssertEquals('', 1000.0, GetPowerOf10(3), 1e-12);
    //  A LOOSER TOLERANCE FOR NEGATIVE POWERS, and deliberately: these are built
    //  by multiplying by 0.1 repeatedly, which is not exact in binary, so
    //  GetPowerOf10(-2) is not bit-identical to 0.01. Worth knowing before using
    //  it as a scale factor in a comparison.
    AssertEquals('', 0.1, GetPowerOf10(-1), 1e-15);
    AssertEquals('', 0.01, GetPowerOf10(-2), 1e-15);
end;

procedure TSimpMathNumericTest.SignTreatsZeroAsPositive;
begin
    AssertEquals('positive', 1, Sign(2.5));
    AssertEquals('negative', -1, Sign(-2.5));
    //  CHARACTERISATION: two-valued, not three. Zero answers 1, so this cannot be
    //  used to detect zero - a caller wanting that must test for it separately.
    AssertEquals('zero is positive here', 1, Sign(0));
end;

procedure TSimpMathNumericTest.SortUpOrdersThreeValues;
var
    a, b, c: double;
begin
    a := 3; b := 1; c := 2;
    SortUp(a, b, c);
    AssertEquals('smallest', 1.0, a, 1e-12);
    AssertEquals('middle', 2.0, b, 1e-12);
    AssertEquals('largest', 3.0, c, 1e-12);

    //  Reverse order is the case a two-swap implementation gets wrong.
    a := 3; b := 2; c := 1;
    SortUp(a, b, c);
    AssertEquals('', 1.0, a, 1e-12);
    AssertEquals('', 2.0, b, 1e-12);
    AssertEquals('', 3.0, c, 1e-12);

    //  Already sorted, and all equal.
    a := 1; b := 2; c := 3;
    SortUp(a, b, c);
    AssertEquals('', 1.0, a, 1e-12);
    a := 5; b := 5; c := 5;
    SortUp(a, b, c);
    AssertEquals('', 5.0, b, 1e-12);
end;

procedure TSimpMathNumericTest.ArcCosMatchesTheLibrary;
begin
    AssertEquals('a half', Math.ArcCos(0.5), SimpMath.ArcCos(0.5), 1e-9);
    AssertEquals('zero', pi / 2, SimpMath.ArcCos(0), 1e-9);
    AssertEquals('one', 0.0, SimpMath.ArcCos(1), 1e-9);
    AssertEquals('minus one', pi, SimpMath.ArcCos(-1), 1e-9);
end;

procedure TSimpMathNumericTest.ArcSinMatchesTheLibraryInsideItsRange;
begin
    AssertEquals('a half', Math.ArcSin(0.5), SimpMath.ArcSin(0.5), 1e-9);
    AssertEquals('minus a half', Math.ArcSin(-0.5), SimpMath.ArcSin(-0.5), 1e-9);
    AssertEquals('zero', 0.0, SimpMath.ArcSin(0), 1e-9);
    AssertEquals('one', pi / 2, SimpMath.ArcSin(1), 1e-9);
end;

procedure TSimpMathNumericTest.ArcSinIsWrongAtMinusOne;
begin
    //  A DEFECT, characterised rather than fixed - this unit is in a sibling
    //  repository. arcsin(-1) is -pi/2; this returns +pi/2, because the body
    //  collapses 1 - x*x to zero and then answers pi/2 without consulting the
    //  sign of x.
    //
    //  REACHABLE FROM THE APPLICATION: argument_axis.FromDisplay calls
    //  ArcSin(DisplayValue * FWaveLength) for the sin(theta)/lambda axis. A
    //  round trip through ToDisplay stays positive, so the ordinary path does not
    //  hit it - but nothing constrains a value the user supplies.
    AssertEquals('the library is right', -pi / 2, Math.ArcSin(-1.0), 1e-9);
    AssertEquals('and this is what SimpMath currently answers',
        pi / 2, SimpMath.ArcSin(-1.0), 1e-9);
end;

procedure TSimpMathNumericTest.ArcSinHasNoDomainGuard;
var
    Raised: boolean;
    R: double;
begin
    //  THE SAME DEFECT, and the sharper half of it: outside [-1, 1] the body
    //  takes the square root of a negative number. Depending on the floating-point
    //  exception mask that is either an exception or a NaN, and the caller above
    //  passes a product of two user-influenced values with nothing checking it.
    //
    //  Asserted as "one of the two, and never a plausible number", because which
    //  one happens depends on the mask the host set.
    Raised := False;
    R := 0;
    try
        R := SimpMath.ArcSin(1.5);
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('raises, or answers not-a-number - never a usable angle',
        Raised or IsNan(R));
end;

procedure TSimpMathNumericTest.LagrangePassesThroughItsPoints;
var
    P: TwoDimArray;
begin
    //  The defining property of an interpolating polynomial: at each node it
    //  returns that node's value exactly.
    SetLength(P, 3);
    P[0][1] := 0; P[0][2] := 1;
    P[1][1] := 1; P[1][2] := 3;
    P[2][1] := 2; P[2][2] := 2;
    AssertEquals('at the first node', 1.0, Lagrange(P, 0), 1e-9);
    AssertEquals('at the second', 3.0, Lagrange(P, 1), 1e-9);
    AssertEquals('at the third', 2.0, Lagrange(P, 2), 1e-9);
end;

procedure TSimpMathNumericTest.LagrangeInterpolatesAStraightLineExactly;
var
    P: TwoDimArray;
begin
    //  Two points on y = 2x + 1: the interpolant must be that line everywhere,
    //  which is what distinguishes interpolation from nearest-node lookup.
    SetLength(P, 2);
    P[0][1] := 0; P[0][2] := 1;
    P[1][1] := 2; P[1][2] := 5;
    AssertEquals('between the nodes', 3.0, Lagrange(P, 1), 1e-9);
    AssertEquals('and outside them', 7.0, Lagrange(P, 3), 1e-9);
end;

procedure TSimpMathNumericTest.LagrangeRefusesAnUnassignedArray;
var
    P: TwoDimArray;
    Raised: boolean;
begin
    //  Named exception rather than an access violation: a nil array is a caller
    //  error and the message says which.
    P := nil;
    Raised := False;
    try
        Lagrange(P, 1);
    except
        on EPointsArrayIsNotAssigned do
            Raised := True;
    end;
    AssertTrue('refused by name', Raised);
end;

procedure TSimpMathNumericTest.TheSecondOrderPolynomialIsCentredOnX0;
begin
    //  A*(x0-x)^2 + B*(x0-x) + C, so at x = x0 only C survives. The CENTRING is
    //  the part worth pinning: it is what makes the coefficients mean something
    //  relative to a peak position rather than to the origin.
    AssertEquals('at the centre only the constant remains',
        7.0, CalcPolinom2(2, 3, 7, 5, 5), 1e-12);
    //  x0 - x = 1
    AssertEquals('one to the left of centre',
        2.0 + 3.0 + 7.0, CalcPolinom2(2, 3, 7, 5, 4), 1e-12);
    //  x0 - x = -1, so the linear term changes sign and the square does not.
    AssertEquals('one to the right of centre',
        2.0 - 3.0 + 7.0, CalcPolinom2(2, 3, 7, 5, 6), 1e-12);
end;

{ ---- the lineshapes, as single points -------------------------------------- }

procedure TSimpMathNumericTest.AGaussianPeaksAtItsCentre;
begin
    //  A IS THE AREA, NOT THE HEIGHT - the declaration says so in a comment
    //  ("Integral of function by definition area") and it is easy to miss,
    //  because every other program's Gaussian is written the other way. The
    //  height is A / (Sigma * sqrt(2*pi)), so a test written against A as a
    //  height fails by a factor of the width.
    AssertEquals('the height is the area over the normalisation',
        100 / (1.5 * Sqrt(2 * Pi)), GaussPoint(100, 1.5, 10, 10), 1E-9);
    //  What DOES hold is that the centre is the largest value, which is what
    //  makes a peak a peak.
    AssertTrue('and it is the largest value',
        GaussPoint(100, 1.5, 10, 10) > GaussPoint(100, 1.5, 10, 11));
    AssertTrue('on both sides',
        GaussPoint(100, 1.5, 10, 10) > GaussPoint(100, 1.5, 10, 9));
end;

procedure TSimpMathNumericTest.AGaussianIsSymmetric;
begin
    //  Symmetry is what distinguishes it from every asymmetric shape beside it
    //  in the menu, and it is the property a sign error breaks.
    AssertEquals('either side of the centre',
        GaussPoint(100, 1.5, 10, 8.5), GaussPoint(100, 1.5, 10, 11.5), 1E-9);
end;

procedure TSimpMathNumericTest.AGaussianFallsToNothingFarAway;
begin
    //  It must reach zero rather than a floor: the model is a SUM of curves,
    //  and a shape with a tail that does not vanish adds a constant to every
    //  other peak in the profile.
    AssertEquals('far away', 0.0, GaussPoint(100, 1.5, 10, 1000), 1E-9);
end;

procedure TSimpMathNumericTest.AWiderGaussianIsLowerAwayFromItsCentre;
begin
    //  Sigma is the width, and the fit varies it. If a larger sigma did not
    //  spread the curve, the width parameter would do nothing and the fit would
    //  wander it without effect.
    AssertTrue('wider spreads further',
        GaussPoint(100, 3.0, 10, 13) > GaussPoint(100, 1.5, 10, 13));
end;

procedure TSimpMathNumericTest.ALorentzianPeaksAtItsCentreToo;
begin
    //  Area-normalised as well, and with its own normalisation - so the two
    //  shapes of equal area do NOT have equal heights.
    AssertTrue('the centre is the largest value',
        LorentzPoint(100, 1.5, 10, 10) > LorentzPoint(100, 1.5, 10, 11));
    AssertTrue('and a Lorentzian of the same area is the taller',
        LorentzPoint(100, 1.5, 10, 10) > GaussPoint(100, 1.5, 10, 10));
end;

procedure TSimpMathNumericTest.ALorentzianHasTheHeavierTail;
begin
    //  THE REASON BOTH EXIST. Same amplitude and width, and the Lorentzian is
    //  the one that still has something to say far from the peak - which is
    //  what a user chooses between when they pick a curve type.
    AssertTrue('further out, Lorentz is above Gauss',
        LorentzPoint(100, 1.5, 10, 20) > GaussPoint(100, 1.5, 10, 20));
end;

procedure TSimpMathNumericTest.APseudoVoigtInterpolatesBetweenTheTwo;
var
    FWHM: double;
begin
    //  Eta is the mixing parameter: nothing but a Gaussian at one end, nothing
    //  but a Lorentzian at the other. A shape that did not reduce to its ends
    //  would make eta mean something the user cannot reason about.
    //
    //  BUT SIGMA IS NOT THE SAME SIGMA. GaussPoint's is a standard deviation;
    //  the pseudo-Voigt's is a FULL WIDTH AT HALF MAXIMUM - two conventions
    //  under one parameter name, in one unit. The conversion is the factor
    //  below, and without it these two shapes of "the same width" are different
    //  widths by a factor of 2.35. See findings.md.
    FWHM := 1.5 * 2 * Sqrt(2 * Ln(2));
    AssertEquals('eta = 0 is the Gaussian, once the widths are made to agree',
        GaussPoint(100, 1.5, 10, 12),
        PseudoVoigtPoint(100, FWHM, 0, 10, 12), 1E-6);
end;

procedure TSimpMathNumericTest.APseudoVoigtSitsBetweenItsEnds;
begin
    //  Halfway is between its own two ends everywhere - compared against
    //  ITSELF at eta 0 and 1, so the width convention cancels and what is left
    //  is the mixing, which is what a sign slip there would break.
    AssertTrue('above its Gaussian end',
        PseudoVoigtPoint(100, 3.5, 0.5, 10, 20) >
        PseudoVoigtPoint(100, 3.5, 0, 10, 20));
    AssertTrue('and below its Lorentzian end',
        PseudoVoigtPoint(100, 3.5, 0.5, 10, 20) <
        PseudoVoigtPoint(100, 3.5, 1, 10, 20));
end;

procedure TSimpMathNumericTest.AnAsymmetricShapeIsAsymmetric;
begin
    //  AND IT IS NOT AREA-NORMALISED. Unlike the three above, this one has no
    //  1/Sigma factor at all - A is its peak HEIGHT. Three shapes in one unit,
    //  three conventions for what A means. See findings.md.
    AssertEquals('A is the height here', 100.0,
        AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, 10, 0.5), 1E-9);
    //  With a non-zero DeltaSigma the two flanks differ - which is the whole
    //  point of the shape, and the thing a test of the centre alone cannot see.
    //  DeltaSigma is the LAST argument, after x - which is not where a reader
    //  expects a shape parameter, and is how this test was written wrongly the
    //  first time.
    AssertTrue('the two sides differ',
        Abs(AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, 8, 0.5) -
            AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, 12, 0.5)) > 1E-6);
end;

procedure TSimpMathNumericTest.AnAsymmetricShapeWithNoAsymmetryIsSymmetric;
begin
    //  DeltaSigma = 0 must reduce it to the symmetric case, or a user who does
    //  not want asymmetry cannot turn it off.
    AssertEquals('either side',
        AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, 8, 0),
        AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, 12, 0), 1E-9);
end;

procedure TSimpMathNumericTest.TheTwoBranchShapeUsesEachBranchOnItsOwnSide;
begin
    //  Two widths and two mixings, one pair per side. Given the same values for
    //  both, it has to reduce to the single-branch shape.
    //  Matched branches must reduce to the single-branch shape, or a user who
    //  does not want two widths cannot ask for one.
    AssertEquals('with matched branches the two sides agree',
        TwoBranchesPseudoVoigtPoint(100, 1.5, 0.5, 1.5, 0.5, 10, 8),
        TwoBranchesPseudoVoigtPoint(100, 1.5, 0.5, 1.5, 0.5, 10, 12), 1E-9);
    //  And with different ones, the sides part company.
    AssertTrue('a wider right branch reaches further right',
        TwoBranchesPseudoVoigtPoint(100, 1.5, 0.5, 3.0, 0.5, 10, 14) >
        TwoBranchesPseudoVoigtPoint(100, 1.5, 0.5, 1.5, 0.5, 10, 14));
end;

procedure TSimpMathNumericTest.EveryShapeIsNonNegativeForAPositiveAmplitude;
var
    x: double;
begin
    //  A count cannot be negative, and a shape that dips below zero adds
    //  negative counts to the model where two curves overlap.
    x := 0;
    while x <= 20 do
    begin
        AssertTrue(Format('the Gaussian at %g', [x]),
            GaussPoint(100, 1.5, 10, x) >= 0);
        AssertTrue(Format('the Lorentzian at %g', [x]),
            LorentzPoint(100, 1.5, 10, x) >= 0);
        AssertTrue(Format('the pseudo-Voigt at %g', [x]),
            PseudoVoigtPoint(100, 1.5, 0.5, 10, x) >= 0);
        AssertTrue(Format('the asymmetric shape at %g', [x]),
            AsymPseudoVoigtPoint(100, 1.5, 0.5, 10, x, 0.5) >= 0);
        x := x + 0.5;
    end;
end;

{ ---- the quadratic the background is built from ---------------------------- }

procedure TSimpMathNumericTest.TheQuadraticIsItsConstantAtItsOwnCentre;
begin
    //  Written about a centre rather than about the origin, so that C is the
    //  value AT the centre - which is what makes it seedable from the data.
    AssertEquals('at the centre', 5.0, CalcPolinom2(2, 3, 5, 10, 10), 1E-9);
end;

procedure TSimpMathNumericTest.TheQuadraticIsSymmetricWhenItsLinearTermIsZero;
begin
    AssertEquals('either side',
        CalcPolinom2(2, 0, 5, 10, 8), CalcPolinom2(2, 0, 5, 10, 12), 1E-9);
end;

procedure TSimpMathNumericTest.TheLinearTermTiltsTheQuadratic;
begin
    //  With B non-zero the two sides part, which is what lets a background
    //  slope.
    AssertTrue('tilted',
        CalcPolinom2(2, 3, 5, 10, 8) <> CalcPolinom2(2, 3, 5, 10, 12));
end;

{ The affine cell helpers - GetD and the three cofactors - are declared only in
  the implementation, so nothing outside SimpMath can call them. They are
  reached through the affine scalar product, modulus and angle, which the
  vector tests above drive in a skewed basis; that is the only way in, and it
  is enough. }

{ ---- stepping an angle ----------------------------------------------------- }

procedure TSimpMathNumericTest.IncreasingPhiWrapsRatherThanRunningOn;
var
    Phi: double;
begin
    //  Phi goes round. A step past the wrap must come out the other side, or a
    //  search that steps it walks off into angles that name no direction.
    Phi := 3.0;
    IncPhi(1.0, Phi);
    AssertTrue('wrapped into range', (Phi >= -Pi) and (Phi <= Pi));
end;

procedure TSimpMathNumericTest.DecreasingPhiWrapsTheOtherWay;
var
    Phi: double;
begin
    Phi := -3.0;
    DecPhi(1.0, Phi);
    AssertTrue('wrapped into range', (Phi >= -Pi) and (Phi <= Pi));
end;

procedure TSimpMathNumericTest.SteppingThetaStaysInsideItsRange;
var
    Theta: double;
begin
    //  Theta does NOT go round - it is a polar angle, and past the pole is the
    //  same direction again. It clamps.
    Theta := 3.0;
    IncTheta(1.0, Theta);
    AssertTrue('clamped', (Theta >= 0) and (Theta <= Pi));
    Theta := 0.1;
    DecTheta(1.0, Theta);
    AssertTrue('clamped the other way', (Theta >= 0) and (Theta <= Pi));
end;

{ ---- the lineshapes as array fillers --------------------------------------- }

const
    { One parameter set for all five, so a reader can compare the filled
      columns across shapes. x0 = 10 with abscissae on both sides of it. }
    SH_A = 100.0;
    SH_SIGMA = 1.5;
    SH_ETA = 0.4;
    SH_X0 = 10.0;
    SH_DELTA = 0.5;
    SH_SIGMA_R = 3.0;
    SH_ETA_R = 0.8;

{ Five one-line adapters, so the nil check below can be one loop over the five
  rather than five copies of the same try/except. }
procedure FillGauss(APoints: TwoDimArray);
begin
    Gauss(APoints, SH_A, SH_SIGMA, SH_X0);
end;

procedure FillLorentz(APoints: TwoDimArray);
begin
    Lorentz(APoints, SH_A, SH_SIGMA, SH_X0);
end;

procedure FillPseudoVoigt(APoints: TwoDimArray);
begin
    PseudoVoigt(APoints, SH_A, SH_SIGMA, SH_ETA, SH_X0);
end;

procedure FillAsym(APoints: TwoDimArray);
begin
    AsymPseudoVoigt(APoints, SH_A, SH_SIGMA, SH_ETA, SH_X0, SH_DELTA);
end;

procedure FillTwoBranches(APoints: TwoDimArray);
begin
    TwoBranchesPseudoVoigt(APoints, SH_A, SH_SIGMA, SH_ETA,
        SH_SIGMA_R, SH_ETA_R, SH_X0);
end;

procedure TSimpMathShapeArrayTest.GivenAbscissae;
var
    i: longint;
begin
    SetLength(FPoints, 5);
    //  8, 9, 10, 11, 12 - two left of x0, the centre, two right of it.
    for i := 0 to 4 do
    begin
        FPoints[i][1] := 8 + i;
        //  A value the filler must overwrite. If a shape skipped a point this
        //  would survive, and a comparison against the formula would then be
        //  comparing a leftover rather than a computed ordinate.
        FPoints[i][2] := -1;
    end;
end;

procedure TSimpMathShapeArrayTest.CheckColumnTwo(const AMessage: string;
    const AExpected: array of double);
var
    i: longint;
begin
    AssertEquals(AMessage + ': one value per point',
        Length(AExpected), Length(FPoints));
    for i := 0 to Length(FPoints) - 1 do
        AssertEquals(AMessage + ' at x = ' + FloatToStr(FPoints[i][1]),
            AExpected[i], FPoints[i][2], 1e-9);
end;

procedure TSimpMathShapeArrayTest.CheckRefusesNil(const AMessage: string;
    AFill: TShapeFill; APoints: TwoDimArray);
var
    Raised: boolean;
begin
    Raised := False;
    try
        AFill(APoints);
    except
        on EPointsArrayIsNotAssigned do
            Raised := True;
    end;
    AssertTrue(AMessage + ': refused by name', Raised);
end;

procedure TSimpMathShapeArrayTest.GaussFillsEveryPointFromItsOwnFormula;
begin
    GivenAbscissae;
    FillGauss(FPoints);
    CheckColumnTwo('gauss', [
        GaussPoint(SH_A, SH_SIGMA, SH_X0, 8),
        GaussPoint(SH_A, SH_SIGMA, SH_X0, 9),
        GaussPoint(SH_A, SH_SIGMA, SH_X0, 10),
        GaussPoint(SH_A, SH_SIGMA, SH_X0, 11),
        GaussPoint(SH_A, SH_SIGMA, SH_X0, 12)]);
end;

procedure TSimpMathShapeArrayTest.LorentzFillsEveryPointFromItsOwnFormula;
begin
    GivenAbscissae;
    FillLorentz(FPoints);
    CheckColumnTwo('lorentz', [
        LorentzPoint(SH_A, SH_SIGMA, SH_X0, 8),
        LorentzPoint(SH_A, SH_SIGMA, SH_X0, 9),
        LorentzPoint(SH_A, SH_SIGMA, SH_X0, 10),
        LorentzPoint(SH_A, SH_SIGMA, SH_X0, 11),
        LorentzPoint(SH_A, SH_SIGMA, SH_X0, 12)]);
end;

procedure TSimpMathShapeArrayTest.PseudoVoigtFillsEveryPointFromItsOwnFormula;
begin
    GivenAbscissae;
    FillPseudoVoigt(FPoints);
    CheckColumnTwo('pseudo-voigt', [
        PseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 8),
        PseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 9),
        PseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 10),
        PseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 11),
        PseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 12)]);
end;

procedure TSimpMathShapeArrayTest.AsymPseudoVoigtFillsEveryPointFromItsOwnFormula;
begin
    //  x0 and x are adjacent arguments of the same type here. Swapping them
    //  would leave a peak of the same height in the same family of shapes -
    //  visible only as a curve that will not fit. Hence a point-by-point
    //  comparison rather than a shape assertion.
    GivenAbscissae;
    FillAsym(FPoints);
    CheckColumnTwo('asymmetric', [
        AsymPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 8, SH_DELTA),
        AsymPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 9, SH_DELTA),
        AsymPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 10, SH_DELTA),
        AsymPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 11, SH_DELTA),
        AsymPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA, SH_X0, 12, SH_DELTA)]);
end;

procedure TSimpMathShapeArrayTest.TwoBranchesFillsEveryPointFromItsOwnFormula;
begin
    //  Deliberately unequal branches - SigmaRight is twice Sigma and EtaRight
    //  is not Eta - so that a filler which passed the left parameters to both
    //  sides, or the abscissa in x0's place, cannot match.
    GivenAbscissae;
    FillTwoBranches(FPoints);
    CheckColumnTwo('two-branch', [
        TwoBranchesPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA,
            SH_SIGMA_R, SH_ETA_R, SH_X0, 8),
        TwoBranchesPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA,
            SH_SIGMA_R, SH_ETA_R, SH_X0, 9),
        TwoBranchesPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA,
            SH_SIGMA_R, SH_ETA_R, SH_X0, 10),
        TwoBranchesPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA,
            SH_SIGMA_R, SH_ETA_R, SH_X0, 11),
        TwoBranchesPseudoVoigtPoint(SH_A, SH_SIGMA, SH_ETA,
            SH_SIGMA_R, SH_ETA_R, SH_X0, 12)]);
end;

procedure TSimpMathShapeArrayTest.FillingLeavesTheAbscissaeAlone;
var
    i: longint;
begin
    //  Column 1 belongs to the caller - a model curve keeps its own abscissae
    //  and recalculates the ordinates on every parameter change. A filler that
    //  wrote column 1 would move the curve out from under the data.
    GivenAbscissae;
    FillGauss(FPoints);
    FillLorentz(FPoints);
    FillPseudoVoigt(FPoints);
    FillAsym(FPoints);
    FillTwoBranches(FPoints);
    for i := 0 to Length(FPoints) - 1 do
        AssertEquals('abscissa ' + IntToStr(i) + ' after five fills',
            8.0 + i, FPoints[i][1], 1e-12);
end;

procedure TSimpMathShapeArrayTest.EveryShapeRefusesAnUnassignedArray;
begin
    //  A named exception, not an access violation, from all five - a nil array
    //  is a caller error and the message says which.
    CheckRefusesNil('gauss', @FillGauss, nil);
    CheckRefusesNil('lorentz', @FillLorentz, nil);
    CheckRefusesNil('pseudo-voigt', @FillPseudoVoigt, nil);
    CheckRefusesNil('asymmetric', @FillAsym, nil);
    CheckRefusesNil('two-branch', @FillTwoBranches, nil);
end;

procedure TSimpMathShapeArrayTest.AnEmptyArrayIsRefusedAsIfItWereUnassigned;
var
    P: TwoDimArray;
begin
    //  CHARACTERISED, NOT ENDORSED. SetLength(P, 0) leaves P nil in FPC, so a
    //  dynamic array cannot tell "no points" apart from "no array" and the
    //  Assigned() guard rejects both. A curve over an empty range therefore
    //  raises rather than doing nothing, which is the more surprising of the
    //  two readings; it is pinned here so that a filler which ever grows a
    //  Length = 0 early return is seen to be a change in behaviour.
    SetLength(P, 0);
    CheckRefusesNil('an empty array', @FillGauss, P);
end;


initialization
    //  Unit tests: pure functions over doubles and fixed-size arrays.
    RegisterTest('unit', TSimpMathVectorTest);
    RegisterTest('unit', TSimpMathNumericTest);
    RegisterTest('unit', TSimpMathShapeArrayTest);
end.
