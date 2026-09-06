// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a curve's position is shown in the parameter table, and read back
when it is typed.)

THE TABLE SHOWS THE AXIS THE USER PICKED. A curve's position is stored once, in
the abscissa the data was measured in, and shown in whichever axis the menu is
set to - so the value in the cell is a TRANSFORM of the value in the model, and
typing into the cell has to run the transform backwards.

WHERE THAT GOES WRONG. If the two directions disagree, reading a cell and typing
the same number back moves the curve. Nothing reports it: the number the user
sees is the number they typed, and the model underneath has changed. On the
identity axis - which is the default, and every screenshot - the transform is
nothing at all, so the defect cannot appear.

AND ONLY POSITIONS ARE TRANSFORMED. An amplitude is a count and a width is a
width; putting them through an axis conversion would scale values that are not
positions, which is the other half of the same rule.
}
unit testcase_curve_list_axis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    mscr_specimen_list, special_curve_parameter, amplitude_curve_parameter,
    argument_axis;

type
    { The two transforms are PROTECTED - the grid presenter calls them through
      inheritance, not from outside - so a descendant is how they are reached,
      here as there. It exposes them and does nothing else. }
    TTestableCurveList = class(TMSCRCurveList)
    public
        function Shown(P: TSpecialCurveParameter): double;
        procedure Typed(P: TSpecialCurveParameter; AValue: double);
    end;

    TCurveListAxisTest = class(TTestCase)
    private
        FList: TTestableCurveList;
        { The parameters this fixture made, so it can free them. }
        FOwned: TList;
        function AParameter(AType: TParameterType;
            AValue: double): TSpecialCurveParameter;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Which parameters are transformed at all.
        procedure AFixedPositionIsShownOnTheChosenAxis;
        procedure AVariedPositionIsTransformedToo;
        procedure AnAmplitudeIsShownAsItIs;
        procedure AWidthIsShownAsItIs;
        procedure TheAbscissaParameterIsShownAsItIs;

        //  That the two directions agree.
        procedure ShowingAndReadingBackIsTheIdentity;
        procedure ReadingBackAndShowingIsTheIdentityToo;
        procedure TypingTheDisplayedValueLeavesTheModelAlone;

        //  On the plain axis.
        procedure OnTheIdentityAxisNothingIsTransformed;

        //  What the list carries with it. Driven on a plain TMSCRCurveList,
        //  not on the descendant above - see the note by the first of them.
        procedure ACopyCarriesTheAxisSettings;
        procedure ACopyCarriesTheWaveLength;
        procedure ACopyIsIndependentOfItsOriginal;
        procedure ADescendantCannotCopyItself;
    end;

implementation

function TTestableCurveList.Shown(P: TSpecialCurveParameter): double;
begin
    Result := RecalcParamValue(P);
end;

procedure TTestableCurveList.Typed(P: TSpecialCurveParameter; AValue: double);
begin
    ReverseCalcParamValue(P, AValue);
end;

procedure TCurveListAxisTest.SetUp;
begin
    FList := TTestableCurveList.Create;
    FOwned := TList.Create;
    //  The diffraction axis, because it is the one whose transform is not the
    //  identity - which is what makes the round trip worth asserting.
    FList.FViewMode := XCM_SINTL;
    FList.FWaveLength := 1.5406;
end;

procedure TCurveListAxisTest.TearDown;
var
    i: longint;
begin
    for i := 0 to FOwned.Count - 1 do
        TSpecialCurveParameter(FOwned[i]).Free;
    FreeAndNil(FOwned);
    FreeAndNil(FList);
end;

function TCurveListAxisTest.AParameter(AType: TParameterType;
    AValue: double): TSpecialCurveParameter;
begin
    //  A concrete class: TSpecialCurveParameter declares abstract methods, so a
    //  bare instance faults as soon as anything real touches it.
    Result := TAmplitudeCurveParameter.Create;
    Result.Name := 'p';
    Result.Type_ := AType;
    Result.Value := AValue;
    FOwned.Add(Result);
end;

{ ---- which parameters are transformed -------------------------------------- }

procedure TCurveListAxisTest.AFixedPositionIsShownOnTheChosenAxis;
var
    P: TSpecialCurveParameter;
begin
    P := AParameter(InvariablePosition, 60);
    AssertTrue('it was transformed',
        Abs(FList.Shown(P) - 60) > 1E-9);
end;

procedure TCurveListAxisTest.AVariedPositionIsTransformedToo;
var
    P: TSpecialCurveParameter;
begin
    //  BOTH KINDS OF POSITION. They differ in whether the fit may move them,
    //  not in what they mean - so a rule that transformed only the fixed one
    //  would show a fitted position on the wrong axis.
    P := AParameter(VariablePosition, 60);
    AssertTrue('it was transformed',
        Abs(FList.Shown(P) - 60) > 1E-9);
    AssertEquals('and the same way as a fixed one',
        FList.Shown(AParameter(InvariablePosition, 60)),
        FList.Shown(P), 1E-12);
end;

procedure TCurveListAxisTest.AnAmplitudeIsShownAsItIs;
var
    P: TSpecialCurveParameter;
begin
    //  A count is not a position. Putting it through an axis conversion would
    //  scale a height by a trigonometric function of an angle it has nothing to
    //  do with.
    P := AParameter(Amplitude, 100);
    AssertEquals('unchanged', 100.0, FList.Shown(P), 1E-12);
end;

procedure TCurveListAxisTest.AWidthIsShownAsItIs;
var
    P: TSpecialCurveParameter;
begin
    //  Arguably it should be - a width in one axis is a different width in
    //  another - but it is not, and that is worth having written down rather
    //  than discovered.
    P := AParameter(special_curve_parameter.Width, 1.5);
    AssertEquals('unchanged', 1.5, FList.Shown(P), 1E-12);
end;

procedure TCurveListAxisTest.TheAbscissaParameterIsShownAsItIs;
var
    P: TSpecialCurveParameter;
begin
    //  The formula's own variable, which is not a position of anything.
    P := AParameter(Argument, 60);
    AssertEquals('unchanged', 60.0, FList.Shown(P), 1E-12);
end;

{ ---- that the two directions agree ----------------------------------------- }

procedure TCurveListAxisTest.ShowingAndReadingBackIsTheIdentity;
var
    P: TSpecialCurveParameter;
    Shown: double;
begin
    //  THE ROUND TRIP, and the reason this file exists. Read a cell, type the
    //  same number back, and the model must not move - because that is exactly
    //  what a user does when they edit one cell of a row and tab past the rest.
    P := AParameter(InvariablePosition, 60);
    Shown := FList.Shown(P);
    FList.Typed(P, Shown);
    AssertEquals('the model is where it was', 60.0, P.Value, 1E-9);
end;

procedure TCurveListAxisTest.ReadingBackAndShowingIsTheIdentityToo;
var
    P: TSpecialCurveParameter;
    Typed: double;
begin
    //  The other direction: a value typed in comes back out looking like what
    //  was typed. A user who types 0.3 and sees 0.2997 has been told their
    //  entry was not taken.
    P := AParameter(InvariablePosition, 0);
    Typed := 0.32;
    FList.Typed(P, Typed);
    AssertEquals('shown as typed', Typed, FList.Shown(P), 1E-9);
end;

procedure TCurveListAxisTest.TypingTheDisplayedValueLeavesTheModelAlone;
var
    P: TSpecialCurveParameter;
    i: longint;
    Shown: double;
begin
    //  Repeated, because a transform that loses a little each way looks correct
    //  once and drifts over a session of editing.
    P := AParameter(InvariablePosition, 60);
    for i := 1 to 20 do
    begin
        Shown := FList.Shown(P);
        FList.Typed(P, Shown);
    end;
    AssertEquals('after twenty round trips', 60.0, P.Value, 1E-6);
end;

{ ---- on the plain axis ----------------------------------------------------- }

procedure TCurveListAxisTest.OnTheIdentityAxisNothingIsTransformed;
var
    P: TSpecialCurveParameter;
begin
    //  THE DEFAULT, and why the defect above is invisible in practice: with the
    //  identity axis the transform is nothing at all, so a broken one and a
    //  correct one behave identically.
    FList.FViewMode := XCM_IDENTITY;
    P := AParameter(InvariablePosition, 60);
    AssertEquals('shown as stored', 60.0, FList.Shown(P), 1E-12);
    FList.Typed(P, 42);
    AssertEquals('and stored as typed', 42.0, P.Value, 1E-12);
end;

{ ---- what the list carries with it ----------------------------------------- }

procedure TCurveListAxisTest.ACopyCarriesTheAxisSettings;
var
    Plain, Copy: TMSCRCurveList;
begin
    //  A copy that lost the axis would show every position on the identity axis
    //  while the original showed them transformed - and the copy is what the
    //  client hands the grid.
    //
    //  ON A PLAIN LIST, because GetCopy constructs TMSCRCurveList by name - see
    //  ADescendantCannotCopyItself below.
    Plain := TMSCRCurveList.Create;
    try
        Plain.FViewMode := XCM_CUSTOM;
        Plain.FCustomName := 'Energy';
        Plain.FCustomUnit := 'eV';
        Plain.FCustomForward := '1239.84/x';
        Plain.FCustomInverse := '1239.84/x';
        Copy := TMSCRCurveList(Plain.GetCopy);
        AssertEquals('the mode', XCM_CUSTOM, Copy.FViewMode);
        AssertEquals('the name', 'Energy', Copy.FCustomName);
        AssertEquals('the unit', 'eV', Copy.FCustomUnit);
        AssertEquals('the formula', '1239.84/x', Copy.FCustomForward);
        AssertEquals('and its inverse', '1239.84/x', Copy.FCustomInverse);
        Copy.Free;
    finally
        Plain.Free;
    end;
end;

procedure TCurveListAxisTest.ACopyCarriesTheWaveLength;
var
    Plain, Copy: TMSCRCurveList;
begin
    //  Without it the copy cannot answer for the diffraction axis at all - the
    //  conversion divides by it.
    Plain := TMSCRCurveList.Create;
    try
        Plain.FWaveLength := 1.5406;
        Copy := TMSCRCurveList(Plain.GetCopy);
        AssertEquals('the wavelength', 1.5406, Copy.FWaveLength, 1E-12);
        Copy.Free;
    finally
        Plain.Free;
    end;
end;

procedure TCurveListAxisTest.ACopyIsIndependentOfItsOriginal;
var
    Plain, Copy: TMSCRCurveList;
begin
    //  Changing the axis on one must not change it on the other, or switching
    //  the menu would rewrite a list somebody else is drawing from.
    Plain := TMSCRCurveList.Create;
    try
        Plain.FViewMode := XCM_SINTL;
        Copy := TMSCRCurveList(Plain.GetCopy);
        Copy.FViewMode := XCM_IDENTITY;
        AssertEquals('the original kept its own', XCM_SINTL, Plain.FViewMode);
        Copy.Free;
    finally
        Plain.Free;
    end;
end;

procedure TCurveListAxisTest.ADescendantCannotCopyItself;
var
    Raised: boolean;
    Copy: TObject;
begin
    //  GetCopy CONSTRUCTS TMSCRCurveList BY NAME rather than by the class it is
    //  called on, and CopyParameters then asserts the two classes match - so a
    //  descendant asking for a copy of itself fails an assertion rather than
    //  getting a copy.
    //
    //  Latent: nothing in either repository derives from this class today, and
    //  this fixture only does so to reach two protected methods. Pinned so that
    //  whoever adds the first real descendant finds a test rather than an
    //  assertion in the middle of a refresh. See findings.md.
    Raised := False;
    Copy := nil;
    try
        Copy := FList.GetCopy;
    except
        on Exception do
            Raised := True;
    end;
    Copy.Free;
    AssertTrue('a descendant cannot copy itself', Raised);
end;

initialization
    //  A unit test: a curve list and a parameter in memory. No grid, no chart.
    RegisterTest('unit', TCurveListAxisTest);
end.
