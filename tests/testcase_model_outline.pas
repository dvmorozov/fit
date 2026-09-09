// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Model panel shows when the framework fills it.)

WHY THESE TESTS EXIST. The panel is shared: for a model built from picks the
framework describes it, and for one placed from its own markup the module that
placed it does. Which of the two fills it is the load-bearing decision, and
getting it the other way round is not cosmetic - deriving it from "does the
contributor have rows?" is what named_points_set records as having generated one
curve per data point and presented as a hang.

The rest is what a row says. Six Gaussians all read "Gaussian", so the position
is what tells the user which one they are looking at, and the handle is what
every operation on one curve takes.
}
unit testcase_model_outline;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, model_outline, module_view_types;

type
    TModelOutlineTest = class(TTestCase)
    private
        FRows: TModelCurveRows;
        procedure AddCurve(const ATitle, AId: string; APosition: double;
            AHasPosition: boolean = True);
        function Outline: TOutline;
    protected
        procedure SetUp; override;
    published
        //  Which contributor fills the panel.
        procedure ATypePlacedByPicksIsTheFrameworksToDescribe;
        procedure ATypePlacedFromItsOwnMarkupIsNot;
        procedure APointSetNameOfOnlySpacesIsNoPointSetName;

        //  What a row reads.
        procedure ARowNamesItsCurveAndWhereItIs;
        procedure ACurveWithNoPositionJustNamesItself;
        procedure ACurveWithNoTitleStillReadsAsSomething;
        procedure TwoCurvesOfOneTypeAreToldApartByPosition;

        //  The outline itself.
        procedure OneRowPerCurveInTheModelsOwnOrder;
        procedure EveryRowIsFlat;
        procedure ARowCarriesItsCurvesHandle;
        procedure ARowWhoseCurveHasNoHandleCarriesNone;
        procedure ARowNamesTheCurveItStandsFor;

        //  Which curve a selected row names, whoever filled the panel.
        procedure AFrameworkRowNamesItsOwnCurve;
        procedure APackRowNamesTheCurveThePackGaveIt;
        procedure ARowThatNamesNoCurveOffersNoHandle;
        procedure ARowIdNoRowCarriesNamesNoCurve;
        procedure NoSelectionNamesNoCurve;
        procedure NoRowIsEverDetached;
        procedure AModelWithNoCurvesIsAnEmptyOutline;

        //  What it says when there is nothing to show.
        procedure WithNothingOpenItSaysToOpenSomething;
        procedure WithAProfileButNoCurvesItSaysHowToPlaceOne;
        procedure ForAMarkupTypeItDoesNotSayHowToPlaceOne;
        procedure TheEmptyTextIsNeverBlank;
    end;

implementation

procedure TModelOutlineTest.SetUp;
begin
    FRows := nil;
end;

procedure TModelOutlineTest.AddCurve(const ATitle, AId: string;
    APosition: double; AHasPosition: boolean);
var
    N: longint;
begin
    N := Length(FRows);
    SetLength(FRows, N + 1);
    FRows[N].Title := ATitle;
    FRows[N].InstanceId := AId;
    FRows[N].Position := APosition;
    FRows[N].HasPosition := AHasPosition;
end;

function TModelOutlineTest.Outline: TOutline;
begin
    Result := ModelOutlineOf(FRows);
end;

{ ---- which contributor fills the panel ---- }

procedure TModelOutlineTest.ATypePlacedByPicksIsTheFrameworksToDescribe;
begin
    //  An empty PlacedByPointSet means one pick per curve, which is a flat list
    //  the framework can build from the curves themselves.
    AssertTrue('the framework fills it', FrameworkFillsStructure(''));
end;

procedure TModelOutlineTest.ATypePlacedFromItsOwnMarkupIsNot;
begin
    //  THE DECISION THAT MATTERS. A type naming its own point set is placed by
    //  marking an extent in it, and only whoever owns that markup can say what
    //  it produced. Answering the other way here is what generated one curve
    //  per data point.
    AssertFalse('its own contributor does',
        FrameworkFillsStructure('some-markup-set'));
end;

procedure TModelOutlineTest.APointSetNameOfOnlySpacesIsNoPointSetName;
begin
    //  The same reading curve_type_menu applies to a group of only spaces: a
    //  name the user cannot see the difference from empty is empty.
    AssertTrue('still the framework', FrameworkFillsStructure('   '));
end;

{ ---- what a row reads ---- }

procedure TModelOutlineTest.ARowNamesItsCurveAndWhereItIs;
begin
    AddCurve('Gaussian', 'A1', 23.5);
    AssertEquals('Gaussian  at 23.5', ModelRowCaption(FRows[0]));
end;

procedure TModelOutlineTest.ACurveWithNoPositionJustNamesItself;
begin
    //  A curve with no position parameter has nowhere to report, and a made-up
    //  zero would read as a curve at the origin.
    AddCurve('User Defined', 'A1', 0, False);
    AssertEquals('User Defined', ModelRowCaption(FRows[0]));
end;

procedure TModelOutlineTest.ACurveWithNoTitleStillReadsAsSomething;
begin
    AddCurve('', 'A1', 5);
    //  Better a word than a blank row: a blank row is indistinguishable from a
    //  broken one.
    AssertEquals('Curve  at 5', ModelRowCaption(FRows[0]));
end;

procedure TModelOutlineTest.TwoCurvesOfOneTypeAreToldApartByPosition;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'A1', 12.25);
    AddCurve('Gaussian', 'A2', 23.5);
    O := Outline;
    //  THE WHOLE REASON THE POSITION IS SHOWN. Six Gaussians all read
    //  "Gaussian", and the user is looking for the one at 23.5.
    AssertTrue('the rows differ', O[0].Caption <> O[1].Caption);
end;

{ ---- the outline ---- }

procedure TModelOutlineTest.OneRowPerCurveInTheModelsOwnOrder;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'A1', 1);
    AddCurve('Lorentzian', 'A2', 2);
    AddCurve('Voigt', 'A3', 3);
    O := Outline;
    AssertEquals('three rows', 3, Length(O));
    //  The model's order, not sorted: it follows the intervals and the picks
    //  inside them, and re-sorting would put the panel out of step with the
    //  parameter table and the chart's colours.
    AssertEquals('A1', O[0].Id);
    AssertEquals('A2', O[1].Id);
    AssertEquals('A3', O[2].Id);
end;

procedure TModelOutlineTest.EveryRowIsFlat;
var
    O: TOutline;
    i: longint;
begin
    AddCurve('Gaussian', 'A1', 1);
    AddCurve('Gaussian', 'A2', 2);
    O := Outline;
    //  Flat is the answer, not a placeholder for a hierarchy: a model built
    //  from picks has no nesting to show.
    for i := 0 to High(O) do
        AssertEquals('indent', 0, O[i].Indent);
end;

procedure TModelOutlineTest.ARowCarriesItsCurvesHandle;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'ABC123', 1);
    O := Outline;
    //  THE HANDLE, because that is what deleting a curve takes - and an index
    //  would name a different curve after any edit that reorders the model.
    AssertEquals('ABC123', O[0].Id);
end;

procedure TModelOutlineTest.ARowWhoseCurveHasNoHandleCarriesNone;
var
    O: TOutline;
begin
    AddCurve('Gaussian', '', 1);
    O := Outline;
    //  Shown, but not addressable. The commands that need a handle stay
    //  disabled over it rather than acting on whichever curve is nearby.
    AssertEquals('', O[0].Id);
end;

procedure TModelOutlineTest.NoRowIsEverDetached;
var
    O: TOutline;
    i: longint;
begin
    AddCurve('Gaussian', 'A1', 1);
    O := Outline;
    //  A flat list has no parent to lose, so the damage flag a module's
    //  hierarchy needs never applies here.
    for i := 0 to High(O) do
        AssertFalse('detached', O[i].IsDetached);
end;

procedure TModelOutlineTest.AModelWithNoCurvesIsAnEmptyOutline;
begin
    //  Empty rather than a row saying "empty": what an empty panel says is the
    //  panel's business, and it depends on WHY - see below.
    AssertEquals('no rows', 0, Length(Outline));
end;

procedure TModelOutlineTest.ARowNamesTheCurveItStandsFor;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'ABC123', 1);
    O := Outline;
    //  THE FRAMEWORK'S OWN ROWS name the curve twice - as the row's identity
    //  and as the curve it stands for - because for these two they ARE the same
    //  handle. A pack's rows are identified by its own markup and name the
    //  curve separately, which is the whole reason the field exists.
    AssertEquals('ABC123', O[0].CurveId);
end;

{ ---- which curve a selected row names ---- }

function RowNaming(const AId, ACurveId: string): TOutlineRow;
begin
    Result := Default(TOutlineRow);
    Result.Id := AId;
    Result.CurveId := ACurveId;
end;

procedure TModelOutlineTest.AFrameworkRowNamesItsOwnCurve;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'ABC123', 1);
    O := Outline;
    AssertEquals('ABC123', CurveHandleForRowId(O, 'ABC123'));
end;

procedure TModelOutlineTest.APackRowNamesTheCurveThePackGaveIt;
var
    O: TOutline;
begin
    //  THE DEFECT THIS EXISTS FOR. A pack identifies its rows by its own markup
    //  - a wave guid, not a curve handle - and the window answered this by
    //  asking whose rows these were, giving nothing for every row a pack put
    //  there. Delete curve was greyed over every pattern in a wave count, and
    //  no framework command could ever apply to one.
    SetLength(O, 1);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    AssertEquals('{CURVE-1}', CurveHandleForRowId(O, 'WAVE-1'));
end;

procedure TModelOutlineTest.ARowThatNamesNoCurveOffersNoHandle;
var
    O: TOutline;
begin
    //  A row a contributor put there for its own reasons - a heading, a note -
    //  stands for no curve, and the commands that need one stay disabled over
    //  it rather than acting on whichever curve is nearby.
    SetLength(O, 1);
    O[0] := RowNaming('NOTE-1', '');
    AssertEquals('', CurveHandleForRowId(O, 'NOTE-1'));
end;

procedure TModelOutlineTest.ARowIdNoRowCarriesNamesNoCurve;
var
    O: TOutline;
begin
    //  The panel is rebuilt under the selection, so an id that named a row a
    //  moment ago may name none now. Nothing rather than a guess.
    SetLength(O, 1);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    AssertEquals('', CurveHandleForRowId(O, 'WAVE-2'));
end;

procedure TModelOutlineTest.NoSelectionNamesNoCurve;
var
    O: TOutline;
begin
    //  An empty selection must not match a row that carries no id of its own -
    //  the empty-panel row is exactly such a row, and matching it would offer
    //  the commands over the sentence that says the panel is empty.
    SetLength(O, 1);
    O[0] := RowNaming('', '{CURVE-1}');
    AssertEquals('', CurveHandleForRowId(O, ''));
end;

{ ---- the empty text ---- }

procedure TModelOutlineTest.WithNothingOpenItSaysToOpenSomething;
begin
    AssertTrue('names the file',
        Pos('data file', EmptyStructureText(False, True)) > 0);
end;

procedure TModelOutlineTest.WithAProfileButNoCurvesItSaysHowToPlaceOne;
begin
    //  "Nothing here" is not something the user can act on. Naming where the
    //  positions are is.
    AssertTrue('names the Tools tab',
        Pos('Tools', EmptyStructureText(True, True)) > 0);
end;

procedure TModelOutlineTest.ForAMarkupTypeItDoesNotSayHowToPlaceOne;
var
    S: string;
begin
    S := EmptyStructureText(True, False);
    //  The framework does not know what the markup is, so it must not tell the
    //  user how to make it - that would be one contributor's panel explaining
    //  itself in another's words.
    AssertTrue('says the type places itself', Pos('markup', S) > 0);
    AssertTrue('and not how', Pos('Tools', S) = 0);
end;

procedure TModelOutlineTest.TheEmptyTextIsNeverBlank;
begin
    //  An empty box is indistinguishable from a broken one, and this panel is
    //  empty in three quite different situations.
    AssertTrue('nothing open', EmptyStructureText(False, True) <> '');
    AssertTrue('open, framework', EmptyStructureText(True, True) <> '');
    AssertTrue('open, contributor', EmptyStructureText(True, False) <> '');
end;

initialization
    //  A unit test: records in, rows out. No window, no tree control and no
    //  module.
    RegisterTest('unit', TModelOutlineTest);
end.
