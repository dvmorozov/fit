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

        //  Whose rows the panel shows.
        procedure AModulesTreeIsShownWhateverTypeIsSelected;
        procedure AModuleWithNothingPlacedLeavesTheFrameworksRows;
        procedure CurvesNoModuleRowNamesAreListedAfterItsTree;
        procedure ACurveAModuleRowNamesIsListedOnce;
        procedure TheSelectedTypesModuleIsPreferredWhenTwoDescribeTheModel;
        procedure WithNothingAnywhereTheSelectedModuleSaysWhy;
        procedure WithNothingAnywhereAPickedTypeIsTheFrameworks;
        procedure OnlyTheLatestPushOfAPanelIsHeld;
        procedure APushIsComposedAndKept;
        procedure TheFrameworksRowsAreKeptAsTheirOwn;
        procedure RowsNoModuleClaimsAreShownAsTheyCome;
        procedure AShowLinkToAKeptRowSaysToSelectItsType;
        procedure AShowLinkToRowsOfAnotherModelSaysTheyAreGone;
        procedure RowsNamingNoCurveOfTheModelAreNotShown;
        procedure RowsNamingNoCurveAreNotShownEvenForTheSelectedType;
        procedure OneRowNamingACurveOfTheModelIsEnough;
        procedure RowsCarryingNoHandleAreShownRatherThanJudged;
        procedure ARowOfTheModulesOwnAsksTheModuleForItsMenu;
        procedure ARowListedAfterItsTreeAsksAsTheFrameworksDoes;
        procedure TheFrameworksOwnPanelAsksAsItAlwaysDid;
        procedure ARowWhoseCurveIsSelectedNeedsNothing;
        procedure ARowWhoseCurveWasClearedSelectsItAgain;
        procedure NoRowSelectedLeavesTheCurveAlone;

        //  Whether the panel is drawn again.
        procedure TheSameRowsAreTheSameOutline;
        procedure AnyDifferenceARowShowsIsADifferentOutline;

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
        procedure AFrameworkRowIsFoundByItsCurve;
        procedure APackRowIsFoundByTheCurveItCarries;
        procedure TheFirstRowStandingForTheCurveIsFound;
        procedure ACurveNoRowStandsForIsNotFound;
        procedure AnEmptyHandleFindsNoRowEvenOneNamingNoCurve;
        procedure ChoosingTheSelectedCurveInTheTableChangesNothing;
        procedure ATableRowNamingNoCurveChangesNothing;
        procedure ACurveChosenInTheTableSelectsTheRowStandingForIt;
        procedure ACurveNoRowShowsIsChosenWithNoRow;
        procedure NoRowIsEverDetached;
        procedure AModelWithNoCurvesIsAnEmptyOutline;

        //  What it says when there is nothing to show.
        procedure WithNothingOpenItSaysToOpenSomething;
        procedure WithAProfileButNoCurvesItSaysHowToPlaceOne;
        procedure ForAMarkupTypeItDoesNotSayHowToPlaceOne;
        procedure TheEmptyTextIsNeverBlank;

        //  What a row explains.
        procedure ARowCarriesTheTopicOfItsCurvesType;
        procedure ARowWhoseTypeIsUnknownCarriesNoTopic;
        procedure TheTopicOfASelectedRowIsFoundByItsId;
        procedure APackRowsTopicIsFoundTheSameWay;
        procedure NoSelectionHasNoTopic;
        procedure ARowIdNoRowCarriesHasNoTopic;
        procedure TheExplainPaneEmptyTextSaysWhatToPointAt;
        procedure ATitledCurveNamesTheTypeBeforeItsNumber;
        procedure ATitleWithNoNumberIsTheTypeName;
        procedure AnEmptyTitleNamesNoType;
        procedure ATitleThatIsOnlyANumberNamesNoType;
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

{ ---- whose rows the panel shows ---- }

function OneRow(const AId: string; const ACurveId: string = ''): TOutline;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0].Id := AId;
    Result[0].CurveId := ACurveId;
end;

function TwoRows(const AFirst, ASecond: string): TOutline;
begin
    //  A parent and its child, each standing for its own curve - the shape a
    //  module describing a hierarchy pushes.
    Result := nil;
    SetLength(Result, 2);
    Result[0].Id := AFirst;
    Result[0].CurveId := AFirst;
    Result[1].Id := ASecond;
    Result[1].CurveId := ASecond;
    Result[1].Indent := 1;
end;

function FrameworkRows(const AHandles: array of string): TOutline;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(AHandles));
    for i := 0 to High(AHandles) do
    begin
        Result[i].Id := AHandles[i];
        Result[i].CurveId := AHandles[i];
    end;
end;

{ THE PANEL DESCRIBES THE MODEL, NOT THE TOOLS LIST. It used to follow the
  selected type: a module's rows were shown only while one of its own types was
  selected. Selecting an ordinary curve type - the next thing to place - swapped
  a nested pattern count for the framework's flat list of the same curves, and
  the model's hierarchy vanished from the one place meant to show it. }
procedure TModelOutlineTest.AModulesTreeIsShownWhateverTypeIsSelected;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('p', 'c'));
    Sources.FrameworkRows := FrameworkRows(['p', 'c']);
    Shown := ComposePanel(Sources, '');
    AssertEquals('the module''s rows, with a type placed by picks selected',
        'some-markup-set', Shown.PanelId);
    AssertEquals(2, Length(Shown.Rows));
    AssertEquals('the child keeps its place under its parent', 1,
        Shown.Rows[1].Indent);
    AssertEquals('and with another module''s type selected',
        'some-markup-set', ComposePanel(Sources, 'other-set').PanelId);
end;

{ WHAT THE OLD RULE PROTECTED, and still must: a module with nothing placed
  pushes an empty outline on every redraw, and that must not replace a model it
  has no part in, nor the selection on it. }
procedure TModelOutlineTest.AModuleWithNothingPlacedLeavesTheFrameworksRows;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', nil);
    Sources.FrameworkRows := FrameworkRows(['g1', 'g2']);
    Shown := ComposePanel(Sources, 'some-markup-set');
    AssertEquals(FrameworkStructureId, Shown.PanelId);
    AssertEquals(2, Length(Shown.Rows));
    AssertEquals('g1', Shown.Rows[0].Id);
end;

{ A MODEL MAY HOLD CURVES OF SEVERAL KINDS. A module's tree describes the curves
  it placed; the rest are still the model, and a panel that dropped them would
  deny what the chart shows (D26). }
procedure TModelOutlineTest.CurvesNoModuleRowNamesAreListedAfterItsTree;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('p', 'c'));
    Sources.FrameworkRows := FrameworkRows(['g', 'p', 'c']);
    Shown := ComposePanel(Sources, '');
    AssertEquals(3, Length(Shown.Rows));
    AssertEquals('the tree first', 'p', Shown.Rows[0].Id);
    AssertEquals('c', Shown.Rows[1].Id);
    AssertEquals('then the curve it does not describe', 'g', Shown.Rows[2].Id);
    AssertEquals('at the top level', 0, Shown.Rows[2].Indent);
end;

procedure TModelOutlineTest.ACurveAModuleRowNamesIsListedOnce;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    //  By the curve the row STANDS FOR, not the row's own id: a pack may
    //  identify its rows by its own markup and carry the handle beside it.
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', OneRow('wave-1', 'h1'));
    Sources.FrameworkRows := FrameworkRows(['h1']);
    Shown := ComposePanel(Sources, '');
    AssertEquals(1, Length(Shown.Rows));
    AssertEquals('wave-1', Shown.Rows[0].Id);
end;

procedure TModelOutlineTest.TheSelectedTypesModuleIsPreferredWhenTwoDescribeTheModel;
var
    Sources: TPanelSources;
begin
    //  The panel has one owner at a time - its row menu asks that module - so
    //  with two describing something the selection breaks the tie, and with
    //  neither of theirs selected the first to have pushed keeps it.
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', OneRow('a', 'a'));
    HoldPanelRows(Sources.Held, 'other-set', OneRow('b', 'b'));
    Sources.FrameworkRows := FrameworkRows(['a', 'b']);
    AssertEquals('other-set', ComposePanel(Sources, 'Other-Set').PanelId);
    AssertEquals('some-markup-set', ComposePanel(Sources, '').PanelId);
end;

procedure TModelOutlineTest.WithNothingAnywhereTheSelectedModuleSaysWhy;
var
    Shown: TPanelContents;
    Sources: TPanelSources;
begin
    //  An empty panel shows its owner's "nothing yet", and only the module
    //  knows what its markup is - so with its type selected, the empty panel is
    //  its to word.
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', nil);
    Shown := ComposePanel(Sources, 'some-markup-set');
    AssertEquals('some-markup-set', Shown.PanelId);
    AssertEquals(0, Length(Shown.Rows));
end;

procedure TModelOutlineTest.WithNothingAnywhereAPickedTypeIsTheFrameworks;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', nil);
    AssertEquals(FrameworkStructureId, ComposePanel(Sources, '').PanelId);
    AssertEquals('nor has the selected module pushed anything yet',
        FrameworkStructureId, ComposePanel(Sources, 'other-set').PanelId);
end;

procedure TModelOutlineTest.OnlyTheLatestPushOfAPanelIsHeld;
var
    Held: THeldPanelRows;
begin
    Held := Default(THeldPanelRows);
    HoldPanelRows(Held, 'some-markup-set', OneRow('old'));
    HoldPanelRows(Held, 'some-markup-set', OneRow('new'));
    AssertEquals('one entry per panel', 1, Length(Held.Ids));
    AssertEquals('new', Held.Rows[0][0].Id);
end;

{ KEPT, because the panel is composed from every source's LATEST rows: the
  framework refreshes far more often than a module redraws, and a module's tree
  must survive each of those refreshes. }
procedure TModelOutlineTest.APushIsComposedAndKept;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    AssertTrue(KeepPanelRows(Sources, 'some-markup-set', ['some-markup-set'],
        OneRow('a', 'a')));
    AssertEquals(1, Length(Sources.Held.Ids));
    AssertEquals('a', Sources.Held.Rows[0][0].Id);
end;

procedure TModelOutlineTest.TheFrameworksRowsAreKeptAsTheirOwn;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    AssertTrue(KeepPanelRows(Sources, FrameworkStructureId, ['some-markup-set'],
        OneRow('f', 'f')));
    AssertEquals('none of them a module''s', 0, Length(Sources.Held.Ids));
    AssertEquals('f', Sources.FrameworkRows[0].Id);
end;

procedure TModelOutlineTest.RowsNoModuleClaimsAreShownAsTheyCome;
var
    Sources: TPanelSources;
begin
    //  No module owns them, so nothing describes the model with them: the
    //  window's own self-check pushes rows like these, and kept they would sit
    //  in the panel over the real model for good.
    Sources := Default(TPanelSources);
    AssertFalse(KeepPanelRows(Sources, 'check.rows', ['some-markup-set'],
        OneRow('x', 'x')));
    AssertEquals(0, Length(Sources.Held.Ids));
    AssertEquals(0, Length(Sources.FrameworkRows));
end;

{ "show" in a report names a row the panel is not showing. Kept rows that still
  describe the model are only outranked - another module's rows are on show -
  and saying they are gone sends the user looking for a deletion that never
  happened. }
procedure TModelOutlineTest.AShowLinkToAKeptRowSaysToSelectItsType;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    Sources.FrameworkRows := FrameworkRows(['a']);
    AssertEquals('nothing kept', RowGoneHint, RowLinkMissHint(Sources, 'a'));
    HoldPanelRows(Sources.Held, 'some-markup-set', OneRow('a', 'a'));
    AssertEquals('kept', RowKeptHint, RowLinkMissHint(Sources, 'a'));
    AssertEquals('not among the kept', RowGoneHint, RowLinkMissHint(Sources, 'zzz'));
    AssertTrue('two different sentences', RowKeptHint <> RowGoneHint);
end;

procedure TModelOutlineTest.AShowLinkToRowsOfAnotherModelSaysTheyAreGone;
var
    Sources: TPanelSources;
begin
    //  Kept, but about a model that is no longer open: selecting a type would
    //  not bring them back, so telling the user to is a false lead.
    Sources := Default(TPanelSources);
    Sources.FrameworkRows := FrameworkRows(['g']);
    HoldPanelRows(Sources.Held, 'some-markup-set', OneRow('a', 'a'));
    AssertEquals(RowGoneHint, RowLinkMissHint(Sources, 'a'));
end;

{ A PUSH DESCRIBES THE MODEL ONLY WHILE IT NAMES A CURVE THE MODEL HOLDS.
  Nothing clears a module's kept rows when a project is closed or another file
  opened, and nothing should have to: a module whose redraw exits early would
  leave its old tree over a model it has no part in. Checked against the
  model's own curves instead, the rule needs no hook every module must
  remember to call. }
procedure TModelOutlineTest.RowsNamingNoCurveOfTheModelAreNotShown;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('old-p', 'old-c'));
    Sources.FrameworkRows := FrameworkRows(['g']);
    Shown := ComposePanel(Sources, '');
    AssertEquals('the model''s own rows', FrameworkStructureId, Shown.PanelId);
    AssertEquals(1, Length(Shown.Rows));
    AssertEquals('g', Shown.Rows[0].Id);
end;

procedure TModelOutlineTest.RowsNamingNoCurveAreNotShownEvenForTheSelectedType;
var
    Sources: TPanelSources;
begin
    //  Nor does selecting the module's type bring them back: they describe a
    //  model that is no longer open.
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('old-p', 'old-c'));
    Sources.FrameworkRows := FrameworkRows(['g']);
    AssertEquals(FrameworkStructureId,
        ComposePanel(Sources, 'some-markup-set').PanelId);
end;

procedure TModelOutlineTest.OneRowNamingACurveOfTheModelIsEnough;
var
    Sources: TPanelSources;
begin
    //  A pattern just placed may be named before the rest are rebuilt; one
    //  curve the model holds is enough to say the rows are about this model.
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('p', 'not-yet'));
    Sources.FrameworkRows := FrameworkRows(['p']);
    AssertEquals('some-markup-set', ComposePanel(Sources, '').PanelId);
end;

{ A PUSH CARRYING NO HANDLE AT ALL CANNOT BE JUDGED against the model, and
  hiding it would hide a module's whole description - a count whose wave ids
  are not GUIDs issues no handles. So it is shown, as every push was before
  rows were checked against the model; only a push that names curves, none of
  them the model's, is known to be about another model. }
procedure TModelOutlineTest.RowsCarryingNoHandleAreShownRatherThanJudged;
var
    Sources: TPanelSources;
    Shown: TPanelContents;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', OneRow('wave-1'));
    Sources.FrameworkRows := FrameworkRows(['g']);
    Shown := ComposePanel(Sources, '');
    AssertEquals('some-markup-set', Shown.PanelId);
    AssertEquals('and the model''s own curve after it', 2, Length(Shown.Rows));
end;

{ WHOSE MENU A ROW ASKS FOR. A module's panel asks only that module - it knows
  its own rows - but the curves listed after its tree are the framework's rows,
  and a module asked about one offers nothing. Those ask every module in turn,
  exactly as the framework's own panel does. }
procedure TModelOutlineTest.ARowOfTheModulesOwnAsksTheModuleForItsMenu;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('p', 'c'));
    Sources.FrameworkRows := FrameworkRows(['p', 'c', 'g']);
    AssertEquals('some-markup-set',
        RowMenuPanelId(ComposePanel(Sources, ''), 'c'));
end;

procedure TModelOutlineTest.ARowListedAfterItsTreeAsksAsTheFrameworksDoes;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    HoldPanelRows(Sources.Held, 'some-markup-set', TwoRows('p', 'c'));
    Sources.FrameworkRows := FrameworkRows(['p', 'c', 'g']);
    AssertEquals(FrameworkStructureId,
        RowMenuPanelId(ComposePanel(Sources, ''), 'g'));
end;

procedure TModelOutlineTest.TheFrameworksOwnPanelAsksAsItAlwaysDid;
var
    Sources: TPanelSources;
begin
    Sources := Default(TPanelSources);
    Sources.FrameworkRows := FrameworkRows(['g']);
    AssertEquals(FrameworkStructureId,
        RowMenuPanelId(ComposePanel(Sources, ''), 'g'));
end;

{ THE SELECTED CURVE IS THE ONE THE SELECTED ROW STANDS FOR. A refused Delete
  curve clears the curve and leaves the row; a panel drawn again used to select
  the row, and so its curve, again. Drawn only when it changes, the panel has
  to be asked for that curve instead. }
procedure TModelOutlineTest.ARowWhoseCurveIsSelectedNeedsNothing;
var
    Handle: string;
begin
    AssertFalse(CurveToReselect(TwoRows('p', 'c'), 'c', 'c', Handle));
end;

procedure TModelOutlineTest.ARowWhoseCurveWasClearedSelectsItAgain;
var
    Handle: string;
begin
    AssertTrue(CurveToReselect(TwoRows('p', 'c'), 'c', '', Handle));
    AssertEquals('c', Handle);
end;

procedure TModelOutlineTest.NoRowSelectedLeavesTheCurveAlone;
var
    Handle: string;
begin
    //  A curve chosen in the Curve Attributes table that no row shows is held
    //  with no row selected, and a refresh must not drop it.
    AssertFalse(CurveToReselect(TwoRows('p', 'c'), '', 'x', Handle));
end;

{ ---- whether the panel is drawn again ---- }

{ NOT REBUILT WHEN NOTHING CHANGED. The panel is refreshed on every command
  refresh, and a rebuild clears the tree, opens every subtree the user closed
  and hands the selection back to its owner - a call into a module, on every
  refresh. The old rule skipped it for a module's rows already on show; the
  composed panel must as well, whoever the rows are from. }
procedure TModelOutlineTest.TheSameRowsAreTheSameOutline;
begin
    AssertTrue(SameOutline(TwoRows('p', 'c'), TwoRows('p', 'c')));
    AssertTrue('nothing is nothing', SameOutline(nil, nil));
end;

procedure TModelOutlineTest.AnyDifferenceARowShowsIsADifferentOutline;
var
    A, B: TOutline;
begin
    A := TwoRows('p', 'c');
    AssertFalse('a row more', SameOutline(A, FrameworkRows(['p', 'c', 'g'])));
    B := Copy(A); B[1].Caption := 'renamed';
    AssertFalse('a caption', SameOutline(A, B));
    B := Copy(A); B[1].Indent := 0;
    AssertFalse('where it hangs', SameOutline(A, B));
    B := Copy(A); B[1].Id := 'other';
    AssertFalse('its identity', SameOutline(A, B));
    B := Copy(A); B[1].CurveId := 'other';
    AssertFalse('the curve it stands for', SameOutline(A, B));
    B := Copy(A); B[1].Topic := 'other';
    AssertFalse('what it explains', SameOutline(A, B));
    B := Copy(A); B[1].IsDetached := True;
    AssertFalse('whether it lost its parent', SameOutline(A, B));
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

procedure TModelOutlineTest.AFrameworkRowIsFoundByItsCurve;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'A1', 1);
    AddCurve('Lorentzian', 'B2', 2);
    O := Outline;
    AssertEquals('the second row', 1, RowIndexForCurveHandle(O, 'B2'));
end;

procedure TModelOutlineTest.APackRowIsFoundByTheCurveItCarries;
var
    O: TOutline;
begin
    //  A pack's row is identified by its own markup; the handle is beside it.
    SetLength(O, 2);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    O[1] := RowNaming('WAVE-2', '{CURVE-2}');
    AssertEquals('found by CurveId, not by Id', 1, RowIndexForCurveHandle(O, '{CURVE-2}'));
    AssertEquals('and not by the row''s own id', -1, RowIndexForCurveHandle(O, 'WAVE-2'));
end;

procedure TModelOutlineTest.TheFirstRowStandingForTheCurveIsFound;
var
    O: TOutline;
begin
    //  A pack may show one curve under more than one row; the first is chosen,
    //  so the answer is stable rather than whichever the search met last.
    SetLength(O, 3);
    O[0] := RowNaming('NOTE', '');
    O[1] := RowNaming('WAVE-1', '{CURVE-1}');
    O[2] := RowNaming('WAVE-1a', '{CURVE-1}');
    AssertEquals(1, RowIndexForCurveHandle(O, '{CURVE-1}'));
end;

procedure TModelOutlineTest.ACurveNoRowStandsForIsNotFound;
var
    O: TOutline;
begin
    AddCurve('Gaussian', 'A1', 1);
    O := Outline;
    AssertEquals(-1, RowIndexForCurveHandle(O, 'Z9'));
end;

procedure TModelOutlineTest.AnEmptyHandleFindsNoRowEvenOneNamingNoCurve;
var
    O: TOutline;
begin
    //  A heading carries no curve, and '' must not select it.
    SetLength(O, 1);
    O[0] := RowNaming('NOTE-1', '');
    AssertEquals(-1, RowIndexForCurveHandle(O, ''));
end;

procedure TModelOutlineTest.ChoosingTheSelectedCurveInTheTableChangesNothing;
var
    O: TOutline;
begin
    SetLength(O, 1);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    AssertFalse(CurveChoiceFromTable(O, '{CURVE-1}', '{CURVE-1}').Changes);
end;

procedure TModelOutlineTest.ATableRowNamingNoCurveChangesNothing;
var
    O: TOutline;
begin
    //  Even with nothing selected: '' equal to '' must not choose "no curve".
    SetLength(O, 1);
    O[0] := RowNaming('NOTE-1', '');
    AssertFalse(CurveChoiceFromTable(O, '', '').Changes);
end;

procedure TModelOutlineTest.ACurveChosenInTheTableSelectsTheRowStandingForIt;
var
    O: TOutline;
    Choice: TTableCurveChoice;
begin
    //  A pack's row: the id is its own markup, so the row is found by the
    //  curve it carries and selected by its id.
    SetLength(O, 2);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    O[1] := RowNaming('WAVE-2', '{CURVE-2}');
    Choice := CurveChoiceFromTable(O, '{CURVE-2}', '{CURVE-1}');
    AssertTrue('it moves', Choice.Changes);
    AssertEquals('to the curve chosen', '{CURVE-2}', Choice.CurveId);
    AssertEquals('by the row standing for it', 'WAVE-2', Choice.RowId);
end;

procedure TModelOutlineTest.ACurveNoRowShowsIsChosenWithNoRow;
var
    O: TOutline;
    Choice: TTableCurveChoice;
begin
    SetLength(O, 1);
    O[0] := RowNaming('WAVE-1', '{CURVE-1}');
    Choice := CurveChoiceFromTable(O, '{CURVE-9}', '{CURVE-1}');
    AssertTrue('it still moves', Choice.Changes);
    AssertEquals('to the curve chosen', '{CURVE-9}', Choice.CurveId);
    AssertEquals('with no row to show it', '', Choice.RowId);
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


function TwoRows: TModelCurveRows;
begin
    SetLength(Result, 2);
    Result[0] := Default(TModelCurveRow);
    Result[0].Title := 'Gaussian';
    Result[0].InstanceId := 'h1';
    Result[0].Topic := 'curve-type/gauss';
    Result[1] := Default(TModelCurveRow);
    Result[1].Title := 'Lorentzian';
    Result[1].InstanceId := 'h2';
end;

procedure TModelOutlineTest.ARowCarriesTheTopicOfItsCurvesType;
begin
    AssertEquals('curve-type/gauss', ModelOutlineOf(TwoRows)[0].Topic);
end;

procedure TModelOutlineTest.ARowWhoseTypeIsUnknownCarriesNoTopic;
begin
    AssertEquals('', ModelOutlineOf(TwoRows)[1].Topic);
end;

procedure TModelOutlineTest.TheTopicOfASelectedRowIsFoundByItsId;
begin
    AssertEquals('curve-type/gauss', TopicForRowId(ModelOutlineOf(TwoRows), 'h1'));
end;

procedure TModelOutlineTest.APackRowsTopicIsFoundTheSameWay;
var
    Rows: TOutline;
begin
    SetLength(Rows, 1);
    Rows[0] := Default(TOutlineRow);
    Rows[0].Id := '{wave-guid}';
    Rows[0].CurveId := 'h9';
    Rows[0].Topic := 'pack/pattern/{wave-guid}';
    AssertEquals('pack/pattern/{wave-guid}', TopicForRowId(Rows, '{wave-guid}'));
end;

procedure TModelOutlineTest.NoSelectionHasNoTopic;
var
    Rows: TOutline;
begin
    //  The sentence of an empty panel is a row with no id; it explains nothing.
    SetLength(Rows, 1);
    Rows[0] := Default(TOutlineRow);
    Rows[0].Topic := 'curve-type/gauss';
    AssertEquals('', TopicForRowId(Rows, ''));
end;

procedure TModelOutlineTest.ARowIdNoRowCarriesHasNoTopic;
begin
    AssertEquals('', TopicForRowId(ModelOutlineOf(TwoRows), 'h7'));
end;

procedure TModelOutlineTest.TheExplainPaneEmptyTextSaysWhatToPointAt;
begin
    AssertTrue(Pos('curve type', ExplainPaneEmptyText) > 0);
    AssertTrue(Pos('Model', ExplainPaneEmptyText) > 0);
    AssertTrue(Pos('menu entry', ExplainPaneEmptyText) > 0);
end;


procedure TModelOutlineTest.ATitledCurveNamesTheTypeBeforeItsNumber;
begin
    AssertEquals('Asym. Pseudo-Voigt', CurveTypeNameOfTitle('Asym. Pseudo-Voigt [3]'));
end;

procedure TModelOutlineTest.ATitleWithNoNumberIsTheTypeName;
begin
    AssertEquals('Gaussian', CurveTypeNameOfTitle('Gaussian'));
end;

procedure TModelOutlineTest.AnEmptyTitleNamesNoType;
begin
    AssertEquals('', CurveTypeNameOfTitle(''));
end;

procedure TModelOutlineTest.ATitleThatIsOnlyANumberNamesNoType;
begin
    AssertEquals('', CurveTypeNameOfTitle(' [3]'));
end;

initialization
    //  A unit test: records in, rows out. No window, no tree control and no
    //  module.
    RegisterTest('unit', TModelOutlineTest);
end.
