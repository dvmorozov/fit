// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which series on the chart were drawn for which curve.)

WHY THESE TESTS EXIST. Deleting a curve has to take every series drawn for it,
and a curve is not one line: an ordinary peak is a single series, a curve placed
from its own markup is that plus the markers its contributor drew, and nothing
on the chart says which belong together. The relation is recorded, and these are
the questions asked of the record.

Two of them would cost the most. A series that belongs to the model as a
whole - the profile, the difference, the background - carries no owner, and
answering it to a curve's query would take the data off the chart with the first
curve deleted. And the owned indices come back HIGHEST FIRST, because a caller
removing them one at a time by index is otherwise shifted by its own earlier
removals: it removes the wrong series, or reads past the end.
}
unit testcase_series_register;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, series_register;

type
    TSeriesRegisterTest = class(TTestCase)
    private
        FReg: TSeriesRegister;
        { Stand-ins for a chart series and a point set. Plain objects, because
          the register names neither type. }
        FOwned: TStringList;
        function NewObject: TObject;
        procedure AddSeries(const AOwner: string; AHasRow: boolean = True);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What is in it.
        procedure ANewRegisterIsEmpty;
        procedure AddingRecordsTheSeriesAndItsPoints;
        procedure EntriesKeepTheOrderTheyWereAddedIn;
        procedure AnIndexOutsideTheRegisterAnswersEmptily;

        //  Finding one.
        procedure ASeriesIsFoundByItsPointSet;
        procedure AndByItself;
        procedure APointSetNothingDrawsIsNotFound;
        procedure NilFindsNothing;

        //  Ownership.
        procedure ACurveWithOneSeriesOwnsIt;
        procedure ACurveWithSeveralOwnsAllOfThem;
        procedure AndOnlyItsOwn;
        procedure OwnedIndicesComeBackHighestFirst;
        procedure ASeriesBelongingToTheModelHasNoOwner;
        procedure AnEmptyHandleOwnsNothing;
        procedure AskingAboutACurveWithNoSeriesIsNotAnError;

        //  Removal.
        procedure RemovingAnEntryShortensTheRegister;
        procedure RemovingFromTheMiddleKeepsEveryOtherPairing;
        procedure RemovingEveryEntryACurveOwnsLeavesTheRest;
        procedure RemovingAnIndexOutsideTheRegisterChangesNothing;
        procedure ClearingEmptiesIt;

        //  The legend row, which is not the chart position.
        procedure AnEntryRemembersWhetherItGotALegendRow;
        procedure AndOneAddedDuringAFitDidNot;
    end;

implementation

procedure TSeriesRegisterTest.SetUp;
begin
    FReg := TSeriesRegister.Create;
    FOwned := TStringList.Create;
    FOwned.OwnsObjects := True;
end;

procedure TSeriesRegisterTest.TearDown;
begin
    FReg.Free;
    FReg := nil;
    //  The stand-ins outlive the register, which holds them without owning
    //  them - exactly as the real one holds the chart's series.
    FOwned.Free;
    FOwned := nil;
end;

function TSeriesRegisterTest.NewObject: TObject;
begin
    Result := TObject.Create;
    FOwned.AddObject('', Result);
end;

procedure TSeriesRegisterTest.AddSeries(const AOwner: string;
    AHasRow: boolean);
begin
    FReg.Add(NewObject, NewObject, AOwner, AHasRow);
end;

{ ---- what is in it ---- }

procedure TSeriesRegisterTest.ANewRegisterIsEmpty;
begin
    AssertEquals('nothing plotted', 0, FReg.Count);
end;

procedure TSeriesRegisterTest.AddingRecordsTheSeriesAndItsPoints;
var
    S, P: TObject;
begin
    S := NewObject;
    P := NewObject;
    FReg.Add(S, P, 'A1', True);
    AssertEquals('one entry', 1, FReg.Count);
    AssertSame('the series', S, FReg.Item(0).Serie);
    AssertSame('and its points', P, FReg.Item(0).Points);
    AssertEquals('and its owner', 'A1', FReg.Item(0).OwnerCurveId);
end;

procedure TSeriesRegisterTest.EntriesKeepTheOrderTheyWereAddedIn;
begin
    AddSeries('A1');
    AddSeries('A2');
    AddSeries('A3');
    //  The chart is append-only and so is this, which is what lets a caller
    //  walk both without drifting.
    AssertEquals('A1', FReg.Item(0).OwnerCurveId);
    AssertEquals('A2', FReg.Item(1).OwnerCurveId);
    AssertEquals('A3', FReg.Item(2).OwnerCurveId);
end;

procedure TSeriesRegisterTest.AnIndexOutsideTheRegisterAnswersEmptily;
begin
    AddSeries('A1');
    //  A default record rather than a fault: the register is read from a poll,
    //  and a fault there is a dialog the user cannot dismiss.
    AssertEquals('', FReg.Item(7).OwnerCurveId);
    AssertEquals('', FReg.Item(-1).OwnerCurveId);
end;

{ ---- finding one ---- }

procedure TSeriesRegisterTest.ASeriesIsFoundByItsPointSet;
var
    S, P: TObject;
begin
    AddSeries('A1');
    S := NewObject;
    P := NewObject;
    FReg.Add(S, P, 'A2', True);
    //  The point set is the key every existing caller already had in hand.
    AssertEquals('the second entry', 1, FReg.IndexOfPoints(P));
end;

procedure TSeriesRegisterTest.AndByItself;
var
    S, P: TObject;
begin
    AddSeries('A1');
    S := NewObject;
    P := NewObject;
    FReg.Add(S, P, 'A2', True);
    AssertEquals('the second entry', 1, FReg.IndexOfSerie(S));
end;

procedure TSeriesRegisterTest.APointSetNothingDrawsIsNotFound;
begin
    AddSeries('A1');
    AssertEquals('not here', -1, FReg.IndexOfPoints(NewObject));
end;

procedure TSeriesRegisterTest.NilFindsNothing;
begin
    AddSeries('A1');
    //  An entry may legitimately hold no point set, so nil must not match the
    //  first one that does not either.
    AssertEquals('nil is not a key', -1, FReg.IndexOfPoints(nil));
    AssertEquals('nor is it a series', -1, FReg.IndexOfSerie(nil));
end;

{ ---- ownership ---- }

procedure TSeriesRegisterTest.ACurveWithOneSeriesOwnsIt;
begin
    AddSeries('A1');
    AssertEquals('one series', 1, Length(FReg.OwnedBy('A1')));
    AssertTrue('and it says so', FReg.AnyOwnedBy('A1'));
end;

procedure TSeriesRegisterTest.ACurveWithSeveralOwnsAllOfThem;
begin
    //  THE CASE THE RECORD EXISTS FOR: a curve placed from its own markup is a
    //  curve series plus the markers its contributor drew for it.
    AddSeries('A1');
    AddSeries('A1');
    AddSeries('A1');
    AssertEquals('all three', 3, Length(FReg.OwnedBy('A1')));
end;

procedure TSeriesRegisterTest.AndOnlyItsOwn;
begin
    AddSeries('A1');
    AddSeries('A2');
    AddSeries('A1');
    AssertEquals('two for the first', 2, Length(FReg.OwnedBy('A1')));
    AssertEquals('one for the second', 1, Length(FReg.OwnedBy('A2')));
end;

procedure TSeriesRegisterTest.OwnedIndicesComeBackHighestFirst;
var
    Owned: TSeriesIndices;
begin
    AddSeries('A1');
    AddSeries('A1');
    AddSeries('A1');
    Owned := FReg.OwnedBy('A1');
    //  THE ORDER IS THE POINT. A caller removing these one at a time by index
    //  would otherwise have its later indices shifted by its own earlier
    //  removals - taking the wrong series off, or reading past the end.
    AssertEquals('three', 3, Length(Owned));
    AssertEquals('highest first', 2, Owned[0]);
    AssertEquals('then', 1, Owned[1]);
    AssertEquals('then', 0, Owned[2]);
end;

procedure TSeriesRegisterTest.ASeriesBelongingToTheModelHasNoOwner;
begin
    //  The profile, the computed profile, the difference, the background, the
    //  bounds, both position series and the picked points. None is anybody's
    //  curve.
    AddSeries('');
    AddSeries('A1');
    AssertEquals('only the curve''s own', 1, Length(FReg.OwnedBy('A1')));
end;

procedure TSeriesRegisterTest.AnEmptyHandleOwnsNothing;
begin
    //  THE ONE THAT WOULD COST THE MOST. Every model-wide series carries an
    //  empty handle, so answering them here would take the data off the chart
    //  with the first curve deleted.
    AddSeries('');
    AddSeries('');
    AssertEquals('nothing', 0, Length(FReg.OwnedBy('')));
    AssertFalse('and nothing to remove', FReg.AnyOwnedBy(''));
end;

procedure TSeriesRegisterTest.AskingAboutACurveWithNoSeriesIsNotAnError;
begin
    AddSeries('A1');
    //  A curve whose series were already taken off, or one the chart never drew.
    AssertEquals('none', 0, Length(FReg.OwnedBy('A9')));
    AssertFalse('and it says so', FReg.AnyOwnedBy('A9'));
end;

{ ---- removal ---- }

procedure TSeriesRegisterTest.RemovingAnEntryShortensTheRegister;
begin
    AddSeries('A1');
    AddSeries('A2');
    FReg.Remove(0);
    AssertEquals('one left', 1, FReg.Count);
    AssertEquals('and it is the other', 'A2', FReg.Item(0).OwnerCurveId);
end;

procedure TSeriesRegisterTest.RemovingFromTheMiddleKeepsEveryOtherPairing;
var
    S1, P1, S3, P3: TObject;
begin
    S1 := NewObject; P1 := NewObject;
    FReg.Add(S1, P1, 'A1', True);
    AddSeries('A2');
    S3 := NewObject; P3 := NewObject;
    FReg.Add(S3, P3, 'A3', True);

    FReg.Remove(1);

    //  EVERY OTHER PAIRING INTACT, which is what a removal from the middle used
    //  to break when the pairing was a position rather than a record.
    AssertEquals('two left', 2, FReg.Count);
    AssertSame('the first still has its points', P1, FReg.Item(0).Points);
    AssertSame('and the third too', P3, FReg.Item(1).Points);
    AssertEquals('found again by series', 1, FReg.IndexOfSerie(S3));
end;

procedure TSeriesRegisterTest.RemovingEveryEntryACurveOwnsLeavesTheRest;
var
    Owned: TSeriesIndices;
    i: longint;
begin
    AddSeries('');        //  the profile
    AddSeries('A1');      //  the curve
    AddSeries('A1');      //  and its markers
    AddSeries('A2');      //  another curve

    Owned := FReg.OwnedBy('A1');
    for i := 0 to High(Owned) do
        FReg.Remove(Owned[i]);

    AssertEquals('two left', 2, FReg.Count);
    AssertEquals('the model''s own survives', '', FReg.Item(0).OwnerCurveId);
    AssertEquals('and the other curve', 'A2', FReg.Item(1).OwnerCurveId);
    AssertFalse('nothing of the first remains', FReg.AnyOwnedBy('A1'));
end;

procedure TSeriesRegisterTest.RemovingAnIndexOutsideTheRegisterChangesNothing;
begin
    AddSeries('A1');
    FReg.Remove(9);
    FReg.Remove(-1);
    AssertEquals('untouched', 1, FReg.Count);
end;

procedure TSeriesRegisterTest.ClearingEmptiesIt;
begin
    AddSeries('A1');
    AddSeries('A2');
    FReg.Clear;
    AssertEquals('empty', 0, FReg.Count);
end;

{ ---- the legend row ---- }

procedure TSeriesRegisterTest.AnEntryRemembersWhetherItGotALegendRow;
begin
    AddSeries('A1', True);
    AssertTrue('it has a row', FReg.Item(0).HasLegendRow);
end;

procedure TSeriesRegisterTest.AndOneAddedDuringAFitDidNot;
begin
    //  Rows are added only while the legend is being updated, and the redraws
    //  during a running fit switch that off - which is why the legend is not
    //  index-parallel to the chart and no row may be found by position.
    AddSeries('A1', False);
    AssertFalse('no row', FReg.Item(0).HasLegendRow);
end;

initialization
    //  A unit test: two plain objects and a handle. The register names no
    //  charting component, which is what lets it be asked these questions at
    //  all - the chart it mirrors cannot be built headlessly.
    RegisterTest('unit', TSeriesRegisterTest);
end.
