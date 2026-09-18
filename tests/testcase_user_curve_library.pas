// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The user's own curve types, as a library that can be asked questions.)

THE SAME SEARCH WAS WRITTEN OUT THREE TIMES in three menu handlers, each walking
the stored curves comparing the clicked item's Tag against the object's address,
and none of the three reachable by anything but a click. The claims worth pinning
are the two that fail silently:

  * The match is by IDENTITY. A menu built against a list that has since changed
    must match NOTHING rather than the curve that now occupies that position -
    otherwise the user selects a curve they did not click on, and there is no
    error to see.

  * "The last curve" is the last one that is not the placeholder, which is not
    the same as the last one. It is asked immediately after the definition
    dialogs report success, so answering with the placeholder selects a curve
    whose formula is "1.0+1.0".
}
unit testcase_user_curve_library;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Contnrs, fpcunit, testregistry,
    app_settings, user_curve_library;

type
    TUserCurveLibraryTest = class(TTestCase)
    private
        FList: TComponentList;
        { Appends a stored curve with the given name, and answers it. }
        function Add(const AName: string): Curve_type;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ACurveIsFoundByItsOwnAddress;
        procedure AStaleTagMatchesNothing;
        procedure ATagOfZeroMatchesNothing;
        procedure NoListMatchesNothing;
        procedure AnEmptyListMatchesNothing;
        procedure TheLastCurveIsTheLastOneCreated;
        procedure ThePlaceholderIsNeverTheLastCurve;
        procedure NotEvenWhenItIsTheOnlyEntry;
        procedure AnEmptyLibraryHasNoLastCurve;
        procedure DeletingTheSelectedCurveClearsTheSelection;
        procedure DeletingAnotherCurveLeavesItAlone;
        procedure OnlyDeletingTheFittedCurveDisturbsTheModel;
        procedure TheDeletionNoticeSaysWhatToDoAndWhatIsOnTheChart;
        procedure TheUnusableNoticeNamesTheWayToDeleteIt;
    end;

implementation

procedure TUserCurveLibraryTest.SetUp;
begin
    //  Owns its entries, as the settings object's list does.
    FList := TComponentList.Create(True);
end;

procedure TUserCurveLibraryTest.TearDown;
begin
    FreeAndNil(FList);
end;

function TUserCurveLibraryTest.Add(const AName: string): Curve_type;
begin
    Result := Curve_type.Create(nil);
    Result.Name := AName;
    FList.Add(Result);
end;

procedure TUserCurveLibraryTest.ACurveIsFoundByItsOwnAddress;
var
    First, Second: Curve_type;
begin
    First := Add('Peak');
    Second := Add('Step');
    AssertTrue('the first', CurveWithTag(FList, PtrInt(First)) = First);
    AssertTrue('the second', CurveWithTag(FList, PtrInt(Second)) = Second);
end;

procedure TUserCurveLibraryTest.AStaleTagMatchesNothing;
var
    Gone: Curve_type;
begin
    //  THE CASE THE IDENTITY COMPARISON EXISTS FOR. The menu was built when this
    //  curve was in the list; by the time the click arrives it is not. Answering
    //  with whatever now sits at its position would select a curve the user did
    //  not click on, with nothing to see.
    Gone := Add('Removed');
    FList.Extract(Gone);
    Add('Something else');
    AssertTrue('no match', CurveWithTag(FList, PtrInt(Gone)) = nil);
    Gone.Free;
end;

procedure TUserCurveLibraryTest.ATagOfZeroMatchesNothing;
begin
    //  What an item that was never given a tag carries.
    Add('Peak');
    AssertTrue('no match', CurveWithTag(FList, 0) = nil);
end;

procedure TUserCurveLibraryTest.NoListMatchesNothing;
begin
    AssertTrue('no list, no curve', CurveWithTag(nil, 12345) = nil);
end;

procedure TUserCurveLibraryTest.AnEmptyListMatchesNothing;
begin
    AssertTrue('nothing stored', CurveWithTag(FList, 12345) = nil);
end;

procedure TUserCurveLibraryTest.TheLastCurveIsTheLastOneCreated;
var
    Last: Curve_type;
begin
    Add('First');
    Add('Second');
    Last := Add('Third');
    AssertTrue('the third', LastCreatedCurve(FList) = Last);
end;

procedure TUserCurveLibraryTest.ThePlaceholderIsNeverTheLastCurve;
var
    Real_: Curve_type;
begin
    //  An old settings file can hold the placeholder AFTER the real entries, and
    //  this is asked right after a curve was created in order to find it.
    Real_ := Add('Peak');
    Add(DUMMY_CURVE_NAME);
    AssertTrue('the real one', LastCreatedCurve(FList) = Real_);
end;

procedure TUserCurveLibraryTest.NotEvenWhenItIsTheOnlyEntry;
begin
    Add(DUMMY_CURVE_NAME);
    //  Nil, so the caller reports that the curve the dialog said it created
    //  never reached the settings - rather than selecting "1.0+1.0".
    AssertTrue('no curve', LastCreatedCurve(FList) = nil);
end;

procedure TUserCurveLibraryTest.AnEmptyLibraryHasNoLastCurve;
begin
    AssertTrue('none', LastCreatedCurve(FList) = nil);
    AssertTrue('and no list at all is the same answer',
        LastCreatedCurve(nil) = nil);
end;

procedure TUserCurveLibraryTest.DeletingTheSelectedCurveClearsTheSelection;
var
    Selected: Curve_type;
begin
    //  A selection pointing at a freed object is worse than no selection: it is
    //  read whenever the menu is rebuilt.
    Selected := Add('Peak');
    AssertTrue('cleared',
        SelectionAfterDeleting(Selected, Selected) = nil);
end;

procedure TUserCurveLibraryTest.DeletingAnotherCurveLeavesItAlone;
var
    Selected, Other: Curve_type;
begin
    Selected := Add('Peak');
    Other := Add('Step');
    AssertTrue('untouched',
        SelectionAfterDeleting(Selected, Other) = Selected);
    //  And with nothing selected there is nothing to clear.
    AssertTrue('still nothing', SelectionAfterDeleting(nil, Other) = nil);
end;

procedure TUserCurveLibraryTest.OnlyDeletingTheFittedCurveDisturbsTheModel;
var
    Fitted, Other: Curve_type;
begin
    //  This is what decides whether the compute server has to be told. It knows
    //  nothing about the deletion, so if it is not told, the next fit produces
    //  more curves of the type just deleted.
    Fitted := Add('Peak');
    Other := Add('Step');
    AssertTrue('the fitted one',
        DeletingLeavesTheModelWithoutACurveType(Fitted, Fitted));
    AssertTrue('any other one',
        not DeletingLeavesTheModelWithoutACurveType(Fitted, Other));
    AssertTrue('with nothing being fitted, nothing is disturbed',
        not DeletingLeavesTheModelWithoutACurveType(nil, Other));
end;

procedure TUserCurveLibraryTest.TheDeletionNoticeSaysWhatToDoAndWhatIsOnTheChart;
var
    Msg: string;
begin
    Msg := DeletedFittedCurveNotice('My peak');
    AssertTrue('it names the curve', Pos('My peak', Msg) > 0);
    AssertTrue('it says what to do next', Pos('Curve Type', Msg) > 0);
    //  THE PART THAT STOPS A SUPPORT QUESTION: the curves already drawn were
    //  built from the deleted formula and stay until the next fit, which looks
    //  exactly like the deletion not having worked.
    AssertTrue('it explains the curves still on the chart',
        Pos('until the next fit', Msg) > 0);
end;

procedure TUserCurveLibraryTest.TheUnusableNoticeNamesTheWayToDeleteIt;
var
    Msg: string;
begin
    Msg := UnusableCurveNotice('User Curves', 'Delete User Curve...');
    AssertTrue('it says why it cannot be used', Pos('no formula', Msg) > 0);
    //  The only thing to do about it is delete it, so the path is spelled out.
    AssertTrue('the group', Pos('User Curves', Msg) > 0);
    AssertTrue('the entry', Pos('Delete User Curve...', Msg) > 0);
end;

initialization
    RegisterTest('unit', TUserCurveLibraryTest);
end.
