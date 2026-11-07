// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The one rule every list of this module is filtered by: nothing
reaches the wizard that this build cannot use.)

The rule asks the loader registry, so these tests register a reader of their
own for an extension nothing else claims and watch the rule follow it - which is
the property that matters: a module adding a reader widens every list here with
no edit to this module.
}
unit testcase_repository_filter;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, repository_filter;

type
    TRepositoryFilterTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        procedure AFileWithAReaderIsUsable;
        procedure AFileNothingReadsIsNot;
        procedure ANameWithNoExtensionIsNot;
        procedure AnArchiveIsUsableForWhatItMayHold;
        procedure TheRuleFollowsTheReadersRegistered;
        procedure UnusableFilesAreLeftOutAndCounted;
        procedure ContainersAreKeptWhateverTheyAreCalled;
        procedure ARecordIsUsableWhenAnyOfItsFilesIs;
        procedure TheRefusalSaysHowMuchWasFoundAndWhyNoneIsShown;
    end;

implementation

uses
    data_loader, data_loader_registry, data_loader_registration;

type
    { A reader for an extension no format uses, registered by this test. }
    TFilterProbeLoader = class(TDataLoader)
    protected
        procedure ParseLines(ALines: TStrings); override;
    end;

procedure TFilterProbeLoader.ParseLines(ALines: TStrings);
begin
end;

function Leaf(const AName: string): TDataSourceItem;
begin
    Result := Default(TDataSourceItem);
    Result.Title := AName;
    Result.FileName := AName;
    Result.IsLeaf := True;
end;

function Container(const ATitle: string): TDataSourceItem;
begin
    Result := Default(TDataSourceItem);
    Result.Title := ATitle;
    Result.IsLeaf := False;
end;

procedure TRepositoryFilterTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
end;

procedure TRepositoryFilterTest.AFileWithAReaderIsUsable;
begin
    AssertTrue(IsUsableFileName('quartz.xy'));
    AssertTrue('whatever the case', IsUsableFileName('QUARTZ.XY'));
end;

procedure TRepositoryFilterTest.AFileNothingReadsIsNot;
begin
    AssertFalse(IsUsableFileName('paper.pdf'));
    AssertFalse(IsUsableFileName('model.glb'));
end;

procedure TRepositoryFilterTest.ANameWithNoExtensionIsNot;
begin
    AssertFalse(IsUsableFileName('README'));
    AssertFalse(IsUsableFileName(''));
end;

procedure TRepositoryFilterTest.AnArchiveIsUsableForWhatItMayHold;
begin
    //  Only opening it can tell; it is listed, and says so when it holds
    //  nothing readable.
    AssertTrue(IsUsableFileName('data.zip'));
    AssertTrue(IsUsableFileName('pattern.xy.gz'));
    AssertTrue(IsUsableFileName('set.tar.gz'));
end;

procedure TRepositoryFilterTest.TheRuleFollowsTheReadersRegistered;
begin
    AssertFalse('nothing reads it yet', IsUsableFileName('scan.zzfilter'));
    RegisterDataLoader(TFilterProbeLoader, '.ZZFILTER', 'Filter probe');
    AssertTrue('a reader registered makes it usable',
        IsUsableFileName('scan.zzfilter'));
end;

procedure TRepositoryFilterTest.UnusableFilesAreLeftOutAndCounted;
var
    Items, Kept: TDataSourceItems;
    Dropped: longint;
begin
    SetLength(Items, 3);
    Items[0] := Leaf('paper.pdf');
    Items[1] := Leaf('quartz.xy');
    Items[2] := Leaf('photo.jpg');
    Kept := UsableItems(Items, Dropped);
    AssertEquals('one kept', 1, Length(Kept));
    AssertEquals('quartz.xy', Kept[0].Title);
    AssertEquals('two left out', 2, Dropped);
end;

procedure TRepositoryFilterTest.ContainersAreKeptWhateverTheyAreCalled;
var
    Items, Kept: TDataSourceItems;
    Dropped: longint;
begin
    //  What a container holds is filtered when it is opened; its own name
    //  says nothing about that.
    SetLength(Items, 2);
    Items[0] := Container('A record named like a paper.pdf');
    Items[1] := Leaf('notes.docx');
    Kept := UsableItems(Items, Dropped);
    AssertEquals(1, Length(Kept));
    AssertFalse('the container stayed', Kept[0].IsLeaf);
    AssertEquals(1, Dropped);
end;

procedure TRepositoryFilterTest.ARecordIsUsableWhenAnyOfItsFilesIs;
begin
    AssertTrue(AnyUsable(['readme.pdf', 'pattern.xy']));
    AssertFalse(AnyUsable(['readme.pdf', 'photo.png']));
    AssertFalse('a record with no files holds nothing', AnyUsable([]));
end;

procedure TRepositoryFilterTest.TheRefusalSaysHowMuchWasFoundAndWhyNoneIsShown;
var
    Message_: string;
begin
    Message_ := NothingUsableMessage(7, 'records');
    AssertTrue(Message_, Pos('Found 7 records', Message_) > 0);
    AssertTrue('and why none is shown: ' + Message_,
        Pos('Fit can read', Message_) > 0);
    AssertTrue('and what to do: ' + Message_, Pos('Try other words', Message_) > 0);
end;

initialization
    RegisterTest('unit', TRepositoryFilterTest);
end.
