// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The windows that ask the user for one number, found by the name the
window asks them by.)

The window names no such window: it asks value_dialogs for one by name, and a
product registers what it carries. So what the user meets when they choose
Set Background Fraction is whatever this registry answers - nothing, in a
product without the window (the command is then not offered), or the window
registered last under that name.

A UNIT test over doubles: no form is created.
}
unit testcase_value_dialogs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, value_dialogs;

type
    { A window that only remembers what it was given. }
    TFixedValueDialog = class(TObject, IValueDialog)
    public
        FValue: double;
        function Execute: boolean;
        function GetValue: double;
        procedure SetValue(AValue: double);
        function Text: string;
    end;

    TValueDialogsTest = class(TTestCase)
    private
        FFirst, FSecond, FOther: TFixedValueDialog;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ANameNobodyRegisteredHasNoWindow;
        procedure AWindowIsFoundByItsName;
        procedure TheLastRegistrationUnderANameWins;
        procedure TwoNamesKeepTheirOwnWindows;
    end;

implementation

{ The names are this test's own, so nothing a product registers is replaced. }
const
    FirstName = 'test.value-dialog.first';
    SecondName = 'test.value-dialog.second';
    { A name no other test uses: a slot an earlier test left must not decide
      which window is the last one registered. }
    ReplacedName = 'test.value-dialog.replaced';

function TFixedValueDialog.Execute: boolean;
begin
    Result := True;
end;

function TFixedValueDialog.GetValue: double;
begin
    Result := FValue;
end;

{ The window as the registry hands it out. }
function AsDialog(ADialog: TFixedValueDialog): IValueDialog;
begin
    Result := ADialog;
end;

procedure TFixedValueDialog.SetValue(AValue: double);
begin
    FValue := AValue;
end;

function TFixedValueDialog.Text: string;
begin
    Result := FloatToStr(FValue);
end;

procedure TValueDialogsTest.SetUp;
begin
    FFirst := TFixedValueDialog.Create;
    FSecond := TFixedValueDialog.Create;
    FOther := TFixedValueDialog.Create;
end;

procedure TValueDialogsTest.TearDown;
begin
    //  The registry outlives the test: leave it naming nothing freed here.
    RegisterValueDialog(FirstName, nil);
    RegisterValueDialog(SecondName, nil);
    RegisterValueDialog(ReplacedName, nil);
    FreeAndNil(FFirst);
    FreeAndNil(FSecond);
    FreeAndNil(FOther);
end;

procedure TValueDialogsTest.ANameNobodyRegisteredHasNoWindow;
begin
    AssertTrue('no window answers a name nothing registered',
        ValueDialog('test.value-dialog.never-registered') = nil);
end;

procedure TValueDialogsTest.AWindowIsFoundByItsName;
begin
    RegisterValueDialog(FirstName, FFirst);
    AssertTrue('the window registered under the name',
        ValueDialog(FirstName) = AsDialog(FFirst));
end;

procedure TValueDialogsTest.TheLastRegistrationUnderANameWins;
begin
    //  There is one client, and a product's forms may be registered again (a
    //  second window built in one session): the newer one is the one asked.
    RegisterValueDialog(ReplacedName, FFirst);
    RegisterValueDialog(ReplacedName, FOther);
    AssertTrue('the newer window answers',
        ValueDialog(ReplacedName) = AsDialog(FOther));
end;

procedure TValueDialogsTest.TwoNamesKeepTheirOwnWindows;
begin
    RegisterValueDialog(FirstName, FFirst);
    RegisterValueDialog(SecondName, FSecond);
    FFirst.SetValue(1.5);
    FSecond.SetValue(0.25);
    AssertEquals('the first name asks the first window', 1.5,
        ValueDialog(FirstName).Value, 0);
    AssertEquals('the second name asks the second', 0.25,
        ValueDialog(SecondName).Value, 0);
end;

initialization
    RegisterTest('unit', TValueDialogsTest);
end.
