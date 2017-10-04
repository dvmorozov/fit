{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used to iterate through set of possible task solution.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CombEnumerator;

{$MODE Delphi}

interface

uses SysUtils;

type
    { Enumerates all possible combinations of a set of discrete values. Value must have at least single possible value. }
    TCombEnumerator = class
    protected
        { Contains numbers of possible values of discrete quantities. Quantities by themselves can be any. }
        NumbersOfValues: array of LongInt;
        { Indexes of possible values of discrete quantities. Set of indexes enumerates possible combinations of quantity values.
          Each index can take value from 0 to the value of corresponding item from NumberOfValues minus 1. }
        ValuesIndexes: array of LongInt;
        { Through index of currently selected combination. }
        FCurrentComb: LongInt;
        { Is True if current combination was set up via SetCurrentComb, False otherwise. }
        FIsCombDefined: Boolean;
        { Returns total number of combinations. }
        function GetCombNumber: LongInt;
        { Returns number of combinations which can be created with quantities belonging to given interval of indexes.
          @param(StartIndex Index of quantity in the NumberOfValues array.)
          @param(StopIndex Index of quantity in the NumberOfValues array.) }
        function GetCombNumberStartStop(
            const StartIndex, StopIndex: LongInt): LongInt;
        { Selects combination by given through index. }
        procedure SetCurrentComb(const ACurrentComb: LongInt); virtual;
        { Returns current value index for the given quantity index. }
        function GetValueIndex(index: LongInt): LongInt;
        { Returns number of discrete quantities. }
        function GetValuesNumber: LongInt;

    public
        destructor Destroy; override;
        { Adds new quantity having given number of discrete values. }
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); virtual;
        { Removes all quantities. }
        procedure ClearListOfNumbersOfValues; virtual;

        property CombNumber: LongInt read GetCombNumber;
        property CurrentComb: LongInt read FCurrentComb write SetCurrentComb;
        property ValueIndex[index: LongInt]: LongInt read GetValueIndex;
        property ValuesNumber: LongInt read GetValuesNumber;
    end;

    { Defines functionality of discrete quantity. Quantity must have at least single value. }
    IDiscretValue = interface
        { Returns number of quantity values. }
        function GetNumberOfValues: LongInt;
        { Returns index of currently selected value. }
        function GetValueIndex: LongInt;
        { Sets up index of currently selected value. }
        procedure SetValueIndex(const AValueIndex: LongInt);

        property NumberOfValues: LongInt read GetNumberOfValues;
        property ValueIndex: LongInt read GetValueIndex write SetValueIndex;
    end;

    { Container of discrete quantities. See @link(IDiscretValue). }
    TCombSelector = class(TCombEnumerator)
    protected
        FValuesList: array of IDiscretValue;
        procedure SetCurrentComb(const ACurrentComb: LongInt); override;

    public
        { In addition to inherited behaviour adds empty item to FValueList. }
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); override;
        { Releases interfaces and destroys the array FValuesList. }
        procedure ClearListOfNumbersOfValues; override;
        { Adds new interface. }
        procedure AddDiscretValue(const AValue: IDiscretValue);
        { Calls @link(ClearListOfNumbersOfValues). }
        procedure ClearDiscretValuesList;
    end;

implementation

destructor TCombEnumerator.Destroy;
begin
    ClearListOfNumbersOfValues;
    inherited Destroy;
end;

function TCombEnumerator.GetCombNumber: LongInt;
begin
    Result := GetCombNumberStartStop(0, ValuesNumber - 1);
end;

function TCombEnumerator.GetCombNumberStartStop(
    const StartIndex, StopIndex: LongInt): LongInt;
var i: LongInt;
begin
    Result := 0;
    for i := StartIndex to StopIndex do
    begin
        if (Result <> 0) and (NumbersOfValues[i] <> 0) then
            Result := Result * NumbersOfValues[i]
        else Result := Result + NumbersOfValues[i];
    end;
end;

procedure TCombEnumerator.AddNumberOfValues(const ANumberOfValues: LongInt);
begin
    SetLength(NumbersOfValues, Length(NumbersOfValues) + 1);
    SetLength(ValuesIndexes, Length(ValuesIndexes) + 1);
    NumbersOfValues[Length(NumbersOfValues) - 1] := ANumberOfValues;
    FIsCombDefined := False;
end;

procedure TCombEnumerator.ClearListOfNumbersOfValues;
begin
    FIsCombDefined := False;
    Finalize(NumbersOfValues);
    Finalize(ValuesIndexes);
end;

procedure TCombEnumerator.SetCurrentComb(const ACurrentComb: LongInt);
var TempCurrentComb, TempCombNumber: LongInt;
    i: LongInt;
begin
    Assert(not ((ACurrentComb < 0) or (ACurrentComb >= CombNumber)));
    //  Algorithm selecting entity combination by through index.
    TempCurrentComb := ACurrentComb;
    FCurrentComb := ACurrentComb;
    for i := 0 to ValuesNumber - 2 do
    begin
        TempCombNumber := GetCombNumberStartStop(i + 1, ValuesNumber - 1);
        ValuesIndexes[i] := TempCurrentComb div TempCombNumber;
        TempCurrentComb := TempCurrentComb mod TempCombNumber;
    end;
    ValuesIndexes[ValuesNumber - 1] := TempCurrentComb;
    FIsCombDefined := True;
end;

function TCombEnumerator.GetValueIndex(index: LongInt): LongInt;
begin
    Assert(FIsCombDefined);
    Assert(not ((index < 0) or (index >= ValuesNumber)));
    Result := ValuesIndexes[index];
end;

function TCombEnumerator.GetValuesNumber: LongInt;
begin
    Result := Length(NumbersOfValues);
end;

procedure TCombSelector.AddDiscretValue(const AValue: IDiscretValue);
begin
    AddNumberOfValues(AValue.NumberOfValues);
    FValuesList[Length(FValuesList) - 1] := AValue;
end;

procedure TCombSelector.ClearDiscretValuesList;
begin
    ClearListOfNumbersOfValues;
end;

procedure TCombSelector.AddNumberOfValues(const ANumberOfValues: LongInt);
begin
    inherited AddNumberOfValues(ANumberOFValues);
    SetLength(FValuesList, Length(FValuesList) + 1);
    FValuesList[Length(FValuesList) - 1] := nil;
end;

procedure TCombSelector.ClearListOfNumbersOfValues;
var i: LongInt;
begin
    inherited ClearListOfNumbersOfValues;
    for i := 0 to Length(FValuesList) - 1 do
        FValuesList[i] := nil;  //  Calling _Release.
    Finalize(FValuesList);
end;

procedure TCombSelector.SetCurrentComb(const ACurrentComb: LongInt);
var i: LongInt;
begin
    inherited SetCurrentComb(ACurrentComb);
    for i := 0 to ValuesNumber - 1 do
        if Assigned(FValuesList[i]) then
            FValuesList[i].ValueIndex := ValueIndex[i];
end;

end.


