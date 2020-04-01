{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit vectors;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SimpMath, SysUtils
{$IFDEF _WINDOWS}
    , Windows
{$ENDIF}
    ;

type
    { Returns value of parameter with given name. }
    FParamRequest = function(Param: string): double of object;
    TCharSet      = set of AnsiChar;
    TVector3Array = array of TDoubleVector3;

    ETools = class(Exception);

const
    { Error constants of the procedure evaluating user defined expression. }
    CALC_NO_ERRORS: longint = 0;
    CALC_INVALID_PARAMETER: longint = 1;
    CALC_INVALID_EXPRESSION: longint = 2;

{ Adds vector to the end of vector array. }
procedure AddVectorToArray(var Arr: TVector3Array; const Vector: TDoubleVector3);
{ Inserts vector into the given position Index. }
procedure InsertVectorIntoArray(var Arr: TVector3Array; const Index: longint;
    const Vector: TDoubleVector3);
{ Deletes vector with given position from array. }
procedure DeleteVectorFromArray(var Arr: TVector3Array; const Index: longint);

type
    TLongArray = array of longint;

{ Deletes item from array. }
procedure DeleteItemLongArr(var Arr: TLongArray; const Index: longint);
{ Inserts item into array. }
procedure InsertItemLongArr(var Arr: TLongArray; const Index: longint;
    const Item: longint);
{ Adds item to array. }
procedure AddItemLongArr(var Arr: TLongArray; const Item: longint);
{ Checks if index of item is valid. Otherwise throws an exception. }
procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: longint);

{ Functions converting vector to string and back. }

{ Mask defining format of the string passed to StringAsDoubleVector3. }
const
    DoubleVector3EditMask = '!\(0\.0999\,\ 0\.0999\,\ 0\.0999\);1;';

{ Converts vector to string. Result has format of DoubleVector3EditMask. }
function DoubleVector3AsString(const Vect: TDoubleVector3;
    { True means that number of digits after decimal separator is fixed. }
    FixedMode: boolean; Precision, Digits: longint): string;

function StringAsDoubleVector3(const Str: string): TDoubleVector3;

function StrToFloatDef(St: string; DefVal: extended): extended;
{ Converts string having format (*.*,*.*,*.*) to vector. }
function StrToVector3(const St: string): TDoubleVector3;
{ Converts vector to string having format (*.*,*.*,*.*). }
function Vector3ToStr(const Vector: TDoubleVector3): string;
{ Converts given number to number with given accuracy. }
function WithGivenAccuracy(
    { The number to convert. }
    Value: double;
    { Required number of decimal digits after decimal separator. }
    Decimals: longint): double;
{ Returns substring of command line string excluding path to executable
  enclosed in quotation marks. }
{$IFDEF _WINDOWS}
{ Delphi specific function. }
function GetCmdLineParameters: string;
{$ENDIF}
{ Returns random negative or positive value. }
function GetRandomWithSign: double;
{ Calculates expression passed via Expression parameter. }
function CalculateSimpExpr(var Expression: string; var ErrorCode: longint;
    const ParamRequest: FParamRequest): double;
{ Calculates expression passed via Expression parameter. }
function CalculateExpr(var Expression: string; var ErrorCode: longint;
    const ParamRequest: FParamRequest): double;
{ Searches char in string. Returns -1 if char not found. }
function GetCharPosition(St: string; Ch: char; Direction: shortint;
    StartIndex: longint): longint;
{ Searches chars from the given set in string.
  Direction = 1 means moving to the right,
  Direction = -1 means moving to the left.
  Returns -1 in the case of error. }
function GetCharSetPosition(St: string; ChSet: TCharSet; Direction: shortint;
    {  Direction =  1 - scan string from the beginning to the end.
       Direction = -1 - scan string from the end to the beginning. }
    StartIndex: longint; var Ch: char): longint;
{ Returns index of array and index of item in this array by through
  index of element among all arrays. }
procedure GetPosInArrays(
    { Array containing lengths of item arrays. }
    const ArraysLengths: array of longint;
    { Throug index of item in arrays. }
    const Index: longint;
    { Index of array containing required item. }
    var ArrayNumber: longint;
    { Index of item in this array. }
    var ArrayIndex: longint);

function ReadComponentByReader(const Reader: TReader): TComponent;

implementation

function StrToFloatDef(St: string; DefVal: extended): extended;
var
    Temp: extended;
begin
    try
        Temp := StrToFloat(St);
    except
        Temp := DefVal;
    end;
    Result := Temp;
end;

function StrToVector3(const St: string): TDoubleVector3;
var
    i:   longint;
    St2: string;
    Index, PrevIndex: longint;
    TempChar: char;
begin
    PrevIndex := 2;
    for i := 1 to 3 do
    begin
        TempChar := char(0);
        Index    := GetCharSetPosition(St, [',', ')'], 1, PrevIndex, TempChar);
        St2      := Copy(St, PrevIndex, Index - PrevIndex);
        Result[i] := StrToFloat(St2);
        PrevIndex := Index + 1;
    end;
end;

function Vector3ToStr(const Vector: TDoubleVector3): string;
begin
    Result := '(' + FloatToStrF(Vector[1], ffGeneral, 6, 4) + ',' +
        FloatToStrF(Vector[2], ffGeneral, 6, 4) + ',' +
        FloatToStrF(Vector[3], ffGeneral, 6, 4) + ')';
end;

{$warnings off}
function DoubleVector3AsString(const Vect: TDoubleVector3; FixedMode: boolean;
    Precision, Digits: longint): string;
var
    St: string;
    SavedDecimalSeparator: char;
begin
    SavedDecimalSeparator :=
{$IF NOT DEFINED(FPC)}
        FormatSettings.
{$ENDIF}
        DecimalSeparator;
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := '.';
    St := '(';
    if FixedMode then
    begin
        St := St + FloatToStrF(Vect[1], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[2], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[3], ffFixed, Precision, Digits);
    end
    else
    begin
        St := St + FloatToStr(Vect[1]) + ', ';
        St := St + FloatToStr(Vect[2]) + ', ';
        St := St + FloatToStr(Vect[3]);
    end;
    St     := St + ')';
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := SavedDecimalSeparator;
    Result := St;
end;

{$warnings on}

function StringAsDoubleVector3(const Str: string): TDoubleVector3;
var
    i, BegIndex, VectIndex: longint;
    Str2: string;
    PrevIsDelimiter: boolean;

begin
    BegIndex  := -1;
    VectIndex := 1;
    PrevIsDelimiter := False;
    for i := 1 to Length(Str) do
        if IsDelimiter('() ,_', Str, i) then
        begin
            if BegIndex <> -1 then
            begin
                Str2 := Copy(Str, BegIndex, i - BegIndex);
                Result[VectIndex] := StrToFloat(Str2);
                Inc(VectIndex);
                BegIndex := -1;
            end;
            PrevIsDelimiter := True;
        end
        else
        begin
            if PrevIsDelimiter then
                BegIndex    := i;
            PrevIsDelimiter := False;
        end;
end;

function WithGivenAccuracy(Value: double; Decimals: longint): double;
var
    PowerOf10: double;
    TempLong:  longint;

begin
    PowerOf10 := GetPowerOf10(Decimals);
    Result    := Value * PowerOf10;
    TempLong  := Round(Result);
    Result    := TempLong / PowerOf10;
end;

{$IFDEF _WINDOWS}
function GetCmdLineParameters: string;
var
    St:    string;
    Index: longint;
    Index1, Index2: longint;
begin
    St     := GetCommandLine;
    Index1 := -1;
    Index2 := -1;

    for Index := Length(St) downto 1 do
        if IsDelimiter('"', St, Index) then
        begin
            Index2 := Index;
            Break;
        end;
    for Index := Index2 - 1 downto 1 do
        if IsDelimiter('"', St, Index) then
        begin
            Index1 := Index;
            Break;
        end;

    if (Index1 = -1) or (Index2 = -1) then
    begin
        Result := '';
        Exit;
    end;
    St := Copy(St, Index1 + 1, Index2 - Index1 - 1);
    if UpperCase(ST) = UpperCase(ParamStr(0)) then
        St := '';
    Result := St;
end;

{$ENDIF}

function GetCharPosition(St: string; Ch: char; Direction: shortint;
    StartIndex: longint): longint;
var
    i: longint;
begin
    if StartIndex > Length(St) then
    begin
        Result := -1;
        Exit;
    end;
    case Direction of
        1:
        begin
            for i := StartIndex to Length(St) do
                if St[i] = Ch then
                begin
                    Result := i;
                    Exit;
                end;
            Result := -1;
        end;
        -1:
        begin
            for i := StartIndex downto 1 do
                if St[i] = Ch then
                begin
                    Result := i;
                    Exit;
                end;
            Result := -1;
        end;
        else
            Result := -1;
    end;
end;

function GetCharSetPosition(St: string; ChSet: TCharSet; Direction: shortint;
    StartIndex: longint; var Ch: char): longint;
var
    i: longint;
begin
    if StartIndex > Length(St) then
    begin
        Result := -1;
        Exit;
    end;
    case Direction of
        1:
        begin
            for i := StartIndex to Length(St) do
                if AnsiChar(St[i]) in ChSet then
                begin
                    Result := i;
                    Ch     := St[i];
                    Exit;
                end;
            Result := -1;
        end;
        -1:
        begin
            for i := StartIndex downto 1 do
                if AnsiChar(St[i]) in ChSet then
                begin
                    Result := i;
                    Ch     := St[i];
                    Exit;
                end;
            Result := -1;
        end;
        else
            Result := -1;
    end;
end;

{$warnings off}
function CalculateSimpExpr(var Expression: string; var ErrorCode: longint;
    const ParamRequest: FParamRequest): double;

    procedure MakeAllOper(var Expression: string; OperSet: TCharSet;
    var ErrorCode: longint; const ParamRequest: FParamRequest);
    var
        Index, IndexL, IndexR: longint;
        ArgStrL, ArgStrR: string;
        ArgL, ArgR: double;
        Value: double;
        St: string;
        TempIndex: longint;
        Oper, TempCh: char;
    begin
        repeat
            Oper  := char(0);
            Index := GetCharSetPosition(Expression, OperSet, 1, 1, Oper);
            if Index = -1 then
                Exit;

            TempCh := char(0);
            IndexL := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], -1, Index - 1, TempCh);
            if IndexL = -1 then
                IndexL := 0;
            IndexR     := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], 1, Index + 1, TempCh);
            if IndexR = -1 then
                IndexR := Length(Expression) + 1;
            if (Expression[1] = '-') then
                if (Index = 1) then
                begin
                    { The case when the first number is negative.
                      Processing '+', '-' operations. }
                    TempIndex := Index;
                    Index     :=
                        GetCharSetPosition(Expression, OperSet, 1, TempIndex + 1, Oper);
                    if Index = -1 then
                        Exit;
                    IndexR := GetCharSetPosition(Expression,
                        ['*', '/', '+', '-'], 1, Index + 1, TempCh);
                    if IndexR = -1 then
                        IndexR := Length(Expression) + 1;
                    IndexL     := 0;
                end
                else
                    IndexL := 0//  ['*', '/']
            ;

            if (IndexR = Index + 1) and (Expression[Index + 1] = '-') then
            begin
                { The case when the number following the operation is negative. }
                IndexR := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexR + 1, TempCh);
                if IndexR = -1 then
                    IndexR := Length(Expression) + 1;
            end;

            if (IndexL = Index - 1) and (Expression[Index] = '-') then
            begin
                { The case when the number preceding the operation is negative. }
                IndexL := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexL - 1, TempCh);
                if IndexR = -1 then
                    IndexL := 0;
            end;

            ArgStrL := Copy(Expression, IndexL + 1, Index - IndexL - 1);
            ArgStrR := Copy(Expression, Index + 1, IndexR - Index - 1);
            Delete(Expression, IndexL + 1, IndexR - IndexL - 1);
            if ArgStrL <> '' then
            begin
                try
                    ArgL := StrToFloat(ArgStrL);
                except
                    if Assigned(ParamRequest) then
                    begin
                        if ArgStrL[1] in ['-', '+'] then
                            case ArgStrL[1] of
                                '-': ArgL :=
                                        (-1) *
                                        ParamRequest(Copy(ArgStrL,
                                        2, Length(ArgStrL) - 1));
                                '+': ArgL :=
                                        ParamRequest(Copy(ArgStrL,
                                        2, Length(ArgStrL) - 1));
                            end
                        else
                            ArgL := ParamRequest(ArgStrL);
                    end{if Assigned(ParamRequest) then...}
                    else
                    begin
                        ErrorCode := CALC_INVALID_PARAMETER;
                        Exit;
                    end;
                end;{except...}
            end     {if ArgStrL <> '' then...}
            else
                ArgL := 0;

            try
                ArgR := StrToFloat(ArgStrR);
            except
                if Assigned(ParamRequest) then
                begin
                    if ArgStrR[1] in ['-', '+'] then
                        case ArgStrR[1] of
                            '-': ArgR :=
                                    (-1) *
                                    ParamRequest(Copy(ArgStrR, 2, Length(ArgStrR) - 1));
                            '+': ArgR :=
                                    ParamRequest(Copy(ArgStrR, 2, Length(ArgStrR) - 1));
                        end
                    else
                        ArgR := ParamRequest(ArgStrR);
                end
                else
                begin
                    ErrorCode := CALC_INVALID_PARAMETER;
                    Exit;
                end;
            end;

            case Oper of
                '*': Value := ArgL * ArgR;
                '/': Value := ArgL / ArgR;
                '+': Value := ArgL + ArgR;
                '-': Value := ArgL - ArgR;
            end;

            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, IndexL + 1);
        until Index = -1;
    end;{MakeAllOper}

var
    SaveDecimalSeparator: char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator :=
{$IF NOT DEFINED(FPC)}
        FormatSettings.
{$ENDIF}
        DecimalSeparator;
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := '.';
    MakeAllOper(Expression, ['*', '/'], ErrorCode, ParamRequest);
    MakeAllOper(Expression, ['+', '-'], ErrorCode, ParamRequest);
    try
        Result := StrToFloat(Expression);
    except
        if Assigned(ParamRequest) then
        begin
            if Expression[1] in ['-', '+'] then
                case Expression[1] of
                    '-': Result :=
                            (-1) * ParamRequest(
                            Copy(Expression, 2, Length(Expression) - 1));
                    '+': Result :=
                            ParamRequest(Copy(Expression, 2, Length(Expression) - 1));
                end
            else
                Result := ParamRequest(Expression);
        end
        else
        begin
            Result    := 0;
            ErrorCode := CALC_INVALID_PARAMETER;
            Exit;
        end;
    end;
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := SaveDecimalSeparator;
end;

function CalculateExpr(var Expression: string; var ErrorCode: longint;
    const ParamRequest: FParamRequest): double;
var
    Index, Index2: longint;
    SExpr: string;
    Value: double;
    St:    string;
    SaveDecimalSeparator: char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator :=
{$IF NOT DEFINED(FPC)}
        FormatSettings.
{$ENDIF}
        DecimalSeparator;
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := '.';
    repeat
        Index := GetCharPosition(Expression, ')', 1, 1);
        if Index <> -1 then
        begin
            Index2 := GetCharPosition(Expression, '(', -1, Index);
            SExpr  := Copy(Expression, Index2 + 1, Index - Index2 - 1);
            Delete(Expression, Index2, Index - Index2 + 1);
            Value := CalculateSimpExpr(SExpr, ErrorCode, ParamRequest);
            if ErrorCode <> 0 then
            begin
                Result := 0;
                Exit;
            end;
            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, Index2);
        end;
    until Index = -1;
    Result := CalculateSimpExpr(Expression, ErrorCode, ParamRequest);
{$IF NOT DEFINED(FPC)}
    FormatSettings.
{$ENDIF}
        DecimalSeparator := SaveDecimalSeparator;
end;

{$warnings on}

function GetRandomWithSign: double;
begin
    case Round(Random) of
        0: Result := (-1) * Random;
        else
            Result := Random;
    end;
end;

procedure GetPosInArrays(const ArraysLengths: array of longint;
    const Index: longint; var ArrayNumber: longint; var ArrayIndex: longint);
var
    TotalNumber: longint;   //  Total number of items in arrays.
    LengthsSum: longint;    //  Sum of array lengths.
    i: longint;
begin
    if Length(ArraysLengths) = 0 then
        raise ETools.Create('ArraysLengths must be assigned...');
    TotalNumber := 0;
    for i := 0 to Length(ArraysLengths) - 1 do
        TotalNumber := TotalNumber + ArraysLengths[i];
    if (Index < 0) or (Index >= TotalNumber) then
        raise ETools.Create('Invalid item index...');

    LengthsSum := 0;
    for i := 0 to Length(ArraysLengths) - 1 do
        if (Index >= LengthsSum) and (Index < LengthsSum + ArraysLengths[i]) then
        begin
            ArrayNumber := i;
            ArrayIndex  := Index - LengthsSum;
            Exit;
        end
        else
            LengthsSum := LengthsSum + ArraysLengths[i];
end;

procedure AddVectorToArray(var Arr: TVector3Array; const Vector: TDoubleVector3);
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Vector;
end;

procedure InsertVectorIntoArray(var Arr: TVector3Array; const Index: longint;
    const Vector: TDoubleVector3);
var
    i: longint;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do
        Arr[i + 1] := Arr[i];
    Arr[Index]     := Vector;
end;

procedure DeleteVectorFromArray(var Arr: TVector3Array; const Index: longint);
var
    i: longint;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    for i := Index + 1 to Length(Arr) - 1 do
        Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

function ReadComponentByReader(const Reader: TReader): TComponent;
var
    SaveReaderOwner: TComponent;
begin
    SaveReaderOwner := Reader.Owner;
    Reader.Owner := nil;
    Result := Reader.ReadComponent(nil);
    Reader.Owner := SaveReaderOwner;
end;

procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: longint);
begin
    if (Index < MinIndex) or (Index > MaxIndex) then
        raise ETools.Create('Invalid item index (' + IntToStr(Index) + ')...');
end;

procedure DeleteItemLongArr(var Arr: TLongArray; const Index: longint);
var
    i: longint;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    for i := Index + 1 to Length(Arr) - 1 do
        Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

procedure InsertItemLongArr(var Arr: TLongArray; const Index: longint;
    const Item: longint);
var
    i: longint;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do
        Arr[i + 1] := Arr[i];
    Arr[Index]     := Item;
end;

procedure AddItemLongArr(var Arr: TLongArray; const Item: longint);
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Item;
end;


end.
