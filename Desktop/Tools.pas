{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of auxiliary functions.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit Tools;

{$MODE Delphi}

interface

uses SysUtils, Classes, SimpMath, CBRCComponent;

type
	{ Returns value of parameter with given name. }
    FParamRequest = function(Param: string): Double of object;

    TCharSet = set of Char;
    TVector3Array = array of TDoubleVector3;

    ETools = class(Exception);

const
	{ Error constants of the procedure evaluating user defined expression. }
    CALC_NO_ERRORS          : LongInt = 0;
    CALC_INVALID_PARAMETER  : LongInt = 1;
    CALC_INVALID_EXPRESSION : LongInt = 2;

{ Adds vector to the end of vector array. }
procedure AddVectorToArray(
    var Arr: TVector3Array;
    const Vector: TDoubleVector3
    );
{ Inserts vector into the given position Index. }
procedure InsertVectorIntoArray(
    var Arr: TVector3Array;
    const Index: LongInt;
    const Vector: TDoubleVector3
    );
{ Deletes vector with given position from array. }	
procedure DeleteVectorFromArray(
    var Arr: TVector3Array;
    const Index: LongInt
    );

type
    TLongArray = array of LongInt;

{ Deletes item from array. }		
procedure DeleteItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt
    );
{ Inserts item into array. }
procedure InsertItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt;
    const Item: LongInt
    );
{ Adds item to array. }	
procedure AddItemLongArr(
    var Arr: TLongArray;
    const Item: LongInt
    );
{ Checks if index of item is valid. Otherwise throws an exception. }
procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: LongInt);

{ Functions converting vector to string and back. }

{ Mask defining format of the string passed to StringAsDoubleVector3. }
const DoubleVector3EditMask = '!\(0\.0999\,\ 0\.0999\,\ 0\.0999\);1;';

{ Converts vector to string. Result has format of DoubleVector3EditMask. }
function DoubleVector3AsString(const Vect: TDoubleVector3;
	{ True means that number of digits after decimal separator is fixed. }
    FixedMode: Boolean;
    Precision, Digits: LongInt): string;

function StringAsDoubleVector3(const Str: string): TDoubleVector3;

function StrToFloatDef(St: string; DefVal: Extended): Extended;
{ Converts string having format (*.*,*.*,*.*) to vector. }
function StrToVector3(const St: string): TDoubleVector3;
{ Converts vector to string having format (*.*,*.*,*.*). }
function Vector3ToStr(const Vector: TDoubleVector3): string;
{ Converts given number to number with given accuracy. }
function WithGivenAccuracy(
	{ The number to convert. }
    Value: Double;
	{ Required number of decimal digits after decimal separator. }
    Decimals: LongInt
    ): Double;
{ Returns substring of command line string excluding path to executable 
  enclosed in quotation marks. }
function GetCmdLineParameters: string;
{ Returns random negative or positive value. }
function GetRandomWithSign: Double;
{ Calculates expression passed via Expression parameter. }
function CalculateSimpExpr(var Expression: string; var ErrorCode: LongInt;
    const ParamRequest: FParamRequest): Double;
{ Calculates expression passed via Expression parameter. }
function CalculateExpr(var Expression: string; var ErrorCode: LongInt;
    const ParamRequest: FParamRequest): Double;
{ Searches char in string. Returns -1 if char not found. }
function GetCharPosition(St: string; Ch: Char;
    Direction: ShortInt; StartIndex: LongInt): LongInt;
{ Searches chars from the given set in string. 
  Direction = 1 means moving to the right,
  Direction = -1 means moving to the left. 
  Returns -1 in the case of error. }
function GetCharSetPosition(St: string; ChSet: TCharSet;
    Direction: ShortInt;
    StartIndex: LongInt; var Ch: Char): LongInt;
{ Returns index of array and index of item in this array by through index of element among all arrays. }
procedure GetPosInArrays(
	{ Array containing lengths of item arrays. }
    const ArraysLengths: array of LongInt;
	{ Throug index of item in arrays. }
    const Index: LongInt;
	{ Index of array containing required item. }
    var ArrayNumber: LongInt;
	{ Index of item in this array. }
    var ArrayIndex: LongInt
    );

function ReadComponentByReader(const Reader: TReader): TComponent;
{ Destructs objects performing additional actions. To the moment it is only checking that
  type is TCBRCComponent and calling its method Free. All application objects should be
  destroyed by this function. }
procedure UtilizeObject(PtrToObject: TObject);

implementation

function StrToFloatDef(St: string; DefVal: Extended): Extended;
var Temp: Extended;
begin
    try Temp := StrToFloat(St);
    except Temp := DefVal; end;
    Result := Temp;
end;

function StrToVector3(const St: string): TDoubleVector3;
var i: LongInt;
    St2: string;
    Index, PrevIndex: LongInt;
    TempChar: Char;
begin
    PrevIndex := 2;     //  символы нумеруются, начиная с 1 ?
    for i := 1 to 3 do
    begin
        TempChar := Char(0);
        Index := GetCharSetPosition(St, [',', ')'], 1, PrevIndex, TempChar);
        St2 := Copy(St, PrevIndex, Index - PrevIndex);
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
function DoubleVector3AsString(const Vect: TDoubleVector3;
    FixedMode: Boolean; Precision, Digits: LongInt): string;
var St: string;
    SavedDecimalSeparator: Char;
begin
    SavedDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    St := '(';
    if FixedMode then begin
        St := St + FloatToStrF(Vect[1], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[2], ffFixed, Precision, Digits) + ', ';
        St := St + FloatToStrF(Vect[3], ffFixed, Precision, Digits);
    end else begin
        St := St + FloatToStr(Vect[1]) + ', ';
        St := St + FloatToStr(Vect[2]) + ', ';
        St := St + FloatToStr(Vect[3]);
    end;
    St := St + ')';
    DecimalSeparator := SavedDecimalSeparator;
    Result := St;
end;
{$warnings on}

function StringAsDoubleVector3(const Str: string): TDoubleVector3;
var i, BegIndex, VectIndex: LongInt;
    Str2: string;
    PrevIsDelimiter: Boolean;
        //  предыдуший символ был среди разделителей ?
begin
    BegIndex := -1; VectIndex := 1; PrevIsDelimiter := False;
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
        end else
        begin
            if PrevIsDelimiter then BegIndex := i;
            PrevIsDelimiter := False;
        end;
end;

function WithGivenAccuracy(Value: Double; Decimals: LongInt): Double;
var PowerOf10: Double;
    TempLong: LongInt;
//    St: string;
begin
    PowerOf10 := GetPowerOf10(Decimals);
    Result := Value * PowerOf10;
    TempLong := Round(Result);
    Result := TempLong / PowerOf10;
//    St := FloatToStrF(Value, ffFixed, 8, Decimals);
//    Result := StrToFloat(St);
end;

function GetCmdLineParameters: string;
var St: string;
    Index: LongInt;
    Index1, Index2: LongInt;
begin
    St := '';//???GetCommandLine;
    Index1 := -1; Index2 := -1;

    for Index := Length(St) downto 1 do
        if IsDelimiter('"', St, Index) then begin Index2 := Index; Break end;
    for Index := Index2 - 1 downto 1 do
        if IsDelimiter('"', St, Index) then begin Index1 := Index; Break end;

    if (Index1 = -1) or (Index2 = -1) then begin Result := ''; Exit end;
    St := Copy(St, Index1 + 1, Index2 - Index1 - 1);
    if UpperCase(ST) = UpperCase(ParamStr(0)) then St := '';
    Result := St;
end;

function GetCharPosition(St: string; Ch: Char;
    Direction: ShortInt; StartIndex: LongInt): LongInt;
var i: LongInt;
begin
    if StartIndex > Length(St) then begin Result := -1; Exit end;
    case Direction of
        1 : begin
            for i := StartIndex to Length(St) do
                if St[i] = Ch then begin Result := i; Exit end;
            Result := -1;
        end;
        -1 : begin
            for i := StartIndex downto 1 do
                if St[i] = Ch then begin Result := i; Exit end;
            Result := -1;
        end;
        else Result := -1;
    end;
end;

function GetCharSetPosition(St: string; ChSet: TCharSet;
    Direction: ShortInt; StartIndex: LongInt; var Ch: Char): LongInt;
var i: LongInt;
begin
    if StartIndex > Length(St) then begin Result := -1; Exit end;
    case Direction of
        1 : begin
            for i := StartIndex to Length(St) do
                if St[i] in ChSet then begin Result := i; Ch := St[i]; Exit end;
            Result := -1;
        end;
        -1 : begin
            for i := StartIndex downto 1 do
                if St[i] in ChSet then begin Result := i; Ch := St[i]; Exit end;
            Result := -1;
        end;
        else Result := -1;
    end;
end;

{$warnings off}
function CalculateSimpExpr(var Expression: string; var ErrorCode: LongInt;
const ParamRequest: FParamRequest): Double;

    procedure MakeAllOper(var Expression: string; OperSet: TCharSet;
        var ErrorCode: LongInt; const ParamRequest: FParamRequest);
    var Index, IndexL, IndexR: LongInt;
        ArgStrL, ArgStrR: string;
        ArgL, ArgR: Double;
        Value: Double;
        St: string;
        TempIndex: LongInt;
        Oper, TempCh: Char;
    begin
        repeat
            Oper := Char(0);
            Index := GetCharSetPosition(Expression, OperSet, 1, 1, Oper);
            if Index = -1 then Exit;
            TempCh := Char(0);
            IndexL := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], -1, Index - 1, TempCh);
            if IndexL = -1 then IndexL := 0;
            IndexR := GetCharSetPosition(Expression,
                ['*', '/', '+', '-'], 1, Index + 1, TempCh);
            if IndexR = -1 then IndexR := Length(Expression) + 1;
            if (Expression[1] = '-') then
            begin
                if (Index = 1) then
                begin
                    //  ситуация, когда первое число отрицательное}
                    //  здесь проходит когда проверяется наличие операций ['+', '-']
                    TempIndex := Index;
                    Index := GetCharSetPosition(Expression, OperSet, 1, TempIndex + 1, Oper);
                    if Index = -1 then Exit;
                    IndexR := GetCharSetPosition(Expression,
                        ['*', '/', '+', '-'], 1, Index + 1, TempCh);
                    if IndexR = -1 then IndexR := Length(Expression) + 1;
                    IndexL := 0;
                end else IndexL := 0    //  ['*', '/']
            end;

            if (IndexR = Index + 1) and (Expression[Index + 1] = '-') then
            begin
                //  ситуация, когда следующее за операцией число отрицательное
                IndexR := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexR + 1, TempCh);
                if IndexR = -1 then IndexR := Length(Expression) + 1;
            end;

            if (IndexL = Index - 1) and (Expression[Index] = '-') then
            begin
                //  ситуация, когда предыдущее операции число отрицательное
                IndexL := GetCharSetPosition(Expression,
                    ['*', '/', '+', '-'], 1, IndexL - 1, TempCh);
                if IndexR = -1 then IndexL := 0;
            end;

            ArgStrL := Copy(Expression, IndexL + 1, Index - IndexL - 1);
            ArgStrR := Copy(Expression, Index + 1, IndexR - Index - 1);
            Delete(Expression, IndexL + 1, IndexR - IndexL - 1);
            if ArgStrL <> '' then
            begin
                try ArgL := StrToFloat(ArgStrL);
                except
                    if Assigned(ParamRequest) then
                    begin
                        if ArgStrL[1] in ['-', '+'] then
                            case ArgStrL[1] of
                                '-': ArgL := (-1) * ParamRequest(Copy(
                                    ArgStrL, 2, Length(ArgStrL) - 1));
                                '+': ArgL := ParamRequest(Copy(ArgStrL, 2,
                                    Length(ArgStrL) - 1));
                            end
                        else ArgL := ParamRequest(ArgStrL)
                    end{if Assigned(ParamRequest) then...}
                    else begin ErrorCode := CALC_INVALID_PARAMETER; Exit end;
                end;{except...}
            end{if ArgStrL <> '' then...}
            else ArgL := 0;

            try ArgR := StrToFloat(ArgStrR);
            except
                if Assigned(ParamRequest) then
                begin
                    if ArgStrR[1] in ['-', '+'] then
                        case ArgStrR[1] of
                            '-': ArgR := (-1) * ParamRequest(Copy(
                                ArgStrR, 2, Length(ArgStrR) - 1));
                            '+': ArgR := ParamRequest(Copy(ArgStrR, 2,
                                Length(ArgStrR) - 1));
                        end
                    else ArgR := ParamRequest(ArgStrR)
                end
                else begin ErrorCode := CALC_INVALID_PARAMETER; Exit end;
            end;

            case Oper of
                '*' : Value := ArgL * ArgR;
                '/' : Value := ArgL / ArgR;
                '+' : Value := ArgL + ArgR;
                '-' : Value := ArgL - ArgR;
            end;

            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, IndexL + 1);
        until Index = -1;
    end;{MakeAllOper}

var SaveDecimalSeparator: Char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    MakeAllOper(Expression, ['*', '/'], ErrorCode, ParamRequest);
    MakeAllOper(Expression, ['+', '-'], ErrorCode, ParamRequest);
    try Result := StrToFloat(Expression);
    except
        if Assigned(ParamRequest) then
        begin
            if Expression[1] in ['-', '+'] then
                case Expression[1] of
                    '-': Result := (-1) * ParamRequest(Copy(
                        Expression, 2, Length(Expression) - 1));
                    '+': Result := ParamRequest(Copy(
                        Expression, 2, Length(Expression) - 1));
                end
            else Result := ParamRequest(Expression)
        end
        else begin Result := 0; ErrorCode := CALC_INVALID_PARAMETER; Exit end;
    end;
    DecimalSeparator := SaveDecimalSeparator;
end;

function CalculateExpr(var Expression: string;
    var ErrorCode: LongInt; const ParamRequest: FParamRequest): Double;
var Index, Index2: LongInt;
    SExpr: string;
    Value: Double;
    St: string;
    SaveDecimalSeparator: Char;
begin
    ErrorCode := CALC_NO_ERRORS;
    SaveDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    repeat
        Index := GetCharPosition(Expression, ')', 1, 1);
        if Index <> -1 then
        begin
            Index2 := GetCharPosition(Expression, '(', -1, Index);
            SExpr := Copy(Expression, Index2 + 1, Index - Index2 - 1);
            Delete(Expression, Index2, Index - Index2 + 1);
            Value := CalculateSimpExpr(SExpr, ErrorCode, ParamRequest);
            if ErrorCode <> 0 then begin Result := 0; Exit end;
            St := FloatToStrF(Value, ffGeneral, 6, 4);
            Insert(St, Expression, Index2);
        end;
    until Index = -1;
    Result := CalculateSimpExpr(Expression, ErrorCode, ParamRequest);
    DecimalSeparator := SaveDecimalSeparator;
end;
{$warnings on}

function GetRandomWithSign: Double;
begin
    case Round(Random) of
        0: Result := (-1) * Random;
        1: Result := Random;
    end;
end;

procedure GetPosInArrays(
    const ArraysLengths: array of LongInt; const Index: LongInt;
    var ArrayNumber: LongInt; var ArrayIndex: LongInt);
var TotalNumber: LongInt;   //  полное число элементов
    LengthsSum: LongInt;    //  сумма длин массивов
    i: LongInt;
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
    begin
        if (Index >= LengthsSum) and (Index < LengthsSum + ArraysLengths[i]) then
        begin
            ArrayNumber := i;
            ArrayIndex := Index - LengthsSum;
            Exit;
        end else LengthsSum := LengthsSum + ArraysLengths[i];
    end;
end;

procedure AddVectorToArray(
    //  добавляет вектор к концу массива векторов
    var Arr: TVector3Array;
    const Vector: TDoubleVector3
    );
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Vector;
end;

procedure InsertVectorIntoArray(
    //  вставляет вектор в массив в позицию Index
    var Arr: TVector3Array;
    const Index: LongInt;
    const Vector: TDoubleVector3
    );
var i: LongInt;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do Arr[i + 1] := Arr[i];
    Arr[Index] := Vector;
end;

procedure DeleteVectorFromArray(
    var Arr: TVector3Array;
    const Index: LongInt
    );
var i: LongInt;
begin
    if (Index < 0) or (Index >= Length(Arr)) then
        raise ETools.Create('Invalid array index...');
    for i := Index + 1 to Length(Arr) - 1 do Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

function ReadComponentByReader(const Reader: TReader): TComponent;
var SaveReaderOwner: TComponent;
begin
    SaveReaderOwner := Reader.Owner;
    Reader.Owner := nil;
    Result := Reader.ReadComponent(nil);
    Reader.Owner := SaveReaderOwner;
end;

procedure UtilizeObject(PtrToObject: TObject);
begin
    if PtrToObject is TCBRCComponent then TCBRCComponent(PtrToObject).Free
    else PtrToObject.Free;
end;

procedure CheckArrItemIndex(const MinIndex, MaxIndex, Index: LongInt);
begin
    if (Index < MinIndex) or (Index > MaxIndex) then
        raise ETools.Create('Invalid item index (' + IntToStr(Index) + ')...');
end;

procedure DeleteItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt
    );
var i: LongInt;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    for i := Index + 1 to Length(Arr) - 1 do Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
end;

procedure InsertItemLongArr(
    var Arr: TLongArray;
    const Index: LongInt;
    const Item: LongInt     //  вставляемое значение
    );
var i: LongInt;
begin
    CheckArrItemIndex(0, Length(Arr) - 1, Index);
    SetLength(Arr, Length(Arr) + 1);
    for i := Length(Arr) - 2 downto Index do Arr[i + 1] := Arr[i];
    Arr[Index] := Item;
end;

procedure AddItemLongArr(
    var Arr: TLongArray;
    const Item: LongInt
    );
begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Item;
end;


end.
