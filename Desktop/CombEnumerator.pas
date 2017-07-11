//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit CombEnumerator;

{$MODE Delphi}

interface

uses SysUtils;

type
    TCombEnumerator = class
        //  klass, realizuyuschiy algoritm perebora vseh
        //  vozmozhnyh znacheniy zadannyh diskretnyh velichin;
        //  !!! velichina dolzhna imet' ne menee odnogo vozmozhnogo znacheniya !!!
    protected
        NumbersOfValues: array of LongInt;
            //  massiv, soderzhaschiy chisla, kazhdoe iz kotoryh predstavlyaet soboy
            //  chislo vozmozhnyh znacheniy, kotoroe mozhet prinimat' nekotoraya
            //  diskretnaya velichina; sama velichina v dannom sluchae ne
            //  konkretizirovana
        ValuesIndexes: array of LongInt;
            //  indeksy vozmozhnyh znacheniy diskretnyh velichin, opredelennye
            //  v sootvetstvii s nomerom tekuschey zadannoy kombinatsii
        FCurrentComb: LongInt;
        FIsCombDefined: Boolean;
            //  soderzhit True, esli tekuschaya kombinatsiya byla opredelena
            //  posredstvom SetCurrentComb, False v protivnom sluchae

        function GetCombNumber: LongInt;
        function GetCombNumberStartStop(
            //  vozvraschaet chislo kombinatsiy, kotoroe mozhno sostavit' iz
            //  diskretnyh velichin, nahodyaschihsya mezhdu startovym i stopovym
            //  ideksami vklyuchitel'no
            const StartIndex, StopIndex: LongInt): LongInt;
        procedure SetCurrentComb(const ACurrentComb: LongInt); virtual;
        function GetValueIndex(index: LongInt): LongInt;
        function GetValuesNumber: LongInt;

    public
        destructor Destroy; override;
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); virtual;
            //  dobavlyaet v spisok novuyu velichinu - kolichestvo
            //  prinimaemyh znacheniy nekotoroy diskretnoy velichiny
        procedure ClearListOfNumbersOfValues; virtual;

        property CombNumber: LongInt
            //  polnoe chislo kombinatsiy, kotoroe mozhno postroit'
            //  iz nekotorogo nabora diskretnyh velichin
            read GetCombNumber;
        property CurrentComb: LongInt
            //  nomer vybrannoy kombinatsii v dannyy moment
            read FCurrentComb               write SetCurrentComb;
        property ValueIndex[index: LongInt]: LongInt
            //  indeks odnogo iz vozmozhnyh diskretnyh znacheniy velichiny,
            //  zadavaemoy parametrom index; indeks opredelyaetsya v sootvetstvii
            //  s tekuschey vybrannoy kombinatsiey
            read GetValueIndex;
        property ValuesNumber: LongInt
            //  kolichestvo diskretnyh velichin, uchastvuyuschih v obrazovanii kombinatsiy
            read GetValuesNumber;
    end;

    IDiscretValue = interface
        //  interfeys diskretnoy velichiny;
        //  !!! velichina dolzhna imet' ne menee
        //  odnogo vozmozhnogo znacheniya !!!
        function GetNumberOfValues: LongInt;
        //  vozvraschaet chislo vozmozhnyh znacheniy diskretnoy velichiny
        function GetValueIndex: LongInt;
        //  vozvraschaet indeks tekuschego znacheniya diskret velichiny
        procedure SetValueIndex(const AValueIndex: LongInt);
        //  ustanavlivaet indeks tekuschego znacheniya diskret velichiny

        property NumberOfValues: LongInt
            //  kolichestvo diskretnyh znacheniy, kotorye mozhet prinimat' velichina
            read GetNumberOfValues;
        property ValueIndex: LongInt
            //  indeks diskretnogo znacheniya, vybrannogo v dannyy moment
            read GetValueIndex              write SetValueIndex;
    end;

    TCombSelector = class(TCombEnumerator)
        //  hranit ukazateli na interfeysy, podderzhivayuschie vybor
        //  indeksov dopustimyh znacheniy diskretnoy velichiny
    protected
        FValuesList: array of IDiscretValue;
        procedure SetCurrentComb(const ACurrentComb: LongInt); override;

    public
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); override;
        procedure ClearListOfNumbersOfValues; override;
        procedure AddDiscretValue(const AValue: IDiscretValue);
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
    //  algoritm vybora kombinatsii znacheniy po indeksu kombinatsii
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
        FValuesList[i] := nil;  //  dlya vyzova _Release
                                //  ??? budet li _Release vyzyvat'sya korrektno,
                                //  esli prosto ispol'zovat' Finalize
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


