//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit DownhillSimplexContainer;

{$MODE Delphi}

interface

uses
    Classes, DownhillSimplexAlgorithm, AlgorithmContainer, Decisions, SysUtils,
    SimpMath, CombEnumerator, CBRCComponent, Tools, MyExceptions;

const
    //  konstanty vozmozhnyh faz raboty s parametrami v TDownhillRealParameters
    PH_CREATING    = 0;     //  faza ozhidaniya vyzova f-i CreateParameters
    PH_WORKING     = 1;     //  faza raboty

type
    TVariableParameter = record
        Value: Double;
        Limited: Boolean;
            //  priznak ogranicheniya vozmozhnyh znacheniy parametra
        MaxLimit, MinLimit: Double;
            //  maksimal'no i minimal'no vozmozhnye znacheniya parametra;
            //  ispol'zuyutya pri uslovii, chto ogranicheniya zadeystvovany
    end;

    IDownhillSimplexParameters = interface(IDiscretValue)
        //  interfeys, predostavlyayuschiy parametry dlya raboty konteynera algoritma;
        //  zdes' svoystva NumberOfValues i ValueIndex, nasledovannye ot IDiscretValue
        //  indeksiruyut diskretnye urovni, zavisyaschie ot parametrov, kotorye mozhet
        //  prinimat' velichina
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        property ParametersNumber: LongInt
            read GetParametersNumber;
            //  chislo parametrov dlya optimizatsii
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter           write SetParameter;
    end;

    IDownhillRealParameters = interface(IDownhillSimplexParameters)
        //  realizuet dopolnitel'nye funktsii raboty s parametrami, esli eto neobhodimo
        procedure CreateParameters;
            //  metod, prednaznachennyy dlya pervonachal'nogo sozdaniya spiska parmetrov
        procedure ParametersUpdated;
            //  cherez etod metod klassu, predostavlyayuschemu interfeys soobschaetsya ob
            //  okonchanii tsikla manipulyatsiy s parametrami
    end;

    IOptimizedFunction = interface
        //  interfeys predostavlyayuschiy znachenie optimiziruemoy funktsii
        function GetOptimizedFunction: Double;
    end;

    IUpdatingResults = interface
        //  interfeys predostavlyayuschiy vozmozhnost' vyvoda rezul'tatov
        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdatingResults(Sender: TComponent);
    end;

    EDownhillRealParameters = class(Exception);

    TDownhillRealParameters = class(TCBRCComponent, IDownhillRealParameters)
        //  komponent - nositel' interfeysa
    protected
        FParameters: Pointer;           //  kazatel' na massiv parametrov
        FParametersNumber: LongInt;     //  real'noe chislo parametrov v massive
        PhaseParameters: Byte;          //  tekuschaya faza raboty s parametrami

        procedure CreateParameters; virtual;
        procedure FreeParameters;
        procedure FillParameters; virtual; abstract;
            //  generiruet parametry i zapolnyaet imi massiv
        procedure ParametersUpdated; virtual; abstract;
            //  preobrazuet parametry iz massiva v
            //  komponenty vektorov magnitnyh momentov

        function GetActualParametersNumber: LongInt; virtual; abstract;
            //  vozvraschaet chislo parametrov poluchennoe
            //  putem neposredstvennogo perescheta
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        function GetNumberOfValues: LongInt; virtual; abstract;
        function GetValueIndex: LongInt; virtual; abstract;
        procedure SetValueIndex(const AValueIndex: LongInt); virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property ParametersNumber: LongInt  read GetParametersNumber;
            //  chislo parametrov dlya optimizatsii; ot IDownhillSimplexParameters
        property Parameter[index: LongInt]: TVariableParameter
            //  ot IDownhillSimplexParameters
                read GetParameter           write SetParameter;

        property NumberOfValues: LongInt    read GetNumberOfValues;
            //  kolichestvo diskretnyh znacheniy, kotorye mozhet prinimat' velichina
        property ValueIndex: LongInt
            //  ot IDiscretValue
            //  indeks diskretnogo znacheniya, vybrannogo v dannyy moment
                read GetValueIndex          write SetValueIndex;
    end;

    EDownhillSimplexContainer = class(Exception);

    TDownhillSimplexContainer = class(TAlgorithmContainer, IDownhillSimplexServer)
        //  rol' konteynera svoditsya k obsluzhivaniyu zaprosov algoritma;
        //  konteyner sledit za popadaniem znacheniy parametrov v
        //  zadannye intervaly (primenyaet tsiklicheskie granichnye usloviya)
        //  !!! etot konteyner bol'she ne sozdaet potok !!!
    protected
        //  massiv interfeysov, kotorye ispol'zuyutsya
        //  dlya polucheniya parametrov
        FParametersInterfaces : array of IDownhillRealParameters;
        FIOptimizedFunction: IOptimizedFunction;
        FIUpdatingResults: IUpdatingResults;

        CombSelector: TCombSelector;

        FFinalTolerance: Double;
        FRestartDisabled: Boolean;
        FExitDerivative: Double;

        EOC: Boolean;
        message: string;
        //  podgotovka okruzheniya i zapusk algoritma
        procedure Running; override;
        procedure RunningFinished; override;

        //  funktsiya bez parametrov, potomu chto peredavalas' v Synchronize
        procedure ShowMessage;
        //  funktsiya bez parametrov, potomu chto peredavalas' v Synchronize
        procedure UpdateMainForm;
        
        procedure FillParameters(Decision: TFloatDecision); virtual;
        procedure CreateAlgorithm; override;
        procedure CreateParameters;
        procedure DestroyAlgorithm; override;

        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double; virtual;
            //  vozvraschaet nachal'nuyu harakteristicheskuyu
            //  dlinu dlya kazhdogo parametra
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision); virtual;
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision); virtual;
            //  rasschityvaet znachenie funktsii dlya parametrov dannogo resheniya
        procedure UpdateResults(Sender: TComponent;
            Decision: TFloatDecision); virtual;
        //  interfeysnyy metod
        function EndOfCalculation(Sender: TComponent): Boolean;
            //  vozvraschaet priznak neobhodimosti zaversheniya rascheta

        function GetIUpdatingResults: IUpdatingResults;
        function GetIOptimizedFunction: IOptimizedFunction;

        function GetIDSPsNumber: LongInt;
        function GetIDSP(index: LongInt): IDownhillRealParameters;
        property IDSPsNumber: LongInt read GetIDSPsNumber;
        property IDSP[index: LongInt]: IDownhillRealParameters read GetIDSP;

        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        property ParametersNumber: LongInt  read GetParametersNumber;
            //  polnoe chislo parametrov dlya optimizatsii
        property Parameter[index: LongInt]: TVariableParameter
                read GetParameter           write SetParameter;

    public
        TotalMinimum: Double;           //  eta peremennaya nuzhna, chtoby
                                        //  opredelyat' obschiy minimum sredi
                                        //  vseh tsiklov optimizatsii
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        
        procedure StopAlgorithm; override;

        procedure ClearListOfIDSPs;
        procedure AddIDSPToList(const IDSP_: IDownhillRealParameters);

        property OptimizedFunction: IOptimizedFunction
                read GetIOptimizedFunction  write FIOptimizedFunction;
        property UpdatingResults: IUpdatingResults
                read GetIUpdatingResults    write FIUpdatingResults;

            //  konteyner dolzhen hranit' kopii etih svoystv, potomu chto v techenii
            //  protsessa rascheta ob'ekt - algoritm mozhet sozdavat'sya neskol'ko raz
        property FinalTolerance: Double
                read FFinalTolerance        write FFinalTolerance;
        property RestartDisabled: Boolean
                read FRestartDisabled       write FRestartDisabled;
        property ExitDerivative: Double
            //  esli izmenenie naibolee optimal'nogo znacheniya za tsikl
            //  men'she zadannogo, to vyhod
                read FExitDerivative        write FExitDerivative;
    end;

implementation

type
    TParametersArray = array[0..MaxInt div SizeOf(TVariableParameter) - 1] of
        TVariableParameter;         //  tip dlya preobrazovaniya ukazatelya
                                    //  na massiv parametrov

function TDownhillSimplexContainer.GetInitParamLength(
    Sender: TComponent; ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := 0.1;
    //  ??? vynesti v zapis' dlya parametra i sdelat' poisk
    //  sootvetstvuyuschego parametra
end;

procedure TDownhillSimplexContainer.FillStartDecision(Sender: TComponent;
    StartDecision: TFloatDecision);
var i: LongInt;
    TempParamsNumber: LongInt;      //  optimizatsiya
    TempParameter: TVariableParameter;
begin
    TempParamsNumber := ParametersNumber;
    StartDecision.ParametersNumber := TempParamsNumber;
    for i := 0 to TempParamsNumber - 1 do begin
        TempParameter := Parameter[i];
        with TempParameter do
            if Limited and not IsValueIntoInterval(MinLimit, MaxLimit, Value) then
                //  konechno, parametr mozhno bylo by ispravit' i zdes', no vse zhe
                //  eto zadacha klienta - predostavit' pravil'nye nachal'nye parametry
                raise EDownhillSimplexContainer.Create(
                    'Parameter value is not into the interval...')
            else StartDecision.Parameters[i] := Value;
    end;{for i := 0 to TempParametersNumber - 1 do...}
end;

procedure TDownhillSimplexContainer.FillParameters(Decision: TFloatDecision);
var i: LongInt;
    TempParamsNumber: LongInt;      //  optimizatsiya
    TempParameter: TVariableParameter;
    TempIDSP: IDownhillRealParameters;
begin
    TempParamsNumber := ParametersNumber;
    for i := 0 to TempParamsNumber - 1 do
    begin
        TempParameter := Parameter[i];
        //  kopiruetsya novoe znachenie parametra
        TempParameter.Value := Decision.Parameters[i];
        with TempParameter do
            if Limited then PutValueIntoInterval(MinLimit, MaxLimit, Value);
        Parameter[i] := TempParameter;
    end;

    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempIDSP.ParametersUpdated;
    end;
end;

procedure TDownhillSimplexContainer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
begin
    FillParameters(Decision);
    Decision.Evaluation := OptimizedFunction.GetOptimizedFunction;
end;

procedure TDownhillSimplexContainer.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    EvaluateDecision(Sender, Decision);
    //  pereschet resheniya zdes' neobhodim, tak kak v dannom
    //  algoritme luchshiy rezul'tat ne obyazatel'no posledniy
    //  (posle restarta)
    //??? proverit', nuzhno li pereschityvat' - Evaluation d.
    //  hranit' znachenie
    if Decision.Evaluation < TotalMinimum then
    begin
        TotalMinimum := Decision.Evaluation;
        UpdateMainForm;
    end;
end;

function TDownhillSimplexContainer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := EOC;
end;

constructor TDownhillSimplexContainer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    CombSelector := TCombSelector.Create;
end;

destructor TDownhillSimplexContainer.Destroy;
begin
    StopAlgorithm;
    DestroyAlgorithm;

    ClearListOfIDSPs;
    UtilizeObject(CombSelector);
    inherited Destroy;
end;

procedure TDownhillSimplexContainer.ClearListOfIDSPs;
var i: LongInt;
begin
    for i := 0 to Length(FParametersInterfaces) - 1 do
        FParametersInterfaces[i] := nil;    //  dlya umen'sheniya schetchika ssylok
                                            //  ??? budut li schetchiki ssylok korrektno
                                            //  umen'shat'sya, esli prosto vyzyvat' Finalize
    Finalize(FParametersInterfaces);
    CombSelector.ClearDiscretValuesList;
end;

procedure TDownhillSimplexContainer.AddIDSPToList(
    const IDSP_: IDownhillRealParameters);
begin
    SetLength(FParametersInterfaces, Length(FParametersInterfaces) + 1);
    FParametersInterfaces[Length(FParametersInterfaces) - 1] := IDSP_;
    CombSelector.AddDiscretValue(IDSP_);
end;

function TDownhillSimplexContainer.GetIDSPsNumber: LongInt;
begin
    Result := Length(FParametersInterfaces);
end;

function TDownhillSimplexContainer.GetIDSP(
    index: LongInt): IDownhillRealParameters;
begin
    Result := FParametersInterfaces[index];
end;

function TDownhillSimplexContainer.GetIUpdatingResults: IUpdatingResults;
begin
    if Assigned(FIUpdatingResults) then Result := FIUpdatingResults
    else raise EDownhillSimplexContainer.Create(
        'Updating results interface must be assigned...');
end;

function TDownhillSimplexContainer.GetIOptimizedFunction: IOptimizedFunction;
begin
    if Assigned(FIOptimizedFunction) then Result := FIOptimizedFunction
    else raise EDownhillSimplexContainer.Create(
        'Optimized function interface must be assigned...');
end;

procedure TDownhillSimplexContainer.StopAlgorithm;
begin
    EOC := True;
end;

procedure TDownhillSimplexContainer.DestroyAlgorithm;
begin
    UtilizeObject(Algorithm);
end;

procedure TDownhillSimplexContainer.UpdateMainForm;
begin
    UpdatingResults.UpdatingResults(Self);
end;

procedure TDownhillSimplexContainer.ShowMessage;
begin
    UpdatingResults.ShowMessage(Self, message);
end;

procedure TDownhillSimplexContainer.RunningFinished;
begin
    message := 'Calculation done...';
    ShowMessage;
end;

procedure TDownhillSimplexContainer.CreateAlgorithm;
begin
    UtilizeObject(Algorithm);
    Algorithm := TDownhillSimplexAlgorithm.Create(nil);
    //    Algorithm := TDownhillSimplexSAAlgorithm.Create(nil);
    with Algorithm as TDownhillSimplexAlgorithm do
    //    with Algorithm as TDownhillSimplexSAAlgorithm do
    begin
        DownhillSimplexServer := Self;
        FinalTolerance := Self.FinalTolerance;
        RestartDisabled := Self.RestartDisabled;
        ExitDerivative := Self.ExitDerivative;
    //        Temperature := 1;     //  dlya TDownhillSimplexSAAlgorithm
    end;
end;

procedure TDownhillSimplexContainer.Running;
var i: LongInt;
begin
    TotalMinimum := OptimizedFunction.GetOptimizedFunction;
    UpdatingResults.ResetCurJobProgress(Self);
    UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, 0);
    for i := 0 to CombSelector.CombNumber - 1 do
    begin
        if EOC then Exit;
        CombSelector.CurrentComb := i;
        CreateParameters;
        //  sozdayutsya parametry dlya novoy kombinatsii
        if ParametersNumber <> 0 then
        begin
            CreateAlgorithm;
            Algorithm.AlgorithmRealization;
        end else
        begin
            message := 'List of parameters is empty for combination ' + IntToStr(i);
            ShowMessage;
        end;
        UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, i + 1);
    end;
end;

function TDownhillSimplexContainer.GetParametersNumber: LongInt;
var i: LongInt;
begin
    Result := 0;
    for i := 0 to IDSPsNumber - 1 do
        Result := Result + IDSP[i].ParametersNumber;
end;

procedure TDownhillSimplexContainer.CreateParameters;
var i: LongInt;
begin
    for i := 0 to IDSPsNumber - 1 do IDSP[i].CreateParameters;
end;

function TDownhillSimplexContainer.GetParameter(
    index: LongInt): TVariableParameter;
var i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
        //  poisk interfeysa v parametry kotorogo popadaet indeks
        if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            Result := TempIDSP.Parameter[index - ParamSum];
            Exit;
        end else ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillSimplexContainer.SetParameter(
    index: LongInt; AParameter: TVariableParameter);
var i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;   //  optimizatsiya
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
        //  poisk interfeysa v parametry kotorogo popadaet indeks
        if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            TempIDSP.Parameter[index - ParamSum] := AParameter;
            Exit;
        end else ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillRealParameters.CreateParameters;
var ActParNum: LongInt;
begin
    FreeParameters;
    ActParNum := GetActualParametersNumber;
    if ActParNum <> 0 then
    begin
        GetMem(FParameters, ActParNum * SizeOf(TVariableParameter));
        FParametersNumber := ActParNum;
        PhaseParameters := PH_WORKING;
        //  faza dolzhna byt' pravil'no ustanovlena pered FillParameters
        FillParameters;
    end else
    begin
        FParameters := nil;
        FParametersNumber := 0;
        PhaseParameters := PH_WORKING;
    end;
end;

procedure TDownhillRealParameters.FreeParameters;
begin
    if PhaseParameters = PH_WORKING then
        if Assigned(FParameters) then
            FreeMem(FParameters(*, FParametersNumber * SizeOf(TVariableParameter)*));
end;

function TDownhillRealParameters.GetParameter(
    index: LongInt): TVariableParameter;
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else Result := TParametersArray(FParameters^)[index];
end;

procedure TDownhillRealParameters.SetParameter(
    index: LongInt; AParameter: TVariableParameter);
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else TParametersArray(FParameters^)[index] := AParameter;
end;

function TDownhillRealParameters.GetParametersNumber: LongInt;
begin
    case PhaseParameters of
        PH_CREATING : raise EDownhillRealParameters.Create(
            'Parameters must be created...');
        PH_WORKING : Result := FParametersNumber;
        else raise EDownhillRealParameters.Create(
            'Invalid phase number...');
    end;
end;

constructor TDownhillRealParameters.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    PhaseParameters := PH_CREATING;
end;

destructor TDownhillRealParameters.Destroy;
begin
    FreeParameters;
    inherited Destroy;
end;

initialization
end.



