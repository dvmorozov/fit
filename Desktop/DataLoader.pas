{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used in data loading.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit DataLoader;

{$MODE Delphi}

interface

uses Classes, SysUtils, SimpMath, SelfCopied;

type
    EFileNotExists = class(Exception);
    EInvalidFileFormat = class(Exception);
    EWavelengthIsNotSpecified = class(Exception);

	{ Generic point set. }
    TPointsSet = class(TSelfCopiedComponent)
    protected
        FPoints: TwoDimArray;
        
        function GetPointsCount: LongInt;
        function GetPointXCoord(index: LongInt): Double; virtual;
        procedure SetPointXCoord(index: LongInt; Value: Double); virtual;
        function GetPointYCoord(index: LongInt): Double; virtual;
        procedure SetPointYCoord(index: LongInt; Value: Double); virtual;
        function GetMaxXCoord: Double;
        function GetMaxYCoord: Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AddNewPoint(XValue, YValue: Double);
        procedure ReplacePoint(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure DeletePoint(XValue: Double);
        procedure Clear;
        procedure Sort; virtual;
		{ Returns index of point with given X, -1 if point not found. }
        function IndexOfValueX(XValue: Double): LongInt;
		{ Returns index of point having X closest to the given value. }
        function IndexOfNearestToX(XValue: Double): LongInt;

        property PointsCount: LongInt read GetPointsCount;
        property Points: TwoDimArray read FPoints;
        property PointXCoord[index: LongInt]: Double
            read GetPointXCoord write SetPointXCoord;
        property PointYCoord[index: LongInt]: Double
            read GetPointYCoord write SetPointYCoord;
        property MaxXCoord: Double read GetMaxXCoord;
        property MaxYCoord: Double read GetMaxYCoord;
    end;

	{ Implements point set of experimental neutronogram. It's assumed that
      point coordinates are expressed in 2 * Theta. }
    TNeutronPointsSet = class(TPointsSet)
    protected
        FLambda: Double;
        
        function GetPointIntensity(index: LongInt): Double;
        procedure SetPointIntensity(index: LongInt; Value: Double);
        function GetPointT(index: LongInt): Double;
        function GetPoint2T(index: LongInt): Double;
        function GetPointSinTL(index: LongInt): Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        procedure CopyPointsFrom(const Points: TPointsSet);
        
        constructor Create(AOwner: TComponent); override;
        constructor CreateFromPoints(
            AOwner: TComponent; const Points: TPointsSet);
        
        property PointIntensity[index: LongInt]: Double
            read GetPointIntensity write SetPointIntensity;
        property Lambda: Double read FLambda write FLambda;
        property PointT[index: LongInt]: Double read GetPointT;
        property Point2T[index: LongInt]: Double read GetPoint2T;
        property PointSinTL[index: LongInt]: Double read GetPointSinTL;
    end;
  
	{ Point set with title. TODO: must implement functionality of argument recalculation. }
    TTitlePointsSet = class(TNeutronPointsSet)
    public
		{ Title which is displayed in chart legend. }
        Title: string;
        
        procedure CopyParameters(const Dest: TObject); override;
        constructor CreateFromPoints(
            AOwner: TComponent; const Points: TPointsSet);
    end;
    
const ZeroCurveAmplitude: Double = 1;

type
    TParameterType = (
		{ Created by user and non-variable. Such parameters are variated together for all 
		instances in the given interval if it isn't disabled by special flag. }
        Shared,
		{ Created by user and variable. }
        Variable,
		{ Created by the application. }
        Calculated,
		{ Argument of expression. Always variable. }
        Argument,
		{ Non-variable parameter describing instance position. }
        InvariablePosition,
		{ Variable parameter describing instance position. }
        VariablePosition
        );

    TSpecialCurveParameter = class(TCollectionItem)
    private
        FName: string;
        FValue: Double;
        FType: TParameterType;
        FVariationDisabled: Boolean;
        FVariationStep: Double;

        FSavedValue: Double;

        function GetValue_: string;
        procedure SetValue_(AValue: string);

    public
        constructor Create(Collection: TCollection); override;
        procedure CopyTo(const Dest: TSpecialCurveParameter);

        property SavedValue: Double read FSavedValue write FSavedValue;
        property Value: Double read FValue write FValue;
        property VariationDisabled: Boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: Double
            read FVariationStep write FVariationStep;

    published
		{ Published for XML-serialization. }
        property Name: string read FName write FName;
		{ String because some problem with XML-serialization as Double. }
        property Value_: string read GetValue_ write SetValue_;
        property Type_: TParameterType read FType write FType;
    end;

	{ Generic type of instance parameter container. }
    Curve_parameters = class(TSelfCopiedComponent)
    protected
        FParams: TCollection;

    public
		{ Initial parameters hash. Should be used for copying optimization. }
        SavedInitHash: Cardinal;
        
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CopyParameters(const Dest: TObject); override;

    published
        { Published for XML-serialization. }
        property Params: TCollection read FParams write FParams;
    end;

	{ Generic container for point set of all calcuated curves. TODO: must
      inherit Lambda from TNeutronPointsSet to adjust calculated and experimental
      curves on chart. }
    TCurvePointsSet = class(TTitlePointsSet)
    protected
		{ List of curve parameters. }
        FParams: Curve_parameters;
		{ List of variable parameters. }
        Links: TList;
		{ Parameters with predefined semantics have constraints, which
          can be associated with curve points. Attributes store pointers
          to parameters with predefined semantics. Parameters are created
          in descendant constructors. }
        AmplitudeP: TSpecialCurveParameter;
        PositionP: TSpecialCurveParameter;
        SigmaP: TSpecialCurveParameter;
        ArgP: TSpecialCurveParameter;

        Fx0IsSet: Boolean;
		{ X0 variation boundaries. }
        Fx0Low, Fx0High: Double;

        //  predostavlyayut dostup k var'iruemym parametram
		{ Returns variable parameter by through index. }
        function GetParam(Index: LongInt): Double; virtual;
		{ Sets value of variable parameter. }
        procedure SetParam(Index: LongInt; Value: Double); virtual;
		{ Returns total number of variable parameters. }
        function GetParamCount: LongInt;
        
		{ Returns value of parameter with given name. }
        function GetParamByName(Name: string): Double; virtual;
        procedure SetParamByName(Name: string; Value: Double); virtual;
        
		{ Initializes pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); virtual;
		{ Initializes indexes of variable parameters with predefined semantics. }
        procedure SetSpecParamVarIndex(
            P: TSpecialCurveParameter; Index: LongInt); virtual;

    protected
        Modified: Boolean;

    protected
        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); virtual; abstract;
        //  umnozhaet tochki krivoy na zadannyy koeffitsient
        procedure ScaleCurve(const Factor: Double);
        //  vypolnyaet pervonachal'noe zapolnenie spiska
        //  var'iruemyh parametrov i ustanovku AmplIndex, PosIndex,
        //  SigmaIndex
        procedure InitLinks;
        
        //  eti funktsii ne vyzyvayut perescheta krivoy
        //  i primenyayutsya pri initsializatsii (no ust. Modified)
        procedure Setx0(Value: Double);
        procedure SetA(Value: Double);
        procedure SetSigma(Value: Double);
        function Getx0: Double;
        function GetA: Double;
        function GetSigma: Double;

    public
        //  indeksy parametrov s predopredelennoy semantikoy;
        //  !!! indeksy zapolnyayutsya tol'ko v tom sluchae, esli
        //  parametry s trebuemymi imenami yavlyayutsya peremennymi
        //  i yavlyayutsya indeksami v spiske Links; eto neobhodimo
        //  dlya togo, chtoby pri dostupe k var'iruemym parametram
        //  cherez Param[Index] mozhno bylo by zadavat' znacheniya
        //  parametrov s predopredelennoy semantikoy s uchetom
        //  ogranicheniy !!!
        //  !!! dolzhny byt' opublikovany dlya privyazki schaga
        //  optimizatsii !!!
        AmplIndex: LongInt;         //  amplituda krivoy
        PosIndex: LongInt;          //  polozhenie krivoy
        SigmaIndex: LongInt;        //  shirina krivoy

        //  granitsy dlya rascheta R-faktora (granitsy vklyuchayutsya)
        MinX, MaxX: Double;
        //  priznak togo, chto granitsy dlya rascheta R-faktora zadany;
        //  esli False, to R-faktor schitaetsya dlya vseh tochek krivoy
        RangeDefined: Boolean;
        //  hash nachal'nyh znacheniy parametrov
        InitHash: Cardinal;
        //  nachal'noe znachenie x0 - ispol'zuetsya v nekotoryh algoritmah
        Initx0: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        //  prinimaet ob'ekt po znacheniyu i hranit ego
        procedure SetParameters(AParams: Curve_parameters);
        //  luchshe f-ey, a ne svoystvom, potomu chto sv-vo
        //  podrazumevaet hranenie dannyh v ob'ekte, a
        //  zdes' etogo ne trebuetsya
        function GetName: string; virtual; abstract;

        procedure ReCalc(const Intervals: TPointsSet
            //  spisok tochek, sostoyaschiy iz par INDEKSOV v koordinate X
            //  obrazuyuschih intervaly, v kotoryh nuzhno rasschityvat' funktsiyu
            //  (granitsy intervalov vklyuchayutsya); ravenstvo nil oznachaet,
            //  chto rasschityvat' f-yu nuzhno dlya vseh tochek; indeksy vmesto
            //  koordinat ispol'zuyutsya dlya togo, chtoby izbavit'sya ot poiska
            );
        //  sohranyaet var'iruemye parametry vo vnutrennih peremennyh
        procedure StoreParams;
        //  vosstanavlivaet var'iruemye parametry iz vnutrennih peremennyh
        procedure RestoreParams;
        procedure CopyParameters(const Dest: TObject); override;
        //  proverka nalichiya parametrov s predopredelennoy semantikoy
        function Hasx0: Boolean;
        function HasA: Boolean;
        function HasSigma: Boolean;
        //  !!! predostavlyayut dostup optimizatoru k var'iruemym parametram !!!
        property Param[index: LongInt]: Double read GetParam write SetParam;
        property ParamCount: LongInt read GetParamCount;
        //  predostavlyayut dostup ko vsem parametram
        property ParamByName[Name: string]: Double
            read GetParamByName write SetParamByName;
        //  vozvraschaet po ssylke ob'ekt, soderzhaschiy spisok vseh parametrov
        property Params: Curve_parameters read FParams;
        //  predostavlyayut dostup k parametram s predopredelennoy semantikoy
        //  dlya dostupa spets. algoritmov;
        //  !!! nuzhno ispol'zovat' f-i Has... dlya proverki nalichiya takih
        //  parametrov !!!
        //  izmenenie parametra x0 ogranicheno dvumya sosednimi tochkami
        property x0: Double read Getx0 write Setx0;
        property A: Double read GetA write SetA;
        property Sigma: Double read GetSigma write SetSigma;
    end;
  
    TGaussPointsSet = class(TCurvePointsSet)
    protected
        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;

    public
        constructor Create(AOwner: TComponent); override;
        function GetName: string; override;
    end;

    type ValuePair = class(TObject)
    public
      X: double;
      Y: double;
    end;

    //  nasleduet vse svoystva i sposoby raboty s nimi
    TLorentzPointsSet = class(TGaussPointsSet)
    protected
        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;
        
    public
        function GetName: string; override;
    end;
    
    TPseudoVoigtPointsSet = class(TCurvePointsSet)
    protected
        //  Eta - otnositel'nyi ves gaussiana i lorentziana
        EtaP: TSpecialCurveParameter;
        EtaIndex: LongInt;
        
        procedure SetEta(Value: Double);
        function GetEta: Double;
        
        //  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        //  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
        //  semantikoy
        procedure SetSpecParamVarIndex(
            P: TSpecialCurveParameter; Index: LongInt); override;
        
        //  predostavlyayut dostup k var'iruemym parametram
        function GetParam(Index: LongInt): Double; override;
        procedure SetParam(Index: LongInt; Value: Double); override;
        
        //  predostavlyayut dostup ko vsem parametram
        function GetParamByName(Name: string): Double; override;
        procedure SetParamByName(Name: string; Value: Double); override;

        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;

    public
        constructor Create(AOwner: TComponent); override;
        function GetName: string; override;
        
        function HasEta: Boolean;
        
        property Eta: Double read GetEta write SetEta;
    end;
    
    TAsymPseudoVoigtPointsSet = class(TPseudoVoigtPointsSet)
    protected
        //  izmenenie poluschiriny levoy i pravoy storon krivoy
        DeltaSigmaP: TSpecialCurveParameter;
        DeltaSigmaIndex: LongInt;

        procedure SetDeltaSigma(Value: Double);
        function GetDeltaSigma: Double;

        //  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        //  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
        //  semantikoy
        procedure SetSpecParamVarIndex(
            P: TSpecialCurveParameter; Index: LongInt); override;

        //  predostavlyayut dostup k var'iruemym parametram
        function GetParam(Index: LongInt): Double; override;
        procedure SetParam(Index: LongInt; Value: Double); override;
        
        //  predostavlyayut dostup ko vsem parametram
        function GetParamByName(Name: string): Double; override;
        procedure SetParamByName(Name: string; Value: Double); override;

        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;
    public
        constructor Create(AOwner: TComponent); override;
        function GetName: string; override;

        function HasDeltaSigma: Boolean;

        property DeltaSigma: Double read GetDeltaSigma write SetDeltaSigma;
    end;

    T2BranchesPseudoVoigtPointsSet = class(TCurvePointsSet)
    protected
        SigmaRightP: TSpecialCurveParameter;
        SigmaRightIndex: LongInt;
        
        EtaRightP: TSpecialCurveParameter;
        EtaRightIndex: LongInt;
        
        EtaP: TSpecialCurveParameter;
        EtaIndex: LongInt;

        procedure SetSigmaRight(Value: Double);
        function GetSigmaRight: Double;
        procedure SetEtaRight(Value: Double);
        function GetEtaRight: Double;
        procedure SetEta(Value: Double);
        function GetEta: Double;

        //  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        //  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
        //  semantikoy
        procedure SetSpecParamVarIndex(
            P: TSpecialCurveParameter; Index: LongInt); override;

        //  predostavlyayut dostup k var'iruemym parametram
        function GetParam(Index: LongInt): Double; override;
        procedure SetParam(Index: LongInt; Value: Double); override;
        
        //  predostavlyayut dostup ko vsem parametram
        function GetParamByName(Name: string): Double; override;
        procedure SetParamByName(Name: string; Value: Double); override;

        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;
    public
        constructor Create(AOwner: TComponent); override;
        function GetName: string; override;

        function HasSigmaRight: Boolean;
        function HasEtaRight: Boolean;
        function HasEta: Boolean;

        property SigmaRight: Double read GetSigmaRight write SetSigmaRight;
        property EtaRight: Double read GetEtaRight write SetEtaRight;
        property Eta: Double read GetEta write SetEta;
    end;

    //  klass pozvolyaet pol'zovatelyu zadavat' imya nabora tocheck;
    //  eto imya otlichaet vse krivye dannogo tipa ot krivyh drugih tipov;
    //  semantika imeni ne sovpadaet s semantikoy zagolovka (Title)
    TNamedPointsSet = class(TCurvePointsSet)
    protected
        FName: string;
        
    public
        function GetName: string; override;
        procedure SetName(AName: string);
    end;

type
    PDouble = ^Double;
    (*
    function ParseAndCalcExpression(Expr: LPCSTR; ParamList: LPCSTR;
        Result: PDouble): LongInt; cdecl;
        external 'MathExpr' name 'ParseAndCalcExpression';
    function GetSymbols: LPCSTR; cdecl;
        external 'MathExpr' name 'GetSymbols';
    procedure FreeSymbols(Symbols: LPCSTR); cdecl;
        external 'MathExpr' name 'FreeSymbols';
    bez ispol'zovaniya...
    *)

type
    //  konteyner dlya tochek pol'zovatel'skoy krivoy, zadannoy v vide vyrazheniya
    TSpecialPointsSet = class(TCurvePointsSet)
    protected
        FExpression: string;        //  vyrazhenie v obschem vide
        FName: string;

        //  vypolnyaet polnyy pereschet tochek funktsii
        procedure DoCalc(const Intervals: TPointsSet); override;
        //  vypolnyaet raschet znacheniya tochki dlya tekuschego znacheniya argumenta
        function CalcValue(ArgValue: Double): Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        function GetName: string; override;
        procedure SetName(AName: string);
        
        property Expression: string read FExpression write FExpression;
    end;
    
    TDataLoader = class(TComponent)
    //  bazovyy klass dlya sozdaniya serii zagruzchikov dlya
    //  kazhdogo tipa zksperimental'nyh faylov dannyh neytronogramm
    protected
        PointsSet: TPointsSet;
        FFileName: string;

        procedure LoadDataSetActually; virtual; abstract;
        procedure CreatePointsSet;

    public
        procedure LoadDataSet(AFileName: string);
        //  !!! PointsSet v Reload udalyat' nel'zya,
        //  t.k. na nego m.b. vneshnie ssylki !!!
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet; virtual;
        destructor Destroy; override;
    end;

    TDATFileLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
    end;


const MIN_VALUE: Double = -1e100;   //  minimal'no vozmozhnoe chislo
      MAX_VALUE: Double =  1e100;

function MyStrToFloat(Str: string): Double;

implementation

uses Main;

function MyStrToFloat(Str: string): Double;
var i: LongInt;
begin
    for i := 1 to Length(Str) do
        if (Str[i] = '.') or (Str[i] = ',') then
            Str[i] := DecimalSeparator;
    Result := StrToFloat(Str);
end;

{============================== TDataLoader ===================================}
function TDataLoader.GetPointsSetCopy: TTitlePointsSet;
begin
    Assert(Assigned(PointsSet));
    Result := TTitlePointsSet.CreateFromPoints(nil, PointsSet);
end;

destructor TDataLoader.Destroy;
begin
    PointsSet.Free;
    inherited Destroy;
end;

procedure TDataLoader.CreatePointsSet;
begin
    if Assigned(PointsSet) then PointsSet.Clear
    else PointsSet := TNeutronPointsSet.Create(nil);
end;

procedure TDataLoader.LoadDataSet(AFileName: string);
begin
    //  nedopustimaya oshibka
    Assert(FileExists(AFileName));

    CreatePointsSet;
    FFileName := AFileName;
    LoadDataSetActually;
end;

procedure TDataLoader.Reload;
begin
    Assert(FFileName <> '');
    Assert(Assigned(PointsSet));

    //  zdes' otsutstvie fayla - dopustimaya oshibka -
    //  fayl m.b. stert posle pervoy zagruzki
    if not FileExists(FFileName) then
        raise EFileNotExists.Create('File ' + FFileName +
            ' does not exists.');

    PointsSet.Clear;
    LoadDataSetActually;
end;

{============================== TDATFileLoader ================================}
procedure TDATFileLoader.LoadDataSetActually;
var //F: TextFile;
    Val1, Val2: Double;
    Data: TStringList;
    Str: string;
    i, j: LongInt;
    BegFound: Boolean;
    BegIndex: LongInt;
    Value1Found, Value2Found, FirstDelimiter: Boolean;

label ExtractValue;
begin
    Assert(FFileName <> '');
    Assert(Assigned(PointsSet));
    
    PointsSet.Clear;
    Data := TStringList.Create;
    try
        Data.LoadFromFile(FFileName);
        for i := 0 to Data.Count - 1 do
        begin
            //  pervaya kolonka - X, vtoraya - Y;
            //  razdelitelem kolonok mozhet byt' lyuboy simvol,
            //  krome tsifr, tochki i zapyatoy
            Str := Data.Strings[i] + ' ';   //  dobavlyaetsya terminator dlya
                                            //  algoritma
            BegFound := False;
            Value1Found := False; Value2Found := False;
            FirstDelimiter := False;
            Val1 := 0; Val2 := 0;
            try
                for j := 1 to Length(Str) do
                begin
                    if ((Str[j] >= Chr($30)) and (Str[j] <= Chr($39))) or
                        (Str[j] = '.') or (Str[j] = ',') then
                    begin
                        if not BegFound then
                        begin
                            BegIndex := j;
                            BegFound := True;
                        end
                        else
                        begin
                            if (Str[j] = '.') or (Str[j] = ',') then
                            begin
                                if not FirstDelimiter then
                                    FirstDelimiter := True
                                else goto ExtractValue;
                            end;
                        end;
                    end
                    else
                    begin
ExtractValue:
                        if BegFound then
                        begin
                            //  naydeno znachenie
                            if not Value1Found then
                            begin
                                Val1 := MyStrToFloat(
                                    Copy(Str, BegIndex, j - BegIndex));
                                Value1Found := True;
                            end
                            else
                            begin
                                //  izvlechenie vtorogo znacheniya
                                Val2 := MyStrToFloat(
                                    Copy(Str, BegIndex, j - BegIndex));
                                Value2Found := True;
                                //  ostatok stroki ignoriruetsya
                                Break;
                            end;
                            FirstDelimiter := False;
                            BegFound := False;
                        end;
                        //  ostal'nye propuskayutsya
                    end;
                end;
            except
                //  dopustimaya oshibka vybrasyvaemaya v vide isklyucheniya
                raise EInvalidFileFormat.Create('File ' +
                    FFileName + ' is not valid DAT-file.')
            end;
            if Value2Found then
            begin
                //  zaschita ot dubley; net osnovaniy predpochest'
                //  odno iz znacheniy s odinakovym X drugomu
                if PointsSet.IndexOfValueX(Val1) = -1 then
                    PointsSet.AddNewPoint(Val1, Val2);
            end;
        end;
    finally
        Data.Free;
    end;
    (*
    AssignFile(F, FFileName);
    Reset(F);
    try
        while not Eof(F) do
        begin
            try
                ReadLn(F, Val1, Val2);
            except
                //  dopustimaya oshibka vybrasyvaemaya v vide isklyucheniya
                raise EInvalidFileFormat.Create('File ' +
                    FFileName + ' is not valid DAT-file.')
            end;
            PointsSet.AddNewPoint(Val1, Val2);
        end;
    finally
        CloseFile(F);
    end;
    *)
end;

{============================== TPointsSet =================================}
function TPointsSet.GetPointsCount: LongInt;
begin
    //Assert(Assigned(FPoints));    dopuskaetsya...
    //  dlina daet kolichestvo par - pravil'no!
    Result := Length(FPoints);
end;

function TPointsSet.GetPointXCoord(index: LongInt): Double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][1];
end;

function TPointsSet.GetPointYCoord(index: LongInt): Double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][2];
end;

procedure TPointsSet.SetPointXCoord(index: LongInt; Value: Double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    FPoints[index][1] := Value;
end;

procedure TPointsSet.SetPointYCoord(index: LongInt; Value: Double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    FPoints[index][2] := Value;
end;

function TPointsSet.GetMaxXCoord: Double;
var i: LongInt;
    MaxX: Double;
begin
    MaxX := PointXCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointXCoord[i] > MaxX then MaxX := PointXCoord[i];
    Result := MaxX;
end;

function TPointsSet.GetMaxYCoord: Double;
var i: LongInt;
    MaxY: Double;
begin
    MaxY := PointYCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointYCoord[i] > MaxY then MaxY := PointYCoord[i];
    Result := MaxY;
end;

procedure TPointsSet.CopyParameters(const Dest: TObject);
var i: LongInt;
begin
    inherited;
    TPointsSet(Dest).Clear;
    for i := 0 to PointsCount - 1 do
        TPointsSet(Dest).AddNewPoint(PointXCoord[i], PointYCoord[i]);
end;

procedure TPointsSet.AddNewPoint(XValue,YValue: Double);
begin
    //Assert(Assigned(FPoints));    pervyy raz initsializiruetsya v SetLength
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[PointsCount - 1][1] := XValue;
    FPoints[PointsCount - 1][2] := YValue;
end;

procedure TPointsSet.ReplacePoint(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var i: LongInt;
begin
    //Assert(Assigned(Points)); dopuskaetsya

    // ischem zadannuyu tochku v vybrannom spiske tochek
    for i := 0 to PointsCount - 1 do
    begin
        if (Abs(PrevXValue - PointXCoord[i]) <= TINY) and
           (Abs(PrevYValue - PointYCoord[i]) <= TINY) then
        begin
            PointXCoord[i] := NewXValue;
            PointYCoord[i] := NewYValue;
            Sort;
            Exit;
        end;
        //  uslovie nuzhno proveryat', t.k. v kachestve pred. koord.
        //  m.b. peredany nuli, chto oznachaet, chto delaetsya popytka
        //  dobavleniya novoy tochki
        if Abs(NewXValue - PointXCoord[i]) <= TINY then
        begin
            if Abs(NewYValue - PointYCoord[i]) <= TINY then
                //  tochka prosto povtoryaetsya - dvuh odinakovyh
                //  tochek byt' ne mozhet
                Exit
            else
            begin
                //  tochek s odinakovym X byt' ne mozhet
                PointYCoord[i] := NewYValue;
                Exit;
            end;
        end;
    end;
    // tochka ne naydena - dobavlyaem novuyu
    AddNewPoint(NewXValue, NewYValue);
    Sort;
end;

procedure TPointsSet.Clear;
begin
    FPoints := nil;
end;

procedure TPointsSet.DeletePoint(XValue: Double);
var j, Index: LongInt;
    NewPoints: TwoDimArray;
    Found: Boolean;
begin
    //  poisk tochki v nabore; otsutstvie ne schitat' oschibkoy
    SetLength(NewPoints, PointsCount - 1);
    Found := False;
    try
        Index := 0;
        for j := 0 to PointsCount - 1 do
        begin
            if (Abs(XValue - PointXCoord[j]) <= TINY) and
                (not Found) then Found := True
            else
            begin
                Assert(Index < PointsCount - 1);
                NewPoints[Index][1] := PointXCoord[j];
                NewPoints[Index][2] := PointYCoord[j];
                Inc(Index);
                //  Break delat' nel'zya, potomu chto nezavisimo ot togo,
                //  zapolnen ves' massiv resul'tata, ili net nuzhno
                //  pravil'no ustanovit' Found; Break privodit k
                //  nevozmozhnosti udalit' poslednyuyu tochku...
            end;
        end;
    except
        NewPoints := nil;
        raise;
    end;
    if Found then
    begin
        FPoints := nil;
        FPoints := NewPoints;
    end else NewPoints := nil;
end;

procedure TPointsSet.Sort;
var NewPoints: TwoDimArray;
    i, j: LongInt;
    MinValueX, MaxValueX: Double;
    CurMaxValueX: Double;
    index: LongInt;
begin
    if PointsCount = 0 then Exit;
    SetLength(NewPoints, PointsCount);
    try
        //  poisk indeksa tochki s min. X
        MinValueX := PointXCoord[0]; index := 0;
        for j := 1 to PointsCount - 1 do
        begin
            if PointXCoord[j] < MinValueX then
            begin
                MinValueX := PointXCoord[j];
                index := j;
            end;
        end;
        NewPoints[0][1] := FPoints[index][1];
        NewPoints[0][2] := FPoints[index][2];
        //  poisk maksimal'nogo X
        MaxValueX := PointXCoord[0];
        for j := 1 to PointsCount - 1 do
            if PointXCoord[j] > MaxValueX then MaxValueX := PointXCoord[j];
        //  tsikl po vse ostavshimsya tochkam novogo massiva
        for i := 1 to PointsCount - 1 do
        begin
            CurMaxValueX := MaxValueX;
            index := -1;
            //  nahodim naimen'shuyu koord. X bol'shuyu zadannoy
            for j := 0 to PointsCount - 1 do
            begin
                if (PointXCoord[j] > MinValueX) and
                   (PointXCoord[j] <= CurMaxValueX) then
                begin
                    CurMaxValueX := PointXCoord[j];
                    index := j;
                end;
            end;
            Assert(index <> -1);    //  ne m.b. takogo, poskol'ku ne
                                    //  d.b. tochek s odinakovym X
            NewPoints[i][1] := FPoints[index][1];
            NewPoints[i][2] := FPoints[index][2];
            MinValueX := CurMaxValueX;
        end;
    except
        NewPoints := nil;
        raise;
    end;
    
    FPoints := nil;
    FPoints := NewPoints;
end;

function TPointsSet.IndexOfValueX(XValue: Double): LongInt;
var i: LongInt;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
    begin
        if (Abs(XValue - PointXCoord[i]) <= TINY) then
        begin Result := i; Exit end;
    end;
end;

function TPointsSet.IndexOfNearestToX(XValue: Double): LongInt;
var i: LongInt;
    Min, Cur: Double;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
    begin
        if i = 0 then
        begin
            Min := Abs(XValue - PointXCoord[i]);
            Result := 0;
        end
        else
        begin
            Cur := Abs(XValue - PointXCoord[i]);
            if Cur < Min then
            begin
                Min := Cur;
                Result := i;
            end;
        end;
    end;
end;

constructor TPointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FPoints := nil;
end;

destructor TPointsSet.Destroy;
begin
    FPoints := nil;
    inherited Destroy;
end;

{============================ TTitlePointsSet =================================}
procedure TTitlePointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TTitlePointsSet(Dest).Title := Title;
end;

constructor TTitlePointsSet.CreateFromPoints(
    AOwner: TComponent; const Points: TPointsSet);
begin
    Assert(Assigned(Points));
    inherited Create(AOwner);
    CopyPointsFrom(Points);
end;

{=========================== TNeutronPointsSet ================================}
procedure TNeutronPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TNeutronPointsSet(Dest).Lambda := Lambda;
end;

constructor TNeutronPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
end;

procedure TNeutronPointsSet.CopyPointsFrom(const Points: TPointsSet);
var i: LongInt;
begin
    Assert(Assigned(Points));
    for i := 0 to Points.PointsCount - 1 do
        AddNewPoint(Points.PointXCoord[i], Points.PointYCoord[i]);
end;

constructor TNeutronPointsSet.CreateFromPoints(
    AOwner: TComponent; const Points: TPointsSet);
begin
    Assert(Assigned(Points));
    inherited Create(AOwner);
    CopyPointsFrom(Points);
end;

function TNeutronPointsSet.GetPointT(index: LongInt): Double;
begin
    Result := FPoints[index][1] / 2;
end;

function TNeutronPointsSet.GetPoint2T(index: LongInt): Double;
begin
    Result := FPoints[index][1];
end;

function TNeutronPointsSet.GetPointSinTL(index: LongInt): Double;
begin
    if Lambda <> 0 then
        Result := Sin((FPoints[index][1] * pi) / (2 * 180)) / Lambda
    else raise EWavelengthIsNotSpecified.Create('Wavelength undefined...')
end;

function TNeutronPointsSet.GetPointIntensity(index: LongInt): Double;
begin
    Result := PointYCoord[index];
end;

procedure TNeutronPointsSet.SetPointIntensity(index: LongInt; Value: Double);
begin
    FPoints[index][2] := Value;
end;

{=========================== TCurvePointsSet ==================================}

constructor TCurvePointsSet.Create(AOwner: TComponent);
begin
    inherited;
    FParams := Curve_parameters.Create(nil);
    Modified := True;
    AmplIndex := -1;
    PosIndex := -1;
    SigmaIndex := -1;
end;

destructor TCurvePointsSet.Destroy;
begin
    Links.Free;
    FParams.Free;
    inherited;
end;

procedure TCurvePointsSet.ReCalc(const Intervals: TPointsSet);
begin
    if Modified then
    begin
        DoCalc(Intervals);
        Modified := False;
    end;
end;

procedure TCurvePointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TCurvePointsSet(Dest).MinX := MinX;
    TCurvePointsSet(Dest).MaxX := MaxX;
    TCurvePointsSet(Dest).RangeDefined := RangeDefined;
    TCurvePointsSet(Dest).SetParameters(Curve_parameters(FParams.GetCopy));
    TCurvePointsSet(Dest).InitHash := InitHash;
end;

function TCurvePointsSet.GetParam(Index: LongInt): Double;
var P: TSpecialCurveParameter;
begin
    Assert(index < GetParamCount);
    
    if Index = PosIndex then Result := x0
    else
    if Index = AmplIndex then Result := A
    else
    if Index = SigmaIndex then Result := Sigma
    else
    begin
        P := TSpecialCurveParameter(Links.Items[index]);
        Result := P.Value;
    end;
end;

procedure TCurvePointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = PosIndex then x0 := Value
    else
    if Index = AmplIndex then A := Value
    else
    if Index = SigmaIndex then Sigma := Value
    else
    begin
        P := TSpecialCurveParameter(Links.Items[Index]);
        P.Value := Value;
    end;
end;

function TCurvePointsSet.GetParamByName(Name: string): Double;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    //  snachala proveryayutsya special'nye imena
    if UpperCase(Name) = 'X0' then
    begin
        Result := x0;
        Exit;
    end
    else
    if UpperCase(Name) = 'A' then
    begin
        Result := A;
        Exit;
    end
    else
    if UpperCase(Name) = 'SIGMA' then
    begin
        Result := Sigma;
        Exit;
    end
    else
    begin
        for i := 0 to FParams.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(FParams.Params.Items[i]);
            if UpperCase(P.Name) = UpperCase(Name) then
            begin
                Result := P.Value;
                Exit;
            end;
        end;
    end;
    Assert(False);
end;

procedure TCurvePointsSet.SetParamByName(Name: string; Value: Double);
var i: LongInt;
    P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Modified := True;
    //  snachala proveryayutsya special'nye imena
    if UpperCase(Name) = 'X0' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(x0): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        x0 := Value;
        Exit;
    end
    else
    if UpperCase(Name) = 'A' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(A): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        A := Value;
        Exit;
    end
    else
    if UpperCase(Name) = 'SIGMA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) + ' SetParamByName(Sigma): Value = ' +
            FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        Sigma := Value;
        Exit;
    end
    else
    begin
        for i := 0 to FParams.Params.Count - 1 do
        begin
            P := TSpecialCurveParameter(FParams.Params.Items[i]);
            if UpperCase(P.Name) = UpperCase(Name) then
            begin
{$IFDEF WRITE_PARAMS_LOG}
                LogStr := IntToStr(LongInt(Self)) +
                    ' SetParamByName('+ Name +'): Value = ' + FloatToStr(Value);
                WriteLog(LogStr, Notification_);
{$ENDIF}
                P.Value := Value;
                Exit;
            end;
        end;
    end;
    Assert(False);
end;

function TCurvePointsSet.GetParamCount: LongInt;
begin
    Result := Links.Count;
end;

procedure TCurvePointsSet.Setx0(Value: Double);
var i: LongInt;
    TempDouble: Double;
    Highindex: LongInt;
    Lowindex: LongInt;
    P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(PositionP));
    P := PositionP;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetX0: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    Value := Abs(Value);
    if not Fx0IsSet then
    begin
        //  pervaya ustanovka parametra
        Fx0IsSet := True;
        P.Value := Value;
        Fx0Low := MIN_VALUE;
        Fx0High := MAX_VALUE;
        Highindex := -1;
        Lowindex := -1;
        //  opredelenie granits variatsii parametra
        for i := 0 to PointsCount - 1 do
        begin
            TempDouble := PointXCoord[i];
            if TempDouble < P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0Low - P.Value) then
                    Fx0Low := TempDouble;
                Lowindex := i;
            end;
            if TempDouble > P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0High - P.Value) then
                    Fx0High := TempDouble;
                Highindex := i;
            end;
        end;
        if Lowindex = -1 then Fx0Low := P.Value;
        if Highindex = -1 then Fx0High := P.Value;
    end
    else
    begin
        if Value < Fx0Low then begin P.Value := Fx0Low; Exit end;
        if Value > Fx0High then begin P.Value := Fx0High; Exit end;
        P.Value := Value;
    end;
end;

procedure TCurvePointsSet.SetA(Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(AmplitudeP));
    Modified := True;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetA: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    AmplitudeP.Value := Abs(Value);
end;

procedure TCurvePointsSet.SetSigma(Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Assert(Assigned(SigmaP));
    Modified := True;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetSigma: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    SigmaP.Value := Abs(Value);
    if SigmaP.Value = 0 then SigmaP.Value := TINY;
end;

function TCurvePointsSet.Hasx0: Boolean;
begin
    if Assigned(PositionP) then Result := True else Result := False;
end;

function TCurvePointsSet.HasA: Boolean;
begin
    if Assigned(AmplitudeP) then Result := True else Result := False;
end;

function TCurvePointsSet.HasSigma: Boolean;
begin
    if Assigned(SigmaP) then Result := True else Result := False;
end;

function TCurvePointsSet.Getx0: Double;
begin
    Assert(Assigned(PositionP));
    Result := PositionP.Value;
end;

function TCurvePointsSet.GetA: Double;
begin
    Assert(Assigned(AmplitudeP));
    Result := AmplitudeP.Value;
end;

function TCurvePointsSet.GetSigma: Double;
begin
    Assert(Assigned(SigmaP));
    Result := SigmaP.Value;
end;

procedure TCurvePointsSet.ScaleCurve(const Factor: Double);
var i: LongInt;
begin
    for i := 0 to PointsCount - 1 do
        Points[i][2] := Points[i][2] * Factor;
end;

procedure TCurvePointsSet.InitLinks;
var i, Index: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    Links.Free;
    Links := TList.Create;

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);

        if (P.Type_ = Variable) or
           (P.Type_ = VariablePosition) then
        begin
            //  zdes' tol'ko var'iruemye
            Index := Links.Add(P);
            //  opredelyayutsya indeksy parametrov s
            //  predopredelennoy semantikoy v spiske
            //  var'iruemyh parametrov
            SetSpecParamVarIndex(P, Index);
        end;
        if P.Type_ = Argument then ArgP := P;

        //  ustanavlivayutsya ukazateli na parametry
        //  s predopredelennoy semantikoy
        SetSpecParamPtr(P);
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TCurvePointsSet.SetSpecParamPtr(P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'SIGMA' then SigmaP := P;
    if UpperCase(P.Name) = 'A' then AmplitudeP := P;
    if (P.Type_ = VariablePosition) or
       (P.Type_ = InvariablePosition) then PositionP := P;
end;
//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TCurvePointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'SIGMA' then SigmaIndex := Index;
    if UpperCase(P.Name) = 'A' then AmplIndex := Index;
    if P.Type_ = VariablePosition then PosIndex := Index;
end;

procedure TCurvePointsSet.StoreParams;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        P.SavedValue := P.Value;
    end;
end;

procedure TCurvePointsSet.RestoreParams;
var i: LongInt;
    P: TSpecialCurveParameter;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));

    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        P.Value := P.SavedValue;
    end;
    Modified := True;
end;

procedure TCurvePointsSet.SetParameters(AParams: Curve_parameters);
begin
    Assert(Assigned(AParams));

    FParams.Free; FParams := AParams;
    InitLinks;
end;

{=========================== TGaussPointsSet ==================================}

constructor TGaussPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'A'; P.Value := 0; P.Type_ := Variable;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'x0'; P.Value := 0;
    P.Type_ := VariablePosition;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigma'; P.Value := 0.25;
    P.Type_ := Variable;       //  ne var'iruetsya otdel'no,
                               //  prinimaet odno znachenie dlya vseh
                               //  krivyh podzadachi (intervala)
    InitLinks;
end;

function TGaussPointsSet.GetName: string;
begin
    Result := 'Gaussian';
end;

procedure TGaussPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
    //x0Index, LastRightIndex: LongInt;
    //Zero: Boolean;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            (*  takoy variant ne daet uskoreniya, a kazhetsya rabotaet
                dazhe chut' medlennee - vse s'edaet poisk indeksov ?!
            for j := IndexOfValueX(Intervals.GetPointXCoord(i shl 1)) to
                IndexOfValueX(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
            *)
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
        end;
    end
    else
    begin
        //  snachala nuzhno obnulit' tochki, chtoby vse tochki, znachenie
        //  funktsii v kotoryh < ZeroCurveAmplitude byli bez musora
        for j := 0 to PointsCount - 1 do PointYCoord[j] := 0;

        //  schitaem optimal'no, ispol'zuya porog nulya i simmetriyu
        (*  optimal'nyi schet rabotaet tol'ko kogda x0 ne var'iruetsya,
            t.e. krivaya simmetrichna otnositel'no izmeneniya indeksa
        x0Index := IndexOfValueX(x0);
        Assert(x0Index <> -1);
        Points[x0Index][2] := GaussPoint(A, Sigma, x0, x0);
        
        Zero := False; LastRightIndex := x0Index;

        for j := x0Index - 1 downto 0 do
        begin
            Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
            if (x0Index shl 1) - j <= PointsCount - 1 then
            begin
                Points[(x0Index shl 1) - j][2] := Points[j][2];
                LastRightIndex := (x0Index shl 1) - j;
            end;
            //  vsegda polozhitelen
            if Points[j][2] < ZeroCurveAmplitude then
            begin
                Zero := True;
                Break;
            end;
        end;

        if not Zero then
            //  0 esche ne dostignut
            for j := LastRightIndex + 1 to PointsCount - 1 do
            begin
                Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
                //  vsegda polozhitelen
                if Points[j][2] < ZeroCurveAmplitude then Break;
            end;
        *)
        //  polnyy pereschet bez optimizatsii
        Gauss(Points, A, Sigma, x0);
    end;
end;

function ComparePairs(Item1, Item2: Pointer): Integer;
begin
    if ValuePair(Item1).X < ValuePair(Item2).X then Result := -1
    else
      if ValuePair(Item1).X > ValuePair(Item2).X then Result := 1
      else Result := 0;
end;

{======================== TPseudoVoigtPointsSet ===============================}

procedure TPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := PseudoVoigtPoint(A, Sigma, Eta, x0, Points[j][1]);
        end;
    end
    else
    begin
        PseudoVoigt(Points, A, Sigma, Eta, x0);
    end;
end;

constructor TPseudoVoigtPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    EtaIndex := -1;
    
    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'A'; P.Value := 0; P.Type_ := Variable;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'x0'; P.Value := 0;
    P.Type_ := VariablePosition;
    //P.Type_ := InvariablePosition;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigma'; P.Value := 0.25;
    //P.Type_ := Variable       //  ne var'iruetsya otdel'no,
                                //  prinimaet odno znachenie dlya vseh
                                //  krivyh podzadachi
    P.Type_ := Shared;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'eta'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna
    InitLinks;
end;

procedure TPseudoVoigtPointsSet.SetEta(Value: Double);
begin
    Assert(Assigned(EtaP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaP.Value := Abs(Value);
    if EtaP.Value > 1 then EtaP.Value := 1;
end;

function TPseudoVoigtPointsSet.GetEta: Double;
begin
    Assert(Assigned(EtaP));
    Result := EtaP.Value;
end;

function TPseudoVoigtPointsSet.HasEta: Boolean;
begin
    if Assigned(EtaP) then Result := True else Result := False;
end;

function TPseudoVoigtPointsSet.GetName: string;
begin
    Result := 'Pseudo-Voigt';
end;

function TPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
var P: TSpecialCurveParameter;
begin
    if UpperCase(Name) = 'ETA' then Result := Eta
    else Result := inherited;
end;

procedure TPseudoVoigtPointsSet.SetParamByName(Name: string; Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Modified := True;
    
    if UpperCase(Name) = 'ETA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := ' SetParamByName(Eta): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        Eta := Value;
    end
    else inherited;
end;

procedure TPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = EtaIndex then
        Eta := Value
    else inherited;
end;

function TPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(index < GetParamCount);

    if Index = EtaIndex then Result := Eta
    else Result := inherited;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then EtaP := P
    else inherited;
end;

//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then EtaIndex := Index
    else inherited;
end;

{=================== T2BranchesPseudoVoigtPointsSet ===========================}

procedure T2BranchesPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := TwoBranchesPseudoVoigtPoint(
                        A, Sigma, Eta, SigmaRight, EtaRight, x0, Points[j][1]);
        end;
    end
    else
    begin
        TwoBranchesPseudoVoigt(Points, A, Sigma, Eta, SigmaRight, EtaRight, x0);
    end;
end;

constructor T2BranchesPseudoVoigtPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    SigmaRightIndex := -1;
    EtaRightIndex := -1;
    EtaIndex := -1;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'A'; P.Value := 0; P.Type_ := Variable;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'x0'; P.Value := 0;
    P.Type_ := VariablePosition;
    //P.Type_ := InvariablePosition;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigma'; P.Value := 0.25;
    P.Type_ := Variable;
    //P.Type_ := Shared;      //  ne var'iruetsya otdel'no,
                                //  prinimaet odno znachenie dlya vseh
                                //  krivyh podzadachi

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'eta'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigmaright'; P.Value := 0.25;
    P.Type_ := Variable;
    //P.Type_ := Shared;      //  ne var'iruetsya otdel'no,
                                //  prinimaet odno znachenie dlya vseh
                                //  krivyh podzadachi

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'etaright'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna
    InitLinks;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetEta(Value: Double);
begin
    Assert(Assigned(EtaP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaP.Value := Abs(Value);
    if EtaP.Value > 1 then EtaP.Value := 1;
end;

function T2BranchesPseudoVoigtPointsSet.GetEta: Double;
begin
    Assert(Assigned(EtaP));
    Result := EtaP.Value;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetEtaRight(Value: Double);
begin
    Assert(Assigned(EtaRightP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaRightP.Value := Abs(Value);
    if EtaRightP.Value > 1 then EtaRightP.Value := 1;
end;

function T2BranchesPseudoVoigtPointsSet.GetEtaRight: Double;
begin
    Assert(Assigned(EtaRightP));
    Result := EtaRightP.Value;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetSigmaRight(Value: Double);
begin
    Assert(Assigned(SigmaRightP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    SigmaRightP.Value := Abs(Value);
    if SigmaRightP.Value = 0 then SigmaRightP.Value := TINY;
end;

function T2BranchesPseudoVoigtPointsSet.GetSigmaRight: Double;
begin
    Assert(Assigned(SigmaRightP));
    Result := SigmaRightP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.HasEta: Boolean;
begin
    if Assigned(EtaP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.HasEtaRight: Boolean;
begin
    if Assigned(EtaRightP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.HasSigmaRight: Boolean;
begin
    if Assigned(SigmaRightP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.GetName: string;
begin
    Result := '2 br. Pseudo-Voigt';
end;

procedure T2BranchesPseudoVoigtPointsSet.SetParamByName(
    Name: string; Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Modified := True;

    if UpperCase(Name) = 'ETARIGHT' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := ' SetParamByName(EtaRight): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        EtaRight := Value;
    end
    else
    begin
        if UpperCase(Name) = 'SIGMARIGHT' then
        begin
{$IFDEF WRITE_PARAMS_LOG}
            LogStr := ' SetParamByName(SigmaRight): Value = ' + FloatToStr(Value);
            WriteLog(LogStr, Notification_);
{$ENDIF}
            SigmaRight := Value;
        end
        else
        begin
            if UpperCase(Name) = 'ETA' then
            begin
{$IFDEF WRITE_PARAMS_LOG}
                LogStr := ' SetParamByName(Eta): Value = ' + FloatToStr(Value);
                WriteLog(LogStr, Notification_);
{$ENDIF}
                Eta := Value;
            end
            else inherited;
        end;
    end;
end;

function T2BranchesPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
begin
    if UpperCase(Name) = 'ETARIGHT' then
        Result := EtaRight
    else
    begin
        if UpperCase(Name) = 'SIGMARIGHT' then
            Result := SigmaRight
        else
        begin
            if UpperCase(Name) = 'ETA' then
                Result := Eta
            else Result := inherited;
        end;
    end;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = EtaRightIndex then
        EtaRight := Value
    else
    begin
        if Index = SigmaRightIndex then
            SigmaRight := Value
        else
        begin
            if Index = EtaIndex then
                Eta := Value
            else inherited;
        end;
    end;
end;

function T2BranchesPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(Index < GetParamCount);

    if Index = EtaRightIndex then
        Result := EtaRight
    else
    begin
        if Index = SigmaRightIndex then
            Result := SigmaRight
        else
        begin
            if Index = EtaIndex then
                Result := Eta
            else Result := inherited;
        end;
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure T2BranchesPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
var Name: string;
begin
    Assert(Assigned(P));
    Name := P.Name;
    if UpperCase(P.Name) = 'ETA' then
        EtaP := P
    else
    begin
        if UpperCase(P.Name) = 'ETARIGHT' then
            EtaRightP := P
        else
        begin
            if UpperCase(P.Name) = 'SIGMARIGHT' then
                SigmaRightP := P
            else inherited;
        end;
    end;
end;

//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy semantikoy
procedure T2BranchesPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
var Name: string;
begin
    Assert(Assigned(P));
    Name := P.Name;
    if UpperCase(P.Name) = 'ETA' then
        EtaIndex := Index
    else
    begin
        if UpperCase(P.Name) = 'ETARIGHT' then
            EtaRightIndex := Index
        else
        begin
            if UpperCase(P.Name) = 'SIGMARIGHT' then
                SigmaRightIndex := Index
            else inherited;
        end;
    end;
end;

{====================== TAsymPseudoVoigtPointsSet =============================}

procedure TAsymPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := AsymPseudoVoigtPoint(
                        A, Sigma, Eta, x0, Points[j][1], DeltaSigma);
        end;
    end
    else
    begin
        AsymPseudoVoigt(Points, A, Sigma, Eta, x0, DeltaSigma);
    end;
end;

constructor TAsymPseudoVoigtPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    DeltaSigmaIndex := -1;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'deltasigma'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna
    InitLinks;
end;

procedure TAsymPseudoVoigtPointsSet.SetDeltaSigma(Value: Double);
begin
    Assert(Assigned(DeltaSigmaP));
    Modified := True;
    //  !!! dopuskayutsya otritsatel'nye znacheniya !!!
    DeltaSigmaP.Value := Value;
end;

function TAsymPseudoVoigtPointsSet.GetDeltaSigma: Double;
begin
    Assert(Assigned(DeltaSigmaP));
    Result := DeltaSigmaP.Value;
end;

function TAsymPseudoVoigtPointsSet.HasDeltaSigma: Boolean;
begin
    if Assigned(DeltaSigmaP) then Result := True else Result := False;
end;

function TAsymPseudoVoigtPointsSet.GetName: string;
begin
    Result := 'Asym. Pseudo-Voigt';
end;

procedure TAsymPseudoVoigtPointsSet.SetParamByName(Name: string; Value: Double);
var P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Modified := True;

    if UpperCase(Name) = 'DELTASIGMA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) +
            ' SetParamByName(DeltaSigma): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        DeltaSigma := Value;
    end
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
begin
    if UpperCase(Name) = 'DELTASIGMA' then Result := DeltaSigma
    else Result := inherited;
end;

procedure TAsymPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = DeltaSigmaIndex then DeltaSigma := Value
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(Index < GetParamCount);

    if Index = DeltaSigmaIndex then Result := DeltaSigma
    else Result := inherited;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TAsymPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'DELTASIGMA' then DeltaSigmaP := P
    else inherited;
end;
//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TAsymPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'DELTASIGMA' then DeltaSigmaIndex := Index
    else inherited;
end;

{========================== TLorentzPointsSet =================================}

function TLorentzPointsSet.GetName: string;
begin
    Result := 'Lorentzian';
end;

procedure TLorentzPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
    //x0Index, LastRightIndex: LongInt;
    //Zero: Boolean;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            (*  takoy variant ne daet uskoreniya, a kazhetsya rabotaet
                dazhe chut' medlennee - vse s'edaet poisk indeksov ?!
            for j := IndexOfValueX(Intervals.GetPointXCoord(i shl 1)) to
                IndexOfValueX(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
            *)
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
        end;
    end
    else
    begin
        //  snachala nuzhno obnulit' tochki, chtoby vse tochki, znachenie
        //  funktsii v kotoryh < ZeroCurveAmplitude byli bez musora
        for j := 0 to PointsCount - 1 do PointYCoord[j] := 0;

        //  schitaem optimal'no, ispol'zuya porog nulya i simmetriyu
        (*  optimal'nyi schet rabotaet tol'ko kogda x0 ne var'iruetsya,
            t.e. krivaya simmetrichna otnositel'no izmeneniya indeksa
        x0Index := IndexOfValueX(x0);
        Points[x0Index][2] := LorentzPoint(A, Sigma, x0, x0);

        Zero := False; LastRightIndex := x0Index;

        for j := x0Index - 1 downto 0 do
        begin
            Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
            if (x0Index shl 1) - j <= PointsCount - 1 then
            begin
                Points[(x0Index shl 1) - j][2] := Points[j][2];
                LastRightIndex := (x0Index shl 1) - j;
            end;
            //  vsegda polozhitelen
            if Points[j][2] < ZeroCurveAmplitude then
            begin
                Zero := True;
                Break;
            end;
        end;

        if not Zero then
            //  0 esche ne dostignut
            for j := LastRightIndex + 1 to PointsCount - 1 do
            begin
                Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
                //  vsegda polozhitelen
                if Points[j][2] < ZeroCurveAmplitude then Break;
            end;
        *)
        //  polnyy pereschet bez optimizatsii
        Lorentz(Points, A, Sigma, x0);
    end;
end;

{============================ TNamedPointsSet =================================}

function TNamedPointsSet.GetName: string;
begin
    Result := FName;
end;

procedure TNamedPointsSet.SetName(AName: string);
begin
    FName := AName;
end;

{=========================== TSpecialPointsSet ================================}

function TSpecialPointsSet.CalcValue(ArgValue: Double): Double;
var P: TSpecialCurveParameter;
    Prs: string;
    i: LongInt;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));
    Assert(Assigned(Links));
    Assert(Assigned(ArgP));
    //  zapolnyaetsya znachenie argumenta;
    P := ArgP;
    P.Value := ArgValue;
    //  sozdaetsya stroka parametrov;
    //  zdes' ispol'zuyutsya vse parametry
    Prs := '';
    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        Prs := Prs + P.Name + '=' + FloatToStr(P.Value) + Chr(0);
    end;
    //  !!! ustanovka znacheniy parametrov i vychislenie vyrazheniya
    //  d. delat'sya atomarnym vyzovom v mnogopotochnoy srede =>
    //  nel'zya predvaritel'no gotovit' znacheniya parametrov !!!
    (*
    if ParseAndCalcExpression(PChar(Expression), PChar(Prs), @Result) <> 1 then
        //??? mozhet zamenit' na Assert nad rezul'tatom
        raise Exception.Create('Inadmissible or invalid expression');

        bez ispol'zovaniya...*)
end;

function TSpecialPointsSet.GetName: string;
begin
    Result := FName;
end;

procedure TSpecialPointsSet.SetName(AName: string);
begin
    FName := AName;
end;

procedure TSpecialPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.GetPointXCoord(i shl 1)) to
                Trunc(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    PointYCoord[j] := CalcValue(PointXCoord[j]);
        end;
    end
    else
    begin
        //  poskol'ku vid krivoy ne izvesten, to optimizatsiya
        //  nevozmozhna - delaem polnyy pereschet
        for j := 0 to PointsCount - 1 do
            PointYCoord[j] := CalcValue(PointXCoord[j]);
    end;
end;

procedure TSpecialPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TSpecialPointsSet(Dest).Expression := Expression;
end;

{======================== TSpecialCurveParameter ==============================}

constructor TSpecialCurveParameter.Create(Collection: TCollection);
begin
    inherited;
    FType := Calculated;
end;

procedure TSpecialCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    Dest.Name := Name;
    Dest.Value := Value;
    Dest.Type_ := Type_;
    Dest.SavedValue := SavedValue;
    Dest.VariationDisabled := VariationDisabled;
    Dest.VariationStep := VariationStep;
end;

function TSpecialCurveParameter.GetValue_: string;
begin
    Result := FloatToStr(FValue);
end;

procedure TSpecialCurveParameter.SetValue_(AValue: string);
begin
    FValue := StrToFloat(AValue);
end;

{========================== Curve_parameters ==================================}

constructor Curve_parameters.Create;
var P: TSpecialCurveParameter;
begin
    inherited;
    FParams := TCollection.Create(TSpecialCurveParameter);
    //  !!! pustaya kollektsiya zapisyvaetsya v XML-potok nekorrektno !!!
    P := TSpecialCurveParameter.Create(FParams);
    P.Name := 'x';
    P.Type_ := Argument;
    P.Value := 0;
end;

destructor Curve_parameters.Destroy;
begin
    FParams.Free;
    inherited;
end;

procedure Curve_parameters.CopyParameters(const Dest: TObject);
var i: LongInt;
    P, New: TSpecialCurveParameter;
begin
    inherited;

    Curve_parameters(Dest).Params.Clear;

    for i := 0 to Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Items[i]);
        New := TSpecialCurveParameter.Create(Curve_parameters(Dest).Params);
        P.CopyTo(New);
    end;
    Curve_parameters(Dest).SavedInitHash := SavedInitHash;
end;

end.


