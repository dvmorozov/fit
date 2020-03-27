{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class representing single optimization task.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_task;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    asym_pseudo_voigt_points_set, Classes, curve_points_set, curve_types_singleton,
    downhill_simplex_minimizer, gauss_points_set, int_curve_type_selector,
    int_minimizer, log, lorentz_points_set, mscr_specimen_list, named_points_set,
    persistent_curve_parameter_container, persistent_curve_parameters,
    points_set, pseudo_voigt_points_set, self_copied_component,
    simple_minimizer, special_curve_parameter, SysUtils,
    two_branches_pseudo_voigt_points_set, typinfo
{$IFDEF _WINDOWS}
{$IFDEF WINDOWS_SPECIFIC}
    , user_points_set
{$ENDIF}
{$ENDIF}
    ;

type
    { Fits profile interval by model curves.
      Provides variable parameters and evaluation function for optimization
      algorithm.
      It is inherited from TComponent to allow inserting into TComponentList. }
    TFitTask = class(TComponent)
    protected
        FBegIndex:    longint;
        FEndIndex:    longint;
        { Enables curve scaling. Generally it should be true, otherwise
          optimization could stuck in local minimum. However it could be
          set to false for some special curve types. }
        FCurveScalingEnabled: boolean;
        FMaxRFactor:  double;
        FCurveTypeSelector: ICurveTypeSelector;
        { Expression defining user curve type. }
        FCurveExpr:   string;
        { Parameters of user defined curve. Parameters are given from the caller. 
          The object is used to construct curve instances. }
        FParams:      Curve_parameters;
        { Part of experimental profile corresponding to model interval. }
        FExpProfile:  TPointsSet;
        { List of background points. }
        FBackground:  TPointsSet;
        FSavedBackground: TPointsSet;
        FBackgroundWasSaved: boolean;
        { The calculated profile. Every value is calculated as a sum of values 
          of corresponding points of every curve (specimen) and background. }
        FCalcProfile: TPointsSet;
        { Contains positions of curves. Only X-coordinates are used. }
        FCurvePositions: TPointsSet;
        { Set of curves used to model experimental data inside given interval. }
        FCurves:  TSelfCopiedCompList;
        { The flag switches on using intervals in calculating R-factors.
          Using ranges is switched off when they are not given to accelerate computation. }
        FUseCurveRanges: boolean;
        { List of parameters of curves which are common for all the instances. }
        FCommonVariableParameters: Curve_parameters;
        { Background parameters. }
        FA, FB, FC, Fx0: double;

        //  ======================= dannye dlya optimizatora ====================
        FMinimizer: TMinimizer;

        { Index of pattern instance (specimen) parameters of which are variated at the moment. }
        FCurveNum: longint;
        { Index of parameter of pattern instance which is variated at the moment. }
        FParamNum: longint;
        FEOC:      boolean;
        { Flag signalling to terminate all internal loops. }
        FTerminated: boolean;
        { Index of common parameter which is variated at the moment. }
        FCommonVaryingIndex: longint;
        { Index of background point amplitude of which is variated at the moment. }
        FBackgroundVaryingIndex: longint;
        { Flag indicating that common parameters are variated at the moment. }
        FCommonVaryingFlag: boolean;
        { Flag indicating that amplitudes of background points are variated at the moment. }
        FBackgroundVaryingFlag: boolean;
        { Enables background variation. }
        FEnableBackgroundVariation: boolean;
        { Enables fast optimization algorithm. }
        FEnableFastMinimizer: boolean;

        FShowCurMin: TShowCurMin;
        FDoneProc:   TThreadMethod;
        function GetProfileIntegral: double;
        function GetCalcProfileIntegral: double;

        { Methods which are used by the optimizer. }

        { Calculates R-factor. }
        function Func: double;
        { Computes evaluation function. }
        procedure CalcFunc;
        { Returns initial variation step for current variable parameter. }
        function GetStep: double;
        { Does nothing. Should be implemented because is used by pointer.
          See OnSetStep. }
        procedure SetStep(NewStepValue: double);
        { Moves iteration to next variable parameter. }
        procedure SetNextParam;
        { Sets iteration to the first variable parameter. }
        procedure SetFirstParam;
        { Returns variable parameter value. }
        function GetParam: double;
        { Sets variable parameter value. }
        procedure SetParam(NewParamValue: double);
        { Returns True at the end of iteration cycle. }
        function EndOfCycle: boolean;
        { Divides all optimization steps by 2. Iterface method for TSimpleMinimizer2. }
        procedure DivideVariationStepBy2;
        procedure MultiplyVariationStep(Factor: double);
        { Returns flag indicating termination of the calculation. }
        function EndOfCalculation: boolean;
        { Calculates R-factor used for optimization. }
        function GetOptimizingRFactor: double;
        { Calculates R-factor used for comparison with maximal acceptable value. }
        function GetRFactor: double;

    protected
        { Current minimum value of R-factor by which maximal acceptable value is set up. 
          Last achived minimum value is stored to avoid redundant computations and locks
          in multithreaded environment. }
        FCurMin:    double;
        FCurSqrMin: double;
        FCurAbsMin: double;
        FCurMinInitialized: boolean;
        { Flag indicating that asynchronous operation executed as subtask was Terminated.
          For this class is always True for now because the class does not support asynchronous operations. }
        FAllDone:   boolean;

        procedure ShowCurMin; virtual;
        procedure Done; virtual;

        function GetSqrRFactor: double;
        function GetAbsRFactor: double;

        { Algorithms are methods executed asynchronously. }

        { Searches for set of curves fitting experimental profile with given accuracy
          sequentially decreasing number of curves. }
        procedure FindGaussesSequentiallyAlg;
        { Executes cycle of fitting of parameters of curves. }
        procedure Optimization;

        { Low-level methods of algorithms. }

        procedure StoreCurveParams;
        procedure RestoreCurveParams;
        { Sums all pattern instances and FBackground into single calculated profile. }
        procedure CalcGaussSum;
        procedure AddCurveToProfile(PS: TPointsSet);
        procedure SubbCurveFromProfile(PS: TPointsSet);
        { Removes from list of curve positions those points
          for which calculated curves have zero amplitude. }
        function DeleteZeros: boolean;
        { Deletes from list of curve positions the point
          in which amplitude of curve is minimal. }
        function DeleteMin(var Deleted: TCurvePointsSet): boolean;
        { Removes from list of curve positions the point
          in which experimental profile has maximal derivative.  }
        function DeleteMaxDerivative(var Deleted: TCurvePointsSet): boolean;

        { Auxiliary methods. }
        { Deletes poins with given X from the list passed via parameter. }
        procedure DeletePoint(var Points: TPointsSet; XValue: double);
        procedure AddPointToCurvePositions(XValue: double);
        { Creates fast optimization algorithm for the 1th step of 2-stage processing,
          by default is disabled.
          TODO: Make configurable. }
        procedure CreateFastMinimizer;
        { Creates downhill simplex algorithm for the 2nd step of 2-stage processing. }
        procedure CreateDHSMinimizer;
        { Calculates hash of initial values of parameters of pattern instance. }
        procedure CalcInitHash(Specimen: TCurvePointsSet);
        function CreatePatternInstance: TCurvePointsSet;
        function MinimumStepAchieved: boolean;
        procedure InitializeVariationSteps;

    public
        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: boolean;
            ACurveScalingEnabled: boolean); virtual;
        destructor Destroy; override;

        { Sets up experimental profile data. }
        procedure SetProfilePointsSet(APointsSet: TPointsSet);
        procedure SetCurvePositions(ACurvePositions: TPointsSet);
        { Returns final list of curve positions. }
        function GetCurvePositions: TPointsSet;
        { Returns final set of model curves (pattern instances). }
        function GetCurvesList: TSelfCopiedCompList;
        { Returns final calculated profile. }
        function GetCalcProfile: TPointsSet;
        { Returns current minimal achived value of R-factor (FCurMin). }
        function GetCurMin: double; virtual;
        function GetCurAbsMin: double; virtual;
        function GetCurSqrMin: double; virtual;
        function GetCurMinInitialized: boolean; virtual;
        function GetAllDone: boolean; virtual;

        procedure SetSpecialCurve(ACurveExpr: string;
            AParams: Curve_parameters);
        { Recreates pattern instances (curves). It should be public
          for initial calculation of R-factor for overall profile. }
        procedure RecreateCurveInstances(SpecimenParameters: TMSCRSpecimenList);
        { Searches pattern specimen by hash and sets its parameters 
          from the given list. }
        procedure SearchSpecimenAndInit(
            SpecimenParameters: TMSCRSpecimenList;
            Specimen: TCurvePointsSet);
        { Recalculates all pattern instances and FBackground.
          Calculates resulting profile. }
        procedure CalculateProfile;

        { Fits curves starting from given parameter set (initially or repeatedly). }
        procedure MinimizeDifference; virtual;
        procedure FindGaussesAgain; virtual;
        { Searches set of curves fitting exprerimental data with given
          accuracy sequentially decreasing number of such curves. }
        procedure MinimizeNumberOfCurves; virtual;
        { Sets up termination flags and returns. }
        procedure StopAsyncOper; virtual;
        { Returns the factor scaling calculated points up to scale of experimental data. }
        function GetScalingFactor: double;

        property MaxRFactor: double write FMaxRFactor;
        { Callback to update information at achieving new minimum. }
        property ServerShowCurMin: TThreadMethod read FShowCurMin write FShowCurMin;
        property ServerDoneProc: TThreadMethod read FDoneProc write FDoneProc;
        { Attributes store indexes of begin and end of the task interval 
          for optimal rebuilding overall resulting profile. }
        property BegIndex: longint read FBegIndex write FBegIndex;
        property EndIndex: longint read FEndIndex write FEndIndex;
    end;

    { The wrapper for future OpenCL implementation. }
    TOpenCLFitTask = class(TComponent)
    public
    end;

implementation

uses
    app, GeneralHashFunctions, SimpMath;

{================================== TFitTask ==================================}

function TFitTask.Func: double;
begin
    Result := GetOptimizingRFactor;
end;

procedure TFitTask.CalcFunc;
begin
    //  krivye ne nuzhno pereschityvat', poskol'ku vse uzhe
    //  pereschitano v SetParam
    CalculateProfile;
end;

function TFitTask.GetCalcProfileIntegral: double;
var
    i: longint;
begin
    Assert(Assigned(FCalcProfile));
    Result := 0;
    for i := 0 to FCalcProfile.PointsCount - 1 do
        Result := Result + FCalcProfile.PointYCoord[i];
end;

function TFitTask.GetProfileIntegral: double;
var
    i: longint;
begin
    Assert(Assigned(FExpProfile));
    Result := 0;
    for i := 0 to FExpProfile.PointsCount - 1 do
        Result := Result + FExpProfile.PointYCoord[i];
end;

function TFitTask.GetOptimizingRFactor: double;
begin
    Result := GetSqrRFactor;
    //Result := GetAbsRFactor;
end;

function TFitTask.GetAbsRFactor: double;
var
    CPS:     TCurvePointsSet;
    i, j:    longint;
    RFactor: double;
    Flag:    boolean;
    RangeDefined: boolean;
    ScalingFactor: double;
    CalcProfileIntegral: double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCalcProfile));
    Assert(Assigned(FExpProfile));

    Assert(Assigned(FCurves));
    //  esli ni u odnoy krivoy diapazon ne zadan,
    //  to R-faktor schitaetsya po vsemu profilyu
    RangeDefined := False;
    if FUseCurveRanges then
        for j := 0 to FCurves.Count - 1 do
        begin
            CPS := TCurvePointsSet(FCurves.Items[j]);
            if CPS.FRangeDefined then
            begin
                RangeDefined := True;
                Break;
            end;
        end;

    RFactor := 0;
    ScalingFactor := GetScalingFactor;
    CalcProfileIntegral := GetCalcProfileIntegral;
    if CalcProfileIntegral = 0 then
        CalcProfileIntegral := 1;

    //  profil' rasschityvaetsya prostoy summoy po vsem rasschitannym krivym,
    //  inache nel'zya poluchit' korrektnyy R-faktor dlya krivyh s ogranicheniyami;
    //  pri vychislenii R-faktora kazhdaya tochka proveryaetsya na prinalezhnost'
    //  k diapazonu kakoy-libo iz krivyh, t.o. v konechnoe znachenie R-faktora
    //  vkladyvayut tol'ko tochki iz ob'edineniya diapazonov vseh krivyh
    for i := 0 to FCalcProfile.PointsCount - 1 do
    begin
        //  proveryaetsya prinadlezhnost' tochki diapazonu krivoy
        if RangeDefined then
        begin
            Flag := False;      //  tochka ne prinadlezhit nikakomu diapazonu
            for j := 0 to FCurves.Count - 1 do
            begin
                CPS := TCurvePointsSet(FCurves.Items[j]);
                if CPS.FRangeDefined and
                    (FCalcProfile.PointXCoord[i] >= CPS.FMinX) and
                    (FCalcProfile.PointXCoord[i] <= CPS.FMaxX) then
                begin
                    Flag := True;
                    Break;
                end;
            end;
        end
        else
            Flag := True;      //  schitaem po vsem tochkam

        if Flag then            //  tochka vklyuchena v raschet R-faktora
            RFactor := RFactor + Abs(FCalcProfile.PointYCoord[i] *
                ScalingFactor - FExpProfile.PointYCoord[i]);
    end;
    RFactor := RFactor / CalcProfileIntegral;
    Result  := RFactor;
end;

function TFitTask.GetRFactor: double;
begin
    Result := GetSqrRFactor;
    //  !!! pri izmenenii tipa nuzhno ispravit' formulu v interfeyse,
    //  FA takzhe sohranyaemoe v FCurMin znachenie (sm. ShowCurMin) !!!
    //Result := GetAbsRFactor;
end;

function TFitTask.GetSqrRFactor: double;
var
    CPS:     TCurvePointsSet;
    i, j:    longint;
    RFactor: double;
    Flag:    boolean;
    RangeDefined: boolean;
    ScalingFactor: double;
    CalcProfileIntegral: double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCalcProfile));
    Assert(Assigned(FExpProfile));

    Assert(Assigned(FCurves));
    //  esli ni u odnoy krivoy diapazon ne zadan,
    //  to R-faktor schitaetsya po vsemu profilyu
    RangeDefined := False;
    if FUseCurveRanges then
        for j := 0 to FCurves.Count - 1 do
        begin
            CPS := TCurvePointsSet(FCurves.Items[j]);
            if CPS.FRangeDefined then
            begin
                RangeDefined := True;
                Break;
            end;
        end;

    RFactor := 0;
    ScalingFactor := GetScalingFactor;
    CalcProfileIntegral := GetCalcProfileIntegral;
    if CalcProfileIntegral = 0 then
        CalcProfileIntegral := 1;

    //  profil' rasschityvaetsya prostoy summoy po vsem rasschitannym krivym,
    //  inache nel'zya poluchit' korrektnyy R-faktor dlya krivyh s ogranicheniyami;
    //  pri vychislenii R-faktora kazhdaya tochka proveryaetsya na prinalezhnost'
    //  k diapazonu kakoy-libo iz krivyh, t.o. v konechnoe znachenie R-faktora
    //  vkladyvayut tol'ko tochki iz ob'edineniya diapazonov vseh krivyh
    for i := 0 to FCalcProfile.PointsCount - 1 do
    begin
        //  proveryaetsya prinadlezhnost' tochki diapazonu krivoy
        if RangeDefined then
        begin
            Flag := False;      //  tochka ne prinadlezhit nikakomu diapazonu
            for j := 0 to FCurves.Count - 1 do
            begin
                CPS := TCurvePointsSet(FCurves.Items[j]);
                if CPS.FRangeDefined and
                    (FCalcProfile.PointXCoord[i] >= CPS.FMinX) and
                    (FCalcProfile.PointXCoord[i] <= CPS.FMaxX) then
                begin
                    Flag := True;
                    Break;
                end;
            end;
        end
        else
            Flag := True;      //  schitaem po vsem tochkam

        if Flag then            //  tochka vklyuchena v raschet R-faktora
            RFactor := RFactor + Sqr(FCalcProfile.PointYCoord[i] *
                ScalingFactor - FExpProfile.PointYCoord[i]);
    end;
    RFactor := RFactor / Sqr(CalcProfileIntegral);
    Result  := RFactor;
end;

function TFitTask.GetStep: double;
var
    Curve: TCurvePointsSet;
begin
    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
        Result := 0.1//  TODO: move into separate "FBackground" class.

    else
    if FCommonVaryingFlag then
        Result := FCommonVariableParameters[FCommonVaryingIndex].VariationStep
    else
    begin
        Curve  := TCurvePointsSet(FCurves.Items[FCurveNum]);
        Result := Curve.VariationSteps[FParamNum];
    end;
end;

{$hints off}
procedure TFitTask.SetStep(NewStepValue: double);
begin

end;

{$hints on}

procedure TFitTask.SetNextParam;
var
    Curve: TCurvePointsSet;
    Count: longint;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));
    Assert(Assigned(FCommonVariableParameters));

    FEOC := True;
    if FCurves.Count <> 0 then
    begin
        //  perebor parametrov krivoy
        Curve := TCurvePointsSet(FCurves.Items[FCurveNum]);
        if FParamNum < Curve.VariableCount - 1 then
        begin
            Inc(FParamNum);
            FEOC := False;
            Exit;
        end;
    end;

    if FCurveNum < FCurves.Count - 1 then
    begin
        //  perebor krivyh
        Inc(FCurveNum);
        FEOC      := False;
        FParamNum := 0;
        Exit;
    end;

    Count := FCommonVariableParameters.Params.Count;
    if FCommonVaryingFlag then
        if FCommonVaryingIndex < Count then
        begin
            Inc(FCommonVaryingIndex);
            while (FCommonVaryingIndex <> Count) and
                FCommonVariableParameters[FCommonVaryingIndex].VariationDisabled do
                Inc(FCommonVaryingIndex);
        end//  poisk sleduyuschego obschego parametra,
    //  variatsiya kotorogo ne zapreschena
    ;

    if FCommonVaryingIndex < Count then
    begin
        FEOC := False;
        FCommonVaryingFlag := True;
        Exit;
    end;

    if FEnableBackgroundVariation then
    begin
        if FBackgroundVaryingFlag then
            Inc(FBackgroundVaryingIndex)
        //  Increments parameter index for next iteration.
        ;

        if FBackgroundVaryingIndex < //FBackground.PointsCount
            4 then
        begin
            //  There are still next variable FBackground parameters.
            FBackgroundVaryingFlag := True;
            FEOC := False;
            Exit;
        end;
    end;
end;

procedure TFitTask.SetFirstParam;
var
    i: longint;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    FCurveNum := 0;
    FParamNum := 0;
    FEOC      := False;
    //  poisk pervogo parametra razreschennogo k variatsii
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        if not FCommonVariableParameters[i].VariationDisabled then
            Break;
    FCommonVaryingIndex     := i;
    FCommonVaryingFlag      := False;
    FBackgroundVaryingIndex := 0;
    FBackgroundVaryingFlag  := False;
    FBackgroundWasSaved     := False;
end;

function TFitTask.GetParam: double;
var
    GP: TCurvePointsSet;
    Parameter: TSpecialCurveParameter;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));
    Assert(Assigned(FCommonVariableParameters));

    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
    begin
        Assert(FBackgroundVaryingIndex < (*Background.PointsCount*)4);
        //Result := FBackground.PointYCoord[FBackgroundVaryingIndex];
        case (FBackgroundVaryingIndex) of
            0: Result := FA;
            1: Result := FB;
            2: Result := FC;
            3: Result := Fx0;
        end;
    end
    else
    if FCommonVaryingFlag then
    begin
        Assert(FCommonVaryingIndex < FCommonVariableParameters.Params.Count);
        Parameter := FCommonVariableParameters[FCommonVaryingIndex];
        Result    := Parameter.Value;
    end
    else
    begin
        Assert(FCurves.Count <> 0);

        GP     := TCurvePointsSet(FCurves.Items[FCurveNum]);
        Result := GP.VariableValues[FParamNum];
    end;
end;

procedure TFitTask.SetParam(NewParamValue: double);
var
    GP: TCurvePointsSet;
    i:  longint;
    Parameter: TSpecialCurveParameter;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));
    Assert(Assigned(FCommonVariableParameters));

    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
    begin
        Assert(FBackgroundVaryingIndex < (*Background.PointsCount*)4);
        //FBackground.PointYCoord[FBackgroundVaryingIndex] := NewParamValue;
        Assert(FSavedBackground.PointsCount = FBackground.PointsCount);
        if not FBackgroundWasSaved then
        begin
            for i := 0 to FSavedBackground.PointsCount - 1 do
                FSavedBackground.PointYCoord[i] := FBackground.PointYCoord[i];
            FBackgroundWasSaved := True;
        end;
        case (FBackgroundVaryingIndex) of
            0: FA  := Abs(NewParamValue);
            1: FB  := NewParamValue;
            2: FC  := Abs(NewParamValue);
            3: Fx0 := NewParamValue;
        end;
    end
    else
    if FCommonVaryingFlag then
    begin
        Assert(FCommonVaryingIndex < FCommonVariableParameters.Params.Count);
        Parameter := FCommonVariableParameters[FCommonVaryingIndex];
        Parameter.Value := NewParamValue;

        //  ustanovka znacheniya obschego parametra u vseh ekzemplyarov
        for i := 0 to FCurves.Count - 1 do
        begin
            GP    := TCurvePointsSet(FCurves.Items[i]);
            GP.ValuesByName[
                FCommonVariableParameters[FCommonVaryingIndex].Name
                ] := NewParamValue;
        end;
        //CalculateProfile;
    end
    else
    begin
        Assert(FCurves.Count <> 0);
        //  takoy algoritm mog by privodit' k nakopleniyu oshibki
        //  v summarnom profile (kogda znachenie intensivnosti
        //  summarnogo profilya otlichaetsya ot summy intensivnostey
        //  vseh krivyh), no pri variatsii Sigma ispol'zuetsya
        //  polnyy pereschet, poetomu nakopleniya ne proishodit
        GP := TCurvePointsSet(FCurves.Items[FCurveNum]);
        //  ??? v nekotoryh sluchayah rabotaet optimal'nee
        //SubbCurveFromProfile(GP);
        GP.VariableValues[FParamNum] := NewParamValue;
        //GP.ReCalc(nil);
        //AddCurveToProfile(GP);
    end;
end;

function TFitTask.EndOfCycle: boolean;
begin
    Result := FEOC;
end;

procedure TFitTask.DivideVariationStepBy2;
begin
    MultiplyVariationStep(0.99);
end;

procedure TFitTask.MultiplyVariationStep(Factor: double);
var
    i: longint;
begin
    Assert(Assigned(FCommonVariableParameters));

    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        FCommonVariableParameters[i].MultiplyVariationStep(Factor);
end;

function TFitTask.EndOfCalculation: boolean;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Result := False;
    if (FMinimizer.CurrentMinimum < FMaxRFactor) then
        Result := True//OutputDebugString(PChar('Desired R-factor achived...'));

    else
    if MinimumStepAchieved then
        Result := True//OutputDebugString(PChar('Minimumu step achived...'));
    ;
end;

constructor TFitTask.Create(AOwner: TComponent; AEnableBackgroundVariation: boolean;
    ACurveScalingEnabled: boolean);
begin
    inherited Create(AOwner);
    FCommonVariableParameters := Curve_parameters.Create(nil);
    FCommonVariableParameters.Params.Clear;
    //  Curve_parameters sozdaet v konstruktore
    //  odin parametr - nuzhno ego udalit'
    FMaxRFactor := 0.01;
    FAllDone    := False;
    //  Sets default curve type
    FCurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    FEnableBackgroundVariation := AEnableBackgroundVariation;
    FEnableFastMinimizer := False;
    FCurveScalingEnabled := ACurveScalingEnabled;
end;

destructor TFitTask.Destroy;
begin
    FExpProfile.Free;
    FCurves.Free;
    FCalcProfile.Free;
    FBackground.Free;
    FSavedBackground.Free;
    FCurvePositions.Free;
    FMinimizer.Free;
    FParams.Free;
    FCommonVariableParameters.Free;
    inherited;
end;

procedure TFitTask.CalculateProfile;
var
    i:     longint;
    Curve: TCurvePointsSet;
    RestoreBackground: boolean;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));
    Assert(FBackground.PointsCount = FSavedBackground.PointsCount);
    Assert(FExpProfile.PointsCount = FSavedBackground.PointsCount);

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := FCurves.Items[i] as TCurvePointsSet;
        Curve.ReCalc(nil);
    end;
    //  raschet tochek fona
    RestoreBackground := False;
    for i := 0 to FBackground.PointsCount - 1 do
    begin
        FBackground.PointYCoord[i] :=
            CalcPolinom2(FA, FB, FC, Fx0, FBackground.PointXCoord[i]);
        if (FBackground.PointYCoord[i] > FExpProfile.PointYCoord[i]) or
            (FBackground.PointYCoord[i] < 0) then
        begin
            RestoreBackground := True;
            Break;
        end;
    end;
    if RestoreBackground then
        for i := 0 to FBackground.PointsCount - 1 do
            FBackground.PointYCoord[i] := FSavedBackground.PointYCoord[i]
    else
        FBackgroundWasSaved := False;

    CalcGaussSum;
end;

procedure TFitTask.StoreCurveParams;
var
    i:  longint;
    PS: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));

    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TCurvePointsSet(FCurves.Items[i]);
        PS.StoreParams;
    end;
end;

procedure TFitTask.RestoreCurveParams;
var
    i:  longint;
    PS: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurves));

    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TCurvePointsSet(FCurves.Items[i]);
        PS.RestoreParams;
    end;
end;

procedure TFitTask.CalcGaussSum;
var
    i:  longint;
    PS: TPointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCalcProfile));
    Assert(Assigned(FBackground));
    Assert(Assigned(FCurves));
    Assert(FBackground.PointsCount = FCalcProfile.PointsCount);

    //  obnulenie profilya
    for i := 0 to FCalcProfile.PointsCount - 1 do
        FCalcProfile.PointYCoord[i] := 0;

    //  vychislenie novogo
    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TPointsSet(FCurves.Items[i]);
        AddCurveToProfile(PS);
    end;
    //  dobavleniye fona
    for i := 0 to FBackground.PointsCount - 1 do
        FCalcProfile.PointYCoord[i] :=
            FCalcProfile.PointYCoord[i] + FBackground.PointYCoord[i];
end;

procedure TFitTask.AddCurveToProfile(PS: TPointsSet);
var
    j: longint;
begin
    for j := 0 to FCalcProfile.PointsCount - 1 do
        FCalcProfile.PointYCoord[j] :=
            FCalcProfile.PointYCoord[j] + PS.PointYCoord[j];
end;

procedure TFitTask.SubbCurveFromProfile(PS: TPointsSet);
var
    j: longint;
begin
    for j := 0 to FCalcProfile.PointsCount - 1 do
        FCalcProfile.PointYCoord[j] :=
            FCalcProfile.PointYCoord[j] - PS.PointYCoord[j];
end;

procedure TFitTask.InitializeVariationSteps;
var
    i, j:  longint;
    Curve: TCurvePointsSet;
begin
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        FCommonVariableParameters[i].InitVariationStep;

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := TCurvePointsSet(FCurves.Items[i]);
        for j := 0 to Curve.VariableCount - 1 do
            Curve.InitVariationStep(j);
    end;
end;

function TFitTask.MinimumStepAchieved: boolean;
var
    i, j:  longint;
    Curve: TCurvePointsSet;
begin
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        if not FCommonVariableParameters[i].MinimumStepAchieved then
        begin
            Result := False;
            Exit;
        end;

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := TCurvePointsSet(FCurves.Items[i]);
        for j := 0 to Curve.VariableCount - 1 do
            if not Curve.MinimumStepAchieved(j) then
            begin
                Result := False;
                Exit;
            end;
    end;
    Result := True;
end;

procedure TFitTask.CreateFastMinimizer;
begin
    FMinimizer.Free;
    FMinimizer := nil;
    FMinimizer := TSimpleMinimizer3.Create(nil);
    FMinimizer.OnFunc := Func;
    FMinimizer.OnCalcFunc := CalcFunc;
    FMinimizer.OnGetStep := GetStep;
    FMinimizer.OnSetStep := SetStep;
    FMinimizer.OnSetNextParam := SetNextParam;
    FMinimizer.OnSetFirstParam := SetFirstParam;
    FMinimizer.OnGetParam := GetParam;
    FMinimizer.OnSetParam := SetParam;
    FMinimizer.OnEndOfCycle := EndOfCycle;
    FMinimizer.OnShowCurMin := ShowCurMin;

    TSimpleMinimizer3(FMinimizer).FEndOfCalculation := EndOfCalculation;
    TSimpleMinimizer3(FMinimizer).FMultiplyVariationStep := MultiplyVariationStep;

    InitializeVariationSteps;

    FEOC      := False;
    FParamNum := 0;
    FCurveNum := 0;
    FCommonVaryingFlag := False;
    FCommonVaryingIndex := 0;
    FBackgroundVaryingFlag := False;
    FBackgroundVaryingIndex := 0;
    FBackgroundWasSaved := False;
end;

procedure TFitTask.CreateDHSMinimizer;
begin
    FMinimizer.Free;
    FMinimizer := nil;
    FMinimizer := TDownhillSimplexMinimizer.Create(nil);
    FMinimizer.OnFunc := Func;
    FMinimizer.OnCalcFunc := CalcFunc;
    FMinimizer.OnGetStep := GetStep;
    FMinimizer.OnSetStep := SetStep;
    FMinimizer.OnSetNextParam := SetNextParam;
    FMinimizer.OnSetFirstParam := SetFirstParam;
    FMinimizer.OnGetParam := GetParam;
    FMinimizer.OnSetParam := SetParam;
    FMinimizer.OnEndOfCycle := EndOfCycle;
    FMinimizer.OnShowCurMin := ShowCurMin;

    InitializeVariationSteps;

    FEOC      := False;
    FParamNum := 0;
    FCurveNum := 0;
    FCommonVaryingFlag := False;
    FCommonVaryingIndex := 0;
    FBackgroundVaryingFlag := False;
    FBackgroundVaryingIndex := 0;
    FBackgroundWasSaved := False;
end;

   //  udalyaet iz spiska vydelennyh tochek te tochki,
   //  dlya kotoryh gaussiany imeyut nulevuyu intensivnost'
function TFitTask.DeleteZeros: boolean;
var
    i, j: longint;
    GP:   TCurvePointsSet;
    MaxA: double;       //     maks. amplituda krivoy
    //  konstanta, men'she kotoroy vse otnositel'nye
    //  amplitudy schitayutsya ravnymi nulyu
const
    ZeroConst: double = 0.001;    //  0.1%
begin
    Result := False;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    MaxA   := 0;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        Assert(GP.A >= 0);
        if GP.A > MaxA then
            MaxA := GP.A;
    end;
    //  esli posle tsikla optimizatsii MaxA = 0 eto
    //  oznachaet, chto model' sovershenno ne sootvetstvuet
    //  dannym, poetomu vse krivye mozhno udalit'
    i := 0;
    while i < FCurves.Count do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        Assert(GP.A >= 0);
        if (MaxA = 0) or (GP.A / MaxA < ZeroConst) then
        begin
            //  udalyaetsya sootvetstvuyuschaya tochka iz
            //  spiska vydelennyh tochek
            for j := 0 to FCurvePositions.PointsCount - 1 do
                if Abs(FCurvePositions.PointXCoord[j] - GP.FInitx0) <= TINY then
                begin
                    DeletePoint(FCurvePositions, GP.FInitx0);
                    Result := True;
                    Break;
                end//if FCurvePositions.PointXCoord[j] = GP.FInitx0 then
            ;
            FCurves.Remove(GP);  //  osvobozhdaet GP
        end
        else
            Inc(i);
    end;
end;

function TFitTask.DeleteMaxDerivative(var Deleted: TCurvePointsSet): boolean;
var
    Der, MaxDer: double;
    First: boolean;
    MaxGP, GP: TCurvePointsSet;
    i, Index: longint;
    SA: TPointsSet;
begin
    Result  := False;
    Deleted := nil;
    MaxGP   := nil;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(FCurves.Count <> 0);
    if FCurves.Count <= 1 then
        Exit;
    Assert(Assigned(FCurvePositions));
    SA := FExpProfile;
    Assert(Assigned(SA));
    if FExpProfile.PointsCount <= 1 then
        Exit;

    First := True;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.Hasx0 then
            Exit;

        if First then
        begin
            //  pervyy raz nahodim proizvodnuyu
            Index := FExpProfile.IndexOfValueX(GP.FInitx0);
            Assert(Index <> -1);
            if Index = FExpProfile.PointsCount - 1 then
                Exit;

            MaxDer := Abs(FExpProfile.PointYCoord[Index + 1] -
                FExpProfile.PointYCoord[Index]);
            MaxGP  := GP;
            First  := False;
        end
        else
        begin
            //  !!! etot algoritm ne mozhet vychislit' proizvodnuyu
            //  v posledney tochke profilya, FA znachit i udalit'
            //  vposledstvii takuyu tochku !!!
            Index := FExpProfile.IndexOfValueX(GP.FInitx0);
            Assert(Index <> -1);
            if Index = FExpProfile.PointsCount - 1 then
                Break;

            Der := Abs(FExpProfile.PointYCoord[Index + 1] -
                FExpProfile.PointYCoord[Index]);

            if Der > MaxDer then
            begin
                MaxDer := Der;
                MaxGP  := GP;
            end;
        end;
    end;
    Assert(Assigned(MaxGP));
    DeletePoint(FCurvePositions, MaxGP.FInitx0);
    Deleted := FCurves.Extract(MaxGP);
    Result  := True;
end;

   //  udalyaet iz spiska vydelennyh tochek tu,
   //  u kotoroy amplituda krivoy minimal'na
function TFitTask.DeleteMin(var Deleted: TCurvePointsSet): boolean;
var
    Min:   double;
    First: boolean;
    MinGP, GP: TCurvePointsSet;
    i:     longint;
begin
    Result := False;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(FCurves.Count <> 0);
    if FCurves.Count <= 1 then
        Exit;

    First := True;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        Assert(GP.A >= 0);
        if First then
        begin
            Min   := GP.A;
            MinGP := GP;
            First := False;
        end
        else
        if GP.A < Min then
        begin
            Min   := GP.A;
            MinGP := GP;
        end;
    end;
    Assert(Assigned(MinGP));
    DeletePoint(FCurvePositions, MinGP.FInitx0);
    Deleted := FCurves.Extract(MinGP);
    Result  := True;
end;

procedure TFitTask.CalcInitHash(Specimen: TCurvePointsSet);
var
    i:     longint;
    Parameter: TPersistentCurveParameterContainer;
    Value: string;
begin
    Assert(Assigned(Specimen));
    Specimen.FInitHash := 0;
    for i := 0 to Specimen.Parameters.Params.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(
            Specimen.Parameters.Params.Items[i]);
        Value     := Parameter.Value_;
        Specimen.FInitHash := Specimen.FInitHash + JSHash(Value);
    end;
end;

procedure TFitTask.SearchSpecimenAndInit(SpecimenParameters: TMSCRSpecimenList;
    Specimen: TCurvePointsSet);
var
    i, j, k: longint;
    CurveParameters: Curve_parameters;
    Parameter, Parameter2: TSpecialCurveParameter;
begin
    //Assert(Assigned(SpecimenParameters));
    //  ravenstvo nil ne protivorechit rasschirennoy semantike metoda; krome
    //  togo neobhodimo dopustit' vozmozhnost' takogo znacheniya pri vyzove
    //  iz drugih metodov klassa
    if not Assigned(SpecimenParameters) then
        Exit;
    Assert(Assigned(Specimen));

    for i := 0 to SpecimenParameters.Count - 1 do
    begin
        CurveParameters := Curve_parameters(SpecimenParameters.Items[i]);

        if CurveParameters.FSavedInitHash = Specimen.FInitHash then
        begin
            //Specimen.SetParameters(Curve_parameters(CurveParameters.GetCopy));
            //  v naborah parametrov, hranyaschihsya v spiske SpecimenParameters
            //  mogut byt' vychislyaemye, kotorye ne nuzhno kopirovat'
            for j := 0 to Specimen.Parameters.Params.Count - 1 do
            begin
                Parameter := Specimen.Parameters[j];
                for k := 0 to CurveParameters.Params.Count - 1 do
                begin
                    Parameter2 := CurveParameters[k];
                    if Parameter.Name = Parameter2.Name then
                    begin
                        Parameter.Value := Parameter2.Value;
                        Break;
                    end;
                end;
            end;
            Break;
        end;
    end;
end;

function TFitTask.CreatePatternInstance: TCurvePointsSet;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
    SelectedCurveTypeId: TCurveTypeId;
begin
    SelectedCurveTypeId := FCurveTypeSelector.GetSelectedCurveType;
    if IsEqualGUID(SelectedCurveTypeId, TLorentzPointsSet.GetCurveTypeId) then
        Result := TLorentzPointsSet.Create(nil)
    else
    if IsEqualGUID(SelectedCurveTypeId, TGaussPointsSet.GetCurveTypeId) then
        Result := TGaussPointsSet.Create(nil)
    else
    if IsEqualGUID(SelectedCurveTypeId, TPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := TPseudoVoigtPointsSet.Create(nil)
    else
    if IsEqualGUID(SelectedCurveTypeId, TAsymPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := TAsymPseudoVoigtPointsSet.Create(nil)
    else
{$IFDEF WINDOWS_SPECIFIC}
    if IsEqualGUID(SelectedCurveTypeId, TUserPointsSet.GetCurveTypeId) then
    begin
        Result := TUserPointsSet.Create(nil);
        TUserPointsSet(Result).Expression := FCurveExpr;
        TUserPointsSet(Result).SetParameters(
            Curve_parameters(FParams.GetCopy));
    end
    else
{$ENDIF}
    if IsEqualGUID(SelectedCurveTypeId,
        T2BranchesPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := T2BranchesPseudoVoigtPointsSet.Create(nil);

    if FCommonVariableParameters.Count = 0 then
        for i := 0 to Result.Parameters.Count - 1 do
            if (Result.Parameters[i].Type_ = Shared) and
                (not Result.Parameters[i].VariationDisabled) then
            begin
                Parameter := Result.Parameters[i].CreateCopy;
                Parameter.InitValue;
                Parameter.InitVariationStep;

                try
                    Container :=
                        TPersistentCurveParameterContainer(
                        FCommonVariableParameters.Params.Add);

                    try
                        Container.Parameter := Parameter;
                    except
                        FCommonVariableParameters.Params.Delete(Container.ID);
                        Container.Free;
                        raise;
                    end;

                except
                    Parameter.Free;
                    raise;
                end;
            end//  Initializing list of common parameters. It is performed only
    //  once when the first curve instance is created (it is assumed
    //  that all the instances have the same type).
    //  TODO: remove the assumption mentioned above.
    ;

    for i := 0 to Result.Parameters.Count - 1 do
    begin
        Result.Parameters[i].InitValue;
        Result.Parameters[i].InitVariationStep;
    end;
end;

procedure TFitTask.RecreateCurveInstances(SpecimenParameters: TMSCRSpecimenList);
var
    i, j, k:    longint;
    Curve:      TCurvePointsSet;
    CurveFound: boolean;
    //  koordinaty tochki privyazki sozdavaemogo avtomaticheski
    //  ekz. patterna, kogda pol'zovatel'skie tochki privyazki
    //  ne zadany
    //CurvePosition,
{$IFDEF WINDOWS_SPECIFIC}
    CurveAmplitude: double;
    SelectedCurveTypeId: TCurveTypeId;
{$ENDIF}
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FCurvePositions));
    Assert(Assigned(FExpProfile));
    Assert(FExpProfile.PointsCount >= 2);

    //  Saves previously created curve instances.
    if not Assigned(FCurves) then
        FCurves := TSelfCopiedCompList.Create(nil);

    //  sozdaem zanovo summarnyy profil'
    if Assigned(FCalcProfile) then
        FCalcProfile.Clear
    else
        FCalcProfile := TPointsSet.Create(nil);
    //  kol-vo tochek profilya ustanavlivaetsya ravnym kol-vu tochek uchastka
    for i := 0 to FExpProfile.PointsCount - 1 do
        FCalcProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);
    //  sozdaem zanovo massiv tochek fona
    if Assigned(FBackground) then
        FBackground.Clear
    else
        FBackground := TPointsSet.Create(nil);
    for i := 0 to FExpProfile.PointsCount - 1 do
        FBackground.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    if Assigned(FSavedBackground) then
        FSavedBackground.Clear
    else
        FSavedBackground := TPointsSet.Create(nil);
    for i := 0 to FExpProfile.PointsCount - 1 do
        FSavedBackground.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    //  proveryaem i udalyaem te ekz. patterna,
    //  polozheniya kot. net sredi vybrannyh tochek
    k := 0;
    while k < FCurves.Count do
    begin
        CurveFound := False;
        Curve      := TCurvePointsSet(FCurves.Items[k]);
        //  esli pattern ne imeet parametra polozheniya, to
        //  ego ekzemplyary ne udalyayutsya
        if not Curve.Hasx0 then
            Break;

        for i := 0 to FCurvePositions.PointsCount - 1 do
            if Curve.FInitx0 = FCurvePositions.PointXCoord[i] then
            begin
                CurveFound := True;
                Break;
            end;

        if not CurveFound then
            FCurves.Remove(Curve)// udalyaem
        //  FCurves po-umolchaniyu osvobozhdaet
        //  komponenty, ssylki na kotorye hranit

        else
            Inc(k);
    end;
    //  uslovie obratnoe dannomu oznachaet, chto spisok krivyh ne
    //  pust, i pri etom pattern ne imeet parametra polozheniya;
    //  v takom sluchae nichego delat' ne nuzhno...
    if (FCurves.Count = 0) or (TCurvePointsSet(FCurves.Items[0]).Hasx0) then
        if FCurvePositions.PointsCount = 0 then
        begin
            //  dobavlyaetsya odin ekzemplyar patterna na dannyy interval;
            //  esli pattern imeet parametr polozheniya, to u ekzemplyara
            //  znachenie etogo parametra ustanavlivaetsya ravnym
            //  seredine intervala
            (*  v pervom variante algoritma dobavlyalsya odin ekzemplyar
                krivoy tipa po umolchaniyu, odnako eto vyzyvalo problemu pri
                ispol'zovanii programmy - nel'zya bylo udalit' iz
                intervala edinstvennuyu krivuyu, sozdannuyu etim
                algoritmom
            Curve := CreatePatternInstance;

            try
                //  dlina kazhdogo ekz. patterna ust. ravnoy
                //  dline uchastka vybrannogo pol'zovatelem
                for j := 0 to SA.PointsCount - 1 do
                    Curve.AddNewPoint(SA.PointXCoord[j], 0);

                //  amplituda i tochka privyazki ustanavlivayutsya po
                //  sredney tochke intervala
                CurvePosition := SA.PointXCoord[SA.PointsCount div 2];
                CurveAmplitude := SA.PointYCoord[SA.PointsCount div 2];
                if Curve.HasA then Curve.A := CurveAmplitude;
                //  ust. polozhenie ekz. patterna
                if Curve.Hasx0 then
                begin
                    Curve.x0 := CurvePosition;
                    Curve.FInitx0 := CurvePosition;
                end;
                if Curve.HasSigma then Curve.Sigma := Sigma;
                //  ne zapolnyaetsya, potomu chto ne nuzhna
                //Curve.Lambda := WaveLength;
                CalcInitHash(Curve);
                SearchSpecimenAndInit(SpecimenParameters, Curve);
                //  dobavlenie novogo ekz. patterna v spisok
                FCurves.Add(Curve);
                //  dobavlenie tochki pryavyazki, kogda pattern imeet tochku
                //  privyazki dlya posleduyuschego otobrazheniya v obschem spiske
                if TCurvePointsSet(FCurves.Items[0]).Hasx0 then
                    FCurvePositions.AddNewPoint(CurvePosition, CurveAmplitude);
            except
                Curve.Free;
                raise;
            end;
            *)
{$IFDEF WINDOWS_SPECIFIC}
            SelectedCurveTypeId := FCurveTypeSelector.GetSelectedCurveType;
            //  teper' sozdaetsya ekzemplyar tol'ko
            //  pol'zovatel'skoy krivoy, kotoraya ne
            //  imeet parametra polozheniya
            if IsEqualGUID(SelectedCurveTypeId, TUserPointsSet.GetCurveTypeId) then
            begin
                Curve := CreatePatternInstance;

                try
                    TUserPointsSet(Curve).Expression := FCurveExpr;
                    TUserPointsSet(Curve).SetParameters(
                        Curve_parameters(FParams.GetCopy));

                    if not Curve.Hasx0 then
                    begin
                        //  dlina kazhdogo ekz. patterna ust. ravnoy
                        //  dline uchastka vybrannogo pol'zovatelem
                        for j := 0 to FExpProfile.PointsCount - 1 do
                            Curve.AddNewPoint(FExpProfile.PointXCoord[j], 0);

                        //  amplituda i tochka privyazki ustanavlivayutsya po
                        //  sredney tochke intervala
                        CurveAmplitude :=
                            FExpProfile.PointYCoord[FExpProfile.PointsCount div 2];
                        if Curve.HasA then
                            Curve.A := CurveAmplitude;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //Curve.Lambda := WaveLength;
                        CalcInitHash(Curve);
                        SearchSpecimenAndInit(SpecimenParameters, Curve);
                        //  dobavlenie novogo ekz. patterna v spisok
                        FCurvesList.Add(Curve);
                    end
                    else
                        Curve.Free;
                except
                    Curve.Free;
                    raise;
                end;
            end;
{$ENDIF}
        end //  if FCurvePositions.PointsCount = 0
        else
            for i := 0 to FCurvePositions.PointsCount - 1 do
            begin
                CurveFound := False;
                for k := 0 to FCurves.Count - 1 do
                begin
                    Curve := TCurvePointsSet(FCurves.Items[k]);
                    if not Curve.Hasx0 then
                        Break;

                    //if Curve.FInitx0 = FCurvePositions.PointXCoord[i] then
                    if Abs(Curve.FInitx0 - FCurvePositions.PointXCoord[i]) <= TINY then
                    begin
                        CurveFound := True;
                        Break;
                    end;
                end;

                if not CurveFound then
                    //  naydena tochka privyazki ekz. patterna, dlya kot. net
                    //  ekzemplyara (pri tom, chto pattern imeet parametr polozheniya)
                    //  ili spisok ekzemplyarov patterna pust
                begin
                    //  sozdaetsya novyy ekz. patterna
                    Curve := CreatePatternInstance;

                    try
                        //  dlina kazhdogo ekz. patterna ust. ravnoy
                        //  dline uchastka vybrannogo pol'zovatelem
                        for j := 0 to FExpProfile.PointsCount - 1 do
                            Curve.AddNewPoint(FExpProfile.PointXCoord[j], 0);

                        if Curve.HasA then
                            Curve.A := FCurvePositions.PointYCoord[i];
                        //  ust. polozhenie ekz. patterna
                        if Curve.Hasx0 then
                        begin
                            Curve.x0      := FCurvePositions.PointXCoord[i];
                            Curve.FInitx0 := FCurvePositions.PointXCoord[i];
                        end;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //Curve.Lambda := WaveLength;
                        CalcInitHash(Curve);
                        SearchSpecimenAndInit(SpecimenParameters, Curve);
                        //  dobavlenie novogo ekz. patterna v spisok
                        FCurves.Add(Curve);
                        //  esli pattern ne imeet parametra polozheniya,
                        //  to sozdaetsya tol'ko odin ekzemplyar
                        if not Curve.Hasx0 then
                            Break;
                    except
                        Curve.Free;
                        raise;
                    end;
                end;
            end//  proveryaem vybrannye tochki i dobavlyaem novye ekz. patterna
    ;
end;

procedure TFitTask.FindGaussesSequentiallyAlg;
var
    ZerosDeleted, PointDeleted: boolean;
    Deleted: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    //  pervyy tsikl optimizatsii
    Optimization;

    Deleted      := nil;
    //  initsializatsiya na sluchay esli uzhe posle pervogo tsikla
    //  optimizatsii fakt. rash. prevyshaet porog
    PointDeleted := False;
    //  udalyaem iz spiska vydelennyh tochek te tochki, dlya
    //  kotoryh gaussiany imeyut nulevuyu amplitudu i te
    //  tochki, v kotoryh proizvodnaya eksp. profilya maksimal'na
    while (GetRFactor < FMaxRFactor) and (not FTerminated) do
    begin
        //  predyduschiy tsikl optimizatsii umen'shil fakt. rash.;
        //  sohranim parametry zdes'
        StoreCurveParams;
        ZerosDeleted := DeleteZeros;
        Deleted.Free;
        Deleted      := nil;
        PointDeleted := DeleteMaxDerivative(Deleted);

        if ZerosDeleted or PointDeleted then
        begin
            CalculateProfile;

            if GetRFactor > FMaxRFactor then
            begin
                Optimization;
                if GetRFactor > FMaxRFactor then
                begin
                    //  ne udalos' s pom. optimizatsii zagnat' fakt. rash. v
                    //  trebuemyy diapazon - vosst. posled. "horoshee" sostoyanie
                    //  !!! vosstanavlivaetsya tol'ko udalennaya krivaya
                    //  v tochke s maksimal'noy proizvodnoy - udalennye
                    //  krivye s nulevoy amplitudoy ne vosstanavlivayutsya !!!
                    RestoreCurveParams;
                    if PointDeleted then
                    begin
                        Assert(Assigned(Deleted));
                        if Deleted.Hasx0 then
                            AddPointToCurvePositions(Deleted.FInitx0);
                        FCurves.Add(Deleted);
                        Deleted      := nil;
                        PointDeleted := False;
                    end;
                    CalculateProfile;
                    ShowCurMin;     //  neobhodimo dlya sohraneniya
                    //  tekuschego znacheniya fakt. rash.
                    Break;
                end;
            end
            else
                ShowCurMin;    //  neobhodimo dlya sohraneniya
            //  tekuschego znacheniya fakt. rash.
        end
        else
            Break;
        if FCurves.Count <= 1 then
            Break;
    end;

    //  udalyaem iz spiska vydelennyh tochek te tochki,
    //  dlya kotoryh gaussiany imeyut nulevuyu amplitudu i
    //  te tochki, v kotoryh amplituda gaussianov minimal'na
    while (GetRFactor < FMaxRFactor) and (not FTerminated) do
    begin
        StoreCurveParams;
        ZerosDeleted := DeleteZeros;
        Deleted.Free;
        Deleted      := nil;
        PointDeleted := DeleteMin(Deleted);

        if ZerosDeleted or PointDeleted then
        begin
            CalculateProfile;

            if GetRFactor > FMaxRFactor then
            begin
                Optimization;
                if GetRFactor > FMaxRFactor then
                begin
                    //  vosst. posled. "horoshee" sostoyanie
                    RestoreCurveParams;
                    if PointDeleted then
                    begin
                        Assert(Assigned(Deleted));
                        if Deleted.Hasx0 then
                            AddPointToCurvePositions(Deleted.FInitx0);
                        FCurves.Add(Deleted);
                        Deleted      := nil;
                        PointDeleted := False;
                    end;

                    CalculateProfile;
                    ShowCurMin;     //  neobhodimo dlya sohraneniya
                    //  tekuschego znacheniya fakt. rash.
                    Break;
                end;
            end
            else
                ShowCurMin;    //  neobhodimo dlya sohraneniya
            //  tekuschego znacheniya fakt. rash.
        end
        else
            Break;
        if FCurves.Count <= 1 then
            Break;
    end;

    Deleted.Free;
    //SigmaVaryingDisabled := False;
    //Optimization;
end;

{$hints off}
procedure TFitTask.Optimization;
var
    ErrorCode: longint;
begin
    if FEnableFastMinimizer then
    begin
        CreateFastMinimizer;
        FMinimizer.Minimize(ErrorCode);
    end;

    CreateDHSMinimizer;
    FMinimizer.Minimize(ErrorCode);

    FMinimizer.Free;
    FMinimizer := nil;
end;

{$hints on}

procedure TFitTask.MinimizeDifference;
   //var    i: LongInt;
   //GP: TPointsSet;
begin
    // nachal'naya initsializatsiya neobhodima, kogda pri
    // vychislenii R-faktora predpolagaetsya, chto vse
    // tochki vychislennogo profilya ne d.b. ravny 0
    //Assert(Assigned(FCurves));
    //with FCurves do
    //  for i := 0 to Count - 1 do
    //  begin
    //      GP := TPointsSet(Items[i]);
    //      if GP is TGaussPointsSet then
    //          TGaussPointsSet(GP).Sigma := 0.6;
    //      TGaussPointsSet(GP).A := 100;
    //  end
    //CalculateProfile;

    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    Optimization;
    Done;
end;

procedure TFitTask.FindGaussesAgain;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    // povtornaya initsializatsiya gaussianov
    RecreateCurveInstances(nil);
    CalculateProfile;
    Optimization;
    Done;
end;

procedure TFitTask.MinimizeNumberOfCurves;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    FindGaussesSequentiallyAlg;
    Done;
end;

procedure TFitTask.AddPointToCurvePositions(XValue: double);
var
    Index: longint;
begin
    Assert(Assigned(FExpProfile));
    Assert(Assigned(FCurvePositions));
    Index := FExpProfile.IndexOfValueX(XValue);
    Assert(Index <> -1);
    FCurvePositions.AddNewPoint(XValue, FExpProfile.PointYCoord[Index]);
end;

//  udalyaet tochku s zadannym X iz spiska vybrannyh tochek
procedure TFitTask.DeletePoint(var Points: TPointsSet; XValue: double);
var
    j:    longint;
    Temp: TPointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    //  chtoby skopirovalis' vse parametry
    Temp := TPointsSet(Points.GetCopy);
    try
        Temp.Clear;
        for j := 0 to Points.PointsCount - 1 do
            if Abs(XValue - Points.PointXCoord[j]) > TINY then
                Temp.AddNewPoint(Points.PointXCoord[j], Points.PointYCoord[j])
        //if XValue <> Points.PointXCoord[j] then
        ;
        Points.Free;
        Points := nil;
        Points := Temp;
    except
        Temp.Free;
        raise;
    end;
end;

procedure TFitTask.SetProfilePointsSet(APointsSet: TPointsSet);
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(APointsSet));

    FExpProfile.Free;
    FExpProfile := nil;
    FExpProfile := APointsSet;
end;

procedure TFitTask.SetCurvePositions(ACurvePositions: TPointsSet);
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(ACurvePositions));

    FCurvePositions.Free;
    FCurvePositions := nil;
    FCurvePositions := ACurvePositions;
end;

function TFitTask.GetCurvePositions: TPointsSet;
begin
    Result := FCurvePositions;
end;

function TFitTask.GetCalcProfile: TPointsSet;
begin
    Result := FCalcProfile;
end;

function TFitTask.GetCurvesList: TSelfCopiedCompList;
begin
    Result := FCurves;
end;

function TFitTask.GetCurMin: double;
begin
    Result := FCurMin;
end;

function TFitTask.GetCurAbsMin: double;
begin
    Result := FCurAbsMin;
end;

function TFitTask.GetCurSqrMin: double;
begin
    Result := FCurSqrMin;
end;

function TFitTask.GetCurMinInitialized: boolean;
begin
    Result := FCurMinInitialized;
end;

function TFitTask.GetAllDone: boolean;
begin
    Result := FAllDone;
end;

procedure TFitTask.ShowCurMin;
begin
    //  !!! nuzhno pereschityvat' fakt. rash. potomu chto:
    //  1. vyvodimaya f-ya mozhet otlichat'sya ot toy, po kot.
    //  proizvoditsya optimizatsiya;
    //  2. parametry mogut izmenit'sya v rezul'tate raboty
    //  spets. algoritmov (naprimer udaleniya "lishnih" krivyh) !!!
    FCurSqrMin := GetSqrRFactor;
    FCurAbsMin := GetAbsRFactor;
    FCurMin    := FCurSqrMin;    //  chtoby ne pereschityvat'
    //  !!! dolzhno sootvetstvovat' GetRFactor !!!
    FCurMinInitialized := True;
    ServerShowCurMin;
end;

procedure TFitTask.Done;
begin
    FAllDone := True;
    ServerDoneProc;
end;

procedure TFitTask.SetSpecialCurve(ACurveExpr: string; AParams: Curve_parameters);
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    try
        Assert(Length(ACurveExpr) <> 0);
        Assert(Assigned(AParams));
    except
        on E: EAssertionFailed do
            WriteLog(E.Message, Warning);
        else
            raise;
    end;

    FCurveExpr := ACurveExpr;
    FParams.Free;
    FParams := AParams;
end;

procedure TFitTask.StopAsyncOper;
begin
    FTerminated := True;
    if Assigned(FMinimizer) then
        FMinimizer.Terminated := True;
end;

function TFitTask.GetScalingFactor: double;
var
    CalcProfileIntegral, ProfileIntegral: double;
begin
    if FCurveScalingEnabled then
    begin
        CalcProfileIntegral := GetCalcProfileIntegral;
        ProfileIntegral     := GetProfileIntegral;

        if (CalcProfileIntegral = 0) or (ProfileIntegral = 0) then
        begin
            Result := 1;
            Exit;
        end;
        Result := ProfileIntegral / CalcProfileIntegral;
    end
    else
        Result := 1;
end;

end.
