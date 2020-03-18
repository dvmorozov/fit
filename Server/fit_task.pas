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
    Classes, SysUtils, points_set, curve_points_set, self_copied_component,
    int_minimizer, simple_minimizer, downhill_simplex_minimizer,
    mscr_specimen_list, lorentz_points_set, gauss_points_set,
    two_branches_pseudo_voigt_points_set, asym_pseudo_voigt_points_set,
    user_points_set, pseudo_voigt_points_set, special_curve_parameter,
    persistent_curve_parameter_container, persistent_curve_parameters, log,
    curve_types_singleton, int_curve_type_selector, named_points_set;

type
    { Fits profile interval by model curves (specimens).
      Provides variable parameters and evaluation function for optimization
      algorithm.
      It is inherited from TComponent to allow inserting into TComponentList. }
    TFitTask = class(TComponent)
    protected
        FBegIndex: LongInt;
        FEndIndex: LongInt;
        { Enables curve scaling. Generally it should be true, otherwise
          optimization could stuck in local minimum. However it could be
          set to false for some special curve types. }
        FCurveScalingEnabled: Boolean;
        FMaxRFactor: Double;
        FCurveTypeSelector: ICurveTypeSelector;
        { Expression defining user curve type. }
        FCurveExpr: string;
        { Parameters of user defined curve. Parameters are given from the caller. 
          The object is used to construct curve instances. }
        Params: Curve_parameters;
        { Part of experimental profile corresponding to model interval. }
        ExpProfile: TPointsSet;
        { List of background points. }
        Background: TPointsSet;
        SavedBackground: TPointsSet;
        BackgroundWasSaved: Boolean;
        { The calculated profile. Every value is calculated as a sum of values 
          of corresponding points of every curve (specimen) and background. }
        CalcProfile: TPointsSet;
        { Contains positions of curves (specimens). Only X-coordinates are used. }
        CurvePositions: TPointsSet;
        { Set of curves (specimens) used to model experimental data inside given interval. }
        CurvesList: TSelfCopiedCompList;
        { The flag switches on using intervals in calculating R-factors.
          Using ranges is switched off when they are not given to accelerate computation. }
        UseCurveRanges: Boolean;
        { List of parameters of pattern instances (specimens) which are common for all the instances. }
        FCommonVariableParameters: Curve_parameters;
        { Background parameters. }
        A, B, C, x0: Double;

        //  ======================= dannye dlya optimizatora ====================
        FMinimizer: TMinimizer;

        { Index of pattern instance (specimen) parameters of which are variated at the moment. }
        CurveNum: LongInt;
        { Index of parameter of pattern instance which is variated at the moment. }
        ParamNum: LongInt;
        EOC: Boolean;
        { Flag signalling to terminate all internal loops. }
        FTerminated: Boolean;
        { Index of common parameter which is variated at the moment. }
        CommonVaryingIndex: LongInt;
        { Index of background point amplitude of which is variated at the moment. }
        BackgroundVaryingIndex: LongInt;
        { Flag indicating that common parameters are variated at the moment. }
        CommonVaryingFlag: Boolean;
        { Flag indicating that amplitudes of background points are variated at the moment. }
        BackgroundVaryingFlag: Boolean;
        { Enables background variation. }
        FEnableBackgroundVariation: Boolean;
        { Enables fast optimization algorithm. }
        FEnableFastMinimizer: Boolean;

        FShowCurMin: TShowCurMin;
        FDoneProc: TThreadMethod;
        function GetProfileIntegral: Double;
        function GetCalcProfileIntegral: Double;

        { Methods which are used by the optimizer. }
        
        { Calculates R-factor. }
        function Func: Double;
        { Computes evaluation function. }
        procedure CalcFunc;
        { Returns initial variation step for current variable parameter. }
        function GetStep: Double;
        { Does nothing. Should be implemented because is used by pointer.
          See OnSetStep. }
        procedure SetStep(NewStepValue: Double);
        { Moves iteration to next variable parameter. }
        procedure SetNextParam;
        { Sets iteration to the first variable parameter. }
        procedure SetFirstParam;
        { Returns variable parameter value. }
        function GetParam: Double;
        { Sets variable parameter value. }
        procedure SetParam(NewParamValue: Double);
        { Returns True at the end of iteration cycle. }
        function EndOfCycle: Boolean;
        { Divides all optimization steps by 2. Iterface method for TSimpleMinimizer2. }
        procedure DivideStepsBy2;
        procedure MultipleSteps(Factor: Double);
        { Returns flag indicating termination of the calculation. }
        function EndOfCalculation: Boolean;
        { Calculates R-factor used for optimization. }
        function GetOptimizingRFactor: Double;
        { Calculates R-factor used for comparison with maximal acceptable value. }
        function GetRFactor: Double;

    protected
        { Current minimum value of R-factor by which maximal acceptable value is set up. 
          Last achived minimum value is stored to avoid redundant computations and locks
          in multithreaded environment. }
        CurMin: Double;
        CurSqrMin: Double;
        CurAbsMin: Double;
        CurMinInitialized: Boolean;
        { Flag indicating that asynchronous operation executed as subtask was Terminated.
          For this class is always True for now because the class does not support asynchronous operations. }
        FAllDone: Boolean;

        procedure ShowCurMin; virtual;
        procedure Done; virtual;

        function GetSqrRFactor: Double;
        function GetAbsRFactor: Double;

        { Algorithms are methods executed asynchronously. }
        
        { Searches for set of curves fitting experimental profile with given accuracy
          sequentially decreasing number of curves. }
        procedure FindGaussesSequentiallyAlg;
        { Executes cycle of fitting of parameters of curves. }
        procedure Optimization;

        { Low-level methods of algorithms. }
        
        procedure StoreCurveParams;
        procedure RestoreCurveParams;
        { Sums all pattern instances and background into single calculated profile. }
        procedure CalcGaussSum;
        procedure AddCurveToProfile(PS: TPointsSet);
        procedure SubbCurveFromProfile(PS: TPointsSet);
        { Removes from list of curve positions those points
          for which calculated curves have zero amplitude. }
        function DeleteZeros: Boolean;
        { Deletes from list of curve positions the point
          in which amplitude of curve is minimal. }
        function DeleteMin(var Deleted: TCurvePointsSet): Boolean;
        { Removes from list of curve positions the point
          in which experimental profile has maximal derivative.  }
        function DeleteMaxDerivative(var Deleted: TCurvePointsSet): Boolean;

        { Auxiliary methods. }
        { Deletes poins with given X from the list passed via parameter. }
        procedure DeletePoint(var Points: TPointsSet; XValue: Double);
        procedure AddPointToCurvePositions(XValue: Double);
        { Creates fast optimization algorithm for the 1th step of 2-stage processing,
          by default is disabled.
          TODO: Make configurable. }
        procedure CreateFastMinimizer;
        { Creates downhill simplex algorithm for the 2nd step of 2-stage processing. }
        procedure CreateDHSMinimizer;
        { Calculates hash of initial values of parameters of pattern instance. }
        procedure CalcInitHash(Specimen: TCurvePointsSet);
        function CreatePatternInstance: TCurvePointsSet;
        function MinimumStepAchieved: Boolean;
        procedure InitializeVariationSteps;

    public
        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: Boolean;
            ACurveScalingEnabled: Boolean); virtual;
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
        { Returns current minimal achived value of R-factor (CurMin). }
        function GetCurMin: Double; virtual;
        function GetCurAbsMin: Double; virtual;
        function GetCurSqrMin: Double; virtual;
        function GetCurMinInitialized: Boolean; virtual;
        function GetAllDone: Boolean; virtual;
        
        procedure SetSpecialCurve(
            ACurveExpr: string; AParams: Curve_parameters);
        { Recreates pattern instances (curves). It should be public
          for initial calculation of R-factor for overall profile. }
        procedure RecreateCurveInstances(SpecimenParameters: TMSCRSpecimenList);
        { Searches pattern specimen by hash and sets its parameters 
          from the given list. }
        procedure SearchSpecimenAndInit(
            SpecimenParameters: TMSCRSpecimenList; Specimen: TCurvePointsSet);
        { Recalculates all pattern instances and background. 
          Calculates resulting profile. }
        procedure CalculateProfile;

        { Fits pattern specimens starting from given parameter set (initially or repeatedly). }
        procedure FindGausses; virtual;
        procedure FindGaussesAgain; virtual;
        { Searches set of pattern specimens (curves) fitting exprerimental data
          with given accuracy sequentially decreasing number of curves. }
        procedure FindGaussesSequentially; virtual;
        { Sets up termination flags and returns. }
        procedure StopAsyncOper; virtual;
        { Returns the factor scaling calculated points up to scale of experimental data. }
        function GetScalingFactor: Double;

        property MaxRFactor: Double write FMaxRFactor;
        { Callback to update information at achieving new minimum. }
        property ServerShowCurMin: TThreadMethod read FShowCurMin write FShowCurMin;
        property ServerDoneProc: TThreadMethod read FDoneProc write FDoneProc;
        { Attributes store indexes of begin and end of the task interval 
          for optimal rebuilding overall resulting profile. }
        property BegIndex: LongInt read FBegIndex write FBegIndex;
        property EndIndex: LongInt read FEndIndex write FEndIndex;
    end;

    { The wrapper for future OpenCL implementation. }
    TOpenCLFitTask = class(TComponent)
    public
    end;

implementation

uses app, SimpMath, GeneralHashFunctions;

{================================== TFitTask ==================================}

function TFitTask.Func: Double;
begin
    Result := GetOptimizingRFactor;
end;

procedure TFitTask.CalcFunc;
begin
    //  krivye ne nuzhno pereschityvat', poskol'ku vse uzhe
    //  pereschitano v SetParam
    CalculateProfile;
end;

function TFitTask.GetCalcProfileIntegral: Double;
var i: LongInt;
begin
    Assert(Assigned(CalcProfile));
    Result := 0;
    for i := 0 to CalcProfile.PointsCount - 1 do
        Result := Result + CalcProfile.PointYCoord[i];
end;

function TFitTask.GetProfileIntegral: Double;
var i: LongInt;
begin
    Assert(Assigned(ExpProfile));
    Result := 0;
    for i := 0 to ExpProfile.PointsCount - 1 do
        Result := Result + ExpProfile.PointYCoord[i];
end;

function TFitTask.GetOptimizingRFactor: Double;
begin
    Result := GetSqrRFactor;
    //Result := GetAbsRFactor;
end;

function TFitTask.GetAbsRFactor: Double;
var CPS: TCurvePointsSet;
    i, j: LongInt;
    RFactor: Double;
    Flag: Boolean;
    RangeDefined: Boolean;
    ScalingFactor: Double;
    CalcProfileIntegral: Double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CalcProfile));
    Assert(Assigned(ExpProfile));

    Assert(Assigned(CurvesList));
    //  esli ni u odnoy krivoy diapazon ne zadan,
    //  to R-faktor schitaetsya po vsemu profilyu
    RangeDefined := False;
    if UseCurveRanges then
    begin
        for j := 0 to CurvesList.Count - 1 do
        begin
            CPS := TCurvePointsSet(CurvesList.Items[j]);
            if CPS.RangeDefined then
            begin
                RangeDefined := True;
                Break;
            end;
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
    for i := 0 to CalcProfile.PointsCount - 1 do
    begin
        //  proveryaetsya prinadlezhnost' tochki diapazonu krivoy
        if RangeDefined then
        begin
            Flag := False;      //  tochka ne prinadlezhit nikakomu diapazonu
            for j := 0 to CurvesList.Count - 1 do
            begin
                CPS := TCurvePointsSet(CurvesList.Items[j]);
                if CPS.RangeDefined and
                   (CalcProfile.PointXCoord[i] >= CPS.MinX) and
                   (CalcProfile.PointXCoord[i] <= CPS.MaxX) then
                begin
                    Flag := True;
                    Break;
                end;
            end;
        end
        else Flag := True;      //  schitaem po vsem tochkam

        if Flag then            //  tochka vklyuchena v raschet R-faktora
        begin
            RFactor := RFactor +
                Abs(CalcProfile.PointYCoord[i] * ScalingFactor -
                    ExpProfile.PointYCoord[i]);
        end;
    end;
    RFactor := RFactor / CalcProfileIntegral;
    Result := RFactor;
end;

function TFitTask.GetRFactor: Double;
begin
    Result := GetSqrRFactor;
    //  !!! pri izmenenii tipa nuzhno ispravit' formulu v interfeyse,
    //  a takzhe sohranyaemoe v CurMin znachenie (sm. ShowCurMin) !!!
    //Result := GetAbsRFactor;
end;

function TFitTask.GetSqrRFactor: Double;
var CPS: TCurvePointsSet;
    i, j: LongInt;
    RFactor: Double;
    Flag: Boolean;
    RangeDefined: Boolean;
    ScalingFactor: Double;
    CalcProfileIntegral: Double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CalcProfile));
    Assert(Assigned(ExpProfile));

    Assert(Assigned(CurvesList));
    //  esli ni u odnoy krivoy diapazon ne zadan,
    //  to R-faktor schitaetsya po vsemu profilyu
    RangeDefined := False;
    if UseCurveRanges then
    begin
        for j := 0 to CurvesList.Count - 1 do
        begin
            CPS := TCurvePointsSet(CurvesList.Items[j]);
            if CPS.RangeDefined then
            begin
                RangeDefined := True;
                Break;
            end;
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
    for i := 0 to CalcProfile.PointsCount - 1 do
    begin
        //  proveryaetsya prinadlezhnost' tochki diapazonu krivoy
        if RangeDefined then
        begin
            Flag := False;      //  tochka ne prinadlezhit nikakomu diapazonu
            for j := 0 to CurvesList.Count - 1 do
            begin
                CPS := TCurvePointsSet(CurvesList.Items[j]);
                if CPS.RangeDefined and
                   (CalcProfile.PointXCoord[i] >= CPS.MinX) and
                   (CalcProfile.PointXCoord[i] <= CPS.MaxX) then
                begin
                    Flag := True;
                    Break;
                end;
            end;
        end
        else Flag := True;      //  schitaem po vsem tochkam

        if Flag then            //  tochka vklyuchena v raschet R-faktora
        begin
            RFactor := RFactor +
                Sqr(CalcProfile.PointYCoord[i] * ScalingFactor -
                    ExpProfile.PointYCoord[i]);
        end;
    end;
    RFactor := RFactor / Sqr(CalcProfileIntegral);
    Result := RFactor;
end;

function TFitTask.GetStep: Double;
var Curve: TCurvePointsSet;
begin
    if FEnableBackgroundVariation and BackgroundVaryingFlag then
    begin
        //  TODO: move into separate "background" class.
        Result := 0.1;
    end
    else
    if CommonVaryingFlag then
    begin
        Result := FCommonVariableParameters[CommonVaryingIndex].VariationStep;
    end
    else
    begin
        Curve := TCurvePointsSet(CurvesList.Items[CurveNum]);
        Result := Curve.VariationSteps[ParamNum];
    end;
end;

{$hints off}
procedure TFitTask.SetStep(NewStepValue: Double);
begin

end;
{$hints on}

procedure TFitTask.SetNextParam;
var Curve: TCurvePointsSet;
    Count: LongInt;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Assigned(FCommonVariableParameters));

    EOC := True;
    if CurvesList.Count <> 0 then
    begin
        //  perebor parametrov krivoy
        Curve := TCurvePointsSet(CurvesList.Items[CurveNum]);
        if ParamNum < Curve.VariableCount - 1 then
        begin
            Inc(ParamNum);
            EOC := False;
            Exit;
        end;
    end;

    if CurveNum < CurvesList.Count - 1 then
    begin
        //  perebor krivyh
        Inc(CurveNum);
        EOC := False;
        ParamNum := 0;
        Exit;
    end;

    Count := FCommonVariableParameters.Params.Count;
    if CommonVaryingFlag then
    begin
        //  poisk sleduyuschego obschego parametra,
        //  variatsiya kotorogo ne zapreschena
        if CommonVaryingIndex < Count then
        begin
            Inc(CommonVaryingIndex);
            while (CommonVaryingIndex <> Count) and
                   FCommonVariableParameters[CommonVaryingIndex].VariationDisabled do
                Inc(CommonVaryingIndex);
        end;
    end;
    
    if CommonVaryingIndex < Count then
    begin
        EOC := False;
        CommonVaryingFlag := True;
        Exit;
    end;

    if FEnableBackgroundVariation then
    begin
        if BackgroundVaryingFlag then
        begin
            //  Increments parameter index for next iteration.
            Inc(BackgroundVaryingIndex);
        end;

        if BackgroundVaryingIndex < //Background.PointsCount
            4 then
        begin
            //  There are still next variable background parameters.
            BackgroundVaryingFlag := True;
            EOC := False;
            Exit;
        end;
    end;
end;

procedure TFitTask.SetFirstParam;
var i: LongInt;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CurveNum := 0;
    ParamNum := 0;
    EOC := False;
    //  poisk pervogo parametra razreschennogo k variatsii
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
    begin
        if not FCommonVariableParameters[i].VariationDisabled then Break;
    end;
    CommonVaryingIndex := i;
    CommonVaryingFlag := False;
    BackgroundVaryingIndex := 0;
    BackgroundVaryingFlag := False;
    BackgroundWasSaved := False;
end;

function TFitTask.GetParam: Double;
var GP: TCurvePointsSet;
    Parameter: TSpecialCurveParameter;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Assigned(FCommonVariableParameters));
    
    if FEnableBackgroundVariation and BackgroundVaryingFlag then
    begin
        Assert(BackgroundVaryingIndex < (*Background.PointsCount*)4);
        //Result := Background.PointYCoord[BackgroundVaryingIndex];
        case(BackgroundVaryingIndex) of
            0: Result := A;
            1: Result := B;
            2: Result := C;
            3: Result := x0;
        end;
    end
    else
    if CommonVaryingFlag then
    begin
        Assert(CommonVaryingIndex < FCommonVariableParameters.Params.Count);
        Parameter := FCommonVariableParameters[CommonVaryingIndex];
        Result := Parameter.Value;
    end
    else
    begin
        Assert(CurvesList.Count <> 0);

        GP := TCurvePointsSet(CurvesList.Items[CurveNum]);
        Result := GP.VariableValues[ParamNum];
    end;
end;

procedure TFitTask.SetParam(NewParamValue: Double);
var GP: TCurvePointsSet;
    i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Assigned(FCommonVariableParameters));
    
    if FEnableBackgroundVariation and BackgroundVaryingFlag then
    begin
        Assert(BackgroundVaryingIndex < (*Background.PointsCount*)4);
        //Background.PointYCoord[BackgroundVaryingIndex] := NewParamValue;
        Assert(SavedBackground.PointsCount = Background.PointsCount);
        if not BackgroundWasSaved then
        begin
            for i := 0 to SavedBackground.PointsCount - 1 do
                SavedBackground.PointYCoord[i] := Background.PointYCoord[i];
            BackgroundWasSaved := True;
        end;
        case(BackgroundVaryingIndex) of
            0: A := Abs(NewParamValue);
            1: B := NewParamValue;
            2: C := Abs(NewParamValue);
            3: x0 := NewParamValue;
        end;
    end
    else
    if CommonVaryingFlag then
    begin
        Assert(CommonVaryingIndex < FCommonVariableParameters.Params.Count);
        Parameter := FCommonVariableParameters[CommonVaryingIndex];
        Parameter.Value := NewParamValue;

        //  ustanovka znacheniya obschego parametra u vseh ekzemplyarov
        for i := 0 to CurvesList.Count - 1 do
        begin
            GP := TCurvePointsSet(CurvesList.Items[i]);
            GP.ValuesByName[
                FCommonVariableParameters[CommonVaryingIndex].Name
            ] := NewParamValue;
        end;
        //CalculateProfile;
    end
    else
    begin
        Assert(CurvesList.Count <> 0);
        //  takoy algoritm mog by privodit' k nakopleniyu oshibki
        //  v summarnom profile (kogda znachenie intensivnosti
        //  summarnogo profilya otlichaetsya ot summy intensivnostey
        //  vseh krivyh), no pri variatsii Sigma ispol'zuetsya
        //  polnyy pereschet, poetomu nakopleniya ne proishodit
        GP := TCurvePointsSet(CurvesList.Items[CurveNum]);
        //  ??? v nekotoryh sluchayah rabotaet optimal'nee
        //SubbCurveFromProfile(GP);
        GP.VariableValues[ParamNum] := NewParamValue;
        //GP.ReCalc(nil);
        //AddCurveToProfile(GP);
    end;
end;

function TFitTask.EndOfCycle: Boolean;
begin
    Result := EOC;
end;

procedure TFitTask.DivideStepsBy2;
begin
    MultipleSteps(0.99);
end;

procedure TFitTask.MultipleSteps(Factor: Double);
var i: LongInt;
begin
    Assert(Assigned(FCommonVariableParameters));
    
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
    begin
        FCommonVariableParameters[i].MultiplyVariationStep(Factor);
    end;
end;

function TFitTask.EndOfCalculation: Boolean;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Result := False;
    if (FMinimizer.CurrentMinimum < FMaxRFactor) then
    begin
        //OutputDebugString(PChar('Desired R-factor achived...'));
        Result := True;
    end
    else
    begin
        if MinimumStepAchieved then
        begin
            //OutputDebugString(PChar('Minimumu step achived...'));
            Result := True;
        end;
    end;
end;

constructor TFitTask.Create(AOwner: TComponent;
    AEnableBackgroundVariation: Boolean; ACurveScalingEnabled: Boolean);
begin
    inherited Create(AOwner);
    FCommonVariableParameters := Curve_parameters.Create(nil);
    FCommonVariableParameters.Params.Clear;     //  Curve_parameters sozdaet v konstruktore
                                                //  odin parametr - nuzhno ego udalit'
    FMaxRFactor := 0.01;
    FAllDone := False;
    //  Sets default curve type
    FCurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    FEnableBackgroundVariation := AEnableBackgroundVariation;
    FEnableFastMinimizer := False;
    FCurveScalingEnabled := ACurveScalingEnabled;
end;

destructor TFitTask.Destroy;
begin
    ExpProfile.Free; ExpProfile := nil;
    CurvesList.Free; CurvesList := nil;
    CalcProfile.Free; CalcProfile := nil;
    Background.Free; Background := nil;
    SavedBackground.Free; SavedBackground := nil;
    CurvePositions.Free; CurvePositions := nil;
    FMinimizer.Free; FMinimizer := nil;
    Params.Free;
    FCommonVariableParameters.Free;
    inherited;
end;

procedure TFitTask.CalculateProfile;
var i: LongInt;
    Curve: TCurvePointsSet;
    RestoreBackground: Boolean;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Background.PointsCount = SavedBackground.PointsCount);
    Assert(ExpProfile.PointsCount = SavedBackground.PointsCount);

    for i := 0 to CurvesList.Count - 1 do
    begin
        Curve := TCurvePointsSet(CurvesList.Items[i]);
        Curve.ReCalc(nil);
    end;
    //  raschet tochek fona
    RestoreBackground := False;
    for i := 0 to Background.PointsCount - 1 do
    begin
        Background.PointYCoord[i] :=
            CalcPolinom2(A, B, C, x0, Background.PointXCoord[i]);
        if (Background.PointYCoord[i] > ExpProfile.PointYCoord[i]) or
           (Background.PointYCoord[i] < 0) then
        begin
            RestoreBackground := True;
            Break;
        end;
    end;
    if RestoreBackground then
    begin
        for i := 0 to Background.PointsCount - 1 do
            Background.PointYCoord[i] := SavedBackground.PointYCoord[i];
    end else BackgroundWasSaved := False;

    CalcGaussSum;
end;

procedure TFitTask.StoreCurveParams;
var i: LongInt;
    PS: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));

    for i := 0 to CurvesList.Count - 1 do
    begin
        PS := TCurvePointsSet(CurvesList.Items[i]);
        PS.StoreParams;
    end;
end;

procedure TFitTask.RestoreCurveParams;
var i: LongInt;
    PS: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));

    for i := 0 to CurvesList.Count - 1 do
    begin
        PS := TCurvePointsSet(CurvesList.Items[i]);
        PS.RestoreParams;
    end;
end;

procedure TFitTask.CalcGaussSum;
var i: LongInt;
    PS: TPointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CalcProfile));
    Assert(Assigned(Background));
    Assert(Assigned(CurvesList));
    Assert(Background.PointsCount = CalcProfile.PointsCount);

    //  obnulenie profilya
    for i := 0 to CalcProfile.PointsCount - 1 do
        CalcProfile.PointYCoord[i] := 0;

    //  vychislenie novogo
    for i := 0 to CurvesList.Count - 1 do
    begin
        PS := TPointsSet(CurvesList.Items[i]);
        AddCurveToProfile(PS);
    end;
    //  dobavleniye fona
    for i := 0 to Background.PointsCount - 1 do
        CalcProfile.PointYCoord[i] :=
            CalcProfile.PointYCoord[i] + Background.PointYCoord[i];
end;

procedure TFitTask.AddCurveToProfile(PS: TPointsSet);
var j: LongInt;
begin
    for j := 0 to CalcProfile.PointsCount - 1 do
        CalcProfile.PointYCoord[j] :=
            CalcProfile.PointYCoord[j] + PS.PointYCoord[j];
end;

procedure TFitTask.SubbCurveFromProfile(PS: TPointsSet);
var j: LongInt;
begin
    for j := 0 to CalcProfile.PointsCount - 1 do
        CalcProfile.PointYCoord[j] :=
            CalcProfile.PointYCoord[j] - PS.PointYCoord[j];
end;

procedure TFitTask.InitializeVariationSteps;
var
    i, j: LongInt;
    Curve: TCurvePointsSet;
begin
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
    begin
        FCommonVariableParameters[i].InitVariationStep;
    end;

    for i := 0 to CurvesList.Count - 1 do
    begin
        Curve := TCurvePointsSet(CurvesList.Items[i]);
        for j := 0 to Curve.VariableCount - 1 do
        begin
            Curve.InitVariationStep(j);
        end;
    end;
end;

function TFitTask.MinimumStepAchieved: Boolean;
var
    i, j: LongInt;
    Curve: TCurvePointsSet;
begin
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
    begin
        if not FCommonVariableParameters[i].MinimumStepAchieved then
        begin
            Result := False;
            Exit;
        end;
    end;

    for i := 0 to CurvesList.Count - 1 do
    begin
        Curve := TCurvePointsSet(CurvesList.Items[i]);
        for j := 0 to Curve.VariableCount - 1 do
        begin
            if not Curve.MinimumStepAchieved(j) then
            begin
                Result := False;
                Exit;
            end;
        end;
    end;
    Result := True;
end;

procedure TFitTask.CreateFastMinimizer;
begin
    FMinimizer.Free; FMinimizer := nil;
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

    TSimpleMinimizer3(FMinimizer).EndOfCalculation := EndOfCalculation;
    TSimpleMinimizer3(FMinimizer).MultipleSteps := MultipleSteps;

    InitializeVariationSteps;

    EOC := False;
    ParamNum := 0;
    CurveNum := 0;
    CommonVaryingFlag := False;
    CommonVaryingIndex := 0;
    BackgroundVaryingFlag := False;
    BackgroundVaryingIndex := 0;
    BackgroundWasSaved := False;
end;

procedure TFitTask.CreateDHSMinimizer;
begin
    FMinimizer.Free; FMinimizer := nil;
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

    EOC := False;
    ParamNum := 0;
    CurveNum := 0;
    CommonVaryingFlag := False;
    CommonVaryingIndex := 0;
    BackgroundVaryingFlag := False;
    BackgroundVaryingIndex := 0;
    BackgroundWasSaved := False;
end;

//  udalyaet iz spiska vydelennyh tochek te tochki,
//  dlya kotoryh gaussiany imeyut nulevuyu intensivnost'
function TFitTask.DeleteZeros: Boolean;
var i, j: LongInt;
    GP: TCurvePointsSet;
    MaxA: Double;       //     maks. amplituda krivoy
//  konstanta, men'she kotoroy vse otnositel'nye
//  amplitudy schitayutsya ravnymi nulyu
const ZeroConst: Double = 0.001;    //  0.1%
begin
    Result := False;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    MaxA := 0;
    for i := 0 to CurvesList.Count - 1 do
    begin
        GP := TCurvePointsSet(CurvesList.Items[i]);
        if not GP.HasA then Exit;

        Assert(GP.A >= 0);
        if GP.A > MaxA then MaxA := GP.A;
    end;
    //  esli posle tsikla optimizatsii MaxA = 0 eto
    //  oznachaet, chto model' sovershenno ne sootvetstvuet
    //  dannym, poetomu vse krivye mozhno udalit'
    i := 0;
    while i < CurvesList.Count do
    begin
        GP := TCurvePointsSet(CurvesList.Items[i]);
        if not GP.HasA then Exit;
        
        Assert(GP.A >= 0);
        if (MaxA = 0) or (GP.A / MaxA < ZeroConst) then
        begin
            //  udalyaetsya sootvetstvuyuschaya tochka iz
            //  spiska vydelennyh tochek
            for j := 0 to CurvePositions.PointsCount - 1 do
            begin
                //if CurvePositions.PointXCoord[j] = GP.Initx0 then
                if Abs(CurvePositions.PointXCoord[j] - GP.Initx0) <= TINY then
                begin
                    DeletePoint(CurvePositions, GP.Initx0);
                    Result := True;
                    Break;
                end;
            end;
            CurvesList.Remove(GP);  //  osvobozhdaet GP
        end else Inc(i);
    end;
end;

function TFitTask.DeleteMaxDerivative(var Deleted: TCurvePointsSet): Boolean;
var Der, MaxDer: Double;
    First: Boolean;
    MaxGP, GP: TCurvePointsSet;
    i, Index: LongInt;
    SA: TPointsSet;
begin
    Result := False;
    Deleted := nil;
    MaxGP := nil;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(CurvesList.Count <> 0);
    if CurvesList.Count <= 1 then Exit;
    Assert(Assigned(CurvePositions));
    SA := ExpProfile;
    Assert(Assigned(SA));
    if ExpProfile.PointsCount <= 1 then Exit;

    First := True;
    for i := 0 to CurvesList.Count - 1 do
    begin
        GP := TCurvePointsSet(CurvesList.Items[i]);
        if not GP.Hasx0 then Exit;
        
        if First then
        begin
            //  pervyy raz nahodim proizvodnuyu
            Index := ExpProfile.IndexOfValueX(GP.Initx0);
            Assert(Index <> -1);
            if Index = ExpProfile.PointsCount - 1 then Exit;

            MaxDer := Abs(ExpProfile.PointYCoord[Index + 1] -
                ExpProfile.PointYCoord[Index]);
            MaxGP := GP;
            First := False;
        end
        else
        begin
            //  !!! etot algoritm ne mozhet vychislit' proizvodnuyu
            //  v posledney tochke profilya, a znachit i udalit'
            //  vposledstvii takuyu tochku !!!
            Index := ExpProfile.IndexOfValueX(GP.Initx0);
            Assert(Index <> -1);
            if Index = ExpProfile.PointsCount - 1 then Break;

            Der := Abs(ExpProfile.PointYCoord[Index + 1] -
                ExpProfile.PointYCoord[Index]);

            if Der > MaxDer then
            begin
                MaxDer := Der;
                MaxGP := GP;
            end;
        end;
    end;
    Assert(Assigned(MaxGP));
    DeletePoint(CurvePositions, MaxGP.Initx0);
    Deleted := CurvesList.Extract(MaxGP);
    Result := True;
end;

//  udalyaet iz spiska vydelennyh tochek tu,
//  u kotoroy amplituda krivoy minimal'na
function TFitTask.DeleteMin(var Deleted: TCurvePointsSet): Boolean;
var Min: Double;
    First: Boolean;
    MinGP, GP: TCurvePointsSet;
    i: LongInt;
begin
    Result := False;
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(CurvesList.Count <> 0);
    if CurvesList.Count <= 1 then Exit;

    First := True;
    for i := 0 to CurvesList.Count - 1 do
    begin
        GP := TCurvePointsSet(CurvesList.Items[i]);
        if not GP.HasA then Exit;
        
        Assert(GP.A >= 0);
        if First then
        begin
            Min := GP.A;
            MinGP := GP;
            First := False;
        end
        else
        begin
            if GP.A < Min then
            begin
                Min := GP.A;
                MinGP := GP;
            end;
        end;
    end;
    Assert(Assigned(MinGP));
    DeletePoint(CurvePositions, MinGP.Initx0);
    Deleted := CurvesList.Extract(MinGP);
    Result := True;
end;

procedure TFitTask.CalcInitHash(Specimen: TCurvePointsSet);
var i: LongInt;
    Parameter: TPersistentCurveParameterContainer;
    Value: string;
begin
    Assert(Assigned(Specimen));
    Specimen.InitHash := 0;
    for i := 0 to Specimen.Parameters.Params.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(
            Specimen.Parameters.Params.Items[i]);
        Value := Parameter.Value_;
        Specimen.InitHash := Specimen.InitHash + JSHash(Value);
    end;
end;

procedure TFitTask.SearchSpecimenAndInit(
    SpecimenParameters: TMSCRSpecimenList; Specimen: TCurvePointsSet);
var i, j, k: LongInt;
    CurveParameters: Curve_parameters;
    Parameter, Parameter2: TSpecialCurveParameter;
begin
    //Assert(Assigned(SpecimenParameters));
    //  ravenstvo nil ne protivorechit rasschirennoy semantike metoda; krome
    //  togo neobhodimo dopustit' vozmozhnost' takogo znacheniya pri vyzove
    //  iz drugih metodov klassa
    if not Assigned(SpecimenParameters) then Exit;
    Assert(Assigned(Specimen));
    
    for i := 0 to SpecimenParameters.Count - 1 do
    begin
        CurveParameters := Curve_parameters(SpecimenParameters.Items[i]);
        
        if CurveParameters.SavedInitHash = Specimen.InitHash then
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
var i: LongInt;
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
    SelectedCurveTypeId: TCurveTypeId;
begin
    SelectedCurveTypeId := FCurveTypeSelector.GetSelectedCurveType;
    if IsEqualGUID(SelectedCurveTypeId, TLorentzPointsSet.GetCurveTypeId) then
    begin
        Result := TLorentzPointsSet.Create(nil);
    end
    else
    if IsEqualGUID(SelectedCurveTypeId, TGaussPointsSet.GetCurveTypeId) then
    begin
        Result := TGaussPointsSet.Create(nil)
    end
    else
    if IsEqualGUID(SelectedCurveTypeId, TPseudoVoigtPointsSet.GetCurveTypeId) then
    begin
        Result := TPseudoVoigtPointsSet.Create(nil)
    end
    else
    if IsEqualGUID(SelectedCurveTypeId, TAsymPseudoVoigtPointsSet.GetCurveTypeId) then
    begin
        Result := TAsymPseudoVoigtPointsSet.Create(nil)
    end
    else
{$IFDEF _WINDOWS}
    if IsEqualGUID(SelectedCurveTypeId, TUserPointsSet.GetCurveTypeId) then
    begin
        Result := TUserPointsSet.Create(nil);
        TUserPointsSet(Result).Expression := FCurveExpr;
        TUserPointsSet(Result).SetParameters(
            Curve_parameters(Params.GetCopy));
    end
    else
{$ENDIF}
    if IsEqualGUID(SelectedCurveTypeId, T2BranchesPseudoVoigtPointsSet.GetCurveTypeId) then
    begin
        Result := T2BranchesPseudoVoigtPointsSet.Create(nil);
    end;
    
    if FCommonVariableParameters.Count = 0 then
    begin
        //  Initializing list of common parameters. It is performed only
        //  once when the first curve instance is created (it is assumed
        //  that all the instances have the same type).
        //  TODO: remove the assumption mentioned above.
        for i := 0 to Result.Parameters.Count - 1 do
        begin
            if (Result.Parameters[i].Type_ = Shared) and (not
                Result.Parameters[i].VariationDisabled) then
            begin
                Parameter := Result.Parameters[i].CreateCopy;
                Parameter.InitValue;
                Parameter.InitVariationStep;

                try
                    Container := TPersistentCurveParameterContainer(
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
            end;
        end;
    end;
    
    for i := 0 to Result.Parameters.Count - 1 do
    begin
        Result.Parameters[i].InitValue;
        Result.Parameters[i].InitVariationStep;
    end;
end;

procedure TFitTask.RecreateCurveInstances(SpecimenParameters: TMSCRSpecimenList);
var i,j,k: LongInt;
    Curve: TCurvePointsSet;
    CurveFound: Boolean;
    //  koordinaty tochki privyazki sozdavaemogo avtomaticheski
    //  ekz. patterna, kogda pol'zovatel'skie tochki privyazki
    //  ne zadany
    //CurvePosition,
    CurveAmplitude: Double;
    SelectedCurveTypeId: TCurveTypeId;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvePositions));
    Assert(Assigned(ExpProfile));
    Assert(ExpProfile.PointsCount >= 2);

    //  Saves previously created curve instances.
    if not Assigned(CurvesList) then
        CurvesList := TSelfCopiedCompList.Create(nil);

    //  sozdaem zanovo summarnyy profil'
    if Assigned(CalcProfile) then CalcProfile.Clear
    else CalcProfile := TPointsSet.Create(nil);
    //  kol-vo tochek profilya ustanavlivaetsya ravnym kol-vu tochek uchastka
    for i := 0 to ExpProfile.PointsCount - 1 do
        CalcProfile.AddNewPoint(ExpProfile.PointXCoord[i], 0);
    //  sozdaem zanovo massiv tochek fona
    if Assigned(Background) then Background.Clear
    else Background := TPointsSet.Create(nil);
    for i := 0 to ExpProfile.PointsCount - 1 do
        Background.AddNewPoint(ExpProfile.PointXCoord[i], 0);

    if Assigned(SavedBackground) then SavedBackground.Clear
    else SavedBackground := TPointsSet.Create(nil);
    for i := 0 to ExpProfile.PointsCount - 1 do
        SavedBackground.AddNewPoint(ExpProfile.PointXCoord[i], 0);

    //  proveryaem i udalyaem te ekz. patterna,
    //  polozheniya kot. net sredi vybrannyh tochek
    k := 0;
    while k < CurvesList.Count do
    begin
        CurveFound := False;
        Curve := TCurvePointsSet(CurvesList.Items[k]);
        //  esli pattern ne imeet parametra polozheniya, to
        //  ego ekzemplyary ne udalyayutsya
        if not Curve.Hasx0 then Break;
        
        for i := 0 to CurvePositions.PointsCount - 1 do
        begin
            if Curve.Initx0 = CurvePositions.PointXCoord[i] then
            begin
                CurveFound := True;
                Break;
            end;
        end;

        if not CurveFound then
        begin // udalyaem
            //  CurvesList po-umolchaniyu osvobozhdaet
            //  komponenty, ssylki na kotorye hranit
            CurvesList.Remove(Curve);
        end
        else Inc(k);
    end;
    //  uslovie obratnoe dannomu oznachaet, chto spisok krivyh ne
    //  pust, i pri etom pattern ne imeet parametra polozheniya;
    //  v takom sluchae nichego delat' ne nuzhno...
    if (CurvesList.Count = 0) or
        (TCurvePointsSet(CurvesList.Items[0]).Hasx0) then
    begin
        if CurvePositions.PointsCount = 0 then
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
                    Curve.Initx0 := CurvePosition;
                end;
                if Curve.HasSigma then Curve.Sigma := Sigma;
                //  ne zapolnyaetsya, potomu chto ne nuzhna
                //Curve.Lambda := WaveLength;
                CalcInitHash(Curve);
                SearchSpecimenAndInit(SpecimenParameters, Curve);
                //  dobavlenie novogo ekz. patterna v spisok
                CurvesList.Add(Curve);
                //  dobavlenie tochki pryavyazki, kogda pattern imeet tochku
                //  privyazki dlya posleduyuschego otobrazheniya v obschem spiske
                if TCurvePointsSet(CurvesList.Items[0]).Hasx0 then
                    CurvePositions.AddNewPoint(CurvePosition, CurveAmplitude);
            except
                Curve.Free;
                raise;
            end;
            *)
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
                        Curve_parameters(Params.GetCopy));

                    if not Curve.Hasx0 then
                    begin
                        //  dlina kazhdogo ekz. patterna ust. ravnoy
                        //  dline uchastka vybrannogo pol'zovatelem
                        for j := 0 to ExpProfile.PointsCount - 1 do
                            Curve.AddNewPoint(ExpProfile.PointXCoord[j], 0);

                        //  amplituda i tochka privyazki ustanavlivayutsya po
                        //  sredney tochke intervala
                        CurveAmplitude := ExpProfile.PointYCoord[ExpProfile.PointsCount div 2];
                        if Curve.HasA then Curve.A := CurveAmplitude;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //Curve.Lambda := WaveLength;
                        CalcInitHash(Curve);
                        SearchSpecimenAndInit(SpecimenParameters, Curve);
                        //  dobavlenie novogo ekz. patterna v spisok
                        CurvesList.Add(Curve);
                    end else Curve.Free;
                except
                    Curve.Free;
                    raise;
                end;
            end;
        end //  if CurvePositions.PointsCount = 0
        else
        begin
            //  proveryaem vybrannye tochki i dobavlyaem novye ekz. patterna
            for i := 0 to CurvePositions.PointsCount - 1 do
            begin
                CurveFound := False;
                for k := 0 to CurvesList.Count - 1 do
                begin
                    Curve := TCurvePointsSet(CurvesList.Items[k]);
                    if not Curve.Hasx0 then Break;

                    //if Curve.Initx0 = CurvePositions.PointXCoord[i] then
                    if Abs(Curve.Initx0 - CurvePositions.PointXCoord[i]) <= TINY then
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
                        for j := 0 to ExpProfile.PointsCount - 1 do
                            Curve.AddNewPoint(ExpProfile.PointXCoord[j], 0);

                        if Curve.HasA then Curve.A := CurvePositions.PointYCoord[i];
                        //  ust. polozhenie ekz. patterna
                        if Curve.Hasx0 then
                        begin
                            Curve.x0 := CurvePositions.PointXCoord[i];
                            Curve.Initx0 := CurvePositions.PointXCoord[i];
                        end;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //Curve.Lambda := WaveLength;
                        CalcInitHash(Curve);
                        SearchSpecimenAndInit(SpecimenParameters, Curve);
                        //  dobavlenie novogo ekz. patterna v spisok
                        CurvesList.Add(Curve);
                        //  esli pattern ne imeet parametra polozheniya,
                        //  to sozdaetsya tol'ko odin ekzemplyar
                        if not Curve.Hasx0 then Break;
                    except
                        Curve.Free;
                        raise;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TFitTask.FindGaussesSequentiallyAlg;
var ZerosDeleted, PointDeleted: Boolean;
    Deleted: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    //  pervyy tsikl optimizatsii
    Optimization;

    Deleted := nil;
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
        Deleted.Free; Deleted := nil;
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
                            AddPointToCurvePositions(Deleted.Initx0);
                        CurvesList.Add(Deleted); Deleted := nil;
                        PointDeleted := False;
                    end;
                    CalculateProfile;
                    ShowCurMin;     //  neobhodimo dlya sohraneniya
                                    //  tekuschego znacheniya fakt. rash.
                    Break;
                end;
            end else ShowCurMin;    //  neobhodimo dlya sohraneniya
                                    //  tekuschego znacheniya fakt. rash.
        end else Break;
        if CurvesList.Count <= 1 then Break;
    end;

    //  udalyaem iz spiska vydelennyh tochek te tochki,
    //  dlya kotoryh gaussiany imeyut nulevuyu amplitudu i
    //  te tochki, v kotoryh amplituda gaussianov minimal'na
    while (GetRFactor < FMaxRFactor) and (not FTerminated) do
    begin
        StoreCurveParams;
        ZerosDeleted := DeleteZeros;
        Deleted.Free; Deleted := nil;
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
                            AddPointToCurvePositions(Deleted.Initx0);
                        CurvesList.Add(Deleted); Deleted := nil;
                        PointDeleted := False;
                    end;

                    CalculateProfile;
                    ShowCurMin;     //  neobhodimo dlya sohraneniya
                                    //  tekuschego znacheniya fakt. rash.
                    Break;
                end;
            end else ShowCurMin;    //  neobhodimo dlya sohraneniya
                                    //  tekuschego znacheniya fakt. rash.
        end else Break;
        if CurvesList.Count <= 1 then Break;
    end;

    Deleted.Free;
    //SigmaVaryingDisabled := False;
    //Optimization;
end;

{$hints off}
procedure TFitTask.Optimization;
var ErrorCode: LongInt;
begin
    if FEnableFastMinimizer then
    begin
        CreateFastMinimizer;
        FMinimizer.Minimize(ErrorCode);
    end;

    CreateDHSMinimizer;
    FMinimizer.Minimize(ErrorCode);

    FMinimizer.Free; FMinimizer := nil;
end;
{$hints on}

procedure TFitTask.FindGausses;
//var    i: LongInt;
    //GP: TPointsSet;
begin
    // nachal'naya initsializatsiya neobhodima, kogda pri
    // vychislenii R-faktora predpolagaetsya, chto vse
    // tochki vychislennogo profilya ne d.b. ravny 0
    //Assert(Assigned(CurvesList));
    //with CurvesList do
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

procedure TFitTask.FindGaussesSequentially;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    FindGaussesSequentiallyAlg;
    Done;
end;

procedure TFitTask.AddPointToCurvePositions(XValue: Double);
var Index: LongInt;
begin
    Assert(Assigned(ExpProfile));
    Assert(Assigned(CurvePositions));
    Index := ExpProfile.IndexOfValueX(XValue);
    Assert(Index <> -1);
    CurvePositions.AddNewPoint(XValue, ExpProfile.PointYCoord[Index]);
end;

//  udalyaet tochku s zadannym X iz spiska vybrannyh tochek
procedure TFitTask.DeletePoint(var Points: TPointsSet; XValue: Double);
var j: LongInt;
    Temp: TPointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    //  chtoby skopirovalis' vse parametry
    Temp := TPointsSet(Points.GetCopy);
    try
        Temp.Clear;
        for j := 0 to Points.PointsCount - 1 do
        begin
            //if XValue <> Points.PointXCoord[j] then
            if Abs(XValue - Points.PointXCoord[j]) > TINY then
                Temp.AddNewPoint(Points.PointXCoord[j], Points.PointYCoord[j]);
        end;
        Points.Free; Points := nil;
        Points := Temp;
    except
        Temp.Free; raise;
    end;
end;

procedure TFitTask.SetProfilePointsSet(APointsSet: TPointsSet);
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(APointsSet));

    ExpProfile.Free; ExpProfile := nil;
    ExpProfile := APointsSet;
end;

procedure TFitTask.SetCurvePositions(ACurvePositions: TPointsSet);
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(ACurvePositions));

    CurvePositions.Free; CurvePositions := nil;
    CurvePositions := ACurvePositions;
end;

function TFitTask.GetCurvePositions: TPointsSet;
begin
    Result := CurvePositions;
end;

function TFitTask.GetCalcProfile: TPointsSet;
begin
    Result := CalcProfile;
end;

function TFitTask.GetCurvesList: TSelfCopiedCompList;
begin
    Result := CurvesList;
end;

function TFitTask.GetCurMin: Double;
begin
    Result := CurMin;
end;

function TFitTask.GetCurAbsMin: Double;
begin
    Result := CurAbsMin;
end;

function TFitTask.GetCurSqrMin: Double;
begin
    Result := CurSqrMin;
end;

function TFitTask.GetCurMinInitialized: Boolean;
begin
    Result := CurMinInitialized;
end;

function TFitTask.GetAllDone: Boolean;
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
    CurSqrMin := GetSqrRFactor;
    CurAbsMin := GetAbsRFactor;
    CurMin := CurSqrMin;    //  chtoby ne pereschityvat'
                            //  !!! dolzhno sootvetstvovat' GetRFactor !!!
    CurMinInitialized := True;
    ServerShowCurMin;
end;

procedure TFitTask.Done;
begin
    FAllDone := True;
    ServerDoneProc;
end;

procedure TFitTask.SetSpecialCurve(
    ACurveExpr: string; AParams: Curve_parameters);
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    try
        Assert(Length(ACurveExpr) <> 0);
        Assert(Assigned(AParams));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Warning);
        else raise;
    end;
    
    FCurveExpr := ACurveExpr;
    Params.Free; Params := AParams;
end;

procedure TFitTask.StopAsyncOper;
begin
    FTerminated := True;
    if Assigned(FMinimizer) then FMinimizer.Terminated := True;
end;

function TFitTask.GetScalingFactor: Double;
var
    CalcProfileIntegral, ProfileIntegral: Double;
begin
    if FCurveScalingEnabled then
    begin
        CalcProfileIntegral := GetCalcProfileIntegral;
        ProfileIntegral := GetProfileIntegral;

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
