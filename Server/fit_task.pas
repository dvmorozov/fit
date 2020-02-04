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

uses Classes, SysUtils, points_set, curve_points_set, self_copied_component,
    int_minimizer, simple_minimizer, downhill_simplex_minimizer, main_calc_thread,
    mscr_specimen_list, int_points_set, lorentz_points_set, gauss_points_set,
    two_branches_pseudo_voigt_points_set, asym_pseudo_voigt_points_set,
    user_points_set, pseudo_voigt_points_set, special_curve_parameter,
    persistent_curve_parameter_container;

type
    { Fits profile interval by model curves (specimens).
      Provides variable parameters and evaluation function for optimization
      algorithm.
      It is inherited from TComponent to allow inserting into TComponentList. }
    TFitTask = class(TComponent)
    protected
        FBegIndex: LongInt;
        FEndIndex: LongInt;

        FMaxRFactor: Double;
        FCurveTypeId: TCurveTypeId;
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
        CommonSpecimenParams: Curve_parameters;
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
        Terminated: Boolean;
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
        FDoneProc: TDoneProc;
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
        { Flag indicating that asynchronous operation executed as subtask was terminated. 
          For this class is always True for now because the class does not support asynchronous operations. }
        AllDone: Boolean;

        procedure ShowCurMin; virtual;
        procedure DoneProc; virtual;

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
        { The method is called for initial creation of curves. 
          It is executed more effectively than Update. }
        //procedure CreateCurves;

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
        function GetPatternSpecimen: TCurvePointsSet;
        function MinimumStepAchieved: Boolean;
        procedure InitializeVariationSteps;

    public
        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: Boolean); virtual;
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
        procedure UpdateCurves(SpecimenParameters: TMSCRSpecimenList);
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
        { Searches set of pattern specimens (curves) fitting exprerimental data with given accuracy
          sequentially decreasing number of curves. }
        procedure FindGaussesSequentially; virtual;

        property MaxRFactor: Double write FMaxRFactor;
        property CurveTypeId: TCurveTypeId write FCurveTypeId;
        { Callback to update information at achieving new minimum. }
        property ShowCurMinExternal: TShowCurMin read FShowCurMin write FShowCurMin;
        property DoneProcExternal: TDoneProc read FDoneProc write FDoneProc;
        { Attributes store indexes of begin and end of the task interval 
          for optimal rebuilding overall resulting profile. }
        property BegIndex: LongInt read FBegIndex write FBegIndex;
        property EndIndex: LongInt read FEndIndex write FEndIndex;
        property ProfileIntegral: Double read GetProfileIntegral;
        property CalcProfileIntegral: Double read GetCalcProfileIntegral;
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
    CPI, PI: Double;
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
    //  vychislyaem normu
    CPI := CalcProfileIntegral; PI := ProfileIntegral;
    if CPI = 0 then begin Result := 1; Exit; end;
    //  spetsial'nyy sluchay, kogda profil' = 0
    if PI = 0 then begin Result := CPI; Exit; end;

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
                Abs(CalcProfile.PointYCoord[i] * PI / CPI -
                    ExpProfile.PointYCoord[i]);
        end;
    end;
    RFactor := RFactor / PI;
    Result := RFactor;
end;
(*
function TFitTask.GetOptimizingRFactor: Double;
var SA: TPointsSet;
    CPS: TCurvePointsSet;
    i, j: LongInt;
    RFactor: Double;
    Flag: Boolean;
    RangeDefined: Boolean;
    CPI, PI: Double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CalcProfile));
    SA := ExpProfile;
    Assert(Assigned(SA));

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
    //  normirovka uluchshaet shodimost' DownhillSimplex'a
    //  vychislyaem normu
    CPI := CalcProfileIntegral; PI := ProfileIntegral;

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
            if CPI <> 0 then
            begin
                if PI <> 0 then
                    RFactor := RFactor +
                        Sqr(CalcProfile.PointYCoord[i] / CPI -
                            SA.PointYCoord[i] / PI)
                else
                    RFactor := RFactor +
                        Sqr(CalcProfile.PointYCoord[i] / CPI);
            end
            else
            begin
                if PI <> 0 then
                    RFactor := RFactor +
                        Sqr(SA.PointYCoord[i] / PI);
            end;
        end;
    end;
    Result := RFactor;
end;
*)
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
    CPI, PI: Double;
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
    //  vychislyaem normu
    CPI := CalcProfileIntegral; PI := ProfileIntegral;
    if CPI = 0 then begin Result := 1; Exit; end;
    //  spetsial'nyy sluchay, kogda profil' = 0
    if PI = 0 then begin Result := CPI; Exit; end;

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
                Sqr(CalcProfile.PointYCoord[i] * PI / CPI -
                    ExpProfile.PointYCoord[i]);
        end;
    end;
    RFactor := RFactor / Sqr(PI);
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
        Result := CommonSpecimenParams[CommonVaryingIndex].VariationStep;
    end
    else
    begin
        Curve := TCurvePointsSet(CurvesList.Items[CurveNum]);
        Result := Curve.Parameters[ParamNum].VariationStep;
    end;
end;

{$hints off}
procedure TFitTask.SetStep(NewStepValue: Double);
begin

end;
{$hints on}

procedure TFitTask.SetNextParam;
var GP: TCurvePointsSet;
    Count: LongInt;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Assigned(CommonSpecimenParams));

    EOC := True;
    if CurvesList.Count <> 0 then
    begin
        //  perebor parametrov krivoy
        GP := TCurvePointsSet(CurvesList.Items[CurveNum]);
        if ParamNum < GP.ParamCount - 1 then
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

    Count := CommonSpecimenParams.Params.Count;
    if CommonVaryingFlag then
    begin
        //  poisk sleduyuschego obschego parametra,
        //  variatsiya kotorogo ne zapreschena
        if CommonVaryingIndex < Count then
        begin
            Inc(CommonVaryingIndex);
            while (CommonVaryingIndex <> Count) and
                   CommonSpecimenParams[CommonVaryingIndex].VariationDisabled do
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
    for i := 0 to CommonSpecimenParams.Params.Count - 1 do
    begin
        if not CommonSpecimenParams[i].VariationDisabled then Break;
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
    Assert(Assigned(CommonSpecimenParams));
    
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
        Assert(CommonVaryingIndex < CommonSpecimenParams.Params.Count);
        Parameter := CommonSpecimenParams[CommonVaryingIndex];
        Result := Parameter.Value;
    end
    else
    begin
        Assert(CurvesList.Count <> 0);

        GP := TCurvePointsSet(CurvesList.Items[CurveNum]);
        Result := GP.Param[ParamNum];
    end;
end;

procedure TFitTask.SetParam(NewParamValue: Double);
var GP: TCurvePointsSet;
    i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Assigned(CommonSpecimenParams));
    
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
        Assert(CommonVaryingIndex < CommonSpecimenParams.Params.Count);
        Parameter := CommonSpecimenParams[CommonVaryingIndex];
        Parameter.Value := NewParamValue;

        //  ustanovka znacheniya obschego parametra u vseh ekzemplyarov
        for i := 0 to CurvesList.Count - 1 do
        begin
            GP := TCurvePointsSet(CurvesList.Items[i]);
            GP.ParamByName[
                CommonSpecimenParams[CommonVaryingIndex].Name
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
        GP.Param[ParamNum] := NewParamValue;
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
    Assert(Assigned(CommonSpecimenParams));
    
    for i := 0 to CommonSpecimenParams.Params.Count - 1 do
    begin
        CommonSpecimenParams[i].MultiplyVariationStep(Factor);
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
    AEnableBackgroundVariation: Boolean);
begin
    inherited Create(AOwner);
    CommonSpecimenParams := Curve_parameters.Create(nil);
    CommonSpecimenParams.Params.Clear;  //  Curve_parameters sozdaet v konstruktore
                                        //  odin parametr - nuzhno ego udalit'
    FMaxRFactor := 0.01;
    //  Sets default curve type
    FCurveTypeId := TGaussPointsSet.GetCurveTypeId_;
    AllDone := True;
    
    FEnableBackgroundVariation := AEnableBackgroundVariation;
    FEnableFastMinimizer := False;
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
    CommonSpecimenParams.Free;
    inherited;
end;

procedure TFitTask.CalculateProfile;
var i: LongInt;
    PS: TCurvePointsSet;
    RestoreBackground: Boolean;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvesList));
    Assert(Background.PointsCount = SavedBackground.PointsCount);
    Assert(ExpProfile.PointsCount = SavedBackground.PointsCount);

    for i := 0 to CurvesList.Count - 1 do
    begin
        PS := TCurvePointsSet(CurvesList.Items[i]);
        PS.ReCalc(nil);
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
    for i := 0 to CommonSpecimenParams.Params.Count - 1 do
    begin
        CommonSpecimenParams[i].InitVariationStep;
    end;

    for i := 0 to CurvesList.Count - 1 do
    begin
        Curve := TCurvePointsSet(CurvesList.Items[i]);
        for j := 0 to Curve.ParamCount - 1 do
        begin
            Curve.Parameters[j].InitVariationStep;
        end;
    end;
end;

function TFitTask.MinimumStepAchieved: Boolean;
var
    i, j: LongInt;
    Curve: TCurvePointsSet;
begin
    for i := 0 to CommonSpecimenParams.Params.Count - 1 do
    begin
        if not CommonSpecimenParams[i].MinimumStepAchieved then
        begin
            Result := False;
            Exit;
        end;
    end;

    for i := 0 to CurvesList.Count - 1 do
    begin
        Curve := TCurvePointsSet(CurvesList.Items[i]);
        for j := 0 to Curve.ParamCount - 1 do
        begin
            if not Curve.Parameters[j].MinimumStepAchieved then
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
(*
procedure TFitTask.CreateCurves;
var i, j: LongInt;
    SA: TPointsSet;
    GP: TCurvePointsSet;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvePositions));
    SA := ExpProfile;
    Assert(Assigned(SA));
    Assert(SA.PointsCount >= 2);

    //  sozdaem zanovo nabor krivyh
    CurvesList.Free; CurvesList := nil;
    CurvesList := TSelfCopiedCompList.Create(nil);

    //  sozdaem zanovo summarnyy profil'
    if Assigned(CalcProfile) then CalcProfile.Clear
    else CalcProfile := TPointsSet.Create(nil);
    //  kol-vo tochek profilya ustanavlivaetsya ravnym kol-vu tochek uchastka
    for i := 0 to SA.PointsCount - 1 do
        CalcProfile.AddNewPoint(SA.PointXCoord[i], 0);
        
    if CurvePositions.PointsCount = 0 then
    begin
        //  dobavlyaetsya odin ekzemplyar patterna na dannyy interval;
        //  esli pattern imeet parametr polozheniya, to u ekzemplyara
        //  znachenie etogo parametra ustanavlivaetsya ravnym
        //  seredine intervala
        GP := GetPatternSpecimen;

        try
            //  dlina kazhdogo ekz. patterna ust. ravnoy
            //  dline uchastka vybrannogo pol'zovatelem
            for j := 0 to SA.PointsCount - 1 do
                GP.AddNewPoint(SA.PointXCoord[j], 0);

            if GP is TUserPointsSet then
            begin
                TUserPointsSet(GP).Expression := FCurveExpr;
                TUserPointsSet(GP).SetParameters(
                    Curve_parameters(Params.GetCopy));
            end;
            //  amplituda i tochka privyazki ustanavlivayutsya po
            //  sredney tochke intervala
            if GP.HasA then GP.A := SA.PointYCoord[SA.PointsCount div 2];
            //  ust. polozhenie ekz. patterna
            if GP.Hasx0 then
            begin
                GP.x0 := SA.PointXCoord[SA.PointsCount div 2];
                GP.Initx0 := SA.PointXCoord[SA.PointsCount div 2];
            end;
            //  ne zapolnyaetsya, potomu chto ne nuzhna
            //GP.Lambda := WaveLength;
            CalcInitHash(GP);
            //  dobavlenie novogo ekz. patterna v spisok
            CurvesList.Add(GP);
        except
            GP.Free;
            raise;
        end;
    end
    else
    begin
        for i := 0 to CurvePositions.PointsCount - 1 do
        begin
            //  sozdaetsya novaya krivaya
            GP := GetPatternSpecimen;

            try
                //  dlina kazhdoy krivoy ust. ravnoy dline uchastka
                //  vybrannogo pol'zovatelem
                for j := 0 to SA.PointsCount - 1 do
                    GP.AddNewPoint(SA.PointXCoord[j], 0);

                if GP.HasA then GP.A := CurvePositions.PointYCoord[i];
                // ust. polozhenie krivoy
                if GP.Hasx0 then
                begin
                    GP.x0 := CurvePositions.PointXCoord[i];
                    GP.Initx0 := CurvePositions.PointXCoord[i];
                end;
                //  ne zapolnyaetsya, potomu chto ne nuzhna
                //GP.Lambda := WaveLength;
                CalcInitHash(GP);
                //  dobavlenie novoy krivoy v spisok krivyh
                CurvesList.Add(GP);
                //  !!! esli net parametra-pozitsii, to sozdaetsya vsego odna
                //  krivaya - ostal'nye tochki iz CurvePositions ignoriruyutsya !!!
                if not GP.Hasx0 then Break;
            except
                GP.Free;
                raise;
            end;
        end;
    end;
end;
*)
procedure TFitTask.CalcInitHash(Specimen: TCurvePointsSet);
var i: LongInt;
    Parameter: TPersistentCurveParameterContainer;
    Value: string;
begin
    Assert(Assigned(Specimen));
    Specimen.InitHash := 0;
    for i := 0 to Specimen.Params.Params.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(
            Specimen.Params.Params.Items[i]);
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
            for j := 0 to Specimen.Params.Params.Count - 1 do
            begin
                Parameter := Specimen.Params[j];
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

function TFitTask.GetPatternSpecimen: TCurvePointsSet;
var i: LongInt;
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
begin
    if IsEqualGUID(FCurveTypeId, TLorentzPointsSet.GetCurveTypeId_) then
    begin
        Result := TLorentzPointsSet.Create(nil);
    end
    else
    if IsEqualGUID(FCurveTypeId, TGaussPointsSet.GetCurveTypeId_) then
        Result := TGaussPointsSet.Create(nil)
    else
    if IsEqualGUID(FCurveTypeId, TPseudoVoigtPointsSet.GetCurveTypeId_) then
    begin
        //Result := T2BranchesPseudoVoigtPointsSet.Create(nil);
        Result := TPseudoVoigtPointsSet.Create(nil)
    end
    else
    if IsEqualGUID(FCurveTypeId, TAsymPseudoVoigtPointsSet.GetCurveTypeId_) then
    begin
        Result := TAsymPseudoVoigtPointsSet.Create(nil)
    end
    else
    if IsEqualGUID(FCurveTypeId, TUserPointsSet.GetCurveTypeId_) then
    begin
{$IFDEF _WINDOWS}
        Result := TUserPointsSet.Create(nil);
        TUserPointsSet(Result).Expression := FCurveExpr;
        TUserPointsSet(Result).SetParameters(
            Curve_parameters(Params.GetCopy));
{$ENDIF}
    end
    else
    if IsEqualGUID(FCurveTypeId, T2BranchesPseudoVoigtPointsSet.GetCurveTypeId_) then
    begin
        //  By default.
        Result := T2BranchesPseudoVoigtPointsSet.Create(nil);
    end;
    
    if CommonSpecimenParams.Params.Count = 0 then
    begin
        //  pervonachal'naya initsyalizatsyya spiska obschih parametrov;
        for i := 0 to Result.Params.Params.Count - 1 do
        begin
            if (Result.Params[i].Type_ = Shared) and (not
                Result.Params[i].VariationDisabled) then
            begin
                //  !!! predpolagaetsya, chto vse krivye odnogo tipa !!!
                Parameter := TSpecialCurveParameter.Create;

                try
                    Result.Params[i].CopyTo(Parameter);
                    //  spetsial'naya initsializatsiya
                    if ((UpperCase(Parameter.Name) = 'SIGMA') or
                       ((UpperCase(Parameter.Name) = 'SIGMARIGTH')))
                        then
                    begin
                        Parameter.Value := 0.25;
                        Parameter.VariationStep := 0.1;
                    end;

                    Container := TPersistentCurveParameterContainer(
                        CommonSpecimenParams.Params.Add);

                    try
                        Container.Parameter := Parameter;
                    except
                        CommonSpecimenParams.Params.Delete(Container.ID);
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
    
    for i := 0 to Result.Params.Params.Count - 1 do
    begin
        //  initsializatsiya znacheniy
        if ((UpperCase(Result.Params[i].Name) = 'SIGMA') or
           ((UpperCase(Result.Params[i].Name) = 'SIGMARIGTH')))
            then
        begin
            Result.Params[i].Value := 0.25;
            Result.Params[i].VariationStep := 0.1;
        end;
    end;
end;

procedure TFitTask.UpdateCurves(SpecimenParameters: TMSCRSpecimenList);
var i,j,k: LongInt;
    SA: TPointsSet;
    GP: TCurvePointsSet;
    Flag: Boolean;
    //  koordinaty tochki privyazki sozdavaemogo avtomaticheski
    //  ekz. patterna, kogda pol'zovatel'skie tochki privyazki
    //  ne zadany
    //SpecX,
    SpecY: Double;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(CurvePositions));
    SA := ExpProfile;
    Assert(Assigned(SA));
    Assert(SA.PointsCount >= 2);

    //  eto delaet vozmozhnym vyzov UpdateCurves
    //  bez predvaritel'nogo vyzova CreateCurves
    if not Assigned(CurvesList) then
        CurvesList := TSelfCopiedCompList.Create(nil);

    //  sozdaem zanovo summarnyy profil'
    if Assigned(CalcProfile) then CalcProfile.Clear
    else CalcProfile := TPointsSet.Create(nil);
    //  kol-vo tochek profilya ustanavlivaetsya ravnym kol-vu tochek uchastka
    for i := 0 to SA.PointsCount - 1 do
        CalcProfile.AddNewPoint(SA.PointXCoord[i], 0);
    //  sozdaem zanovo massiv tochek fona
    if Assigned(Background) then Background.Clear
    else Background := TPointsSet.Create(nil);
    for i := 0 to SA.PointsCount - 1 do
        Background.AddNewPoint(SA.PointXCoord[i], 0);

    if Assigned(SavedBackground) then SavedBackground.Clear
    else SavedBackground := TPointsSet.Create(nil);
    for i := 0 to SA.PointsCount - 1 do
        SavedBackground.AddNewPoint(SA.PointXCoord[i], 0);

    //  proveryaem i udalyaem te ekz. patterna,
    //  polozheniya kot. net sredi vybrannyh tochek
    k := 0;
    while k < CurvesList.Count do
    begin
        Flag := False;
        GP := TCurvePointsSet(CurvesList.Items[k]);
        //  esli pattern ne imeet parametra polozheniya, to
        //  ego ekzemplyary ne udalyayutsya
        if not GP.Hasx0 then Break;
        
        for i := 0 to CurvePositions.PointsCount - 1 do
        begin
            if GP.Initx0 = CurvePositions.PointXCoord[i] then
            begin
                Flag := True;
                Break;
            end;
        end;

        if not Flag then
        begin // udalyaem
            //  CurvesList po-umolchaniyu osvobozhdaet
            //  komponenty, ssylki na kotorye hranit
            CurvesList.Remove(GP);
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
                krivoy lyubogo tipa, odnako eto vyzyvalo problemu pri
                ispol'zovanii programmy - nel'zya bylo udalit' iz
                intervala edinstvennuyu krivuyu, sozdannuyu etim
                algoritmom
            GP := GetPatternSpecimen;

            try
                //  dlina kazhdogo ekz. patterna ust. ravnoy
                //  dline uchastka vybrannogo pol'zovatelem
                for j := 0 to SA.PointsCount - 1 do
                    GP.AddNewPoint(SA.PointXCoord[j], 0);

                //  amplituda i tochka privyazki ustanavlivayutsya po
                //  sredney tochke intervala
                SpecX := SA.PointXCoord[SA.PointsCount div 2];
                SpecY := SA.PointYCoord[SA.PointsCount div 2];
                if GP.HasA then GP.A := SpecY;
                //  ust. polozhenie ekz. patterna
                if GP.Hasx0 then
                begin
                    GP.x0 := SpecX;
                    GP.Initx0 := SpecX;
                end;
                if GP.HasSigma then GP.Sigma := Sigma;
                //  ne zapolnyaetsya, potomu chto ne nuzhna
                //GP.Lambda := WaveLength;
                CalcInitHash(GP);
                SearchSpecimenAndInit(SpecimenParameters, GP);
                //  dobavlenie novogo ekz. patterna v spisok
                CurvesList.Add(GP);
                //  dobavlenie tochki pryavyazki, kogda pattern imeet tochku
                //  privyazki dlya posleduyuschego otobrazheniya v obschem spiske
                if TCurvePointsSet(CurvesList.Items[0]).Hasx0 then
                    CurvePositions.AddNewPoint(SpecX, SpecY);
            except
                GP.Free;
                raise;
            end;
            *)
            //  teper' sozdaetsya ekzemplyar tol'ko
            //  pol'zovatel'skoy krivoy, kotoraya ne
            //  imeet parametra polozheniya
            if IsEqualGUID(FCurveTypeId, TUserPointsSet.GetCurveTypeId_) then
            begin
                GP := GetPatternSpecimen;

                try
                    TUserPointsSet(GP).Expression := FCurveExpr;
                    TUserPointsSet(GP).SetParameters(
                        Curve_parameters(Params.GetCopy));

                    if not GP.Hasx0 then
                    begin
                        //  dlina kazhdogo ekz. patterna ust. ravnoy
                        //  dline uchastka vybrannogo pol'zovatelem
                        for j := 0 to SA.PointsCount - 1 do
                            GP.AddNewPoint(SA.PointXCoord[j], 0);

                        //  amplituda i tochka privyazki ustanavlivayutsya po
                        //  sredney tochke intervala
                        SpecY := SA.PointYCoord[SA.PointsCount div 2];
                        if GP.HasA then GP.A := SpecY;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //GP.Lambda := WaveLength;
                        CalcInitHash(GP);
                        SearchSpecimenAndInit(SpecimenParameters, GP);
                        //  dobavlenie novogo ekz. patterna v spisok
                        CurvesList.Add(GP);
                    end else GP.Free;
                except
                    GP.Free;
                    raise;
                end;
            end;
        end //  if CurvePositions.PointsCount = 0
        else
        begin
            //  proveryaem vybrannye tochki i dobavlyaem novye ekz. patterna
            for i := 0 to CurvePositions.PointsCount - 1 do
            begin
                Flag := False;
                for k := 0 to CurvesList.Count - 1 do
                begin
                    GP := TCurvePointsSet(CurvesList.Items[k]);
                    if not GP.Hasx0 then Break;

                    //if GP.Initx0 = CurvePositions.PointXCoord[i] then
                    if Abs(GP.Initx0 - CurvePositions.PointXCoord[i]) <= TINY then
                    begin
                        Flag := True;
                        Break;
                    end;
                end;

                if not Flag then
                //  naydena tochka privyazki ekz. patterna, dlya kot. net
                //  ekzemplyara (pri tom, chto pattern imeet parametr polozheniya)
                //  ili spisok ekzemplyarov patterna pust
                begin
                    //  sozdaetsya novyy ekz. patterna
                    GP := GetPatternSpecimen;

                    try
                        //  dlina kazhdogo ekz. patterna ust. ravnoy
                        //  dline uchastka vybrannogo pol'zovatelem
                        for j := 0 to SA.PointsCount - 1 do
                            GP.AddNewPoint(SA.PointXCoord[j], 0);

                        if GP.HasA then GP.A := CurvePositions.PointYCoord[i];
                        //  ust. polozhenie ekz. patterna
                        if GP.Hasx0 then
                        begin
                            GP.x0 := CurvePositions.PointXCoord[i];
                            GP.Initx0 := CurvePositions.PointXCoord[i];
                        end;
                        //  ne zapolnyaetsya, potomu chto ne nuzhna
                        //GP.Lambda := WaveLength;
                        CalcInitHash(GP);
                        SearchSpecimenAndInit(SpecimenParameters, GP);
                        //  dobavlenie novogo ekz. patterna v spisok
                        CurvesList.Add(GP);
                        //  esli pattern ne imeet parametra polozheniya,
                        //  to sozdaetsya tol'ko odin ekzemplyar
                        if not GP.Hasx0 then Break;
                    except
                        GP.Free;
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
    while (GetRFactor < FMaxRFactor) and (not Terminated) do
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
    while (GetRFactor < FMaxRFactor) and (not Terminated) do
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
    DoneProc;
end;

procedure TFitTask.FindGaussesAgain;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    // povtornaya initsializatsiya gaussianov
    UpdateCurves(nil);
    CalculateProfile;
    Optimization;
    DoneProc;
end;

procedure TFitTask.FindGaussesSequentially;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    FindGaussesSequentiallyAlg;
    DoneProc;
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
    Result := AllDone;
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
    ShowCurMinExternal;
end;

procedure TFitTask.DoneProc;
begin
    AllDone := True;
    DoneProcExternal;
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
        on E: EAssertionFailed do WriteLog(E.Message, Surprising);
        else raise;
    end;
    
    FCurveExpr := ACurveExpr;
    Params.Free; Params := AParams;
end;

end.



