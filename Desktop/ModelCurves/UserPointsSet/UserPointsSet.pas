{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for user curve given as expression.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit UserPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, PointsSet, CurvePointsSet, NamedPointsSet,
  CurveTypesSingleton, IntPointsSet;

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

    { Container for points of user curve given as expression. }
    TUserPointsSet = class(TNamedPointsSet)
    protected
        { Expression given in general text form. }
        FExpression: string;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        { Performs calculation of function value for given value of argument. }
        function CalcValue(ArgValue: Double): Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeId: TCurveTypeId; override;
        class function GetCurveTypeId_: TCurveTypeId;
        { Returns true if curve type has parameters which should be configured
          by user, otherwise returns false. }
        function HasConfigurableParameters: Boolean; override;
        { Displays dialog for set up user configurable parameters. Returns true
          if dialog was confirmed and false if it was cancelled. }
        function ShowConfigurationDialog: Boolean; override;
        { Returns true if user configurable parameters have default values,
          otherwise returns false. }
        function HasDefaults: Boolean; override;
        { Sets up default values for user configurable parameters. }
        procedure SetDefaults; override;

        property Expression: string read FExpression write FExpression;
    end;
    
implementation

uses CreateUserPointsSetDialog, UserPointsSetPropDialog, Settings, Controls,
  Main, MyExceptions, Dialogs, Unit1;

{=========================== TUserPointsSet ================================}

function TUserPointsSet.GetCurveTypeName: string;
begin
    Result := 'User defined';
end;

function TUserPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := GetCurveTypeId_;
end;

class function TUserPointsSet.GetCurveTypeId_: TCurveTypeId;
begin
    Result := StringToGUID('{d8cafce5-8b03-4cce-9e93-ea28acb8e7ca}');
end;

function TUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := True;
end;

function TUserPointsSet.ShowConfigurationDialog: Boolean;
var ct: Curve_type;
    Success: Boolean;
label dlg1, dlg2;
begin
    Result := True; //???
{$IFNDEF EXCLUDE_SOMETHING}
    //  mashina sostoyaniy
dlg1:
    ct := nil;
    CreateSpecialCurveDlg.ActiveControl := CreateSpecialCurveDlg.EditExpression;
    case CreateSpecialCurveDlg.ShowModal of
        mrOk:
            begin
                Success := False;

                try
                    //  pervonach. razbor
                    FitClientApp_.FitClient.SetSpecialCurveParameters(
                        CreateSpecialCurveDlg.EditExpression.Text, nil);
                    Success := True;
                except
                    on E: EUserException do
                        begin
                            MessageDlg(E.Message, mtError, [mbOk], 0);
                        end;
                    else raise;
                end;

                if Success then
                begin
                    ct := Curve_type.Create(nil);
                    ct.Name := CreateSpecialCurveDlg.EditCurveName.Text;
                    ct.Expression := CreateSpecialCurveDlg.EditExpression.Text;
                    FormMain.Settings.Curve_types.Add(ct);
                    ct.Parameters :=
                        FitClientApp_.FitClient.GetSpecialCurveParameters;
                    FormMain.WriteCurve(ct);

                    //DeleteDummyCurve;
                    //  dobavlyaem vo vse menyu novyy element
                    FormMain.AddCurveMenuItem(ct);

                    goto dlg2;
                end
                else goto dlg1;
            end;
    else Exit;
    end;

dlg2:
    UserPointsSetPropDlg.ct := ct;
    case UserPointsSetPropDlg.ShowModal of
        mrOk:
            begin
                //  perezapisyvaetsya dlya sohraneniya
                //  sdelannyh ustanovok parametrov
                DeleteFile(PChar(ct.FileName));
                FormMain.WriteCurve(ct);
            end;
        mrRetry:
            begin
                //  pri vozvrate udalyaem
                FormMain.DeleteCurve(ct);
                goto dlg1;
            end;
    else Exit;
    end;
{$ENDIF}
end;

function TUserPointsSet.HasDefaults: Boolean;
begin
    Result := False;
end;

procedure TUserPointsSet.SetDefaults;
begin
    //  Do nothing.
end;

function TUserPointsSet.CalcValue(ArgValue: Double): Double;
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

procedure TUserPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
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

procedure TUserPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TUserPointsSet(Dest).Expression := Expression;
end;

var CTS: TCurveTypesSingleton;

initialization
    CTS := TCurveTypesSingleton.Create;
    CTS.RegisterCurveType(TUserPointsSet);
end.

