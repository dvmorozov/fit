{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of auxiliary data containers.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit mscr_specimen_list;

{$MODE Delphi}

interface

uses Classes, data_classes, SimpMath, special_curve_parameter, SysUtils;

const
    { Display mode constants. }
    XCM_2T    = 0;
    XCM_T     = 1;
    XCM_SINTL = 2;

const
    StartPosName: string  = 'Start Pos.';
    FinishPosName: string = 'Finish Pos.';

type
    { Defines container for curves.
      Allows to input/output angles in different representations. 
      In copying data from grid verifies them and adds to the list 
      only data for which corresponding rows are correct. }
    TMSCRCurveList = class(TCurveList)
    protected
        function RecalcParamValue(P: TSpecialCurveParameter): double; override;
        procedure ReverseCalcParamValue(P: TSpecialCurveParameter;
            NewValue: double); override;

    public
        { Vawelength at which neutronogram was recorded. }
        Lambda:   double;
        { It is supposed that data are given in 2 * Theta format. }
        ViewMode: longint;

        function GetCopy: TObject; override;
        procedure CopyParameters(Dest: TObject); override;
    end;

    { Container of curves which is stored in XML-stream. }
    Parameters_list = class(TComponent)
    private
        FParameters: TMSCRCurveList;

    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;

    published
        property Parameters: TMSCRCurveList read FParameters write FParameters;
    end;

implementation

function TMSCRCurveList.RecalcParamValue(P: TSpecialCurveParameter): double;
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) or
        (P.Name = StartPosName) or (P.Name = FinishPosName) then
        case ViewMode of
            XCM_2T:
                Result := P.Value;
            //  schitaetsya, chto iznachal'no koordinaty zadany v 2*Theta


            XCM_T:
                Result := P.Value / 2;//  pereschet v Theta


            XCM_SINTL:
            begin
                Assert(Lambda <> 0);
                //  pereschet v Sin(Theta)/Lambda
                Result := Sin((P.Value * pi) / (2 * 180)) / Lambda;
            end;
        end{case ViewMode of...}
    else
        Result := P.Value;
end;

procedure TMSCRCurveList.ReverseCalcParamValue(P: TSpecialCurveParameter;
    NewValue: double);
begin
    if (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition) or
        (P.Name = StartPosName) or (P.Name = FinishPosName) then
        case ViewMode of
            XCM_T: P.Value  := NewValue * 2;
            XCM_2T: P.Value := NewValue;
            XCM_SINTL:
            begin
                Assert(Lambda <> 0);
                P.Value := 2 * (180 / pi) * ArcSin(NewValue * Lambda);
            end;
        end{case ViewMode of...}
    else
        P.Value := NewValue;
end;

function TMSCRCurveList.GetCopy: TObject;
begin
    Result := TMSCRCurveList.Create(nil);
    CopyParameters(Result);
end;

procedure TMSCRCurveList.CopyParameters(Dest: TObject);
begin
    inherited;
    TMSCRCurveList(Dest).Lambda   := Lambda;
    TMSCRCurveList(Dest).ViewMode := ViewMode;
end;

{ Parameters_list }

constructor Parameters_list.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    FParameters := TMSCRCurveList.Create(nil);
end;

destructor Parameters_list.Destroy;
begin
    FParameters.Free;
    inherited Destroy;
end;

{$warnings off}
initialization
    RegisterClass(TMSCRCurveList);
    DecimalSeparator := '.';
end.
{$warnings on}
