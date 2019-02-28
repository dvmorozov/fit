{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of stub class receiving messages from client.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit FitServerStub;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitServer, CommonTypes, PointsSet, SelfCopied,
    MSCRDataClasses, MyExceptions, TitlePointsSet, CurvePointsSet,
    NamedPointsSet;

type
    { For transmission through network class converts exceptions into error codes. }
    TFitServerStub = class(TObject)
    protected
        FServer: TFitServer;
        FRecreateServer: procedure of object;
    public
        { Getting / setting attributes of the server. }
        
        function GetMaxRFactor: Double;
        procedure SetMaxRFactor(AMaxRFactor: Double);
        function GetBackFactor: Double;
        procedure SetBackFactor(ABackFactor: Double);
        function GetCurveThresh: Double;
        procedure SetCurveThresh(ACurveThresh: Double);
        function GetCurveType: TCurveTypeId;
        procedure SetCurveType(ACurveType: TCurveTypeId);
        function GetState: TFitServerState;
        function GetWaveLength: Double;
        procedure SetWaveLength(AWaveLength: Double);
        function GetSelectedAreaMode: Boolean;

        { Wrappers to server methods. They should not throw exceptions. 
          Return codes: 0 - success, -1 - inadmissible state, -2 - fatal error. }
          
        function SmoothProfile(var ErrMsg: string): LongInt;
        function SubtractAllBackground(
            Auto: Boolean; var ErrMsg: string): LongInt;
        function DoAllAutomatically(var ErrMsg: string): LongInt;
        function FindGausses(var ErrMsg: string): LongInt;
        function FindGaussesAgain(var ErrMsg: string): LongInt;
        function FindGaussesSequentially(var ErrMsg: string): LongInt;
        function FindPeakBounds(var ErrMsg: string): LongInt;
        function FindBackPoints(var ErrMsg: string): LongInt;
        function FindPeakPositions(var ErrMsg: string): LongInt;
        function AllPointsAsPeakPositions(var ErrMsg: string): LongInt;

        function StopAsyncOper(var ErrMsg: string): LongInt;
        function AsyncOper(var ErrMsg: string): Boolean;
        function GetCalcTimeStr(var ErrMsg: string): string;
        function GetRFactorStr(var ErrMsg: string): string;
        function GetAbsRFactorStr(var ErrMsg: string): string;
        function GetSqrRFactorStr(var ErrMsg: string): string;
        function SelectArea(
            StartPointIndex, StopPointIndex: LongInt;
            var ErrMsg: string): LongInt;
        function ReturnToTotalProfile(var ErrMsg: string): LongInt;
        function CreateSpecimenList(var ErrMsg: string): LongInt;

        { Data setting. Set methods create new objects. The responsibility
          to free them is put on server. }
        
        function SetProfilePointsSet(
            APointsSet: TTitlePointsSet; var ErrMsg: string): LongInt;
        function SetBackgroundPointsSet(
            ABackgroundPoints: TTitlePointsSet; var ErrMsg: string): LongInt;
        function SetCurvePositions(
            ACurvePositions: TPointsSet; var ErrMsg: string): LongInt;
        function SetRFactorIntervals(
            ARFactorIntervals: TPointsSet; var ErrMsg: string): LongInt;
        function SetSpecialCurveParameters(
            ACurveExpr: string;
            { Equality to Nil means initialization. }
            CP: Curve_parameters;
            var ErrMsg: string
            ): LongInt;
            
        function AddPointToData(
            XValue, YValue: Double; var ErrMsg: string): LongInt;
        function AddPointToBackground(
            XValue, YValue: Double; var ErrMsg: string): LongInt;
        function AddPointToRFactorIntervals(
            XValue, YValue: Double; var ErrMsg: string): LongInt;
        function AddPointToCurvePositions(
            XValue, YValue: Double; var ErrMsg: string): LongInt;

        function ReplacePointInData(PrevXValue, PrevYValue,
            NewXValue, NewYValue: Double; var ErrMsg: string): LongInt;
        function ReplacePointInBackground(PrevXValue, PrevYValue,
            NewXValue, NewYValue: Double; var ErrMsg: string): LongInt;
        function ReplacePointInRFactorIntervals(PrevXValue, PrevYValue,
            NewXValue, NewYValue: Double; var ErrMsg: string): LongInt;
        function ReplacePointInCurvePositions(PrevXValue, PrevYValue,
            NewXValue, NewYValue: Double; var ErrMsg: string): LongInt;

        { Data getting. }
        
        function GetProfilePointsSet(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetSelectedArea(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetBackgroundPoints(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetCurvePositions(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetRFactorIntervals(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetSpecialCurveParameters(var CP: Curve_parameters; var
            ErrMsg: string): LongInt;
        { Returns list of curve (specimen) parameters. }
        function GetSpecimenList(var Points: TMSCRSpecimenList;
            var ErrMsg: string): LongInt;
//{$IFDEF FIT}
        { Returns list of components containing points of curves (specimens). }
        function GetCurvesList(var Points: TSelfCopiedCompList;
            var ErrMsg: string): LongInt;
//{$ELSE}
        { Simplified way of transmission via network. }
        
        function GetSpecimenCount(
            var Count: LongInt; var ErrMsg: string): LongInt;
        function GetSpecimenPoints(SpecIndex: LongInt;
            var Points: TPointsSet; var Name: string; var ErrMsg: string): LongInt;
        function GetSpecimenParameterCount(SpecIndex: LongInt;
            var Count: LongInt; var ErrMsg: string): LongInt;
        function GetSpecimenParameter(SpecIndex: LongInt; ParamIndex: LongInt;
            var Name: string; var Value: Double; var Type_: LongInt;
            var ErrMsg: string): LongInt;
        function SetSpecimenParameter(SpecIndex: LongInt; ParamIndex: LongInt;
            Value: Double; var ErrMsg: string): LongInt;
//{$ENDIF}
        function GetCalcProfilePointsSet(var Points: TPointsSet;
            var ErrMsg: string): LongInt;
        function GetDeltaProfilePointsSet(var Points: TPointsSet;
            var ErrMsg: string): LongInt;

        { Wrappers to server attributes. WSDL does not support properties. }
        //property MaxRFactor: Double read GetMaxRFactor write SetMaxRFactor;
        //property BackFactor: Double read GetBackFactor write SetBackFactor;
        //property CurveThresh: Double read GetCurveThresh write SetCurveThresh;
        //property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;
        //property State: TFitServerState read GetState;
        //property WaveLength: Double read GetWaveLength write SetWaveLength;

        property RecreateServer: TRecreateServer
            read FRecreateServer write FRecreateServer;
        property Server: TFitServer read FServer write FServer;
    end;

implementation

uses Main;

{=========================== TFitServerStub ===================================}
function TFitServerStub.SmoothProfile(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SmoothProfile;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SubtractAllBackground(
    Auto: Boolean; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.SubtractAllBackground(Auto);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.DoAllAutomatically(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.DoAllAutomatically;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindGausses(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindGausses;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindGaussesAgain(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindGaussesAgain;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindGaussesSequentially(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindGaussesSequentially;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindPeakBounds(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindPeakBounds;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindBackPoints(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindBackPoints;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.FindPeakPositions(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.FindPeakPositions;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AllPointsAsPeakPositions(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.AllPointsAsPeakPositions;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.CreateSpecimenList(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.CreateSpecimenList;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SelectArea(
    StartPointIndex, StopPointIndex: LongInt; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SelectArea(StartPointIndex, StopPointIndex);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.ReturnToTotalProfile(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.ReturnToTotalProfile;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.StopAsyncOper(var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.StopAsyncOper;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AsyncOper(var ErrMsg: string): Boolean;
begin
    try
        Result := False;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := False; ErrMsg := '';
        Result := Server.AsyncOper;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := False;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := False;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetCalcTimeStr(var ErrMsg: string): string;
begin
    try
        Result := '';
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := ''; ErrMsg := '';
        Result := Server.GetCalcTimeStr;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := '';
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := '';
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetRFactorStr(var ErrMsg: string): string;
begin
    try
        Result := '';
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := ''; ErrMsg := '';
        Result := Server.GetRFactorStr;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := '';
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := '';
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetAbsRFactorStr(var ErrMsg: string): string;
begin
    try
        Result := '';
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := ''; ErrMsg := '';
        Result := Server.GetAbsRFactorStr;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := '';
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := '';
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSqrRFactorStr(var ErrMsg: string): string;
begin
    try
        Result := '';
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := ''; ErrMsg := '';
        Result := Server.GetSqrRFactorStr;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := '';
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := '';
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSelectedAreaMode: Boolean;
begin
    try
        Result := False;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.SelectedAreaMode;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := False;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetMaxRFactor: Double;
begin
    try
        Result := 0;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.MaxRFactor;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := 0;
        end else RecreateServer;
    end;
end;

procedure TFitServerStub.SetMaxRFactor(AMaxRFactor: Double);
begin
    try
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Server.MaxRFactor := AMaxRFactor;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetBackFactor: Double;
begin
    try
        Result := 0;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.BackFactor;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := 0;
        end else RecreateServer;
    end;
end;

procedure TFitServerStub.SetBackFactor(ABackFactor: Double);
begin
    try
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Server.BackFactor := ABackFactor;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetCurveThresh: Double;
begin
    try
        Result := 0;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.CurveThresh;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := 0;
        end else RecreateServer;
    end;
end;

procedure TFitServerStub.SetCurveThresh(ACurveThresh: Double);
begin
    try
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Server.CurveThresh := ACurveThresh;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetCurveType: TCurveTypeId;
begin
    try
        Result := Server.CurveTypeId;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

procedure TFitServerStub.SetCurveType(ACurveType: TCurveTypeId);
begin
    try
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Server.CurveTypeId := ACurveType;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetSpecialCurveParameters(
    ACurveExpr: string;
    CP: Curve_parameters;   //  ravenstvo nil oznachaet
                            //  pervonachal'nuyu initsializatsiyu
    var ErrMsg: string
    ): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        if Assigned(CP) then
            Server.SetSpecialCurveParameters(
                ACurveExpr, Curve_parameters(CP.GetCopy))
        else
            Server.SetSpecialCurveParameters(ACurveExpr, nil);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetProfilePointsSet(
    APointsSet: TTitlePointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SetProfilePointsSet(TTitlePointsSet(APointsSet.GetCopy));
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetBackgroundPointsSet(
    ABackgroundPoints: TTitlePointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SetBackgroundPointsSet(
            TTitlePointsSet(ABackgroundPoints.GetCopy));
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetCurvePositions(
    ACurvePositions: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SetCurvePositions(TPointsSet(ACurvePositions.GetCopy));
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetRFactorIntervals(
    ARFactorIntervals: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        ErrMsg := Server.SetRFactorIntervals(
            TPointsSet(ARFactorIntervals.GetCopy));
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetProfilePointsSet(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetProfilePointsSet;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSelectedArea(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetSelectedArea;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetCalcProfilePointsSet(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetCalcProfilePointsSet;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetDeltaProfilePointsSet(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetDeltaProfilePointsSet;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetBackgroundPoints(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetBackgroundPoints;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetCurvePositions(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetCurvePositions;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetRFactorIntervals(
    var Points: TPointsSet; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetRFactorIntervals;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSpecialCurveParameters(
    var CP: Curve_parameters; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        CP := Server.GetSpecialCurveParameters;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSpecimenList(
    var Points: TMSCRSpecimenList; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetSpecimenList;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

//{$IFDEF FIT}
function TFitServerStub.GetCurvesList(
    var Points: TSelfCopiedCompList; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Points := Server.GetCurvesList;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;
//{$ELSE}
function TFitServerStub.GetSpecimenCount(
    var Count: LongInt; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Count := Server.GetSpecimenCount;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSpecimenPoints(SpecIndex: LongInt;
    var Points: TPointsSet; var Name: string; var ErrMsg: string): LongInt;
var CPS: TNamedPointsSet;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        CPS := Server.GetSpecimenPoints(SpecIndex);
        Points := CPS; Name := CPS.GetTypeName; Result := 0;
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSpecimenParameterCount(SpecIndex: LongInt;
    var Count: LongInt; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Count := Server.GetSpecimenParameterCount(SpecIndex);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.SetSpecimenParameter(
    SpecIndex: LongInt; ParamIndex: LongInt;
    Value: Double; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.SetSpecimenParameter(SpecIndex, ParamIndex, Value);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetSpecimenParameter(SpecIndex: LongInt;
    ParamIndex: LongInt; var Name: string; var Value: Double;
    var Type_: LongInt; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.GetSpecimenParameter(SpecIndex, ParamIndex, Name, Value, Type_);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;
//{$ENDIF}

function TFitServerStub.GetState: TFitServerState;
begin
    try
        Result := ProfileWaiting;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.State;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := ProfileWaiting;
        end else RecreateServer;
    end;
end;

function TFitServerStub.GetWaveLength: Double;
begin
    try
        Result := 0;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := Server.WaveLength;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := 0;
        end else RecreateServer;
    end;
end;

procedure TFitServerStub.SetWaveLength(AWaveLength: Double);
begin
    try
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Server.WaveLength := AWaveLength;
    except
        on E: Exception do
        begin
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AddPointToData(
    XValue, YValue: Double; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.AddPointToData(XValue, YValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AddPointToBackground(
    XValue, YValue: Double; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.AddPointToBackground(XValue, YValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AddPointToRFactorIntervals(
    XValue, YValue: Double; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.AddPointToRFactorIntervals(XValue, YValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.AddPointToCurvePositions(
    XValue, YValue: Double; var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.AddPointToCurvePositions(XValue, YValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.ReplacePointInData(PrevXValue, PrevYValue, NewXValue, NewYValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.ReplacePointInRFactorIntervals(
            PrevXValue, PrevYValue, NewXValue, NewYValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

function TFitServerStub.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    var ErrMsg: string): LongInt;
begin
    try
        Result := -2;
        Assert(Assigned(RecreateServer));
    except
        on E: Exception do
        begin WriteLog(CreateErrorMessage(E.Message), Fatal); Exit; end
        else Exit;
    end;

    try
        Result := 0; ErrMsg := '';
        Server.ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue);
    except
        on E: EUserException do
        begin
            ErrMsg := E.Message;
            Result := -1;
        end;
        on E: Exception do
        begin
            ErrMsg := E.Message;
            WriteLog(CreateErrorMessage(E.Message), Fatal);
            RecreateServer;
            Result := -2;
        end else RecreateServer;
    end;
end;

end.



