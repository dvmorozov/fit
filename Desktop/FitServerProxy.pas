
unit FitServerProxy;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitClientStub, MyExceptions;

type
    TFitServerProxy = class(TObject)
    protected
        FFitStub: TFitClientStub;

    public
        procedure ShowCurMin(Min: Double);
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
        //  pryamoy ukazatel', poka prilozheniya ne razdeleny;
        //  ne dopuskaetsya ravenstvo nil
        property FitStub: TFitClientStub read FFitStub write FFitStub;
    end;

implementation

procedure TFitServerProxy.ShowCurMin(Min: Double);
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.ShowCurMin(Min);
end;

procedure TFitServerProxy.Done;
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.Done;
end;

procedure TFitServerProxy.FindPeakBoundsDone;
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.FindPeakBoundsDone;
end;

procedure TFitServerProxy.FindBackPointsDone;
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.FindBackPointsDone;
end;

procedure TFitServerProxy.FindPeakPositionsDone;
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.FindPeakPositionsDone;
end;

end.



