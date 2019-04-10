{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of proxy class transmitting messages from server back to client.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit FitServerProxy;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses SysUtils, FitClientStub, MyExceptions, IntClientCallback,
     CBRCComponent;

type
    { Proxy class transmitting messages from server back to client. }
    TFitServerProxy = class(TCBRCComponent, IClientCallback)
    protected
        FFitStub: TFitClientStub;

    public
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
        { Pointer to the client part receiving messages. }
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

procedure TFitServerProxy.ShowProfile;
begin
    try
        Assert(Assigned(FitStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitStub.ShowProfile;
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



