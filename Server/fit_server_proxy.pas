{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of proxy class transmitting messages from server back to client.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit fit_server_proxy;

{$MODE Delphi}

interface

uses SysUtils, fit_client_stub, MyExceptions, int_client_callback;

type
    { Proxy class transmitting messages from server back to client. }
    TFitServerProxy = class(TInterfacedObject, IClientCallback)
    protected
        FFitClientStub: TFitClientStub;

    public
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
        { Pointer to the client part receiving messages. }
        property FitClientStub: TFitClientStub
            read FFitClientStub write FFitClientStub;
    end;

implementation

procedure TFitServerProxy.ShowCurMin(Min: Double);
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.ShowCurMin(Min);
end;

procedure TFitServerProxy.ShowProfile;
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.ShowProfile;
end;

procedure TFitServerProxy.Done;
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.Done;
end;

procedure TFitServerProxy.FindPeakBoundsDone;
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.FindPeakBoundsDone;
end;

procedure TFitServerProxy.FindBackPointsDone;
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.FindBackPointsDone;
end;

procedure TFitServerProxy.FindPeakPositionsDone;
begin
    try
        Assert(Assigned(FitClientStub));
    except
        on E: EAssertionFailed do raise EUserException.Create(E.Message)
        else raise;
    end;
    FitClientStub.FindPeakPositionsDone;
end;

end.



