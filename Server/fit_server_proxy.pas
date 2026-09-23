// SPDX-License-Identifier: GPL-3.0-or-later
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

uses fit_client_stub, int_client_callback, checks;

type
    { Proxy class transmitting messages from server back to client. }
    TFitServerProxy = class(TInterfacedObject, IClientCallback)
    protected
        FFitClientStub: TFitClientStub;

    public
        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;
        { Pointer to the client part receiving messages. }
        property FitClientStub: TFitClientStub read FFitClientStub write FFitClientStub;
    end;

implementation

{ WHY THERE IS NO try/except HERE.

  Every method used to wrap its precondition in

      except on E: EAssertionFailed do raise EUserException.Create(E.Message)

  which could not fire: CheckAssigned raises EInternalCheckFailed, which
  checks.pas keeps deliberately distinct from EUserException. So what left
  this unit was the internal check, by way of the `else raise` - while the
  code said the opposite, at length, in six places, and anyone auditing how
  faults reach the user would have counted this as a place where they do.

  The surviving behaviour is the right one, and it is now the stated one: a
  callback with no client behind it is this program being wrong about itself,
  not the user doing something unsupported. It is logged where it happens and
  raised as a defect. }

procedure TFitServerProxy.ShowCurMin(Min: double);
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.ShowCurMin(Min);
end;

procedure TFitServerProxy.ShowProfile;
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.ShowProfile;
end;

procedure TFitServerProxy.Done;
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.Done;
end;

procedure TFitServerProxy.ComputeCurveBoundsDone;
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.ComputeCurveBoundsDone;
end;

procedure TFitServerProxy.ComputeBackgroundPointsDone;
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.ComputeBackgroundPointsDone;
end;

procedure TFitServerProxy.ComputeCurvePositionsDone;
begin
    CheckAssigned(FitClientStub, 'the client callback');
    FitClientStub.ComputeCurvePositionsDone;
end;

end.
