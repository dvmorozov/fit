// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of stub class implementing callbacks to client.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_client_stub;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    int_client_callback;

type
    TFitClientStub = class(TInterfacedObject, IClientCallback)
    protected
        FFitClient: TObject;

    public
        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        property FitClient: TObject read FFitClient write FFitClient;
    end;

implementation

uses
    fit_client, checks;

procedure TFitClientStub.ShowCurMin(Min: double);
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).ShowCurMin(Min);
end;

procedure TFitClientStub.ShowProfile;
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).ShowProfile;
end;

procedure TFitClientStub.Done;
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).Done;
end;

procedure TFitClientStub.ComputeCurveBoundsDone;
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).ComputeCurveBoundsDone;
end;

procedure TFitClientStub.ComputeBackgroundPointsDone;
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).ComputeBackgroundPointsDone;
end;

procedure TFitClientStub.ComputeCurvePositionsDone;
begin
    CheckAssigned(FitClient, 'the client this callback reports to');

    TFitClient(FitClient).ComputeCurvePositionsDone;
end;

end.
