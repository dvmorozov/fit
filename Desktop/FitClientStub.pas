{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of stub class implementing callbacks to client.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit FitClientStub;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses IntClientCallback, CBRCComponent;
  
type
    TFitClientStub = class(TCBRCComponent, IClientCallback)
    protected
        FFitClient: TObject;

    public
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        property FitClient: TObject
            read FFitClient write FFitClient;
    end;

implementation

uses FitClient;

procedure TFitClientStub.ShowCurMin(Min: Double);
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).ShowCurMin(Min);
end;

procedure TFitClientStub.ShowProfile;
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).ShowProfile;
end;

procedure TFitClientStub.Done;
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).Done;
end;

procedure TFitClientStub.FindPeakBoundsDone;
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).FindPeakBoundsDone;
end;

procedure TFitClientStub.FindBackPointsDone;
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).FindBackPointsDone;
end;

procedure TFitClientStub.FindPeakPositionsDone;
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).FindPeakPositionsDone;
end;

end.



