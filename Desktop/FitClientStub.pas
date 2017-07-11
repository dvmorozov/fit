
unit FitClientStub;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils;//, FitClient;
  
type
    TFitClientStub = class(TObject)
    protected
        //  zaschita ot circular reference
        FFitClient: TObject;//TFitClient;
    public
    
        procedure ShowCurMin(Min: Double);
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        property FitClient: TObject //TFitClient
            read FFitClient write FFitClient;
    end;

implementation

uses FitClient;

procedure TFitClientStub.ShowCurMin(Min: Double);
begin
    Assert(Assigned(FitClient));
    TFitClient(FitClient).ShowCurMin(Min);
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



