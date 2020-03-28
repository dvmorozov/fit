unit ClientCallback;

{$mode delphi}

interface

uses
    Classes, SysUtils;

{ Defines callback functions called from server to client. }
type
    IClientCallback = interface
        procedure ShowCurMin(Min: double);
        procedure ShowProfile();
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;
    end;

implementation

end.
