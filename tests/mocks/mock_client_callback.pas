// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IClientCallback that only remembers what it was told.)

WHAT IT IS FOR. The engine reports progress by calling back: ShowCurMin as the
R-factor improves, ShowProfile when there is something new to draw, then Done.
Nothing in the suite had ever observed those calls, so the only way into
TFitService's progress paths was to run a real fit against a real client.

IT RECORDS AND DOES NOT ASSERT. A mock that failed inside its own callback would
report the failure from wherever - and on whatever thread - the engine happened to
call it, and the message would name the mock rather than the expectation. The test
asserts afterwards, on its own thread, against the log.

THE SEQUENCE IS THE CONTRACT, not the final value. A progress report that went UP
is the failure this exists to catch, and a mock keeping only the last minimum
could not see it.

See mock_support for the -SIcorba lifetime rule.
}
unit mock_client_callback;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, int_client_callback, mock_support;

type
    TMockClientCallback = class(TMockBase, IClientCallback)
    private
        FMinima: array of double;
    public
        //  IClientCallback
        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        { Every R-factor reported, in the order reported. }
        function MinimaCount: longint;
        function MinimumAt(AIndex: longint): double;
        { The last one reported, or a negative number when none was. }
        function LastMinimum: double;
        { True when every reported minimum was no worse than the one before it.
          An optimiser that reports a worse value has either lost its best result
          or is reporting the wrong variable. }
        function MinimaNeverGotWorse: boolean;
    end;

implementation

procedure TMockClientCallback.ShowCurMin(Min: double);
begin
    SetLength(FMinima, Length(FMinima) + 1);
    FMinima[High(FMinima)] := Min;
    FLog.Note('ShowCurMin', FloatToStr(Min));
end;

procedure TMockClientCallback.ShowProfile;
begin
    FLog.Note('ShowProfile');
end;

procedure TMockClientCallback.Done;
begin
    FLog.Note('Done');
end;

procedure TMockClientCallback.ComputeCurveBoundsDone;
begin
    FLog.Note('ComputeCurveBoundsDone');
end;

procedure TMockClientCallback.ComputeBackgroundPointsDone;
begin
    FLog.Note('ComputeBackgroundPointsDone');
end;

procedure TMockClientCallback.ComputeCurvePositionsDone;
begin
    FLog.Note('ComputeCurvePositionsDone');
end;

function TMockClientCallback.MinimaCount: longint;
begin
    Result := Length(FMinima);
end;

function TMockClientCallback.MinimumAt(AIndex: longint): double;
begin
    Result := FMinima[AIndex];
end;

function TMockClientCallback.LastMinimum: double;
begin
    if Length(FMinima) = 0 then
        Result := -1
    else
        Result := FMinima[High(FMinima)];
end;

function TMockClientCallback.MinimaNeverGotWorse: boolean;
var
    i: longint;
begin
    Result := True;
    for i := 1 to High(FMinima) do
        if FMinima[i] > FMinima[i - 1] then
        begin
            Result := False;
            Exit;
        end;
end;

end.
