// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The adapter that fits on the Python sidecar, with no Python running.)

WHAT IS BEING TESTED. Not lmfit, and not the sidecar: the adapter between them
and the engine. It describes a live task as a fit problem, posts it, and writes
the answer back into the task - and each of those is a decision that can be wrong
in a way no fit would reveal. A rejection reported as success leaves the user
looking at unfitted curves labelled as fitted.

The whole class sat at zero covered lines because the request was built inline
around a TFPHTTPClient. TPythonFitBackend.Post is now the seam; see
tests/mocks/mock_python_transport. Nothing here opens a socket and nothing runs
an optimiser, so these are unit tests.

WHY THE TASK IS REAL. The problem sent to Python is built from a TFitTask, and
what a task reports about its own curves is most of what can go wrong here. A
task is built the way testcase_fit_marshalling builds one - from a problem - so
the adapter is exercised against the real thing rather than a stand-in for it.
}
unit testcase_python_backend;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_task, fit_problem_json, fit_task_marshalling, int_fit_backend,
    gauss_points_set, SimpMath, mock_python_transport, mock_server_transport,
    server_fit_backend;

type
    TPythonBackendTest = class(TTestCase)
    private
        FBackend: TMockPythonBackend;
        FTask: TFitTask;
        { A one-Gaussian task, the smallest thing with a curve to fit. }
        function NewTask: TFitTask;
        { Runs Fit and returns the error message, or '' when it did not raise. }
        function FitAndCatch: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What it calls itself, and where it posts.
        procedure TheBackendNamesTheAlgorithmItUses;
        procedure ATrailingSlashInTheUrlIsNotDoubled;
        procedure SeveralTrailingSlashesAreAllRemoved;
        procedure TheFitGoesToTheFitRoute;

        //  What it sends.
        procedure TheProblemSentCarriesTheProfile;
        procedure TheProblemSentCarriesTheFormula;
        procedure OnePostPerFit;

        //  What it makes of the answer.
        procedure AGoodReplyIsTheResult;
        procedure AnUnreadableReplyIsRefused;
        procedure AnEmptyReplyIsRefused;
        procedure TheSidecarsOwnRejectionReachesTheCaller;
        procedure ARejectionIsNotReportedAsASuccess;
        procedure ARejectionWithNoMessageStillReports;
        procedure ATransportFailureIsNotSwallowed;

        //  What it writes back.
        procedure TheFittedParametersReachTheTask;
    end;

    { The other remote backend. Same seam, same failure modes, different
      defaults - and its own class because what the two get WRONG differs: this
      one has a default URL and no timeouts. }
    TServerBackendTest = class(TTestCase)
    private
        FBackend: TMockServerBackend;
        FTask: TFitTask;
        function NewTask: TFitTask;
        function FitAndCatch: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnEmptyUrlFallsBackToTheLocalDefault;
        procedure ATrailingSlashIsTrimmed;
        procedure TheNameCarriesTheUrlItWillUse;
        procedure TheFitGoesToTheFitRoute;
        procedure TheProblemSentCarriesTheProfile;

        procedure AServerThatAnswersHealthIsAvailable;
        procedure AServerThatDoesNotAnswerIsNotAvailable;
        procedure AvailabilityIsAskedOfTheHealthRoute;

        procedure AGoodReplyIsTheResult;
        procedure AnUnreadableReplyIsRefused;
        procedure AFailedFitIsRefusedWithItsCode;
        procedure ATransportFailureIsNotSwallowed;
        procedure TheFittedParametersReachTheTask;
    end;

implementation

const
    URL = 'http://127.0.0.1:8788';

procedure TPythonBackendTest.SetUp;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    FBackend := TMockPythonBackend.Create(URL);
    FTask := NewTask;
end;

procedure TPythonBackendTest.TearDown;
begin
    FreeAndNil(FTask);
    FreeAndNil(FBackend);
end;

function TPythonBackendTest.NewTask: TFitTask;
var
    P: TFitProblem;
    x: double;
    n: integer;
begin
    P := Default(TFitProblem);
    n := 0;
    x := 0;
    while x <= 20 + 1E-9 do
    begin
        SetLength(P.ProfileX, n + 1);
        SetLength(P.ProfileY, n + 1);
        P.ProfileX[n] := x;
        P.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
        Inc(n);
        x := x + 0.5;
    end;
    P.PositionsX := TDoubleArray.Create(10);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    P.MinimizerKind := 0;
    Result := BuildTaskFromProblem(P);
end;

function TPythonBackendTest.FitAndCatch: string;
begin
    Result := '';
    try
        FBackend.Fit(FTask);
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

{ ---- identity and address -------------------------------------------------- }

procedure TPythonBackendTest.TheBackendNamesTheAlgorithmItUses;
begin
    //  The name is shown in the interface beside the native minimizer, so it has
    //  to say which algorithm is being run and not merely "Python".
    AssertTrue('it names Python: ' + FBackend.Name,
        Pos('Python', FBackend.Name) > 0);
    AssertTrue('and the algorithm', Pos('lmfit', FBackend.Name) > 0);
end;

procedure TPythonBackendTest.ATrailingSlashInTheUrlIsNotDoubled;
var
    B: TMockPythonBackend;
begin
    //  The URL comes from a setting a user typed, and '//fit' is a different path
    //  to most servers - a 404 whose cause is a character nobody can see.
    B := TMockPythonBackend.Create(URL + '/');
    try
        B.Reply('{"errorCode":0,"rFactor":0.1}');
        B.Fit(FTask);
        AssertEquals('one slash', URL + '/fit', B.LastUrl);
    finally
        B.Free;
    end;
end;

procedure TPythonBackendTest.SeveralTrailingSlashesAreAllRemoved;
var
    B: TMockPythonBackend;
begin
    //  Trimming one slash is the version of this that a copied-and-pasted URL
    //  defeats.
    B := TMockPythonBackend.Create(URL + '///');
    try
        B.Reply('{"errorCode":0,"rFactor":0.1}');
        B.Fit(FTask);
        AssertEquals('one slash', URL + '/fit', B.LastUrl);
    finally
        B.Free;
    end;
end;

procedure TPythonBackendTest.TheFitGoesToTheFitRoute;
begin
    FBackend.Reply('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertEquals('the fit route', URL + '/fit', FBackend.LastUrl);
end;

{ ---- what is sent ---------------------------------------------------------- }

procedure TPythonBackendTest.TheProblemSentCarriesTheProfile;
begin
    //  Without the data there is nothing to fit, and the sidecar would answer
    //  with a rejection the user reads as "the Python backend is broken".
    FBackend.Reply('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertTrue('the profile abscissae', Pos('"profileX"', FBackend.LastBody) > 0);
    AssertTrue('and the ordinates', Pos('"profileY"', FBackend.LastBody) > 0);
end;

procedure TPythonBackendTest.TheProblemSentCarriesTheFormula;
begin
    //  THE MODEL ITSELF. The Python side has no curve library: it is handed the
    //  expression and evaluates it, so a problem sent without one fits nothing at
    //  all.
    FBackend.Reply('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertTrue('an expression is present: ' + Copy(FBackend.LastBody, 1, 120),
        Pos('"expression"', FBackend.LastBody) > 0);
end;

procedure TPythonBackendTest.OnePostPerFit;
begin
    //  A retry loop here would double every fit's cost against a sidecar that
    //  already bounds its own effort.
    FBackend.Reply('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertEquals('one request', 1, FBackend.Calls);
end;

{ ---- what is made of the answer -------------------------------------------- }

procedure TPythonBackendTest.AGoodReplyIsTheResult;
var
    R: TFitResult;
begin
    FBackend.Reply('{"errorCode":0,"rFactor":0.0425}');
    R := FBackend.Fit(FTask);
    AssertEquals('no error', 0, R.ErrorCode);
    AssertEquals('the R-factor the sidecar reported', 0.0425, R.RFactor, 1E-9);
end;

procedure TPythonBackendTest.AnUnreadableReplyIsRefused;
begin
    //  Not JSON at all - a proxy error page, say. Treating it as an empty result
    //  would report a successful fit with an R-factor of zero, which reads as a
    //  perfect fit.
    FBackend.Reply('<html>502 Bad Gateway</html>');
    AssertTrue('it refused: ' + FitAndCatch, FitAndCatch <> '');
end;

procedure TPythonBackendTest.AnEmptyReplyIsRefused;
begin
    FBackend.Reply('');
    AssertTrue('it refused', FitAndCatch <> '');
end;

procedure TPythonBackendTest.TheSidecarsOwnRejectionReachesTheCaller;
var
    Msg: string;
begin
    //  The sidecar knows why it refused - a formula it cannot evaluate, a
    //  parameter with no bounds - and that sentence is the only useful thing the
    //  user can be told. Replacing it with a generic failure throws away the
    //  answer to the question the user is about to ask.
    FBackend.Reply('{"errorCode":7,"error":"unknown function frobnicate()"}');
    Msg := FitAndCatch;
    AssertTrue('it refused', Msg <> '');
    AssertTrue('and said why: ' + Msg, Pos('frobnicate', Msg) > 0);
end;

procedure TPythonBackendTest.ARejectionIsNotReportedAsASuccess;
var
    Raised: boolean;
begin
    //  A non-zero error code with a plausible R-factor beside it. Returning the
    //  record instead of raising is the failure that leaves the user looking at
    //  unfitted curves presented as fitted.
    FBackend.Reply('{"errorCode":3,"rFactor":0.01,"error":"refused"}');
    Raised := False;
    try
        FBackend.Fit(FTask);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('a rejection raises', Raised);
end;

procedure TPythonBackendTest.ARejectionWithNoMessageStillReports;
var
    Msg: string;
begin
    //  A refusal with no explanation still has to be a refusal. Falling back to
    //  the whole body is right here: it is at least evidence.
    FBackend.Reply('{"errorCode":5}');
    Msg := FitAndCatch;
    AssertTrue('it refused', Msg <> '');
end;

procedure TPythonBackendTest.ATransportFailureIsNotSwallowed;
var
    Msg: string;
begin
    //  The sidecar is not answering - it died on start-up for want of a library,
    //  which is the ordinary way this backend is unavailable. The failure must
    //  reach the caller rather than becoming an empty result.
    FBackend.FailWith('connection refused');
    Msg := FitAndCatch;
    AssertTrue('it was reported: ' + Msg, Msg <> '');
end;

{ ---- what is written back -------------------------------------------------- }

procedure TPythonBackendTest.TheFittedParametersReachTheTask;
var
    Q: TFitProblem;
    Before, After: double;

    { The named parameter of the task's first seed curve. }
    function SeedParam(const AProblem: TFitProblem; const AName: string;
        out AValue: double): boolean;
    var
        j: integer;
    begin
        Result := False;
        if Length(AProblem.Curves) = 0 then
            Exit;
        for j := 0 to High(AProblem.Curves[0].Params) do
            if AProblem.Curves[0].Params[j].Name = AName then
            begin
                AValue := AProblem.Curves[0].Params[j].Value;
                Exit(True);
            end;
    end;

begin
    //  THE POINT OF THE WHOLE ADAPTER. The fitted values have to land in the
    //  live task, because everything downstream - the curves drawn, the parameter
    //  table, the statistics - is built from the task and not from the reply. An
    //  adapter that returned the right R-factor and wrote nothing back would look
    //  entirely successful and change nothing on screen.
    Q := BuildProblemFromTask(FTask);
    AssertTrue('the seed amplitude is readable', SeedParam(Q, 'A', Before));

    //  Answer with the same curve shape the request carried, one parameter moved
    //  to a value the seed cannot already hold.
    FBackend.Reply('{"errorCode":0,"rFactor":0.01,"curves":[{"params":[' +
        '{"name":"A","value":123.5},{"name":"sigma","value":2.25}]}]}');
    FBackend.Fit(FTask);

    Q := BuildProblemFromTask(FTask);
    AssertTrue('the amplitude is still readable', SeedParam(Q, 'A', After));
    AssertEquals('and it is the fitted value', 123.5, After, 1E-6);
    AssertTrue('which is not what was sent', Abs(After - Before) > 1E-6);
end;

{ ======================== the compute-server backend ======================== }

procedure TServerBackendTest.SetUp;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    FBackend := TMockServerBackend.Create('http://compute.example:9000');
    FTask := NewTask;
end;

procedure TServerBackendTest.TearDown;
begin
    FreeAndNil(FTask);
    FreeAndNil(FBackend);
end;

function TServerBackendTest.NewTask: TFitTask;
var
    P: TFitProblem;
    x: double;
    n: integer;
begin
    P := Default(TFitProblem);
    n := 0;
    x := 0;
    while x <= 20 + 1E-9 do
    begin
        SetLength(P.ProfileX, n + 1);
        SetLength(P.ProfileY, n + 1);
        P.ProfileX[n] := x;
        P.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
        Inc(n);
        x := x + 0.5;
    end;
    P.PositionsX := TDoubleArray.Create(10);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    P.MinimizerKind := 0;
    Result := BuildTaskFromProblem(P);
end;

function TServerBackendTest.FitAndCatch: string;
begin
    Result := '';
    try
        FBackend.Fit(FTask);
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TServerBackendTest.AnEmptyUrlFallsBackToTheLocalDefault;
var
    B: TMockServerBackend;
begin
    //  An empty setting means "the server on this machine", not "no server":
    //  the compute server's own default port is where a local one is listening.
    B := TMockServerBackend.Create('');
    try
        AssertTrue('a URL was chosen: ' + B.Url, B.Url <> '');
        AssertTrue('and it is loopback', Pos('127.0.0.1', B.Url) > 0);
    finally
        B.Free;
    end;
end;

procedure TServerBackendTest.ATrailingSlashIsTrimmed;
var
    B: TMockServerBackend;
begin
    B := TMockServerBackend.Create('http://compute.example:9000/');
    try
        B.ReplyToPost('{"errorCode":0,"rFactor":0.1}');
        B.Fit(FTask);
        AssertEquals('one slash', 'http://compute.example:9000/fit',
            B.LastPostUrl);
    finally
        B.Free;
    end;
end;

procedure TServerBackendTest.TheNameCarriesTheUrlItWillUse;
begin
    //  Shown in the interface beside the other backends. WHICH server is the
    //  only thing that distinguishes two of these, so a name without it is
    //  ambiguous exactly when it matters.
    AssertTrue('the name names the server: ' + FBackend.Name,
        Pos('compute.example:9000', FBackend.Name) > 0);
end;

procedure TServerBackendTest.TheFitGoesToTheFitRoute;
begin
    FBackend.ReplyToPost('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertEquals('the fit route', 'http://compute.example:9000/fit',
        FBackend.LastPostUrl);
end;

procedure TServerBackendTest.TheProblemSentCarriesTheProfile;
begin
    FBackend.ReplyToPost('{"errorCode":0,"rFactor":0.1}');
    FBackend.Fit(FTask);
    AssertTrue('the abscissae', Pos('"profileX"', FBackend.LastBody) > 0);
    AssertTrue('the ordinates', Pos('"profileY"', FBackend.LastBody) > 0);
    AssertTrue('and the model', Pos('"expression"', FBackend.LastBody) > 0);
end;

procedure TServerBackendTest.AServerThatAnswersHealthIsAvailable;
begin
    FBackend.ReplyToGet('{"ok":true}');
    AssertTrue('available', FBackend.IsAvailable);
end;

procedure TServerBackendTest.AServerThatDoesNotAnswerIsNotAvailable;
begin
    //  FALSE, not an exception. This is asked on the desktop's start-up path to
    //  decide whether remote fitting is offered at all, and a server not being
    //  there is the ordinary case, not a fault.
    FBackend.FailGetWith('connection refused');
    AssertFalse('not available', FBackend.IsAvailable);
end;

procedure TServerBackendTest.AvailabilityIsAskedOfTheHealthRoute;
begin
    //  The health route and not the fit route: asking /fit would submit an empty
    //  problem to a working server just to find out that it is working.
    FBackend.ReplyToGet('{"ok":true}');
    FBackend.IsAvailable;
    AssertEquals('the health route', 'http://compute.example:9000/health',
        FBackend.LastGetUrl);
    AssertEquals('and nothing was posted', 0, FBackend.Posts);
end;

procedure TServerBackendTest.AGoodReplyIsTheResult;
var
    R: TFitResult;
begin
    FBackend.ReplyToPost('{"errorCode":0,"rFactor":0.0625}');
    R := FBackend.Fit(FTask);
    AssertEquals('no error', 0, R.ErrorCode);
    AssertEquals('the R-factor', 0.0625, R.RFactor, 1E-9);
end;

procedure TServerBackendTest.AnUnreadableReplyIsRefused;
begin
    FBackend.ReplyToPost('<html>504 Gateway Timeout</html>');
    AssertTrue('it refused', FitAndCatch <> '');
end;

procedure TServerBackendTest.AFailedFitIsRefusedWithItsCode;
var
    Msg: string;
begin
    //  THE CODE, because that is all this backend gets: unlike the Python
    //  sidecar the compute server sends no message with its refusal, so the
    //  number is the only thing that distinguishes one failure from another.
    FBackend.ReplyToPost('{"errorCode":4,"rFactor":0.0}');
    Msg := FitAndCatch;
    AssertTrue('it refused', Msg <> '');
    AssertTrue('and said which code: ' + Msg, Pos('4', Msg) > 0);
end;

procedure TServerBackendTest.ATransportFailureIsNotSwallowed;
begin
    //  Unlike IsAvailable, a FIT against an unreachable server is a failure the
    //  user has to be told about - they asked for it.
    FBackend.FailPostWith('connection reset');
    AssertTrue('it was reported', FitAndCatch <> '');
end;

procedure TServerBackendTest.TheFittedParametersReachTheTask;
var
    Q: TFitProblem;
    Before, After: double;

    function SeedParam(const AProblem: TFitProblem; const AName: string;
        out AValue: double): boolean;
    var
        j: integer;
    begin
        Result := False;
        if Length(AProblem.Curves) = 0 then
            Exit;
        for j := 0 to High(AProblem.Curves[0].Params) do
            if AProblem.Curves[0].Params[j].Name = AName then
            begin
                AValue := AProblem.Curves[0].Params[j].Value;
                Exit(True);
            end;
    end;

begin
    Q := BuildProblemFromTask(FTask);
    AssertTrue('the seed amplitude is readable', SeedParam(Q, 'A', Before));
    FBackend.ReplyToPost('{"errorCode":0,"rFactor":0.01,"curves":[{"params":[' +
        '{"name":"A","value":77.25},{"name":"sigma","value":1.75}]}]}');
    FBackend.Fit(FTask);
    Q := BuildProblemFromTask(FTask);
    AssertTrue('still readable', SeedParam(Q, 'A', After));
    AssertEquals('and it is the fitted value', 77.25, After, 1E-6);
    AssertTrue('which is not the seed', Abs(After - Before) > 1E-6);
end;

initialization
    //  A unit test: no sidecar, no socket, no optimiser - the task is real and
    //  the transport is not.
    RegisterTest('unit', TPythonBackendTest);
    RegisterTest('unit', TServerBackendTest);
end.
