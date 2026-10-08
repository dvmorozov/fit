// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A kept connection the server has closed is opened again, not fatal.)

THE DEFECT. The compute server closes a kept connection that has sat idle for
KEPT_CONNECTION_IDLE_MS, as HTTP/1.1 allows. The client's next read on it writes
to a socket the peer has closed, and on macOS and Linux that raises SIGPIPE,
whose default action ends the process: exit 141, no exception, no log line.
THttpFitService.Fetch already treats a dropped connection as ordinary - it opens
another and asks again - but that code was never reached. Found refitting
FullProfile.fitproj from the private suite, where the reading connection sat idle
through a 71-second fit (findings.md).

THROUGH THE REAL THINGS: the server class fit_server runs, on a socket, and the
client's own Fetch. Integration, because it crosses a socket and waits out the
server's idle limit. Before the fix this test does not fail - the process running
it is killed.
}
unit testcase_kept_connection;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fphttpserver, httpdefs, ssockets,
    Sockets, BaseUnix, keep_alive_http_server, http_fit_service, broken_pipes,
    worker_process_harness, title_points_set, fphttpclient;

type
    { THttpFitService with its one read made callable: Fetch is the seam every
      read crosses, and protected because only a transport double needs it. }
    TFetchingService = class(THttpFitService)
    public
        function Read(const AUrl: string): string;
    end;

    { A kept connection that ends with a RESET rather than an orderly close:
      SO_LINGER at zero. A peer may end a connection either way, and a reset
      is the one after which the client's next write raises SIGPIPE at once -
      which is what killed the private suite after a 71-second fit. }
    TResettingConnection = class(TKeptHttpConnection)
    protected
        procedure SetupSocket; override;
    end;

    { The server fit_server runs, answering every request with {}; with
      AResets, its kept connections end with a reset. }
    TAnsweringServer = class(TKeepAliveHttpServer)
    protected
        function CreateConnection(Data: TSocketStream): TFPHTTPConnection; override;
        procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
            var AResponse: TFPHTTPConnectionResponse); override;
    public
        Resets: boolean;
        { How many connections it has accepted. }
        Connections: longint;
    end;

    TServerThread = class(TThread)
    public
        Server: TAnsweringServer;
    protected
        procedure Execute; override;
    end;

    TKeptConnectionTest = class(TTestCase)
    private
        { How SIGPIPE was handled when the test started, put back after it. }
        FInherited: SigActionRec;
        FServer: TAnsweringServer;
        FThread: TServerThread;
        FUrl: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
        procedure ReadTwiceAcrossTheIdleLimit(AResets: boolean);
    published
        procedure AReadAfterTheServerClosedTheConnectionOpensAnother;
        procedure AReadAfterTheServerResetTheConnectionOpensAnother;
    end;

    { WHAT THE KEEP-ALIVE SERVER IS FOR, on the loopback, in milliseconds: a
      client's requests share one connection unless the client asks to close
      it - which is what keeps a poll during a fit off the accept queue. }
    TKeepAliveServerTest = class(TTestCase)
    private
        FServer: TAnsweringServer;
        FThread: TServerThread;
        function UrlOf: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TwoRequestsShareOneKeptConnection;
        procedure AClientThatAsksToCloseGetsAConnectionEachTime;
    end;

    { THE SERVER'S SIDE: a client that asks for a fit and is gone - the window
      closed, the connection reset - before the reply is written. Writing it
      raised SIGPIPE in fit_server, and every session it served went with it.
      Through the real fit_server, started with SIGPIPE at its default, as the
      launcher or the Finder starts it: on macOS that signal goes to the
      PROCESS, so only what the program itself does at start-up decides it. }
    TGoneClientTest = class(TWorkerProcessTest)
    private
        FInherited: SigActionRec;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AClientGoneBeforeItsReplyDoesNotEndTheServer;
    end;

    { What every program does first, in this process: no peer, no wait. }
    TBrokenPipesTest = class(TTestCase)
    published
        procedure AProcessThatIgnoresBrokenPipesSaysSo;
    end;

implementation

const
    { A port nothing else in the suites uses. }
    TEST_PORT = 18791;

function TFetchingService.Read(const AUrl: string): string;
begin
    Result := Fetch(AUrl, 5000);
end;

procedure TResettingConnection.SetupSocket;
var
    Linger: TLinger;
begin
    inherited SetupSocket;
    Linger.l_onoff := 1;
    Linger.l_linger := 0;
    fpSetSockOpt(Socket.Handle, SOL_SOCKET, SO_LINGER, @Linger, SizeOf(Linger));
end;

function TAnsweringServer.CreateConnection(Data: TSocketStream): TFPHTTPConnection;
begin
    InterLockedIncrement(Connections);
    if Resets then
        Result := TResettingConnection.Create(Self, Data)
    else
        Result := inherited CreateConnection(Data);
end;

procedure TAnsweringServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
begin
    inherited HandleRequest(ARequest, AResponse);
    AResponse.Code := 200;
    AResponse.ContentType := 'application/json';
    AResponse.Content := '{}';
end;

procedure TServerThread.Execute;
begin
    //  Blocks in its accept loop until Active is cleared.
    Server.Active := True;
end;

procedure TKeptConnectionTest.SetUp;
var
    Default_: SigActionRec;
begin
    //  AS A PROCESS STARTED FROM THE FINDER HAS IT. The suites are started from
    //  PowerShell, whose runtime ignores SIGPIPE - and a child inherits an
    //  ignored signal - so without this the test passes for a reason the
    //  application never has: run directly from a shell, it was killed.
    FillChar(Default_, SizeOf(Default_), 0);
    Default_.sa_Handler := SigActionHandler(SIG_DFL);
    FPSigaction(SIGPIPE, @Default_, @FInherited);
    //  AND THEN WHAT EVERY PROGRAM'S MAIN DOES FIRST (broken_pipes; the script
    //  test network_boundary holds every program to it). Without this line the
    //  reset case kills the process with exit 141 - measured.
    IgnoreBrokenPipes;
    FServer := TAnsweringServer.Create(nil);
    FServer.Port := TEST_PORT;
    FServer.Threaded := True;
    //  So clearing Active ends the accept loop promptly.
    FServer.AcceptIdleTimeout := 100;
    FThread := TServerThread.Create(True);
    FThread.Server := FServer;
    FThread.Start;
    Sleep(300);
    FUrl := Format('http://127.0.0.1:%d/anything', [TEST_PORT]);
end;

procedure TKeptConnectionTest.TearDown;
begin
    FServer.Active := False;
    FThread.WaitFor;
    FreeAndNil(FThread);
    FreeAndNil(FServer);
    FPSigaction(SIGPIPE, @FInherited, nil);
end;

procedure TKeptConnectionTest.AReadAfterTheServerClosedTheConnectionOpensAnother;
begin
    ReadTwiceAcrossTheIdleLimit(False);
end;

procedure TKeptConnectionTest.AReadAfterTheServerResetTheConnectionOpensAnother;
begin
    ReadTwiceAcrossTheIdleLimit(True);
end;

procedure TKeptConnectionTest.ReadTwiceAcrossTheIdleLimit(AResets: boolean);
var
    Client: TFetchingService;
begin
    FServer.Resets := AResets;
    Client := TFetchingService.Create('http://127.0.0.1:' + IntToStr(TEST_PORT));
    try
        AssertEquals('the first read', '{}', Trim(Client.Read(FUrl)));
        AssertEquals('over one connection', 1, Client.ConnectionsOpened);
        //  Past the server's idle limit: it has closed the kept connection.
        Sleep(KEPT_CONNECTION_IDLE_MS + 1500);
        AssertEquals('the next read is answered', '{}', Trim(Client.Read(FUrl)));
    finally
        Client.Free;
    end;
end;

const
    { Its own port, so the slower test's connections are not counted here. }
    KEEP_ALIVE_PORT = 18792;

procedure TKeepAliveServerTest.SetUp;
begin
    FServer := TAnsweringServer.Create(nil);
    FServer.Port := KEEP_ALIVE_PORT;
    FServer.Threaded := True;
    FServer.AcceptIdleTimeout := 100;
    FThread := TServerThread.Create(True);
    FThread.Server := FServer;
    FThread.Start;
    Sleep(200);
end;

procedure TKeepAliveServerTest.TearDown;
begin
    FServer.Active := False;
    FThread.WaitFor;
    FreeAndNil(FThread);
    FreeAndNil(FServer);
end;

function TKeepAliveServerTest.UrlOf: string;
begin
    Result := Format('http://127.0.0.1:%d/anything', [KEEP_ALIVE_PORT]);
end;

procedure TKeepAliveServerTest.TwoRequestsShareOneKeptConnection;
var
    Client: TFPHTTPClient;
begin
    Client := TFPHTTPClient.Create(nil);
    try
        Client.KeepConnection := True;
        AssertEquals('{}', Trim(Client.Get(UrlOf)));
        AssertEquals('{}', Trim(Client.Get(UrlOf)));
        AssertEquals('one connection for both', 1, FServer.Connections);
    finally
        Client.Free;
    end;
end;

procedure TKeepAliveServerTest.AClientThatAsksToCloseGetsAConnectionEachTime;
var
    Client: TFPHTTPClient;
begin
    //  A client not keeping its connection sends "Connection: close", and the
    //  server ends the connection with that reply.
    Client := TFPHTTPClient.Create(nil);
    try
        Client.KeepConnection := False;
        AssertEquals('{}', Trim(Client.Get(UrlOf)));
        AssertEquals('{}', Trim(Client.Get(UrlOf)));
        AssertEquals('a connection each', 2, FServer.Connections);
    finally
        Client.Free;
    end;
end;

procedure TGoneClientTest.SetUp;
var
    Default_: SigActionRec;
begin
    //  The server is started with SIGPIPE at its default - the suites run
    //  under PowerShell, which ignores it, and a child would inherit that.
    FillChar(Default_, SizeOf(Default_), 0);
    Default_.sa_Handler := SigActionHandler(SIG_DFL);
    FPSigaction(SIGPIPE, @Default_, @FInherited);
    try
        inherited SetUp;
    finally
        FPSigaction(SIGPIPE, @FInherited, nil);
    end;
end;

procedure TGoneClientTest.TearDown;
begin
    inherited TearDown;
end;

procedure TGoneClientTest.AClientGoneBeforeItsReplyDoesNotEndTheServer;
var
    Positions: TTitlePointsSet;
    S: TInetSocket;
    Linger: TLinger;
    Request, Base: string;
    Probe: TFPHTTPClient;
begin
    //  A model to fit, through the client: its fit is what the gone client
    //  asks for, and takes long enough that the reply comes after the reset.
    FSvc.SetProfilePointsSet(GaussianProfile);
    Positions := TTitlePointsSet.Create(nil);
    Positions.AddNewPoint(10, 100);
    FSvc.SetCurvePositions(Positions);
    Base := Format('http://127.0.0.1:%d', [WorkerTestPort]);
    //  A fresh server numbers its first problem 1; asked, not assumed.
    Probe := TFPHTTPClient.Create(nil);
    try
        AssertTrue('the problem is 1',
            Pos('"ok" : true', Probe.Get(Base + '/problems/1/settings')) > 0);
    finally
        Probe.Free;
    end;

    S := TInetSocket.Create('127.0.0.1', WorkerTestPort);
    try
        Request := 'POST /problems/1/actions/minimize-difference HTTP/1.1'#13#10 +
            'Host: 127.0.0.1'#13#10'Content-Length: 0'#13#10#13#10;
        S.WriteBuffer(Request[1], Length(Request));
        Linger.l_onoff := 1;
        Linger.l_linger := 0;
        fpSetSockOpt(S.Handle, SOL_SOCKET, SO_LINGER, @Linger, SizeOf(Linger));
    finally
        //  A reset, with the fit still running and its reply to come.
        S.Free;
    end;
    Sleep(4000);

    AssertTrue('fit_server is still running', FProc.Running);
    AssertTrue('and answering', FSvc.IsAvailable);
end;

procedure TBrokenPipesTest.AProcessThatIgnoresBrokenPipesSaysSo;
var
    Default_, Was, Now_: SigActionRec;
begin
    //  From the default, as a process started from the Finder has it.
    FillChar(Default_, SizeOf(Default_), 0);
    Default_.sa_Handler := SigActionHandler(SIG_DFL);
    FPSigaction(SIGPIPE, @Default_, @Was);
    try
        IgnoreBrokenPipes;
        FillChar(Now_, SizeOf(Now_), 0);
        FPSigaction(SIGPIPE, nil, @Now_);
        AssertTrue('SIGPIPE is ignored',
            Now_.sa_Handler = SigActionHandler(SIG_IGN));
    finally
        FPSigaction(SIGPIPE, @Was, nil);
    end;
end;

initialization
    RegisterTest('integration', TKeptConnectionTest);
    RegisterTest('unit', TBrokenPipesTest);
    //  In its own process, on the loopback, in milliseconds.
    RegisterTest('unit', TKeepAliveServerTest);
    RegisterTest('integration', TGoneClientTest);
end.
