// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An HTTP server that keeps a connection open across requests.)

WHY THIS EXISTS. TFPHTTPServer in FPC 3.2.2 serves exactly ONE request per
connection: TFPHTTPConnection.HandleRequest reads a request, answers it and
returns, and the connection thread then frees the connection - which closes the
socket. There is no property to ask it for anything else.

That is invisible on the client side, because TFPHTTPClient with
KeepConnection reconnects silently inside the same client object. What it costs
is paid by the window: Animation Mode polls the running fit several times a
second from the UI thread, and every poll that has to connect anew goes onto
the accept queue of a server busy with the fit. Caught in the act, the main
thread sat in connect() while the fit ran and the window drew three frames in
fifty seconds.

WHAT THIS ADDS. The connection handles requests in a LOOP: after each reply it
waits, briefly, for the next request on the same socket. It stops when the peer
closes, when the peer asked for the connection to be closed, or when the socket
has been idle long enough that holding a thread for it is not worth it - and a
client whose kept connection is closed that way simply opens another, which is
the ordinary HTTP/1.1 contract.

The base class does the work of one request; this only decides whether to ask
it again.
}
unit keep_alive_http_server;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Sockets, ssockets, fphttpserver, httpdefs, httpprotocol;

const
    { How long a kept connection may sit idle before the server closes it and
      gives the thread back. Long enough to cover the window's polling cadence
      (and the pause between two user actions), short enough that the transient
      connections - every request the client sends with "Connection: close" -
      do not pile threads up behind them. }
    KEPT_CONNECTION_IDLE_MS = 5000;

type
    { One connection, serving as many requests as the peer sends over it. }
    TKeptHttpConnection = class(TFPHTTPConnection)
    private
        FKeepOpen: boolean;
        { True when another request is already on its way in. False when the
          peer closed, or nothing arrived before the idle timeout. }
        function AnotherRequestIsComing: boolean;
    protected
        procedure SetupSocket; override;
    public
        constructor Create(AServer: TFPCustomHTTPServer; ASocket: TSocketStream);
        procedure HandleRequest; override;
        { Cleared by the server when the request asked for the connection to be
          closed after its reply. }
        property KeepOpen: boolean read FKeepOpen write FKeepOpen;
    end;

    { The server that hands out the connection above. }
    TKeepAliveHttpServer = class(TFPHTTPServer)
    protected
        function CreateConnection(Data: TSocketStream): TFPHTTPConnection; override;
        procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
            var AResponse: TFPHTTPConnectionResponse); override;
    end;

implementation

const
    { Look at what is in the socket without taking it. Same value everywhere
      this program runs (Linux, Windows, macOS), and naming it here saves an
      ifdef over three units that each spell it differently. }
    PEEK_FLAG = 2;

constructor TKeptHttpConnection.Create(AServer: TFPCustomHTTPServer;
    ASocket: TSocketStream);
begin
    inherited Create(AServer, ASocket);
    FKeepOpen := True;
end;

procedure TKeptHttpConnection.SetupSocket;
begin
    inherited SetupSocket;
    //  A read that never returns would hold this connection's thread for as
    //  long as the process lives; with a timeout, a peer that stops talking
    //  costs the wait below and nothing more.
    try
        if Assigned(Socket) then
            Socket.IOTimeout := KEPT_CONNECTION_IDLE_MS;
    except
        //  An option this platform refuses is not a reason to refuse the
        //  request: without it the connection simply is not kept.
        on E: Exception do
            FKeepOpen := False;
    end;
end;

function TKeptHttpConnection.AnotherRequestIsComing: boolean;
var
    Peeked: byte;
begin
    Result := False;
    if not Assigned(Socket) then
        Exit;
    //  Blocks up to the socket's read timeout: >0 means a request is already
    //  arriving, 0 means the peer closed, <0 means the wait ran out.
    Result := fpRecv(Socket.Handle, @Peeked, 1, PEEK_FLAG) > 0;
end;

procedure TKeptHttpConnection.HandleRequest;
begin
    repeat
        //  One request and its reply, exactly as the base class does it - it
        //  does not close the socket, the connection thread does that when
        //  this returns.
        inherited HandleRequest;
    until (not FKeepOpen) or (not AnotherRequestIsComing);
end;

function TKeepAliveHttpServer.CreateConnection(Data: TSocketStream):
    TFPHTTPConnection;
begin
    Result := TKeptHttpConnection.Create(Self, Data);
end;

procedure TKeepAliveHttpServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
var
    Conn: TFPHTTPConnection;
begin
    //  The peer decides: "Connection: close" (and anything older than HTTP/1.1,
    //  where keeping it is not the default) ends the connection with this reply.
    if Assigned(ARequest) then
    begin
        //  The request carries the connection it arrived on; the response's own
        //  reference to it is not published, and its Connection property is the
        //  HEADER of that name.
        Conn := ARequest.Connection;
        if Conn is TKeptHttpConnection then
            //  Only ever narrowed: a connection that could not be given a read
            //  timeout has already given up on being kept.
            TKeptHttpConnection(Conn).KeepOpen :=
                TKeptHttpConnection(Conn).KeepOpen and
                (CompareText(Trim(ARequest.GetHeader(hhConnection)), 'close') <> 0) and
                (Trim(ARequest.ProtocolVersion) = '1.1');
    end;
    inherited HandleRequest(ARequest, AResponse);
end;

end.
