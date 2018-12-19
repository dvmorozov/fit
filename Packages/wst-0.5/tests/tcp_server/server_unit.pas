unit server_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Dialogs,
  WSocket, WSocketS;

  
Type

  { TTcpSrvClient }

  TTcpSrvClient = class(TWSocketClient)
  Private
    FConnectTime: TDateTime;
    FDataLentgh: LongInt;
    FRequestStream : TStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy();override;
    function TryRead():Boolean;
    property ConnectTime : TDateTime Read FConnectTime Write FConnectTime;
    property RequestStream : TStream Read FRequestStream;
    property DataLentgh : LongInt Read FDataLentgh;
  end;

  { TTcpSrvApp }

  TTcpSrvApp = class
  Private
    procedure HandleClientConnect(Sender: TObject;Client: TWSocketClient; Error: Word);
    procedure HandleClientDisconnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure HandleBgException(Sender: TObject; E: Exception;
      var CanClose: Boolean);
  private
    FWSocketServer: TWSocketServer;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client : TTcpSrvClient);
    procedure ClientBgException(Sender       : TObject;
                                E            : Exception;
                                var CanClose : Boolean);
    procedure ClientLineLimitExceeded(Sender        : TObject;
                                      Cnt           : LongInt;
                                      var ClearData : Boolean);
  Public
    constructor Create();
    destructor Destroy();override;
    procedure Display(Msg : String);
    procedure Start();
    procedure Stop();
    function IsActive():Boolean;
  End;
  
Implementation
uses umain, server_service_intf, server_service_imputils, binary_streamer;

procedure LogMsg(const Msg : String);
Begin
  fMain.LogMessage(Msg);
End;

procedure TTcpSrvApp.Display(Msg : String);
begin
  LogMsg(Msg);
end;

procedure TTcpSrvApp.Start();
begin
  Display('Starting...');
  FWSocketServer.Proto       := 'tcp';         { Use TCP protocol  }
  FWSocketServer.Port        := '1234';
  FWSocketServer.Addr        := '0.0.0.0';     { Use any interface }
  FWSocketServer.ClientClass := TTcpSrvClient;
  FWSocketServer.Listen;                       { Start litening    }
  Display('Waiting for clients...');
end;

procedure TTcpSrvApp.Stop();
begin
  FWSocketServer.CloseDelayed();
end;

function TTcpSrvApp.IsActive(): Boolean;
begin
  Result := ( FWSocketServer.State < wsClosed );
end;

procedure TTcpSrvApp.HandleClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connected.' +
                ' Remote: ' + PeerAddr + '/' + PeerPort +
                ' Local: '  + GetXAddr + '/' + GetXPort);
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount) +
                ' clients connected.');
        LineMode            := False;
        LineEdit            := False;
        OnDataAvailable     := @ClientDataAvailable;
        OnLineLimitExceeded := @ClientLineLimitExceeded;
        OnBgException       := @ClientBgException;
        ConnectTime         := Now;
    end;
end;

procedure TTcpSrvApp.HandleClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client disconnecting : ' + PeerAddr + '   ' +
                'Duration: ' + FormatDateTime('hh:nn:ss',
                Now - ConnectTime));
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount - 1) +
                ' clients connected.');
    end;
end;

procedure TTcpSrvApp.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : LongInt;
    var ClearData : Boolean);
begin
    with Sender as TTcpSrvClient do begin
        Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
        ClearData := TRUE;
        Close;
    end;
end;

constructor TTcpSrvApp.Create();
begin
  FWSocketServer := TWSocketServer.Create(Nil);
  FWSocketServer.Banner := '';
  FWSocketServer.OnClientConnect := @HandleClientConnect;
  FWSocketServer.OnBgException := @HandleBgException;
  FWSocketServer.OnClientDisconnect := @HandleClientDisconnect;
end;

destructor TTcpSrvApp.Destroy();
begin
  FWSocketServer.Free();
end;

procedure TTcpSrvApp.ClientDataAvailable(Sender : TObject;Error  : Word);
Var
  cliTCP : TTcpSrvClient;
begin
  cliTCP := Sender as TTcpSrvClient;
  //Display('ClientDataAvailable()');
  If cliTCP.TryRead() And ( cliTCP.DataLentgh > 0 ) Then
    ProcessData(cliTCP)
end;

procedure TTcpSrvApp.ProcessData(Client : TTcpSrvClient);
Var
  buff, trgt,ctntyp : string;
  rqst : IRequestBuffer;
  wrtr : IDataStore;
  rdr : IDataStoreReader;
  inStream, outStream, bufStream : TMemoryStream;
  i : Integer;
begin
  inStream := Nil;
  outStream := Nil;
  bufStream := Nil;
  Try
    Client.RequestStream.Position := 0;
    Try
      inStream := TMemoryStream.Create();
      outStream := TMemoryStream.Create();
      bufStream := TMemoryStream.Create();
      rdr := CreateBinaryReader(Client.RequestStream);
      trgt := rdr.ReadStr();
      ctntyp := rdr.ReadStr();
      buff := rdr.ReadStr();
      inStream.Write(buff[1],Length(buff));
      inStream.Position := 0;
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,bufStream);
      HandleServiceRequest(rqst);
      i := bufStream.Size;
      SetLength(buff,i);
      bufStream.Position := 0;
      bufStream.Read(buff[1],i);
      wrtr := CreateBinaryWriter(outStream);
      wrtr.WriteStr(buff);
      //Display('ProcessData() resp Ln =' + IntToStr(i) + '; resp = ' + buff);
      Client.Send(outStream.Memory,outStream.Size);
    Finally
      //Display('ProcessData()>> END');
      bufStream.Free();
      outStream.Free();
      inStream.Free();
      Client.FDataLentgh := -1;
      Client.RequestStream.Size := 0;
    End;
  Except
    On e : Exception Do
      Display('ProcessData()>> Exception = '+e.Message);
  End;
end;

procedure TTcpSrvApp.HandleBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Server exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  { Hoping that server will still work ! }
end;

procedure TTcpSrvApp.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   { Goodbye client ! }
end;

{ TTcpSrvClient }

constructor TTcpSrvClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLentgh := -1;
  FRequestStream := TMemoryStream.Create();
end;

destructor TTcpSrvClient.Destroy();
begin
  FRequestStream.Free();
  inherited Destroy();
end;

function TTcpSrvClient.TryRead(): Boolean;
Var
  i,j : PtrInt;
  buff : string;
begin
  If ( FDataLentgh < 0 ) Then Begin
    i := 4;
    If ( Receive(@FDataLentgh,i) < 4 ) Then Begin
      FDataLentgh := -1;
      Result := False;
      Exit;
    End;
    FDataLentgh := Reverse_32(FDataLentgh);
  End;
  If ( FDataLentgh > FRequestStream.Size ) Then Begin
    i := Min((FDataLentgh-FRequestStream.Size),1024);
    SetLength(buff,i);
    j := Receive(@(buff[1]),i);
    FRequestStream.Write(buff[1],j);
    //LogMsg(Format('Read %d bytes;  buff=%s',[j,buff]));
  End;
  Result := ( FDataLentgh <= FRequestStream.Size );
  //LogMsg(Format('TryRead() >> FDataLentgh=%d;  Size=%d',[FDataLentgh,FRequestStream.Size]));
end;

end.
