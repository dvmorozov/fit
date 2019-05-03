{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Defines the server application class which is basic unit of interaction with client.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit FitServerApp;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils,
{$IFDEF FIT}
     { Connects back to client in monolithic application for callbacks. }
     FitServerProxy,
{$ELSE}
     { Server build. Key SERVER is not necessary. }
     FitViewer,
{$ENDIF}
     { Receives messages from client. }
     FitServerStub,
     { Implements server logic. }
     FitServer,
     { Contains server form with chart component. }
     FormServer,
     { Contains algorithm container. }
     FitServerMultithreaded;
type
    { Class of server application. This class is basic unit of interaction
      with client. }
    TFitServerApp = class(TComponent)
    protected
{$IFDEF FIT}
        FFitProxy: TFitServerProxy;
{$ELSE}
        FViewer: TFitViewer;
        FForm: TFormMain;
{$ENDIF}
        FFitStub: TFitServerStub;
        FFitServer: TFitServer;

        procedure RecreateServer;
        procedure OnException(Sender: TObject; E: Exception);

    public
        constructor Create;
        destructor Destroy; override;

{$IFDEF FIT}
        property FitProxy: TFitServerProxy read FFitProxy;
{$ELSE}
        property Viewer: TFitViewer read FViewer;
        property Form: TFormMain read FForm;
{$ENDIF}
{$IFNDEF FIT}
        property FitStub: TFitServerStub read FFitStub;
{$ELSE}
        property FitStub: TFitServer read FFitServer;
{$ENDIF}
    end;

implementation

uses Main;

{================================ TFitServerApp ===============================}

constructor TFitServerApp.Create;
begin
    //  raskommentirovat' pri rabote servera otdel'noy programmoy
    //Application.OnException := OnException;
{$IFDEF FIT}
    FFitProxy := TFitServerProxy.Create(nil);
{$ELSE}
    FForm := TFormMain.Create(nil);
    FViewer := TFitViewer.Create(nil);
    FViewer.Form := FForm;
{$ENDIF}
    FFitStub := TFitServerStub.Create;
    FFitStub.RecreateServer := RecreateServer;
    RecreateServer;

    FFitServer := FFitStub.Server;
end;

destructor TFitServerApp.Destroy;
begin
    FFitServer.Free;    //  server dolzhen unichtozhat'sya pervym,
                        //  chtoby byli zaversheny dochernie potoki
{$IFDEF FIT}
    FFitProxy.Free;
{$ELSE}
    FViewer.Free;
    FForm.Free;
{$ENDIF}
    FFitStub.Free;
end;

procedure TFitServerApp.RecreateServer;
begin
    FFitServer.Free; FFitServer := nil;
    FFitServer := TFitServerMultithreaded.Create(nil);
{$IFDEF FIT}
    FFitServer.FitProxy := FFitProxy;
{$ENDIF}
    FFitStub.Server := FFitServer;
end;

procedure TFitServerApp.OnException(Sender: TObject; E: Exception);
begin
    WriteLog(E.Message, Fatal);
end;

end.



