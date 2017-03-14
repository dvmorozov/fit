unit FitServerApp;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils,
{$IFDEF FIT}
     FitServerProxy,
{$ELSE}
     //  sborka servera - mozhno ne ispol'zovat' SERVER
     PointSetViewer,
{$ENDIF}
     FitServerStub, FitServer,
     (* FitServerWithThread, *) FitServerMultithreaded,
{$IFDEF SERVER}
     FormServer,
{$ENDIF}
     Forms;
type
    //  klass servernogo prilozheniya;
    //  v servere etot klass sluzhit osnovnoy edinitsey
    //  raboty s klientom (eto problema - the problem)
    TFitServerApp = class(TComponent)
    protected
{$IFDEF FIT}
        FFitProxy: TFitServerProxy;
{$ELSE}
        FViewer: TFitViewer;
        FForm: TForm1;
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
        property Form: TForm1 read FForm;
{$ENDIF}
        property FitStub: TFitServerStub read FFitStub;
    end;

implementation

uses Main;

{================================ TFitServerApp ===============================}

constructor TFitServerApp.Create;
begin
    //  raskommentirovat' pri rabote servera otdel'noy programmoy
    //Application.OnException := OnException;
{$IFDEF FIT}
    FFitProxy := TFitServerProxy.Create;
{$ELSE}
    FForm := TForm1.Create(nil);
    FViewer := TFitViewer.Create(nil);
    FViewer.Form := FForm;
{$ENDIF}
    FFitStub := TFitServerStub.Create;
    FFitStub.RecreateServer := RecreateServer;
    RecreateServer;
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
    //FFitServer := TFitServerWithThread.Create(nil);
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



