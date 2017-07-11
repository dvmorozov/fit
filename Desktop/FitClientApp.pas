unit FitClientApp;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitClient, FitClientStub, FitClientProxy;

type
    //  klass-konteyner (agregat), kot. ob'edinyaet
    //  komponenty klientskoy chasti programmy, krome UI
    TFitClientApp = class(TObject)
    protected
        FFitProxy: TFitClientProxy;
        FFitStub: TFitClientStub;
        FFitClient: TFitClient;

    public
        constructor Create;
        destructor Destroy; override;

        property FitClient: TFitClient read FFitClient;
        property FitStub: TFitClientStub read FFitStub;
        property FitProxy: TFitClientProxy read FFitProxy;
    end;

implementation

{============================== TFitClientApp =================================}
constructor TFitClientApp.Create;
begin
    inherited;
    FFitProxy := TFitClientProxy.Create;
    FFitStub := TFitClientStub.Create;
    FFitClient := TFitClient.Create(nil);

    FFitClient.FitProxy := FFitProxy;
    FFitStub.FitClient := FFitClient;
end;

destructor TFitClientApp.Destroy;
begin
    FFitClient.Free;
    FFitStub.Free;
    FFitProxy.Free;
end;

end.



