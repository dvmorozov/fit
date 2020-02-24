{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class representing the application.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_client_app;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses SysUtils, fit_client, fit_client_stub, extension_data_loader_injector;

type
    { Container class (agregate), which integrates all application
      components except UI. }
    TFitClientApp = class(TObject)
    private
        FFitStub: TFitClientStub;
        FFitClient: TFitClient;
        FDataLoaderInjector: TExtensionDataLoaderInjector;

    public
        constructor Create;
        destructor Destroy; override;

        property FitClient: TFitClient read FFitClient;
        property FitStub: TFitClientStub read FFitStub;
    end;

implementation

{============================== TFitClientApp =================================}

constructor TFitClientApp.Create;
begin
    inherited;
    FFitStub := TFitClientStub.Create(nil);
    FDataLoaderInjector := TExtensionDataLoaderInjector.Create;
    FFitClient := TFitClient.CreateWithInjector(nil, FDataLoaderInjector);

    FFitStub.FitClient := FFitClient;
end;

destructor TFitClientApp.Destroy;
begin
    FFitClient.Free;
    FFitStub.Free;
    FDataLoaderInjector.Free;
end;

end.



