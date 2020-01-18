{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class representing the application.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_client_app;

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
    FDataLoaderInjector := TExtensionDataLoaderInjector.Create(nil);
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



