{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of data loader injector based on file extension.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit ExtensionDataLoaderInjector;

{$MODE Delphi}

interface

uses Classes, SysUtils, DataLoader, IntDataLoader, IntDataLoaderInjector,
  CBRCComponent;

type
    { Implementation of data loader injector based on file extension. }
    TExtensionDataLoaderInjector = class(TCBRCComponent, IDataLoaderInjector)
    private
        FDataLoader: TDataLoader;

    public
        function CreateDataLoader(AFileName: string): IDataLoader;
        destructor Destroy; override;
    end;

implementation

function TExtensionDataLoaderInjector.CreateDataLoader(
  AFileName: string): IDataLoader;
begin
    if FDataLoader <> nil then
        FDataLoader.Free;
    FDataLoader := TDATFileLoader.Create(nil);
    Result := FDataLoader;
end;

destructor TExtensionDataLoaderInjector.Destroy;
begin
    if FDataLoader <> nil then
        FDataLoader.Free;
    inherited;
end;

end.

