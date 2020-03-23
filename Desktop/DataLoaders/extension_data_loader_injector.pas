{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of data loader injector based on file extension.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit extension_data_loader_injector;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses SysUtils, data_loader, dat_file_loader, csv_file_loader,
    int_data_loader, int_data_loader_injector;

type
    { Implementation of data loader injector based on file extension. }
    TExtensionDataLoaderInjector = class(TInterfacedObject, IDataLoaderInjector)
    private
        FDataLoader: TDataLoader;

    public
        function CreateDataLoader(AFileName: string): IDataLoader;
        destructor Destroy; override;
    end;

implementation

function TExtensionDataLoaderInjector.CreateDataLoader(AFileName: string): IDataLoader;
var
    Ext: string;
begin
    if FDataLoader <> nil then
        FDataLoader.Free;

    Ext := UpperCase(ExtractFileExt(AFileName));

    if Ext = '.DAT' then
        FDataLoader := TDATFileLoader.Create(nil)
    else
    if Ext = '.CSV' then
        FDataLoader := TCSVFileLoader.Create(nil)
    else
        raise EInvalidFileType.Create('Invalid file extension.');

    Result := FDataLoader;
end;

destructor TExtensionDataLoaderInjector.Destroy;
begin
    if FDataLoader <> nil then
        FDataLoader.Free;
    inherited;
end;

end.
