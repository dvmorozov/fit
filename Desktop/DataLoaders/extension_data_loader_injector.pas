// SPDX-License-Identifier: GPL-3.0-or-later
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

uses
    data_loader, int_data_loader, data_loader_registry,
    int_data_loader_injector, SysUtils;

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

{ Which loader opens this file is now the registry's answer, not an if-chain
  here: a loader declares the extensions it handles, so a format added by the
  framework or by a module needs no edit to this unit. What stays here is what
  this class is actually for - owning the instance and freeing the previous
  one. }
function TExtensionDataLoaderInjector.CreateDataLoader(AFileName: string): IDataLoader;
var
    LoaderClass: TDataLoaderClass;
begin
    if Assigned(FDataLoader) then
        FDataLoader.Free;
    FDataLoader := nil;

    LoaderClass := FindDataLoaderClass(AFileName);
    if not Assigned(LoaderClass) then
        //  Names the extension and what the build can actually read. "Invalid
        //  file extension" left the user guessing at both.
        raise EInvalidFileType.Create('Cannot open ' +
            ExtractFileName(AFileName) + ': no installed reader handles ' +
            ExtractFileExt(AFileName) + ' files.');

    FDataLoader := LoaderClass.Create(nil);
    Result := FDataLoader;
end;

destructor TExtensionDataLoaderInjector.Destroy;
begin
    FDataLoader.Free;
    inherited;
end;

end.
