{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for data loader injector.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_data_loader_injector;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses int_data_loader;

type
    { Interface defining basic operation for creating data loader. }
    IDataLoaderInjector = interface
        ['{b6d01424-04d5-4a30-98f6-b81ef17cedb3}']
        function CreateDataLoader(AFileName: string): IDataLoader;
    end;

implementation

end.


