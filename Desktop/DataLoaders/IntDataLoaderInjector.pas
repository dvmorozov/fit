{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of interfaces for data loader injectors.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit IntDataLoaderInjector;

{$MODE Delphi}

interface

uses Classes, SysUtils, IntDataLoader;

type
    { Interface defining basic operation for creating data loader. }
    IDataLoaderInjector = interface
        function CreateDataLoader(AFileName: string): IDataLoader;
    end;

implementation

end.


