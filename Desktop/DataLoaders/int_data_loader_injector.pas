{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for data loader injector.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_data_loader_injector;

interface

uses int_data_loader;

type
    { Interface defining basic operation for creating data loader. }
    IDataLoaderInjector = interface
        function CreateDataLoader(AFileName: string): IDataLoader;
    end;

implementation

end.


