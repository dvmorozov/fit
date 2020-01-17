{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of class loading data from CSV-files.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit csv_file_loader;

{$MODE Delphi}

interface

uses Classes, SysUtils, data_loader;

type
    { Loads data from CSV-file consisting from lines having pairs
      of position and values. TODO: implement LoadDataSetActually. }
    TCSVFileLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
    end;

implementation

uses app;

{============================== TCSVFileLoader ================================}

procedure TCSVFileLoader.LoadDataSetActually;
begin
    raise ENotImplemented.Create(
        'TCSVFileLoader.LoadDataSetActually not implemented.');
end;

end.

