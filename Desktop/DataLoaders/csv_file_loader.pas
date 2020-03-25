{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of class loading data from CSV-files.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit csv_file_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, data_loader, SysUtils;

type
    { Loads data from CSV-file consisting from lines having pairs
      of position and values. TODO: implement LoadDataSetActually. }
    TCSVFileLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
    end;

implementation

uses
    app;

{============================== TCSVFileLoader ================================}

procedure TCSVFileLoader.LoadDataSetActually;
begin
    raise ENotImplemented.Create(
        'TCSVFileLoader.LoadDataSetActually not implemented.');
end;

end.
