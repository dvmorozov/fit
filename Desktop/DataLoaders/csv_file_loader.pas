// SPDX-License-Identifier: GPL-3.0-or-later
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
        procedure ParseLines(ALines: TStrings); override;
    end;

implementation

//  `uses app` REMOVED, not tidied. app.pas uses Forms and its
//  initialization constructs a desktop client application object plus an
//  HTTP client aimed at the default server URL. This unit referenced
//  neither identifier app.pas exports, so the clause bought nothing and
//  cost the LCL - and, in the compute server, a client of itself built on
//  every start-up. See docs/contributing/findings.md.

{============================== TCSVFileLoader ================================}

procedure TCSVFileLoader.ParseLines(ALines: TStrings);
begin
    raise ENotImplemented.Create(
        'TCSVFileLoader.ParseLines not implemented.');
end;

end.
