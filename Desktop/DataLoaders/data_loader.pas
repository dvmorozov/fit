// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used in data loading.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit data_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, int_data_loader, neutron_points_set, points_set, SysUtils,
    title_points_set;

type
    EFileNotExists = class(Exception);
    EInvalidFileType = class(Exception);
    EInvalidFileFormat = class(Exception);

    { Basic class for building loaders for different types of data files.

      READING THE FILE AND PARSING IT ARE SEPARATE, and that separation is the
      whole reason this class has a template method. Every loader used to be a
      single LoadDataSetActually that did LoadFromFile and then parsed what it
      got, so the parser - which is where every format decision lives, including
      the silent ones - could only be reached by putting a file on disk. That
      makes its test an integration test by this project's own rule, and coverage
      is measured over the unit half: several hundred lines of format handling
      were exercised by tests that counted for nothing.
      See docs/contributing/testing.md. }
    TDataLoader = class(TComponent, IDataLoader)
    protected
        FPointsSet: TPointsSet;
        FFileName:  string;
        { What to call the source in an error message: the file name for a file,
          and something a reader can act on when the lines came from memory. }
        FSourceName: string;

        { THE PARSER: lines in, points out. No file, no path, no I/O.
          Override this rather than LoadDataSetActually. }
        procedure ParseLines(ALines: TStrings); virtual; abstract;

        { Reads the file and hands its lines to ParseLines. Virtual only because a
          format that is not line-based would have to replace it; none is. }
        procedure LoadDataSetActually; virtual;
        procedure CreatePointsSet;

    public
        procedure LoadDataSet(AFileName: string);
        { The same parse, over lines already in memory. This is what a test drives,
          and it is the SAME path a file load takes - the file version only reads
          the lines first - so a test through here cannot pass over a parser the
          application does not actually use. }
        procedure LoadFromLines(ALines: TStrings);
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet; virtual;
        destructor Destroy; override;
    end;

function MyStrToFloat(Str: string): double;

implementation


uses
    checks;

{$warnings off}
function MyStrToFloat(Str: string): double;
var
    i: longint;
begin
    for i := 1 to Length(Str) do
        if (Str[i] = '.') or (Str[i] = ',') then
            Str[i] := DecimalSeparator;
    Result := StrToFloat(Str);
end;

{$warnings on}

{============================== TDataLoader ===================================}

function TDataLoader.GetPointsSetCopy: TTitlePointsSet;
begin
    CheckAssigned(FPointsSet, 'the points this loader read');

    Result := TTitlePointsSet.CreateFromPoints(nil, FPointsSet);
end;

destructor TDataLoader.Destroy;
begin
    FPointsSet.Free;
    inherited;
end;

procedure TDataLoader.CreatePointsSet;
begin
    if Assigned(FPointsSet) then
        FPointsSet.Clear
    else
        FPointsSet := TNeutronPointsSet.Create(nil);
end;

procedure TDataLoader.LoadDataSet(AFileName: string);
begin
    //  A file the user chose and that is no longer there is an ordinary
    //  outcome, not a defect - the same condition Reload already reports
    //  this way.
    if not FileExists(AFileName) then
        raise EFileNotExists.Create('File ' + AFileName + ' does not exists.');

    CreatePointsSet;
    FFileName := AFileName;
    FSourceName := AFileName;
    LoadDataSetActually;
end;

procedure TDataLoader.LoadFromLines(ALines: TStrings);
begin
    CheckAssigned(ALines, 'the lines to load a data set from');

    CreatePointsSet;
    //  No file was read, so there is no path to quote. A message naming an empty
    //  path would read as a missing file rather than as unusable content.
    FFileName := '';
    FSourceName := 'the supplied lines';
    ParseLines(ALines);
end;

procedure TDataLoader.LoadDataSetActually;
var
    Data: TStringList;
begin
    CheckThat(FFileName <> '', 'the file to read must be chosen before the data set is loaded');

    Data := TStringList.Create;
    try
        Data.LoadFromFile(FFileName);
        ParseLines(Data);
    finally
        Data.Free;
    end;
end;

procedure TDataLoader.Reload;
begin
    //  Object FPointsSet must be saved because
    //  there can be external pointers to it.
    CheckThat(FFileName <> '', 'only a data set that came from a file can be reloaded');
    CheckAssigned(FPointsSet, 'the points to reload into');

    if not FileExists(FFileName) then
        raise EFileNotExists.Create('File ' + FFileName +
            ' does not exists.');

    FPointsSet.Clear;
    LoadDataSetActually;
end;

end.
