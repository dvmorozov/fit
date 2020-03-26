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

    { Basic class for building loaders for different types of data files. }
    TDataLoader = class(TComponent, IDataLoader)
    protected
        FPointsSet: TPointsSet;
        FFileName:  string;

        procedure LoadDataSetActually; virtual; abstract;
        procedure CreatePointsSet;

    public
        procedure LoadDataSet(AFileName: string);
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet; virtual;
        destructor Destroy; override;
    end;

function MyStrToFloat(Str: string): double;

implementation

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
    Assert(Assigned(FPointsSet));
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
    Assert(FileExists(AFileName));

    CreatePointsSet;
    FFileName := AFileName;
    LoadDataSetActually;
end;

procedure TDataLoader.Reload;
begin
    //  Object FPointsSet must be saved because
    //  there can be external pointers to it.
    Assert(FFileName <> '');
    Assert(Assigned(FPointsSet));

    if not FileExists(FFileName) then
        raise EFileNotExists.Create('File ' + FFileName +
            ' does not exists.');

    FPointsSet.Clear;
    LoadDataSetActually;
end;

end.
