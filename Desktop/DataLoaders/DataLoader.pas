{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used in data loading.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit DataLoader;

{$MODE Delphi}

interface

uses Classes, SysUtils, SimpMath, SelfCopied, PointsSets, IntDataLoader;

type
    EFileNotExists = class(Exception);
    EInvalidFileType = class(Exception);
    EInvalidFileFormat = class(Exception);

    { Basic class for building loaders for different types of data files. }
    TDataLoader = class(TComponent, IDataLoader)
    protected
        PointsSet: TPointsSet;
        FFileName: string;

        procedure LoadDataSetActually; virtual; abstract;
        procedure CreatePointsSet;

    public
        procedure LoadDataSet(AFileName: string);
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet; virtual;
        destructor Destroy; override;
    end;

const 
      { The minimal allowed number. }
      MIN_VALUE: Double = -1e100;
      { The maximal allowed number. }
      MAX_VALUE: Double =  1e100;

function MyStrToFloat(Str: string): Double;

implementation

uses Main;

function MyStrToFloat(Str: string): Double;
var i: LongInt;
begin
    for i := 1 to Length(Str) do
        if (Str[i] = '.') or (Str[i] = ',') then
            Str[i] := DecimalSeparator;
    Result := StrToFloat(Str);
end;

{============================== TDataLoader ===================================}

function TDataLoader.GetPointsSetCopy: TTitlePointsSet;
begin
    Assert(Assigned(PointsSet));
    Result := TTitlePointsSet.CreateFromPoints(nil, PointsSet);
end;

destructor TDataLoader.Destroy;
begin
    PointsSet.Free;
    inherited Destroy;
end;

procedure TDataLoader.CreatePointsSet;
begin
    if Assigned(PointsSet) then PointsSet.Clear
    else PointsSet := TNeutronPointsSet.Create(nil);
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
    //  Object PointsSet must be saved because 
    //  there can be external pointers to it.
    Assert(FFileName <> '');
    Assert(Assigned(PointsSet));

    if not FileExists(FFileName) then
        raise EFileNotExists.Create('File ' + FFileName +
            ' does not exists.');

    PointsSet.Clear;
    LoadDataSetActually;
end;

end.

