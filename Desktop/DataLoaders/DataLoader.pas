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

    { Loads data from ordinary DAT-file consisting from lines having pairs
      of position and values. }
    TDATFileLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
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

{============================== TDATFileLoader ================================}

procedure TDATFileLoader.LoadDataSetActually;
var //F: TextFile;
    Val1, Val2: Double;
    Data: TStringList;
    Str: string;
    i, j: LongInt;
    BegFound: Boolean;
    BegIndex: LongInt;
    Value1Found, Value2Found, FirstDelimiter: Boolean;

label ExtractValue;
begin
    Assert(FFileName <> '');
    Assert(Assigned(PointsSet));
    
    PointsSet.Clear;
    Data := TStringList.Create;
    try
        Data.LoadFromFile(FFileName);
        for i := 0 to Data.Count - 1 do
        begin
            { The first column - X (argument), the second - Y (value).  
              Column separator can be any symbol except numbers, point and comma. }
            Str := Data.Strings[i] + ' ';   { Terminating symbol is added for algorithm. }
                                            
            BegFound := False;
            Value1Found := False; Value2Found := False;
            FirstDelimiter := False;
            Val1 := 0; Val2 := 0;
            try
                for j := 1 to Length(Str) do
                begin
                    if ((Str[j] >= Chr($30)) and (Str[j] <= Chr($39))) or
                        (Str[j] = '.') or (Str[j] = ',') then
                    begin
                        if not BegFound then
                        begin
                            BegIndex := j;
                            BegFound := True;
                        end
                        else
                        begin
                            if (Str[j] = '.') or (Str[j] = ',') then
                            begin
                                if not FirstDelimiter then
                                    FirstDelimiter := True
                                else goto ExtractValue;
                            end;
                        end;
                    end
                    else
                    begin
ExtractValue:
                        if BegFound then
                        begin
                            if not Value1Found then
                            begin
                                //  The first value (argument) is extracted.
                                Val1 := MyStrToFloat(
                                    Copy(Str, BegIndex, j - BegIndex));
                                Value1Found := True;
                            end
                            else
                            begin
                                //  The second value (function) is extracted.
                                Val2 := MyStrToFloat(
                                    Copy(Str, BegIndex, j - BegIndex));
                                Value2Found := True;
                                //  Rest of the line is ignored.
                                Break;
                            end;
                            FirstDelimiter := False;
                            BegFound := False;
                        end;
                    end;
                end;
            except
                raise EInvalidFileFormat.Create('File ' +
                    FFileName + ' is not valid DAT-file.')
            end;
            if Value2Found then
            begin
                //  Duplicates by argument value are ignored.
                if PointsSet.IndexOfValueX(Val1) = -1 then
                    PointsSet.AddNewPoint(Val1, Val2);
            end;
        end;
    finally
        Data.Free;
    end;
    (*
    AssignFile(F, FFileName);
    Reset(F);
    try
        while not Eof(F) do
        begin
            try
                ReadLn(F, Val1, Val2);
            except
                //  dopustimaya oshibka vybrasyvaemaya v vide isklyucheniya
                raise EInvalidFileFormat.Create('File ' +
                    FFileName + ' is not valid DAT-file.')
            end;
            PointsSet.AddNewPoint(Val1, Val2);
        end;
    finally
        CloseFile(F);
    end;
    *)
end;

end.


