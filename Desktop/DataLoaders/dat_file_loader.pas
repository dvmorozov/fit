{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of class loading data from DAT-files.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit dat_file_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, data_loader;

type
    { Loads data from ordinary DAT-file consisting from lines having pairs
      of position and values. }
    TDATFileLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
    end;

implementation


{============================== TDATFileLoader ================================}

procedure TDATFileLoader.LoadDataSetActually;
var //F: TextFile;
    Val1, Val2: double;
    Data:     TStringList;
    Str:      string;
    i, j:     longint;
    BegFound: boolean;
    BegIndex: longint;
    Value1Found, Value2Found, FirstDelimiter: boolean;

label
    ExtractValue;
begin
    Assert(FFileName <> '');
    Assert(Assigned(FPointsSet));

    FPointsSet.Clear;
    Data := TStringList.Create;
    try
        Data.LoadFromFile(FFileName);
        for i := 0 to Data.Count - 1 do
        begin
            { The first column - X (argument), the second - Y (value).  
              Column separator can be any symbol except numbers, point and comma. }
            Str := Data.Strings[i] + ' ';
            { Terminating symbol is added for algorithm. }

            BegFound := False;
            Value1Found := False;
            Value2Found := False;
            FirstDelimiter := False;
            Val1 := 0;
            Val2 := 0;
            try
                for j := 1 to Length(Str) do
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
                                if not FirstDelimiter then
                                    FirstDelimiter := True
                                else
                                    goto ExtractValue;
                        end;
                    end
                    else
                        ExtractValue:
                            if BegFound then
                            begin
                                if not Value1Found then
                                begin
                                    //  The first value (argument) is extracted.
                                    Val1 :=
                                        MyStrToFloat(Copy(Str, BegIndex, j - BegIndex));
                                    Value1Found := True;
                                end
                                else
                                begin
                                    //  The second value (function) is extracted.
                                    Val2 :=
                                        MyStrToFloat(Copy(Str, BegIndex, j - BegIndex));
                                    Value2Found := True;
                                    //  Rest of the line is ignored.
                                    Break;
                                end;
                                FirstDelimiter := False;
                                BegFound := False;
                            end;
            except
                raise EInvalidFileFormat.Create('File ' +
                    FFileName + ' is not valid DAT-file.')
            end;
            if Value2Found then
                if FPointsSet.IndexOfValueX(Val1) = -1 then
                    FPointsSet.AddNewPoint(Val1, Val2)
                //  Duplicates by argument value are ignored.
            ;
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
            FPointsSet.AddNewPoint(Val1, Val2);
        end;
    finally
        CloseFile(F);
    end;
    *)
end;

end.
