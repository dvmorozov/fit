unit specimen_parameters_file;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_parameters_file: string;

implementation

uses data, app;

const
    PairCount = 2;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Resulting parameter values'
            //'Сохраненные результаты подгонки параметров модели'
        ),
        ('CaptRFactorValues',
            'Difference factors:'
            //'Факторы расходимости:'
        )
        );

function ReplaceStrings_specimen_parameters_file(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_specimen_parameters_file: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('specimen_parameters_file.htm');
        Result := ReplaceStrings_specimen_parameters_file(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

