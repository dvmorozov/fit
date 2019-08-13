unit file_results;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_file_results: string;
function ReplaceStrings_file_results(Text: string): string;

implementation

uses data, app;

const
    PairCount = 6;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Fittin results'
            //'The page of fitting results for selected file'
            //  'Результаты подгонки моделей выбранного файла экспериментальных данных'
        ),
        ('HintFile',
            'File "UserFileName".'
            //  'Файл "UserFileName".'
        ),  //  UserFileName budet zameneno dalee nazvaniem faila
        ('LinkFileResults',
            'Fitting results for selected file'
            //  'Результаты расчетов с данными файла'
        ),
        ('LinkBackToProject',
            'Project'
        ),
        ('CaptButDeleteResult',
            'Delete selected result'
            //  'Удалить выбранный результат'
        ),
        ('LinkOpenData',
            'Open file data'
            //  'Открыть данные'
        )
        );

function ReplaceStrings_file_results(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_file_results: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('file_results.htm');
        Result := ReplaceStrings_file_results(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

