unit fitting_process;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_fitting_process: string;
function ReplaceStrings_fitting_process(Text: string): string;

const
    HintMinimizeDifference:         string = 'Minimizing total difference between experimental and calculated data is performed.';
                                                                                //'Выполняется минимизация расхождения.';
    HintMinimizeNumberOfSpecimens:  string = 'Minimizing number of pattern specimens is performed.';
                                                                                //'Выполняется минимизация числа экземпляров паттерна.';
    HintDoAllAutomatically:         string = 'Automatic model fitting is performed.';
                                                                                //'Выполняется подгонка параметров модели, созданной способом "по-умолчанию".';

implementation

uses data, Main;

const
    PairCount = 1; // 2; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Fitting progress'
            //'Fitting model parameters is performed'
            //'Выполняется подгонка параметров модели'
        )
        (*, zameneno kartinkoy
        ('CaptButStopFitting',
            'Abort calculation'
            //'Остановить расчет'
        )
        *)
        );

function ReplaceStrings_fitting_process(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_fitting_process: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('fitting_process.htm');
        Result := ReplaceStrings_fitting_process(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

