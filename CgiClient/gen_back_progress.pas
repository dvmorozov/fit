unit gen_back_progress;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_gen_back_progress: string;

implementation

uses data, Main;

const
    PairCount = 2; // 3; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Baseline progress'
            //'Finding baseline points is performed'
            //'Выполняется генерация точек фона'
        ),
        (* zameneno kartinkoy
        ('CaptButStopBackGen',
            'Abort calculation'
            //'Остановить расчет'
        ),
        *)
        ('HintBackProgress',
            'Finding baseline points is performed. Please wait for completion.'
            //'Запущен процесс поиска точек фона. Пожалуйста ожидайте завершения!'
        )
        );

function ReplaceStrings_gen_back_progress(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_gen_back_progress: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('gen_back_progress.htm');
        Result := ReplaceStrings_gen_back_progress(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

