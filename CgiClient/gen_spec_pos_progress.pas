unit gen_spec_pos_progress;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_gen_spec_pos_progress: string;

implementation

uses data, Main;

const
    PairCount = 2; // 3;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Position progress'
            //'Generating specimen positions is performed'
            //'Выполняется генерация точек привязки экземпляров паттерна'
        ),
        (* zameneno kartinkoy
        ('CaptButStopSpecPosGen',
            'Abort calculation'
            //'Остановить расчет'
        ),
        *)
        ('HintSpecPosProgress',
            'Generating specimen positions is performed. Please wait for completion.'
            //'Запущен процесс поиска точек привязки. Пожалуйста ожидайте завершения!'
        )
        );

function ReplaceStrings_gen_spec_pos_progress(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_gen_spec_pos_progress: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('gen_spec_pos_progress.htm');
        Result := ReplaceStrings_gen_spec_pos_progress(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

