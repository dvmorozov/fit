unit fitting_progress;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_fitting_progress: string;

implementation

uses data, fitting_process;

const
    PairCount = 2; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('HintFitting',
            'Fitting in progress. Please wait for completion.'
            //'Решение в процессе'
        ),
        ('CaptTimeElapsed',
            'Elapsed time:'
            //'Время счета:'
        )
        );

function ReplaceStrings_fitting_progress(Text: string): string;
begin
    Result := ReplaceStrings_fitting_process(Text);
    Result := ReplaceStrings(Result, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_fitting_progress: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('fitting_progress.htm');
        Result := ReplaceStrings_fitting_progress(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

