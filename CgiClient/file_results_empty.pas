unit file_results_empty;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_file_results_empty: string;

implementation

uses data, file_results;

const
    PairCount = 1;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('CaptNoResults',
            'No results associated with this file'
            //  'Ни одного результата еще не получено'
        )
        );

function ReplaceStrings_file_results_empty(Text: string): string;
begin
    Result := ReplaceStrings_file_results(Text);
    Result := ReplaceStrings(Result, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_file_results_empty: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('file_results_empty.htm');
        Result := ReplaceStrings_file_results_empty(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

