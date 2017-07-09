unit error;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_error: string;

implementation

uses data, Main;

const
    PairCount = 1; // 3; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('CaptFitServiceError',
            'Error'
            //'Ошибка'
        )
        (*, isklyucheno iz dizayna
        ('CaptDescription',
            'Description'
            //'Описание'
        ),
        *)
        (* zameneno kartinkoy
        ('CaptButGoToStart',
            'Go to login page'
            //'Возврат к началу'
        )
        *)
        );

function ReplaceStrings_error(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, CommonPairArray, CommonPairCount);
    Result := ReplaceStrings(Result, PairArray, PairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_error: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('error.htm');
        Result := ReplaceStrings_error(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

