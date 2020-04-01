unit background_more;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_background_more: string;

implementation

uses data, background;

const
    PairCount = 1;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('CaptBackFactor',
//'ƒол€ максимальной амплитуды, используема€ дл€ автоматической генерации точек фона = '
'The fraction of the maximum data value for automatic selection of background points = '
        )
        );

function ReplaceStrings_background_more(Text: string): string;
begin
    Result := ReplaceStrings_background(Text);
    Result := ReplaceStrings(Result, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_background_more: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('background_more.htm');
        Result := ReplaceStrings_background_more(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

