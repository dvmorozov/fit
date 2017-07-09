unit pattern_more;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_pattern_more: string;

implementation

uses data, pattern;

function ReplaceStrings_pattern_more(Text: string): string;
begin
    Result := ReplaceStrings_pattern(Text);
    //Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_pattern_more: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('pattern_more.htm');
        Result := ReplaceStrings_pattern_more(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

