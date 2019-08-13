unit pattern;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_pattern: string;
function ReplaceStrings_pattern(Text: string): string;

implementation

uses data, app;

const
    PairCount = 10; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Pattern'
            //'The page of pattern selection'
            //'����� �������� ��� �������� ������'
        ),
        ('HintPatternSelection',
            'Select pattern that is most like to the experimental data.'
            //'�������� �������, �� ������� �������� ������ ��������� ����� ' +
            //'��� ��� ����������������� ������.'
        ),
        ('CaptPatternName',
            'Pattern'
            //'�������'
        ),
        ('CaptPatternExpr',
            'Expression'
            //'���������'
        ),
        ('CaptButCreatePattern',
            'Create'
            //'�������'
        ),
        ('CaptButEditPattern',
            'Edit'
            //'�������'
        ),
        ('CaptButDeletePattern',
            'Delete'
            //'�������'
        ),
        ('CaptAction',
            'Action with pattern'
            //'�������� ��� ���������'
        ),
        ('CaptButDoAction',
            //  !!! vremenno, poka sozdanie patterna pol'zovatelem zaprescheno !!!
            'Select pattern'
            //'Execute'
            //'���������'
        ),
        ('CaptButNextStage',
            'Select pattern and go to next stage'
            //'������� � ������� � ��������� ������'
        )
        );

function ReplaceStrings_pattern(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_pattern: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('pattern.htm');
        Result := ReplaceStrings_pattern(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

