unit specimen_positions;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_positions: string;

implementation

uses data, app;

const
    PairCount = 9; // 11; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Curve positions'
            //'The page of selection of specimen positions'
            //'����� ����� �������� ����������� ��������'
        ),
        ('CaptCurvePosFull',
            'Curve positions'
            //'����� �������� ����������� ��������'
        ),
        ('HintCurvePositions',
            'Curve positions.'
            //'����� �������� ����������� ��������.'
        ),
        (* zameneno kartinkoy
        ('CaptButGenerate',
            'Generate'
            //'������������'
        ),
        *)
        ('CaptPointSelection',
            'Curve positions selection'
            //'����� ����� ��������'
        ),
        ('CaptArgument',
            'Argument'
            //'��������'
        ),
        ('CaptValue',
            'Value'
            //'��������'
        ),
        ('CaptSpecPosition',
            ' ' // 'Is s. p.?'
            //'����� ��������?'
        ),
        (* zameneno kartinkoy
        ('CaptButSpecPosition',
            'Select / Unselect'
            //'����� �������� / �� ����� ��������'
        ),
        *)
        ('CaptButNextStage',
            'Curve bounds'
            //'������� � ������ ���������� ����������'
        ),
        ('HintValues',
'To add point to the list of specimen positions type its argument. ' +
'Then press the button. To delete press the button again.'
//'��� ���������� ����� � ������ ����� �������� ����������� �������� ������� �� ��������. ' +
//'������� ������. ��������� ������� ������ ������� ����� �� ������ ����� ��������.'
        )
        );

function ReplaceStrings_specimen_positions(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_specimen_positions: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('specimen_positions.htm');
        Result := ReplaceStrings_specimen_positions(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

