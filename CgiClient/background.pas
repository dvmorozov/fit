unit background;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_background: string;
function ReplaceStrings_background(Text: string): string;

implementation

uses data, app;

const
    PairCount = 11; // 13;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Background'      // 'The page of background points input'
            //  '�������� ����� ����� ����'
        ),
        ('CaptBackground',
            'Automatic selection of background points'
            //  '�������������� ���������'
        ),
        ('HintBackground',
'Automatic selection of background points is possible for the time present ' +
'only if the background looks like smooth concave curve.'
//'�������������� ��������� ����� �������� ������ ���� ��� ���������� ������� ' +
//'�������� ���� ������.'
        ),
        ('CaptButMore',
            'More'
            //  '�������������'
        ),
        (* zameneno kartinkoy
        ('CaptButGenerate',
            'Select automatically'
            //  '������������'
        ),
        *)
        ('CaptPointSelection',
            'Background points'
            //  '����� ����� ����'
        ),
        ('CaptArgument',
            'Argument'
            //  '��������'
        ),
        ('CaptValue',
            'Value'
            //  '��������'
        ),
        ('CaptBackPoint',
            ' ' //'Is b. p.?'
            //  '����� ����?'
        ),
        //('CaptButBackPoint', '����� ����'),
        //('CaptButNotBackPoint', '�� ����� ����'),
        (* zameneno kartinkoy
        ('CaptButBackPoint',
            'Select / Unselect'
            //  '����� ���� / �� ����� ����'
        ),
        *)
        ('CaptButSkip',
            'Skip background'
            //  '���������� �������� ����'
        ),
        ('CaptButDelBackground',
            'Remove background'
            //  '������� ��� � ������� � �������� ������'
        ),
        ('HintValues',
'To select the point as background point type its argument in the input field.' +
'Then press the button. To unselect press the button again.'
//'��� ������ ����� � �������� ����� ���� ������� �� �������� � ��������������� ����. ' +
//'������� ������. ��� ������ ������ ������� ������ ��� ���.'
        )
        );

function ReplaceStrings_background(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_background: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('background.htm');
        Result := ReplaceStrings_background(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

