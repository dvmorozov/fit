unit specimen_parameters;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_parameters: string;

implementation

uses data, app;

const
    PairCount = 10; // 11; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Curve parameters'
            //'The page of input of specimen parameters'
            //'������������� �������� ���������� ����������� ��������'
        ),
        ('CaptCurveParametersTable',
            'Curve parameters table'
            //'������� ���������� ����������� ��������'
        ),
        ('HintSelectedPattern',
            'Here you can set specimen parameter values.'
            //'����� ����� ������ �������� ���������� ����������� ��������.'
        ),
        (* zameneno kartinkoy
        ('CaptButUpdateCurve',
            'Apply'
            //'��������'
        ),
        *)
        ('LinkProject',
            'Project'
            //'��������� � �������'
        ),
        ('LinkResults',
            'Results'
            //'��������� � �����������'
        ),
        ('CaptRFactorValues',
            'Difference factors:'
            //'������� ������������:'
        ),
        ('CaptButNextStage',
            'Fitting'
            //'������� � �������� ����������'
        ),
        (* zameneno kartinkoy
        ('CaptButSaveSpecParameters',
            'Save as result'
            //'��������� ���������'
        ),
        *)
        ('CaptResultName',
            'Result name'
            //'��� ������� ����������'
        ),
        ('CaptResultDescription',
            'Description'
            //'�������������� ����������'
        ),
        ('CaptSaveResult',
            'Save this results'
        )
        );

function ReplaceStrings_specimen_parameters(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_specimen_parameters: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('specimen_parameters.htm');
        Result := ReplaceStrings_specimen_parameters(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

