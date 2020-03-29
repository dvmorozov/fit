unit fitting;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_fitting: string;

implementation

uses data, app;

const
    PairCount = 6;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Fitting procedures'
            //'The page of fitting method selection'
            //  '����� ������� �������� ������'
        ),
        ('CaptFittingProc', 'Fitting procedures'),
        ('HintMinimizeDifference',
'Minimize total difference between experimental and model curves.'
//'����� �������� ����������, ��� ������� ����������� ����� ��������� � ' +
//'�������� ������ ��������� ��������.'
        ),
        ('HintMinimizeNumberOfCurves1',
'Minimize number of curves on the condition that the difference '    +
'between experimental and calculated data in the specimen application interval '+
'does not exceed the given value.'
        ),
        ('HintMinimizeNumberOfCurves2',
//'����� �������� ����������, ��� ������� ������ ������������ �� ������ '         +
//'����������� ����������� � ���������� ����������� �������� ����������.<BR>'     +
'This algorithm combines minimization of the difference factor with '           +
'consecutive decreasing of number of curves. Thus the algorithm '    +
'suited for initially excessive number of curves. This may be useful in the '+
'cases of complex profile, when it is not possible to select specimen '         +
'positions unambiguously. The algorithm requires defining of the maximum '      +
'acceptable value of difference factor for each of specimen application '       +
'intervals. At that the total value of difference factor will not be more than '+
'maximum acceptable value multiplied by the number of pattern application '     +
'intervals.'
//������ �������� �������� ����������� ������� ������������ � ����������������
//����������� ���������� ����������� ��������. ����� ������� ��������
//������������ � ������������ ������� ����������� ����� ����������� ��������.
//��� ����� ���� ������� � ������� ������� ����� �������, ����� ����������
//���������� ������ ����� �������� ����������� ��������. �������� �������
//������� ����������� ����������� �������� ������� ������������ ��� �������
//��������� ���������� ����������� ��������. ��� ���� �������� ������� �������
//������������ �� �������� ����������� ���������� �������� ���������� ��
//����� ���������� ���������� ����������� ��������.
        ),
        ('HintMaxAcceptableValue',
'Maximum acceptable value of difference factor ' +
//'����������� ���������� �������� ������� ������������ ' +
'<NOBR><SUP>&#931;(I<SUB>c</SUB>-I<SUB>e</SUB>)<SUP>2</SUP></SUP>/<SUB>(&#931;I<SUB>e</SUB>)<SUP>2</SUP></SUB></NOBR>' +
' for every specimen application interval = '
//' ��� ������� ��������� ���������� ����������� �������� = '
        )
        (*, zameneno kartinkoy
        ('CaptButMinimizeDifference',
            'Start'
            //  '�������������� �����������'
        ),
        ('CaptButMinimizeNumberOfCurves',
            'Start'
            //  '�������������� ����� ����������� ��������'
        )
        *)
        );

function ReplaceStrings_fitting(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_fitting: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('fitting.htm');
        Result := ReplaceStrings_fitting(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

