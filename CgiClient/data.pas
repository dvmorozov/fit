unit data;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

{$i data.inc}

type TStringPair = array[1..2] of string;

const
    CommonPairCount = 9; // 10;   //  kol-vo elementov
    CommonPairArray: array[1..CommonPairCount] of TStringPair = (
        ('CaptDataInput',
            'Data input'
            //  '���� ������'
        ),
        ('CaptBackDeleting',
            'Baseline'
            //  '�������� ����'
        ),
        ('CaptModelCreating',
            'Model'
            //  '��������� ������'
        ),
        ('CaptModelFitting',
            'Fitting'
            //  '�������� ������'
        ),
        ('CaptPatternSelection',
            'Pattern'
            //  '����� ��������'
        ),
        ('CaptSpecimenPositions',
            'Specimen positions'
            //  '����� �������� �����������'
        ),
        ('CaptSpecimenIntervals',
            'Specimen application intervals'
            //  '����� ���������� ����������'
        ),
        ('CaptSpecimenParameters',
            'Specimen parameters'
            //  '��������� �����������'
        ),
        ('HintAbout',   //  feedback
'&#169; <A TITLE="" HREF="mailto:dvmorozov@mail.ru">D.Morozov</A> [ ' +
'<A TITLE="" HREF="http://ru.linkedin.com/pub/dmitry-morozov/55/265/743/">LinkedIn</A> ]' +
', 2008-2014. Last updated: ' + CompileTime
//'������ � ���������� <A TITLE="" HREF="mailto:dvmorozov@mail.ru">D.Morozov</A>, ' +
//'2008-2009<BR><A TITLE="" HREF="mailto:dvmorozov@mail.ru">�������, ������, �����������</A>' +
//'<BR>��������� ����������: 8 ����� 2009'
        )
        (*, zameneno kartinkoy
        ('CaptButLogOut',
            'Log out'
            //  '��������� ������'
        )
        *)
        );

function PrepareTemplate_data: string;
function ReplaceStrings(Text: string; Pairs: array of TStringPair;
    PairCount: LongInt): string;
  
implementation

uses Main;

const
    PairCount = 12;// 14; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'The page of experimental data input'
            //  '�������� ����� ����������������� ������'
        ),
        ('CaptDataPoints',
            'Points of experimental data'
            //  '����� ����������������� ������'
        ),
        ('HintData',
            'File "UserFileName".'
            //  '���� "UserFileName".'
        ),  //  UserFileName budet zameneno nazvaniem vvedennym pol'zovatelem
        ('CaptDataEditing',
            'Data table'
            //  '�������������� ������'
        ),
        ('CaptArgument',
            'Argument'
            //  '��������'
        ),
        ('CaptValue',
            'Value'
            //  '��������'
        ),
        (* zameneno kartinkoy
        ('CaptButAdd',
            'Add / Change / Delete'
            //  '�������� / �������� / �������'
        ),
        *)
        ('CaptButSaveChanges',
            'Save changes'
            //  '��������� ���������'
        ),
        //('CaptButDel', '�������'),
        ('LinkNextStage',
            'Baseline'
            //  '������� � ������ ����� ����'
        ),
        ('LinkSkip',
            'Skip baseline'
            //  '���������� �������� ����'
        ),
        ('LinkProject',
            'Project'
            //'��������� � �������'
        ),
        ('HintDoAllAutomatically',
'It is possible to create model and to fit parameters completely automatically ' +
'for the data that looks like set of gauss peaks.'
//'��� ������, ������� ��� ������� � ������ ��������� �����,' +
//'���������� ����������� ��������������� �������� ������ �' +
//'�������� �� ����������.'
        ),
        (* zameneno kartinkoy
        ('CaptButDoAllAutomatically',
            'Do all automatically'
            //  '���������� �������������'
        ),
        *)
        ('HintValues',
'To add or delete the point type its argument and value in appropriate fields. ' +
'To change the point type its argument and new value. ' +
'Finally press the button.'
//'��� ���������� ����� ������� � ��������������� ���� �������� � ��������. ' +
//'��� ��������� ������������ ����� ������� �� �������� � ����� ��������. ' +
//'��� �������� ����� ������� �� �������� � ��������. ������� ������.'
        )
        //('CaptButGoToChunk', '�������')
        );
        
function ReplaceStrings_data(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_data: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('data.htm');
        Result := ReplaceStrings_data(Page.Text);
    finally
        Page.Free;
    end;
end;

function ReplaceStrings(Text: string; Pairs: array of TStringPair;
    PairCount: LongInt): string;
var Index: LongInt;
begin
    Result := Text;
    for Index := 0 to PairCount - 1 do
    begin
        Result := StringReplace(
            Result, Pairs[Index][1], Pairs[Index][2], [rfReplaceAll]);
    end;
end;

end.

