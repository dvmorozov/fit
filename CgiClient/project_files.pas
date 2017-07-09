unit project_files;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_project_files: string;
function ReplaceStrings_project_files(Text: string): string;

implementation

uses data, Main;

const
    PairCount = 11; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Experimental data files'
            //'The page of experimental data associated with selected project'
            //'����� ����������������� ������, ��������� � ��������� ��������'
        ),
        ('HintProject',
            'Project "ProjectName"'
            //'������ "ProjectName"'
        ),  //  ProjectName budet zameneno dalee nazvaniem proekta
        ('HintFiles',
'The file must be text file with every line containing pair consisting of ' +
'argument and value. Any symbol could be used as delimiter except ciphers, ' +
'point and comma. When the name of file to import not assigned a new empty ' +
'file will be associated with the project.'
//'���� ������ ���� ��������� � ������ ������ ������ ��������� ���� "��������-��������". ' +
//'� �������� ����������� ����� �������������� ����� ������, ����� ����, ����� � �������. ' +
//'���� ��� �������������� ����� �� ������, �� ����� ������ ����� ���� ��� ������.'
        ),
        ('LinkResults',
            'Results'
            //'����������'
        ),
        (* zameneno kartinkoy
        ('CaptButImport',
            'Create / Import'
            //'�������� ������ / ������'
        ),
        *)
        ('CaptProjectFiles',
            'Project files'
            //'����� �������'
        ),
        ('CaptButDeleteFile',
            'Delete selected file'
            //'������� ��������� ����'
        ),
        ('CaptNewName',
            'File name'
            //'��������'
        ),
        ('CaptDescription',
            'Description'
            //'��������'
        ),
        ('LinkProjects',
            'Projects'
            //'�������'
        ),
        ('CaptCreateFile',
            'Create file'
        ),
        ('CaptLocalFileName',
            'Local file name'
        )
        );

function ReplaceStrings_project_files(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_project_files: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('project_files.htm');
        Result := ReplaceStrings_project_files(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

