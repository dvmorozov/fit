unit projects;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_projects: string;
function ReplaceStrings_projects(Text: string): string;

implementation

uses data, Main;

const
    PairCount = 6;  //  7; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Your projects'
            //'���� �������'
        ),
        ('HintProjects',
            'Select a project'
            //'�������� ������'
        ),
        ('CaptProjects',
            'Projects'
            //'�������'
        ),
        (* zameneno kartinkoy
        ('CaptButDeleteProject',
            'Delete project'
            //'������� ������'
        ),
        ('CaptButCreateProject',
            'Create project'
            //'������� ������'
        ),
        *)
        ('CaptNewName',
            'Project name'
            //'��������'
        ),
        ('CaptDescription',
            'Description'
            //'��������'
        ),
        ('CaptCreateProject',
            'Create project'
        )
        );

function ReplaceStrings_projects(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_projects: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('projects.htm');
        Result := ReplaceStrings_projects(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

