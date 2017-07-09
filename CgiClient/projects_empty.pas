unit projects_empty;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_projects_empty: string;

implementation

uses data, projects;

const
    PairCount = 1;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('CaptProjects',
            'No projects'
            //'Ни одного проекта еще не создано'
        )
        );

function ReplaceStrings_projects_empty(Text: string): string;
begin
    //  !!! d.b. pervym !!!
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings_projects(Result);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_projects_empty: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('projects_empty.htm');
        Result := ReplaceStrings_projects_empty(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

