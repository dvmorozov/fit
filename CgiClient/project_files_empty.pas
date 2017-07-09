unit project_files_empty;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_project_files_empty: string;

implementation

uses data, project_files;

const
    PairCount = 2; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('CaptProjectFiles',
            'No files associated with this project'
            //'Ни одного файла еще не загружено'
        ),
        ('LinkProjects',
            'Projects'
            //'Проекты'
        )
        );

function ReplaceStrings_project_files_empty(Text: string): string;
begin
    //  !!! d.b. pervym !!!
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings_project_files(Result);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
end;

function PrepareTemplate_project_files_empty: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('project_files_empty.htm');
        Result := ReplaceStrings_project_files_empty(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

