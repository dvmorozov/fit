unit specimen_parameters;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_parameters: string;

implementation

uses data, Main;

const
    PairCount = 10; // 11; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Specimen parameters'
            //'The page of input of specimen parameters'
            //'Корректировка значений параметров экземпляров паттерна'
        ),
        ('CaptSpecimenParametersTable',
            'Specimen parameters table'
            //'Таблица параметров экземпляров паттерна'
        ),
        ('HintSelectedPattern',
            'Here you can set specimen parameter values.'
            //'Здесь можно задать значения параметров экземпляров паттерна.'
        ),
        (* zameneno kartinkoy
        ('CaptButUpdateSpecimen',
            'Apply'
            //'Изменить'
        ),
        *)
        ('LinkProject',
            'Project'
            //'Вернуться к проекту'
        ),
        ('LinkResults',
            'Results'
            //'Вернуться к результатам'
        ),
        ('CaptRFactorValues',
            'Difference factors:'
            //'Факторы расходимости:'
        ),
        ('CaptButNextStage',
            'Fitting'
            //'Перейти к подгонке параметров'
        ),
        (* zameneno kartinkoy
        ('CaptButSaveSpecParameters',
            'Save as result'
            //'Сохранить параметры'
        ),
        *)
        ('CaptResultName',
            'Result name'
            //'Имя таблицы параметров'
        ),
        ('CaptResultDescription',
            'Description'
            //'Дополнительная информация'
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
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
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

