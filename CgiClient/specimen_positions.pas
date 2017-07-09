unit specimen_positions;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_positions: string;

implementation

uses data, Main;

const
    PairCount = 9; // 11; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Specimen positions'
            //'The page of selection of specimen positions'
            //'Выбор точек привязки экземпляров паттерна'
        ),
        ('CaptSpecimenPosFull',
            'Specimen positions'
            //'Точки привязки экземпляров паттерна'
        ),
        ('HintSpecimenPositions',
            'Specimen positions.'
            //'Точки привязки экземпляров паттерна.'
        ),
        (* zameneno kartinkoy
        ('CaptButGenerate',
            'Generate'
            //'Генерировать'
        ),
        *)
        ('CaptPointSelection',
            'Specimen positions selection'
            //'Выбор точек привязки'
        ),
        ('CaptArgument',
            'Argument'
            //'Аргумент'
        ),
        ('CaptValue',
            'Value'
            //'Значение'
        ),
        ('CaptSpecPosition',
            ' ' // 'Is s. p.?'
            //'Точка привязки?'
        ),
        (* zameneno kartinkoy
        ('CaptButSpecPosition',
            'Select / Unselect'
            //'Точка привязки / Не точка привязки'
        ),
        *)
        ('CaptButNextStage',
            'Specimen application intervals'
            //'Перейти к выбору интервалов применения'
        ),
        ('HintValues',
'To add point to the list of specimen positions type its argument. ' +
'Then press the button. To delete press the button again.'
//'Для добавления точки в список точек привязки экземпляров паттерна введите ее аргумент. ' +
//'Нажмите кнопку. Повторное нажатие кнопки удаляет точку из списка точек привязки.'
        )
        );

function ReplaceStrings_specimen_positions(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
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

