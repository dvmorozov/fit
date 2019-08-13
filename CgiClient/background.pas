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
            'Baseline'      // 'The page of baseline points input'
            //  'Страница ввода точек фона'
        ),
        ('CaptBackground',
            'Automatic selection of baseline points'
            //  'Автоматическая генерация'
        ),
        ('HintBackground',
'Automatic selection of baseline points is possible for the time present ' +
'only if the baseline looks like smooth concave curve.'
//'Автоматическая генерация точек возможна только если фон напоминает гладкую ' +
//'вогнутую вниз кривую.'
        ),
        ('CaptButMore',
            'More'
            //  'Дополнительно'
        ),
        (* zameneno kartinkoy
        ('CaptButGenerate',
            'Select automatically'
            //  'Генерировать'
        ),
        *)
        ('CaptPointSelection',
            'Baseline points'
            //  'Выбор точек фона'
        ),
        ('CaptArgument',
            'Argument'
            //  'Аргумент'
        ),
        ('CaptValue',
            'Value'
            //  'Значение'
        ),
        ('CaptBackPoint',
            ' ' //'Is b. p.?'
            //  'Точка фона?'
        ),
        //('CaptButBackPoint', 'Точка фона'),
        //('CaptButNotBackPoint', 'Не точка фона'),
        (* zameneno kartinkoy
        ('CaptButBackPoint',
            'Select / Unselect'
            //  'Точка фона / Не точка фона'
        ),
        *)
        ('CaptButSkip',
            'Skip baseline'
            //  'Пропустить удаление фона'
        ),
        ('CaptButDelBackground',
            'Remove baseline'
            //  'Удалить фон и перейти к созданию модели'
        ),
        ('HintValues',
'To select the point as baseline point type its argument in the input field.' +
'Then press the button. To unselect press the button again.'
//'Для выбора точки в качестве точки фона введите ее аргумент в соответствующее поле. ' +
//'Нажмите кнопку. Для отмены выбора нажмите кнопку еще раз.'
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

