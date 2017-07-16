unit specimen_intervals;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_specimen_intervals: string;

implementation

uses data, Main;

const
    PairCount = 9; // 12; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Specimen intervals'
            //'The page of selection of specimen application intervals'
            //'Выбор интервалов применения экземпляров паттерна'
        ),
        ('HintIntervals1',
            'The division of whole experimental data on specimen '              +
            'application intervals is advisable only for data looking '         +
            'like set of peaks. The possibility of that division based on '     +
            'assumption that sections of data corresponding to different peaks '+
            'obtained at different physical conditions and therefore do not '   +
            'have common parameters. Besides the difference between '           +
            'experimental and calculated profiles in regions of relatively '    +
            'great amplitude forms most important contribution into the '       +
            'difference factor. As a result visually worse coincidence '        +
            'between experimental and calculated curves achieved in regions of '+
            'relatively small amplitude.<BR>'                                   +
            'If a few specimen application intervals was created the total '    +
            'difference factor is calculated by summation of the individual '   +
            'values.<BR>'                                                       +
            'If the division on intervals has no meaning for your data simply ' +
            'create a single interval covering all the data.'
            //Разделение массива эспериментальных данных на интервалы
            //применения экземпляров паттерна целесообразно проводить
            //главным образом для данных, имеющих вид профиля. Возможность
            //такого разделения основана на предположении, что участки
            //экспериментальных данных, соответствующие различным пикам,
            //получены при физически разных условиях и поэтому не имеют
            //общих параметров. Кроме того, в значение фактора расходимости
            //по всему массиву данных указанного типа наибольший вклад дает
            //расхождение экспериментального и рассчитанного профилей в
            //районах пиков относительно большой амплитуды. В результате
            //после подгонки параметров экземпляров паттерна в районах
            //пиков относительно малой амплитуды достигается субъективно
            //худшее соответствие между экспериментальной и рассчитанной
            //кривыми, чем в областях с большой амплитудой.
            //При создании нескольких интервалов итоговое значение
            //фактора расходимости получается суммированием значений,
            //полученных на всех интервалах.
            //Если для Ваших данных разделение на интервалы не имеет смысла,
            //то просто создайте один интервал, включающий все данные.
        ),  //  !!! v kontse dolzhen byt' probel !!!
        ('HintIntervals2',
            'The specimen application intervals could be '                      +
            'generated automatically at present time only for data '            +
            'looking like set of peaks. '                                       +
            //'Интервалы применения экземпляров паттернов могут быть ' +
            //'сгенерированы автоматически только для данных, имеющих ' +
            //'вид профиля. '
            'The specimen application interval cuts down specimens which '      +
            'have the position inside.'
            //'Интервалом ограничиваются экземпляры паттерна, ' +
            //'имеющие точку привязки в нем.'
        ),
        (* zameneno kartinkoy
        ('CaptButGenerate',
            'Generate'
            //'Генерировать'
        ),
        *)
        ('CaptPointSelection',
            'Boundaries selection'
            //'Выбор границ интервалов'
        ),
        ('CaptArgument',
            'Argument'
            //'Аргумент'
        ),
        ('CaptValue',
            'Value'
            //'Значение'
        ),
        ('CaptBoundPoint',
            ' ' // 'Boundary'
            //'Точка границы?'
        ),
        (* zameneno kartinkoy
        ('CaptButBoundPoint',
            'Select / Unselect'
            //'Точка границы / Не точка границы'
        ),
        *)
        (*
        ('CaptButSkip',
            'Skip boundaries selection'
            //'Пропустить выбор границ'
        ),
        *)
        ('CaptButNextStage',
            'Specimen parameters'
            //'Перейти к заданию значений параметров'
        ),
        ('HintValues',
'To add point to the list of interval boundaries type its argument. '           +
'Then press the button. To delete press the button again.'
//'Для добавления точки в список границ интервалов применения экземпляров '     +
//'паттерна введите ее аргумент. ' +
//'Нажмите кнопку. Повторное нажатие кнопки удаляет точку из списка точек привязки.'
        )
        );

function ReplaceStrings_specimen_intervals(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_specimen_intervals: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('specimen_intervals.htm');
        Result := ReplaceStrings_specimen_intervals(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

