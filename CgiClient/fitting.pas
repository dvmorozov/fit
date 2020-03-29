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
            //  'Выбор способа подгонки модели'
        ),
        ('CaptFittingProc', 'Fitting procedures'),
        ('HintMinimizeDifference',
'Minimize total difference between experimental and model curves.'
//'Поиск значений параметров, при которых расхождение между модельной и ' +
//'реальной кривой достигает минимума.'
        ),
        ('HintMinimizeNumberOfCurves1',
'Minimize number of curves on the condition that the difference '    +
'between experimental and calculated data in the specimen application interval '+
'does not exceed the given value.'
        ),
        ('HintMinimizeNumberOfCurves2',
//'Поиск значений параметров, при которых фактор расходимости не больше '         +
//'максимально допустимого и количество экземпляров паттерна минимально.<BR>'     +
'This algorithm combines minimization of the difference factor with '           +
'consecutive decreasing of number of curves. Thus the algorithm '    +
'suited for initially excessive number of curves. This may be useful in the '+
'cases of complex profile, when it is not possible to select specimen '         +
'positions unambiguously. The algorithm requires defining of the maximum '      +
'acceptable value of difference factor for each of specimen application '       +
'intervals. At that the total value of difference factor will not be more than '+
'maximum acceptable value multiplied by the number of pattern application '     +
'intervals.'
//Данный алгоритм сочетает минимизацию фактора расходимости с последовательным
//уменьшением количества экземпляров паттерна. Таким образом алгоритм
//приспособлен к изначальному заданию избыточного числа экземпляров паттерна.
//Это может быть полезно в случаях сложной формы профиля, когда невозможно
//однозначно задать точки привязки экземпляров паттерна. Алгоритм требует
//задания максимально допустимого значения функции расходимости для каждого
//интервала применения экземпляров паттерна. При этом значение полного фактора
//расходимости не превысит максимально допустимое значение умноженное на
//число интервалов применения экземпляров паттерна.
        ),
        ('HintMaxAcceptableValue',
'Maximum acceptable value of difference factor ' +
//'Максимально допустимое значение фактора расходимости ' +
'<NOBR><SUP>&#931;(I<SUB>c</SUB>-I<SUB>e</SUB>)<SUP>2</SUP></SUP>/<SUB>(&#931;I<SUB>e</SUB>)<SUP>2</SUP></SUB></NOBR>' +
' for every specimen application interval = '
//' для каждого интервала применения экземпляров паттерна = '
        )
        (*, zameneno kartinkoy
        ('CaptButMinimizeDifference',
            'Start'
            //  'Минимизировать расхождение'
        ),
        ('CaptButMinimizeNumberOfCurves',
            'Start'
            //  'Минимизировать число экземпляров паттерна'
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

