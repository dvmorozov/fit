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
            //  'Ввод данных'
        ),
        ('CaptBackDeleting',
            'Baseline'
            //  'Удаление фона'
        ),
        ('CaptModelCreating',
            'Model'
            //  'Коррекция модели'
        ),
        ('CaptModelFitting',
            'Fitting'
            //  'Подгонка модели'
        ),
        ('CaptPatternSelection',
            'Pattern'
            //  'Выбор паттерна'
        ),
        ('CaptSpecimenPositions',
            'Specimen positions'
            //  'Точки привязки экземпляров'
        ),
        ('CaptSpecimenIntervals',
            'Specimen application intervals'
            //  'Выбор интервалов применения'
        ),
        ('CaptSpecimenParameters',
            'Specimen parameters'
            //  'Параметры экземпляров'
        ),
        ('HintAbout',   //  feedback
'&#169; <A TITLE="" HREF="mailto:dvmorozov@mail.ru">D.Morozov</A> [ ' +
'<A TITLE="" HREF="http://ru.linkedin.com/pub/dmitry-morozov/55/265/743/">LinkedIn</A> ]' +
', 2008-2014. Last updated: ' + CompileTime
//'Дизайн и разработка <A TITLE="" HREF="mailto:dvmorozov@mail.ru">D.Morozov</A>, ' +
//'2008-2009<BR><A TITLE="" HREF="mailto:dvmorozov@mail.ru">Вопросы, отзывы, предложения</A>' +
//'<BR>Последнее обновление: 8 марта 2009'
        )
        (*, zameneno kartinkoy
        ('CaptButLogOut',
            'Log out'
            //  'Завершить работу'
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
            //  'Страница ввода экспериментальных данных'
        ),
        ('CaptDataPoints',
            'Points of experimental data'
            //  'Точки экспериментальных данных'
        ),
        ('HintData',
            'File "UserFileName".'
            //  'Файл "UserFileName".'
        ),  //  UserFileName budet zameneno nazvaniem vvedennym pol'zovatelem
        ('CaptDataEditing',
            'Data table'
            //  'Редактирование данных'
        ),
        ('CaptArgument',
            'Argument'
            //  'Аргумент'
        ),
        ('CaptValue',
            'Value'
            //  'Значение'
        ),
        (* zameneno kartinkoy
        ('CaptButAdd',
            'Add / Change / Delete'
            //  'Добавить / Изменить / Удалить'
        ),
        *)
        ('CaptButSaveChanges',
            'Save changes'
            //  'Сохранить изменения'
        ),
        //('CaptButDel', 'Удалить'),
        ('LinkNextStage',
            'Baseline'
            //  'Перейти к выбору точек фона'
        ),
        ('LinkSkip',
            'Skip baseline'
            //  'Пропустить удаление фона'
        ),
        ('LinkProject',
            'Project'
            //'Вернуться к проекту'
        ),
        ('HintDoAllAutomatically',
'It is possible to create model and to fit parameters completely automatically ' +
'for the data that looks like set of gauss peaks.'
//'Для данных, имеющих вид профиля с пиками гауссовой формы,' +
//'существует возможность автоматического создания модели и' +
//'подгонки ее параметров.'
        ),
        (* zameneno kartinkoy
        ('CaptButDoAllAutomatically',
            'Do all automatically'
            //  'Обработать автоматически'
        ),
        *)
        ('HintValues',
'To add or delete the point type its argument and value in appropriate fields. ' +
'To change the point type its argument and new value. ' +
'Finally press the button.'
//'Для добавления точки введите в соответствующие поля аргумент и значение. ' +
//'Для изменения существующей точки введите ее аргумент и новое значение. ' +
//'Для удаления точки введите ее аргумент и значение. Нажмите кнопку.'
        )
        //('CaptButGoToChunk', 'Перейти')
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

