unit start;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_start: string;

const
    CaptStartAbout: string = 'About';   //'О программе';
    HintStartAbout: string =
        //  !!! nuzhno pravil'no ukazyvat' imya stranitsy, v sootvetstvii s yazykom !!!
        'This applicaton is intended for determination of parameters of  '      +
        'models approximately coinciding with some kinds of experimental data. '+
        'The goal was to create convenient tool and fulfil some ideas about '   +
        'improving effectiveness and quality of solution.'                      +
        //'Сервис предназначен для определения параметров моделей приближенно '   +
        //'описывающих некоторые классы экспериментальных данных. Цель '          +
        //'заключалась в создании удобного инструмента и реализации некоторых '   +
        //'идей относительно улучшения эффективности и качества решения.'         +
        
        '<BR><BR>'                                                              +
        'The application has following features:'                               +
        '<UL type=disc><LI>The model is represented in the form of collection ' +
        'of instances of some base pattern found in the experimental data.'     +
        '<LI>The base pattern can be selected from a few forms: gaussian, '     +
        'lorentzian, pseudo-voigt.'                                             +
        '<LI>Optionally the background can be removed.'                           +
        '<LI>Some of instances are automatically excluded from the model'       +
        'based on theirs parameters.'                                           +
        '<LI>Fitting results are stored together with experimental data '       +
        'in form of projects.'                                                  +
        '</UL>'                                                                 +
        //'Сервис обладает следующими возможностями:'                             +
        //'<UL type=disc><LI>Представление полной модели как набора '             +
        //'экземпляров некоторого базового мотива (паттерна).'                    +
        //'<LI>Выбор одного из нескольких стандартных паттернов.'                 +
        //'<LI>Отсечение фона.'                                                   +
        //'<LI>Разделение экспериментальных данных на участки, на которых '       +
        //'подгонка осуществляется независимо (интервалы применения паттерна).'   +
        //'<LI>Автоматическое исключение некоторых экземпляров паттерна из '      +
        //'модели.'                                                               +
        //'</UL>'                                                                 +
        
        'The application is best suited for fitting data looking like a set '   +
        'of peaks, although may be used for modelling other kinds of data. '    +
        //'Лучше всего сервис '                                                   +
        //'подходит для обработки данных имеющих вид дифракционных профилей, '    +
        //'хотя может быть использован и для моделирования данных другого вида. ' +
        //'Сервис позволяет организовать ваши данные в виде '                     +
        //'проектов и хранить совместно с результатами моделирования.'            +

        '<BR><BR>'                                                              +
        'The online vesion of the application gives you following advantages: ' +
        //'Реализация программы в виде web-сервиса даёт вам следующие '           +
        //'преимущества: '                                                        +

        '<UL type=disc><LI>'                                                    +
        'Accessibility of data and results from any kind of computer or mobile device.' +
        //'Доступность данных и результатов с любого компьютера в Сети.'          +

        '<LI>'                                                                  +
        'No installation. '                                                     +
        //'Consequently no payment for license on the '                           +
        //'installation and no restrictions on the number of computers by which ' +
        //'you can work with the software. You must pay only for usage of the '   +
        //'service.'                                                              +
        
        //'Нет процедуры установки. Следовательно нет платы за лицензию на '      +
        //'установку и нет ограничений на количество компьютеров, с которых '     +
        //'можно пользоваться программой. Оплачивается только использование '     +
        //'программы.'                                                            +
        
        // Написать про сохранение данных в облаке.

        '<LI>'                                                                  +
        'You are always use up to date version of the application. '            +
        //'and no payments for a new '                                            +
        //'functionality. '                                                       +

        //'Нет необходимости обновления и оплаты дополнительной '                 +
        //'функциональности. '                                                    +
        //'Вам всегда доступна самая последняя версия программы.'                 +
        '<LI>'                                                                  +
        'But there are traditional desktop version of the application '         +
        'available for <a href="https://ServerName/fit/Fit-i386-win32.zip">Windows</a> ' +
        'and <a href="https://ServerName/fit/Fit-i386-linux.zip">Linux</a>.'     +

        '</UL>'                                                                 +
        'Working with the service consists from sequence of stages:'            +
        //'Работа с сервисом построена в виде последовательности стадий:'         +
        
        '<OL><LI>'                                                              +
        'Input or import of experimental data.'                                 +
        //'Ввод или импорт экспериментальных данных.'                             +
        
        '<LI>'                                                                  +
        'Removing '                                                             +
        '<A HREF="#" Title="Part of experimental data that can not be taken into account by the model.">' +
        'background</A> if this is necessary.'                                  +
        //'Удаление '                                                             +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_background_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'фона</A> данных, если это необходимо.'                                 +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Parametrized curve that in the best way coincides with the motif contained in the experimental data.">' +
        'pattern</A> for '                                                      +
        '<A HREF="#" Title="Set of parametrized pattern instances which in adding form a curve approximating the experimental data in the best way.">' +
        'model</A> creation.'                                                   +
        //'Выбор '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_pattern_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'паттерна</A> для создания '                                            +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_model_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'модели</A>.'                                                           +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Initial value of position parameter of the pattern instance.">' +
        'instance positions</A> of selected pattern, if the pattern has '       +
        '<A HREF="#" Title="One of pattern parameters that can be used as origin for the pattern curve. ' +
        'This may be the position of exremal or inflection point of the pattern curve.">' +
        'position parameter</A>. For data that looks like set of peaks '        +
        'this job could be done automatically, after than '                     +
        'could be corrected manually.'                                          +
        //'Выбор '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_spec_positions_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'точек привязки</A> экземпляра паттерна, если паттерн имеет '           +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_spec_positions_param_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'параметр привязки</A>. Для данных имеющих вид профиля '                +
        //'эта работа может быть выполнена автоматически, после чего '            +
        //'скорректирована вручную.'                                              +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Interval of data arguments for which pattern instance, resulting curve, difference curve values' +
        'are calculated. At least one pattern instance will be created for every pattern application interval.">' +
        'instance bounds</A>. For data that looks like set of '  +
        'peaks this job could be done automatically, after than '               +
        'could be corrected manually.'                                          +
        //'Выбор '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_specimen_application_intervals_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'интервалов применения экземпляров паттерна</A>. '                      +
        //'Для данных, имеющих вид профиля эта работа может быть выполнена '      +
        //'автоматически, после чего скорректирована вручную.'                    +

        '<LI>'                                                                  +
        'Fitting of free parameters of the model to achieve one from '          +
        'following goals:'                                                      +
        //'Подгонка свободных параметров модели таким образом, чтобы '            +
        //'достигалась одна из следующих целей:'                                  +

        '<UL type=disc>'                                                        +
        '<LI>'                                                                  +
        'Minimize total difference between experimental and calculated data.'   +
        //'Минимизировать общее расхождение.'                                     +
        
        '<LI>'                                                                  +
        'Minimize number of pattern instances by the condition that the '       +
        'difference between experimental and calculated data in the instance '  +
        'application interval does not exceed the given value.'                 +
        //'Минимизировать расхождение при минимальном числе экземпляров '         +
        //'паттерна.'                                                             +

        '</UL></OL>'                                                            +
        'In the cases when the data looks like set of peaks all the job ('      +
        'background removing, model creation and fitting) '                       +
        'can be done fully automatically.';
        //'В случаях, когда данные имеют вид профиля вся работа может быть '      +
        //'выполнена полностью автоматически.';

implementation

uses data, app;

const
    PairCount = 10; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Online service for data modelling, fitting and storing'
            //  'Стартовая страница Fit Easily'
        ),
        ('CaptFitService', 'Fit Easily'),
        ('CaptLogIn',
            'Log in'
            //  'Вход'
        ),
        ('CaptName',
            'Username (case sensitive)'
            //  'Имя'
        ),
        ('CaptPassword',
            'Password'
            //  'Пароль'
        ),
        ('HintNewUser1',
            'Before first use of the service you must to '
            //  'Чтобы начать пользоваться сервисом необходимо '
        ),
        ('Registration',
            'create an account'
            //  'зарегистрироваться'
        ),
        ('HintNewUser2',
            ' ! The registration is simple and free.' +
            'You may also log in for '
        ),
        ('Evaluation',
            'evaluation'
        ),
        ('HintNewUser3', ' purposes without registration.'
        )
        );

function ReplaceStrings_start(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_start: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('start.htm');
        Result := ReplaceStrings_start(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

