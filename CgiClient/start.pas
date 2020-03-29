unit start;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_start: string;

const
    CaptStartAbout: string = 'About';   //'� ���������';
    HintStartAbout: string =
        //  !!! nuzhno pravil'no ukazyvat' imya stranitsy, v sootvetstvii s yazykom !!!
        'This applicaton is intended for determination of parameters of  '      +
        'models approximately coinciding with some kinds of experimental data. '+
        'The goal was to create convenient tool and fulfil some ideas about '   +
        'improving effectiveness and quality of solution.'                      +
        //'������ ������������ ��� ����������� ���������� ������� ����������� '   +
        //'����������� ��������� ������ ����������������� ������. ���� '          +
        //'����������� � �������� �������� ����������� � ���������� ��������� '   +
        //'���� ������������ ��������� ������������� � �������� �������.'         +
        
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
        //'������ �������� ���������� �������������:'                             +
        //'<UL type=disc><LI>������������� ������ ������ ��� ������ '             +
        //'����������� ���������� �������� ������ (��������).'                    +
        //'<LI>����� ������ �� ���������� ����������� ���������.'                 +
        //'<LI>��������� ����.'                                                   +
        //'<LI>���������� ����������������� ������ �� �������, �� ������� '       +
        //'�������� �������������� ���������� (��������� ���������� ��������).'   +
        //'<LI>�������������� ���������� ��������� ����������� �������� �� '      +
        //'������.'                                                               +
        //'</UL>'                                                                 +
        
        'The application is best suited for fitting data looking like a set '   +
        'of peaks, although may be used for modelling other kinds of data. '    +
        //'����� ����� ������ '                                                   +
        //'�������� ��� ��������� ������ ������� ��� ������������� ��������, '    +
        //'���� ����� ���� ����������� � ��� ������������� ������ ������� ����. ' +
        //'������ ��������� ������������ ���� ������ � ���� '                     +
        //'�������� � ������� ��������� � ������������ �������������.'            +

        '<BR><BR>'                                                              +
        'The online vesion of the application gives you following advantages: ' +
        //'���������� ��������� � ���� web-������� ��� ��� ��������� '           +
        //'������������: '                                                        +

        '<UL type=disc><LI>'                                                    +
        'Accessibility of data and results from any kind of computer or mobile device.' +
        //'����������� ������ � ����������� � ������ ���������� � ����.'          +

        '<LI>'                                                                  +
        'No installation. '                                                     +
        //'Consequently no payment for license on the '                           +
        //'installation and no restrictions on the number of computers by which ' +
        //'you can work with the software. You must pay only for usage of the '   +
        //'service.'                                                              +
        
        //'��� ��������� ���������. ������������� ��� ����� �� �������� �� '      +
        //'��������� � ��� ����������� �� ���������� �����������, � ������� '     +
        //'����� ������������ ����������. ������������ ������ ������������� '     +
        //'���������.'                                                            +
        
        // �������� ��� ���������� ������ � ������.

        '<LI>'                                                                  +
        'You are always use up to date version of the application. '            +
        //'and no payments for a new '                                            +
        //'functionality. '                                                       +

        //'��� ������������� ���������� � ������ �������������� '                 +
        //'����������������. '                                                    +
        //'��� ������ �������� ����� ��������� ������ ���������.'                 +
        '<LI>'                                                                  +
        'But there are traditional desktop version of the application '         +
        'available for <a href="https://ServerName/fit/Fit-i386-win32.zip">Windows</a> ' +
        'and <a href="https://ServerName/fit/Fit-i386-linux.zip">Linux</a>.'     +

        '</UL>'                                                                 +
        'Working with the service consists from sequence of stages:'            +
        //'������ � �������� ��������� � ���� ������������������ ������:'         +
        
        '<OL><LI>'                                                              +
        'Input or import of experimental data.'                                 +
        //'���� ��� ������ ����������������� ������.'                             +
        
        '<LI>'                                                                  +
        'Removing '                                                             +
        '<A HREF="#" Title="Part of experimental data that can not be taken into account by the model.">' +
        'background</A> if this is necessary.'                                  +
        //'�������� '                                                             +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_background_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'����</A> ������, ���� ��� ����������.'                                 +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Parametrized curve that in the best way coincides with the motif contained in the experimental data.">' +
        'pattern</A> for '                                                      +
        '<A HREF="#" Title="Set of parametrized pattern instances which in adding form a curve approximating the experimental data in the best way.">' +
        'model</A> creation.'                                                   +
        //'����� '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_pattern_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'��������</A> ��� �������� '                                            +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_model_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'������</A>.'                                                           +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Initial value of position parameter of the pattern instance.">' +
        'instance positions</A> of selected pattern, if the pattern has '       +
        '<A HREF="#" Title="One of pattern parameters that can be used as origin for the pattern curve. ' +
        'This may be the position of exremal or inflection point of the pattern curve.">' +
        'position parameter</A>. For data that looks like set of peaks '        +
        'this job could be done automatically, after than '                     +
        'could be corrected manually.'                                          +
        //'����� '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_spec_positions_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'����� ��������</A> ���������� ��������, ���� ������� ����� '           +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_spec_positions_param_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'�������� ��������</A>. ��� ������ ������� ��� ������� '                +
        //'��� ������ ����� ���� ��������� �������������, ����� ���� '            +
        //'��������������� �������.'                                              +

        '<LI>'                                                                  +
        'Selection of '                                                         +
        '<A HREF="#" Title="Interval of data arguments for which pattern instance, resulting curve, difference curve values' +
        'are calculated. At least one pattern instance will be created for every pattern application interval.">' +
        'instance bounds</A>. For data that looks like set of '  +
        'peaks this job could be done automatically, after than '               +
        'could be corrected manually.'                                          +
        //'����� '                                                                +
        //'<A HREF="#" ONCLICK="window.open(''https://ServerName/fit/_specimen_application_intervals_rus.htm'','''',''Toolbar=0,Location=0,Directories=0,Status=0,Menubar=0,Scrollbars=1,Resizable=1,Width=550,Height=100'');">' +
        //'���������� ���������� ����������� ��������</A>. '                      +
        //'��� ������, ������� ��� ������� ��� ������ ����� ���� ��������� '      +
        //'�������������, ����� ���� ��������������� �������.'                    +

        '<LI>'                                                                  +
        'Fitting of free parameters of the model to achieve one from '          +
        'following goals:'                                                      +
        //'�������� ��������� ���������� ������ ����� �������, ����� '            +
        //'����������� ���� �� ��������� �����:'                                  +

        '<UL type=disc>'                                                        +
        '<LI>'                                                                  +
        'Minimize total difference between experimental and calculated data.'   +
        //'�������������� ����� �����������.'                                     +
        
        '<LI>'                                                                  +
        'Minimize number of pattern instances by the condition that the '       +
        'difference between experimental and calculated data in the instance '  +
        'application interval does not exceed the given value.'                 +
        //'�������������� ����������� ��� ����������� ����� ����������� '         +
        //'��������.'                                                             +

        '</UL></OL>'                                                            +
        'In the cases when the data looks like set of peaks all the job ('      +
        'background removing, model creation and fitting) '                       +
        'can be done fully automatically.';
        //'� �������, ����� ������ ����� ��� ������� ��� ������ ����� ���� '      +
        //'��������� ��������� �������������.';

implementation

uses data, app;

const
    PairCount = 10; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Online service for data modelling, fitting and storing'
            //  '��������� �������� Fit Easily'
        ),
        ('CaptFitService', 'Fit Easily'),
        ('CaptLogIn',
            'Log in'
            //  '����'
        ),
        ('CaptName',
            'Username (case sensitive)'
            //  '���'
        ),
        ('CaptPassword',
            'Password'
            //  '������'
        ),
        ('HintNewUser1',
            'Before first use of the service you must to '
            //  '����� ������ ������������ �������� ���������� '
        ),
        ('Registration',
            'create an account'
            //  '������������������'
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

