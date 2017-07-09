unit registration_free;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, cgiModules;

function PrepareTemplate_registration: string;
//  dlya dostupa iz evaluation
function ReplaceStrings_registration(Text: string): string;

const
    YetNotRegistered:               string = 'You are yet not registered!';     //'Вы еще не зарегистрированы!';
    RegistrationPage:               string =
'Please closely read the license agreement. Then fill the form.';
                                                                                //'Страница регистрации';
    AlreadyRegistered:              string = ' already registered.';            //' уже зарегистрирован.';
    RegisteredSucessfully:          string = 'Registration completed sucessfully';
                                                                                //'Регистрация прошла успешно';
    Differs:                        string = 'The password differs from its copy. Please refill the form.';
    InadmissibleSymbols:            string = 'Inadmissible symbols in the username.';

implementation

uses data, Main;

const
    PairCount = 10; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Registration'
            //'The registration page'
            //  'Регистрация нового пользователя'
        ),
        ('CaptFitService', 'Registration'),
        ('CaptLogIn', 'Registration'),
        ('CaptRegistration', 'Fit Easily registration'),
        ('CaptLicense', 'End User License Agreement'),
        ('CaptName',
            'Username'
            //  'Имя'
        ),
        ('CaptPassword',
            'Password'
            //  'Пароль'
        ),
        ('CaptRetypePassword',
            'Retype password'
            //  'Повторите пароль'
        ),
        ('CaptEMail',
            'E-mail'
        ),
        (* zameneno kartinkoy
        ('CaptButRegister',
            'Register'
            //  'Зарегистрироваться'
        ),
        *)
        ('HintLicense',
'BY USING THE FIT EASILY APPLICATION ("APPLICATION" BELOW), YOU ARE AGREEING '  +
'TO BE BOUND BY THE TERMS OF THIS AGREEMENT.<BR><BR>'                           +
'Please read this license carefully before using the application. '             +
'By successful registering or continuing to use the service, you '              +
'are agreeing to be bound by the terms of this license. If you do '             +
'not agree with terms of this license, please cease using the application.'     +
'<BR><BR>'                                                                      +
'APPLICATION LICENSE<BR><BR>'                                                   +
'You are acknowledging that the copyright of the application and its '          +
'associated documentation and executables is the property of the developer. '   +
'Once registered, you are granted a non-exclusive free only for current '       +
'version license to use this application.<BR><BR>'                              +
'The current version of application is provided for free, but it is not '       +
'guaranteed for future releases.<BR><BR>'                                       +
'You should not modify, translate, reverse engineer, decompile or disassemble ' +
'the application or any part thereof or otherwise attempt to derive source '    +
'code or create derivative works there from. You are not allowed to remove, '   +
'alter or destroy any proprietary, trademark or copyright markings or notices ' +
'placed upon or contained with the application.<BR><BR>'                        +
'Your rights under this license will terminate automatically without further '  +
'notice from the developer if you fail to comply with any term(s) of this '     +
'license. Upon termination of this license, you must cease all use of the '     +
'application.<BR><BR>'                                                          +
'LIMITED WARRANTY<BR><BR>'                                                      +
'YOU ARE EXPRESSLY ACKNOWLEDGING AND AGREEING THAT USE OF THE APPLICATION IS '  +
'AT YOUR OWN RISK AND THAT THE APPLICATION AND ALL RELATED DOCUMENTATION ARE '  +
'PROVIDED "AS IS" WITHOUT ANY WARRANTIES OR CONDITIONS WHATSOEVER. DEVELOPER '  +
'WILL NOT BE LIABLE FOR DATA LOSS, DAMAGES, AND LOSS OF PROFITS OR ANY OTHER '  +
'KIND OF LOSS WHILE USING OR MISUSING THIS SOFTWARE.<BR><BR>'                   +
'DEVELOPER does NOT guarantee or warrant that:<UL type=disc>'                   +
'<LI>The application will be completely error-free or fault-free or reliable.'  +
'<LI>The application is suitable for a particular purpose.'                     +
'<LI>The application will meet your requirements.'                              +
'<LI>The defects in the application will be corrected.'                         +
'</UL>'                                                                         +
'The developer will do his best to fix bugs and to provide customers '          +
'with convenient sevices.'
        )
        );

function ReplaceStrings_registration(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_registration: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('registration_free.htm');
        Result := ReplaceStrings_registration(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

