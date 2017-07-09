unit registration;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function PrepareTemplate_registration: string;

implementation

uses data;

const
    PairCount = 8; //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'The registration page'
            //  'Регистрация нового пользователя'
        ),
        ('CaptFitService', 'Registration'),
        ('CaptRegistration', 'Fit Service registration'),
        ('CaptLicense', 'License agreement'),
        ('CaptName',
            'Username'
            //  'Имя'
        ),
        ('CaptPassword',
            'Password'
            //  'Пароль'
        ),
        ('CaptButRegister',
            'Register'
            //  'Зарегистрироваться'
        ),
        ('CaptRetypePassword',
            'Retype password'
            //  'Повторите пароль'
        )
        );
        
const
    YetNotRegistered:               string = 'You are yet not registered!';     //'Вы еще не зарегистрированы!';
    RegistrationPage:               string = 'Type your username. The password will be sent to you via e-mail.';
                                                                                //'Страница регистрации';
    AlreadyRegistered:              string = ' already registered.';            //' уже зарегистрирован.';
    RegisteredSucessfully:          string = 'Registration completed sucessfully';
                                                                                //'Регистрация прошла успешно';
    Differs:                        string = 'The password differs from its copy. Please refill the form.';
    InadmissibleSymbols:            string = 'Inadmissible symbols in the username.';

function ReplaceStrings_registration(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    Pair[1][1] := 'ServerName'; Pair[1][2] := Application.ServerName;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_registration: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('registration.htm');
        Result := ReplaceStrings_registration(Page.Text);
    finally
        Page.Free;
    end;
end;

end.

