unit evaluation;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;
  
function PrepareTemplate_evaluation: string;

implementation

uses data, registration_free, Main;

const
    PairCount = 4;  //  kol-vo elementov
    PairArray: array[1..PairCount] of TStringPair = (
        ('Title',
            'Evaluation'
            //  'Стартовая страница Fit Easily'
        ),
        ('CaptLogIn',
            'Start evaluation'
            //  'Вход'
        ),
        ('HintEvaluation1',
            'You can evaluate functionality of the service without need of registration. ' +
            'The special account exists to do this. After log in with this account you will ' +
            'be able to work with some dataset. After you have left the service all changes ' +
            'you''ll have made and results you''ll have obtained will be discarded.'
            //  'Вы можете оценить работу сервиса без регистрации. Для этого '
            //  'существует специальная учетная запись. После входа с этой '
            //  'вы получаете доступ к некоторому набору данных, на котором '
            //  'можете опробовать функциональность сервиса. После выхода '
            //  'все сделанные вами изменения будут отброшены.'
        ),
        ('HintEvaluation2',
            'Log in with evaluation account.'
            //  'Вход с оценочной учетной записью.'
        )
        );

function ReplaceStrings_evaluation(Text: string): string;
var Pair: array[1..1] of TStringPair;
begin
    Result := ReplaceStrings(Text, PairArray, PairCount);
    Result := ReplaceStrings(Result, CommonPairArray, CommonPairCount);
    //  zapolneniye licenzionnogo soglascheniya
    Result := ReplaceStrings_registration(Result);
    Pair[1][1] := 'ServerName'; Pair[1][2] := ExternalIP + ':' + ExternalPort;
    Result := ReplaceStrings(Result, Pair, 1);
end;

function PrepareTemplate_evaluation: string;
var Page: TStringList;
begin
    Result := '';
    Page := TStringList.Create;
    try
        Page.LoadFromFile('evaluation.htm');
        Result := ReplaceStrings_evaluation(Page.Text);
    finally
        Page.Free;
    end;
end;


end.

