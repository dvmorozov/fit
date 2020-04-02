unit Unit2;

{$mode objfpc}{$H+}
{$DEFINE WINDOWS}

interface

uses
    cgiModules, Classes, curve_points_set, dat_file_loader,
    data_loader, gauss_points_set, lorentz_points_set,
    LResources, points_set, pseudo_voigt_points_set,
    SysUtils, title_points_set;

type

    { TCGIDatamodule2 }

    TCGIDatamodule2 = class(TCGIDatamodule)
    private
        { private declarations }
        procedure HandleCommand(Command: string);
        //  !!! d. ispol'zovat' cur_chunk_num, poskol'ku
        //  vysyvaetsya vo vseh sluchayah !!!
        procedure GoToBackground;
        procedure BackMore;
        procedure ComputeBackgroundPoints;
        procedure SelectBackPoint;

        procedure GoToPattern;
        procedure DeleteBackground;
        //  perehod k oknu sozdaniya patterna
        procedure GoToPatternMore;
        procedure DoPatternAction;
        //  perehod k oknu redaktirovaniya patterna
        procedure EditPattern;
        procedure DeletePattern;
        procedure CreatePatternActually;
        //  izmenenie parametrov patterna
        procedure UpdatePattern;
        procedure UpdateCurve;

        procedure GoToCurveBounds;
        procedure ComputeCurveBounds;
        procedure SaveSpecParameters;

        procedure GoToStartPage(Capt: string; Hint: string);
        procedure GoToFitting;
        procedure MinimizeDifference;
        procedure MinimizeNumberOfCurves;
        procedure DoAllAutomatically;

        //  perehod k oknu vybora tochek privyazki patterna
        procedure GoToCurvePositions;
        procedure SelectCurvePosition;
        procedure GenerateCurvePositions;

        procedure GoToCurveParameters;
        procedure UpdateCurveParameters;
        procedure LogIn;
        procedure StartEvaluation;
        procedure LogOut;
        procedure DoRegister;
        procedure GoToRegistration(const Hint: string);
        procedure GoToEvaluation;

        procedure GoToProjects;
        procedure GoToProjectsActual;
        procedure UploadData;
        procedure SaveData;
        procedure DeleteData;
        procedure DeleteProject;
        procedure CreateProject;
        procedure OpenData;

        procedure OpenFileResult;
        procedure DeleteFileResult;

        procedure AddPointToProfile;
        procedure GoToFirstData;
        procedure GoToPrevData;
        procedure GoToLastData;
        procedure GoToNextData;
        //  obrabotka klikov na ssylki na kuski
        procedure GoToChunkData;
        procedure OpenProject;
        procedure GetFileResults;
        procedure GetFileResultsActually(ProjectName: string; FileName: string);

        //  vyvodit stranitsu projecta bez predvaritel'noy proverki
        procedure OpenProjectActual(ProjectFileName: string);

        procedure RefreshSpecIntProgress;
        procedure RefreshSpecPosProgress;
        procedure RefreshBackProgress;
        procedure Refresh;
        procedure StopFitting;
        procedure StopBackGenProcess;
        procedure StopSpecIntGenProcess;
        procedure StopSpecPosGenProcess;
        //  vybor tochki granitsy intervala primeneniya ekz. patternov
        procedure SelectBoundPoint;
        //  vozvraschaet nabor strok s nomerami kuskov dlya vstavki ssylok v stranitsu
        function GetChunkNumbers(CurChunkNum: longint; ChunkCount: longint): TStringList;

        function InsertChunkLinks(TmplIn: string; ChunkNum: longint;
            Data: TPointsSet): string;
        function InsertDataByTemplate(ChunkNum: longint;
        (* dlya zapolneniya schablona *)
            Data: TPointsSet; ProjectName: string; FileName: string): string;
        function InsertKey(Template: string; Key: string): string;

        function InsertDataWithTagByTemplate(Page: string;
            Data: TPointsSet; Selected: TPointsSet): string;
        //  special'niy variant vstavki dannyh dlya granits intervalov
        function InsertDataWithTagByBounds(Page: string;
            Data: TPointsSet; SpecBounds: TPointsSet): string;
        function InsertNamesByTemplate(TemplateIn: string;
            Names: TStringList; Files: TStringList): string;

        function GetGraphURL: string;
        procedure GetGraph;
        procedure GetSavedGraph;
        //  obnovleniye vremennoy metki klyucha
        procedure WriteKeyToFile;
        procedure WriteProblemFile(ProjectName: string; FileName: string;
            ProblemID: longint);
        procedure DeleteProblem(ProblemID: longint);
        //  izvlekaet imya pol'zovatelya po kluchu;
        //  vozvraschaet False kogda kluch prosrochen
        //  (chto yavlyaetsya dopustimym sostoyaniem)
        function GetUserName: boolean;
        procedure GetDataNames(ProblemID: longint; var UserName: string;
            var ProjectName: string; var FileName: string);
        //  izvlekaet spisok projectov pol'zovatelya
        function GetUserProjects(const UserName: string;
            out ProjectFiles: TStringList): TStringList;
        //  izvlekaet spisok failov proekta
        function GetUserProjectFiles(const ProjectName: string;
            out Files: TStringList): TStringList;
        //  vozvraschaet spisok imen rezul'tatov, cherez Files peredaet
        //  nazvaniya sootvetstvuyuschih failov
        function GetProjectFileResults(const ProjectName: string;
            FileName: string; out Files: TStringList): TStringList;
        //  poluchenie indeksa dlya sozdaniya imeni novogo faila
        function GetNewFileIndex(const UserName: string;
            const ProjectName: string): longint;
        //  poluchenie indeksa dlya sozdaniya imeni novogo projekta
        function GetNewProjectIndex: longint;
        function GetProjectName(ProjectFileName: string): string;
        procedure GetProjectProperties(ProjectFileName: string;
            out ProjectName: string; out ProjectDescription: string);
        procedure GetFileProperties(ProjectFileName: string;
            FileName: string; out UserFileName: string; out FileDescription: string);
        function FillTemplateBySpecParameters(Page: string): string;
        function ExtractTemplate(var From: string; MarkerBegin: string;
            MarkerEnd: string; var Position: longint): string;
        //  vozvraschaet html-kod grecheskogo simvola, sootvetstvuyuschego
        //  dannomu imeni (ili samo imya, esli sootvetstvie ne naydeno)
        function NameToGreekSymbol(Name_: string): string;

    public
        { public declarations }
        procedure CGIDatamodule2CGIRequest(Sender: TObject);
    end;

procedure CopyDir(SrcDir: string; DstDir: string);
procedure CopyFile(SrcFile: string; DstFile: string);
procedure ShowErrorMessage(Msg: string);

var
    CGIDatamodule2: TCGIDatamodule2;

implementation

uses app, background, background_more, base_service_intf, binary_formatter,
    Data, error, evaluation, file_results, file_results_empty,
    fit_server_proxy, fitting, fitting_process,
    fitting_progress, gen_back_progress, gen_spec_int_progress,
    gen_spec_pos_progress, GeneralHashfunctions, mscr_specimen_list,
    pattern, pattern_more, project_files, project_files_empty,
    projects, projects_empty, registration_free,
    Settings,
    soap_formatter, specimen_intervals, specimen_parameters,
    specimen_parameters_file, specimen_positions,
    start;

const
    CommandNotFound: string = 'Command not found.';
    //'Команда не обнаружена.';
    UnrecognizedCommand: string = 'Unrecognized command.';
    //'Команда не известна.';
    InvalidArgument: string = 'Inadmissible argument.';
    //'Введено недопустимое значение аргумента.';
    InvalidValue: string = 'Inadmissible value.';
    //'Введено недопустимое значение.';
    InvalidSpecIndex: string = 'Inadmissible specimen number.';
    //'Введен недопустимый номер экземпляра паттерна.';
    InvalidParValue: string = 'Inadmissible parameter value.';
    //'Введено недопустимое значение параметра';
    ProblemIDIsAbsent: string = 'Lack of session number.';
    //'Отсутствует номер сеанса.';
    InvalidProblemID: string = 'Inadmissible session number';
    //'Недопустимый номер сеанса.';
    ChunkNumberIsAbsent: string = 'Lack of chunk number';
    //'Отсутствует номер куска.';
    PointMustBeSelectedFromData: string =
        'The argument must be choosed among arguments of data.';
    //'Аргумент должен быть выбран среди аргументов точек данных.';
    NoFileName: string = 'Inadmissible file name.';
    //'Недопустимое имя файла.';
    Yes: string   = 'Yes';                             //'Да';
    No: string    = 'No';                              //'Нет';
    //  tipy granits intervala
    Left: string  = 'L';
    //'Левая';
    Right: string = 'R';
    //'Правая';

    InvalidPassword: string = 'Incorrect password!';
    //'Неверный пароль!';
    RepeatInput: string  = 'Repeat input!';
    //'Повторите ввод!';
    UserNameIsAbsent: string = 'Username must not be empty!';
    //'Имя пользователя должно быть введено!';
    InadmissibleUserName: string =
        'Inadmissible username. User with such name already registered.';
    //'Недопустимое имя пользователя.';
    PasswordIsAbsent: string = 'The password must not be empty!';
    //'Пароль не может быть пустым!';
    TimeOut: string      =
        'The temporary key expired. Please log in again.';
    //'Действие временного ключа истекло. Требуется повторная аутентификация!';
    ProjectNameIsAbsent: string = 'Lack of project name.';
    //'Отсутствует имя проекта.';
    FileNameIsAbsent: string = 'Lack of file name';
    //'Отсутствует имя файла.';
    UserWithName: string = 'User with username ';
    //'Пользователь с именем ';
    PleaseLogIn: string  = 'Please type your username and password.';
    //'Введите свои реквизиты.';
    UnsuccessfulFileOperation: string = 'Unsuccessful file operation.';
    //'Ошибка операции с файлом.';
    InvalidKeyValue: string = 'Inadmissible key value.';
    //'Недопустимый код ключа.';
    Profile: string      = 'Exp. data';
    //'Данные';
    HeaderCurveNumber: string = 'Curve number';
    //'Номер экземпляра';

{$ifdef windows}
    KeyDir: string  = '..\data\tmp\';
    DataDir: string = '..\data\';
{$else}
    KeyDir: string  = '/home/www/tmp/';
    DataDir: string = '/home/www/data/';
{$endif}

    TINY: double = 1e-6;

{ TCGIDatamodule2 }

procedure TCGIDatamodule2.CGIDatamodule2CGIRequest(Sender: TObject);
var
    Page:    TStringList;
    Command: string;
begin
    try
        Page := TStringList.Create;
        try
            //  poluchenie spiska peremennyh i vybor comandy
            Page.Clear;
            Application.GetRequestVarList(Page, True);

            //  !!! zdes' ne dolzhno byt' vyvoda zagolovka, potomu chto
            //  pri obrabotke komandy mozhet vozniknut' iskluchenie !!!

            if Page.IndexOf('command') = -1 then
            begin
                //  bez etogo ne rabotaet - net avtomaticheskoy vstavki zagolovka
                //  (oschibka v EmitContentType)
                Application.ContentType := 'text/html';
                Application.EmitContentType;
                //raise ECGIAppException.Create(CommandNotFound);
                //  net komandy - perehod k startovoy stranitse
                GoToStartPage(CaptStartAbout, HintStartAbout);
{$IFDEF WINDOWS}
                //MessageBeep(SystemExclamation);
{$ELSE}
                Beep;
{$ENDIF}
                WriteLog('Start page visited', Notification_);
            end
            else
            begin
                Command := Application.RequestVariables['command'];
                if Command = 'get_graph' then
                    try
                        Application.ContentType := 'image/png';
                        Application.EmitContentType;
                        GetGraph;
                    except
                        //  soobschenie ob oschibke ne vyvoditsya,
                        //  poskol'ku zagolovok ne html
                        on E: Exception do
                            WriteLog(E.Message, Fatal);
                    end
                else
                if Command = 'get_saved_graph' then
                    try
                        Application.ContentType := 'image/png';
                        Application.EmitContentType;
                        GetSavedGraph;
                    except
                        //  soobschenie ob oschibke ne vyvoditsya,
                        //  poskol'ku zagolovok ne html
                        on E: Exception do
                            WriteLog(E.Message, Fatal);
                    end
                else
                begin
                    //  bez etogo ne rabotaet - net avtomaticheskoy vstavki zagolovka
                    //  (oschibka v EmitContentType)
                    Application.ContentType := 'text/html';
                    Application.EmitContentType;
                    HandleCommand(Command);
                end;
            end;
        finally
            Page.Free;
        end;
    except
        //  standartnyi obrabotchik isklyucheny ne ispol'zuetsya,
        //  poskolku vydaet slischkom mnogo nenuzhnoy informatsii
        on E: Exception do
        begin
            ShowErrorMessage(E.Message);
            //  vyvoditsya pomimo teksta stranitsy
{$IFDEF DEBUG}
            Page := TStringList.Create;
            Page.Clear;
            Application.GetCGIVarList(Page);
            CGIDatamodule2.AddResponseLn(
                '===================== GetCGIVarList ===================');
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn(Page.Text);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn(
                '=======================================================');
            CGIDatamodule2.AddResponseLn('<BR>');

            Page.Clear;
            Application.GetRequestVarList(Page);
            CGIDatamodule2.AddResponseLn(
                '===================== GetRequestVarList ===============');
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn(Page.Text);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn(
                '=======================================================');
            CGIDatamodule2.AddResponseLn('<BR>');

            CGIDatamodule2.AddResponseLn('AuthType: ' + Application.AuthType);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ContentLength: ' +
                IntToStr(Application.ContentLength));
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ContentType: ' + Application.ContentType);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('GatewayInterface: ' +
                Application.GatewayInterface);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('Info: ' + Application.PathInfo);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('PathTranslated: ' +
                Application.PathTranslated);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('QueryString: ' + Application.QueryString);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('RemoteAddress: ' + Application.RemoteAddress);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('RemoteHost: ' + Application.RemoteHost);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('RemoteIdent: ' + Application.RemoteIdent);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('RemoteUser: ' + Application.RemoteUser);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('RequestMethod: ' + Application.RequestMethod);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ScriptName: ' + Application.ScriptName);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ServerName: ' + Application.ServerName);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ServerPort: ' +
                IntToStr(Application.ServerPort));
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ServerProtocol: ' +
                Application.ServerProtocol);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('ServerSoftware: ' +
                Application.ServerSoftware);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPAccept: ' + Application.HTTPAccept);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPAcceptCharset: ' +
                Application.HTTPAcceptCharset);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPAcceptEncoding: ' +
                Application.HTTPAcceptEncoding);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPIfModifiedSince: ' +
                Application.HTTPIfModifiedSince);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPReferer: ' + Application.HTTPReferer);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('HTTPUserAgent: ' + Application.HTTPUserAgent);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('Email: ' + Application.Email);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn('Administrator: ' + Application.Administrator);
            CGIDatamodule2.AddResponseLn('<BR>');
            CGIDatamodule2.AddResponseLn(
                '=======================================================');
            Page.Free;
{$ENDIF}
        end;
    end;
end;

procedure TCGIDatamodule2.HandleCommand(Command: string);
begin
    WriteLog('HandleCommand', Notification_);
    if Command = 'add_point_to_profile' then
        AddPointToProfile
    else
    if Command = 'go_to_background' then
        GoToBackground
    else
    if Command = 'back_more' then
        BackMore
    else
    if Command = 'generate_back_points' then
        ComputeBackgroundPoints
    else
    if Command = 'select_back_point' then
        SelectBackPoint
    else
    if Command = 'skip_background' then
        GoToPattern
    else
    if Command = 'delete_background' then
        DeleteBackground
    else
    if Command = 'create_pattern' then
        GoToPatternMore
    else
    if Command = 'do_pattern_action' then
        DoPatternAction
    else
    if Command = 'create_pattern_actually' then
        CreatePatternActually
    else
    if Command = 'update_pattern' then
        UpdatePattern
    else
    if Command = 'update_specimen' then
        UpdateCurve
    else
    if Command = 'go_to_specimen_intervals' then
        GoToCurveBounds
    else
    if Command = 'go_to_fitting' then
        GoToFitting
    else
    if Command = 'go_to_projects' then
        GoToProjects
    else
    if Command = 'minimize_difference' then
        MinimizeDifference
    else
    if Command = 'minimize_number_of_specimens' then
        MinimizeNumberOfCurves
    else
    if Command = 'do_all_automatically' then
        DoAllAutomatically
    else
    if Command = 'log_in' then
        LogIn
    else
    if Command = 'log_out' then
        LogOut
    else
    if Command = 'registration' then
        GoToRegistration(RegistrationPage)
    else
    if Command = 'evaluation' then
        GoToEvaluation
    else
    if Command = 'start_evaluation' then
        StartEvaluation
    else
    if Command = 'register' then
        DoRegister
    else
    if Command = 'go_to_start_page' then
    begin
        GoToStartPage(CaptStartAbout, HintStartAbout);
{$IFDEF WINDOW}
        MessageBeep(SystemExclamation);
{$ELSE}
        Beep;
{$ENDIF}
        WriteLog('Back to start page', Notification_);
    end
    else
    if Command = 'generate_specimen_positions' then
        GenerateCurvePositions
    else
    if Command = 'generate_specimen_intervals' then
        ComputeCurveBounds
    else
    if Command = 'go_to_specimen_parameters' then
        GoToCurveParameters
    else
    if Command = 'select_specimen_position' then
        SelectCurvePosition
    else
    if Command = 'select_bound_point' then
        SelectBoundPoint
    else
    if Command = 'update_specimen_parameters' then
        UpdateCurveParameters
    else
    if Command = 'upload_data' then
        UploadData
    else
    if Command = 'save_data' then
        SaveData
    else
    if Command = 'refresh_back_progress' then
        RefreshBackProgress
    else
    if Command = 'refresh_spec_pos_progress' then
        RefreshSpecPosProgress
    else
    if Command = 'refresh_spec_int_progress' then
        RefreshSpecIntProgress
    else
    if Command = 'refresh' then
        Refresh
    else
    if Command = 'stop_fitting' then
        StopFitting
    else
    if Command = 'stop_back_gen_process' then
        StopBackGenProcess
    else
    if Command = 'stop_spec_int_gen_process' then
        StopSpecIntGenProcess
    else
    if Command = 'stop_spec_pos_gen_process' then
        StopSpecPosGenProcess
    else
    if Command = 'go_to_first_data' then
        GoToFirstData
    else
    if Command = 'go_to_prev_data' then
        GoToPrevData
    else
    if Command = 'go_to_last_data' then
        GoToLastData
    else
    if Command = 'go_to_next_data' then
        GoToNextData
    else
    if Command = 'go_to_chunk_data' then
        GoToChunkData
    else
    if Command = 'go_to_chunk_background' then
        GoToBackground
    else
    if Command = 'go_to_chunk_background_more' then
        BackMore
    else
    if Command = 'go_to_chunk_specimen_intervals' then
        GoToCurveBounds
    else
    if Command = 'go_to_chunk_specimen_positions' then
        GoToCurvePositions
    else
    if Command = 'open_project' then
        OpenProject
    else
    if Command = 'delete_data' then
        DeleteData
    else
    if Command = 'open_data' then
        OpenData
    else
    if Command = 'delete_project' then
        DeleteProject
    else
    if Command = 'create_project' then
        CreateProject
    else
    if Command = 'save_spec_parameters' then
        SaveSpecParameters
    else
    if Command = 'get_file_results' then
        GetFileResults
    else
    if Command = 'delete_file_result' then
        DeleteFileResult
    else
    if Command = 'open_file_result' then
        OpenFileResult
    else
        raise ECGIAppException.Create(UnrecognizedCommand);
end;

procedure TCGIDatamodule2.DeleteData;
var
    FileName:    string;
    ProjectName: string;
    DirName:     string;    //  katalog pol'zovatelya
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;

    SysUtils.DeleteFile(DirName + Slash + ProjectName + Slash + FileName);
    //  udalenie faila svoistv
    FileName := ChangeFileExt(FileName, '.properties');
    SysUtils.DeleteFile(DirName + Slash + ProjectName + Slash + FileName);

    OpenProjectActual(ProjectName);
end;

procedure TCGIDatamodule2.GoToProjects;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    GoToProjectsActual;
end;

procedure TCGIDatamodule2.DeleteProject;
var
    ProjectName: string;
    DirName: string;    //  katalog pol'zovatelya
    R: TSearchRec;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;

    //  udalenie vseh faylov v direktorii proekta
    if FindFirst(DirName + Slash + ProjectName + Slash + '*',
        faAnyFile, R) = 0 then
        try
            repeat
                if (R.Name <> '.') and (R.Name <> '..') then
                    SysUtils.DeleteFile(DirName + Slash +
                        ProjectName + Slash + R.Name);
            until FindNext(R) <> 0;
        finally
            SysUtils.FindClose(R);
        end;
    //  udalenie direktorii proekta
    RemoveDir(DirName + Slash + ProjectName);
    GoToProjectsActual;
end;

procedure TCGIDatamodule2.CreateProject;
var
    ProjectName, ProjectDescription: string;
    ProjectDirName: string;
    DirName: string;    //  katalog pol'zovatelya
    R: TSearchRec;
    P: Project_v1;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);
    ProjectDescription := Application.RequestVariables['project_description'];
    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;

    //  sozdanie direktorii proekta
    ProjectDirName := IntToStr(GetNewProjectIndex);
    CreateDir(DirName + Slash + ProjectDirName);
    //  sozdanie faila svoistv proekta
    P := Project_v1.Create(nil);
    try
        P.Name := ProjectName;
        P.Description := ProjectDescription;
        WriteProperties(DirName + Slash + ProjectDirName + Slash +
            '.properties', P);
    finally
        P.Free;
    end;
    GoToProjectsActual;
end;

procedure TCGIDatamodule2.OpenData;
var
    FileName: string;
    ProjectName: string;
    DirName:  string;    //  katalog pol'zovatelya
    DataLoader: TDATFileLoader;
    Data:     TTitlePointsSet;
    Template: string;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;

    //  sozdanie zadachi
    Proxy.CreateProblem;
    WriteProblemFile(ProjectName, FileName, Proxy.GetProblemId);

    //  zagruzka fayla v zadachu
    DataLoader := TDATFileLoader.Create(nil);
    try
        DataLoader.LoadDataSet(DirName + Slash + ProjectName + Slash + FileName);
        Data := DataLoader.GetPointsSetCopy;
        Data.Title := Profile;
        try
            //  !!! ustanovka pustogo spiska vyzyvaet iskluchenie !!!
            if Data.PointsCount <> 0 then
                Proxy.SetProfilePointsSet(Data);
        finally
            Data.Free;
        end;
    finally
        DataLoader.Free;
    end;

    Data := Proxy.GetProfileChunk(1);
    //Data := Proxy.GetProfilePointsSet;
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(1, Data, ProjectName, FileName);
        Template := InsertChunkLinks(Template, 1, Data);
        Template := InsertKey(Template, Key);
    finally
        Data.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.SaveData;
var
    FileName: string;
    ProjectName: string;
    DirName: string;    //  katalog pol'zovatelya
    Data: TTitlePointsSet;
    Template: string;
    CurChunkNum: longint;
    F: TextFile;
    i: longint;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;

    GetDataNames(Proxy.GetProblemId, UserName, ProjectName, FileName);
    //  poka dannye sohranyayutsya tol'ko v vide DAT-faila
    Data := Proxy.GetProfilePointsSet;
    try
        AssignFile(F, DirName + Slash + ProjectName + Slash + FileName);
        try
            ReWrite(F);
            for i := 0 to Data.PointsCount - 1 do
                WriteLn(F,
                    FloatToStr(Data.PointXCoord[i]) + ' ' +
                    FloatToStr(Data.PointYCoord[i]));
        finally
            CloseFile(F);
        end;
    finally
        Data.Free;
    end;

    Data := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Data, ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Data);
        Template := InsertKey(Template, Key);
    finally
        Data.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.UploadData;
var
    FileName, NewFileBaseName: string;
    ProjectName: string;
    DirName: string;    //  katalog pol'zovatelya
    LastFileIndex: longint;
    From, To_: TFileStream;
    P: File_v1;
    UserFileName, FileDescription: string;
begin
    WriteLog('UploadData', Notification_);

    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);
    UserFileName := Application.RequestVariables['user_file_name'];
    if Trim(UserFileName) = '' then
        raise Exception.Create(FileNameIsAbsent);
    FileDescription := Application.RequestVariables['file_description'];

    //  vypolnenie zaprosa
    //  proverka imeni pol'zovatelya
    DirName := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;

    //  poluchenie indeksa dlya sozdaniya imeni novogo faila
    LastFileIndex   := GetNewFileIndex(UserName, ProjectName);
    NewFileBaseName := IntToStr(LastFileIndex);

    //  izvlekaetsya imya vremennogo faila, sozdavaemogo web-serverom
    FileName := Application.UploadedFileName('file');//RequestVariables['file'];
    //if FileName = '' then raise Exception.Create(NoFileName);
    //  pustoe pole faila oznachaet, chto dolzhen byt' sozdan novyi fail

    if FileName <> '' then
    begin
{$IFDEF WINDOWS}
        //  !!! etot glyuk nablyudalsya tol'ko v denwer'e !!!
        //  udalyaetsya '/tmp/\' iz nachala...
        //  05/05/2013 nablyudaetsya v Apache 2.2 na XP.

        (*  na vista ne pomogaet - cgi-skript ne mozhet sozdat'
            vremennyi fail - ispravlen tekst v cgiapp
            (TCgiApplication.GetTempCGIFileName)
        *)
        (*
        if Length(FileName) > 6 then
            FileName := Copy(FileName, 7, Length(FileName) - 6);
        FileName := '..\..\..\tmp\' + FileName;
        WriteLog(FileName, Main.Notification_);
        *)
{$ENDIF}
        //  kopirovaniye faila v proekt
        From := nil;
        To_  := nil;
        try
            From := TFileStream.Create(FileName, fmOpenRead);
            To_  := TFileStream.Create(DirName + Slash + ProjectName +
                Slash + NewFileBaseName + '.dat', fmCreate);
            To_.CopyFrom(From, 0);
        finally
            From.Free;
            To_.Free;
        end;
    end
    else
    begin
        //  sozdaetsya pustoy fail
        To_ := nil;
        try
            To_ := TFileStream.Create(DirName + Slash + ProjectName +
                Slash + NewFileBaseName + '.dat', fmCreate);
        finally
            To_.Free;
        end;
    end;
    //  sozdanie faila svoistv faila dannyh
    P := File_v1.Create(nil);
    try
        P.Name := UserFileName;
        P.Description := FileDescription;
        WriteProperties(DirName + Slash + ProjectName + Slash +
            NewFileBaseName + '.properties', P);
    finally
        P.Free;
    end;
    OpenProjectActual(ProjectName);
end;

procedure TCGIDatamodule2.GoToFirstData;
var
    Template: string;
    Points:   TPointsSet;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    Points := Proxy.GetProfileChunk(1);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(1, Points, ProjectName, FileName);
        Template := InsertChunkLinks(Template, 1, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToPrevData;
var
    Template:    string;
    Points:      TPointsSet;
    CurChunkNum: longint;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    if CurChunkNum > 1 then
        Dec(CurChunkNum);
    Points := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Points,
            ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToLastData;
var
    Template:    string;
    Points:      TPointsSet;
    CurChunkNum: longint;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    CurChunkNum := Proxy.GetProfileChunkCount;
    Points      := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Points,
            ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToNextData;
var
    Template:    string;
    Points:      TPointsSet;
    CurChunkNum: longint;
    ChunkCount:  longint;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    ChunkCount := Proxy.GetProfileChunkCount;
    Inc(CurChunkNum);
    if CurChunkNum > ChunkCount then
        CurChunkNum := ChunkCount;
    Points := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Points,
            ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToChunkData;
var
    Template:    string;
    Points:      TPointsSet;
    CurChunkNum: longint;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    Points := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Points,
            ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.Refresh;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        //  povtor vyvoda stranitsy
        Template   := PrepareTemplate_fitting_progress;
        Pair[1][1] := 'ProblemID';
        Pair[1][2] := IntToStr(Proxy.GetProblemId);
        Template   := ReplaceStrings(Template, Pair, 1);

        Pair[1][1] := 'CalcTime';
        Pair[1][2] := Proxy.GetCalcTimeStr;
        Template   := ReplaceStrings(Template, Pair, 1);

        Template := InsertKey(Template, Key);

        //  vyvod stranitsy
        CGIDatamodule2.AddResponseLn(Template);
    end
    else
        //  vyvod resul'tatov
        GoToCurveParameters;
end;

procedure TCGIDatamodule2.RefreshBackProgress;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        //  povtor vyvoda stranitsy
        Template   := PrepareTemplate_gen_back_progress;
        Pair[1][1] := 'ProblemID';
        Pair[1][2] := IntToStr(Proxy.GetProblemId);
        Template   := ReplaceStrings(Template, Pair, 1);

        Pair[1][1] := 'CurChunkNum';
        Pair[1][2] := IntToStr(CurChunkNum);
        Template   := ReplaceStrings(Template, Pair, 1);

        Template := InsertKey(Template, Key);
        //  vyvod stranitsy
        CGIDatamodule2.AddResponseLn(Template);
    end
    else
        //  vyvod resul'tatov
        GoToBackground;
end;

procedure TCGIDatamodule2.RefreshSpecPosProgress;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        //  povtor vyvoda stranitsy
        Template   := PrepareTemplate_gen_spec_pos_progress;
        Pair[1][1] := 'ProblemID';
        Pair[1][2] := IntToStr(Proxy.GetProblemId);
        Template   := ReplaceStrings(Template, Pair, 1);

        Pair[1][1] := 'CurChunkNum';
        Pair[1][2] := IntToStr(CurChunkNum);
        Template   := ReplaceStrings(Template, Pair, 1);

        Template := InsertKey(Template, Key);
        //  vyvod stranitsy
        CGIDatamodule2.AddResponseLn(Template);
    end
    else
        //  vyvod resul'tatov
        GoToCurvePositions;
end;

procedure TCGIDatamodule2.RefreshSpecIntProgress;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        //  povtor vyvoda stranitsy
        Template   := PrepareTemplate_gen_spec_int_progress;
        Pair[1][1] := 'ProblemID';
        Pair[1][2] := IntToStr(Proxy.GetProblemId);
        Template   := ReplaceStrings(Template, Pair, 1);

        Pair[1][1] := 'CurChunkNum';
        Pair[1][2] := IntToStr(CurChunkNum);
        Template   := ReplaceStrings(Template, Pair, 1);

        Template := InsertKey(Template, Key);
        //  vyvod stranitsy
        CGIDatamodule2.AddResponseLn(Template);
    end
    else
        //  vyvod resul'tatov
        GoToCurveBounds;
end;

procedure TCGIDatamodule2.StopBackGenProcess;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        Proxy.StopAsyncOper;
        RefreshBackProgress;
    end
    else
        //  vyvod resul'tatov
        GoToBackground;
end;

procedure TCGIDatamodule2.StopSpecIntGenProcess;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        Proxy.StopAsyncOper;
        RefreshSpecIntProgress;
    end
    else
        //  vyvod resul'tatov
        GoToCurveBounds;
end;

procedure TCGIDatamodule2.StopSpecPosGenProcess;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        Proxy.StopAsyncOper;
        RefreshSpecPosProgress;
    end
    else
        //  vyvod resul'tatov
        GoToCurvePositions;
end;

procedure TCGIDatamodule2.StopFitting;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    if Proxy.AsyncOper then
    begin
        Proxy.StopAsyncOper;
        Refresh;
    end
    else
        //  vyvod resul'tatov
        GoToCurveParameters;
end;

procedure TCGIDatamodule2.AddPointToProfile;
var
    Template:    string;
    Argument, Value: double;
    Points:      TPointsSet;
    CurChunkNum: longint;
    ProjectName, FileName: string;
begin
    //  vypolnenie zaprosa
    try
        Argument := MyStrToFloat(Application.RequestVariables['argument']);
    except
        raise Exception.Create(InvalidArgument);
    end;
    try
        Value := MyStrToFloat(Application.RequestVariables['value']);
    except
        raise Exception.Create(InvalidValue);
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if FileName = '' then
        raise Exception.Create(FileNameIsAbsent);

    Proxy.AddPointToProfile(Argument, Value);
    //Points := Proxy.GetProfilePointsSet;
    //  vsegda izvlekaetsya posledniy kusok, dazhe esli v resul'tate
    //  sortirovki novaya tochka popala ne v nego...
    //  perehodit' k poslednemu kusku ne goditsya, kogda tochka byla
    //  prosto izmenena - nuzhno otlichat' takuyu situatsiyu, a poka
    //  ostavim tak... (???)
    //CurChunkNum := Proxy.GetProfileChunkCount;
    Points := Proxy.GetProfileChunk(CurChunkNum);
    try
        //  chtenie schablona i zapolnenie ego parametrov
        Template := InsertDataByTemplate(CurChunkNum, Points,
            ProjectName, FileName);
        Template := InsertChunkLinks(Template, CurChunkNum, Points);
        Template := InsertKey(Template, Key);
    finally
        Points.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

   //  !!! d. ispol'zovat' cur_chunk_num, poskol'ku
   //  vysyvaetsya vo vseh sluchayah !!!
procedure TCGIDatamodule2.GoToBackground;
var
    Template: string;
    Data, Background: TPointsSet;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //Data := Proxy.GetProfilePointsSet;
    Data := Proxy.GetProfileChunk(CurChunkNum);
    try
        Background := Proxy.GetBackgroundPoints;
        try
            //  chtenie schablona i zapolnenie ego parametrov
            Template   := PrepareTemplate_background;
            Pair[1][1] := 'ProblemID';
            Pair[1][2] := IntToStr(Proxy.GetProblemId);
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'GraphURL';
            Pair[1][2] := GetGraphURL;
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'CurChunkNum';
            Pair[1][2] := IntToStr(CurChunkNum);
            Template   := ReplaceStrings(Template, Pair, 1);

            Template := InsertDataWithTagByTemplate(
                Template, Data, Background);
            Template := InsertChunkLinks(Template, CurChunkNum, Data);
            Template := InsertKey(Template, Key);
        finally
            Background.Free;
        end;
    finally
        Data.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.BackMore;
var
    Template: string;
    Argument, Value: double;
    Data, Background: TPointsSet;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //Data := Proxy.GetProfilePointsSet;
    Data := Proxy.GetProfileChunk(CurChunkNum);
    try
        Background := Proxy.GetBackgroundPoints;
        try
            //  chtenie schablona i zapolnenie ego parametrov
            Template   := PrepareTemplate_background_more;
            Pair[1][1] := 'ProblemID';
            Pair[1][2] := IntToStr(Proxy.GetProblemId);
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'GraphURL';
            Pair[1][2] := GetGraphURL;
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'CurChunkNum';
            Pair[1][2] := IntToStr(CurChunkNum);
            Template   := ReplaceStrings(Template, Pair, 1);

            Template := InsertDataWithTagByTemplate(
                Template, Data, Background);
            Template := InsertChunkLinks(Template, CurChunkNum, Data);
            Template := InsertKey(Template, Key);
        finally
            Background.Free;
        end;
    finally
        Data.Free;
    end;

    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.ComputeBackgroundPoints;
var
    BackF: string;
    CurChunkNum: longint;
    BackFactor: double;
    Template: string;
    Pair: array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  ne ubirat' - parametry d. proveryat'sya do vypolneniya
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;

    BackF := '';
    try
        BackF := Trim(Application.RequestVariables['back_factor']);
    except
        //  nichego straschnogo - prodolzhaem s factorom po-umolchaniyu
    end;
    if BackF <> '' then
        try
            BackFactor := MyStrToFloat(BackF);
        except
            //  znachenie bylo nepravil'no vvedeno
            raise Exception.Create(InvalidValue);
        end;
    //  vypolnenie zaprosa
    if BackF <> '' then
        Proxy.SetBackFactor(BackFactor);
    Proxy.ComputeBackgroundPoints;
    //GoToBackground;
    //  protsess asinhronnyi, poetomu perehodim k oknu progressa;
    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_gen_back_progress;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(CurChunkNum);
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.SelectBackPoint;
var
    Argument, Value: double;
    Data:  TPointsSet;
    i:     longint;
    Found: boolean;
    CurChunkNum: longint;
begin
    //  vypolnenie zaprosa
    try
        Argument := MyStrToFloat(Application.RequestVariables['argument']);
    except
        raise Exception.Create(InvalidArgument);
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - parametry d. proveryat'sya do vypolneniya
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  !!! poisk vo vseh dannyh !!!
    Data := Proxy.GetProfilePointsSet;
    try
        //  tochki fona mogut byt' vybrany tol'ko sredi tochek dannyh
        //  poisk poluchennoy tochki sredi tochek dannyh
        Found := False;
        for i := 0 to Data.PointsCount - 1 do
            if Abs(Argument - Data.PointXCoord[i]) <= TINY then
            begin
                Value := Data.PointYCoord[i];
                Found := True;
                Break;  //  tochka naidena...
            end;
        if not Found then
            raise Exception.Create(PointMustBeSelectedFromData);

        Proxy.AddPointToBackground(Argument, Value);
    finally
        Data.Free;
    end;
    GoToBackground;
end;

procedure TCGIDatamodule2.GoToPattern;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_pattern;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'GraphURL';
    Pair[1][2] := GetGraphURL;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(CurChunkNum);
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.DeleteBackground;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    Proxy.SubtractBackground(False);
    GoToPattern;
end;

procedure TCGIDatamodule2.DoPatternAction;
var
    Command:     string;
    PatternType: string;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  poluchenie koda komandy
    Command := Application.RequestVariables['action'];
    if Command = '' then
        raise Exception.Create(CommandNotFound);

    if Command = 'edit_pattern' then
        //  perehod k oknu redaktirovaniya patterna
        EditPattern
    else
    if Command = 'delete_pattern' then
        DeletePattern
    else
    if Command = 'select_pattern' then
    begin
        //  izvlechenie tipa patterna
        PatternType := Application.RequestVariables['pattern_type'];
        if UpperCase(Trim(PatternType)) = 'LORENTZIAN' then
            Proxy.SetCurveType(TLorentzPointsSet.GetCurveTypeId_)
        else
        if UpperCase(Trim(PatternType)) = 'PSEUDOVOIGT' then
            Proxy.SetCurveType(TPseudoVoigtPointsSet.GetCurveTypeId_)
        else
            Proxy.SetCurveType(TGaussPointsSet.GetCurveTypeId_);
        //??? special'nye patterny poka ne obrabatyvayutsya
        GoToCurvePositions;
    end
    else
        raise Exception.Create(UnrecognizedCommand);
end;

//  perehod k oknu sozdaniya patterna
procedure TCGIDatamodule2.GoToPatternMore;
var
    CurChunkNum: longint;
    Template: string;
    Pair: array[1..1] of TStringPair;
begin
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_pattern_more;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'GraphURL';
    Pair[1][2] := GetGraphURL;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(CurChunkNum);
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

//  perehod k oknu redaktirovaniya patterna
procedure TCGIDatamodule2.EditPattern;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  ??? vypolnenie zaprosa
    GoToPatternMore;
end;

procedure TCGIDatamodule2.DeletePattern;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  ??? vypolnenie zaprosa
    GoToPattern;
end;

//  perehod k oknu vybora tochek privyazki patterna
procedure TCGIDatamodule2.GoToCurvePositions;
var
    Template: string;
    Data, SpecPositions: TPointsSet;
    Pair:     array[1..1] of TStringPair;
    CurChunkNum: longint;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //Data := Proxy.GetProfilePointsSet;
    Data := Proxy.GetProfileChunk(CurChunkNum);
    try
        SpecPositions := Proxy.GetCurvePositions;
        try
            //  chtenie schablona i zapolnenie ego parametrov
            Template   := PrepareTemplate_specimen_positions;
            Pair[1][1] := 'ProblemID';
            Pair[1][2] := IntToStr(Proxy.GetProblemId);
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'GraphURL';
            Pair[1][2] := GetGraphURL;
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'CurChunkNum';
            Pair[1][2] := IntToStr(CurChunkNum);
            Template   := ReplaceStrings(Template, Pair, 1);

            Template := InsertDataWithTagByTemplate(
                Template, Data, SpecPositions);
            Template := InsertChunkLinks(Template, CurChunkNum, Data);
            Template := InsertKey(Template, Key);
        finally
            SpecPositions.Free;
        end;
    finally
        Data.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.CreatePatternActually;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  ??? vypolnenie zaprosa
    GoToPattern;
end;

procedure TCGIDatamodule2.UpdatePattern;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  ??? vypolnenie zaprosa
    GoToPattern;
end;

procedure TCGIDatamodule2.UpdateCurve;
var
    CurChunkNum: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  ??? vypolnenie zaprosa
    GoToCurveParameters;
end;

procedure TCGIDatamodule2.GoToCurveBounds;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
    Data, Bounds: TPointsSet;
    CurChunkNum: longint;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa (poluchenie dannyh fona)
    //Data := Proxy.GetProfilePointsSet;
    Data := Proxy.GetProfileChunk(CurChunkNum);
    try
        Bounds := Proxy.GetRFactorBounds;
        try
            Template   := PrepareTemplate_specimen_intervals;
            Pair[1][1] := 'ProblemID';
            Pair[1][2] := IntToStr(Proxy.GetProblemId);
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'GraphURL';
            Pair[1][2] := GetGraphURL;
            Template   := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'CurChunkNum';
            Pair[1][2] := IntToStr(CurChunkNum);
            Template   := ReplaceStrings(Template, Pair, 1);

            Template := InsertDataWithTagByBounds(
                Template, Data, Bounds);
            Template := InsertChunkLinks(Template, CurChunkNum, Data);
            Template := InsertKey(Template, Key);
        finally
            Bounds.Free;
        end;
    finally
        Data.Free;
    end;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToFitting;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  vypolnenie zaprosa (poluchenie dannyh fona)

    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_fitting;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'MaxRFactor';
    Pair[1][2] := FloatToStrF(Proxy.GetMaxRFactor, ffGeneral, 10, 8);
    Template   := ReplaceStrings(Template, Pair, 1);

    Template := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.MinimizeDifference;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    Template   := PrepareTemplate_fitting_process;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);
    Pair[1][1] := 'HintFitting';
    Pair[1][2] := HintMinimizeDifference;
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    Proxy.MinimizeDifference;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.MinimizeNumberOfCurves;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    Template   := PrepareTemplate_fitting_process;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);
    Pair[1][1] := 'HintFitting';
    Pair[1][2] := HintMinimizeNumberOfCurves;
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    Proxy.MinimizeNumberOfCurves;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.DoAllAutomatically;
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    Template   := PrepareTemplate_fitting_process;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);
    Pair[1][1] := 'HintFitting';
    Pair[1][2] := HintDoAllAutomatically;
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    Proxy.DoAllAutomatically;
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToStartPage(Capt: string; Hint: string);
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  vypolnenie zaprosa

    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_start;
    Pair[1][1] := 'CaptService';
    Pair[1][2] := Capt;
    Template   := ReplaceStrings(Template, Pair, 1);
    Pair[1][1] := 'HintService';
    Pair[1][2] := Hint;
    Template   := ReplaceStrings(Template, Pair, 1);
    Pair[1][1] := 'ServerName';
    Pair[1][2] := ExternalIP;
    Template   := ReplaceStrings(Template, Pair, 1);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToRegistration(const Hint: string);
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  vypolnenie zaprosa

    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_registration;
    Pair[1][1] := 'HintRegistration';
    Pair[1][2] := Hint;
    Template   := ReplaceStrings(Template, Pair, 1);

    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.GoToEvaluation;
var
    Template: string;
begin
    //  vypolnenie zaprosa

    //  chtenie schablona i zapolnenie ego parametrov
    Template := PrepareTemplate_evaluation;

    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

function TCGIDatamodule2.InsertKey(Template: string; Key: string): string;
var
    Pair: array[1..1] of TStringPair;
begin
    Pair[1][1] := 'Key';
    Pair[1][2] := Key;
    Result     := ReplaceStrings(Template, Pair, 1);
end;

function TCGIDatamodule2.GetNewFileIndex(const UserName: string;
    const ProjectName: string): longint;
var
    DatName: string;
    //DatExt: string;
    //ExtPos: LongInt;
    R: TSearchRec;
    FileIndex: longint;
begin
    Result := 0;
    if FindFirst(DataDir + UserName + Slash + ProjectName + Slash +
        '*.dat', faArchive + faAnyFile, R) = 0 then
        try
            repeat
                try
                    DatName := ExtractFileName(R.Name);

                    DatName := ChangeFileExt(DatName, '');
                    //DatExt := ExtractFileExt(R.Name);
                    //ExtPos := Pos(DatExt, DatName);
                    //Delete(DatName, ExtPos, Length(DatExt));

                    //  dopuskaetsya nalichie failov, imenovannyh ne chislami
                    FileIndex := StrToInt(DatName);
                    if FileIndex > Result then
                        Result := FileIndex;
                except
                end;
            until FindNext(R) <> 0;
        finally
            SysUtils.FindClose(R);
        end;
    //  sozdanie imeni novogo faila
    Inc(Result);
end;

function TCGIDatamodule2.GetNewProjectIndex: longint;
var
    R:   TSearchRec;
    FileIndex: longint;
    Dir: string;
begin
    Result := 0;
    //  !!! ustanovka faDirectory pochemu-to ne rabotaet pod linux
    //  poka rabotosposobnost' obespechivaetsya tem, chto vse faily,
    //  krome pwd schitayutsya katalogami !!!
    if FindFirst(DataDir + UserName + Slash + '*',
        (* faDirectory *) faAnyFile, R) = 0 then
        try
            repeat
                //if R.Attr = faDirectory then
                //begin
                Dir := ExtractFileName(R.Name);
                if (Dir <> '.') and (Dir <> '..') and
                    (Dir <> 'pwd') then
                    try
                        //  dopuskaetsya nalichie papok, imenovannyh ne chislami
                        FileIndex := StrToInt(R.Name);
                        if FileIndex > Result then
                            Result := FileIndex;
                    except
                    end;
                //end;
            until FindNext(R) <> 0;
        finally
            SysUtils.FindClose(R);
        end;
    //  sozdanie imeni novogo proekta
    Inc(Result);
end;

function TCGIDatamodule2.GetProjectFileResults(const ProjectName: string;
    FileName: string; out Files: TStringList): TStringList;
var
    R:      TSearchRec;
    SPName: string;
    Content: TStringList;
    NamePos, i: longint;
    TermFound: boolean;

const
    BegMarker: string = '<META name="ResultName" content="';
begin
    Result  := nil;
    Files   := nil;
    Content := nil;
    try
        Result  := TStringList.Create;
        Files   := TStringList.Create;
        Content := TStringList.Create;

        try
            FileName := ChangeFileExt(FileName, '');
            if FindFirst(DataDir + UserName + Slash + ProjectName +
                Slash + FileName + '.*.sp', faArchive + faAnyFile, R) = 0 then
                try
                    repeat
                        SPName := ExtractFileName(R.Name);
                        Files.Add(SPName);
                        try
                            Content.LoadFromFile(DataDir + UserName +
                                Slash + ProjectName + Slash + SPName);
                        except
                            raise Exception.Create(UnsuccessfulFileOperation);
                        end;

                        ChangeFileExt(SPName, '');

                        NamePos := Pos(BegMarker, Content.Text);
                        if NamePos <> 0 then
                        begin
                            NamePos   := NamePos + Length(BegMarker);
                            TermFound := False;
                            for i := NamePos to Length(Content.Text) do
                                if Content.Text[i] = '"' then
                                begin
                                    TermFound := True;
                                    Break;
                                end;

                            if TermFound then
                                Dec(i);

                            if i - NamePos + 1 > 0 then
                                SPName := Copy(Content.Text, NamePos, i - NamePos + 1);
                        end;

                        Result.Add(SPName);
                    until FindNext(R) <> 0;
                finally
                    SysUtils.FindClose(R);
                end;
        finally
            Content.Free;
        end;
    except
        Result.Free;
        Files.Free;
        raise;
    end;
end;

function TCGIDatamodule2.GetUserProjectFiles(const ProjectName: string;
    out Files: TStringList): TStringList;
var
    R: TSearchRec;
    FileName: string;
    P: File_v1;
begin
    Result := nil;
    Files  := nil;
    try
        Result := TStringList.Create;
        Files  := TStringList.Create;

        if FindFirst(DataDir + UserName + Slash + ProjectName +
            Slash + '*.dat', faArchive + faAnyFile, R) = 0 then
            try
                repeat
                    FileName := ExtractFileName(R.Name);
                    Files.Add(FileName);

                    FileName := ChangeFileExt(FileName, '.properties');
                    //  chtenie svoystv faila
                    P := ReadFileProperties_v1(DataDir + UserName +
                        Slash + ProjectName + Slash + FileName);
                    try
                        Result.Add(P.Name);
                    finally
                        P.Free;
                    end;
                until FindNext(R) <> 0;
            finally
                SysUtils.FindClose(R);
            end;
    except
        Result.Free;
        Files.Free;
        raise;
    end;
end;

function TCGIDatamodule2.GetUserProjects(const UserName: string;
    out ProjectFiles: TStringList): TStringList;
var
    R:   TSearchRec;
    Dir: string;
begin
    Result := nil;
    ProjectFiles := nil;
    try
        Result := TStringList.Create;
        ProjectFiles := TStringList.Create;
        //  !!! ustanovka faDirectory pochemu-to ne rabotaet pod linux
        //  poka rabotosposobnost' obespechivaetsya tem, chto vse faily,
        //  krome pwd schitayutsya katalogami !!!
        if FindFirst(DataDir + UserName + Slash + '*',
            (* faDirectory *) faAnyFile, R) = 0 then
            try
                repeat
                    //if R.Attr = faDirectory then
                    //begin
                    Dir := ExtractFileName(R.Name);
                    if (Dir <> '.') and (Dir <> '..') and
                        (Dir <> 'pwd') then
                    begin
                        //  chtenie svoystv proekta
                        Result.Add(GetProjectName(Dir));
                        ProjectFiles.Add(Dir);
                    end;
                    //end;
                until FindNext(R) <> 0;
            finally
                SysUtils.FindClose(R);
            end;
    except
        Result.Free;
        ProjectFiles.Free;
        raise;
    end;
end;

//  vozvraschaet False kogda kluch prosrochen
function TCGIDatamodule2.GetUserName: boolean;
var
    F:    TextFile;
    Temp: string;
    KeyFileName: string;
    TimeStamp, Today: TTimeStamp;
    DeltaTime: int64;
begin
    Result      := True;
    KeyFileName := KeyDir + Key + '.key';
    try
        AssignFile(F, KeyFileName);
        try
            Reset(F);
            ReadLn(F, UserName);
            //??? proverka dopustimosti imeni pol'zovatelya
            ReadLn(F, Temp);
            TimeStamp.Date := StrToInt(Temp);
            ReadLn(F, Temp);
            TimeStamp.Time := StrToInt(Temp);
        finally
            CloseFile(F);
        end;
    except
        //  net fayla, sootvetstvuyuschego kluchu, ili fayl isporchen
        raise Exception.Create(InvalidKeyValue);
    end;

    //  proverka vremeni deystviya klyucha;
    //  proverka imeni pol'zovatelya
    Today     := DateTimeToTimeStamp(Now);
    DeltaTime :=
        int64(TimeStampToMSecs(Today)) - int64(TimeStampToMSecs(TimeStamp));
    if (DeltaTime > 3600000) or (not DirectoryExists(DataDir + UserName)) then
    begin
        //  kluch prosrochen
        SysUtils.DeleteFile(KeyFileName);
        Result := False;
        Exit;
    end;
    //  obnovlenie vremennoy metki klucha
    WriteKeyToFile;
end;

procedure TCGIDatamodule2.DeleteProblem(ProblemID: longint);
begin
    SysUtils.DeleteFile(KeyDir + IntToStr(ProblemID) + '.problem');
    Proxy.DiscardProblem(ProblemId);
end;

procedure TCGIDatamodule2.WriteProblemFile(ProjectName: string;
    FileName: string; ProblemID: longint);
var
    ProblemFileName: string;
    //TimeStamp: TTimeStamp;
    F: TextFile;
begin
    //TimeStamp := DateTimeToTimeStamp(Now);
    ProblemFileName := KeyDir + IntToStr(ProblemID) + '.problem';
    try
        AssignFile(F, ProblemFileName);
        try
            Rewrite(F);
            WriteLn(F, UserName);
            WriteLn(F, ProjectName);
            WriteLn(F, FileName);
            //WriteLn(F, IntToStr(TimeStamp.Date));
            //WriteLn(F, IntToStr(TimeStamp.Time));
        finally
            CloseFile(F);
        end;
    except
        raise Exception.Create(UnsuccessfulFileOperation);
    end;
end;

procedure TCGIDatamodule2.GetDataNames(ProblemID: longint; var UserName: string;
    var ProjectName: string; var FileName: string);
var
    ProblemFileName: string;
    F: TextFile;
begin
    ProblemFileName := KeyDir + IntToStr(ProblemID) + '.problem';
    try
        AssignFile(F, ProblemFileName);
        try
            Reset(F);
            ReadLn(F, UserName);
            ReadLn(F, ProjectName);
            ReadLn(F, FileName);
        finally
            CloseFile(F);
        end;
    except
        raise Exception.Create(InvalidProblemID);
    end;
end;

procedure TCGIDatamodule2.WriteKeyToFile;
var
    KeyFileName: string;
    TimeStamp: TTimeStamp;
    F: TextFile;
begin
    TimeStamp   := DateTimeToTimeStamp(Now);
    KeyFileName := KeyDir + Key + '.key';
    try
        AssignFile(F, KeyFileName);
        try
            Rewrite(F);
            WriteLn(F, UserName);
            WriteLn(F, IntToStr(TimeStamp.Date));
            WriteLn(F, IntToStr(TimeStamp.Time));
        finally
            CloseFile(F);
        end;
    except
        raise Exception.Create(UnsuccessfulFileOperation);
    end;
end;

procedure TCGIDatamodule2.DoRegister;
var
    UserName: string;
    Password, Password2: string;
    DirName, EMail: string;
    F: TextFile;
    i: longint;
    InadmissibleChars: set of
    char = ['\', '/', '*', '.', ',', '!', '?', ';', ':', '{', '}',
    '(', ')', '[', ']', '"', '''', '|', '@', '#', '$', '%', '^',
    '&', '_', '-', '+', '=', '<', '>'];
begin
    //  vypolnenie zaprosa
    UserName := Application.RequestVariables['username'];
    if Trim(UserName) = '' then
    begin
        GoToRegistration(UserNameIsAbsent);
        Exit;
    end;
    Password := Trim(Application.RequestVariables['password']);
    if Password = '' then
    begin
        GoToRegistration(PasswordIsAbsent);
        Exit;
    end;
    Password2 := Trim(Application.RequestVariables['password2']);
    if Password <> Password2 then
    begin
        GoToRegistration(Differs);
        Exit;
    end;
    EMail := Trim(Application.RequestVariables['email']);
    //  proverka dopustimosti simvolov v imeni pol'zovatelya
    for i := 1 to Length(Password) do
        if (Password[i] in InadmissibleChars) then
        begin
            GoToRegistration(InadmissibleSymbols);
            Exit;
        end;
    Password := LowerCase(IntToHex(JSHash(Password), 8));
    //  proverka dopustimosti imeni pol'zovatelya
    DirName  := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        //  registratsiya novogo pol'zovatelya
        CreateDir(DirName);
        AssignFile(F, DirName + Slash + 'pwd');
        try
            try
                ReWrite(F);
                WriteLn(F, Password);
                WriteLn(F, EMail);
            finally
                CloseFile(F);
            end;
{$IFDEF WINDOW}
            MessageBeep(SystemExclamation);
{$ELSE}
            Beep;
{$ENDIF}
            WriteLog('New registration: ' + UserName, Notification_);
        except
            raise Exception.Create(UnsuccessfulFileOperation);
        end;
        GoToStartPage(RegisteredSucessfully, PleaseLogIn);
    end
    else
        GoToRegistration(UserWithName + UserName + AlreadyRegistered);
end;

procedure TCGIDatamodule2.LogOut;
var
    KeyFileName: string;
begin
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(CaptStartAbout, HintStartAbout);
        Exit;
    end;
    //  proverka togo, chto kluch deystvitel'no
    //  yavlyaetsya kluchom
    if not GetUserName then
    begin
        GoToStartPage(CaptStartAbout, HintStartAbout);
        Exit;
    end;
    try
        //  !!! syuda prihodit iz okna parametrov ekzemplyarov patterna !!!
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
        //  udalenie zadachi po zaverschenii raboty
        DeleteProblem(Proxy.GetProblemId);
    except
        //  ProblemID - optsionalnyi parametr
    end;
    KeyFileName := KeyDir + Key + '.key';
    SysUtils.DeleteFile(KeyFileName);
    GoToStartPage(CaptStartAbout, HintStartAbout);
end;

procedure TCGIDatamodule2.LogIn;
var
    Password: string;
    DirName: string;
    PwdFileName: string;
    PassFromFile: string;
    i: longint;
    SymbolRange: longint;
    F: TextFile;
begin
    //  vypolnenie zaprosa
    UserName := Application.RequestVariables['username'];
    if Trim(UserName) = '' then
    begin
        GoToStartPage(RepeatInput, UserNameIsAbsent);
        Exit;
    end;
    Password := Application.RequestVariables['password'];
    if Trim(Password) = '' then
    begin
        GoToStartPage(RepeatInput, PasswordIsAbsent);
        Exit;
    end;
    Password := LowerCase(IntToHex(JSHash(Password), 8));
    //  proverka imeni pol'zovatelya
    DirName  := DataDir + UserName;
    if not DirectoryExists(DirName) then
    begin
        GoToRegistration(YetNotRegistered);
        Exit;
    end;
    //  chtenie parolya
    PwdFileName := DirName + Slash + 'pwd';
    try
        AssignFile(F, PwdFileName);
        try
            Reset(F);
            ReadLn(F, PassFromFile);    //  zdes' mozhet byt' iskluchenie
        finally
            CloseFile(F);
        end;
    except
        raise Exception.Create(InvalidPassword);
    end;
    //  proverka parolya
    if PassFromFile <> Password then
    begin
        GoToStartPage(RepeatInput, InvalidPassword);
        Exit;
    end;

    //  autentifikatsiya proschla uspeschno
    //  sozdanie klyuchevogo fayla
    Randomize;
    SetLength(Key, 20);
    for i := 1 to 20 do
    begin
        if i = 1 then
            //  pervyi simvol - ne tsifra
            SymbolRange := Random(2)
        else
            SymbolRange := Random(3);

        case SymbolRange of
            0: Key[i] := Chr(Random(26) + $41);
            1: Key[i] := Chr(Random(26) + $61);
            2: Key[i] := Chr(Random(10) + $30);
        end;
    end;

    WriteKeyToFile;
    GoToProjectsActual;
    WriteLog('Login: ' + UserName, Notification_);
end;

procedure TCGIDatamodule2.StartEvaluation;
var
    DirName, EvalDirName: string;
    PassFromFile: string;
    i: longint;
    SymbolRange: longint;
    F: TextFile;
begin
    //  vypolnenie zaprosa
    Randomize;
    SetLength(Key, 20);
    //  sozdaetsya sluchaynoe imya pol'zovatelya
    for i := 1 to 20 do
    begin
        if i = 1 then
            //  pervyi simvol - ne tsifra
            SymbolRange := Random(2)
        else
            SymbolRange := Random(3);

        case SymbolRange of
            0: Key[i] := Chr(Random(26) + $41);
            1: Key[i] := Chr(Random(26) + $61);
            2: Key[i] := Chr(Random(10) + $30);
        end;
    end;
    UserName := '$' + Key;
    //  dobavlyaetsya prefiks dlya vydeleniya
    //  imen ocenochnyh akkauntov
    //  ??? nuzhno dobavlyat' datu v imya faila
    DirName  := DataDir + UserName;
    CreateDir(DirName);
    //  kopirovaniye ocenochnyh failov
    EvalDirName := DataDir + 'Evaluator';
    CopyDir(EvalDirName, DirName);

    //  sozdanie klyuchevogo fayla
    for i := 1 to 20 do
    begin
        if i = 1 then
            //  pervyi simvol - ne tsifra
            SymbolRange := Random(2)
        else
            SymbolRange := Random(3);

        case SymbolRange of
            0: Key[i] := Chr(Random(26) + $41);
            1: Key[i] := Chr(Random(26) + $61);
            2: Key[i] := Chr(Random(10) + $30);
        end;
    end;

    WriteKeyToFile;
    GoToProjectsActual;
    WriteLog('Login: ' + UserName, Notification_);
end;

procedure TCGIDatamodule2.GoToProjectsActual;
var
    Template: string;
    Projects, ProjectFiles: TStringList;
begin
    Projects := GetUserProjects(UserName, ProjectFiles);
    try
        if Projects.Count <> 0 then
        begin
            Template := PrepareTemplate_projects;
            Template := InsertNamesByTemplate(Template, Projects, ProjectFiles);
        end
        else
            Template := PrepareTemplate_projects_empty;
    finally
        Projects.Free;
        ProjectFiles.Free;
    end;
    Template := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.OpenProject;
var
    ProjectName, FileName: string;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  !!! syuda prihodit iz okna parametrov ekzemplyarov patterna !!!
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
        GetDataNames(Proxy.GetProblemId, UserName, ProjectName, FileName);
        //  udalenie zadachi po zaverschenii raboty
        DeleteProblem(Proxy.GetProblemId);
    except
        //  ProblemID - optsional'nyi parametr
        ProjectName := Application.RequestVariables['project_name'];
        if Trim(ProjectName) = '' then
            raise Exception.Create(ProjectNameIsAbsent);
    end;
    //  vypolnenie zaprosa
    OpenProjectActual(ProjectName);
end;

procedure TCGIDatamodule2.GetFileResults;
var
    ProjectName: string;
    FileName:    string;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  !!! syuda prihodit iz okna parametrov ekzemplyarov patterna !!!
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
        GetDataNames(Proxy.GetProblemId, UserName, ProjectName, FileName);
        //  udalenie zadachi po zaverschenii raboty
        DeleteProblem(Proxy.GetProblemId);
    except
        //  ProblemID - optsional'nyi parametr
        ProjectName := Application.RequestVariables['project_name'];
        if Trim(ProjectName) = '' then
            raise Exception.Create(ProjectNameIsAbsent);
        //  zdes' peredaetsya imya faila dannyh, dlya
        //  kotorogo nuzhno vyvesti spisok resul'tatov
        FileName := Application.RequestVariables['file_name'];
        if Trim(FileName) = '' then
            raise Exception.Create(FileNameIsAbsent);
    end;
    //  vypolnenie zaprosa
    GetFileResultsActually(ProjectName, FileName);
end;

procedure TCGIDatamodule2.GetFileResultsActually(ProjectName: string;
    FileName: string);
var
    Template: string;
    Names, Files: TStringList;
    Pair: array[1..1] of TStringPair;
    UserFileName, FileDescription: string;
begin
    Names := GetProjectFileResults(ProjectName, FileName, Files);
    try
        if Names.Count <> 0 then
        begin
            Template := PrepareTemplate_file_results;
            Template := InsertNamesByTemplate(Template, Names, Files);
        end
        else
            Template := PrepareTemplate_file_results_empty;
    finally
        Names.Free;
        Files.Free;
    end;
    Template := InsertKey(Template, Key);

    Pair[1][1] := 'ProjectFileName';
    Pair[1][2] := ProjectName;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'DataFileName';
    Pair[1][2] := FileName;
    Template   := ReplaceStrings(Template, Pair, 1);

    GetFileProperties(ProjectName, FileName, UserFileName, FileDescription);
    Pair[1][1] := 'UserFileName';
    Pair[1][2] := UserFileName;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'FileDescription';
    Pair[1][2] := FileDescription;
    Template   := ReplaceStrings(Template, Pair, 1);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.OpenFileResult;
var
    ProjectName: string;
    FileName: string;
    Template: string;
    Pair:    array[1..1] of TStringPair;
    Content: TStringList;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if Trim(FileName) = '' then
        raise Exception.Create(FileNameIsAbsent);

    Content := TStringList.Create;
    try
        try
            Content.LoadFromFile(
                DataDir + UserName + Slash + ProjectName + Slash + FileName);
        except
            raise Exception.Create(UnsuccessfulFileOperation);
        end;
        Template := Content.Text;
    finally
        Content.Free;
    end;

    Template := InsertKey(Template, Key);

    Pair[1][1] := 'ProjectName';
    Pair[1][2] := ProjectName;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'GraphFileName';
    Pair[1][2] := ChangeFileExt(FileName, '.png');
    Template   := ReplaceStrings(Template, Pair, 1);

    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.DeleteFileResult;
var
    ProjectName: string;
    FileName: string;
    DataFileName: string;
    R: TSearchRec;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;

    ProjectName := Application.RequestVariables['project_name'];
    if Trim(ProjectName) = '' then
        raise Exception.Create(ProjectNameIsAbsent);

    FileName := Application.RequestVariables['file_name'];
    if Trim(FileName) = '' then
        raise Exception.Create(FileNameIsAbsent);

    DataFileName := Application.RequestVariables['data_file_name'];
    if Trim(DataFileName) = '' then
        raise Exception.Create(FileNameIsAbsent);

    //  ??? zaverschaetsya vydachei soobscheniya s preduprezhdeniem

    //  ??? eta chast' dolzhna vysyvat'sya po special'noy komande,
    //  pri podtverzhdenii udaleniya;
    //  poisk i udalenie vseh failov, otnosyaschihsya k dannomu rezul'tatu
    FileName := ChangeFileExt(FileName, '');
    if FindFirst(DataDir + UserName + Slash + ProjectName + Slash +
        FileName + '.*', faArchive + faAnyFile, R) = 0 then
        try
            repeat
                SysUtils.DeleteFile(DataDir + UserName + Slash +
                    ProjectName + Slash + R.Name);
            until FindNext(R) <> 0;
        finally
            SysUtils.FindClose(R);
        end;
    GetFileResultsActually(ProjectName, DataFileName);
end;

function TCGIDatamodule2.GetProjectName(ProjectFileName: string): string;
var
    P: Project_v1;
begin
    P := ReadProjectProperties_v1(DataDir + UserName + Slash +
        ProjectFileName + Slash + '.properties');
    try
        Result := P.Name;
    finally
        P.Free;
    end;
end;

procedure TCGIDatamodule2.GetFileProperties(ProjectFileName: string;
    FileName: string; out UserFileName: string; out FileDescription: string);
var
    P: File_v1;
begin
    //  chtenie svoystv faila
    FileName := ChangeFileExt(FileName, '.properties');
    P := ReadFileProperties_v1(DataDir + UserName + Slash +
        ProjectFileName + Slash + FileName);
    try
        UserFileName    := P.Name;
        FileDescription := P.Description;
    finally
        P.Free;
    end;
end;

procedure TCGIDatamodule2.GetProjectProperties(ProjectFileName: string;
    out ProjectName: string; out ProjectDescription: string);
var
    P: Project_v1;
begin
    P := ReadProjectProperties_v1(DataDir + UserName + Slash +
        ProjectFileName + Slash + '.properties');
    try
        ProjectName := P.Name;
        ProjectDescription := P.Description;
    finally
        P.Free;
    end;
end;

procedure TCGIDatamodule2.OpenProjectActual(ProjectFileName: string);
var
    Template: string;
    Names, Files: TStringList;
    Pair: array[1..1] of TStringPair;
    ProjectName, ProjectDescription: string;
begin
    Names := GetUserProjectFiles(ProjectFileName, Files);
    try
        if Names.Count <> 0 then
        begin
            Template := PrepareTemplate_project_files;
            Template := InsertNamesByTemplate(Template, Names, Files);
        end
        else
            Template := PrepareTemplate_project_files_empty;
    finally
        Names.Free;
        Files.Free;
    end;
    Template := InsertKey(Template, Key);

    Pair[1][1] := 'ProjectFileName';
    Pair[1][2] := ProjectFileName;
    Template   := ReplaceStrings(Template, Pair, 1);

    GetProjectProperties(ProjectFileName, ProjectName, ProjectDescription);

    Pair[1][1] := 'ProjectName';
    Pair[1][2] := ProjectName;
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'ProjectDescription';
    Pair[1][2] := ProjectDescription;
    Template   := ReplaceStrings(Template, Pair, 1);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure ShowErrorMessage(Msg: string);
var
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  vypolnenie zaprosa

    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_error;
    Pair[1][1] := 'HintDescription';
    Pair[1][2] := Msg;
    Template   := ReplaceStrings(Template, Pair, 1);

    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

const
    BeginMarker: string = 'BeginDataTemplate';
    EndMarker: string   = 'EndDataTemplate';
    BeginChunkTemplate: string = 'BeginChunkTemplate';
    EndChunkTemplate: string = 'EndChunkTemplate';

function TCGIDatamodule2.InsertChunkLinks(TmplIn: string; ChunkNum: longint;
    Data: TPointsSet): string;
var
    TmplBegin, TmplEnd, TmplLen: longint;
    Template, DataStr: string;
    i:    longint;
    Pair: array[1..1] of TStringPair;

    List: TStringList;
    ChunkCount: longint;
begin
    Result    := TmplIn;
    //  zapolnenie ssylok na kuski
    TmplBegin := Pos(BeginChunkTemplate, Result);
    TmplEnd   := Pos(EndChunkTemplate, Result);
    if (TmplBegin > 0) and (TmplEnd > 0) then
    begin
        TmplLen  := TmplEnd - TmplBegin - Length(BeginChunkTemplate);
        Template := Copy(Result, TmplBegin + Length(BeginChunkTemplate), TmplLen);
        Delete(Result, TmplBegin, TmplLen + Length(BeginChunkTemplate) +
            Length(EndChunkTemplate));

        if Assigned(Data) then
        begin
            //  etu funktsiyu mozhno vysyvat' tol'ko kogda
            //  ustanovleny dannye
            ChunkCount := Proxy.GetProfileChunkCount;
            List := GetChunkNumbers(ChunkNum, ChunkCount);
            try
                for i := 0 to List.Count - 1 do
                begin
                    Pair[1][1] := 'ChunkNumber';
                    Pair[1][2] := List.Strings[i];
                    DataStr    := ReplaceStrings(Template, Pair, 1);

                    Insert(DataStr, Result, TmplBegin);
                    TmplBegin := TmplBegin + Length(DataStr);
                end;
            finally
                List.Free;
            end;
        end;    //  ssylki mozhno ne vstavlyat'
    end;
end;

function ValToStr(Value: double): string;
begin
    //  ogranichenie min. znacheniya pri isp. ffGeneral;
    //  chisla po modulyu men'she takogo - 0.00001 -
    //  otobrazhayutsya v eksponentsial'noy forme, poetomu
    //  budem schitat' takoe chislo minimal'no dopustimym
    //if Abs(Value) < 0.00001 then Value := 0;
    //  chisla takogo tipa - 10000000.999999 - izobrazhayutsya
    //  kak est', t.e. so vsemi razryadami;

    //  ffGeneral ne podhodit - ploho, kogda u chisel v
    //  tablitse raznoe chislo znakov posle zapyatoy;
    //  pri ispol'zovanii ffFixed malen'kie chisla
    //  okruglyayutsya do nulya avtomaticheski
    Result := FloatToStrF(Value, ffFixed, 8, 4);
end;

function TCGIDatamodule2.InsertDataByTemplate(ChunkNum: longint;
    (* dlya zapolneniya schablona *)
    Data: TPointsSet; ProjectName: string; FileName: string): string;
var
    TmplBegin, TmplEnd, TmplLen: longint;
    Template, DataStr: string;
    X, Y: double;
    i:    longint;
    Pair: array[1..1] of TStringPair;
    UserFileName, FileDescription: string;
begin
    Result     := PrepareTemplate_data;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Result     := ReplaceStrings(Result, Pair, 1);

    Pair[1][1] := 'GraphURL';
    Pair[1][2] := GetGraphURL;
    Result     := ReplaceStrings(Result, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(ChunkNum);
    Result     := ReplaceStrings(Result, Pair, 1);

    GetFileProperties(ProjectName, FileName, UserFileName, FileDescription);
    //  !!! d. vyzyvat'sya do zameny FileName !!!
    Pair[1][1] := 'UserFileName';
    Pair[1][2] := UserFileName;
    Result     := ReplaceStrings(Result, Pair, 1);

    Pair[1][1] := 'FileDescription';
    Pair[1][2] := FileDescription;
    Result     := ReplaceStrings(Result, Pair, 1);

    Pair[1][1] := 'ProjectName';
    Pair[1][2] := ProjectName;
    Result     := ReplaceStrings(Result, Pair, 1);

    Pair[1][1] := 'FileName';
    Pair[1][2] := FileName;
    Result     := ReplaceStrings(Result, Pair, 1);

    TmplBegin := Pos(BeginMarker, Result);
    TmplEnd   := Pos(EndMarker, Result);

    if (TmplBegin > 0) and (TmplEnd > 0) then
    begin
        TmplLen  := TmplEnd - TmplBegin - Length(BeginMarker);
        Template := Copy(Result, TmplBegin + Length(BeginMarker), TmplLen);
        Delete(Result, TmplBegin, TmplLen + Length(BeginMarker) + Length(EndMarker));

        //  dopuskaetsya ravenstvo nil
        if Assigned(Data) then
        begin
            Data.Sort;
            for i := 0 to Data.PointsCount - 1 do
            begin
                X := Data.PointXCoord[i];
                Y := Data.PointYCoord[i];
                Pair[1][1] := 'XValue';
                Pair[1][2] := ValToStr(X);
                DataStr := ReplaceStrings(Template, Pair, 1);
                Pair[1][1] := 'YValue';
                Pair[1][2] := ValToStr(Y);
                DataStr := ReplaceStrings(DataStr, Pair, 1);

                Insert(DataStr, Result, TmplBegin);
                TmplBegin := TmplBegin + Length(DataStr);
            end;
        end;
    end;
end;

function TCGIDatamodule2.InsertNamesByTemplate(TemplateIn: string;
    Names: TStringList; Files: TStringList): string;
var
    TmplBegin, TmplEnd, TmplLen: longint;
    Template, DataStr: string;
    i:    longint;
    Pair: array[1..1] of TStringPair;

const
    BeginProjectTemplate: string = 'BeginNamesTemplate';
    EndProjectTemplate: string   = 'EndNamesTemplate';
begin
    Result := TemplateIn;

    TmplBegin := Pos(BeginProjectTemplate, Result);
    TmplEnd   := Pos(EndProjectTemplate, Result);

    if (TmplBegin > 0) and (TmplEnd > 0) then
    begin
        TmplLen  := TmplEnd - TmplBegin - Length(BeginProjectTemplate);
        Template := Copy(Result, TmplBegin +
            Length(BeginProjectTemplate), TmplLen);
        Delete(Result, TmplBegin,
            TmplLen + Length(BeginProjectTemplate) + Length(EndProjectTemplate));

        Assert(Assigned(Names));
        Assert(Assigned(Files));
        Assert(Names.Count = Files.Count);
        for i := 0 to Names.Count - 1 do
        begin
            Pair[1][1] := 'NameFromNames';
            Pair[1][2] := Names.Strings[i];
            DataStr    := ReplaceStrings(Template, Pair, 1);

            Pair[1][1] := 'NameFromFileNames';
            Pair[1][2] := Files.Strings[i];
            DataStr    := ReplaceStrings(DataStr, Pair, 1);

            Insert(DataStr, Result, TmplBegin);
            TmplBegin := TmplBegin + Length(DataStr);
        end;
    end;
end;

function TCGIDatamodule2.InsertDataWithTagByBounds(Page: string;
    Data: TPointsSet; SpecBounds: TPointsSet): string;
var
    TmplBegin, TmplEnd, TmplLen: longint;
    Template, DataStr: string;
    X, Y:  double;
    i, j:  longint;
    Pair:  array[1..1] of TStringPair;
    Found: boolean;
begin
    TmplBegin := Pos(BeginMarker, Page);
    TmplEnd   := Pos(EndMarker, Page);

    if (TmplBegin > 0) and (TmplEnd > 0) then
    begin
        TmplLen  := TmplEnd - TmplBegin - Length(BeginMarker);
        Template := Copy(Page, TmplBegin + Length(BeginMarker), TmplLen);
        Delete(Page, TmplBegin, TmplLen + Length(BeginMarker) + Length(EndMarker));

        Result := Page;
        //  dopuskaetsya ravenstvo nil
        if Assigned(Data) then
        begin
            Data.Sort;
            for i := 0 to Data.PointsCount - 1 do
            begin
                X := Data.PointXCoord[i];
                Y := Data.PointYCoord[i];
                Pair[1][1] := 'XValue';
                Pair[1][2] := ValToStr(X);
                DataStr := ReplaceStrings(Template, Pair, 1);
                Pair[1][1] := 'YValue';
                Pair[1][2] := ValToStr(Y);
                DataStr := ReplaceStrings(DataStr, Pair, 1);
                Pair[1][1] := 'Selected';

                Found := False;
                //  dopuskaetsya ravenstvo nil
                if Assigned(SpecBounds) then
                begin
                    SpecBounds.Sort; //  dlya pravil'noy ustanovki Left, Right
                    for j := 0 to SpecBounds.PointsCount - 1 do
                        if (Abs(X - SpecBounds.PointXCoord[j]) <= TINY) and
                            (Abs(Y - SpecBounds.PointYCoord[j]) <= TINY) then
                        begin
                            Found := True;
                            Break;
                        end;
                end;

                if Found then
                begin
                    //  tochki 1, 3, 5... - pravye granitsy
                    if Odd(j) then
                        Pair[1][2] := Right
                    else
                        Pair[1][2] := Left;
                end
                //  chtoby ne zagromozhdat' tablitsu slovo No ne vyvoditsya
                else
                    Pair[1][2] := '';  //  No;
                DataStr := ReplaceStrings(DataStr, Pair, 1);

                Insert(DataStr, Result, TmplBegin);
                TmplBegin := TmplBegin + Length(DataStr);
            end;
        end;
    end
    else
        Result := Page;
end;

function TCGIDatamodule2.InsertDataWithTagByTemplate(Page: string;
    Data: TPointsSet; Selected: TPointsSet): string;
var
    TmplBegin, TmplEnd, TmplLen: longint;
    Template, DataStr: string;
    X, Y:  double;
    i, j:  longint;
    Pair:  array[1..1] of TStringPair;
    Found: boolean;
begin
    TmplBegin := Pos(BeginMarker, Page);
    TmplEnd   := Pos(EndMarker, Page);

    if (TmplBegin > 0) and (TmplEnd > 0) then
    begin
        TmplLen  := TmplEnd - TmplBegin - Length(BeginMarker);
        Template := Copy(Page, TmplBegin + Length(BeginMarker), TmplLen);
        Delete(Page, TmplBegin, TmplLen + Length(BeginMarker) + Length(EndMarker));

        Result := Page;
        //  dopuskaetsya ravenstvo nil
        if Assigned(Data) then
        begin
            Data.Sort;
            for i := 0 to Data.PointsCount - 1 do
            begin
                X := Data.PointXCoord[i];
                Y := Data.PointYCoord[i];
                Pair[1][1] := 'XValue';
                Pair[1][2] := ValToStr(X);
                DataStr := ReplaceStrings(Template, Pair, 1);
                Pair[1][1] := 'YValue';
                Pair[1][2] := ValToStr(Y);
                DataStr := ReplaceStrings(DataStr, Pair, 1);
                Pair[1][1] := 'Selected';

                Found := False;
                //  dopuskaetsya ravenstvo nil
                if Assigned(Selected) then
                    for j := 0 to Selected.PointsCount - 1 do
                        if (Abs(X - Selected.PointXCoord[j]) <= TINY) and
                            (Abs(Y - Selected.PointYCoord[j]) <= TINY) then
                        begin
                            Found := True;
                            Break;
                        end;

                if Found then
                    Pair[1][2] := '+' // Yes
                //  chtoby ne zagromozhdat' tablitsu slovo No ne vyvoditsya
                else
                    Pair[1][2] := '';  //  No;
                DataStr := ReplaceStrings(DataStr, Pair, 1);

                Insert(DataStr, Result, TmplBegin);
                TmplBegin := TmplBegin + Length(DataStr);
            end;
        end;
    end
    else
        Result := Page;
end;

const
    //  markery schablona stroki znacheniy parametrov ekzemplyara patterna
    BeginCurveParameters: string = 'BeginCurveParameters';
    EndCurveParameters: string = 'EndCurveParameters';
    //  markery schablona znacheniya parametra ekzemplyara patterna
    BeginValueItem: string = 'BeginValueItem';
    EndValueItem: string   = 'EndValueItem';
    //  marker znacheniya parametra ekzemplyara patterna
    ItemValue: string      = 'ItemValue';
    //  markery schablona zagolovka parametra ekzemplyara patterna
    BeginHeaderItem: string = 'BeginHeaderItem';
    EndHeaderItem: string  = 'EndHeaderItem';
    //  marker zagolovka parametra ekzemplyara patterna
    HeaderItemName: string = 'HeaderItemName';
    //BeginDividerItem: string = 'BeginDividerItem';
    //EndDividerItem: string = 'EndDividerItem';
    //  markery schablona dlya polya vvoda obychnogo parametra
    BeginInputItem: string = 'BeginInputItem';
    EndInputItem: string   = 'EndInputItem';
    //  markery schablona dlya polya vvoda vychislyaemogo parametra
    BeginInputCalculatedItem: string = 'BeginInputCalculatedItem';
    EndInputCalculatedItem: string = 'EndInputCalculatedItem';

    ItemName: string = 'ItemName';

function TCGIDatamodule2.ExtractTemplate(var From: string; MarkerBegin: string;
    MarkerEnd: string; var Position: longint): string;
var
    TmplEnd, TmplLen: longint;
begin
    Result   := '';
    Position := Pos(MarkerBegin, From);
    TmplEnd  := Pos(MarkerEnd, From);
    if (Position > 0) and (TmplEnd > 0) then
    begin
        TmplLen := TmplEnd - Position - Length(MarkerBegin);
        Result  := Copy(From, Position + Length(MarkerBegin), TmplLen);
        Delete(From, Position, TmplLen + Length(MarkerBegin) + Length(MarkerEnd));
    end;
end;

function TCGIDatamodule2.NameToGreekSymbol(Name_: string): string;
begin
    if UpperCase(Name_) = 'ALPHA' then
    begin
        if Name_[1] = 'A' then
            Result := '&#913;'
        else
            Result := '&#945;';
    end
    else
    if UpperCase(Name_) = 'BETA' then
    begin
        if Name_[1] = 'B' then
            Result := '&#914;'
        else
            Result := '&#946;';
    end
    else
    if UpperCase(Name_) = 'GAMMA' then
    begin
        if Name_[1] = 'G' then
            Result := '&#915;'
        else
            Result := '&#947;';
    end
    else
    if UpperCase(Name_) = 'DELTA' then
    begin
        if Name_[1] = 'D' then
            Result := '&#916;'
        else
            Result := '&#948;';
    end
    else
    if UpperCase(Name_) = 'EPSILON' then
    begin
        if Name_[1] = 'E' then
            Result := '&#917;'
        else
            Result := '&#949;';
    end
    else
    if UpperCase(Name_) = 'ZETA' then
    begin
        if Name_[1] = 'Z' then
            Result := '&#918;'
        else
            Result := '&#950;';
    end
    else
    if UpperCase(Name_) = 'ETA' then
    begin
        if Name_[1] = 'E' then
            Result := '&#919;'
        else
            Result := '&#951;';
    end
    else
    if UpperCase(Name_) = 'THETA' then
    begin
        if Name_[1] = 'T' then
            Result := '&#920;'
        else
            Result := '&#952;';
    end
    else
    if UpperCase(Name_) = 'IOTA' then
    begin
        if Name_[1] = 'I' then
            Result := '&#921;'
        else
            Result := '&#953;';
    end
    else
    if UpperCase(Name_) = 'KAPPA' then
    begin
        if Name_[1] = 'K' then
            Result := '&#922;'
        else
            Result := '&#954;';
    end
    else
    if UpperCase(Name_) = 'LAMBDA' then
    begin
        if Name_[1] = 'L' then
            Result := '&#923;'
        else
            Result := '&#955;';
    end
    else
    if UpperCase(Name_) = 'MU' then
    begin
        if Name_[1] = 'M' then
            Result := '&#924;'
        else
            Result := '&#956;';
    end
    else
    if UpperCase(Name_) = 'NU' then
    begin
        if Name_[1] = 'N' then
            Result := '&#925;'
        else
            Result := '&#957;';
    end
    else
    if UpperCase(Name_) = 'XI' then
    begin
        if Name_[1] = 'X' then
            Result := '&#926;'
        else
            Result := '&#958;';
    end
    else
    if UpperCase(Name_) = 'OMICRON' then
    begin
        if Name_[1] = 'O' then
            Result := '&#927;'
        else
            Result := '&#959;';
    end
    else
    if UpperCase(Name_) = 'PI' then
    begin
        if Name_[1] = 'P' then
            Result := '&#928;'
        else
            Result := '&#960;';
    end
    else
    if UpperCase(Name_) = 'RHO' then
    begin
        if Name_[1] = 'R' then
            Result := '&#929;'
        else
            Result := '&#961;';
    end
    else
    if UpperCase(Name_) = 'SIGMA' then
    begin
        if Name_[1] = 'S' then
            Result := '&#931;'
        else
            Result := '&#963;';
    end
    else
    if UpperCase(Name_) = 'TAU' then
    begin
        if Name_[1] = 'T' then
            Result := '&#932;'
        else
            Result := '&#964;';
    end
    else
    if UpperCase(Name_) = 'UPSILON' then
    begin
        if Name_[1] = 'U' then
            Result := '&#933;'
        else
            Result := '&#965;';
    end
    else
    if UpperCase(Name_) = 'PHI' then
    begin
        if Name_[1] = 'P' then
            Result := '&#934;'
        else
            Result := '&#966;';
    end
    else
    if UpperCase(Name_) = 'CHI' then
    begin
        if Name_[1] = 'C' then
            Result := '&#935;'
        else
            Result := '&#967;';
    end
    else
    if UpperCase(Name_) = 'PSI' then
    begin
        if Name_[1] = 'P' then
            Result := '&#936;'
        else
            Result := '&#968;';
    end
    else
    if UpperCase(Name_) = 'OMEGA' then
    begin
        if Name_[1] = 'O' then
            Result := '&#937;'
        else
            Result := '&#969;';
    end
    else
        Result := Name_;
end;

function TCGIDatamodule2.FillTemplateBySpecParameters(Page: string): string;
var
    Pair: array[1..1] of TStringPair;
    SpecParams: TMSCRCurveList;
    i, j, ParamCount: longint;
    CP:   Curve_parameters;
    P:    TSpecialCurveParameter;
    FillHeaders: boolean;
    TmplHeaderBegin,    //  polozhenie schablona zagolovka parametra
    //TmplDividerBegin,
    TmplValueBegin,     //  polozhenie schablona znacheniya parametra
    TmplInputBegin,     //  polozhenie poley vvoda znacheniy parametrov
    TmplCalculatedInputBegin, TmplCurveBegin,
    //  polozhenie strok znacheniy parametrov
    TmplEnd, TmplLen, TmplBeginTemp: longint;
    TmplHeader,
    //TmplDivider,
    TmplInput,          //  schablon polya vvoda znacheniya obychnogo parametra
    TmplCalculatedInput,//  schablon vychislyaemogo parametra
    TmplValue,          //  schablon znacheniya parametra
    TmplCurve,       //  schablon stroki znacheniy parametrov
    Data, TmplTemp: string;
begin
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Page := ReplaceStrings(Page, Pair, 1);

    Pair[1][1] := 'GraphURL';
    Pair[1][2] := GetGraphURL;
    Page := ReplaceStrings(Page, Pair, 1);

    SpecParams := Proxy.GetCurveList;
    try
        FillHeaders := True;
        //  !!! shablony dolzhny udalyat'sya iz teksta nezavisimo ot nalichiya
        //  ekzemplyarov patterna !!!

        //  izvlechenie schablona zagolovka parametra
        TmplHeader := ExtractTemplate(Page, BeginHeaderItem,
            EndHeaderItem, TmplHeaderBegin);

        //  snachala izvlekaetsya schablon stroki parametrov
        TmplCurve := ExtractTemplate(Page, BeginCurveParameters,
            EndCurveParameters, TmplCurveBegin);

        //  !!! schablon znacheniya parametra vybiraetsya iz schablona stroki !!!
        TmplValue := ExtractTemplate(TmplCurve, BeginValueItem,
            EndValueItem, TmplValueBegin);
        (*
        TmplDivider := ExtractTemplate(Page,
            BeginDividerItem, EndDividerItem, TmplDividerBegin);
        *)
        //  !!! schablon polya vvoda obychnogo parametra dolzhen
        //  stoyat' ran'sche schablona vvoda vychislyaemogo parametra !!!
        TmplInput := ExtractTemplate(Page, BeginInputItem, EndInputItem,
            TmplInputBegin);

        TmplCalculatedInput :=
            ExtractTemplate(Page, BeginInputCalculatedItem,
            EndInputCalculatedItem, TmplCalculatedInputBegin);

        ParamCount := 0;
        for i := 0 to SpecParams.Count - 1 do
        begin
            CP := Curve_parameters(SpecParams.Items[i]);
            //  vyvod zagolovkov; !!! zagolovki vyvoldyatsya po imenam
            //  parametrov pervogo v spiske ekzemplyara patterna !!!
            if FillHeaders then
            begin
                if Length(TmplCurve) <> 0 then
                begin
                    TmplTemp      := '';
                    TmplBeginTemp := TmplHeaderBegin;
                    if Length(TmplHeader) <> 0 then
                    begin
                        //  vstavlyaetsya zagolovok nomera ekzemplyara
                        Pair[1][1] := HeaderItemName;
                        Pair[1][2] := HeaderCurveNumber;
                        Data := ReplaceStrings(TmplHeader, Pair, 1);

                        Insert(Data, TmplTemp, TmplBeginTemp);
                        TmplBeginTemp := TmplBeginTemp + Length(Data);
                        //  vyvod razdelitelya
                        (*
                        Insert(TmplDivider, Page, TmplDividerBegin);
                        TmplDividerBegin := TmplDividerBegin + Length(TmplDivider);
                        TmplInputBegin := TmplInputBegin + Length(TmplDivider);
                        *)
                        //  vyvod poley vvoda; polya vvoda d
                        Pair[1][1] := ItemName;
                        Pair[1][2] := HeaderCurveNumber;
                        Data := ReplaceStrings(TmplInput, Pair, 1);
                        Insert(Data, Page, TmplInputBegin);
                        TmplInputBegin := TmplInputBegin + Length(Data);

                        for j := 0 to CP.Params.Count - 1 do
                        begin
                            P := TSpecialCurveParameter(CP.Params.Items[j]);
                            if P.Type_ <> Argument then
                            begin
                                Pair[1][1] := HeaderItemName;
                                Pair[1][2] := NameToGreekSymbol(P.Name);
                                Data := ReplaceStrings(TmplHeader, Pair, 1);

                                Insert(Data, TmplTemp, TmplBeginTemp);
                                TmplBeginTemp := TmplBeginTemp + Length(Data);
                                //  vyvod razdelitelya
                                (*
                                Insert(TmplDivider, Page, TmplDividerBegin);
                                TmplDividerBegin := TmplDividerBegin + Length(TmplDivider);
                                TmplInputBegin := TmplInputBegin + Length(TmplDivider);
                                *)
                                if (P.Type_ <> Calculated) and
                                    (P.Type_ <> InvariablePosition) and
                                    (P.Type_ <> VariablePosition) then
                                begin
                                    //  vyvod poley vvoda
                                    Pair[1][1] := ItemName;
                                    Pair[1][2] := P.Name;
                                    Data := ReplaceStrings(TmplInput, Pair, 1);
                                    Insert(Data, Page, TmplInputBegin);
                                    TmplInputBegin := TmplInputBegin + Length(Data);
                                end
                                else
                                begin
                                    //Pair[1][1] := ItemName;
                                    //Pair[1][2] := P.Name;
                                    //Data := ReplaceStrings(TmplCalculatedInput, Pair, 1);
                                    Data := TmplCalculatedInput;
                                    Insert(Data, Page, TmplInputBegin);
                                    TmplInputBegin := TmplInputBegin + Length(Data);
                                end;
                            end;
                        end;
                    end;
                    Insert(TmplTemp, Page, TmplHeaderBegin);
                    TmplCurveBegin := TmplCurveBegin + Length(TmplTemp);
                    ParamCount     := CP.Params.Count + 1;  //  dobavlyaetsya pole nomera
                end;
                FillHeaders := False;
            end;
            //  vyvod znacheniy parametrov
            if Length(TmplCurve) <> 0 then
            begin
                TmplTemp      := TmplCurve;
                TmplBeginTemp := TmplValueBegin;
                if Length(TmplValue) <> 0 then
                begin
                    //  vyvod nomera ekzemplyara
                    Pair[1][1] := ItemValue;
                    Pair[1][2] := IntToStr(i + 1);
                    Data := ReplaceStrings(TmplValue, Pair, 1);

                    Insert(Data, TmplTemp, TmplBeginTemp);
                    TmplBeginTemp := TmplBeginTemp + Length(Data);

                    for j := 0 to CP.Params.Count - 1 do
                    begin
                        P := TSpecialCurveParameter(CP.Params.Items[j]);
                        if P.Type_ <> Argument then
                        begin
                            Pair[1][1] := ItemValue;
                            Pair[1][2] := ValToStr(P.Value);
                            Data := ReplaceStrings(TmplValue, Pair, 1);

                            Insert(Data, TmplTemp, TmplBeginTemp);
                            TmplBeginTemp := TmplBeginTemp + Length(Data);
                        end;
                    end;
                end;
                Insert(TmplTemp, Page, TmplCurveBegin);
                TmplCurveBegin := TmplCurveBegin + Length(TmplTemp);
            end;
        end;
        //  vyvod chisla parametrov
        Pair[1][1] := 'ParameterCount';
        Pair[1][2] := IntToStr(ParamCount);
        Page := ReplaceStrings(Page, Pair, 1);
    finally
        SpecParams.Free;
    end;
    Result := Page;
end;

procedure TCGIDatamodule2.GoToCurveParameters;
var
    Page: string;
    Pair: array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  chtenie schablona i zapolnenie ego parametrov
    Page := PrepareTemplate_specimen_parameters;

    Pair[1][1] := 'SqrRFactorValue';
    Pair[1][2] := Proxy.GetSqrRFactorStr;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'AbsRFactorValue';
    Pair[1][2] := Proxy.GetAbsRFactorStr;
    Page := ReplaceStrings(Page, Pair, 1);

    Page := FillTemplateBySpecParameters(Page);
    Page := InsertKey(Page, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Page);
end;

procedure TCGIDatamodule2.SaveSpecParameters;
var
    ProjectName, FileName: string;
    Page:   string;
    ResultName, ResultDescription: string;
    F:      TextFile;
    Pair:   array[1..1] of TStringPair;
    R:      TSearchRec;
    FileIndex, NewFileIndex: longint;
    SPName: string;
    DotPos: longint;
    Stream: TMemoryStream;
    GraphFile: TFileStream;
    GraphFileName: string;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;

    ResultName := Application.RequestVariables['result_name'];
    ResultDescription := Application.RequestVariables['result_description'];

    GetDataNames(Proxy.GetProblemId, UserName, ProjectName, FileName);
    //  poisk poslednego rezul'tata, sootvetstvuyuschego
    //  dannomu imeni faila
    FileName     := ChangeFileExt(FileName, '');
    NewFileIndex := 0;
    if FindFirst(DataDir + UserName + Slash + ProjectName + Slash +
        FileName + '.*.sp', faArchive + faAnyFile, R) = 0 then
        try
            repeat
                try
                    SPName := ExtractFileName(R.Name);
                    SPName := ChangeFileExt(SPName, '');
                    //  dopuskaetsya nalichie failov, imenovannyh ne chislami
                    DotPos := Pos('.', SPName);
                    if DotPos <> 0 then
                    begin
                        Delete(SPName, 1, DotPos);
                        FileIndex := StrToInt(SPName);
                        if FileIndex > NewFileIndex then
                            NewFileIndex := FileIndex;
                    end;
                except
                end;
            until FindNext(R) <> 0;
        finally
            SysUtils.FindClose(R);
        end;
    //  sozdanie imeni novogo faila
    Inc(NewFileIndex);

    //  sohranenie grafika dlya resul'tata
    //??? schirina i vysota dolzhny izvlekat'sya iz stranitsy,
    //  no dolzhny byt' predusmotreny znacheniya po-umolchaniyu
    Stream    := nil;
    GraphFile := nil;
    Stream    := Proxy.GetGraph(600, 450);
    GraphFileName := FileName + '.' + IntToStr(NewFileIndex) + '.png';
    GraphFile := TFileStream.Create(DataDir + UserName + Slash +
        ProjectName + Slash + GraphFileName, fmCreate);
    try
        GraphFile.CopyFrom(Stream, 0(* !!! kopirovat' vse !!! *));
    finally
        Stream.Free;
        GraphFile.Free;
    end;

    if Trim(ResultName) = '' then
        ResultName := 'Curve parameters set number ' +
            IntToStr(NewFileIndex)//  sozdanie imeni rezul'tata "po-umolchaniyu"
    ;
    Page := PrepareTemplate_specimen_parameters_file;
    Page := FillTemplateBySpecParameters(Page);

    Pair[1][1] := 'HintResultName';
    Pair[1][2] := ResultName;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'HintResultDescription';
    Pair[1][2] := ResultDescription;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'ProjectName';
    Pair[1][2] := ProjectName;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'GraphFileName';
    Pair[1][2] := GraphFileName;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'SqrRFactorValue';
    Pair[1][2] := Proxy.GetSqrRFactorStr;
    Page := ReplaceStrings(Page, Pair, 1);
    Pair[1][1] := 'AbsRFactorValue';
    Pair[1][2] := Proxy.GetAbsRFactorStr;
    Page := ReplaceStrings(Page, Pair, 1);

    SPName := DataDir + UserName + Slash + ProjectName + Slash +
        FileName + '.' + IntToStr(NewFileIndex) + '.sp';
    AssignFile(F, SPName);
    try
        ReWrite(F);
        WriteLn(F, Page);
    finally
        CloseFile(F);
    end;
    GoToCurveParameters;
end;

procedure TCGIDatamodule2.UpdateCurveParameters;
var
    SpecIndex: longint;
    i, j, ParamCount: longint;
    Name_, Value_: string;
    Value: double;
    Type_: longint;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;

    //  izvlechenie nomera ekzemplyara patterna
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        SpecIndex :=
            StrToInt(Application.RequestVariables[HeaderCurveNumber]) - 1;
    except
        raise Exception.Create(InvalidSpecIndex);
    end;
    //  poluchenie parametrov ekzemplyara patterna po zadannomu indeksu
    ParamCount := Proxy.GetCurveParameterCount(SpecIndex);
    //  perebor parametrov ekzemplyara i proverka nalichiya imen sredi
    //  peredannyh parametrov
    for i := 0 to ParamCount - 1 do
    begin
        Proxy.GetCurveParameter(SpecIndex, i, Name_, Value, Type_);
        if (TParameterType(Type_) <> Argument) and
            (TParameterType(Type_) <> Calculated) and
            (TParameterType(Type_) <> InvariablePosition) and
            (TParameterType(Type_) <> VariablePosition) then
        begin
            //  izvlechenie parametra i preobrazovanie ego k tipu double
            //  s proverkoy i vozbuzhdeniem isklucheniya
            Value_ := Application.RequestVariables[Name_];
            if Trim(Value_) <> '' then
            begin
                //  eto pozvolyaet ne zadavat' znacheniya parametrov,
                //  kotorye izmenyat' ne nuzhno
                try
                    Value := MyStrToFloat(Value_);
                except
                    raise Exception.Create(InvalidParValue);
                end;
                Proxy.SetCurveParameter(SpecIndex, i, Value);
            end;
        end;
    end;
    GoToCurveParameters;
end;

//  perekluchaet priznak vybora tochki privyazki
procedure TCGIDatamodule2.SelectCurvePosition;
var
    Argument, Value: double;
    Data:  TPointsSet;
    Found: boolean;
    i:     longint;
    CurChunkNum: longint;
begin
    try
        Argument := MyStrToFloat(Application.RequestVariables['argument']);
    except
        raise Exception.Create(InvalidArgument);
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //  !!! poisk sredi vseh tochek dannyh !!!
    Data := Proxy.GetProfilePointsSet;
    try
        //  tochki privyazki mogut byt' vybrany tol'ko sredi tochek dannyh
        //  poisk poluchennoy tochki sredi tochek dannyh
        Found := False;
        for i := 0 to Data.PointsCount - 1 do
            if Abs(Argument - Data.PointXCoord[i]) <= TINY then
            begin
                Value := Data.PointYCoord[i];
                Found := True;
                Break;  //  tochka naidena...
            end;
        if not Found then
            raise Exception.Create(PointMustBeSelectedFromData);
    finally
        Data.Free;
    end;
    Proxy.AddPointToCurvePositions(Argument, Value);
    GoToCurvePositions;
end;

//  perekluchaet priznak vybora tochki privyazki
procedure TCGIDatamodule2.SelectBoundPoint;
var
    Argument, Value: double;
    Data:  TPointsSet;
    Found: boolean;
    CurChunkNum, i: longint;
begin
    try
        Argument := MyStrToFloat(Application.RequestVariables['argument']);
    except
        raise Exception.Create(InvalidArgument);
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa
    //  !!! poisk sredi vseh tochek dannyh !!!
    Data := Proxy.GetProfilePointsSet;
    try
        //  tochki privyazki mogut byt' vybrany tol'ko sredi tochek dannyh
        //  poisk poluchennoy tochki sredi tochek dannyh
        Found := False;
        for i := 0 to Data.PointsCount - 1 do
            if Abs(Argument - Data.PointXCoord[i]) <= TINY then
            begin
                Value := Data.PointYCoord[i];
                Found := True;
                Break;  //  tochka naidena...
            end;
        if not Found then
            raise Exception.Create(PointMustBeSelectedFromData);
    finally
        Data.Free;
    end;
    Proxy.AddPointToRFactorBounds(Argument, Value);
    GoToCurveBounds;
end;

procedure TCGIDatamodule2.GenerateCurvePositions;
var
    CurChunkNum: longint;
    BackF:    string;//???сделать извлечение и установку
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa (poluchenie dannyh fona)
    Proxy.ComputeCurvePositions;
    //GoToCurvePositions;
    //  protsess asinhronnyi, poetomu perehodim k oknu progressa;
    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_gen_spec_pos_progress;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(CurChunkNum);
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

procedure TCGIDatamodule2.ComputeCurveBounds;
var
    CurChunkNum: longint;
    BackF:    string;//???сделать извлечение и установку
    Template: string;
    Pair:     array[1..1] of TStringPair;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    if Trim(Key) = '' then
    begin
        //  kluch ne byl peredan
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
    begin
        GoToStartPage(RepeatInput, TimeOut);
        Exit;
    end;
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    try
        //  ne ubirat' - snachala proveryayutsya parametry
        CurChunkNum := StrToInt(Application.RequestVariables['cur_chunk_num']);
    except
        raise Exception.Create(ChunkNumberIsAbsent);
    end;
    //  vypolnenie zaprosa (poluchenie dannyh fona)
    Proxy.ComputeCurveBounds;
    //GoToCurveBounds;
    //  protsess asinhronnyi, poetomu perehodim k oknu progressa;
    //  chtenie schablona i zapolnenie ego parametrov
    Template   := PrepareTemplate_gen_spec_int_progress;
    Pair[1][1] := 'ProblemID';
    Pair[1][2] := IntToStr(Proxy.GetProblemId);
    Template   := ReplaceStrings(Template, Pair, 1);

    Pair[1][1] := 'CurChunkNum';
    Pair[1][2] := IntToStr(CurChunkNum);
    Template   := ReplaceStrings(Template, Pair, 1);
    Template   := InsertKey(Template, Key);
    //  vyvod stranitsy
    CGIDatamodule2.AddResponseLn(Template);
end;

function TCGIDatamodule2.GetGraphURL: string;
begin
    Assert(Proxy.GetProblemId <> 0);
    Result := 'https://' + ExternalIP +
        '/cgi-bin/fit.cgi?command=get_graph&problem_id=' +
        IntToStr(Proxy.GetProblemId);
end;

procedure TCGIDatamodule2.GetGraph;
var
    Stream: TMemoryStream;
    Width, Height: longint;
begin
    try
        //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
        Proxy.SetProblemId(StrToInt(Application.RequestVariables['problem_id']));
    except
        raise Exception.Create(ProblemIDIsAbsent);
    end;
    //  schirina i vysota izvlekayutsya iz stranitsy;
    //  krome togo predusmotreny znacheniya po-umolchaniyu
    try
        Width := StrToInt(Application.RequestVariables['width']);
    except
        Width := 600;
    end;
    try
        Height := StrToInt(Application.RequestVariables['height']);
    except
        Height := 450;
    end;
    Stream := Proxy.GetGraph(Width, Height);
    try
        CGIDatamodule2.Response.CopyFrom(Stream, 0(* !!! kopirovat' vse !!! *));
    finally
        Stream.Free;
    end;
end;

procedure TCGIDatamodule2.GetSavedGraph;
var
    Stream: TFileStream;
    ProjectName, GraphFileName: string;
begin
    //  global'naya peremennaya - ispol'zuetsya dlya vypolneniya zaprosa
    Key := Application.RequestVariables['key'];
    //  !!! zdes' vyvodit' stranitsy oschibok nel'zya,
    //  poskol'ku tip kontenta ne html !!!
    if Trim(Key) = '' then
        raise Exception.Create(InvalidKeyValue);

    //  proverka dopustimosti imeni pol'zovatelya i obnovlenie klyucha
    if not GetUserName then
        raise Exception.Create(InadmissibleUserName);

    GraphFileName := Application.RequestVariables['graph_file_name'];
    //  !!! zdes' vyvodit' stranitsy oschibok nel'zya,
    //  poskol'ku tip kontenta ne html !!!
    if Trim(GraphFileName) = '' then
        raise Exception.Create(NoFileName);

    ProjectName := Application.RequestVariables['project_name'];
    //  !!! zdes' vyvodit' stranitsy oschibok nel'zya,
    //  poskol'ku tip kontenta ne html !!!
    if Trim(ProjectName) = '' then
        raise Exception.Create(NoFileName);

    Stream := TFileStream.Create(DataDir + UserName + Slash +
        ProjectName + Slash + GraphFileName, fmOpenRead);
    try
        CGIDatamodule2.Response.CopyFrom(Stream, 0(* !!! kopirovat' vse !!! *));
    finally
        Stream.Free;
    end;
end;

function TCGIDatamodule2.GetChunkNumbers(CurChunkNum: longint;
    ChunkCount: longint): TStringList;
var
    LeftChunkNum: longint;      //  nomer kuska nachinayuschego seriyu s schagom 1
    ChunkRoundNum: longint;
    StepChunkNum: longint;      //  progressiruyuschiy schag numeratsii kuskov
    ChunkMarker: longint;
    RightChunkNum: longint;     //  nomer kuska, poslednego v serii s schagom 1
    i: longint;
begin
    Result := TStringList.Create;
    Result.Sorted := False;

    LeftChunkNum  := (CurChunkNum div 10) * 10;
    RightChunkNum := LeftChunkNum + 10;
    if LeftChunkNum = 0 then
        LeftChunkNum := 1;
    Result.Add(IntToStr(LeftChunkNum));
    //  dvizhenie v storonu men'schih nomerov
    StepChunkNum := 10;
    while LeftChunkNum >= StepChunkNum do
    begin
        //  okruglenie do tochnosti NextStepChunkNum
        ChunkRoundNum := (LeftChunkNum div StepChunkNum) * StepChunkNum;
        ChunkMarker   := ChunkRoundNum - StepChunkNum;
        if ChunkMarker = 0 then
            ChunkMarker := 1;
        //  vstavlyaem v nachalo...
        Result.Insert(0, IntToStr(ChunkMarker));
        StepChunkNum := StepChunkNum * 10;
    end;
    //  dvizhenie v storonu bol'schih nomerov
    for i := LeftChunkNum + 1 to RightChunkNum do
    begin
        if i > ChunkCount then
            Exit;
        Result.Add(IntToStr(i));
    end;
    StepChunkNum := 10;
    repeat
        ChunkRoundNum := (RightChunkNum div StepChunkNum) * StepChunkNum;
        if ChunkRoundNum = 0 then
            Break;
        ChunkMarker := ChunkRoundNum + StepChunkNum;
        if ChunkMarker > ChunkCount then
            Break;
        Result.Add(IntToStr(ChunkMarker));
        StepChunkNum := StepChunkNum * 10;
    until False;
end;

procedure CopyDir(SrcDir: string; DstDir: string);
var
    F:     TSearchRec;
    FName: string;
begin
    if FindFirst(SrcDir + Slash + '*', faAnyFile, F) = 0 then
    begin
        repeat
            FName := ExtractFileName(F.Name);
            if (FName <> '.') and (FName <> '..') then
                if (F.Attr and faDirectory) <> 0 then
                begin
                    //  rekursivnyi vyzov dlya direktorii
                    ForceDirectories(DstDir + Slash + FName);
                    CopyDir(SrcDir + Slash + FName,
                        DstDir + Slash + FName);
                end
                else
                    CopyFile(SrcDir + Slash + FName,
                        DstDir + Slash + FName)//  obrabotka fayla
            ;
        until FindNext(F) <> 0;
        FindClose(F);
    end;
end;

procedure CopyFile(SrcFile: string; DstFile: string);
var
    Src, Dst: TFileStream;
begin
    Src := TFileStream.Create(SrcFile, fmOpenRead);
    Dst := TFileStream.Create(DstFile, fmCreate);
    Dst.CopyFrom(Src, 0);
    Dst.Free;
    Src.Free;
end;

initialization
    //{$I Unit2.lrs}
end.
