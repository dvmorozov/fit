program Fit;

{$DEFINE UseCThreads}

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces,   //  LCL widgetset
    Forms,
    Variants,
    app,
    form_main,
    about_box_dialog,
    input_wavelength_dialog,
    input_back_factor_dialog,
    input_max_rfactor_dialog,
    fit_client,
    fit_client_stub,
    fit_client_app,
    app_settings,
    TurboPowerIPro,
    SelfCheckedComponentList,
    StrUtils,
    common_types,
    table_components,
    self_copied_component,
    ClientCallback,
    main_calc_thread,
    GeneralHashFunctions,
    create_user_points_set_dlg,
    user_points_set_prop_dialog,
    configurable_points_set,
    configurable_user_points_set,
    user_points_set,
    int_fit_service,
    fit_server,
    fit_task,
    int_fit_server,
    fit_server_with_thread,
    fit_task_with_thread,
    persistent_curve_parameter_container,
    persistent_curve_parameters,
    special_curve_parameter;

{$R manifest.res}

{$IFDEF _WINDOWS}
{$R *.res}
{$ENDIF}

    function CmdLineParamFound(ParamName: string; var Value: string): boolean;
    const
        { assume that command line parameters
      start with "/" or "\" character }
        Token = '/\';
    var
        i, j, TokenPos: integer;
        Param: string;

    begin
        Value  := '';
        Result := False;

        for i := 1 to ParamCount do
        begin
            Param := ParamStr(i);
            for j := 1 to Length(Token) do
                if (Token[j] = Param[1]) and AnsiContainsStr(Param, ParamName) then
                begin
                    Result   := True;
                    TokenPos := Pos('=', Param);
                    if TokenPos <> 0 then
                        Value := Copy(Param, TokenPos + 1, Length(Param) - TokenPos);
                    Exit;
                end;
        end;
    end;

var
    CmdLineParam: string;
begin
    Application.Initialize;
    Application.CreateForm(TFormMain, FormMain);
    FormMain.ApplicationProperties1.Title := 'Fit';
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
    Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
    Application.CreateForm(TInputMaxRFactorDlg, InputMaxRFactorDlg);
    Application.CreateForm(TCreateUserPointsSetDlg, CreateUserPointsSetDlg);
    Application.CreateForm(TUserPointsSetPropDlg, UserPointsSetPropDlg);
    //  Syntax /INFILE=file_name.dat.
    CmdLineParam := '';
    if CmdLineParamFound('INFILE', CmdLineParam) then
        FormMain.LoadDataFile(CmdLineParam);
    if CmdLineParamFound('WRITE_PARAMS_LOG', CmdLineParam) then
        WriteParamsLog := True;
    Application.Run;
end.
