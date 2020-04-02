program Fit;

{$DEFINE UseCThreads}

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    about_box_dialog,
    app,
    app_settings,
    ClientCallback,
    configurable_points_set,
    configurable_user_points_set,
    create_user_points_set_dlg,
    fit_client,
    fit_client_app,
    fit_client_stub,
    fit_service,
    fit_service_with_thread,
    fit_task,
    fit_task_with_thread,
    form_main,
    Forms,
    GeneralHashFunctions,
    input_back_factor_dialog,
    set_maximum_rfactor_dialog,
    input_wavelength_dialog,
    Interfaces,   //  LCL widgetset
    main_calc_thread,
    component_list,
    data_classes,
    downhill_simplex_minimizer,
    fit_server_app,
    fit_service_multithreaded,
    int_minimizer,
    mscr_specimen_list,
    simple_minimizer,
    persistent_curve_parameter_container,
    persistent_curve_parameters,
    self_copied_component,
    SelfCheckedComponentList,
    special_curve_parameter,
    StrUtils,
    table_components,
    vectors,
    TurboPowerIPro,
    user_points_set,
    user_points_set_prop_dialog,
    Variants;

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
    FormMain.ApplicationProperties.Title := 'Fit';
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
    Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
    Application.CreateForm(TSetMaximumRFactorDlg, SetMaximumRFactorDlg);
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
