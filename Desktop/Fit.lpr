program Fit;

{  THE STRICT MODE, AND IT IS LOAD-BEARING RATHER THAN A HOUSE STYLE. Fit.lpi
   compiles in Delphi syntax mode, where `@Routine` yields an untyped pointer
   that is assignment-compatible with ANY procedural variable - so wiring a seam
   with the wrong signature compiles silently. That shipped: `@FileExists`
   passed to a one-argument check bound to the two-argument UnicodeString
   overload, and the project last open could never be found again.

   Under objfpc the compiler refuses it by signature, which is what every other
   unit here is checked by. This is also the file that needs it most: a program
   file is pure wiring, and no test links it.

   fit_server.lpr and fit_tests.lpr already declare the same, and
   tools/build-tests/syntax_mode.tests.ps1 keeps all three that way. }
{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    //  The DWARF line-info reader (lnfodwrf) is linked in by the project's debug
    //  options, not listed here: it is what turns the backtrace addresses that
    //  client_log.LogClientFatalException writes into unit and line numbers.
    //  Naming it here as well is a duplicate identifier.
    about_box_dialog,
    app,
    app_settings,
    ClientCallback,
    configurable_points_set,
    configurable_user_points_set,
    create_user_points_set_dlg,
    fit_client,
    http_fit_service,
    fit_client_app,
    fit_client_stub,
    form_main,
    Forms,
    //  What this program accepts on its command line. Moved out of the nested
    //  function below, because a rule nothing can link is a rule nothing checks -
    //  and this one decides whether a file the desktop opens with Fit arrives.
    command_line_switches,
    //  Which of a project, a data file or nothing to open at start-up.
    recent_project,
    startup_sequence,
    project_workflow,
    log,
    client_log,
    input_back_factor_dialog,
    set_maximum_rfactor_dialog,
    input_wavelength_dialog,
    Interfaces,   //  LCL widgetset
    curve_list,
    mscr_specimen_list,
    persistent_curve_parameter_container,
    persistent_curve_parameters,
    self_copied_component,
    special_curve_parameter,
    Classes,
    StrUtils,
    SysUtils,
    curve_list_grid,
    ui_dpi,
    TurboPowerIPro,
    user_points_set,
    user_points_set_prop_dialog,
    RunningThread,
    Variants;

//  Keeps the GUI responsive during long fits; injected into RunningThread, which no
//  longer depends on the LCL itself.
procedure FitProcessMessages;
begin
    Application.ProcessMessages;
end;

//  NO SECOND MANIFEST HERE.
//
//  This used to be {$R manifest.res} - a hand-made RT_MANIFEST, id 1, that said
//  the application is "CompanyName.ProductName.YourApp" and said nothing at all
//  about DPI. The .lpi ALSO asks Lazarus for a manifest (UseXPManifest), which
//  lands in Fit.res under the same resource id. Two RT_MANIFEST id 1 resources
//  in one binary is not a spare: the linker keeps one of them, and which one is
//  not something the build states. When the stale one won, Windows read an
//  application that never declared dpiAware, so it ran the whole GUI through the
//  bitmap stretcher on a scaled display - the blurred, half-size window this
//  removal is about.
//
//  The manifest now comes from exactly one place: the .lpi's <XPManifest>, which
//  carries the real identity and <DpiAware Value="True/PM_V2"/>.

//  THE APPLICATION ICON, ON EVERY PLATFORM AND IN EVERY PROJECT THAT COMPILES
//  THIS PROGRAM.
//
//  Not the project resource. {$R *.res} is Fit.res HERE, but this file is also
//  the main unit of the module's own Desktop/FitPro.lpi, where the same
//  directive means FitPro.res - a file that project has never had. So the
//  packaged private application was built with no MAINICON in it at all, its
//  window advertised no _NET_WM_ICON, and the desktop drew its blank
//  placeholder. Nothing about the build said so; the icon was simply absent.
//
//  fit_icon.res is named, not globbed, so it is the same resource whichever
//  project is being built. It is regenerated from Desktop/Fit.ico with
//  fpcres by the maintainers' icon task, and the .lpi carries <Icon Value="-1"/> so
//  Lazarus does NOT also embed one - two MAINICON groups in one binary is a
//  duplicate resource, not a spare.
{$R fit_icon.res}

//  Version info and the XP manifest; on this project it also USED to carry the
//  icon. Left in place for what it still holds.
{$R *.res}

type
    { THE WINDOW, AS THE START-UP SEQUENCE NEEDS IT. Five one-line forwarders and
      no decision: which of the switches wins, what a missing file means and
      what to say about it are startup_sequence's and recent_project's, where
      tests reach them.

      Here rather than on TFormMain for the coverage rule's sake -
      tools/coverage/wrappers.txt says the window's line total may only shrink -
      and it belongs here anyway: the log is the application's, not the window's. }
    TAppStartupHost = class(TObject, IStartupHost)
    public
        function LastProject: string;
        function OpenProject(const APath: string): boolean;
        procedure LoadDataFile(const APath: string);
        procedure Warn(const AMessage: string);
        procedure Note(const AMessage: string);
        procedure ForgetLastProject;
    end;

function TAppStartupHost.LastProject: string;
begin
    Result := FormMain.LastProjectFile;
end;

function TAppStartupHost.OpenProject(const APath: string): boolean;
begin
    Result := FormMain.ProjectFlow.OpenProjectAt(APath);
end;

procedure TAppStartupHost.ForgetLastProject;
begin
    FormMain.ForgetLastProject;
end;

procedure TAppStartupHost.LoadDataFile(const APath: string);
begin
    FormMain.LoadDataFile(APath);
end;

procedure TAppStartupHost.Warn(const AMessage: string);
begin
    WriteLog(AMessage, Warning);
end;

procedure TAppStartupHost.Note(const AMessage: string);
begin
    //  Debug: of interest only to someone asking why a session opened what it
    //  opened, and that question is asked from a log file after the fact.
    WriteLog(AMessage, Debug);
end;

    { A path as given on the command line, made usable on this platform.

      Separators are normalised so the same switch works on every OS - an IDE run
      parameter may carry Windows backslashes - and a relative path that is not
      there is retried against the executable's own directory, which is what
      makes a switch work when the program is started from somewhere else. }
    function ResolvedPath(const AValue: string): string;
    begin
        Result := Trim(AValue);
        if Result = '' then
            Exit;
        Result := AnsiReplaceStr(Result, '', DirectorySeparator);
        Result := AnsiReplaceStr(Result, '/', DirectorySeparator);
        if not FileExists(Result) then
            Result := ExtractFilePath(ParamStr(0)) + Result;
    end;

    function CmdLineParamFound(ParamName: string; var Value: string): boolean;
    var
        Args: TStringList;
    begin
        //  THE RULE ITSELF IS IN command_line_switches, where a test can reach
        //  it. What is left here is this process's own arguments, which is the
        //  one part a test cannot supply.
        Args := TStringList.Create;
        try
            CommandLineArgs(Args);
            Result := SwitchFound(Args, ParamName, Value);
        finally
            Args.Free;
        end;
    end;

var
    CmdLineParam: string;
    ProjectParam, InFileParam: string;
    StartupHost: TAppStartupHost;
    LogLevel: TMsgType;
    UiDPI: integer;
begin
    //  The client keeps its own log, separate from the compute server's: the two
    //  are different processes and would interleave into one unreadable file.
    StartClientLog;
    //  Nothing has to be passed to get a full log: the client starts at the Debug
    //  tier with the trace tier on (see log.LogLevel and client_log). /LOG_LEVEL
    //  therefore only ever turns the log DOWN - for a user who wants a quieter
    //  file without a rebuild. Syntax /LOG_LEVEL=warning
    //  (fatal|warning|notification|debug).
    if CmdLineParamFound('LOG_LEVEL', CmdLineParam) then
    begin
        if TryParseLogLevel(CmdLineParam, LogLevel) then
        begin
            SetLogLevel(LogLevel);
            //  The trace tier is Debug-level detail; asking for less than Debug
            //  must silence it too, or the boolean would keep feeding WriteLog
            //  lines it then drops.
            if LogLevel < Debug then
                WriteClientTraceLog := False;
        end
        else
            //  Not silently ignored: a mistyped level would otherwise look like
            //  a client that refuses to log.
            WriteLog('/LOG_LEVEL: unknown level "' + CmdLineParam +
                '", keeping the default', Warning);
    end;
    WriteLog('Fit client started: ' + ParamStr(0), Notification);

    //  Set BEFORE Initialize and before any form is created: Application.Scaled is
    //  the master switch for LCL's high-DPI autoscaling, and every TForm reads it
    //  while it is being constructed (TCustomDesignControl.Create). Flipping it
    //  afterwards leaves the forms already built at their design-time size.
    //  Each form then scales itself from its own DesignTimePPI to the PPI of the
    //  monitor it opens on; the forms opt in by default, so there is nothing to
    //  set per form.
    Application.Scaled := True;
    //  Syntax /DPI=192. An explicit ppi for the interface, for the case where
    //  the display cannot be asked - see ui_dpi. Read here rather than there so
    //  that every command-line switch is parsed in one place.
    if CmdLineParamFound('DPI', CmdLineParam) then
    begin
        if TryStrToInt(Trim(CmdLineParam), UiDPI) and (UiDPI > 0) then
            SetUiPixelsPerInchOverride(UiDPI)
        else
            //  Not silently ignored: a mistyped /DPI would otherwise look like
            //  an application that refuses to scale.
            WriteLog('/DPI: not a positive number: "' + CmdLineParam +
                '", working the scale out instead', Warning);
    end;
    Application.Initialize;
    //  AFTER Initialize, which is what asks the widget set for the screen, and
    //  BEFORE the first CreateForm below, because a form takes the ppi it will
    //  live at from the screen while it is being constructed.
    ApplyUiPixelsPerInch;
    //  The parameter table seeds its column widths and row heights from this,
    //  and used to read Screen directly - which is what put Forms, and so the
    //  LCL, on the fitting engine's dependency path. Set here, after
    //  ApplyUiPixelsPerInch has corrected what the widget set reports, because
    //  a value left at the unscaled default gives a column too narrow for the
    //  number in it on a scaled display - the exact bug the seed sizes were
    //  written to fix.
    CurveListPixelsPerInch := Screen.PixelsPerInch;
    WriteLog('Parameter table seeds scaled at ' +
        IntToStr(CurveListPixelsPerInch) + ' ppi', Debug);
    RunningThread.OnProcessMessages := @FitProcessMessages;
    Application.CreateForm(TFormMain, FormMain);
    LogFormScaling(FormMain);
    FormMain.ApplicationProperties.Title := 'Fit';
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
    Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
    Application.CreateForm(TSetMaximumRFactorDlg, SetMaximumRFactorDlg);
    Application.CreateForm(TCreateUserPointsSetDlg, CreateUserPointsSetDlg);
    Application.CreateForm(TUserPointsSetPropDlg, UserPointsSetPropDlg);
    //  WHAT TO OPEN. Syntax /PROJECT=file.fitproj and /INFILE=file.dat; with
    //  neither, the project last open is offered again.
    //
    //  NOTHING BELOW DECIDES ANYTHING. It reads the two switches and resolves
    //  their paths; which one wins, what a missing file means and what is said
    //  about it are recent_project's and startup_sequence's, where tests reach
    //  them. /INFILE still means "start fresh with this data" and still
    //  outranks the remembered project - opening that and then loading data
    //  into it would silently modify a document nobody asked to open.
    CmdLineParam := '';
    if not CmdLineParamFound('PROJECT', CmdLineParam) then
        CmdLineParam := '';
    ProjectParam := ResolvedPath(CmdLineParam);
    CmdLineParam := '';
    if not CmdLineParamFound('INFILE', CmdLineParam) then
        CmdLineParam := '';
    InFileParam := ResolvedPath(CmdLineParam);

    //  AND THE SEQUENCE ITSELF IS NOT HERE EITHER, which is the fix for the
    //  one defect this block has had: reading the window, deciding and acting
    //  used to be written out below, in a program file no test links, and the
    //  existence check handed to the decision was wrong. startup_sequence is
    //  driven by testcase_startup_sequence exactly as it is driven here -
    //  including this entry point, the one that supplies its own check.
    StartupHost := TAppStartupHost.Create;
    try
        RunStartup(ProjectParam, InFileParam, StartupHost);
    finally
        //  CORBA interfaces do not count references, so this is not optional.
        StartupHost.Free;
    end;
    if CmdLineParamFound('WRITE_PARAMS_LOG', CmdLineParam) then
        WriteParamsLog := True;
    //  Syntax /CHECK_UI. THE WINDOW CHECKING ITSELF, and the only place some of
    //  these questions can be answered at all: they are about real widgets with
    //  resolved anchors, a real font and the state the model is actually in, and
    //  the headless suites deliberately link none of that.
    //
    //  ONE SWITCH FOR ALL OF IT, and it was called /CHECK_LAYOUT while captions
    //  were all it measured. A second switch would have meant a second task and
    //  a second step in every build, one of which would eventually be run while
    //  the other was forgotten - and nothing here needs separating: the forms
    //  are already built, /INFILE above has already opened a profile, and the
    //  task that drives this already starts the compute server.
    //
    //  Here, after all the forms exist and after the scaling above, which is the
    //  only point at which the answers are the ones the user would see.
    if CmdLineParamFound('CHECK_UI', CmdLineParam) then
    begin
        //  Every caption against the control holding it.
        ReportClippedCaptions;
        //  And every command against the two surfaces that draw it.
        FormMain.ReportSurfaceDisagreements;
        //  And whether the Model panel's row commands can ever be reached,
        //  which needs a model - so this one builds the smallest one that has
        //  curves in it. Last, because it is the only check that changes
        //  anything.
        FormMain.ReportRowCommandReachability;
        //  And the legend against the chart it describes - after the model
        //  above, which is what puts more than the profile in it.
        FormMain.ReportLegendPairing;
        //  A measurement, not a session. Nothing here needs a window and the
        //  caller that drives this must not sit waiting for one to be closed;
        //  what it asked for is in the log by the time this returns.
        Application.Terminate;
    end;
    Application.Run;
    WriteLog('Fit client stopped', Notification);
end.
