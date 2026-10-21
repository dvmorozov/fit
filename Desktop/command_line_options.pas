// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the client accepts on its command line: --name=value, read
against one table of options.)

WHY IT REPLACED command_line_switches (2026-10-19). That rule matched a switch's
name anywhere in an argument - its value included - and stopped at the first
match. /INFILE=/tmp/PROJECT/x.dat counted as /PROJECT too, a switch whose name
began another's counted as both, and one given twice was read once. The
scripting module needs --script beside --script-check and a --param that
repeats, which that rule could not give.

THE FCL READS THE VALUES. TCustomApplication (fcl-base custapp) is the parser
every FPC program already carries - the LCL's TApplication descends from it -
and its HasOption, GetOptionValue and GetOptionValues read --name=value through
the virtual Params[], which TArgumentList below fills from a list. So the
parsing is the FCL's and a test can hand it any arguments.

THE CHECK AGAINST THE TABLE IS OURS, AND THAT IS NOT A CHOICE. The FCL's own
CheckOptions reads System.ParamStr directly rather than Params[] (custapp.pp,
"O:=Paramstr(I)"), so it can only ever check the arguments this process was
started with, and it stops at the first mistake unless told otherwise. The loop
in TCommandLine.Create checks the same three things - an unknown option, a flag
given a value, an option missing its value - over the list, all at once, plus
the one the FCL does not check: an option given twice that may not repeat.

THE DOUBLE DASH IS OURS AND NOTHING ELSE IS. A single-dash argument belongs to
the toolkit - macOS adds -psn_<number> to a program the Finder starts, Qt reads
-platform and -style - and the LCL's debug logger reads --debug-log and
--debug-enable itself. Refusing any of those would refuse a start the user
cannot change, so they are passed over; a bare path is passed over too, as it
always was (the launcher translates one, launcher_rules.SwitchForArgument).

THE OLD SPELLING, FOR ONE RELEASE. /NAME=value (or \NAME=value), with NAME the
option's name in capitals and _ for - (OldSpellingOf), is read as --name=value
and leaves a Note naming the new spelling. It is matched whole, so
/SCRIPT_CHECK is never /SCRIPT and an absolute Unix path is never an option.
The release after the one that ships this deletes the translation (the
publishing pipeline plan, Stage 11, and the roadmap).
}
unit command_line_options;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, custapp, int_automation;

type
    { One option the client accepts. }
    TCommandLineOption = record
        { Without the dashes: 'project'. }
        Name: string;
        { What the value is, for the help ('file'); empty for a flag. }
        ValueName: string;
        Help: string;
        { Whether it may be given more than once (--param). }
        Repeatable: boolean;
        { Whether giving it starts a run nobody watches - the window checking
          itself, a recording, a script: no notices asked, no update check, no
          dialog at the end of a fit, and a command line it cannot read stops
          it (CommandLineVerdict). }
        Unattended: boolean;
        { Whether giving it starts with nothing open: the project last open is
          not reopened, because what the run does must not depend on what
          this machine had open before - a script. A recording is unattended
          but not fresh: it shows the session as the user left it. }
        StartsFresh: boolean;
    end;

    { The options a program accepts, in the order its help lists them. }
    TCommandLineOptions = class
    private
        FItems: array of TCommandLineOption;
        function GetCount: integer;
        function GetItem(AIndex: integer): TCommandLineOption;
    public
        { Adds an option; adding a name already there replaces it, so a unit
          registering itself twice changes nothing. }
        procedure Add(const AName, AValueName, AHelp: string;
            ARepeatable: boolean = False; AUnattended: boolean = False;
            AStartsFresh: boolean = False);
        function IndexOf(const AName: string): integer;
        property Count: integer read GetCount;
        property Items[AIndex: integer]: TCommandLineOption read GetItem; default;
    end;

    { A command line read against a table. }
    TCommandLine = class
    private
        FTable: TCommandLineOptions;
        FReader: TCustomApplication;
        FErrors, FNotes: TStringList;
        function Known(const AName: string): integer;
    public
        { Reads AArgs - the arguments, without the program's own path - against
          ATable, which is not owned. }
        constructor Create(AArgs: TStrings; ATable: TCommandLineOptions);
        destructor Destroy; override;
        function Has(const AName: string): boolean;
        { The option's value; '' when it is absent or a flag. }
        function Value(const AName: string): string;
        { Every value of a repeatable option, in the order given. }
        function Values(const AName: string): TStringArray;
        { What could not be read, one line each, every one of them. }
        property Errors: TStringList read FErrors;
        { What was read but is spelt the old way, one line each. }
        property Notes: TStringList read FNotes;
    end;

{ The table the client reads its command line against: the framework's own
  options, then whatever units of this build register. }
function ClientOptions: TCommandLineOptions;

{ Adds an option to ClientOptions. A unit linked only into some builds - the
  recording, a module - registers its own options, so a build without it does
  not accept them. }
procedure RegisterCommandLineOption(const AName, AValueName, AHelp: string;
    ARepeatable: boolean = False; AUnattended: boolean = False;
    AStartsFresh: boolean = False);

{ Whether ALine names any option that starts a run nobody watches. }
function UnattendedRequested(ALine: TCommandLine; ATable: TCommandLineOptions): boolean;

{ Whether ALine names any option that starts with nothing open. }
function FreshStartRequested(ALine: TCommandLine; ATable: TCommandLineOptions): boolean;

type
    { What a unit does once the window has opened, when its option was given:
      a script runner, say. It drives the window only through AHost. }
    TStartupAction = procedure(ALine: TCommandLine; AHost: IAutomationHost);

{ Has AAction run after start-up whenever AOption is on the command line. A
  module registers its own; the framework names none. }
procedure RegisterStartupAction(const AOption: string; AAction: TStartupAction);

{ Runs the actions whose options ALine names, in the order they registered.
  The number run. }
function RunStartupActions(ALine: TCommandLine; AHost: IAutomationHost): integer;

{ The name an option had before --name: capitals, with _ for -. }
function OldSpellingOf(const AName: string): string;

{ The usage text --help prints, from ATable. }
function CommandLineHelp(const AProgram: string; ATable: TCommandLineOptions): string;

type
    { What the client does with a command line before anything else. }
    TCommandLineVerdict = (
        { Start as usual; any mistakes are written to the log and the console. }
        cvStart,
        { Print the help and close. }
        cvHelp,
        { Print the version and close. }
        cvVersion,
        { Close with exit code 2, having said why: a mistake in an unattended
          run, where nobody would read a warning and the caller needs to know. }
        cvRefuse);

{ The verdict on ALine. AUnattended is a run nobody watches - the window
  checking itself, a recording, a script. An interactive start with a mistake
  still starts: the option was mistyped, not the program, and the old rule
  started in silence where this one says what it did not understand. }
function CommandLineVerdict(ALine: TCommandLine; AUnattended: boolean): TCommandLineVerdict;

{ The command line this process was started with, as a list - ParamStr(1) up.
  Separate from the reading, so that the reading can be exercised without a
  process started with the arguments it is about. }
procedure CommandLineArgs(ADest: TStrings);

implementation

const
    { Options the LCL's debug logger reads for itself (LazLogger). }
    ToolkitOptions: array[0..2] of string =
        ('debug-log', 'debug-enable', 'debug-disable');

type
    { THE FCL's PARSER OVER A LIST. Params[] and ParamCount are virtual, and
      everything this unit asks of the parser goes through them. }
    TArgumentList = class(TCustomApplication)
    private
        FArgs: TStringList;
    protected
        function GetParams(AIndex: integer): string; override;
        function GetParamCount: integer; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        property Args: TStringList read FArgs;
    end;

var
    TheClientOptions: TCommandLineOptions = nil;

{ ---- TArgumentList ---------------------------------------------------------- }

constructor TArgumentList.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FArgs := TStringList.Create;
end;

destructor TArgumentList.Destroy;
begin
    FArgs.Free;
    inherited Destroy;
end;

function TArgumentList.GetParams(AIndex: integer): string;
begin
    //  Params[0] is the program itself, which no option is.
    if (AIndex < 1) or (AIndex > FArgs.Count) then
        Result := ''
    else
        Result := FArgs[AIndex - 1];
end;

function TArgumentList.GetParamCount: integer;
begin
    Result := FArgs.Count;
end;

{ ---- TCommandLineOptions ------------------------------------------------------ }

function TCommandLineOptions.GetCount: integer;
begin
    Result := Length(FItems);
end;

function TCommandLineOptions.GetItem(AIndex: integer): TCommandLineOption;
begin
    Result := FItems[AIndex];
end;

procedure TCommandLineOptions.Add(const AName, AValueName, AHelp: string;
    ARepeatable: boolean; AUnattended: boolean; AStartsFresh: boolean);
var
    I: integer;
begin
    I := IndexOf(AName);
    if I < 0 then
    begin
        I := Length(FItems);
        SetLength(FItems, I + 1);
    end;
    FItems[I].Name := AName;
    FItems[I].ValueName := AValueName;
    FItems[I].Help := AHelp;
    FItems[I].Repeatable := ARepeatable;
    FItems[I].Unattended := AUnattended;
    FItems[I].StartsFresh := AStartsFresh;
end;

function TCommandLineOptions.IndexOf(const AName: string): integer;
begin
    for Result := 0 to High(FItems) do
        if FItems[Result].Name = AName then
            Exit;
    Result := -1;
end;

{ ---- the old spelling --------------------------------------------------------- }

function OldSpellingOf(const AName: string): string;
begin
    Result := StringReplace(UpperCase(AName), '-', '_', [rfReplaceAll]);
end;

{ The option an argument spelt the old way names, or -1. Whole names only: the
  part between the slash and the equals sign (or the end) must be one option's
  old name exactly. }
function OldSpellingIndex(const AArg: string; ATable: TCommandLineOptions;
    out AValue: string; out AHasValue: boolean): integer;
var
    Eq: integer;
    Head: string;
begin
    Result := -1;
    AValue := '';
    AHasValue := False;
    if (Length(AArg) < 2) or not (AArg[1] in ['/', '\']) then
        Exit;
    Eq := Pos('=', AArg);
    if Eq > 0 then
        Head := Copy(AArg, 2, Eq - 2)
    else
        Head := Copy(AArg, 2, MaxInt);
    for Result := 0 to ATable.Count - 1 do
        if OldSpellingOf(ATable[Result].Name) = Head then
        begin
            AHasValue := Eq > 0;
            if AHasValue then
                AValue := Copy(AArg, Eq + 1, MaxInt);
            Exit;
        end;
    Result := -1;
end;

{ ---- TCommandLine ---------------------------------------------------------- }

function TCommandLine.Known(const AName: string): integer;
begin
    Result := FTable.IndexOf(AName);
end;

constructor TCommandLine.Create(AArgs: TStrings; ATable: TCommandLineOptions);
var
    Arg, Name, OldValue, Own: string;
    I, Eq, Index: integer;
    HasValue, Toolkit: boolean;
    Seen: array of boolean;
    Option: TCommandLineOption;
begin
    inherited Create;
    FTable := ATable;
    FErrors := TStringList.Create;
    FNotes := TStringList.Create;
    FReader := TArgumentList.Create(nil);
    SetLength(Seen, FTable.Count);
    for I := 0 to AArgs.Count - 1 do
    begin
        Arg := AArgs[I];
        //  THE OLD SPELLING becomes the new one before anything else looks.
        Index := OldSpellingIndex(Arg, FTable, OldValue, HasValue);
        if Index >= 0 then
        begin
            Name := FTable[Index].Name;
            if HasValue then
                Arg := '--' + Name + '=' + OldValue
            else
                Arg := '--' + Name;
            FNotes.Add(Format('/%s is the old spelling of --%s, and stops working ' +
                'in the next release: write %s', [OldSpellingOf(Name), Name, Arg]));
        end;
        //  Only the double dash is ours (the header says why).
        if Copy(Arg, 1, 2) <> '--' then
            Continue;
        Eq := Pos('=', Arg);
        HasValue := Eq > 0;
        if HasValue then
            Name := Copy(Arg, 3, Eq - 3)
        else
            Name := Copy(Arg, 3, MaxInt);
        Toolkit := False;
        for Own in ToolkitOptions do
            Toolkit := Toolkit or (Name = Own);
        if Toolkit then
            Continue;
        Index := Known(Name);
        if Index < 0 then
        begin
            FErrors.Add(Format('--%s is not an option of this program; ' +
                '--help lists the ones it has', [Name]));
            Continue;
        end;
        Option := FTable[Index];
        if (Option.ValueName = '') and HasValue then
        begin
            FErrors.Add(Format('--%s takes no value: write --%s alone',
                [Name, Name]));
            Continue;
        end;
        if (Option.ValueName <> '') and not HasValue then
        begin
            FErrors.Add(Format('--%s needs a value: write --%s=<%s>',
                [Name, Name, Option.ValueName]));
            Continue;
        end;
        if Seen[Index] and not Option.Repeatable then
        begin
            FErrors.Add(Format('--%s is given more than once, and only one ' +
                'can be used', [Name]));
            Continue;
        end;
        Seen[Index] := True;
        TArgumentList(FReader).Args.Add(Arg);
    end;
end;

destructor TCommandLine.Destroy;
begin
    FReader.Free;
    FNotes.Free;
    FErrors.Free;
    inherited Destroy;
end;

function TCommandLine.Has(const AName: string): boolean;
begin
    Result := FReader.HasOption(AName);
end;

function TCommandLine.Value(const AName: string): string;
begin
    Result := FReader.GetOptionValue(AName);
end;

function TCommandLine.Values(const AName: string): TStringArray;
var
    Found: TStringArray;
    I, N: integer;
begin
    Result := nil;
    //  #255 is no short option, as in the FCL's own GetOptionValue(S). The
    //  FCL searches from the end, so its list comes back last first.
    Found := FReader.GetOptionValues(#255, AName);
    N := Length(Found);
    SetLength(Result, N);
    for I := 0 to N - 1 do
        Result[I] := Found[N - 1 - I];
end;

{ ---- the help ---------------------------------------------------------------- }

function CommandLineHelp(const AProgram: string; ATable: TCommandLineOptions): string;
var
    I: integer;
    Usage: string;
begin
    Result := Format('Usage: %s [option...]', [AProgram]) + LineEnding + LineEnding;
    for I := 0 to ATable.Count - 1 do
    begin
        Usage := '  --' + ATable[I].Name;
        if ATable[I].ValueName <> '' then
            Usage := Usage + '=<' + ATable[I].ValueName + '>';
        Result := Result + Usage + LineEnding + '      ' + ATable[I].Help;
        if ATable[I].Repeatable then
            Result := Result + ' It may be given more than once.';
        Result := Result + LineEnding;
    end;
    Result := Result + LineEnding +
        'A path as the only argument names no option: the launcher translates ' +
        'one into --project or --infile.' + LineEnding;
end;

function CommandLineVerdict(ALine: TCommandLine; AUnattended: boolean): TCommandLineVerdict;
begin
    //  Help first: asking for the list is the answer to a mistake.
    if ALine.Has('help') then
        Result := cvHelp
    else if ALine.Has('version') then
        Result := cvVersion
    else if (ALine.Errors.Count > 0) and AUnattended then
        Result := cvRefuse
    else
        Result := cvStart;
end;

{ ---- the client's table -------------------------------------------------------- }

function ClientOptions: TCommandLineOptions;
begin
    if TheClientOptions = nil then
    begin
        TheClientOptions := TCommandLineOptions.Create;
        //  EVERY SWITCH Fit.lpr has read, in the order the guide gives them.
        //  The help texts are the guide's bullets too (guide_projects), so
        //  each says what a user needs, not only what a developer does.
        TheClientOptions.Add('project', 'file',
            'Opens that project. It wins over everything else. If the file is ' +
            'not there, Fit starts with nothing open and says so in the log; it ' +
            'never opens a different project instead.');
        TheClientOptions.Add('infile', 'file',
            'Starts a new project with that data file imported. It wins over ' +
            'the project remembered from last time.');
        TheClientOptions.Add('log-level', 'level',
            'How much goes to the log: fatal, warning, notification, debug or ' +
            'trace. The default is debug, which already records almost ' +
            'everything, so this mostly makes the log quieter; trace adds the ' +
            'most repetitive lines as well. An unknown level is reported in the ' +
            'log and the default kept.');
        TheClientOptions.Add('dpi', 'number',
            'The screen resolution the window is scaled for, in pixels per ' +
            'inch, for a display that does not report it correctly - 192 for a ' +
            '200 % display.');
        TheClientOptions.Add('write-params-log', '',
            'Writes to the log every value assigned to a parameter of a ' +
            'user-defined curve.');
        TheClientOptions.Add('check-ui', '',
            'For the program''s developers: the window checks its own layout ' +
            'and behaviour, runs a fit, writes the results to the log and ' +
            'closes.', False, True);
        TheClientOptions.Add('help', '', 'Prints this list and closes.');
        TheClientOptions.Add('version', '', 'Prints the version and closes.');
    end;
    Result := TheClientOptions;
end;

procedure RegisterCommandLineOption(const AName, AValueName, AHelp: string;
    ARepeatable: boolean; AUnattended: boolean; AStartsFresh: boolean);
begin
    ClientOptions.Add(AName, AValueName, AHelp, ARepeatable, AUnattended,
        AStartsFresh);
end;

function UnattendedRequested(ALine: TCommandLine; ATable: TCommandLineOptions): boolean;
var
    I: integer;
begin
    for I := 0 to ATable.Count - 1 do
        if ATable[I].Unattended and ALine.Has(ATable[I].Name) then
            Exit(True);
    Result := False;
end;

function FreshStartRequested(ALine: TCommandLine; ATable: TCommandLineOptions): boolean;
var
    I: integer;
begin
    for I := 0 to ATable.Count - 1 do
        if ATable[I].StartsFresh and ALine.Has(ATable[I].Name) then
            Exit(True);
    Result := False;
end;

type
    TStartupEntry = record
        Option: string;
        Action: TStartupAction;
    end;

var
    StartupEntries: array of TStartupEntry;

procedure RegisterStartupAction(const AOption: string; AAction: TStartupAction);
var
    I: integer;
begin
    I := Length(StartupEntries);
    SetLength(StartupEntries, I + 1);
    StartupEntries[I].Option := AOption;
    StartupEntries[I].Action := AAction;
end;

function RunStartupActions(ALine: TCommandLine; AHost: IAutomationHost): integer;
var
    Entry: TStartupEntry;
begin
    Result := 0;
    for Entry in StartupEntries do
        if ALine.Has(Entry.Option) then
        begin
            Entry.Action(ALine, AHost);
            Inc(Result);
        end;
end;

procedure CommandLineArgs(ADest: TStrings);
var
    I: integer;
begin
    ADest.Clear;
    for I := 1 to ParamCount do
        ADest.Add(ParamStr(I));
end;

finalization
    FreeAndNil(TheClientOptions);
end.
