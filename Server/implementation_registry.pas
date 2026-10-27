// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Several implementations of one abstraction, from any provider, and
which of them runs.)

THE RULE THIS HOLDS (decisions.md, "Implementations from any provider"). The
application defines abstractions - the fitting engine, the peak search, the
background arithmetic, and whatever comes next - and lets the user choose which
implementation fills each. Where an implementation runs is its own business:
native code in the server, the Python sidecar, another machine. Two things are
fixed, and this unit is where they are enforced rather than remembered:

  - EVERY ABSTRACTION HAS A NATIVE IMPLEMENTATION, the default and the
    fallback. The first implementation registered for a contract must be a
    native one, and is refused otherwise; so a build without any optional
    provider has a usable program (non-negotiable 4).
  - AN OPTIONAL PROVIDER ONLY ADDS ALTERNATIVES. When the chosen one cannot run
    - its provider is not available here - the default runs instead and the
    answer says why (ResolveImplementation, worded by fit_advice), which is
    non-negotiable 7.

WHY ONE REGISTRY rather than a flag on each. The fitting engine had the pattern
first, with a provider-named flag (NeedsPythonSidecar) on its own record: a
second provider would have been a second flag, and the peak search and the
background arithmetic, which took "the last registration wins", had no way to
hold an alternative at all. An implementation now DECLARES its provider, a
provider declares whether it is available, and the one resolution below is
what every abstraction asks - capabilities, not a table of which choice works
where (non-negotiable 5). A contract keeps its own record for what only it
needs (an engine's formula and weighting); this holds what every one shares.

WHAT A PROVIDER IS HERE: a name, a sentence a user can read, and a probe the
host owning it sets (SetProviderProbe) - fit_server asks its sidecar whether
it is configured. NO PROBE IS NOT ABSENCE: a host that never said how to know
- an in-process test handing the engine a URL itself - gets the chosen one,
which then reports its own failure as it always did. The native provider is
always available; it is the process answering.

THE USER'S CHOICE is a machine setting: its own section of settings.json,
'implementations', one key per contract (ChosenImplementation). A contract
whose choice was persisted before this existed keeps it where it was - the
engine's kind is in the window's settings, in every project and on the wire,
and moving it would have been a migration with nothing gained.

DECLARATIONS ONLY. The desktop client links this to build its menus and its
explanations, and contains no engine (non-negotiable 1): nothing here names a
backend, a session or a task.

EVERY IMPLEMENTATION AND PROVIDER EXPLAINS ITSELF through the explanation
registry (non-negotiable 10), under 'implementations/...', made from what it
declared - so registering is explaining, with no second registration to forget.
}
unit implementation_registry;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry;

const
    { The process answering: always available, the default and the fallback. }
    NativeProvider = 'native';
    { The Python sidecar fit_server owns as a child process. Declared here
      because the framework ships it; a provider a module brings registers
      itself with RegisterProvider. }
    PythonSidecarProvider = 'python-sidecar';

    { The section of settings.json the choices are kept in. }
    ImplementationsSection = 'implementations';
    { What every implementation's and provider's explanation is filed under. }
    ImplementationsNamespace = 'implementations';

type
    EImplementationRegistration = class(Exception);

    { Whether a provider can run here, asked when a choice is resolved. }
    TProviderProbe = function: boolean of object;

    TProviderInfo = record
        Id: string;
        { How a user reads it: "the Python sidecar". }
        Name: string;
        { One or more sentences: what it is, and what having it takes. }
        Description: string;
    end;

    TProviderInfoArray = array of TProviderInfo;

    TImplementationInfo = record
        { The abstraction it implements: 'minimizer', 'peak-search'. }
        Contract: string;
        { Stable within the contract: what a choice is kept as. }
        Id: string;
        Name: string;
        Description: string;
        { The provider it runs on; '' is the native one. }
        Provider: string;
    end;

    TImplementationInfoArray = array of TImplementationInfo;

    { What runs for a choice. }
    TImplementationChoice = record
        { The implementation that runs; '' only when the contract has none. }
        Id: string;
        { True when it is not the one chosen. }
        FellBack: boolean;
        { Why not, in a user's words, when FellBack. }
        Why: string;
    end;

{ Declares a provider. Idempotent for the same id - every host declares the
  framework's own - and refused for one with no id or no name. }
procedure RegisterProvider(const AInfo: TProviderInfo);
function RegisteredProviders: TProviderInfoArray;
function FindProvider(const AId: string; out AInfo: TProviderInfo): boolean;
{ How the host owning AId knows it can run; nil forgets it. }
procedure SetProviderProbe(const AId: string; AProbe: TProviderProbe);
{ Whether AId can run here: the native one always; another when it is
  declared and its probe - when its host set one - says so. }
function ProviderIsAvailable(const AId: string): boolean;

{ Adds an implementation to its contract. Refused, naming what is wrong: with
  no contract, id, name or description; for a provider nobody declared; a
  second time for one id; and as the FIRST of a contract when it is not
  native - that one is the default and the fallback. }
procedure RegisterImplementation(const AInfo: TImplementationInfo);
{ Takes every implementation of AContract out: the build has none. }
procedure UnregisterContract(const AContract: string);
{ In registration order, the default first. }
function ImplementationsOf(const AContract: string): TImplementationInfoArray;
function FindImplementation(const AContract, AId: string;
    out AInfo: TImplementationInfo): boolean;
{ The first registered - native, by construction - or '' when there is none. }
function DefaultImplementationId(const AContract: string): string;

{ What runs when AChosenId is chosen: it, when this build has it and its
  provider is available; the default otherwise, saying why. '' chooses the
  default, which is no fallback. }
function ResolveImplementation(const AContract,
    AChosenId: string): TImplementationChoice;

{ The choice this machine keeps for AContract, '' when none was made. }
function ChosenImplementation(const AContract: string): string;
procedure ChooseImplementation(const AContract, AId: string);
{ ResolveImplementation of what this machine chose. }
function ImplementationInForce(const AContract: string): TImplementationChoice;

{ 'implementations/<contract>/<id>' and 'implementations/provider/<id>'. }
function ImplementationTopic(const AContract, AId: string): string;
function ProviderTopic(const AId: string): string;
{ The one provider explaining everything registered here. Registered with the
  explanation registry by the first RegisterImplementation. }
function ImplementationExplanationProvider: IExplanationProvider;

implementation

uses
    fit_advice, machine_settings;

const
    { The second segment of a provider's topic, which is why no contract may
      be called this. }
    ProviderSegment = 'provider';

type
    TProviderEntry = record
        Info: TProviderInfo;
        Probe: TProviderProbe;
    end;

    TImplementationExplanations = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

var
    Providers: array of TProviderEntry;
    Implementations: TImplementationInfoArray;
    Explanations: TImplementationExplanations = nil;

function ProviderIndex(const AId: string): longint;
var
    i: longint;
begin
    for i := 0 to High(Providers) do
        if SameText(Providers[i].Info.Id, AId) then
            Exit(i);
    Result := -1;
end;

procedure RegisterProvider(const AInfo: TProviderInfo);
begin
    if Trim(AInfo.Id) = '' then
        raise EImplementationRegistration.Create(
            'a provider was declared with no id, so nothing could name it');
    if Trim(AInfo.Name) = '' then
        raise EImplementationRegistration.CreateFmt(
            'provider "%s" was declared with no name, so no message could ' +
            'say what is missing', [AInfo.Id]);
    if ProviderIndex(AInfo.Id) >= 0 then
        Exit;
    SetLength(Providers, Length(Providers) + 1);
    Providers[High(Providers)].Info := AInfo;
    Providers[High(Providers)].Probe := nil;
end;

function RegisteredProviders: TProviderInfoArray;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(Providers));
    for i := 0 to High(Providers) do
        Result[i] := Providers[i].Info;
end;

function FindProvider(const AId: string; out AInfo: TProviderInfo): boolean;
var
    i: longint;
begin
    AInfo := Default(TProviderInfo);
    i := ProviderIndex(AId);
    Result := i >= 0;
    if Result then
        AInfo := Providers[i].Info;
end;

procedure SetProviderProbe(const AId: string; AProbe: TProviderProbe);
var
    i: longint;
begin
    i := ProviderIndex(AId);
    if i < 0 then
        raise EImplementationRegistration.CreateFmt(
            'a probe was set for provider "%s", which nothing declared', [AId]);
    Providers[i].Probe := AProbe;
end;

function ProviderIsAvailable(const AId: string): boolean;
var
    i: longint;
begin
    if (AId = '') or SameText(AId, NativeProvider) then
        Exit(True);
    i := ProviderIndex(AId);
    if i < 0 then
        Exit(False);
    //  NO PROBE IS NOT ABSENCE - see the header: the host never said how to
    //  know, and the implementation chosen reports its own failure.
    if not Assigned(Providers[i].Probe) then
        Exit(True);
    Result := Providers[i].Probe();
end;

function ProviderOf(const AInfo: TImplementationInfo): string;
begin
    Result := AInfo.Provider;
    if Result = '' then
        Result := NativeProvider;
end;

function ProviderName(const AId: string): string;
var
    Info: TProviderInfo;
begin
    if FindProvider(AId, Info) then
        Result := Info.Name
    else
        Result := AId;
end;

function ImplementationIndex(const AContract, AId: string): longint;
var
    i: longint;
begin
    for i := 0 to High(Implementations) do
        if SameText(Implementations[i].Contract, AContract) and
           SameText(Implementations[i].Id, AId) then
            Exit(i);
    Result := -1;
end;

procedure RegisterImplementation(const AInfo: TImplementationInfo);
var
    What: string;
begin
    What := Format('an implementation of "%s" ("%s")', [AInfo.Contract, AInfo.Name]);
    if Trim(AInfo.Contract) = '' then
        raise EImplementationRegistration.Create(What +
            ' names no contract, so nothing could choose it');
    if SameText(AInfo.Contract, ProviderSegment) then
        raise EImplementationRegistration.Create(What + ': "' +
            ProviderSegment + '" is where providers are explained, not a contract');
    if Trim(AInfo.Id) = '' then
        raise EImplementationRegistration.Create(What +
            ' has no id, so no choice could be kept for it');
    if Trim(AInfo.Name) = '' then
        raise EImplementationRegistration.Create(What +
            ' has no name, so nothing could offer it');
    if Trim(AInfo.Description) = '' then
        raise EImplementationRegistration.Create(What +
            ' has no description, so it could not explain itself');
    if ProviderIndex(ProviderOf(AInfo)) < 0 then
        raise EImplementationRegistration.CreateFmt(
            '%s runs on provider "%s", which nothing declared',
            [What, ProviderOf(AInfo)]);
    if ImplementationIndex(AInfo.Contract, AInfo.Id) >= 0 then
        raise EImplementationRegistration.CreateFmt(
            '%s: id "%s" is already taken, and two answering to one id would ' +
            'leave one offered while the other ran', [What, AInfo.Id]);
    //  THE DEFAULT AND THE FALLBACK IS NATIVE, by construction rather than by
    //  convention: a build where it were not would have nothing to fall back
    //  to when an optional provider is absent.
    if (DefaultImplementationId(AInfo.Contract) = '') and
       not SameText(ProviderOf(AInfo), NativeProvider) then
        raise EImplementationRegistration.CreateFmt(
            '%s runs on "%s", and the first implementation of a contract must ' +
            'be native: it is the default, and what runs when another cannot',
            [What, ProviderOf(AInfo)]);

    SetLength(Implementations, Length(Implementations) + 1);
    Implementations[High(Implementations)] := AInfo;
    //  REGISTERING IS EXPLAINING. Idempotent by namespace.
    RegisterExplanationProvider(ImplementationExplanationProvider);
end;

procedure UnregisterContract(const AContract: string);
var
    i, n: longint;
begin
    n := 0;
    for i := 0 to High(Implementations) do
        if not SameText(Implementations[i].Contract, AContract) then
        begin
            Implementations[n] := Implementations[i];
            Inc(n);
        end;
    SetLength(Implementations, n);
end;

function ImplementationsOf(const AContract: string): TImplementationInfoArray;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(Implementations) do
        if SameText(Implementations[i].Contract, AContract) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Implementations[i];
        end;
end;

function FindImplementation(const AContract, AId: string;
    out AInfo: TImplementationInfo): boolean;
var
    i: longint;
begin
    AInfo := Default(TImplementationInfo);
    i := ImplementationIndex(AContract, AId);
    Result := i >= 0;
    if Result then
        AInfo := Implementations[i];
end;

function DefaultImplementationId(const AContract: string): string;
var
    i: longint;
begin
    for i := 0 to High(Implementations) do
        if SameText(Implementations[i].Contract, AContract) then
            Exit(Implementations[i].Id);
    Result := '';
end;

function ResolveImplementation(const AContract,
    AChosenId: string): TImplementationChoice;
var
    Chosen, Fallback: TImplementationInfo;
begin
    Result := Default(TImplementationChoice);
    Result.Id := DefaultImplementationId(AContract);
    if (Result.Id = '') or (AChosenId = '') then
        Exit;
    FindImplementation(AContract, Result.Id, Fallback);
    if not FindImplementation(AContract, AChosenId, Chosen) then
    begin
        Result.FellBack := True;
        Result.Why := AdviseUnknownImplementation(AChosenId, Fallback.Name);
        Exit;
    end;
    if not ProviderIsAvailable(ProviderOf(Chosen)) then
    begin
        Result.FellBack := True;
        Result.Why := AdviseUnavailableProvider(Chosen.Name,
            ProviderName(ProviderOf(Chosen)), Fallback.Name);
        Exit;
    end;
    Result.Id := Chosen.Id;
end;

function ChosenImplementation(const AContract: string): string;
begin
    Result := MachineSettings.Str(ImplementationsSection, AContract, '');
end;

procedure ChooseImplementation(const AContract, AId: string);
begin
    MachineSettings.SetStr(ImplementationsSection, AContract, AId);
end;

function ImplementationInForce(const AContract: string): TImplementationChoice;
begin
    //  NOTHING TO CHOOSE BETWEEN, nothing to read: asked from a fit's worker
    //  threads, and the settings are the window's to write.
    if Length(ImplementationsOf(AContract)) <= 1 then
        Exit(ResolveImplementation(AContract, ''));
    Result := ResolveImplementation(AContract, ChosenImplementation(AContract));
end;

function ImplementationTopic(const AContract, AId: string): string;
begin
    Result := LowerCase(ImplementationsNamespace + '/' + AContract + '/' + AId);
end;

function ProviderTopic(const AId: string): string;
begin
    Result := LowerCase(ImplementationsNamespace + '/' + ProviderSegment + '/' + AId);
end;

{ TImplementationExplanations }

function TImplementationExplanations.Namespace: string;
begin
    Result := ImplementationsNamespace;
end;

function TImplementationExplanations.StaticTopics: TStringArray;
var
    i, n: longint;
begin
    Result := nil;
    SetLength(Result, Length(Providers) + Length(Implementations));
    n := 0;
    for i := 0 to High(Providers) do
    begin
        Result[n] := ProviderTopic(Providers[i].Info.Id);
        Inc(n);
    end;
    for i := 0 to High(Implementations) do
    begin
        Result[n] := ImplementationTopic(Implementations[i].Contract,
            Implementations[i].Id);
        Inc(n);
    end;
end;

{ A sentence for a summary: what was declared, ending as one. }
function AsSummary(const AText: string): string;
begin
    Result := Trim(AText);
    if (Result <> '') and not (Result[Length(Result)] in ['.', '!', '?']) then
        Result := Result + '.';
end;

function ExplainProvider(const AInfo: TProviderInfo): TExplanation;
var
    Summary: string;
begin
    Summary := AsSummary(AInfo.Description);
    if Summary = '' then
        Summary := 'Where some implementations of this program''s abstractions run.';
    //  The name is written to sit inside a sentence ("needs the Python
    //  sidecar"); as a title it starts with a capital.
    Result := NewExplanation(ProviderTopic(AInfo.Id),
        UpperCase(Copy(AInfo.Name, 1, 1)) + Copy(AInfo.Name, 2, MaxInt),
        Summary, esModelChoice);
    AddParagraph(Result, 'A provider is where an implementation runs. This ' +
        'program defines its abstractions - the fitting engine, the peak ' +
        'search, the background arithmetic - and lets the user choose which ' +
        'implementation fills each; every one has a native implementation, ' +
        'the default, and a provider only adds alternatives beside it.');
    if SameText(AInfo.Id, NativeProvider) then
        AddParagraph(Result, 'The native provider is the compute server ' +
            'itself. It is always there, which is why every default runs on it.')
    else
        AddParagraph(Result, 'When this provider is not available, whatever ' +
            'was chosen from it is replaced by the default for as long as it ' +
            'is missing, and the reply that would have used it says so. The ' +
            'choice itself is kept.');
    AddLimitation(Result, 'Whether a provider is available is asked when a ' +
        'choice is used, not watched: one that stops part way through an ' +
        'operation reports its own failure.');
    AddRelated(Result, ProviderTopic(NativeProvider));
end;

function ExplainImplementation(const AInfo: TImplementationInfo): TExplanation;
var
    Default_: TImplementationInfo;
    Provider: string;
begin
    Provider := ProviderOf(AInfo);
    Result := NewExplanation(ImplementationTopic(AInfo.Contract, AInfo.Id),
        AInfo.Name, AsSummary(AInfo.Description), esModelChoice);
    AddParagraph(Result, Format('One of the implementations of "%s" this ' +
        'build offers. It runs on %s.', [AInfo.Contract, ProviderName(Provider)]));
    if SameText(DefaultImplementationId(AInfo.Contract), AInfo.Id) then
        AddParagraph(Result, 'It is the default: what runs until another is ' +
            'chosen, and whenever the chosen one cannot run here.')
    else if FindImplementation(AInfo.Contract,
        DefaultImplementationId(AInfo.Contract), Default_) then
        AddParagraph(Result, Format('While %s is not available, "%s" runs ' +
            'instead, and the answer says why; the choice is kept.',
            [ProviderName(Provider), Default_.Name]));
    if not SameText(Provider, NativeProvider) then
        AddLimitation(Result, Format('Needs %s.', [ProviderName(Provider)]));
    AddRelated(Result, ProviderTopic(Provider));
end;

function TImplementationExplanations.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
var
    i: longint;
begin
    AExplanation := Default(TExplanation);
    for i := 0 to High(Providers) do
        if SameText(ProviderTopic(Providers[i].Info.Id), ATopic) then
        begin
            AExplanation := ExplainProvider(Providers[i].Info);
            Exit(True);
        end;
    for i := 0 to High(Implementations) do
        if SameText(ImplementationTopic(Implementations[i].Contract,
            Implementations[i].Id), ATopic) then
        begin
            AExplanation := ExplainImplementation(Implementations[i]);
            Exit(True);
        end;
    Result := False;
end;

function ImplementationExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Explanations) then
        Explanations := TImplementationExplanations.Create;
    Result := Explanations;
end;

procedure DeclareFrameworkProviders;
var
    Info: TProviderInfo;
begin
    Info := Default(TProviderInfo);
    Info.Id := NativeProvider;
    Info.Name := 'the compute server itself';
    Info.Description := 'Code built into this program''s compute server, ' +
        'which needs nothing installed beside it.';
    RegisterProvider(Info);

    Info := Default(TProviderInfo);
    Info.Id := PythonSidecarProvider;
    Info.Name := 'the Python sidecar';
    Info.Description := 'A Python process the compute server starts on ' +
        'first use and owns. The installers ship it with its libraries; a ' +
        'copy without it loses the alternatives it offers, not a function.';
    RegisterProvider(Info);
end;

initialization
    //  BEFORE ANY IMPLEMENTATION can name them: an implementation's provider is
    //  checked when it registers, and the framework's own are always there.
    DeclareFrameworkProviders;

finalization
    Explanations.Free;
end.
