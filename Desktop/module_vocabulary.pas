// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The words a field uses for the framework's generic workflow, supplied
by the active module.)

THE FRAMEWORK'S WORDS ARE ONE FIELD'S WORDS. "Curve positions" is what a
diffraction pattern's peaks are called; a wave count has none, and a field of
its own would say something else. The axis titles and the pointer's readout
already come from the axis modes in force, and the Model panel's caption from
the owner of the model's curves. This is the same rule for the names of the
series a chart draws for the workflow every field shares: where the picks go,
where they land, which stretches are fitted, what is left over.

A CLOSED LIST OF TERMS, NOT A TABLE OF WORDS PER FEATURE. The framework names
what its own series stand for (TVocabularyTerm) and gives each its default
words; a module says a term differently by registering its own. Nothing here
says which module has which word, so a module added later changes no line of
the framework's.

THE ACTIVE MODULE is the owner of the selected curve type, the one the Model
panel's caption follows (curve_type_registration.CurveTypeOwner): a model holds
one module's curves only, and with nothing in it the selection says which
module the user is about to work in.

WHY THE MENUS ARE NOT RENAMED. A menu entry is named in the guide by the path a
user follows and a test holds the designed form to that, so the menu captions
stay the designed form's; a module's words reach them through its own guide
chapter, which says them. What is renamed is what the program draws itself and a
script names: the legend's rows.

A LEGEND CAPTION STANDS FOR EVERY ROW THAT CARRIES IT (legend_entries), so two
terms may never read the same in one module: that would make one script command
hide two layers. Findings walks the registrations and says so by name.
}
unit module_vocabulary;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { What the framework's own series stand for. }
    TVocabularyTerm = (
        { Where the user picked the positions a curve is placed at. }
        vtCurvePositions,
        { Where the fit put the curves, beside the picks. }
        vtFittedPositions,
        { The stretches of the profile that are fitted, each on its own. }
        vtFitIntervals,
        { The points the background is drawn through. }
        vtBackgroundPoints,
        { Data minus model. }
        vtDifference,
        { The stretches where the difference is not noise. }
        vtDifferenceNotNoise
        );

    TVocabularyStrings = array of string;

    { What the modules said, kept apart from the one the program uses so a test
      builds its own. }
    TModuleVocabulary = class(TObject)
    private
        FModules: TVocabularyStrings;
        FTerms: array of TVocabularyTerm;
        FWords: TVocabularyStrings;
    public
        { AModule says ATerm in AWords. AModule is the name its curve types are
          registered under (curve_type_registration.ExpectCurveTypes). An empty
          word is refused, and so is a second, different word for the same term
          of the same module: two registrations disagreeing is a front door
          called with two meanings, and the later one winning would hide which.
          The same word again is a no-op, because a front door may be called
          twice. }
        procedure Say(const AModule: string; ATerm: TVocabularyTerm;
            const AWords: string);
        { How AModule says ATerm: its own words, else the framework's. }
        function WordsFor(const AModule: string; ATerm: TVocabularyTerm): string;
        { The modules that said anything, in registration order. }
        function Modules: TVocabularyStrings;
        { Every registration that would make two legend rows of one module read
          alike, or that names a module under which no curve type is
          registered; empty when they are sound. AKnownModules are the names the
          curve types are registered under. }
        function Findings(const AKnownModules: array of string): TVocabularyStrings;
    end;

{ The framework's own words for ATerm: what a module that says nothing gets. }
function FrameworkWords(ATerm: TVocabularyTerm): string;

{ The vocabulary this program uses: what every module's front door registered. }
function ProgramVocabulary: TModuleVocabulary;

{ Who the active module is, by the name its curve types are registered under:
  the owner of the selected curve type. }
function ActiveVocabularyModule: string;

{ How the active module says ATerm. }
function VocabularyWords(ATerm: TVocabularyTerm): string;

implementation

uses
    curve_type_registration, curve_types_singleton;

var
    TheVocabulary: TModuleVocabulary = nil;

function FrameworkWords(ATerm: TVocabularyTerm): string;
begin
    case ATerm of
        vtCurvePositions:     Result := 'Curve positions';
        vtFittedPositions:    Result := 'Fitted positions';
        vtFitIntervals:       Result := 'Fit intervals';
        vtBackgroundPoints:   Result := 'Background points';
        vtDifference:         Result := 'Difference';
        vtDifferenceNotNoise: Result := 'Difference not noise';
    end;
end;

procedure TModuleVocabulary.Say(const AModule: string; ATerm: TVocabularyTerm;
    const AWords: string);
var
    i: longint;
begin
    if Trim(AModule) = '' then
        raise EArgumentException.Create('a vocabulary names the module it belongs to');
    if Trim(AWords) = '' then
        raise EArgumentException.CreateFmt(
            'the module %s gave an empty word for %s', [AModule,
            FrameworkWords(ATerm)]);
    for i := 0 to High(FModules) do
        if SameText(FModules[i], AModule) and (FTerms[i] = ATerm) then
        begin
            if FWords[i] = AWords then
                Exit;
            raise EArgumentException.CreateFmt(
                'the module %s says "%s" as "%s" and as "%s"',
                [AModule, FrameworkWords(ATerm), FWords[i], AWords]);
        end;
    SetLength(FModules, Length(FModules) + 1);
    SetLength(FTerms, Length(FTerms) + 1);
    SetLength(FWords, Length(FWords) + 1);
    FModules[High(FModules)] := AModule;
    FTerms[High(FTerms)] := ATerm;
    FWords[High(FWords)] := AWords;
end;

function TModuleVocabulary.WordsFor(const AModule: string;
    ATerm: TVocabularyTerm): string;
var
    i: longint;
begin
    for i := 0 to High(FModules) do
        if SameText(FModules[i], AModule) and (FTerms[i] = ATerm) then
            Exit(FWords[i]);
    Result := FrameworkWords(ATerm);
end;

function TModuleVocabulary.Modules: TVocabularyStrings;
var
    i, j: longint;
    Seen: boolean;
begin
    Result := nil;
    for i := 0 to High(FModules) do
    begin
        Seen := False;
        for j := 0 to High(Result) do
            if SameText(Result[j], FModules[i]) then
                Seen := True;
        if not Seen then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := FModules[i];
        end;
    end;
end;

function TModuleVocabulary.Findings(
    const AKnownModules: array of string): TVocabularyStrings;

    procedure Add(const AText: string);
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := AText;
    end;

    function Known(const AModule: string): boolean;
    var
        K: string;
    begin
        Result := False;
        for K in AKnownModules do
            if SameText(K, AModule) then
                Exit(True);
    end;

var
    Module: string;
    A, B: longint;
begin
    Result := nil;
    for Module in Modules do
    begin
        if not Known(Module) then
            Add(Format('%s registered words but no curve type under that name',
                [Module]));
        //  By ordinal, not Succ: Succ of the last term is past the type.
        for A := Ord(Low(TVocabularyTerm)) to Ord(High(TVocabularyTerm)) do
            for B := A + 1 to Ord(High(TVocabularyTerm)) do
                if SameText(WordsFor(Module, TVocabularyTerm(A)),
                    WordsFor(Module, TVocabularyTerm(B))) then
                    Add(Format('%s says %s and %s alike ("%s")',
                        [Module, FrameworkWords(TVocabularyTerm(A)),
                        FrameworkWords(TVocabularyTerm(B)),
                        WordsFor(Module, TVocabularyTerm(A))]));
    end;
end;

function ProgramVocabulary: TModuleVocabulary;
begin
    if TheVocabulary = nil then
        TheVocabulary := TModuleVocabulary.Create;
    Result := TheVocabulary;
end;

function ActiveVocabularyModule: string;
begin
    Result := CurveTypeOwner(
        TCurveTypesSingleton.CreateCurveTypeSelector.GetSelectedCurveType);
end;

function VocabularyWords(ATerm: TVocabularyTerm): string;
begin
    Result := ProgramVocabulary.WordsFor(ActiveVocabularyModule, ATerm);
end;

finalization
    TheVocabulary.Free;

end.
