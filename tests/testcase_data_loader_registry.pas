// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The loader registry's own logic, and the limit of what it can prove.)

WHAT THIS BINARY CAN AND CANNOT CHECK, stated up front for the same reason
testcase_curve_type_registration states it: this binary links every loader unit,
so an assertion here that "the loaders are registered" is true by construction
and would pass in a build whose application never called RegisterAllDataLoaders.
That is a false guard, and this codebase has already shipped one.

So what is tested here is the registry's OWN rules - that a claim is honoured,
that a conflicting claim is refused, that an unknown extension resolves to
nothing rather than to something arbitrary - plus the dialog filter, which is the
place a drift between "what we offer" and "what we can read" would show up.
}
unit testcase_data_loader_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_loader, data_loader_registry, data_loader_registration,
    dat_file_loader,
    coordinate_axis, axis_mode_registry, axis_mode_registration,
    int_data_loader, extension_data_loader_injector;

type
    TDataLoaderRegistryTest = class(TTestCase)
    published
        procedure AKnownExtensionResolvesToItsLoader;
        procedure TwoColumnTextResolvesToTheSameReaderAsDat;
        procedure TheLookupIgnoresCase;
        procedure AnUnknownExtensionResolvesToNothing;
        procedure RegisteringTwiceIsHarmless;
        procedure TwoLoadersCannotClaimTheSameExtension;
        procedure ALoaderMustNameAnExtension;
        procedure TheDialogFilterOffersOnlyWhatIsRegistered;
        //  Registered with something missing.
        procedure ALoaderWithNoClassIsRefused;
        procedure ALoaderNamingOnlyEmptyExtensionsIsRefused;
        procedure AnEmptyExtensionAmongGoodOnesIsSkipped;
        procedure AnExtensionWithoutItsDotStillMatches;
        //  What a module's front door meets: it registers its own loader, and a
        //  front door may be called twice.
        procedure TheSameLoaderRegisteredAgainIsANoOp;
        procedure TheSameLoaderClaimingSomethingElseIsRefused;
        //  What a format says its coordinates are, through the injector the
        //  client loads a file with.
        procedure ADatFileSaysNothingSinceAnySeriesIsSavedAsOne;
        procedure ATwoColumnFileSaysNothingThoughTheReaderIsTheSame;
        procedure EveryDeclaredCoordinateIsARegisteredMode;
        procedure TheSameLoaderDeclaringOtherCoordinatesIsRefused;
    end;


implementation

type
    { A loader that exists only to be registered wrongly. Never reads anything -
      the registration rules are what is under test, not the reading. }
    TFakeLoader = class(TDataLoader)
    protected
        procedure LoadDataSetActually; override;
    end;

procedure TFakeLoader.LoadDataSetActually;
begin
    //  Deliberately empty: no test here gets far enough to load.
end;

procedure TDataLoaderRegistryTest.AKnownExtensionResolvesToItsLoader;
begin
    RegisterAllDataLoaders;
    AssertTrue('a .dat profile must resolve to the two-column reader',
        FindDataLoaderClass('sample.dat') = TDATFileLoader);
end;

{ THE TWO-COLUMN READER IS NOT ABOUT DIFFRACTION. It takes the first two numbers
  on each line and skips everything else, which is what '.xy' and the two-column
  '.txt' that repositories and spectrometers emit are. Registering the same class
  a second time - rather than writing another parser - is what keeps one answer
  to "what is a column of numbers?", and it is also what lets Stage 4 move the
  '.DAT' registration alone into a module later. }
procedure TDataLoaderRegistryTest.TwoColumnTextResolvesToTheSameReaderAsDat;
begin
    RegisterAllDataLoaders;
    AssertTrue('a .xy file must resolve to the two-column reader',
        FindDataLoaderClass('spectrum.xy') = TDATFileLoader);
    AssertTrue('and so must a two-column .txt',
        FindDataLoaderClass('spectrum.txt') = TDATFileLoader);
    AssertTrue('the filter must offer .xy',
        Pos('*.xy', DataLoaderDialogFilter) > 0);
end;

procedure TDataLoaderRegistryTest.TheLookupIgnoresCase;
begin
    RegisterAllDataLoaders;
    //  File systems differ on this and users type either; resolving only one
    //  would make the application refuse a file it can plainly read.
    AssertTrue('.DAT and .dat are the same format',
        FindDataLoaderClass('SAMPLE.DAT') = FindDataLoaderClass('sample.dat'));
end;

procedure TDataLoaderRegistryTest.AnUnknownExtensionResolvesToNothing;
begin
    RegisterAllDataLoaders;
    //  Nil, so the caller can say which extension it was in its own words. A
    //  fallback to "some loader" would report a parse failure for a file that
    //  was simply never supported.
    AssertTrue('an unsupported extension has no loader',
        FindDataLoaderClass('notes.hdf5') = nil);
end;

procedure TDataLoaderRegistryTest.RegisteringTwiceIsHarmless;
var
    Before: longint;
begin
    RegisterAllDataLoaders;
    Before := DataLoaderCount;
    //  Start-up may run more than once in a session - a re-created client, a
    //  test - and the second run must not trip the duplicate-claim rule.
    RegisterAllDataLoaders;
    AssertEquals('registering again must not add loaders',
        Before, DataLoaderCount);
end;

{ A MODULE REGISTERS ITS OWN LOADERS FROM ITS FRONT DOOR, and a front door is
  required to be idempotent: a host may call it twice, and a test usually does.
  The framework's own set is guarded by a flag inside RegisterAllDataLoaders, so
  the rule had never been asked of the registry itself - and a module that
  registered a loader would have had its SECOND call refused as a duplicate
  claim, by the guard meant to catch two DIFFERENT loaders claiming one
  extension.

  So an IDENTICAL registration - same class, same extensions, same name - is a
  no-op, and everything else about that extension is still refused. The identity
  includes the format name: two registrations differing only in what they call
  the format are two different intentions, and silently keeping the first would
  put a name in the file dialog that its loader disagrees with. }
procedure TDataLoaderRegistryTest.TheSameLoaderRegisteredAgainIsANoOp;
var
    Before: longint;
begin
    RegisterDataLoader(TFakeLoader, '.ZZ1', 'Fake format');
    Before := DataLoaderCount;
    RegisterDataLoader(TFakeLoader, '.ZZ1', 'Fake format');
    AssertEquals('an identical registration must not add a loader',
        Before, DataLoaderCount);
    AssertTrue('and the loader must still be the one that resolves',
        FindDataLoaderClass('x.zz1') = TFakeLoader);
end;

procedure TDataLoaderRegistryTest.TheSameLoaderClaimingSomethingElseIsRefused;
var
    Raised: boolean;
begin
    RegisterDataLoader(TFakeLoader, '.ZZ2', 'Fake format');
    Raised := False;
    try
        //  The same class, the same extension, a DIFFERENT name for the format.
        //  Not a repeated call: a changed one, and which of the two names the
        //  dialog would show is nobody's decision.
        RegisterDataLoader(TFakeLoader, '.ZZ2', 'Fake format, renamed');
    except
        on E: EDataLoaderRegistration do
            Raised := True;
    end;
    AssertTrue('a changed registration must be refused', Raised);
end;

function ModeOf(const AFileName: string; ADimension: TAxisDimension): string;
var
    Injector: TExtensionDataLoaderInjector;
begin
    Injector := TExtensionDataLoaderInjector.Create;
    try
        Result := Injector.CreateDataLoader(AFileName).CoordinateMode(ADimension);
    finally
        Injector.Free;
    end;
end;

procedure TDataLoaderRegistryTest.ADatFileSaysNothingSinceAnySeriesIsSavedAsOne;
begin
    //  A price series saved as '.dat' was captioned in scattering angle when
    //  the format declared 2 Theta. A diffraction model says it instead.
    RegisterAllDataLoaders;
    AssertEquals('', ModeOf('synthetic-random-walk.dat', adArgument));
    AssertEquals('', ModeOf('synthetic-random-walk.dat', adValue));
end;

procedure TDataLoaderRegistryTest.ATwoColumnFileSaysNothingThoughTheReaderIsTheSame;
begin
    //  The reason this is the REGISTRATION's to say: one reader, two formats.
    RegisterAllDataLoaders;
    AssertEquals('', ModeOf('spectrum.xy', adArgument));
    AssertEquals('', ModeOf('spectrum.txt', adValue));
end;

procedure TDataLoaderRegistryTest.EveryDeclaredCoordinateIsARegisteredMode;
var
    Loaders: TDataLoaderInfoArray;
    i: longint;
begin
    //  THE WALK. A format naming a mode no one registered would silently say
    //  nothing - the rule passes unregistered modes over - so it fails here, by
    //  format, instead.
    RegisterAllDataLoaders;
    RegisterAllAxisModes;
    Loaders := RegisteredDataLoaders;
    for i := 0 to High(Loaders) do
    begin
        if Loaders[i].ArgumentMode <> '' then
            AssertTrue(Loaders[i].FormatName + ' declares an argument mode ' +
                'nothing registered: ' + Loaders[i].ArgumentMode,
                AxisModeShows(Loaders[i].ArgumentMode, adArgument));
        if Loaders[i].ValueMode <> '' then
            AssertTrue(Loaders[i].FormatName + ' declares a value mode ' +
                'nothing registered: ' + Loaders[i].ValueMode,
                AxisModeShows(Loaders[i].ValueMode, adValue));
    end;
end;

procedure TDataLoaderRegistryTest.TheSameLoaderDeclaringOtherCoordinatesIsRefused;
var
    Raised: boolean;
begin
    RegisterDataLoader(TFakeLoader, '.ZZ3', 'Fake format', 'position', 'value');
    Raised := False;
    try
        RegisterDataLoader(TFakeLoader, '.ZZ3', 'Fake format', 'position',
            'logarithmic');
    except
        on EDataLoaderRegistration do
            Raised := True;
    end;
    AssertTrue('two intentions, not a repeat', Raised);
end;

procedure TDataLoaderRegistryTest.TwoLoadersCannotClaimTheSameExtension;
var
    Raised: boolean;
begin
    RegisterAllDataLoaders;
    Raised := False;
    try
        RegisterDataLoader(TFakeLoader, '.DAT', 'Impostor');
    except
        on E: EDataLoaderRegistration do
            Raised := True;
    end;
    //  Otherwise which loader opens a .dat file would depend on the order two
    //  uses clauses happen to be in, and the loser would be dead code that
    //  still looks installed.
    AssertTrue('a second claim on .DAT must be refused', Raised);
end;

procedure TDataLoaderRegistryTest.ALoaderMustNameAnExtension;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterDataLoader(TFakeLoader, '', 'Unreachable');
    except
        on E: EDataLoaderRegistration do
            Raised := True;
    end;
    //  A loader nothing can route to is not a working loader, and registering
    //  it quietly is how a format ends up "supported" but unopenable.
    AssertTrue('a loader claiming no extension must be refused', Raised);
end;

procedure TDataLoaderRegistryTest.TheDialogFilterOffersOnlyWhatIsRegistered;
var
    Filter: string;
begin
    RegisterAllDataLoaders;
    Filter := DataLoaderDialogFilter;
    AssertTrue('the filter must offer .dat', Pos('*.dat', Filter) > 0);
    AssertTrue('the filter must offer .xy', Pos('*.xy', Filter) > 0);
    //  The point of deriving it: the dialog used to carry its own hand-written
    //  list, so it could offer a format the build had no reader for.
    AssertTrue('the filter must not offer a format with no reader',
        Pos('*.hdf5', Filter) = 0);
end;

{ ------------------- registered with something missing ---------------------- }

{ THE REFUSALS WHOEVER ADDS A LOADER MEETS FIRST, and their only feedback: these
  fire at link time, before any window exists. The duplicate-extension refusal
  was already covered here; these were not.

  AN EXTENSION LIST IS A SEMICOLON-SEPARATED STRING, which is the part that
  invites mistakes - a trailing separator, a stray space, a list that is all
  separators. What each of those does is worth knowing, because a loader with no
  reachable extension is installed and unreachable, and nothing at run time says
  so. }

procedure TDataLoaderRegistryTest.ALoaderWithNoClassIsRefused;
var
    Raised: boolean;
begin
    //  A registration naming extensions and no class would put those extensions
    //  beyond the reach of any other loader - claimed by nothing - so every file
    //  of that kind would then fail to open with "no loader" while the registry
    //  insists one is installed.
    Raised := False;
    try
        RegisterDataLoader(nil, '.zzz', 'Nothing at all');
    except
        on E: EDataLoaderRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TDataLoaderRegistryTest.ALoaderNamingOnlyEmptyExtensionsIsRefused;
var
    Raised: boolean;
begin
    //  ';;' IS A LIST OF NOTHING. Accepted, the loader is installed and no file
    //  name can ever resolve to it - which looks exactly like a loader that is
    //  present and simply never chosen.
    Raised := False;
    try
        RegisterDataLoader(TFakeLoader, ';;', 'Only separators');
    except
        on E: EDataLoaderRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TDataLoaderRegistryTest.AnEmptyExtensionAmongGoodOnesIsSkipped;
begin
    //  A TRAILING SEPARATOR IS ORDINARY, and it must not cost the registration.
    //  Refusing the whole list because it ends in ';' would make a harmless typo
    //  a link-time failure; skipping the empty entry keeps the real ones.
    RegisterDataLoader(TFakeLoader, '.aa1;;.aa2;', 'With an empty entry');
    AssertTrue('the first extension resolves',
        FindDataLoaderClass('x.aa1') = TFakeLoader);
    AssertTrue('and so does the one after the empty entry',
        FindDataLoaderClass('x.aa2') = TFakeLoader);
end;

procedure TDataLoaderRegistryTest.AnExtensionWithoutItsDotStillMatches;
begin
    //  THE DOT IS SUPPLIED IF IT IS MISSING, because a caller writing 'txt' and
    //  a caller writing '.txt' plainly mean the same thing - and a registry that
    //  distinguished them would install a loader nothing could reach, with the
    //  registration looking perfectly correct.
    RegisterDataLoader(TFakeLoader, 'aa3', 'No leading dot');
    AssertTrue('registered without a dot, found with one',
        FindDataLoaderClass('x.aa3') = TFakeLoader);
end;

initialization
    RegisterTest('unit', TDataLoaderRegistryTest);
end.
