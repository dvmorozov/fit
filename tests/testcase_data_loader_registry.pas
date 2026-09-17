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
    dat_file_loader, ohlc_csv_loader;

type
    TDataLoaderRegistryTest = class(TTestCase)
    published
        procedure AKnownExtensionResolvesToItsLoader;
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
    AssertTrue('a .csv file must resolve to the OHLC reader',
        FindDataLoaderClass('prices.csv') = TOHLCFileLoader);
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
    AssertTrue('the filter must offer .csv', Pos('*.csv', Filter) > 0);
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
