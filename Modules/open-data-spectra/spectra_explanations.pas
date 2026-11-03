// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What this module puts in front of a user, and what each thing is.)

EVERYTHING A MODULE ADDS EXPLAINS ITSELF - a source, a file format, a refusal.
Each topic here says what the thing is, what it rests on (a published standard,
a service's own pages, or this software's choice), what it does NOT cover, and
where to read more. The registry walk fails a source registered without one, so
this file is not optional documentation: it is part of the contribution.
}
unit spectra_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, explanation_registry, static_explanations;

const
    SpectraNamespace = 'spectra';
    NistSourceTopic = 'spectra/source-nist';
    JcampFormatTopic = 'spectra/jcamp-dx';
    SpcFormatTopic = 'spectra/spc';

function SpectraExplanations: TExplanations;
procedure RegisterSpectraExplanations;

implementation

var
    Provider: TStaticExplanationProvider = nil;

function NistSource: TExplanation;
begin
    Result := NewExplanation(NistSourceTopic, 'NIST Chemistry WebBook',
        'The NIST WebBook source finds a substance by name and fetches the ' +
        'infrared, mass or UV-visible spectrum NIST publishes for it.',
        esModelChoice);
    AddParagraph(Result, 'Type a substance''s name. When it matches one ' +
        'substance, its spectra are listed; when it matches several, the ' +
        'matches are listed and opening one shows its spectra. Each spectrum ' +
        'is downloaded as a JCAMP-DX file and read like any other data file.');
    AddParagraph(Result, 'The NIST Chemistry WebBook is a public reference ' +
        'collection of the US National Institute of Standards and Technology. ' +
        'Fit fetches what a user asks for and nothing else: it does not ' +
        'search the collection for a match to a measured spectrum, identify a ' +
        'substance or interpret what it downloads.');
    AddLimitation(Result, 'The WebBook publishes no programming interface, so ' +
        'Fit reads its ordinary pages. If those pages change, this source ' +
        'stops finding spectra and says so rather than guessing.');
    AddLimitation(Result, 'Only infrared, mass and UV-visible spectra are ' +
        'fetched. The WebBook holds much else - thermochemistry, ion ' +
        'energetics - which is not data of the kind this program fits.');
    AddLimitation(Result, 'NIST states the conditions under which its data ' +
        'may be used, and some spectra are contributed by others under their ' +
        'own terms. A download is the user''s own use of that service.');
    AddReference(Result, 'NIST Chemistry WebBook, NIST Standard Reference ' +
        'Database Number 69', 'doi:10.18434/T4D303',
        'https://webbook.nist.gov/chemistry/');
    AddRelated(Result, JcampFormatTopic);
end;

function JcampFormat: TExplanation;
begin
    //  CANONICAL: the file format is defined by a published standard, and the
    //  rules this reader follows are quoted from it rather than chosen here.
    Result := NewExplanation(JcampFormatTopic, 'JCAMP-DX files',
        'JCAMP-DX is the published interchange format for spectra, and Fit ' +
        'reads its x-y data as an ordinary profile.', esCanonical);
    Result.Quote := 'The data form (X++(Y..Y)) is used for equally spaced ' +
        'data... The ASDF forms SQZ, DIF and DUP compress the Y values.';
    AddParagraph(Result, 'A JCAMP-DX file states the first x value, the step ' +
        'between points and the factors its numbers are scaled by, then the ' +
        'numbers themselves - which may be written plainly, packed, or ' +
        'compressed as differences and repeats. Fit reads all of those forms, ' +
        'and peak tables of x-y pairs.');
    AddParagraph(Result, 'The scaling matters: ignoring the factors gives a ' +
        'spectrum of the right shape and the wrong size. A file that states ' +
        'neither a step nor the values one could be worked out from is ' +
        'refused rather than read with a guessed axis.');
    AddLimitation(Result, 'A file holding several spectra in one block reads ' +
        'only its first spectrum, and says so. Joining them would make one ' +
        'curve whose x values run backwards in the middle.');
    AddLimitation(Result, 'Only the x-y data is read. Metadata such as the ' +
        'instrument, the state of the sample and the owner is not kept with ' +
        'the profile.');
    AddReference(Result, 'McDonald R. S., Wilks P. A., JCAMP-DX: A Standard ' +
        'Form for Exchange of Infrared Spectra in Computer Readable Form',
        'Applied Spectroscopy 42(1), 151-162, 1988',
        'https://doi.org/10.1366/0003702884428562');
    AddRelated(Result, NistSourceTopic);
end;

function SpcFormat: TExplanation;
begin
    //  A CONVENTION, not canonical: the layout is Galactic's published one,
    //  but its text is not quoted here, and a rule that cannot quote its
    //  source does not call itself canonical.
    Result := NewExplanation(SpcFormatTopic, 'SPC files',
        'SPC is the binary file Galactic''s and Thermo''s spectroscopy ' +
        'software writes, and Fit reads its spectrum as an ordinary profile.',
        esConvention);
    AddParagraph(Result, 'The x values run evenly from the first to the last ' +
        'the header names, or are read from the array the file carries when ' +
        'the spectrum is not evenly spaced. The y values are scaled by the ' +
        'header''s exponent as the specification says, so a spectrum stored ' +
        'as integers comes out in the units it was measured in: 2 to the ' +
        'exponent times the integer, divided by 2 to the 32 (or 16 for 16-bit ' +
        'data), and floating-point values as they are when the exponent is ' +
        '80h.');
    AddLimitation(Result, 'Only the first spectrum of a file holding several ' +
        'is read, and a file whose spectra each have their own x values is ' +
        'refused, saying so.');
    AddLimitation(Result, 'Only the new format, written least significant ' +
        'byte first, is read; the old format and the big-endian one are ' +
        'refused by name. Export such a file as JCAMP-DX.');
    AddLimitation(Result, 'The axis units, the memo and the log block are not ' +
        'kept with the profile.');
    AddReference(Result, 'Galactic Industries Corporation, the SPC file ' +
        'format specification (SPC.H)', 'the new-format header, version 4Bh',
        '');
    AddRelated(Result, JcampFormatTopic);
end;

function SpectraExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, NistSource);
    AppendExplanation(Result, JcampFormat);
    AppendExplanation(Result, SpcFormat);
end;

procedure RegisterSpectraExplanations;
begin
    if not Assigned(Provider) then
        Provider := TStaticExplanationProvider.Create(SpectraNamespace,
            @SpectraExplanations);
    RegisterExplanationProvider(Provider);
end;

finalization
    Provider.Free;
end.
