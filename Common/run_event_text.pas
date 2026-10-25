// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the Run Log says of each decision the framework makes.)

THE APP EXPLAINS ITSELF (non-negotiable 10): a run that removed a curve says
which, by what rule, with the numbers the rule compared, and names the guide
topic the rule is explained in. Worded here, over plain values, so every line
is a function a test reads; the engine only says when.
}
unit run_event_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_progress_json, residual_randomness;

const
    { Who decided, for the framework's own decisions; a module names itself. }
    RunEventFramework = 'framework';
    { The headings of the log. }
    RunSectionReduction = 'Reduction';
    RunSectionResult = 'Result';

{ A curve taken away because BIC is lower without it. AName is its type;
  AWhere its position, when AHasWhere. }
function CurveRemovedEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout: double): TRunEvent;

{ A curve BIC would have taken away, kept because the difference where it
  matters is random with it (APWith) and not without it (APWithout). }
function CurveKeptAsPatternEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout, APWith, APWithout: double): TRunEvent;

{ A curve taken away because it describes nothing the noise could not
  (TNamedPointsSet.DescribesOnlyNoise). }
function CurveRemovedAsNoiseEvent(const AName: string; AHasWhere: boolean;
    AWhere: double): TRunEvent;

{ What the run left: whether the difference is only noise. }
function RunVerdictEvent(const AR: TResidualRandomness): TRunEvent;

implementation

function Named(const AName: string; AHasWhere: boolean; AWhere: double): string;
begin
    if AHasWhere then
        Result := Format('%s at %s', [AName, FloatToStrF(AWhere, ffGeneral,
            6, 0)])
    else
        Result := AName;
end;

function NewEvent(const ASection, ATopic, ATitle, ADetail: string): TRunEvent;
begin
    Result := Default(TRunEvent);
    Result.Source := RunEventFramework;
    Result.Section := ASection;
    Result.Topic := ATopic;
    Result.Title := ATitle;
    Result.Detail := ADetail;
end;

function CurveRemovedEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout: double): TRunEvent;
begin
    Result := NewEvent(RunSectionReduction, 'fitting/how-many-curves',
        Named(AName, AHasWhere, AWhere) + ' removed',
        Format('BIC %.1f with it, %.1f without: what it described of the ' +
        'data was not worth its parameters, so the model is better without ' +
        'it.', [ABicWith, ABicWithout]));
end;

function CurveKeptAsPatternEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout, APWith, APWithout: double): TRunEvent;
begin
    Result := NewEvent(RunSectionReduction, 'fitting/is-the-difference-noise',
        Named(AName, AHasWhere, AWhere) + ' kept',
        Format('BIC was lower without it (%.1f with it, %.1f without), but ' +
        'where it matters the difference between the data and the model is ' +
        'noise with it (Ljung-Box p = %.2g) and is not without it (p = %.2g): ' +
        'taking it away would leave a pattern the model no longer describes.',
        [ABicWith, ABicWithout, APWith, APWithout]));
end;

function CurveRemovedAsNoiseEvent(const AName: string; AHasWhere: boolean;
    AWhere: double): TRunEvent;
begin
    Result := NewEvent(RunSectionReduction, 'fitting/how-many-curves',
        Named(AName, AHasWhere, AWhere) + ' removed: only noise',
        'As the fit left it, it describes nothing the noise of the data could ' +
        'not produce by chance, so it was taken away whatever BIC said.');
end;

function RunVerdictEvent(const AR: TResidualRandomness): TRunEvent;
var
    Detail: string;
begin
    if ResidualIsRandom(AR) then
        Detail := Format('Tested over each fit interval, and at each of its ' +
            '%d sample(s) for rhythms of every length: the difference between ' +
            'the data and the model reads as noise.', [AR.Samples])
    else if Length(AR.NonRandomRanges) > 0 then
        Detail := Format('Tested over each fit interval, and at each of its ' +
            '%d sample(s) for rhythms of every length: %d stretch(es) of the ' +
            'difference are not noise - the model has something left to ' +
            'describe there. They are shaded on the chart.',
            [AR.Samples, Length(AR.NonRandomRanges)])
    else
        Detail := Format('Tested over each fit interval: the difference is ' +
            'not noise (p = %.2g), though no place in it is singled out.',
            [AR.P]);
    Result := NewEvent(RunSectionResult, 'fitting/is-the-difference-noise',
        ResidualVerdictText(AR), Detail);
end;

end.
