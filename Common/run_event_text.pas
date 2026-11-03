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
    SysUtils, Math, fit_progress_json, residual_randomness;

const
    { Who decided, for the framework's own decisions; a module names itself. }
    RunEventFramework = 'framework';
    { The headings of the log. }
    RunSectionReduction = 'Reduction';
    RunSectionResult = 'Result';
    { The heading every stage of a run is logged under, run or skipped. }
    RunSectionStages = 'Stages';
    { The guide topic the stages are explained in (guide_fitting). }
    RunStagesTopic = 'fitting/the-stages-of-a-run';

type
    { The stages a run may go through, in the order a run takes them. }
    TRunStage = (
        { Where the curves start: with a background, one pick on each sample of
        the fit intervals; without, the peaks found by the peak search. }
        rsPeakSearch,
        { Where the data stand above the background: the fit intervals. }
        rsIntervalSearch,
        { The background fitted to the floor of the data, peaks left out. }
        rsBackgroundFloor,
        { Curves that pay for nothing taken away. }
        rsReduction,
        { The peaks of every fit interval fitted. }
        rsPeakFit,
        { The background and the intervals fitted in turns. }
        rsBackgroundTurns);

{ A curve taken away because BIC is lower without it. AName is its type;
  AWhere its position, when AHasWhere. }
function CurveRemovedEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout: double;
    const AFigures: string = ''): TRunEvent;

{ A curve BIC would have taken away, kept because the difference where it
  matters is random with it (APWith) and not without it (APWithout). }
function CurveKeptAsPatternEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout, APWith, APWithout: double;
    const AFigures: string = ''): TRunEvent;

{ THE FIGURES A BIC REMOVAL WAS JUDGED BY, in words: the chi-square of the
  model with the curve and without it (the residual over the noise measured in
  this fit interval), what the removal saves in parameters (APars of
  ln(APoints) each), and the noise there, as a standard deviation. The
  difference of the two chi-squares against the saving is the whole decision,
  so a removal that looks wrong can be seen to be wrong - or right. }
function BicFiguresText(AChiWith, AChiWithout: double;
    AParsWith, AParsWithout, APoints: longint; ASigma: double): string;

{ A curve taken away because it describes nothing the noise could not
  (TNamedPointsSet.DescribesOnlyNoise). }
function CurveRemovedAsNoiseEvent(const AName: string; AHasWhere: boolean;
    AWhere: double): TRunEvent;

{ What the run left: whether the difference is only noise. }
function RunVerdictEvent(const AR: TResidualRandomness): TRunEvent;

{ The stage AStage's name, as the Run Log and the guide write it. }
function RunStageName(AStage: TRunStage): string;

{ AStage ran; ADetail says what it found or did, or is empty. }
function StageRanEvent(AStage: TRunStage; const ADetail: string): TRunEvent;

{ AStage did not run, for AReason - which is never empty: a stage left out
  without a word is a run the user cannot follow. }
function StageSkippedEvent(AStage: TRunStage; const AReason: string): TRunEvent;

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

function RunStageName(AStage: TRunStage): string;
begin
    case AStage of
        rsPeakSearch: Result := 'Finding the peaks';
        rsIntervalSearch: Result := 'Finding the fit intervals';
        rsBackgroundFloor: Result := 'Fitting the background to the floor of the data';
        rsReduction: Result := 'Removing curves that pay for nothing';
        rsPeakFit: Result := 'Fitting the peaks';
        rsBackgroundTurns: Result := 'Fitting the background in turns';
    end;
end;

function StageRanEvent(AStage: TRunStage; const ADetail: string): TRunEvent;
begin
    Result := NewEvent(RunSectionStages, RunStagesTopic, RunStageName(AStage),
        ADetail);
end;

function StageSkippedEvent(AStage: TRunStage; const AReason: string): TRunEvent;
begin
    if Trim(AReason) = '' then
        raise Exception.Create('a skipped stage says why: ' +
            RunStageName(AStage));
    Result := NewEvent(RunSectionStages, RunStagesTopic,
        RunStageName(AStage) + ' - skipped', AReason);
end;

function BicFiguresText(AChiWith, AChiWithout: double;
    AParsWith, AParsWithout, APoints: longint; ASigma: double): string;
var
    ChiRise: string;
begin
    //  A sign printed by hand: Format's own has no plus.
    ChiRise := Format('%.1f', [AChiWithout - AChiWith]);
    if AChiWithout >= AChiWith then
        ChiRise := '+' + ChiRise;
    Result := Format(' Chi-square %.1f with it and %.1f without (%s); ' +
        'its %d parameter(s) are worth %.1f of BIC over %d points; noise here ' +
        '%.2f.', [AChiWith, AChiWithout, ChiRise,
        AParsWith - AParsWithout, (AParsWith - AParsWithout) *
        Ln(Max(APoints, 1)), APoints, ASigma]);
end;

function CurveRemovedEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout: double;
    const AFigures: string): TRunEvent;
begin
    Result := NewEvent(RunSectionReduction, 'fitting/how-many-curves',
        Named(AName, AHasWhere, AWhere) + ' removed',
        Format('BIC %.1f with it, %.1f without: what it described of the ' +
        'data was not worth its parameters, so the model is better without ' +
        'it.', [ABicWith, ABicWithout]) + AFigures);
end;

function CurveKeptAsPatternEvent(const AName: string; AHasWhere: boolean;
    AWhere, ABicWith, ABicWithout, APWith, APWithout: double;
    const AFigures: string): TRunEvent;
begin
    Result := NewEvent(RunSectionReduction, 'fitting/is-the-difference-noise',
        Named(AName, AHasWhere, AWhere) + ' kept',
        Format('BIC was lower without it (%.1f with it, %.1f without), but ' +
        'where it matters the difference between the data and the model is ' +
        'noise with it (Ljung-Box p = %.2g) and is not without it (p = %.2g): ' +
        'taking it away would leave a pattern the model no longer describes.',
        [ABicWith, ABicWithout, APWith, APWithout]) + AFigures);
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
