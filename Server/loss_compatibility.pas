// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which objectives may be used with which models.)

ONE rule, in ONE place, derived from CAPABILITIES rather than from a table of
type names (D18):

    a self-normalising loss may not be used with a model whose amplitude is
    unbounded.

Both halves are properties the participants already declare about themselves -
fit_loss.LossIsSelfNormalising and TNamedPointsSet.AmplitudeIsUnbounded - so a
seventh pattern type, or a fifth loss function, becomes compatible or
incompatible automatically. Nothing here enumerates anything, which is the whole
point: the enumerated version rots the moment someone adds a type and does not
find this file.

WHY THE RULE. A loss normalised by the MODEL's integral is reduced by inflating
the model, because scaling leaves the numerator alone and grows the denominator.
That is not a fit improving; it is the measure being gamed. Peak types never
went there - their amplitude is seeded from the data - so the legacy R-factor was
safe for 25 years of diffraction work and stays the default. A curve whose
amplitude is free finds the degenerate direction immediately, so the pairing is
refused rather than silently producing a confident wrong answer.

Enforced in BOTH places, because they fail differently: the UI disables what
cannot be chosen (so the user is never offered a dead end), and the engine
refuses what it is nonetheless asked to do (because a client is not to be
trusted, and because the demo runner and tests reach the engine directly).

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit loss_compatibility;

{$mode objfpc}{$H+}

interface

uses
    fit_loss, SysUtils;

{ The rule itself, stated over the two capabilities alone so it can be tested
  exhaustively without constructing a single curve. }
function LossAllowedForCapability(ALossKind: longint;
    AAmplitudeIsUnbounded: boolean): boolean;

{ The objective to fall back to when the selected one is not allowed. Chosen as
  the corrected R-factor rather than plain least squares: it is the same measure
  the user asked for, only normalised by the data instead of by the model, so
  the fallback preserves their intent. }
function DefaultLossFor(AAmplitudeIsUnbounded: boolean): longint;

{ Why a pairing was refused, for the log, the hint and the server's error. }
function LossRefusalReason(ALossKind: longint): string;

implementation

function LossAllowedForCapability(ALossKind: longint;
    AAmplitudeIsUnbounded: boolean): boolean;
begin
    Result := not (LossIsSelfNormalising(ALossKind) and AAmplitudeIsUnbounded);
end;

function DefaultLossFor(AAmplitudeIsUnbounded: boolean): longint;
begin
    if AAmplitudeIsUnbounded then
        Result := LOSS_KIND_RFACTOR
    else
        Result := LOSS_KIND_RFACTOR_LEGACY;
end;

function LossRefusalReason(ALossKind: longint): string;
begin
    Result := Format(
        '"%s" is normalised by the model itself, so a model whose amplitude is ' +
        'free to grow can lower it by growing rather than by fitting the data. ' +
        'Use "%s", which normalises by the data instead.',
        [LossName(ALossKind), LossName(LOSS_KIND_RFACTOR)]);
end;

end.
