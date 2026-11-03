// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How a residual is weighted: the two names, and what an unrecognised
one means.)

WEIGHTING IS WHAT TURNS A CURVE INTO A RESULT WITH MEANINGFUL UNCERTAINTIES, so
getting it wrong does not fail - it answers a different question and reports the
answer with the same confidence. Counting data wants 1/sqrt(counts); unweighted
is for data that is not counts.

IT LIVES IN Common/ FOR THE SAME REASON rest_polling does: the setting is
persisted by the desktop client, carried on the wire, held by the service and
the task, and finally interpreted by the Python sidecar - six places, in two
processes, that must not disagree about what a value means. Each of those used
to write the name 'poisson' as a bare literal and decide the default for itself.

THE RULE IS THE SIDECAR'S AND IS COPIED HERE DELIBERATELY. Worker/py/fitting.py
reads it as

    if kind == "none": unweighted
    else:              poisson

so anything that is not EXACTLY the four lower-case letters is poisson -
including the empty string, which is what a settings file written before this
setting existed carries, and including 'None'. WeightingIsUnweighted is that
same test, case-sensitive to match, and the tests say so: a Pascal side that was
lenient where Python is strict would read 'None' as unweighted while the backend
fitting it read it as poisson, and the two halves of one fit would minimise
different things.

WHICH IS WHY NORMALISING AT THE BOUNDARY IS THE REAL PROTECTION. Everything
stored or sent goes through WeightingOrDefault, so only the two canonical names
ever reach a settings file or the wire and the strictness above can never be
reached by accident - only by a value this program did not write.
}
unit fit_weighting;

{$MODE Delphi}

interface

const
    { 1/sqrt(max(counts,1)) - counting statistics, and the right default for the
      diffraction profiles this program was written for. }
    WEIGHTING_POISSON = 'poisson';
    { Unweighted: every point contributes equally, for data that is not counts. }
    WEIGHTING_NONE = 'none';

{ True when AWeighting asks for an unweighted fit.

  EXACT, LOWER CASE, matching Worker/py/fitting.py. See the note above: the
  strictness is shared on purpose, not an oversight. }
function WeightingIsUnweighted(const AWeighting: string): boolean;

{ AWeighting reduced to one of the two names above. Anything unrecognised - the
  empty string most of all, which is every settings file older than the setting
  - becomes the poisson default, which is what both engines already do with it. }
function WeightingOrDefault(const AWeighting: string): string;

{ True when AWeighting is one of the two names exactly as written. Nothing in
  the program refuses a value on this basis; it is here so a test can state
  which strings are canonical without repeating them. }
function IsCanonicalWeighting(const AWeighting: string): boolean;

implementation

function WeightingIsUnweighted(const AWeighting: string): boolean;
begin
    Result := AWeighting = WEIGHTING_NONE;
end;

function WeightingOrDefault(const AWeighting: string): string;
begin
    if WeightingIsUnweighted(AWeighting) then
        Result := WEIGHTING_NONE
    else
        Result := WEIGHTING_POISSON;
end;

function IsCanonicalWeighting(const AWeighting: string): boolean;
begin
    Result := (AWeighting = WEIGHTING_POISSON) or (AWeighting = WEIGHTING_NONE);
end;

end.
