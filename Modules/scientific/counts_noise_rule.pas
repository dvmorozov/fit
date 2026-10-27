// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The noise of counts: the rule every curve type of this module names.)

A diffraction or spectroscopy profile is a count of photons, neutrons or
electrons per channel. A count's variance grows in proportion to the count
(Poisson), and a detector or a background taken off adds a constant part:
variance = a + b * level, the Poisson-Gaussian model of raw counting data
(Foi et al. 2008). Measured from the data by fit_reduction.NoiseVariancesOf -
the framework's straight-line tool - over the whole profile.

The fit's displayed figures keep weighing each difference by the square root
of the value, as they always have for counts (WeighsFiguresAsCounts); the
verdict, the Difference structure strip, the scalogram and the reduction by
BIC divide by the measured line, which a background taken off - the copy
Fit > Automatically reduces - cannot falsify.
}
unit counts_noise_rule;

{$mode objfpc}{$H+}

interface

uses
    explanation, fit_reduction, noise_rule;

type
    TCountsNoiseRule = class(TNoiseRule)
    public
        class function Id: string; override;
        class function Variances(const AY: array of double): TNoiseVariances;
            override;
        class function WeighsFiguresAsCounts: boolean; override;
        class function Explanation: TExplanation; override;
    end;

implementation

class function TCountsNoiseRule.Id: string;
begin
    Result := 'counts';
end;

class function TCountsNoiseRule.Variances(
    const AY: array of double): TNoiseVariances;
begin
    Result := NoiseVariancesOf(AY);
end;

class function TCountsNoiseRule.WeighsFiguresAsCounts: boolean;
begin
    Result := True;
end;

class function TCountsNoiseRule.Explanation: TExplanation;
begin
    Result := NewExplanation('', 'The noise of counts',
        'A count''s noise grows with the count: its variance is a straight ' +
        'line in the measured value, a + b * value, measured from how much ' +
        'neighbouring points scatter.', esConvention);
    AddParagraph(Result, 'A diffraction or spectroscopy profile counts ' +
        'photons, neutrons or electrons, and a count''s variance is the count ' +
        'itself (Poisson). A detector''s own noise, or a background that has ' +
        'been taken off, adds a part that does not grow: so the variance is a ' +
        'line in the value, the Poisson-Gaussian model of raw counting data.');
    AddParagraph(Result, 'It is measured from the data alone. Three ' +
        'neighbouring points y1, y2, y3 give y1 - 2 y2 + y3, which a smooth ' +
        'curve hardly reaches and independent noise of variance s^2 reaches ' +
        'with variance 6 s^2. The points are sorted by value into up to ten ' +
        'groups; in each, the median of those squares over six, over 0.4549 ' +
        '(the median of a chi-square of one degree), is the group''s variance, ' +
        'and a line through the groups - its slope never below zero - gives a ' +
        'and b. The median keeps a peak''s few steep points from carrying it.');
    AddParagraph(Result, 'Every curve type of a diffraction or spectroscopy ' +
        'profile names this rule. The verdict on the difference, its shaded ' +
        'stretches, the Difference structure strip, the Residual Spectrum and ' +
        'Fit > Remove Curves By > BIC divide each difference by this noise; ' +
        'the reduced chi-squared, AIC and BIC shown after a fit weigh it by ' +
        'the square root of the value, counting statistics, as they always ' +
        'have.');
    AddLimitation(Result, 'A peak only a few points wide adds its own ' +
        'curvature to the scatter, so its noise reads somewhat high.');
    AddLimitation(Result, 'Data whose noise grows faster than the value - a ' +
        'price, whose scatter is a share of itself - are not counts: the line ' +
        'then falls below zero at small values, where it is held at a quarter ' +
        'of the smallest group''s variance, far too small. A curve type for ' +
        'such data names a rule of its own.');
    AddLimitation(Result, 'It assumes the noise of neighbouring points is ' +
        'independent.');
    AddReference(Result, 'Foi, A., Trimeche, M., Katkovnik, V. and ' +
        'Egiazarian, K., "Practical Poissonian-Gaussian noise modeling and ' +
        'fitting for single-image raw-data", IEEE Transactions on Image ' +
        'Processing 17 (10), 1737-1754 (2008)', '',
        'https://doi.org/10.1109/TIP.2008.2001399');
end;

end.
