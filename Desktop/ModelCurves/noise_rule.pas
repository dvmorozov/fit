// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How the noise of a field's data is measured: the contract every
curve type names an implementation of.)

WHY EVERY CURVE TYPE NAMES ONE. Whether a difference between the data and the
model is noise is a question about the noise, and the noise is the field's: a
count's variance grows with the count, a price's with the square of the price.
The framework knows neither. Each curve type therefore names the rule of its
field (TNamedPointsSet.NoiseRule) - by construction: a type that names none is
refused when it is registered and when an instance is made - and everything
that weighs a difference by the noise asks that rule: the fit's figures, the
reduction's BIC, the verdict and the shaded stretches, the Difference
structure strip and a module's scalogram. Judged by one field's rule whatever
the type said, a century of the S&P 500 had its quiet years named as structure
and its loud ones read as noise (findings.md, "The Difference structure strip
judged a price as counts").

A RULE EXPLAINS ITSELF (non-negotiable 10): it is a topic of the noise-rule
namespace (curve_type_explanations), reached from every curve type that names
it, with its standing, limitations and sources.

The framework ships no rule. fit_reduction.NoiseVariancesOf is a numerical
tool - a straight line in the level through the scatter of neighbouring
points - which a field's rule may use or not; which data it describes is the
rule's claim.
}
unit noise_rule;

{$mode objfpc}{$H+}

interface

uses
    explanation, fit_reduction;

const
    NoiseRuleNamespace = 'noise-rule';

type
    { A field's rule for the noise of its data. Never instantiated: every
      member is a class function, as a curve type's are. }
    TNoiseRule = class
    public
        { What names the rule in its topic, 'noise-rule/<Id>': lower case,
          unique among the rules a build links. }
        class function Id: string; virtual; abstract;
        { The variance of the noise at each point of AY - the whole profile -
          one per point. }
        class function Variances(const AY: array of double): TNoiseVariances;
            virtual; abstract;
        { Whether the fit's displayed figures - reduced chi-squared, AIC and
          BIC - weigh each difference as counting statistics, by the square root
          of the value, as they always have for counts; otherwise they divide
          by Variances. The verdict, the strip and the reduction use Variances
          either way. False by default: a rule that measures says so. }
        class function WeighsFiguresAsCounts: boolean; virtual;
        { What the rule is, why it holds for its field, its standing, its
          limits and its sources. The topic and, when empty, the title are
          stamped by the provider. }
        class function Explanation: TExplanation; virtual; abstract;
    end;

    TNoiseRuleClass = class of TNoiseRule;

{ 'noise-rule/<Id>'. }
function NoiseRuleTopic(ARule: TNoiseRuleClass): string;

implementation

class function TNoiseRule.WeighsFiguresAsCounts: boolean;
begin
    Result := False;
end;

function NoiseRuleTopic(ARule: TNoiseRuleClass): string;
begin
    Result := NoiseRuleNamespace + '/' + ARule.Id;
end;

end.
