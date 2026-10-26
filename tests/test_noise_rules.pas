// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Noise rules for the tests' own curve types.)

Every curve type names the rule its field's noise is measured by
(TNamedPointsSet.NoiseRule), and the framework ships none - so a curve type a
test defines names one of these. TTestNoiseRule is complete but for Variances,
so a probe states only the noise it is about.
}
unit test_noise_rules;

{$mode objfpc}{$H+}

interface

uses
    explanation, fit_reduction, noise_rule;

type
    { A test's rule: an id from its class name and a complete explanation. }
    TTestNoiseRule = class(TNoiseRule)
    public
        class function Id: string; override;
        class function Explanation: TExplanation; override;
    end;

    { Counts, as the framework's straight-line tool measures them, and the
      figures weighed as counts. }
    TTestCountsNoiseRule = class(TTestNoiseRule)
    public
        class function Variances(const AY: array of double): TNoiseVariances;
            override;
        class function WeighsFiguresAsCounts: boolean; override;
    end;

implementation

uses
    SysUtils;

class function TTestNoiseRule.Id: string;
begin
    Result := 'test-' + LowerCase(ClassName);
end;

class function TTestNoiseRule.Explanation: TExplanation;
begin
    Result := NewExplanation('', ClassName,
        'A noise rule a test defines for its own curve type.', esModelChoice);
    AddParagraph(Result, 'It exists so a test''s curve type names a rule, as ' +
        'every curve type does.');
    AddLimitation(Result, 'It is a test''s, not a field''s.');
end;

class function TTestCountsNoiseRule.Variances(
    const AY: array of double): TNoiseVariances;
begin
    Result := NoiseVariancesOf(AY);
end;

class function TTestCountsNoiseRule.WeighsFiguresAsCounts: boolean;
begin
    Result := True;
end;

end.
