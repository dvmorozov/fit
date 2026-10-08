// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A report a module contributes: its verdicts on what the user built.)

WHY THIS EXISTS. A module may judge a whole model against the rules of its field
and owe the user the complete answer - every rule, what was measured, whether it
passed, and why the rule exists. An explanation (explanation.pas) is one topic's
teaching; a status line is one sentence. Neither is a list of verdicts. This is.

IT IS DATA, NOT A WIDGET, like every other module contribution (int_ui_host): the
module fills records and the host decides how a report looks. Nothing here names
a field. A report is sections of findings; a finding has a status, a measured
value and a limit as the module wrote them, and a TOPIC - the explanation of the
rule it applies, resolved through the one explanation registry.

ZERO IS INFORMATION. rpInfo is the enum's first value on purpose: a finding left
unconfigured must not read as a pass, and certainly not as a failure.

WHY A STANDING IS NOT A FIELD HERE. Whether a rule is the field's canon, a
convention or this software's choice belongs to the rule's explanation, which
must quote its source to claim canon. A finding carrying its own standing could
claim authority no explanation backs, so the host reads it from the topic.
}
unit module_report_types;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation_focus;

type
    TReportStatus = (
        //  Neither a pass nor a failure: context, or a rule that does not apply.
        rpInfo,
        rpPass,
        //  Unusual rather than wrong - a guideline missed.
        rpWarning,
        //  A rule is broken.
        rpFail
        );

    TReportFinding = record
        Status: TReportStatus;
        { The module's own word for what kind of check this is - 'rule',
          'guideline', 'structure'. Shown, never interpreted. }
        Kind: string;
        { What was checked, and the module's sentence about this case. }
        Title: string;
        Detail: string;
        { Already formatted by the module, which alone knows the units. }
        Measured: string;
        Limit: string;
        { The explanation of the rule applied; '' for none. }
        Topic: string;
        { The row of the host's model panel this finding is about; '' for none. }
        RowId: string;
    end;

    TReportFindings = array of TReportFinding;

    TReportSection = record
        Title: string;
        Topic: string;
        RowId: string;
        { Nesting, for a model whose parts contain parts. }
        Indent: longint;
        { The part claims a parent the model does not have. }
        IsDetached: boolean;
        Findings: TReportFindings;
    end;

    TReportSections = array of TReportSection;

    TModuleReport = record
        { The contributing module's name, which is what a host keys it by. }
        Module: string;
        Title: string;
        { The overall answer, in the module's words and as a status. }
        VerdictText: string;
        Verdict: TReportStatus;
        Summary: string;
        { Where this report comes from when that is not "now" - a record kept
          in a saved file, say. '' for a report that is simply current. }
        Provenance: string;
        { Findings about the whole model rather than one part of it. }
        Header: TReportFindings;
        Sections: TReportSections;
    end;

function NewReportFinding(AStatus: TReportStatus; const AKind, ATitle,
    ADetail, ATopic: string): TReportFinding;

{ True when the report holds no finding and no section: nothing to show. }
function ReportIsEmpty(const AReport: TModuleReport): boolean;

{ A status in a word, so no report depends on colour to be read. }
function ReportStatusCaption(AStatus: TReportStatus): string;

{ Every topic the report links to, each once, in the order they appear: the
  header's findings, then each section's own topic followed by its findings'. }
function ReportTopics(const AReport: TModuleReport): TStringArray;

{ Every way the report fails to explain itself, one sentence each: a failure or
  a warning that links to no explanation, and any topic AResolves does not
  resolve. AResolves may be nil, which checks only the missing topics. Empty for
  a well-formed report. }
function ModuleReportFindings(const AReport: TModuleReport;
    AResolves: TTopicResolves): TStringArray;

implementation

procedure Append(var AItems: TStringArray; const AText: string);
begin
    SetLength(AItems, Length(AItems) + 1);
    AItems[High(AItems)] := AText;
end;

function NewReportFinding(AStatus: TReportStatus; const AKind, ATitle,
    ADetail, ATopic: string): TReportFinding;
begin
    Result := Default(TReportFinding);
    Result.Status := AStatus;
    Result.Kind := AKind;
    Result.Title := ATitle;
    Result.Detail := ADetail;
    Result.Topic := ATopic;
end;

function ReportIsEmpty(const AReport: TModuleReport): boolean;
begin
    Result := (Length(AReport.Header) = 0) and (Length(AReport.Sections) = 0);
end;

function ReportStatusCaption(AStatus: TReportStatus): string;
begin
    case AStatus of
        rpPass:    Result := 'Pass';
        rpWarning: Result := 'Warning';
        rpFail:    Result := 'Fail';
        else       Result := 'Info';
    end;
end;

function ReportTopics(const AReport: TModuleReport): TStringArray;

    procedure Take(const ATopic: string);
    var
        k: integer;
    begin
        if ATopic = '' then
            Exit;
        for k := 0 to High(Result) do
            if Result[k] = ATopic then
                Exit;
        Append(Result, ATopic);
    end;

var
    i, j: integer;
begin
    Result := nil;
    for i := 0 to High(AReport.Header) do
        Take(AReport.Header[i].Topic);
    for i := 0 to High(AReport.Sections) do
    begin
        Take(AReport.Sections[i].Topic);
        for j := 0 to High(AReport.Sections[i].Findings) do
            Take(AReport.Sections[i].Findings[j].Topic);
    end;
end;

function ModuleReportFindings(const AReport: TModuleReport;
    AResolves: TTopicResolves): TStringArray;
var
    Found: TStringArray;

    procedure CheckFinding(const AWhere: string; const F: TReportFinding);
    begin
        //  A failure says something is wrong; without its rule's explanation
        //  it cannot say why, which is the half the user can act on.
        if (F.Status in [rpWarning, rpFail]) and (F.Topic = '') then
            Append(Found, Format('%s: the %s finding "%s" links to no ' +
                'explanation.', [AWhere, LowerCase(ReportStatusCaption(F.Status)),
                F.Title]));
        if (F.Topic <> '') and Assigned(AResolves) and (not AResolves(F.Topic)) then
            Append(Found, Format('%s: the finding "%s" links to %s, which ' +
                'explains nothing.', [AWhere, F.Title, F.Topic]));
    end;

var
    i, j: integer;
begin
    Found := nil;
    for i := 0 to High(AReport.Header) do
        CheckFinding('the report as a whole', AReport.Header[i]);
    for i := 0 to High(AReport.Sections) do
    begin
        if (AReport.Sections[i].Topic <> '') and Assigned(AResolves) and
            (not AResolves(AReport.Sections[i].Topic)) then
            Append(Found, Format('section "%s" links to %s, which explains ' +
                'nothing.', [AReport.Sections[i].Title, AReport.Sections[i].Topic]));
        for j := 0 to High(AReport.Sections[i].Findings) do
            CheckFinding(Format('section "%s"', [AReport.Sections[i].Title]),
                AReport.Sections[i].Findings[j]);
    end;
    Result := Found;
end;

end.
