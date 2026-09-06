// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a saved session holds: the inputs, the values a fit found, and the
working context - as plain records that name no engine and no widget set.)

THE DOCUMENT IS THE INPUTS PLUS THE FITTED VALUES, AND NOTHING ELSE.

The compute server demolishes and rebuilds every curve instance from its inputs
on every model edit (roadmap 10, settled), so a file that stores those inputs
stores exactly what the running engine stores. Everything else it shows - the
calculated profile, the delta, where the built curves sit, the per-round
attribute report, the statistics of the current model - is a pure function of
them, and storing a function of the inputs beside the inputs creates a second
source of truth that can go stale. A derived value that quietly contradicts what
it was derived from is this codebase's signature failure.

THE ONE EXCEPTION, and the reason the format exists: the fitted parameter values.
They come from an optimiser run rather than from the inputs, so nothing can
recompute them, and re-attaching them to the rebuilt instances under their
handles is what makes "reopen and carry on fitting" mean anything.

WHY THIS UNIT NAMES NOTHING FROM THE ENGINE. Settings cross as plain longints,
doubles and strings, exactly as they cross the wire, so this layer can be tested
without standing up a service - the same discipline fit_points_json and
fit_problem_json already state for the same reason. The mapping to and from a
live IFitService is the desktop's job, one layer up.

TWO THINGS ARE STORED IN A FORM THAT MAY LOOK ODD, and both are deliberate:

  * the profile is stored AS THE SERVER HOLDS IT, not as the source file reads.
    Subtracting a background and smoothing rewrite it in place and cannot be
    replayed from the source, so restoring the source's points would restore a
    different problem from the one that was saved;

  * the selected interval is stored as INDICES. The profile it indexes may have
    been smoothed, so a coordinate might no longer name a sample; an index
    always does.
}
unit fit_project_document;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fit_points_json, fit_statistics, fit_project_archive;

const
    { The document's own version.

      An ADDITIVE change does not bump it: a new member, a new part, a new
      section. Only a change that would make an existing reader MISREAD existing
      data does - because that, and not novelty, is what an old build has to be
      kept away from. }
    ProjectFormatVersion = 1;

    { What a reader must support to open a file this build writes.

      SEPARATE FROM THE VERSION ABOVE, and that separation is the whole point: a
      newer build that only ADDED things leaves this alone, so its files still
      open in older builds that would read them perfectly well. Raising it locks
      those builds out, and is therefore a decision rather than a side effect. }
    ProjectMinReaderVersion = 1;

    { The parts this build writes. Named here rather than spelled at each use so
      that the writer, the reader and the tests cannot disagree about them - the
      same reason rest_polling names the polled routes in one place. }
    ProjectManifestPart = 'manifest.json';
    ProjectProblemPart  = 'problem.json';
    ProjectResultsPart  = 'results.json';
    ProjectUiPart       = 'ui.json';
    { Where a module's own document goes: modules/<module name>.json. }
    ProjectModulePartPrefix = 'modules/';

type
    { One parameter of one curve, as a restore hands it back.

      BY NAME, because a model may hold curves of DIFFERENT types, and two types
      can differ in parameter count and, at equal counts, in parameter name. An
      ordinal would put one curve's value under another's heading - which is a
      defect the parameters grid has already had. }
    TProjectParam = record
        Name:  string;
        Value: double;
        { The optimiser's standard error; < 0 when it estimated none. }
        Error: double;
    end;
    TProjectParams = array of TProjectParam;

    { One curve instance: which one it is, what it holds, and whether an
      optimiser is where that came from. }
    TProjectCurve = record
        { The handle, as TEXT. A GUID written as a JSON number arrives as 0 -
          a defect this protocol has already had - and here that would orphan
          every fitted value in the file at once. }
        Id: string;
        { Whether an OPTIMISER produced these values, as opposed to their being
          seeds the project happened to save. It cannot be derived: every
          instance has values from the moment it is placed. }
        Fitted: boolean;
        Params: TProjectParams;
    end;
    TProjectCurves = array of TProjectCurve;

    { Everything the engine is told before it is given anything to work on. }
    TProjectSettings = record
        CurveTypeId: string;
        WaveLength: double;
        MaxRFactor: double;
        BackFactor: double;
        CurveThresh: double;
        MinimizerKind: longint;
        LossKind: longint;
        Weighting: string;
        BackgroundVariationEnabled: boolean;
        CurveScalingEnabled: boolean;
    end;

    { Where the data came from. NOT a dependency: the profile itself is in the
      file, so a project opens on a machine that has never seen the source. This
      is what lets the application say where the data came from and notice that
      it has changed since. }
    TProjectProvenance = record
        SourcePath: string;
        SourceSize: int64;
        SourceHash: string;
        LoaderName: string;
        AppVersion: string;
    end;

    { The working context: what the user would otherwise have to set up again.

      NO WINDOW GEOMETRY. Size, position and maximised state are per-machine,
      and a project opened on a different display would restore a window
      off-screen. Those belong in the application's settings, which are already
      where they live. }
    TProjectUi = record
        ViewMode: longint;
        ViewModeChosenByUser: boolean;
        SelectionMode: longint;
        ActiveTab: longint;
        { The selected curve BY HANDLE, not by row: the model's order is
          derived, so a row index means a different curve after any edit. }
        SelectedCurveId: string;
        CustomAxisName: string;
        CustomAxisUnit: string;
        CustomAxisForward: string;
        CustomAxisInverse: string;
    end;

    { One module's own document, opaque to the framework.

      THE FRAMEWORK NEVER READS THIS. It asks each registered module for its
      state, stores whatever JSON comes back under the module's name, and posts
      it back on restore. That is what lets a module extend the project file
      without the framework naming a module - the rule that keeps a module in a
      repository this one has never heard of. }
    TProjectModuleDocument = record
        Module: string;
        Content: string;
    end;
    TProjectModuleDocuments = array of TProjectModuleDocument;

    { A whole saved session. }
    TProjectDocument = record
        FormatVersion: longint;
        MinReaderVersion: longint;
        CreatedUtc: string;
        ModifiedUtc: string;
        Provenance: TProjectProvenance;

        //  INPUTS. Positions carries one handle per pick in its Ids.
        Profile: TPointsData;
        Background: TPointsData;
        Bounds: TPointsData;
        Positions: TPointsData;
        Settings: TProjectSettings;

        SelectedIntervalInForce: boolean;
        SelectedIntervalFrom: longint;
        SelectedIntervalTo: longint;

        HasUserCurve: boolean;
        UserCurveExpression: string;
        UserCurveParams: TProjectParams;

        //  RESULTS. Absent when the session was saved before any fit ran.
        Curves: TProjectCurves;
        RFactor: double;
        { PROVENANCE OF THE FIT, not a gate. On opening, the recomputed figure
          is compared with this and a mismatch is LOGGED naming both - never a
          refusal. A project whose numbers no longer reproduce is still the
          user's work, and the warning is what stops the discrepancy being
          silent. }
        Statistics: TFitStatistics;

        HasUi: boolean;
        Ui: TProjectUi;

        { What each module asked to keep. Opaque here on purpose. }
        ModuleDocuments: TProjectModuleDocuments;

        { EVERY PART EXACTLY AS READ, and the mechanism behind the format's one
          real extensibility promise. A build writes back the parts it knows and
          hands these through untouched, so a section - or a member inside a
          section - written by a newer build survives being opened and saved by
          an older one. Empty for a document that was never read from a file. }
        AsRead: TProjectParts;
    end;

{ AModified as this format writes a time, and ACreated carried from the document
  being replaced - or equal to AModified when there is none.

  A PURE FUNCTION over a supplied time rather than a call to Now, because "the
  created stamp survives a re-save" is the only thing here worth checking and it
  cannot be checked against a clock. UTC, and stamped in a form that sorts as
  text: a project file may be read by anything, and a local time in an unstated
  zone is not a time. }
procedure StampProject(var ADoc: TProjectDocument;
    const APreviousCreated: string; ANow: TDateTime);

{ A document with nothing in it, every field explicit - so a field added later
  cannot be silently left as whatever the stack held. }
function EmptyProjectDocument: TProjectDocument;

{ Whether this build can open a file whose manifest demands AMinReaderVersion. }
function CanReadProjectVersion(AMinReaderVersion: longint): boolean;

{ The name of the part carrying AModule's own document. }
function ModulePartName(const AModule: string): string;

{ The module a part name belongs to, or '' when the part is not a module's.
  The inverse of ModulePartName, and the only thing that reads a part name
  apart. }
function ModuleOfPartName(const APartName: string): string;

implementation

function EmptyProjectDocument: TProjectDocument;
begin
    Result := Default(TProjectDocument);
    Result.FormatVersion := ProjectFormatVersion;
    Result.MinReaderVersion := ProjectMinReaderVersion;
    Result.Statistics := EmptyFitStatistics;
    //  Not zero: zero is a perfect fit, and "no fit has run" must not read as
    //  one. Negative is what the engine's own reporting uses for the same
    //  reason.
    Result.RFactor := -1;
end;

function CanReadProjectVersion(AMinReaderVersion: longint): boolean;
begin
    Result := AMinReaderVersion <= ProjectFormatVersion;
end;

function ModulePartName(const AModule: string): string;
begin
    Result := ProjectModulePartPrefix + AModule + '.json';
end;

function ModuleOfPartName(const APartName: string): string;
const
    Suffix = '.json';
var
    Body: string;
begin
    Result := '';
    if Copy(APartName, 1, Length(ProjectModulePartPrefix)) <>
        ProjectModulePartPrefix then
        Exit;
    Body := Copy(APartName, Length(ProjectModulePartPrefix) + 1, MaxInt);
    if Copy(Body, Length(Body) - Length(Suffix) + 1, Length(Suffix)) <> Suffix
    then
        Exit;
    Result := Copy(Body, 1, Length(Body) - Length(Suffix));
end;

procedure StampProject(var ADoc: TProjectDocument;
    const APreviousCreated: string; ANow: TDateTime);
begin
    ADoc.ModifiedUtc := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', ANow);
    //  CARRIED, because "created" means when this project came into being and
    //  not when it was last written. A re-save that reset it would make the
    //  field mean the same as the one beside it.
    if Trim(APreviousCreated) <> '' then
        ADoc.CreatedUtc := APreviousCreated
    else
        ADoc.CreatedUtc := ADoc.ModifiedUtc;
end;

end.
