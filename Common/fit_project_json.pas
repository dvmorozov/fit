// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The project document as JSON parts, and the two preservation rules
that make the format extensible rather than merely versioned.)

WHAT IS HERE. One encode/decode pair per section, plus the manifest that says
what version wrote the file and what a reader must support to open it.

THE TWO PRESERVATION RULES, which are the whole reason this is not a dozen
inlined ToJSON calls:

  1. A PART THIS BUILD DOES NOT KNOW is written back byte for byte. That is the
     container's doing (fit_project_archive.WithPart); this unit's part is to
     start from the parts as read rather than from an empty list.

  2. A MEMBER THIS BUILD DOES NOT KNOW, inside a section it DOES know, is written
     back too. Without this, opening a newer project in an older build and saving
     deletes whatever the newer build added to a shared section - and the user
     who did it has nothing to tell them so. `Preserving` is the mechanism: the
     new object is built as usual, then every member of the old object that the
     new one does not name is copied across.

WHY fpjson AND NOT fpjsonrtti. Streaming published properties would make the file
format BE the class layout, so renaming a field would silently change the format
- exactly the coupling the existing wire codecs avoid by being plain records over
plain fpjson. This unit follows them.

NUMBERS ARE WRITTEN AT FULL PRECISION. A pick's abscissa and the value stored
against its curve are compared elsewhere with tolerances as tight as 1e-9
(curve_identity_registry.SEED_EPSILON), so a value that loses digits on the way
out comes back as a different value and its curve silently orphans.
}
unit fit_project_json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpjson, jsonparser,
    fit_points_json, fit_statistics,
    fit_project_archive, fit_project_document;

{ The document as the parts of an archive.

  Starts from ADoc.AsRead, so a part this build never understood is carried
  through untouched, and so is a member inside a part that it did not write. }
function ProjectToParts(const ADoc: TProjectDocument): TProjectParts;

{ Reads a document out of the parts of an archive.

  False - with AFault saying why, in words for the user - when the parts are not
  a project this build can open. Never raises: this reads a file the user chose,
  and choosing the wrong one is an ordinary mistake. }
function ProjectFromParts(const AParts: TProjectParts;
    out ADoc: TProjectDocument; out AFault: string): boolean;

{ The same over a stream holding the container itself. }
function WriteProjectToStream(const ADoc: TProjectDocument;
    AStream: TStream): boolean;
function ReadProjectFromStream(AStream: TStream; out ADoc: TProjectDocument;
    out AFault: string): boolean;

implementation

{ ---- small helpers --------------------------------------------------------- }

{ Parses AText as an object, or nil. Never raises. }
function AsObject(const AText: string): TJSONObject;
var
    D: TJSONData;
begin
    Result := nil;
    if Trim(AText) = '' then
        Exit;
    D := nil;
    try
        D := GetJSON(AText);
    except
        D := nil;
    end;
    if D is TJSONObject then
        Result := TJSONObject(D)
    else
        D.Free;
end;

{ ANew, plus every member of the section as it was READ that ANew does not name.

  THIS IS RULE 2. A newer build adds a member to a section this build also
  writes; opening the project here and saving it must not delete that member.
  Copies rather than references, because ANew outlives the parsed original. }
function Preserving(ANew: TJSONObject; const AOriginal: string): string;
var
    Old: TJSONObject;
    i: longint;
begin
    Old := AsObject(AOriginal);
    try
        if Assigned(Old) then
            for i := 0 to Old.Count - 1 do
                if ANew.IndexOfName(Old.Names[i]) < 0 then
                    ANew.Add(Old.Names[i], Old.Items[i].Clone);
        Result := ANew.AsJSON;
    finally
        Old.Free;
        ANew.Free;
    end;
end;

{ The part named AName as it was read, or '' - what Preserving starts from. }
function OriginalPart(const ADoc: TProjectDocument; const AName: string): string;
begin
    if not PartContent(ADoc.AsRead, AName, Result) then
        Result := '';
end;

function ParamsToJson(const AParams: TProjectParams): TJSONArray;
var
    i: longint;
    O: TJSONObject;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(AParams) do
    begin
        O := TJSONObject.Create;
        O.Add('name', AParams[i].Name);
        O.Add('value', AParams[i].Value);
        O.Add('error', AParams[i].Error);
        //  Only where there was a choice, so a fitted curve's entry is as
        //  it always was.
        if AParams[i].HasKind then
            O.Add('type', AParams[i].Kind);
        if AParams[i].Held then
            O.Add('held', True);
        Result.Add(O);
    end;
end;

function ParamsFromJson(AArray: TJSONData): TProjectParams;
var
    i: longint;
    A: TJSONArray;
    O: TJSONObject;
begin
    Result := nil;
    if not (AArray is TJSONArray) then
        Exit;
    A := TJSONArray(AArray);
    SetLength(Result, A.Count);
    for i := 0 to A.Count - 1 do
    begin
        if not (A.Items[i] is TJSONObject) then
            Continue;
        O := TJSONObject(A.Items[i]);
        Result[i].Name := O.Get('name', '');
        Result[i].Value := O.Get('value', 0.0);
        //  -1 is "the optimiser estimated none", which is what a parameter
        //  carries until one does.
        Result[i].Error := O.Get('error', -1.0);
        //  Absent from a project written before roles were kept: the engine
        //  then decides them, as it did when that project was written.
        Result[i].HasKind := O.Find('type') <> nil;
        Result[i].Kind := O.Get('type', 0);
        Result[i].Held := O.Get('held', False);
    end;
end;

{ A point set as a section member. The Ids array rides along for the picks and
  is absent everywhere else, exactly as it is on the wire. }
function PointsMember(const AP: TPointsData): TJSONObject;
begin
    Result := PointsToJson(AP);
end;

function PointsMemberFrom(AData: TJSONData): TPointsData;
begin
    Result := Default(TPointsData);
    if AData is TJSONObject then
        PointsFromJson(TJSONObject(AData), Result);
end;

{ ---- the manifest ---------------------------------------------------------- }

function ManifestJson(const ADoc: TProjectDocument): string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    O.Add('formatVersion', ProjectFormatVersion);
    O.Add('minReaderVersion', ProjectMinReaderVersion);
    O.Add('appVersion', ADoc.Provenance.AppVersion);
    O.Add('created', ADoc.CreatedUtc);
    O.Add('modified', ADoc.ModifiedUtc);
    Result := Preserving(O, OriginalPart(ADoc, ProjectManifestPart));
end;

{ ---- the inputs ------------------------------------------------------------ }

function ProblemJson(const ADoc: TProjectDocument): string;
var
    O, S, Prov, UserCurve, Interval, Origin: TJSONObject;
begin
    O := TJSONObject.Create;
    O.Add('profile', PointsMember(ADoc.Profile));
    O.Add('background', PointsMember(ADoc.Background));
    O.Add('bounds', PointsMember(ADoc.Bounds));
    O.Add('positions', PointsMember(ADoc.Positions));

    S := TJSONObject.Create;
    S.Add('curveType', ADoc.Settings.CurveTypeId);
    S.Add('waveLength', ADoc.Settings.WaveLength);
    S.Add('maxRFactor', ADoc.Settings.MaxRFactor);
    S.Add('backFactor', ADoc.Settings.BackFactor);
    S.Add('curveThresh', ADoc.Settings.CurveThresh);
    S.Add('minimizerKind', ADoc.Settings.MinimizerKind);
    S.Add('lossKind', ADoc.Settings.LossKind);
    S.Add('weighting', ADoc.Settings.Weighting);
    S.Add('backgroundVariation', ADoc.Settings.BackgroundVariationEnabled);
    S.Add('curveScaling', ADoc.Settings.CurveScalingEnabled);
    O.Add('settings', S);

    if ADoc.SelectedIntervalInForce then
    begin
        Interval := TJSONObject.Create;
        //  INDICES. The profile stored beside them may have been smoothed, so a
        //  coordinate would not necessarily name a sample any more.
        Interval.Add('from', ADoc.SelectedIntervalFrom);
        Interval.Add('to', ADoc.SelectedIntervalTo);
        O.Add('selectedInterval', Interval);
    end;

    if ADoc.HasUserCurve then
    begin
        UserCurve := TJSONObject.Create;
        UserCurve.Add('expression', ADoc.UserCurveExpression);
        UserCurve.Add('params', ParamsToJson(ADoc.UserCurveParams));
        O.Add('userCurve', UserCurve);
    end;

    Prov := TJSONObject.Create;
    Prov.Add('sourcePath', ADoc.Provenance.SourcePath);
    Prov.Add('sourceSize', ADoc.Provenance.SourceSize);
    Prov.Add('sourceHash', ADoc.Provenance.SourceHash);
    Prov.Add('loader', ADoc.Provenance.LoaderName);
    //  WRITTEN ONLY WHEN THERE IS ONE, so a project from a file the user chose
    //  themselves carries exactly the keys it always carried.
    if ADoc.Provenance.OriginSourceId <> '' then
    begin
        Origin := TJSONObject.Create;
        Origin.Add('source', ADoc.Provenance.OriginSourceId);
        Origin.Add('sourceTitle', ADoc.Provenance.OriginSourceTitle);
        Origin.Add('query', ADoc.Provenance.OriginQuery);
        Origin.Add('address', ADoc.Provenance.OriginAddress);
        Origin.Add('retrievedAt', ADoc.Provenance.OriginRetrievedAt);
        Prov.Add('origin', Origin);
    end;
    O.Add('provenance', Prov);

    Result := Preserving(O, OriginalPart(ADoc, ProjectProblemPart));
end;

procedure ProblemFromJson(const AText: string; var ADoc: TProjectDocument);
var
    O, S, Prov, UserCurve, Interval, Origin: TJSONObject;
    D: TJSONData;
begin
    O := AsObject(AText);
    if not Assigned(O) then
        Exit;
    try
        ADoc.Profile := PointsMemberFrom(O.Find('profile'));
        ADoc.Background := PointsMemberFrom(O.Find('background'));
        ADoc.Bounds := PointsMemberFrom(O.Find('bounds'));
        ADoc.Positions := PointsMemberFrom(O.Find('positions'));

        D := O.Find('settings');
        if D is TJSONObject then
        begin
            S := TJSONObject(D);
            ADoc.Settings.Stated := [];
            if S.Find('waveLength') <> nil then
                Include(ADoc.Settings.Stated, psWaveLength);
            if S.Find('maxRFactor') <> nil then
                Include(ADoc.Settings.Stated, psMaxRFactor);
            if S.Find('backFactor') <> nil then
                Include(ADoc.Settings.Stated, psBackFactor);
            if S.Find('curveThresh') <> nil then
                Include(ADoc.Settings.Stated, psCurveThresh);
            if S.Find('minimizerKind') <> nil then
                Include(ADoc.Settings.Stated, psMinimizerKind);
            if S.Find('lossKind') <> nil then
                Include(ADoc.Settings.Stated, psLossKind);
            if S.Find('weighting') <> nil then
                Include(ADoc.Settings.Stated, psWeighting);
            if S.Find('backgroundVariation') <> nil then
                Include(ADoc.Settings.Stated, psBackgroundVariation);
            if S.Find('curveScaling') <> nil then
                Include(ADoc.Settings.Stated, psCurveScaling);
            ADoc.Settings.CurveTypeId := S.Get('curveType', '');
            ADoc.Settings.WaveLength := S.Get('waveLength', 0.0);
            ADoc.Settings.MaxRFactor := S.Get('maxRFactor', 0.0);
            ADoc.Settings.BackFactor := S.Get('backFactor', 0.0);
            ADoc.Settings.CurveThresh := S.Get('curveThresh', 0.0);
            ADoc.Settings.MinimizerKind := S.Get('minimizerKind', 0);
            ADoc.Settings.LossKind := S.Get('lossKind', 0);
            ADoc.Settings.Weighting := S.Get('weighting', '');
            ADoc.Settings.BackgroundVariationEnabled :=
                S.Get('backgroundVariation', False);
            ADoc.Settings.CurveScalingEnabled := S.Get('curveScaling', False);
        end;

        D := O.Find('selectedInterval');
        ADoc.SelectedIntervalInForce := D is TJSONObject;
        if ADoc.SelectedIntervalInForce then
        begin
            Interval := TJSONObject(D);
            ADoc.SelectedIntervalFrom := Interval.Get('from', 0);
            ADoc.SelectedIntervalTo := Interval.Get('to', 0);
        end;

        D := O.Find('userCurve');
        ADoc.HasUserCurve := D is TJSONObject;
        if ADoc.HasUserCurve then
        begin
            UserCurve := TJSONObject(D);
            ADoc.UserCurveExpression := UserCurve.Get('expression', '');
            ADoc.UserCurveParams := ParamsFromJson(UserCurve.Find('params'));
        end;

        D := O.Find('provenance');
        if D is TJSONObject then
        begin
            Prov := TJSONObject(D);
            ADoc.Provenance.SourcePath := Prov.Get('sourcePath', '');
            ADoc.Provenance.SourceSize := Prov.Get('sourceSize', 0);
            ADoc.Provenance.SourceHash := Prov.Get('sourceHash', '');
            ADoc.Provenance.LoaderName := Prov.Get('loader', '');
            //  Absent in every project written before data sources existed,
            //  and in every project made from a file the user chose.
            D := Prov.Find('origin');
            if D is TJSONObject then
            begin
                Origin := TJSONObject(D);
                ADoc.Provenance.OriginSourceId := Origin.Get('source', '');
                ADoc.Provenance.OriginSourceTitle := Origin.Get('sourceTitle', '');
                ADoc.Provenance.OriginQuery := Origin.Get('query', '');
                ADoc.Provenance.OriginAddress := Origin.Get('address', '');
                ADoc.Provenance.OriginRetrievedAt := Origin.Get('retrievedAt', '');
            end;
        end;
    finally
        O.Free;
    end;
end;

{ ---- the results ----------------------------------------------------------- }

function ResultsJson(const ADoc: TProjectDocument): string;
var
    O, C, St: TJSONObject;
    Curves: TJSONArray;
    i: longint;
begin
    O := TJSONObject.Create;
    Curves := TJSONArray.Create;
    for i := 0 to High(ADoc.Curves) do
    begin
        C := TJSONObject.Create;
        //  QUOTED. A GUID written as a JSON number arrives as 0, and here that
        //  would orphan every fitted value in the file at once.
        C.Add('id', ADoc.Curves[i].Id);
        C.Add('fitted', ADoc.Curves[i].Fitted);
        C.Add('params', ParamsToJson(ADoc.Curves[i].Params));
        Curves.Add(C);
    end;
    O.Add('curves', Curves);
    O.Add('rFactor', ADoc.RFactor);

    St := TJSONObject.Create;
    St.Add('valid', ADoc.Statistics.Valid);
    St.Add('dataPoints', ADoc.Statistics.DataPoints);
    St.Add('params', ADoc.Statistics.Params);
    St.Add('degreesOfFreedom', ADoc.Statistics.DegreesOfFreedom);
    St.Add('chiSquare', ADoc.Statistics.ChiSquare);
    St.Add('reducedChiSquare', ADoc.Statistics.ReducedChiSquare);
    St.Add('rSquared', ADoc.Statistics.RSquared);
    St.Add('aic', ADoc.Statistics.AIC);
    St.Add('bic', ADoc.Statistics.BIC);
    O.Add('statistics', St);

    Result := Preserving(O, OriginalPart(ADoc, ProjectResultsPart));
end;

procedure ResultsFromJson(const AText: string; var ADoc: TProjectDocument);
var
    O, C, St: TJSONObject;
    D: TJSONData;
    Curves: TJSONArray;
    i: longint;
begin
    O := AsObject(AText);
    if not Assigned(O) then
        Exit;
    try
        D := O.Find('curves');
        if D is TJSONArray then
        begin
            Curves := TJSONArray(D);
            SetLength(ADoc.Curves, Curves.Count);
            for i := 0 to Curves.Count - 1 do
            begin
                if not (Curves.Items[i] is TJSONObject) then
                    Continue;
                C := TJSONObject(Curves.Items[i]);
                ADoc.Curves[i].Id := C.Get('id', '');
                ADoc.Curves[i].Fitted := C.Get('fitted', False);
                ADoc.Curves[i].Params := ParamsFromJson(C.Find('params'));
            end;
        end;
        ADoc.RFactor := O.Get('rFactor', -1.0);

        D := O.Find('statistics');
        if D is TJSONObject then
        begin
            St := TJSONObject(D);
            ADoc.Statistics.Valid := St.Get('valid', False);
            ADoc.Statistics.DataPoints := St.Get('dataPoints', 0);
            ADoc.Statistics.Params := St.Get('params', 0);
            ADoc.Statistics.DegreesOfFreedom := St.Get('degreesOfFreedom', 0);
            ADoc.Statistics.ChiSquare := St.Get('chiSquare', 0.0);
            ADoc.Statistics.ReducedChiSquare := St.Get('reducedChiSquare', 0.0);
            ADoc.Statistics.RSquared := St.Get('rSquared', 0.0);
            ADoc.Statistics.AIC := St.Get('aic', 0.0);
            ADoc.Statistics.BIC := St.Get('bic', 0.0);
        end;
    finally
        O.Free;
    end;
end;

{ ---- the working context --------------------------------------------------- }

{ One coordinate's axis under its keys. The custom definition's keys are
  APrefix + 'Name', 'Unit', 'Forward' and 'Inverse' - for the argument, the
  ones every earlier file already used. }
procedure AddAxis(O: TJSONObject; const AModeKey, ADataKey, APrefix: string;
    const AAxis: TProjectAxis);
begin
    O.Add(AModeKey, AAxis.Mode);
    O.Add(AModeKey + 'ChosenByUser', AAxis.ChosenByUser);
    O.Add(ADataKey, AAxis.DataMode);
    O.Add(APrefix + 'Name', AAxis.CustomName);
    O.Add(APrefix + 'Unit', AAxis.CustomUnit);
    O.Add(APrefix + 'Forward', AAxis.CustomForward);
    O.Add(APrefix + 'Inverse', AAxis.CustomInverse);
end;

procedure ReadAxis(O: TJSONObject; const AModeKey, ADataKey, APrefix: string;
    out AAxis: TProjectAxis);
begin
    AAxis.Mode := O.Get(AModeKey, '');
    AAxis.ChosenByUser := O.Get(AModeKey + 'ChosenByUser', False);
    AAxis.DataMode := O.Get(ADataKey, '');
    AAxis.CustomName := O.Get(APrefix + 'Name', '');
    AAxis.CustomUnit := O.Get(APrefix + 'Unit', '');
    AAxis.CustomForward := O.Get(APrefix + 'Forward', '');
    AAxis.CustomInverse := O.Get(APrefix + 'Inverse', '');
end;

{ A bar's date as text a person can read in the file: the day, and the time
  of day only when there is one. }
function DateText(AValue: TDateTime): string;
begin
    if Frac(AValue) = 0 then
        Result := FormatDateTime('yyyy"-"mm"-"dd', AValue)
    else
        Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', AValue);
end;

function TryDateOfText(const AText: string; out AValue: TDateTime): boolean;
var
    Year, Month, Day, Hour, Minute, Second: longint;
    TimePart: TDateTime;
begin
    Result := False;
    AValue := 0;
    if not (Length(AText) in [10, 19]) or (AText[5] <> '-') or
        (AText[8] <> '-') then
        Exit;
    if not (TryStrToInt(Copy(AText, 1, 4), Year) and
        TryStrToInt(Copy(AText, 6, 2), Month) and
        TryStrToInt(Copy(AText, 9, 2), Day) and
        TryEncodeDate(Year, Month, Day, AValue)) then
        Exit;
    if Length(AText) = 19 then
    begin
        if (AText[11] <> 'T') or (AText[14] <> ':') or (AText[17] <> ':') then
            Exit;
        if not (TryStrToInt(Copy(AText, 12, 2), Hour) and
            TryStrToInt(Copy(AText, 15, 2), Minute) and
            TryStrToInt(Copy(AText, 18, 2), Second) and
            TryEncodeTime(Hour, Minute, Second, 0, TimePart)) then
            Exit;
        AValue := AValue + TimePart;
    end;
    Result := True;
end;

procedure AddDates(O: TJSONObject; const ADates: TProjectDates);
var
    A: TJSONArray;
    i: longint;
begin
    //  NOT WRITTEN AT ALL without dates, so the file of any profile that has
    //  none reads exactly as it did.
    if Length(ADates) = 0 then
        Exit;
    A := TJSONArray.Create;
    for i := 0 to High(ADates) do
        A.Add(DateText(ADates[i]));
    O.Add('argumentDates', A);
end;

function DatesOf(O: TJSONObject): TProjectDates;
var
    A: TJSONArray;
    i: longint;
begin
    Result := nil;
    if O.IndexOfName('argumentDates') < 0 then
        Exit;
    if not (O.Elements['argumentDates'] is TJSONArray) then
        Exit;
    A := TJSONArray(O.Elements['argumentDates']);
    SetLength(Result, A.Count);
    //  ONE PER BAR OR NONE: a list with a hole in it would name every later
    //  bar by its neighbour's day.
    for i := 0 to A.Count - 1 do
        if (A.Types[i] <> jtString) or
            not TryDateOfText(A.Strings[i], Result[i]) then
            Exit(nil);
end;

function UiJson(const ADoc: TProjectDocument): string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    //  THE AXES BY ID. 'viewMode' is not written any more: a build that still
    //  reads it would find it absent and fall back to its own default, where a
    //  stale integer beside 'viewModeChosenByUser' would move it onto an axis.
    //  The argument's custom definition keeps its keys, so it survives either
    //  way.
    AddAxis(O, 'argumentAxis', 'dataArgumentAxis', 'customAxis', ADoc.Ui.Argument);
    AddAxis(O, 'valueAxis', 'dataValueAxis', 'customValueAxis', ADoc.Ui.Value);
    AddDates(O, ADoc.Ui.ArgumentDates);
    O.Add('selectionMode', ADoc.Ui.SelectionMode);
    O.Add('activeTab', ADoc.Ui.ActiveTab);
    O.Add('selectedCurve', ADoc.Ui.SelectedCurveId);
    Result := Preserving(O, OriginalPart(ADoc, ProjectUiPart));
end;

procedure UiFromJson(const AText: string; var ADoc: TProjectDocument);
var
    O: TJSONObject;
begin
    O := AsObject(AText);
    ADoc.HasUi := Assigned(O);
    if not ADoc.HasUi then
        Exit;
    try
        ReadAxis(O, 'argumentAxis', 'dataArgumentAxis', 'customAxis',
            ADoc.Ui.Argument);
        ReadAxis(O, 'valueAxis', 'dataValueAxis', 'customValueAxis',
            ADoc.Ui.Value);
        ADoc.Ui.ArgumentDates := DatesOf(O);
        //  A FILE WRITTEN BEFORE AXIS IDS says which argument axis it was on by
        //  an integer, and whether that was chosen by the flag beside it. Kept
        //  for the desktop to migrate; this unit knows no axis by name.
        ADoc.Ui.HasLegacyViewMode := (O.IndexOfName('argumentAxis') < 0) and
            (O.IndexOfName('viewMode') >= 0);
        ADoc.Ui.LegacyViewMode := 0;
        if ADoc.Ui.HasLegacyViewMode then
        begin
            ADoc.Ui.LegacyViewMode := O.Get('viewMode', 0);
            ADoc.Ui.Argument.ChosenByUser := O.Get('viewModeChosenByUser', False);
        end;
        ADoc.Ui.SelectionMode := O.Get('selectionMode', 0);
        ADoc.Ui.ActiveTab := O.Get('activeTab', 0);
        ADoc.Ui.SelectedCurveId := O.Get('selectedCurve', '');
    finally
        O.Free;
    end;
end;

{ ---- the whole document ---------------------------------------------------- }

function ProjectToParts(const ADoc: TProjectDocument): TProjectParts;
var
    i: longint;
begin
    //  FROM THE PARTS AS READ, not from an empty list. That single choice is
    //  what carries a section written by a newer build through this one
    //  untouched - rule 1 of the two at the top of this unit.
    Result := ADoc.AsRead;
    Result := WithPart(Result, ProjectManifestPart, ManifestJson(ADoc));
    Result := WithPart(Result, ProjectProblemPart, ProblemJson(ADoc));
    Result := WithPart(Result, ProjectResultsPart, ResultsJson(ADoc));
    Result := WithPart(Result, ProjectUiPart, UiJson(ADoc));
    //  Each module's own document, exactly as the module handed it over. The
    //  framework does not parse it and must not: a module's state is the
    //  module's business, and reading it here would mean the framework knowing
    //  what a module keeps.
    for i := 0 to High(ADoc.ModuleDocuments) do
        Result := WithPart(Result,
            ModulePartName(ADoc.ModuleDocuments[i].Module),
            ADoc.ModuleDocuments[i].Content);
end;

function ProjectFromParts(const AParts: TProjectParts;
    out ADoc: TProjectDocument; out AFault: string): boolean;
var
    Manifest: TJSONObject;
    Text, Module: string;
    Needs, i: longint;
begin
    ADoc := EmptyProjectDocument;
    AFault := '';
    Result := False;

    if not PartContent(AParts, ProjectManifestPart, Text) then
    begin
        AFault := 'This file has no project manifest, so it is not a Fit ' +
            'project.';
        Exit;
    end;

    Manifest := AsObject(Text);
    if not Assigned(Manifest) then
    begin
        AFault := 'This project''s manifest could not be read, so the file ' +
            'is damaged.';
        Exit;
    end;
    try
        ADoc.FormatVersion := Manifest.Get('formatVersion',
            ProjectFormatVersion);
        Needs := Manifest.Get('minReaderVersion', ProjectFormatVersion);
        ADoc.MinReaderVersion := Needs;
        if not CanReadProjectVersion(Needs) then
        begin
            //  REFUSED, NAMING WHAT IT NEEDS. Half-reading a file written by a
            //  build that changed the meaning of a field brings a project back
            //  subtly wrong instead of not at all, and only the second of those
            //  is recoverable.
            AFault := Format('This project needs version %d of the project ' +
                'file format and this build reads version %d. Use a newer ' +
                'version of Fit to open it.',
                [Needs, ProjectFormatVersion]);
            Exit;
        end;
        ADoc.Provenance.AppVersion := Manifest.Get('appVersion', '');
        ADoc.CreatedUtc := Manifest.Get('created', '');
        ADoc.ModifiedUtc := Manifest.Get('modified', '');
    finally
        Manifest.Free;
    end;

    if PartContent(AParts, ProjectProblemPart, Text) then
        ProblemFromJson(Text, ADoc);
    if PartContent(AParts, ProjectResultsPart, Text) then
        ResultsFromJson(Text, ADoc);
    if PartContent(AParts, ProjectUiPart, Text) then
        UiFromJson(Text, ADoc);

    //  Every module part there is, by name. Read generically rather than by
    //  asking the registry which modules exist: a project may carry the state
    //  of a module this build does not have, and dropping it on load would
    //  destroy it on the next save.
    for i := 0 to High(AParts) do
    begin
        Module := ModuleOfPartName(AParts[i].Name);
        if Module = '' then
            Continue;
        SetLength(ADoc.ModuleDocuments, Length(ADoc.ModuleDocuments) + 1);
        ADoc.ModuleDocuments[High(ADoc.ModuleDocuments)].Module := Module;
        ADoc.ModuleDocuments[High(ADoc.ModuleDocuments)].Content :=
            AParts[i].Content;
    end;

    //  Kept whole, so that writing this document back out preserves both the
    //  parts and the members this build did not read.
    ADoc.AsRead := AParts;
    Result := True;
end;

function WriteProjectToStream(const ADoc: TProjectDocument;
    AStream: TStream): boolean;
begin
    Result := WriteProjectArchive(ProjectToParts(ADoc), AStream);
end;

function ReadProjectFromStream(AStream: TStream; out ADoc: TProjectDocument;
    out AFault: string): boolean;
var
    Parts: TProjectParts;
begin
    ADoc := EmptyProjectDocument;
    AFault := '';
    if not ReadProjectArchive(AStream, Parts) then
    begin
        AFault := 'This file is not a Fit project.';
        Exit(False);
    end;
    Result := ProjectFromParts(Parts, ADoc, AFault);
end;

end.
