// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a place data comes from is, and what it must be able to answer.)

A SOURCE FINDS AND FETCHES; IT NEVER PARSES DATA. It says which extension the
file it hands over has, and data_loader_registry decides what reads it - so a
price CSV downloaded from a feed, the same CSV opened from disk, and a CSV
arriving from any source yet to be written all go through ONE parser. The moment
a source parses its own numbers there are two answers to "what is a column of
numbers?", and they disagree the first time either is corrected.

What a source DOES parse is its own catalogue: the JSON or HTML that says which
records exist and which files a record holds. That is metadata about where the
data is, not the data.

CAPABILITIES, NOT ENUMERATION. A source declares what it needs and what it
produces - its query fields, its category, whether it needs the network, which
extensions come out - and the wizard derives every question it asks from that.
Nothing anywhere lists "source X has step Y". Adding a source is a unit plus one
registration line, in the framework or in any module.

THE WALK IS WHAT KEEPS THIS HONEST: a registry-walking test asserts that every
registered source explains itself and that every extension it names has a reader,
so the next source registered without either fails by name rather than failing in
front of a user.
}
unit data_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_web_client;

type
    { Anything a source refuses or cannot do, worded for the user. }
    EDataSourceError = class(Exception);

    { What kind of answer a query field takes. Deliberately few: these are the
      shapes a catalogue actually asks for, and a field kind nothing uses is a
      widget nobody has ever seen. }
    TInputFieldKind = (
        ifText,     //  free text; Choices, when there are any, are offered
        ifChoice,   //  one OF Choices, which are then the whole of it
        ifDate      //  a date, chosen from a calendar; empty is "unbounded"
        );

    { ONE THING A USER CAN CHOOSE, and the thing a service is then asked for.

      THE TWO ARE NOT THE SAME, and that is the point. A service names a series
      'SP500' and a substance 'C71432'; a user knows "S&P 500" and "benzene".
      Making somebody look an identifier up before they can ask a question is
      the difference between a program for the people who built it and one for
      the people who use it - so the CAPTION is what is shown, the VALUE is
      what travels, and no window ever shows the value. }
    TInputChoice = record
        Caption: string;
        Value: string;
    end;

    TInputChoices = array of TInputChoice;

    { A question a source asks, declared as data. A source names no widget: the
      wizard renders these, exactly as the window builds a module's menu from
      TUiMenuDecl. }
    TInputFieldDecl = record
        Id: string;
        Caption: string;
        { What the field means, shown beside it. }
        Hint: string;
        { An explanation topic, or '' - the same contract a menu entry carries. }
        Topic: string;
        Kind: TInputFieldKind;
        { For ifChoice everything that may be chosen; for ifText what is
          offered beside the box, which may still be typed past. Empty for a
          field whose answers nobody can enumerate - an address, a DOI. }
        Choices: TInputChoices;
        { The VALUE (never a caption) the field starts at. }
        DefaultValue: string;
        { Whether the search can run without it. }
        Required: boolean;
        { THE FIELD IS ASKED ONLY WHILE the field EnabledWhenField holds
          EnabledWhenValue - dates, say, only while a Range choice is "Custom
          dates". Empty for a field that is always asked. Declared rather than
          wired, so the wizard greys it, says why, and leaves it out of the
          query - one rule for every source (FieldIsEnabled). }
        EnabledWhenField: string;
        EnabledWhenValue: string;
    end;

    TInputFieldDecls = array of TInputFieldDecl;

    { What a source says about itself. }
    TDataSourceInfo = record
        { Unique, lower case, and stable: it is written into a project's
          provenance, so renaming it would orphan what it identifies. }
        Id: string;
        Title: string;
        { What the wizard groups it under - 'General', 'Markets', 'Spectra'. A
          category exists because a source uses it; nothing lists them. }
        Category: string;
        { One line under the title in the source list. }
        Summary: string;
        { Its explanation topic. Required: everything a user meets explains
          itself, and the registry walk refuses a source without one. }
        Topic: string;
        { The extensions files from here arrive with, semicolon-separated and
          dot-prefixed. Every one of them must have a registered loader, which
          the walk checks - a source that can only deliver files nothing reads
          is a dead end the user meets instead of the build. }
        ProducesExtensions: string;
        { Whether reaching it needs the network. A source that does not - the
          samples that ship with the program - still works with no connection
          and is what the end-to-end test drives. }
        NeedsNetwork: boolean;
        { What the source asks before it can search. Empty means it needs
          nothing, and the wizard then skips the Find step entirely. }
        QueryFields: TInputFieldDecls;
        { Whether a search can answer with containers whose children are the
          files - a record holding several spectra, a dataset holding several
          files. When it cannot, every result is a file and the Choose step is
          where the search lands rather than a step of its own. }
        HasContainers: boolean;
    end;

    { One thing a search found: a file to download, or a container holding
      files. The identity a source uses to fetch it is its own business - a URL,
      a record number, a path - so Ref carries whatever that is. }
    TDataSourceItem = record
        { Unique within one search, and stable while it is on screen. }
        Id: string;
        Title: string;
        { The column beside the title: authors, a date, a licence, a symbol. }
        Details: string;
        { What the source will call the file it downloads. Its extension is what
          decides which loader reads it, so a source that knows the shape of
          what it emits states it here rather than hoping the URL ends well. }
        FileName: string;
        { Whatever the source needs to fetch or expand this item. }
        Ref: string;
        { Bytes, or 0 when the catalogue did not say. }
        Size: int64;
        IsLeaf: boolean;
    end;

    TDataSourceItems = array of TDataSourceItem;

    { One answer to one query field. }
    TQueryValue = record
        Id: string;
        Value: string;
    end;

    TDataSourceQuery = array of TQueryValue;

    { The base every source descends from.

      IT IS HANDED ITS WEB CLIENT rather than making one, which is what lets
      every source be tested against canned replies with no network, and what
      lets one cancel and one progress report cover them all. }
    TDataSource = class(TObject)
    protected
        FWeb: IWebClient;
    public
        { AWeb is how this source reaches the network, and the only way it may.
          A source that needs none - the samples that ship with the program -
          is handed one anyway and ignores it, so creating a source is one
          call whatever it is. }
        constructor Create(AWeb: IWebClient); virtual;

        { What this source is. A class function because the wizard has to list
          sources before it creates any of them. }
        class function Info: TDataSourceInfo; virtual; abstract;

        { What AQuery finds here. Items may be containers only when the source
          said HasContainers. Raises EDataSourceError, worded for the user, when
          the query cannot be answered. }
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems;
            virtual; abstract;

        { What is inside a container. Never called for a leaf. }
        function Children(const AItem: TDataSourceItem): TDataSourceItems; virtual;

        { Writes the item's bytes into ADest and answers the file name they
          should be saved under - which is what decides the loader. }
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            virtual; abstract;
    end;

    TDataSourceClass = class of TDataSource;

{ The value given for a field, or its declared default when the query is silent
  about it. One rule, so "the user left it empty" means the same everywhere. }
function QueryValue(const AQuery: TDataSourceQuery; const AId: string): string;

{ AQuery with AId set to AValue, replacing any previous answer. }
function WithQueryValue(const AQuery: TDataSourceQuery;
    const AId, AValue: string): TDataSourceQuery;

{ A query holding every field's default - what the Find step starts from. }
function DefaultQuery(const AFields: TInputFieldDecls): TDataSourceQuery;

{ Which required field is still empty, or '' when the query is answerable.
  Returns the field's CAPTION: it is shown to the user as the reason the Search
  button is disabled. }
function MissingRequiredField(const AFields: TInputFieldDecls;
    const AQuery: TDataSourceQuery): string;

{ A text field, and the three other shapes. Helpers rather than record literals
  so that a field added to TInputFieldDecl does not have to be filled in at
  every declaration site. }
function TextField(const AId, ACaption, AHint: string;
    ARequired: boolean = True): TInputFieldDecl;

{ A field answered by choosing one of AChoices and nothing else. }
function ChoiceField(const AId, ACaption, AHint: string;
    const AChoices: TInputChoices; const ADefault: string): TInputFieldDecl;

{ Free text with the common answers offered beside it - a name, where the
  service knows thousands and a user knows six. }
function SuggestedField(const AId, ACaption, AHint: string;
    const AChoices: TInputChoices): TInputFieldDecl;

{ A date, chosen from a calendar. Never required: an empty date means "as far
  back as there is", which is a request rather than an unanswered question. }
function DateField(const AId, ACaption, AHint: string): TInputFieldDecl;

{ AField, asked only while the field AFieldId holds AValue. }
function EnabledWhen(const AField: TInputFieldDecl;
    const AFieldId, AValue: string): TInputFieldDecl;

{ Whether the field AId is asked, given what AQuery answers so far. A field
  nothing declares, or one with no condition, is. }
function FieldIsEnabled(const AFields: TInputFieldDecls; const AId: string;
    const AQuery: TDataSourceQuery): boolean;

{ Why the field AId may not be asked, in the user's words - 'Used only when
  Range is "Custom dates".' - or '' for a field that is always asked. What the
  wizard shows beside a greyed field, so the greying explains itself. }
function FieldCondition(const AFields: TInputFieldDecls;
    const AId: string): string;

{ AQuery as a source is asked it: every field not asked is empty, whatever was
  typed into it before it was greyed. }
function EffectiveQuery(const AFields: TInputFieldDecls;
    const AQuery: TDataSourceQuery): TDataSourceQuery;

{ Choices from captions and values in pairs:
  Choices(['S&P 500', 'SP500', 'Dow Jones', 'DJIA']). Raises when the count is
  odd, which is a mistake in a declaration that would otherwise show an
  identifier to a user as though it were a name. }
function Choices(const ACaptionsAndValues: array of string): TInputChoices;

{ The value behind a caption, or '' - what a window asks once a user has
  chosen. }
function ValueOfChoice(const AChoices: TInputChoices;
    const ACaption: string): string;

{ The caption of a value, or the value itself when nothing offers it, which is
  what a query remembered from a build with a different catalogue looks like. }
function CaptionOfChoice(const AChoices: TInputChoices;
    const AValue: string): string;

{ ATEXT as it may appear inside a URL's query string. Written here rather than
  taken from fcl-web: this is the desktop client, which has no other reason to
  link a web framework, and the rule is eleven lines of RFC 3986. }
function UrlEncoded(const AText: string): string;

implementation

function QueryValue(const AQuery: TDataSourceQuery; const AId: string): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AQuery) do
        if SameText(AQuery[i].Id, AId) then
            Exit(AQuery[i].Value);
end;

function WithQueryValue(const AQuery: TDataSourceQuery;
    const AId, AValue: string): TDataSourceQuery;
var
    i: longint;
begin
    Result := Copy(AQuery, 0, Length(AQuery));
    for i := 0 to High(Result) do
        if SameText(Result[i].Id, AId) then
        begin
            Result[i].Value := AValue;
            Exit;
        end;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Id := AId;
    Result[High(Result)].Value := AValue;
end;

function DefaultQuery(const AFields: TInputFieldDecls): TDataSourceQuery;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(AFields));
    for i := 0 to High(AFields) do
    begin
        Result[i].Id := AFields[i].Id;
        Result[i].Value := AFields[i].DefaultValue;
    end;
end;

function MissingRequiredField(const AFields: TInputFieldDecls;
    const AQuery: TDataSourceQuery): string;
var
    i: longint;
begin
    Result := '';
    //  A GREYED FIELD IS NOT ASKED, so it cannot be missing.
    for i := 0 to High(AFields) do
        if AFields[i].Required and
            FieldIsEnabled(AFields, AFields[i].Id, AQuery) and
            (Trim(QueryValue(AQuery, AFields[i].Id)) = '') then
            Exit(AFields[i].Caption);
end;

function TextField(const AId, ACaption, AHint: string;
    ARequired: boolean = True): TInputFieldDecl;
begin
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    Result.Topic := '';
    Result.Kind := ifText;
    Result.Choices := nil;
    Result.DefaultValue := '';
    Result.Required := ARequired;
    Result.EnabledWhenField := '';
    Result.EnabledWhenValue := '';
end;

function EnabledWhen(const AField: TInputFieldDecl;
    const AFieldId, AValue: string): TInputFieldDecl;
begin
    Result := AField;
    Result.EnabledWhenField := AFieldId;
    Result.EnabledWhenValue := AValue;
end;

function IndexOfField(const AFields: TInputFieldDecls; const AId: string): longint;
var
    i: longint;
begin
    for i := 0 to High(AFields) do
        if SameText(AFields[i].Id, AId) then
            Exit(i);
    Result := -1;
end;

function FieldIsEnabled(const AFields: TInputFieldDecls; const AId: string;
    const AQuery: TDataSourceQuery): boolean;
var
    i: longint;
begin
    i := IndexOfField(AFields, AId);
    if (i < 0) or (AFields[i].EnabledWhenField = '') then
        Exit(True);
    //  The value compared, never the caption: a caption may be reworded and
    //  the value is what the query holds.
    Result := QueryValue(AQuery, AFields[i].EnabledWhenField) =
        AFields[i].EnabledWhenValue;
end;

function FieldCondition(const AFields: TInputFieldDecls;
    const AId: string): string;
var
    i, Guard: longint;
    Name_, Answer: string;
begin
    Result := '';
    i := IndexOfField(AFields, AId);
    if (i < 0) or (AFields[i].EnabledWhenField = '') then
        Exit;
    //  IN THE USER'S WORDS: the guarding field by its caption, and the answer
    //  by the caption of the choice when it is one.
    Guard := IndexOfField(AFields, AFields[i].EnabledWhenField);
    Name_ := AFields[i].EnabledWhenField;
    Answer := AFields[i].EnabledWhenValue;
    if Guard >= 0 then
    begin
        Name_ := AFields[Guard].Caption;
        Answer := CaptionOfChoice(AFields[Guard].Choices, Answer);
    end;
    Result := 'Used only when ' + Name_ + ' is "' + Answer + '".';
end;

function EffectiveQuery(const AFields: TInputFieldDecls;
    const AQuery: TDataSourceQuery): TDataSourceQuery;
var
    i: longint;
begin
    Result := Copy(AQuery, 0, Length(AQuery));
    for i := 0 to High(Result) do
        if not FieldIsEnabled(AFields, Result[i].Id, AQuery) then
            Result[i].Value := '';
end;

function ChoiceField(const AId, ACaption, AHint: string;
    const AChoices: TInputChoices; const ADefault: string): TInputFieldDecl;
begin
    Result := TextField(AId, ACaption, AHint, True);
    Result.Kind := ifChoice;
    Result.Choices := AChoices;
    Result.DefaultValue := ADefault;
end;

function SuggestedField(const AId, ACaption, AHint: string;
    const AChoices: TInputChoices): TInputFieldDecl;
begin
    Result := TextField(AId, ACaption, AHint, True);
    Result.Choices := AChoices;
end;

function DateField(const AId, ACaption, AHint: string): TInputFieldDecl;
begin
    //  Never required: an empty date means "as far back as there is", which is
    //  a sensible request rather than an unanswered question.
    Result := TextField(AId, ACaption, AHint, False);
    Result.Kind := ifDate;
end;

function Choices(const ACaptionsAndValues: array of string): TInputChoices;
var
    i: longint;
begin
    Result := nil;
    if Odd(Length(ACaptionsAndValues)) then
        //  A declaration that pairs up wrongly would put an identifier in front
        //  of a user as though it were a name, which is the one thing captions
        //  exist to prevent.
        raise EDataSourceError.Create(
            'a list of choices must be captions and values in pairs');
    SetLength(Result, Length(ACaptionsAndValues) div 2);
    for i := 0 to High(Result) do
    begin
        Result[i].Caption := ACaptionsAndValues[i * 2];
        Result[i].Value := ACaptionsAndValues[i * 2 + 1];
    end;
end;

function ValueOfChoice(const AChoices: TInputChoices;
    const ACaption: string): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AChoices) do
        if AChoices[i].Caption = ACaption then
            Exit(AChoices[i].Value);
end;

function CaptionOfChoice(const AChoices: TInputChoices;
    const AValue: string): string;
var
    i: longint;
begin
    for i := 0 to High(AChoices) do
        if AChoices[i].Value = AValue then
            Exit(AChoices[i].Caption);
    //  Not found: a query remembered from a build whose catalogue has since
    //  changed. Showing the value is honest, and still recognisable.
    Result := AValue;
end;

function UrlEncoded(const AText: string): string;
const
    //  Unreserved characters, which must NOT be escaped: escaping them is legal
    //  but some services compare query strings as text.
    Unreserved = ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'];
var
    i: longint;
begin
    Result := '';
    for i := 1 to Length(AText) do
        if AText[i] in Unreserved then
            Result := Result + AText[i]
        else if AText[i] = ' ' then
            Result := Result + '+'
        else
            Result := Result + '%' + IntToHex(Ord(AText[i]), 2);
end;

constructor TDataSource.Create(AWeb: IWebClient);
begin
    inherited Create;
    FWeb := AWeb;
end;

function TDataSource.Children(const AItem: TDataSourceItem): TDataSourceItems;
begin
    //  Most sources answer with files only. Saying so here means a source that
    //  has no containers writes nothing, rather than an override that returns
    //  an empty array in every one of them.
    Result := nil;
end;

end.
