// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Every explanation this build can give, as a browsable index.)

WHY IT EXISTS. The Explain pane answers "what is THIS?" for whatever is in
focus, which helps a user who already knows what to point at. Help > Explain
Everything is for the one who does not: every curve type, every rule a module
enforces, in one list. What the list holds is decided here, where a test can
read it; the window only draws the rows.

GROUPED BY NAMESPACE, in registration order - the framework's own topics first,
then each module's, which is the order a reader meets them in the application.
Within a namespace, ordered by title regardless of case, because a list in
registration order is one nobody can search by eye.

A LISTED TOPIC THAT DOES NOT RESOLVE IS LEFT OUT rather than shown dead: the
completeness tests are where that is reported, and an index entry leading to an
empty page teaches the user that the index is broken.
}
unit explanation_index;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, explanation_registry;

type
    TExplanationIndexRow = record
        { A namespace heading, or a topic's title. }
        Caption: string;
        { The topic to show; '' on a heading. }
        Topic: string;
        IsHeader: boolean;
    end;

    TExplanationIndexRows = array of TExplanationIndexRow;

{ A namespace as a heading reads: 'curve-type' becomes 'Curve type'. }
function NamespaceCaption(const ANamespace: string): string;

function ExplanationIndexOf(
    const AProviders: TExplanationProviders): TExplanationIndexRows;

implementation

function NamespaceCaption(const ANamespace: string): string;
begin
    Result := StringReplace(ANamespace, '-', ' ', [rfReplaceAll]);
    if Result <> '' then
        Result := UpperCase(Copy(Result, 1, 1)) + Copy(Result, 2, MaxInt);
end;

function ExplanationIndexOf(
    const AProviders: TExplanationProviders): TExplanationIndexRows;
var
    i, j, N: longint;
    Topics: TStringArray;
    Entries: TStringList;
    E: TExplanation;
    Caption: string;
begin
    Result := nil;
    N := 0;
    Entries := TStringList.Create;
    try
        for i := 0 to High(AProviders) do
        begin
            Entries.Clear;
            Topics := AProviders[i].StaticTopics;
            for j := 0 to High(Topics) do
                if FindExplanationIn(AProviders, Topics[j], E) then
                begin
                    Caption := E.Title;
                    if Caption = '' then
                        Caption := Topics[j];
                    //  Keyed by the lower-cased title so the sort ignores case,
                    //  the topic after it so two equal titles stay apart.
                    Entries.AddObject(LowerCase(Caption) + #0 + Topics[j],
                        TObject(PtrInt(j)));
                end;
            if Entries.Count = 0 then
                Continue;
            Entries.Sort;

            SetLength(Result, N + 1 + Entries.Count);
            Result[N].Caption := NamespaceCaption(AProviders[i].Namespace);
            Result[N].Topic := '';
            Result[N].IsHeader := True;
            Inc(N);
            for j := 0 to Entries.Count - 1 do
            begin
                FindExplanationIn(AProviders,
                    Topics[PtrInt(Entries.Objects[j])], E);
                Result[N].Topic := E.Topic;
                Result[N].Caption := E.Title;
                if Result[N].Caption = '' then
                    Result[N].Caption := E.Topic;
                Result[N].IsHeader := False;
                Inc(N);
            end;
        end;
    finally
        Entries.Free;
    end;
end;

end.
