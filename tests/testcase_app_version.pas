// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reporting which build is running: the version, and the window title.)

The version shown to the user is read out of the running binary, so what these
tests can assert depends on the binary running them - and the test suite carries
no version resource of its own. That is not a gap: it is the case that matters
most. A build with no version info is exactly the situation in which the old
About box lied ("version 1.1"), and the situation in which a naive title would
read "Fit " with a space hanging off the end, or " - Fit" with a separator and
no document.

So what is pinned here is the shape of the answer in both cases, and above all
that a missing version is survivable - the version names a build in a title bar,
and a binary that cannot name itself must still start.
}
unit testcase_app_version;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, app_version;

type
    TAppVersionTest = class(TTestCase)
    published
        procedure ReadingTheVersionNeverRaises;
        procedure AVersionThatIsReportedHasFourNumbers;
        procedure TheAnswerDoesNotChangeBetweenCalls;
        procedure WithoutADocumentTheTitleIsTheApplication;
        procedure TheDocumentComesFirst;
        procedure TheTitleNeverEndsInASeparator;
    end;

implementation

function IsFourNumbers(const S: string): boolean;
var
    I, Dots: integer;
begin
    Result := False;
    Dots := 0;
    if S = '' then Exit;
    for I := 1 to Length(S) do
        if S[I] = '.' then Inc(Dots)
        else if not (S[I] in ['0'..'9']) then Exit;
    Result := (Dots = 3) and (S[1] <> '.') and (S[Length(S)] <> '.');
end;

procedure TAppVersionTest.ReadingTheVersionNeverRaises;
begin
    //  The whole point of the announced fallback: a binary built without version
    //  info reports nothing and carries on. Before this, the alternative on the
    //  table was raising - which would have turned a cosmetic gap into an app
    //  that will not open.
    try
        GetAppVersion;
    except
        on E: Exception do
            Fail('Reading the version raised ' + E.ClassName + ': ' + E.Message);
    end;
end;

procedure TAppVersionTest.AVersionThatIsReportedHasFourNumbers;
begin
    //  Either the four numbers from the .lpi, or nothing at all. Never a partial
    //  string, and never a placeholder that reads like a real version.
    if GetAppVersion <> '' then
        AssertTrue('"' + GetAppVersion + '" is not four dot-separated numbers',
            IsFourNumbers(GetAppVersion));
end;

procedure TAppVersionTest.TheAnswerDoesNotChangeBetweenCalls;
begin
    //  Cached after the first read. The title bar, the About box and any log line
    //  must agree, and a caller must not pay for reading the executable again.
    AssertEquals(GetAppVersion, GetAppVersion);
end;

procedure TAppVersionTest.WithoutADocumentTheTitleIsTheApplication;
var
    Expected: string;
begin
    Expected := 'Fit';
    if GetAppVersion <> '' then Expected := Expected + ' ' + GetAppVersion;
    AssertEquals(Expected, GetWindowTitle('Fit', ''));
end;

procedure TAppVersionTest.TheDocumentComesFirst;
var
    Title: string;
begin
    //  The convention every desktop follows, and the reason for it: a task bar
    //  truncates from the right, so the name that distinguishes this window from
    //  the next one has to be at the left.
    Title := GetWindowTitle('Fit', '2.dat');
    AssertEquals('2.dat', Copy(Title, 1, 5));
    AssertTrue('the application name is missing from "' + Title + '"',
        Pos('Fit', Title) > 0);
end;

procedure TAppVersionTest.TheTitleNeverEndsInASeparator;
var
    Title: string;
begin
    //  What a missing version used to produce when it was pasted in blindly:
    //  'Fit ' with a trailing space, which shows up as a ragged title bar.
    Title := GetWindowTitle('Fit', '');
    AssertEquals(Title, TrimRight(Title));
    AssertTrue('the title ends in a separator: "' + Title + '"',
        Copy(Title, Length(Title) - 1, 2) <> ' -');
end;

initialization
    RegisterTest('unit', TAppVersionTest);

end.
