// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one per-machine directory this application keeps things in.)

WHERE IT CAME FROM. This was inside sidecar_launch, deciding where the Python
virtual environment lives. It is not about Python: it is "where does this
application keep per-user data that is not a document?", and the second answer
needed is where a downloaded data file is cached. Two copies of the rule would
put one of them under the roaming profile the day somebody corrected only one.

WHY THE ENVIRONMENT IS AN ARGUMENT rather than something read here: every branch
is then reachable from a test without setting a variable in the test process, and
a machine that can name none of them gets '' rather than a path rooted at ''.

LOCALAPPDATA RATHER THAN APPDATA on Windows: the roaming profile would carry
compiled extensions and cached downloads between machines, where the first mean
nothing and the second are simply large.

THE NAME DIFFERS BY PLATFORM - 'Fit' on Windows, 'fit' under XDG - because that
is what each platform's own conventions look like, and because it is what the
sidecar has always used: changing it would strand the environments already
installed on every machine.
}
unit app_data_root;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

{ The directory holding this application's per-user data, or '' when the
  environment names no home at all. Each argument is one environment variable,
  empty when it is unset; which ones exist on this platform is the caller's
  business, and the unused ones arrive empty. }
function AppDataRootFrom(const ALocalAppData, AXdgData, AHome: string): string;

{ The same, reading this platform's variables. The ONE thing here that touches
  the environment, and it reads it as an input: no file is opened and no
  directory is listed. }
function AppDataRoot: string;

{ ASubdirectory of the root, or '' when there is no root - so a caller cannot
  turn "nowhere" into a relative path that resolves against the current
  directory, which is wherever the application happened to be started. }
function AppDataDir(const ASubdirectory: string): string;

implementation

function AppDataRootFrom(const ALocalAppData, AXdgData, AHome: string): string;
var
    Base: string;
begin
    Result := '';
{$IFDEF WINDOWS}
    Base := ALocalAppData;
    if Base <> '' then
        Result := IncludeTrailingPathDelimiter(Base) + 'Fit';
{$ELSE}
    Base := AXdgData;
    if Base = '' then
    begin
        Base := AHome;
        if Base <> '' then
            Base := IncludeTrailingPathDelimiter(Base) + '.local/share';
    end;
    if Base <> '' then
        Result := IncludeTrailingPathDelimiter(Base) + 'fit';
{$ENDIF}
end;

function AppDataRoot: string;
begin
{$IFDEF WINDOWS}
    Result := AppDataRootFrom(GetEnvironmentVariable('LOCALAPPDATA'), '', '');
{$ELSE}
    Result := AppDataRootFrom('', GetEnvironmentVariable('XDG_DATA_HOME'),
        GetEnvironmentVariable('HOME'));
{$ENDIF}
end;

function AppDataDir(const ASubdirectory: string): string;
begin
    Result := AppDataRoot;
    if Result <> '' then
        Result := IncludeTrailingPathDelimiter(Result) + ASubdirectory;
end;

end.
