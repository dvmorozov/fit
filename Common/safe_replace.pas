// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Replacing a file so that a failed write leaves the old one whole.)

WHY THIS EXISTS. A project was saved by opening the target with fmCreate, which
truncates it on the spot, and writing into it. A save that failed half way - a
full disk, a network share gone - left neither the old project nor the new one,
for the one operation whose whole purpose is not losing work.

THE NEW CONTENT GOES BESIDE THE FILE FIRST, into a sibling in the same
directory, and only a complete one takes the file's name. Beside, not in a
temporary directory: a rename is only a rename within one volume, and a copy
across volumes is the half-written file again.

THE SWAP HAS NO REPLACE-IN-ONE-STEP on every platform - a rename onto an
existing file fails on Windows - so the old file is moved aside first and put
back if the new one cannot take its place. At no moment is there only a
partial file under the name.
}
unit safe_replace;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ Makes APath hold exactly the bytes of AContent, from its current position to
  its end. True when it does; False, with AFault saying why in words, when it
  does not - and then APath is exactly as it was, and nothing is left beside
  it. }
function ReplaceFileWith(const APath: string; AContent: TStream;
    out AFault: string): boolean;

implementation

function ReplaceFileWith(const APath: string; AContent: TStream;
    out AFault: string): boolean;
var
    Fresh, Aside: string;
    Target: TFileStream;
begin
    Result := False;
    AFault := '';
    Fresh := APath + '.saving';
    Aside := APath + '.previous';
    try
        Target := TFileStream.Create(Fresh, fmCreate);
        try
            Target.CopyFrom(AContent, AContent.Size - AContent.Position);
        finally
            Target.Free;
        end;
    except
        on E: Exception do
        begin
            DeleteFile(Fresh);
            AFault := E.Message;
            Exit;
        end;
    end;

    if FileExists(APath) then
    begin
        DeleteFile(Aside);
        if not RenameFile(APath, Aside) then
        begin
            DeleteFile(Fresh);
            AFault := 'the existing file could not be moved aside to replace it';
            Exit;
        end;
    end;
    if not RenameFile(Fresh, APath) then
    begin
        //  PUT BACK, so the name still holds the last complete project.
        if FileExists(Aside) then
            RenameFile(Aside, APath);
        DeleteFile(Fresh);
        AFault := 'the new file could not take the old one''s name';
        Exit;
    end;
    DeleteFile(Aside);
    Result := True;
end;

end.
