// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reads the first sheet of an .xlsx or .ods workbook as a table of
numbers.)

A TABLE READER WITH ANOTHER WAY IN. A workbook is a zip of XML; this opens it,
turns its first sheet into tab-separated lines (spreadsheet_sheet), and hands
them to the table reader it descends from - so a sheet and a CSV of the same
numbers give the same profile, the same column names and the same choice of
value column. spreadsheet_sheet says why this is not a spreadsheet library.

WHICH FORMAT is told by the parts inside, not by the extension: an .xlsx holds
xl/workbook.xml, an .ods holds content.xml. A file named either that is
neither - a web page saved under the name - is refused saying so.
}
unit spreadsheet_file_loader;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, csv_file_loader;

type
    TSpreadsheetFileLoader = class(TCSVFileLoader)
    protected
        procedure LoadDataSetActually; override;
    end;

{ The parts of the zip in AData named in ANames, each as a stream (or nil
  when the zip has no such part), in the order asked. The caller frees them.
  Raises, naming AWhat, when the bytes are not a zip. }
function ZipParts(AData: TStream; const AWhat: string;
    const ANames: array of string): TList;

{ The first sheet of the workbook in AData - an .xlsx or an .ods, told by the
  parts inside - as tab-separated lines. AWHAT names it in a refusal. What the
  loader hands the table reader; separate from the file so a test can give it
  a workbook built in memory. }
function WorkbookLines(AData: TStream; const AWhat: string): TStringList;

implementation

uses
    zipper, StrUtils, data_loader, spreadsheet_sheet;

type
    { Keeps each part the unzipper inflates, by its name. }
    TPartCatcher = class(TObject)
    private
        FParts: TStringList;
        FData: TStream;
        procedure OpenInput(Sender: TObject; var AStream: TStream);
        procedure CreateOutput(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
        procedure DoneOutput(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
    public
        constructor Create;
        destructor Destroy; override;
    end;

constructor TPartCatcher.Create;
begin
    inherited Create;
    FParts := TStringList.Create;
    FParts.OwnsObjects := True;
end;

destructor TPartCatcher.Destroy;
begin
    FParts.Free;
    inherited Destroy;
end;

procedure TPartCatcher.OpenInput(Sender: TObject; var AStream: TStream);
var
    Copy_: TMemoryStream;
begin
    //  A copy the unzipper frees when it is done, as it does any input.
    Copy_ := TMemoryStream.Create;
    FData.Position := 0;
    Copy_.CopyFrom(FData, FData.Size);
    Copy_.Position := 0;
    AStream := Copy_;
end;

procedure TPartCatcher.CreateOutput(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
begin
    AStream := TMemoryStream.Create;
    FParts.AddObject(AItem.ArchiveFileName, AStream);
end;

procedure TPartCatcher.DoneOutput(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
begin
    //  Kept: the catcher owns it, not the unzipper.
    AStream := nil;
end;

function ZipParts(AData: TStream; const AWhat: string;
    const ANames: array of string): TList;
var
    Zip: TUnZipper;
    Catcher: TPartCatcher;
    Wanted: TStringList;
    i, Index: longint;
begin
    Result := TList.Create;
    Zip := TUnZipper.Create;
    Catcher := TPartCatcher.Create;
    Wanted := TStringList.Create;
    try
        try
            Catcher.FData := AData;
            Zip.OnOpenInputStream := @Catcher.OpenInput;
            Zip.OnCreateStream := @Catcher.CreateOutput;
            Zip.OnDoneStream := @Catcher.DoneOutput;
            Zip.Examine;
            for i := 0 to Zip.Entries.Count - 1 do
                if Wanted.IndexOf(Zip.Entries[i].ArchiveFileName) < 0 then
                    Wanted.Add(Zip.Entries[i].ArchiveFileName);
            //  ONLY THE PARTS ASKED FOR are inflated: a workbook may carry
            //  images and a thumbnail that nothing here reads.
            for i := Wanted.Count - 1 downto 0 do
                if not AnsiMatchText(Wanted[i], ANames) then
                    Wanted.Delete(i);
            if Wanted.Count > 0 then
                Zip.UnZipFiles(Wanted);
        except
            on E: Exception do
            begin
                Result.Free;
                raise EInvalidFileFormat.Create(AWhat + ' could not be ' +
                    'opened as a workbook (' + E.Message + '): it may be ' +
                    'damaged, or not a spreadsheet at all.');
            end;
        end;
        for i := 0 to High(ANames) do
        begin
            Index := Catcher.FParts.IndexOf(ANames[i]);
            if Index < 0 then
                Result.Add(nil)
            else
            begin
                Result.Add(Catcher.FParts.Objects[Index]);
                //  Handed over: the caller frees it now.
                Catcher.FParts.Objects[Index] := nil;
            end;
        end;
    finally
        Wanted.Free;
        Catcher.Free;
        Zip.Free;
    end;
end;

procedure FreeParts(AParts: TList);
var
    i: longint;
begin
    if AParts = nil then
        Exit;
    for i := 0 to AParts.Count - 1 do
        TObject(AParts[i]).Free;
    AParts.Free;
end;

function WorkbookLines(AData: TStream; const AWhat: string): TStringList;
var
    Parts, SheetParts: TList;
    SheetPath: string;
begin
    Result := nil;
    Parts := ZipParts(AData, AWhat, ['xl/workbook.xml',
        'xl/_rels/workbook.xml.rels', 'xl/sharedStrings.xml', 'content.xml']);
    try
        if Parts[0] <> nil then
        begin
            SheetPath := XlsxFirstSheetPath(TStream(Parts[0]), TStream(Parts[1]));
            SheetParts := ZipParts(AData, AWhat, [SheetPath]);
            try
                if SheetParts[0] = nil then
                    raise EInvalidFileFormat.Create(AWhat + ' names its first ' +
                        'sheet ' + SheetPath + ', and holds no such part.');
                Result := XlsxSheetLines(TStream(SheetParts[0]), TStream(Parts[2]));
            finally
                FreeParts(SheetParts);
            end;
        end
        else if Parts[3] <> nil then
            Result := OdsSheetLines(TStream(Parts[3]))
        else
            raise EInvalidFileFormat.Create(AWhat + ' is a zip archive but not ' +
                'a workbook: it holds neither an .xlsx''s xl/workbook.xml nor ' +
                'an .ods''s content.xml.');
    finally
        FreeParts(Parts);
    end;
end;

procedure TSpreadsheetFileLoader.LoadDataSetActually;
var
    Data: TFileStream;
    Lines: TStringList;
begin
    Data := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
        Lines := WorkbookLines(Data, FFileName);
        try
            ParseLines(Lines);
        finally
            Lines.Free;
        end;
    finally
        Data.Free;
    end;
end;

end.
