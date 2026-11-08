// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The main window's menu as the form file designs it, for the tests that
check what other text says about it.)

The window cannot be built in a test process - the nogui widget set has no menu
support - so what the user will find in the menu bar is read from
form_main.lfm, the file the window is streamed from. The guide names every
command by its path, and the status line sends the user to menu items by name:
both are checked against this, so neither can name an entry that is not there.
}
unit menu_paths;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, source_scan;

{ The leaf entries of the designed main menu, as 'File > Import Profile'. An
  entry bound to an action reads the action's caption, as the running window
  does; a separator and anything hidden in the form file are left out. }
function DesignedMenuPaths: TStringArray;

{ A caption as text names it: no accelerator mark, no trailing ellipsis. }
function Plain(const ACaption: string): string;

{ The Caption the form file AFormFile (relative to the repository) gives the
  component AName, or '' when it has none. }
function DesignedCaption(const AFormFile, AName: string): string;

{ Every control that names an action the form file does not declare, as
  'ControlName -> ActionName'. }
function UndeclaredActionReferences: TStringArray;

{ The Hint the form file gives the action AName - what the status line shows
  while the pointer rests on its menu entry - or '' when it has none. }
function DesignedActionHint(const AName: string): string;

implementation

function Unquoted(const AValue: string): string;
var
    i: longint;
    InQuote: boolean;
begin
    //  An .lfm string: 'text', with '' for a quote and #39-style codes between
    //  quoted runs. Only what a menu caption uses is decoded.
    Result := '';
    InQuote := False;
    i := 1;
    while i <= Length(AValue) do
    begin
        if AValue[i] = '''' then
        begin
            if InQuote and (i < Length(AValue)) and (AValue[i + 1] = '''') then
            begin
                Result := Result + '''';
                Inc(i);
            end
            else
                InQuote := not InQuote;
        end
        else if InQuote then
            Result := Result + AValue[i];
        Inc(i);
    end;
end;

function Plain(const ACaption: string): string;
begin
    Result := StringReplace(ACaption, '&', '', [rfReplaceAll]);
    if Copy(Result, Length(Result) - 2, 3) = '...' then
        SetLength(Result, Length(Result) - 3);
    Result := Trim(Result);
end;

type
    TLfmObject = record
        Indent: longint;
        Name: string;
        ClassName_: string;
        Caption: string;
        Action: string;
        Visible: boolean;
        Children: longint;
    end;

function IndentOf(const ALine: string): longint;
begin
    Result := 0;
    while (Result < Length(ALine)) and (ALine[Result + 1] = ' ') do
        Inc(Result);
end;

function DesignedMenuPaths: TStringArray;
var
    Lines: TStringList;
    ActionCaptions, HiddenActions: TStringList;
    Stack: array of TLfmObject;
    Line, Trimmed, Path, Caption: string;
    i, k, Colon: longint;
    InMenu, Visible: boolean;
    Top: TLfmObject;
begin
    Result := nil;
    if RepoRoot = '' then
        raise Exception.Create('the repository root was not found');
    Lines := TStringList.Create;
    ActionCaptions := TStringList.Create;
    HiddenActions := TStringList.Create;
    try
        Lines.LoadFromFile(RepoRoot + 'Desktop' + PathDelim + 'Forms' +
            PathDelim + 'form_main.lfm');

        //  The actions first: the menu names them before the action list
        //  declares them.
        Stack := nil;
        for i := 0 to Lines.Count - 1 do
        begin
            Line := Lines[i];
            Trimmed := Trim(Line);
            if Copy(Trimmed, 1, 7) = 'object ' then
            begin
                SetLength(Stack, Length(Stack) + 1);
                Stack[High(Stack)] := Default(TLfmObject);
                Stack[High(Stack)].Indent := IndentOf(Line);
                Colon := Pos(':', Trimmed);
                Stack[High(Stack)].Name := Trim(Copy(Trimmed, 8, Colon - 8));
                Stack[High(Stack)].ClassName_ := Trim(Copy(Trimmed, Colon + 1, MaxInt));
            end
            else if (Trimmed = 'end') and (Length(Stack) > 0) and
                (Stack[High(Stack)].Indent = IndentOf(Line)) then
                SetLength(Stack, Length(Stack) - 1)
            else if (Length(Stack) > 0) and
                (Stack[High(Stack)].ClassName_ = 'TAction') and
                (IndentOf(Line) = Stack[High(Stack)].Indent + 2) then
            begin
                if Copy(Trimmed, 1, 10) = 'Caption = ' then
                    ActionCaptions.Values[Stack[High(Stack)].Name] :=
                        Unquoted(Copy(Trimmed, 11, MaxInt))
                else if Trimmed = 'Visible = False' then
                    HiddenActions.Add(Stack[High(Stack)].Name);
            end;
        end;

        Stack := nil;
        InMenu := False;
        for i := 0 to Lines.Count - 1 do
        begin
            Line := Lines[i];
            Trimmed := Trim(Line);
            if Copy(Trimmed, 1, 7) = 'object ' then
            begin
                SetLength(Stack, Length(Stack) + 1);
                Stack[High(Stack)] := Default(TLfmObject);
                Stack[High(Stack)].Indent := IndentOf(Line);
                Stack[High(Stack)].Visible := True;
                Colon := Pos(':', Trimmed);
                Stack[High(Stack)].Name := Trim(Copy(Trimmed, 8, Colon - 8));
                Stack[High(Stack)].ClassName_ := Trim(Copy(Trimmed, Colon + 1, MaxInt));
                if Stack[High(Stack)].ClassName_ = 'TMainMenu' then
                    InMenu := True;
                if Length(Stack) > 1 then
                    Inc(Stack[High(Stack) - 1].Children);
            end
            else if (Trimmed = 'end') and (Length(Stack) > 0) and
                (Stack[High(Stack)].Indent = IndentOf(Line)) then
            begin
                Top := Stack[High(Stack)];
                if Top.ClassName_ = 'TMainMenu' then
                    InMenu := False;
                if InMenu and (Top.ClassName_ = 'TMenuItem') and
                    (Top.Children = 0) then
                begin
                    Path := '';
                    Visible := True;
                    for k := 0 to High(Stack) do
                        if Stack[k].ClassName_ = 'TMenuItem' then
                        begin
                            Caption := Stack[k].Caption;
                            if Stack[k].Action <> '' then
                            begin
                                Caption := ActionCaptions.Values[Stack[k].Action];
                                if HiddenActions.IndexOf(Stack[k].Action) >= 0 then
                                    Visible := False;
                            end;
                            if not Stack[k].Visible then
                                Visible := False;
                            if Path <> '' then
                                Path := Path + ' > ';
                            Path := Path + Plain(Caption);
                        end;
                    if Visible and (Top.Caption <> '-') then
                    begin
                        SetLength(Result, Length(Result) + 1);
                        Result[High(Result)] := Path;
                    end;
                end;
                SetLength(Stack, Length(Stack) - 1);
            end
            else if (Length(Stack) > 0) and
                (IndentOf(Line) = Stack[High(Stack)].Indent + 2) then
            begin
                if Copy(Trimmed, 1, 10) = 'Caption = ' then
                    Stack[High(Stack)].Caption := Unquoted(Copy(Trimmed, 11, MaxInt))
                else if Copy(Trimmed, 1, 9) = 'Action = ' then
                    Stack[High(Stack)].Action := Trim(Copy(Trimmed, 10, MaxInt))
                else if Trimmed = 'Visible = False' then
                    Stack[High(Stack)].Visible := False;
            end;
        end;
    finally
        HiddenActions.Free;
        ActionCaptions.Free;
        Lines.Free;
    end;
end;

function DesignedCaption(const AFormFile, AName: string): string;
var
    Lines: TStringList;
    i, Indent: longint;
    Trimmed: string;
    Inside: boolean;
begin
    Result := '';
    if RepoRoot = '' then
        raise Exception.Create('the repository root was not found');
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(RepoRoot + StringReplace(AFormFile, '/', PathDelim,
            [rfReplaceAll]));
        Inside := False;
        Indent := 0;
        for i := 0 to Lines.Count - 1 do
        begin
            Trimmed := Trim(Lines[i]);
            if not Inside then
            begin
                if Copy(Trimmed, 1, Length('object ' + AName + ':')) =
                    'object ' + AName + ':' then
                begin
                    Inside := True;
                    Indent := IndentOf(Lines[i]);
                end;
            end
            else if (Trimmed = 'end') and (IndentOf(Lines[i]) = Indent) then
                Exit
            else if (IndentOf(Lines[i]) = Indent + 2) and
                (Copy(Trimmed, 1, 10) = 'Caption = ') then
                Result := Unquoted(Copy(Trimmed, 11, MaxInt));
        end;
    finally
        Lines.Free;
    end;
end;

function UndeclaredActionReferences: TStringArray;
var
    Lines, Declared: TStringList;
    i, Colon: longint;
    Trimmed, Owner: string;
begin
    Result := nil;
    if RepoRoot = '' then
        raise Exception.Create('the repository root was not found');
    Lines := TStringList.Create;
    Declared := TStringList.Create;
    try
        Lines.LoadFromFile(RepoRoot + 'Desktop' + PathDelim + 'Forms' +
            PathDelim + 'form_main.lfm');
        for i := 0 to Lines.Count - 1 do
        begin
            Trimmed := Trim(Lines[i]);
            if (Copy(Trimmed, 1, 7) = 'object ') and
                (Pos(': TAction', Trimmed) > 0) then
                Declared.Add(Trim(Copy(Trimmed, 8, Pos(':', Trimmed) - 8)));
        end;
        Owner := '';
        for i := 0 to Lines.Count - 1 do
        begin
            Trimmed := Trim(Lines[i]);
            if Copy(Trimmed, 1, 7) = 'object ' then
            begin
                Colon := Pos(':', Trimmed);
                Owner := Trim(Copy(Trimmed, 8, Colon - 8));
            end
            else if (Copy(Trimmed, 1, 9) = 'Action = ') and
                (Declared.IndexOf(Trim(Copy(Trimmed, 10, MaxInt))) < 0) then
            begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := Owner + ' -> ' +
                    Trim(Copy(Trimmed, 10, MaxInt));
            end;
        end;
    finally
        Declared.Free;
        Lines.Free;
    end;
end;

function DesignedActionHint(const AName: string): string;
var
    Lines: TStringList;
    i, Indent: longint;
    Trimmed: string;
    Inside: boolean;
begin
    Result := '';
    if RepoRoot = '' then
        raise Exception.Create('the repository root was not found');
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(RepoRoot + 'Desktop' + PathDelim + 'Forms' +
            PathDelim + 'form_main.lfm');
        Inside := False;
        Indent := 0;
        for i := 0 to Lines.Count - 1 do
        begin
            Trimmed := Trim(Lines[i]);
            if not Inside then
            begin
                if Trimmed = 'object ' + AName + ': TAction' then
                begin
                    Inside := True;
                    Indent := IndentOf(Lines[i]);
                end;
            end
            else if (Trimmed = 'end') and (IndentOf(Lines[i]) = Indent) then
                Exit
            else if Copy(Trimmed, 1, 7) = 'Hint = ' then
                Result := Unquoted(Copy(Trimmed, 8, MaxInt));
        end;
    finally
        Lines.Free;
    end;
end;

end.
