// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(This machine's settings with their file in memory.)

A CLASS, NOT AN INTERFACE, so mock_support's lifetime rule is simply "the test
frees it": TMachineSettings put its disk behind three protected virtual methods
(the shape recent_project_store had) rather than behind an interface, and this
overrides them - so every owner of a section is driven through the real class.
}
unit mock_machine_settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, machine_settings;

type
    { What ReadText finds, what WriteText was last given, and how often an
      unreadable file was set aside. }
    TMemoryMachineSettings = class(TMachineSettings)
    protected
        function ReadText(out AText: string): boolean; override;
        procedure WriteText(const AText: string); override;
        procedure SetAsideUnreadable; override;
    public
        HasFile: boolean;
        Stored: string;
        Writes: longint;
        SetAside: longint;
        { Started on a machine whose file holds AStored, or that has no file at
          all when AHasFile is False. }
        constructor CreateWith(AHasFile: boolean; const AStored: string);
    end;

implementation

constructor TMemoryMachineSettings.CreateWith(AHasFile: boolean; const AStored: string);
begin
    HasFile := AHasFile;
    Stored := AStored;
    Writes := 0;
    SetAside := 0;
    inherited Create('memory');
end;

function TMemoryMachineSettings.ReadText(out AText: string): boolean;
begin
    AText := Stored;
    Result := HasFile;
end;

procedure TMemoryMachineSettings.WriteText(const AText: string);
begin
    Stored := AText;
    HasFile := True;
    Inc(Writes);
end;

procedure TMemoryMachineSettings.SetAsideUnreadable;
begin
    Inc(SetAside);
    HasFile := False;
    Stored := '';
end;

end.
