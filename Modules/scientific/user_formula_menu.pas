// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The formulas a user saved, and their place under Model > Curve Type
> User: one entry each, and a submenu to delete them.)

THE WINDOW KEPT ALL OF THIS UNTIL 2026-10-17 - the list, the files, the menu
entries and what choosing or deleting one does - and so named the scientific
module's dialogs and files in every product, one without the module included. It is this
module's now, declared through the window's seam for a group of the Curve Type
menu (curve_type_menu_extension) and acting through the host it is handed
(IUiHost), so the window names none of it. The rules it applies were already
pulled out where a test reaches them (user_curve_library); what moved is the
part that was the window's - the file reading and writing, and the gestures.

READ WHEN FIRST ASKED, not at registration: the front door runs in the compute
server too, which has no menu and must not read the user's settings folder.
}
unit user_formula_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Contnrs, int_ui_host, curve_type_menu_extension,
    named_points_set, saved_formula;

const
    { What the delete submenu is called, and named in the notices that send
      the user to it. }
    DeleteUserCurveCaption = 'Delete User Curve';

type
    TUserFormulaMenu = class(TObject, ICurveTypeMenuExtension)
    private
        FDir: string;
        FLoaded: boolean;
        FCurves: TComponentList;
        FSelected: Curve_type;
        procedure EnsureLoaded;
        function Find(const AId, APrefix: string): Curve_type;
        procedure Write(ACurve: Curve_type);
    public
        { ADir is where the formulas are kept, one *.cpr file each.
          SEAM: an empty ADir keeps them in memory only - nothing is read or
          written - so the gestures can be tested as unit tests, without a
          folder (testcase_user_formula_menu). The program always names its
          settings folder (UserFormulas). }
        constructor Create(const ADir: string);
        destructor Destroy; override;

        //  ICurveTypeMenuExtension
        function Group: string;
        function MenuItems: TUiMenuDeclArray;
        procedure Command(const AId: string; AHost: IUiHost);
        procedure CurveTypeSelected(const ATypeId: TCurveTypeId);
        function SelectedFormula: string;

        { What the definition dialogs do with a formula (int_curve_type_storage):
          a new one is kept and written, a changed one written again, one the
          user cancelled is removed with its file. }
        procedure Add(ACurve: Curve_type);
        procedure Update(ACurve: Curve_type);
        procedure Remove(ACurve: Curve_type);

        { The saved formulas, read on first use. }
        function Curves: TComponentList;
        property Selected: Curve_type read FSelected;
    end;

{ The one instance this program keeps, over its settings folder. }
function UserFormulas: TUserFormulaMenu;

implementation

uses
    Laz_XMLCfg, app_settings, log, curve_type_menu, int_curve_factory,
    curve_types_singleton, persistent_curve_parameters, user_curve_library;

const
    SelectPrefix = 'select:';
    DeletePrefix = 'delete:';
    DeleteSubmenuId = 'delete';

var
    Instance: TUserFormulaMenu;

function UserFormulas: TUserFormulaMenu;
begin
    if not Assigned(Instance) then
        Instance := TUserFormulaMenu.Create(GetConfigDir);
    Result := Instance;
end;

type
    { Resolves the class a saved file names - reading is handed a method. }
    TFormulaClassFinder = class
        procedure FindClass(Reader: TReader; const AClassName: string;
            var AComponentClass: TComponentClass);
    end;

procedure TFormulaClassFinder.FindClass(Reader: TReader;
    const AClassName: string; var AComponentClass: TComponentClass);
begin
    AComponentClass := SavedFormulaComponentClass(AClassName);
end;

constructor TUserFormulaMenu.Create(const ADir: string);
begin
    inherited Create;
    if ADir <> '' then
        FDir := IncludeTrailingPathDelimiter(ADir)
    else
        FDir := '';
    FCurves := TComponentList.Create(True);
end;

destructor TUserFormulaMenu.Destroy;
begin
    FCurves.Free;
    inherited;
end;

procedure TUserFormulaMenu.EnsureLoaded;
var
    SearchRec: TSearchRec;
    FileName: string;
    XMLConfig: TXMLConfig;
    CurveType: Curve_type;
    Finder: TFormulaClassFinder;
begin
    if FLoaded then
        Exit;
    FLoaded := True;
    //  No folder: nothing to read (the seam above).
    if FDir = '' then
        Exit;
    Finder := TFormulaClassFinder.Create;
    try
        if FindFirst(FDir + '*.cpr', faAnyFile, SearchRec) = 0 then
        begin
            repeat
                FileName := FDir + SearchRec.Name;
                XMLConfig := TXMLConfig.Create(FileName);
                try
                    //  Nil first: the reader fills a nil reference and frees
                    //  nothing it did not create.
                    CurveType := nil;
                    try
                        ReadComponentFromXMLConfig(XMLConfig, 'Component',
                            TComponent(CurveType), @Finder.FindClass, nil);
                        if not Assigned(CurveType) then
                            Continue;
                        if not UserCurveIsUsable(CurveType.Expression) then
                        begin
                            //  A formula with no expression can never be
                            //  selected; drop it and its stale file (such
                            //  files came from older builds).
                            FreeAndNil(CurveType);
                            SysUtils.DeleteFile(FileName);
                        end
                        else
                        begin
                            CurveType.FFileName := FileName;
                            FCurves.Add(CurveType);
                        end;
                    except
                        on E: Exception do
                        begin
                            CurveType.Free;
                            WriteLog('a saved formula could not be read from ' +
                                FileName + ': ' + E.Message, Warning);
                        end;
                    end;
                finally
                    XMLConfig.Free;
                end;
            until FindNext(SearchRec) <> 0;
            FindClose(SearchRec);
        end;
    finally
        Finder.Free;
    end;
end;

function TUserFormulaMenu.Curves: TComponentList;
begin
    EnsureLoaded;
    Result := FCurves;
end;

function TUserFormulaMenu.Group: string;
begin
    Result := UserCurveGroup;
end;

function TUserFormulaMenu.MenuItems: TUiMenuDeclArray;

    procedure AddDecl(const AId, AParent, ACaption: string; AKind: TUiMenuKind;
        AChecked: boolean);
    var
        D: TUiMenuDecl;
    begin
        D := Default(TUiMenuDecl);
        D.Id := AId;
        D.Parent := AParent;
        D.Caption := ACaption;
        D.Kind := AKind;
        D.Checked := AChecked;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := D;
    end;

var
    i: longint;
    Ct: Curve_type;
    Listed: boolean;
begin
    Result := nil;
    EnsureLoaded;
    Listed := False;
    for i := 0 to FCurves.Count - 1 do
        if UserCurveIsListed(Curve_type(FCurves[i])) then
            Listed := True;
    if not Listed then
        Exit;
    //  Below the type that creates one, a separator, then one entry per
    //  formula - ticked while it is the one in use - and the submenu that
    //  deletes them. NAMED BY THE OBJECT'S ADDRESS (user_curve_library.
    //  CurveWithTag): an entry built against a list that changed since
    //  matches nothing rather than the formula now in its place.
    AddDecl('separator', '', '-', mkSeparator, False);
    for i := 0 to FCurves.Count - 1 do
    begin
        Ct := Curve_type(FCurves[i]);
        if UserCurveIsListed(Ct) then
            AddDecl(SelectPrefix + IntToStr(PtrInt(Ct)), '', Ct.Name, mkToggle,
                Ct = FSelected);
    end;
    AddDecl(DeleteSubmenuId, '', DeleteUserCurveCaption, mkSubmenu, False);
    for i := 0 to FCurves.Count - 1 do
    begin
        Ct := Curve_type(FCurves[i]);
        if UserCurveIsListed(Ct) then
            AddDecl(DeletePrefix + IntToStr(PtrInt(Ct)), DeleteSubmenuId, Ct.Name,
                mkCommand, False);
    end;
end;

function TUserFormulaMenu.Find(const AId, APrefix: string): Curve_type;
var
    Tag: Int64;
begin
    Result := nil;
    if Copy(AId, 1, Length(APrefix)) <> APrefix then
        Exit;
    if not TryStrToInt64(Copy(AId, Length(APrefix) + 1, MaxInt), Tag) then
        Exit;
    Result := CurveWithTag(FCurves, PtrInt(Tag));
end;

procedure TUserFormulaMenu.Command(const AId: string; AHost: IUiHost);
var
    Ct: Curve_type;
    WasBeingFitted: boolean;
    Name_: string;
begin
    EnsureLoaded;
    Ct := Find(AId, SelectPrefix);
    if Assigned(Ct) then
    begin
        //  WHETHER IT CAN BE SELECTED AT ALL: a formula saved without its
        //  expression cannot become a curve, and selecting it used to fail an
        //  assertion in the optimiser rather than saying so here.
        if not UserCurveIsUsable(Ct.Expression) then
            AHost.ShowMessage('User-defined curve',
                UnusableCurveNotice(UserCurveGroup, DeleteUserCurveCaption),
                umWarning)
        else
        begin
            //  A COPY: the host takes ownership of what it is handed, and this
            //  formula keeps owning its own parameters.
            AHost.SelectFormula(Ct.Expression,
                Curve_parameters(Ct.Parameters.GetCopy));
            FSelected := Ct;
        end;
        //  The tick moves to the formula chosen.
        AHost.RefreshCurveTypeMenu;
        Exit;
    end;

    Ct := Find(AId, DeletePrefix);
    if not Assigned(Ct) then
        Exit;
    //  Deleting the formula CURRENTLY BEING FITTED leaves the model with a
    //  curve type that exists nowhere in the menu any more. The formula lives
    //  on the server, which knows nothing of this, so it is told - and the
    //  user is told why the next fit is refused.
    WasBeingFitted := DeletingLeavesTheModelWithoutACurveType(FSelected, Ct);
    Name_ := Ct.Name;
    FSelected := SelectionAfterDeleting(FSelected, Ct);
    SysUtils.DeleteFile(Ct.FFileName);
    FCurves.Remove(Ct);
    AHost.RefreshCurveTypeMenu;
    if WasBeingFitted then
    begin
        AHost.ForgetFormula;
        AHost.ShowMessageLater('User-defined curve',
            DeletedFittedCurveNotice(Name_), umInfo);
    end;
end;

procedure TUserFormulaMenu.CurveTypeSelected(const ATypeId: TCurveTypeId);
var
    Cls: TCurveClass;
begin
    EnsureLoaded;
    //  Choosing a formula type CREATED a formula through its dialogs, and
    //  that one is now in use; choosing any other type leaves none in use.
    //  Asked of the type chosen, not compared with one id: a build may hold
    //  more than one formula type (a test's, a module's).
    Cls := FindCurveClassById(ATypeId);
    if Assigned(Cls) and Cls.IsDefinedByFormula then
    begin
        FSelected := LastCreatedCurve(FCurves);
        if not Assigned(FSelected) then
            //  Not fatal - no entry shows as ticked - but it means the formula
            //  the dialogs reported creating never reached the list.
            WriteLog('a user formula was created but none is stored', Warning);
    end
    else
        FSelected := nil;
end;

function TUserFormulaMenu.SelectedFormula: string;
begin
    if Assigned(FSelected) then
        Result := FSelected.Expression
    else
        Result := '';
end;

procedure TUserFormulaMenu.Write(ACurve: Curve_type);
var
    XMLConfig: TXMLConfig;
begin
    //  No folder: nothing to write (the seam in Create).
    if FDir = '' then
        Exit;
    ACurve.FFileName := UserCurveFileName(FDir, Now);
    XMLConfig := TXMLConfig.Create(ACurve.FFileName);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', ACurve);
        XMLConfig.Flush;
    finally
        XMLConfig.Free;
    end;
end;

procedure TUserFormulaMenu.Add(ACurve: Curve_type);
begin
    EnsureLoaded;
    FCurves.Add(ACurve);
    Write(ACurve);
end;

procedure TUserFormulaMenu.Update(ACurve: Curve_type);
begin
    SysUtils.DeleteFile(ACurve.FFileName);
    Write(ACurve);
end;

procedure TUserFormulaMenu.Remove(ACurve: Curve_type);
begin
    EnsureLoaded;
    FSelected := SelectionAfterDeleting(FSelected, ACurve);
    SysUtils.DeleteFile(ACurve.FFileName);
    FCurves.Remove(ACurve);
end;

finalization
    Instance.Free;
end.
