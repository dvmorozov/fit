// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of settings containers. 
Names of classes have non standard form because they 
are serialized into setting file.)

WHERE THE WINDOW'S SETTINGS ARE KEPT: the 'app' section of this machine's
settings.json (machine_settings), one key per published property of
Settings_v1 (WriteAppSettings, ReadAppSettings). They were config.xml, streamed
through the component writer below, until everything this machine remembers
became one file; that file is still READ once, by legacy_settings_import, and
never written. The component streaming stays for the curve types a user
defines (*.cpr), which are documents of their own rather than settings.

Copyright (C) Dmitry Morozov
}
unit app_settings;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, Contnrs, Laz_DOM, Laz_XMLCfg, Laz_XMLStreaming, LCLProc,
    mscr_specimen_list, persistent_curve_parameters, SysUtils, TypInfo,
    fpjson, machine_settings,

    //  The two weighting names, and what an unrecognised one means.
    fit_weighting,
    //  The size a pane with no stored size is drawn at, so the default lives
    //  in one place rather than in a literal here and another in the form.
    report_zoom;

type
    { Contains and serializes attributes of mathematical expression. }
    Curve_type = class(TComponent)
    private
        FName: string;
        FExpression: string;
        FParameters: Curve_parameters;

        procedure SetParameters(AParameters: Curve_parameters);


        function GetParams: TCollection;
        procedure SetParams(AParams: TCollection);

    public
        { File name to serialize / deserialize data. }
        FFileName: string;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure DefineProperties(Filer: TFiler); override;
        property Parameters: Curve_parameters read FParameters write SetParameters;

    published
        { Published properties are used in XML-serializing. }

        property Name: string read FName write FName;
        property Expression: string read FExpression write FExpression;
        //  By this way component is not written into XML-stream, we need to use DefineProperties.
        //property Params: Curve_parameters read FParams write FParams;
        { Expression parameters. }
        property Params: TCollection read GetParams write SetParams;
    end;

    { Contains and serializes application settings. }
    Settings_v1 = class(TComponent)
    private
        FCurveTypes: TComponentList;
        FReserved:   longint;
        FMinimizerKind: longint;
        FLossKind:   longint;
        FSelectedCurveType: string;
        FServerUrl:  string;
        FWeighting:  string;
        FDownloadFolder: string;
        FAnimationMode: boolean;
        FReportZoom: longint;
        FExplainZoom: longint;
        FTheme: string;


    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;
        { Does not work with XML-streams. }
        procedure DefineProperties(Filer: TFiler); override;

        property Curve_types: TComponentList
            read FCurveTypes write FCurveTypes;

    published
        { NO AXIS IS KEPT HERE. The axes are the project's: a choice kept in the
          settings followed the user from one project into the next - a price
          series opened on 2*Theta because a diffraction project had been on
          it. A project saves and restores its own (project_ui_context); a new
          one starts on Automatic. What older files still hold under ViewMode,
          ArgumentAxis and the rest is dropped on reading
          (DropUnpublishedProperties). }
        { Dummy property. Prevents exceptions in reading. }
        property Reserved: longint read FReserved write FReserved;
        { Persisted minimizer algorithm (MIN_KIND_* constant). }
        property MinimizerKind: longint read FMinimizerKind write FMinimizerKind;
        { Persisted objective (LOSS_KIND_* constant). 0 is the corrected
          R-factor, so a settings file written before this existed loads onto the
          right objective rather than an absent one. }
        property LossKind: longint read FLossKind write FLossKind;
        { NOTHING ABOUT PRICE DATA. Which column a .csv file is read by was
          kept here as PriceValueColumn and PriceArgument; it is the price-series
          module's choice now, kept in its own preferences
          (module_preferences), and an older file's two entries are dropped on
          reading (DropUnpublishedProperties). }
        { Persisted curve type, as a GUID string.
          A STRING, not the TGuid itself: only simple published types survive the
          settings writer, and a string is also readable in the file and stable
          if the id is ever re-issued. Empty means "never chosen", which is what
          an older settings file says, and the registry's default then applies -
          so upgrading does not silently move a user onto a different model. }
        property SelectedCurveType: string
            read FSelectedCurveType write FSelectedCurveType;
        { Persisted compute-server URL. Empty means the default address:
          server_connection.DefaultServerUrl, which follows FIT_PORT. }
        property ServerUrl: string read FServerUrl write FServerUrl;
        { Persisted residual weighting for the Python backend ('poisson'/'none'). }
        property Weighting: string read FWeighting write FWeighting;
        { NO PROJECT PATH IS KEPT HERE. The project to reopen and the list
          behind File > Open Recent name files on one machine, and the
          settings travel - a checkout's var/profile is copied between
          machines, a Windows profile roams. They are recent_project_store's,
          in this machine's own folder. }
        { Where a file fetched from a data source is kept.

          EMPTY MEANS THE DEFAULT - the per-user data directory app_data_root
          decides - rather than "nowhere": a machine's layout can change
          between sessions, and a path written out when the user never chose
          one would pin the old one. Set only when the user has chosen a
          folder in the wizard. }
        property DownloadFolder: string
            read FDownloadFolder write FDownloadFolder;
        { Whether a running fit redraws the model rather than charting its loss.
          False - which is what every settings file written before this existed
          says - leaves the live loss chart as the view a fit gets. }
        property AnimationMode: boolean read FAnimationMode write FAnimationMode;
        { How large the module report tabs and the Explain pane are drawn, as a
          percentage of the HTML component's own size - one of
          report_zoom.ReportZoomSteps.

          TWO NUMBERS, NOT ONE. Each pane is zoomed by its own two buttons, and
          a single stored size would move the pane the user did not click on.

          ZERO IS WHAT EVERY FILE WRITTEN BEFORE THIS EXISTED SAYS, and it is
          not a size: report_zoom.UsableZoom turns it - and anything else off
          the ladder - back into the default, so an older file and a
          hand-edited one both open at the size the application has always
          drawn. }
        property ReportZoom: longint read FReportZoom write FReportZoom;
        property ExplainZoom: longint read FExplainZoom write FExplainZoom;
        { View > Theme: app_theme.ThemeModeSetting's word for the mode -
          'system', 'light' or 'dark'. A WORD, not an ordinal, so the file
          reads as what the user chose and a reordered enumeration cannot move
          anyone. EMPTY IS WHAT EVERY FILE WRITTEN BEFORE THIS EXISTED SAYS, and
          ThemeModeFromSetting reads it, and anything it does not know, as
          Follow System - how such a file always opened. }
        property Theme: string read FTheme write FTheme;
    end;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
    var DestroyDriver: boolean): TReader;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    { Root component which are read from stream [in, out]. }
    var RootComponent: TComponent; OnFindComponentClass: TFindComponentClassEvent;
    { Owner of newly created component. }
    TheOwner: TComponent);

{ The class a component a settings file names as AClassName is read as - the
  settings themselves, or a user's curve type - or nil for anything else, which
  is then not read at all. Compared as Pascal compares names: in any case. }
function SettingsComponentClass(const AClassName: string): TComponentClass;

const
    { The section of this machine's settings.json the window's settings are. }
    AppSettingsSection = 'app';

{ Writes ASettings as AMachine's 'app' section: every property Settings_v1
  publishes beyond TComponent's own, by name. A key already there that this
  build does not publish - a newer build's - is kept. }
procedure WriteAppSettings(AMachine: TMachineSettings; ASettings: Settings_v1);

{ Reads into ASettings what AMachine's 'app' section holds. A key this build
  does not publish, and a value of the wrong kind, are passed over and leave
  the field as it was: nothing raises, so one odd entry never costs the rest. }
procedure ReadAppSettings(AMachine: TMachineSettings; ASettings: Settings_v1);

implementation

{$warnings off}
function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
    Append: boolean; var DestroyDriver: boolean): TWriter;
var
    Driver: TAbstractObjectWriter;
begin
    Driver := TXMLObjectWriter.Create(ADoc, Path, Append);
    DestroyDriver := True;
    Result := TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
    var DestroyDriver: boolean): TReader;
var
    p:      Pointer;
    Driver: TAbstractObjectReader;
    Stream: TMemoryStream;
begin
    Stream := TMemoryStream.Create;
    try
        Result := TReader.Create(Stream, 256);
        DestroyDriver := False;
        // hack to set a write protected variable.
        // DestroyDriver := True; TReader will free it
        Driver := TXMLObjectReader.Create(ADoc, Path);
        p      := @Result.Driver;
        Result.Driver.Free;
        TAbstractObjectReader(p^) := Driver;
    finally
        Stream := nil;
    end;
end;

{$warnings on}

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    AComponent: TComponent);
var
    Writer: TWriter;
    DestroyDriver: boolean;
begin
    Writer := nil;
    DestroyDriver := False;
    try
        Writer := CreateXMLWriter(XMLConfig.Document, Path, False, DestroyDriver);
        XMLConfig.Modified := True;
        Writer.WriteRootComponent(AComponent);
        XMLConfig.Flush;
    finally
        if DestroyDriver then
            Writer.Driver.Free;
        Writer.Free;
    end;
end;

{ Removes from the stored root component every property AClass does not
  publish - a field a NEWER build wrote, read by this one.

  WHY BEFORE READING, NOT WHILE. Reading raised "Unknown property" on the first
  such field, and the window's fallback then threw EVERY setting away - the
  last project, the recent list, the server - over a field that means nothing
  to this build. The reader's own escape, OnPropertyNotFound with Skip, does
  not work over this XML driver: TXMLObjectReader.SkipValue reads the value
  TYPE and leaves the reader one element past where it expects to be, so the
  next property raises "invalid ElementPosition" instead (Lazarus 3,
  laz_xmlstreaming.pas). So the stored document is pruned first, and only this
  build's copy of it: nothing is written back until the settings are saved. }
procedure DropUnpublishedProperties(XMLConfig: TXMLConfig; const Path: string;
    AClass: TClass);
var
    Node, Props, Child, Next: TDOMNode;
    Name_: string;
begin
    Node := XMLConfig.Document.DocumentElement.FindNode(Path);
    if not Assigned(Node) then
        Exit;
    Node := Node.FindNode('component');
    if not Assigned(Node) then
        Exit;
    Props := Node.FindNode('properties');
    if not Assigned(Props) then
        Exit;
    Child := Props.FirstChild;
    while Assigned(Child) do
    begin
        Next := Child.NextSibling;
        if Child is TDOMElement then
        begin
            Name_ := TDOMElement(Child).GetAttribute('name');
            if (Name_ <> '') and not Assigned(GetPropInfo(AClass, Name_)) then
                Props.RemoveChild(Child).Free;
        end;
        Child := Next;
    end;
end;

procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
    var RootComponent: TComponent; OnFindComponentClass: TFindComponentClassEvent;
    TheOwner: TComponent);
var
    DestroyDriver: boolean;
    Reader:      TReader;
    IsInherited: boolean;
    AClassName:  string;
    AClass:      TComponentClass;
begin
    Reader := nil;
    DestroyDriver := False;
    try
        Reader := CreateXMLReader(XMLConfig.Document, Path, DestroyDriver);
        Reader.OnFindComponentClass := OnFindComponentClass;

        // get root class
        AClassName := (Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
        if IsInherited then
            // inherited is not supported by this simple function
            raise Exception.Create('ReadComponentFromXMLConfig: ' +
                '"inherited" is not supported by this function');

        AClass := nil;
        //  poisk tipa klassa po imeni klassa
        OnFindComponentClass(nil, AClassName, AClass);
        if AClass = nil then
            raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);
        //  What a newer build stored and this one does not know, left out.
        DropUnpublishedProperties(XMLConfig, Path, AClass);

        if RootComponent = nil then
        begin
            // create root component
            // first create the new instance and set the variable ...
            RootComponent := AClass.NewInstance as TComponent;
            // then call the constructor
            RootComponent.Create(TheOwner);
        end
        else
        if not RootComponent.InheritsFrom(AClass) then
            raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                [AClassName, RootComponent.ClassName])
        // there is a root component, check if class is compatible
        ;

        Reader.ReadRootComponent(RootComponent);
    finally
        if DestroyDriver then
            Reader.Driver.Free;
        Reader.Free;
    end;
end;

{================================ Settings_v1 =================================}

constructor Settings_v1.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    FCurveTypes := TComponentList.Create;
    FReserved   := -1;
    FMinimizerKind := 0;   //  MIN_KIND_DHS - original Downhill Simplex by default
    FLossKind := 0;        //  LOSS_KIND_RFACTOR - the corrected, data-normalised form
    //  Empty, not a hard-coded id: "the user has never chosen" must be
    //  distinguishable from "the user chose this one".
    FSelectedCurveType := '';
    FServerUrl := '';      //  the default address; see DefaultServerUrl
    FWeighting := WEIGHTING_POISSON;
    //  The size the HTML component draws at on its own, so a first run looks
    //  exactly like every build before the zoom buttons existed.
    FReportZoom := ReportZoomDefault;
    FExplainZoom := ReportZoomDefault;
end;

destructor Settings_v1.Destroy;
begin
    FCurveTypes.Free;
    inherited Destroy;
end;

{ EMPTY ON PURPOSE, AND NOT A STUB - do not delete it for looking like one.

  TComponent.DefineProperties defines the pseudo-properties Left and Top out of
  DesignInfo. Not calling inherited is what keeps a designer's idea of where a
  component sat out of a settings file, which has no window in it and no use for
  one. (At the DesignInfo a runtime-created object has, the base class would omit
  them anyway - so this is a statement of intent about the file format rather
  than something that changes today's bytes.)

  It used to carry a commented-out DefineProperty for the curve-type list, with
  a note saying the filer cannot carry a TComponentList through an XML stream,
  and the two read/write callbacks it named. Those callbacks had no other
  reference in any repository - a commented-out line is not a caller - so they
  were four methods that could never run. The curve-type list is rebuilt by the
  application instead, which TSettingsModelTest.CurveTypesAreNotPersisted pins
  so nobody reads the empty list on the far side as data loss. }
procedure Settings_v1.DefineProperties(Filer: TFiler);
begin
end;

{================================ Curve_type ==================================}

constructor Curve_type.Create(AOwner: TComponent);
begin
    inherited;
    Parameters := Curve_parameters.Create(nil);
end;

destructor Curve_type.Destroy;
begin
    Parameters.Free;
    inherited;
end;

procedure Curve_type.SetParameters(AParameters: Curve_parameters);
begin
    FParameters.Free;
    FParameters := AParameters;
end;

{ Empty on purpose, exactly as Settings_v1's is - see the comment there.

  The parameters ARE persisted, through the published Params property rather
  than through a filer callback; testcase_curve_params_persistence drives that
  path. The two callbacks this override used to name in a commented-out line
  have been deleted with it. }
procedure Curve_type.DefineProperties(Filer: TFiler);
begin
end;

function Curve_type.GetParams: TCollection;
begin
    Result := Parameters.Params;
end;

procedure Curve_type.SetParams(AParams: TCollection);
begin
    Parameters.Params.Free;
    Parameters.Params := AParams;
end;

function SettingsComponentClass(const AClassName: string): TComponentClass;
begin
    if CompareText(AClassName, 'Settings_v1') = 0 then
        Result := Settings_v1
    else if CompareText(AClassName, 'Curve_type') = 0 then
        Result := Curve_type
    else
        Result := nil;
end;

{ The properties a settings file carries: published by ASettings' class and
  not by TComponent, whose Name and Tag are a form designer's. }
function IsSettingsProperty(AInfo: PPropInfo): boolean;
begin
    Result := not Assigned(GetPropInfo(TComponent, AInfo^.Name));
end;

procedure WriteAppSettings(AMachine: TMachineSettings; ASettings: Settings_v1);
var
    Section: TJSONObject;
    Props: PPropList;
    Count, i, j: longint;
    Info: PPropInfo;
begin
    //  THE SECTION AS FOUND, so a newer build's keys survive an older one.
    Section := AMachine.Section(AppSettingsSection);
    Count := GetPropList(ASettings, Props);
    try
        for i := 0 to Count - 1 do
        begin
            Info := Props^[i];
            if not IsSettingsProperty(Info) then
                Continue;
            //  ONE ENTRY PER KEY in whatever case the file spelled it.
            for j := Section.Count - 1 downto 0 do
                if CompareText(Section.Names[j], Info^.Name) = 0 then
                    Section.Delete(j);
            case Info^.PropType^.Kind of
                tkInteger, tkInt64:
                    Section.Add(Info^.Name, GetOrdProp(ASettings, Info));
                tkBool:
                    Section.Add(Info^.Name, GetOrdProp(ASettings, Info) <> 0);
                tkSString, tkLString, tkAString, tkUString, tkWString:
                    Section.Add(Info^.Name, GetStrProp(ASettings, Info));
            end;
        end;
    finally
        FreeMem(Props);
    end;
    AMachine.ReplaceSection(AppSettingsSection, Section);
end;

procedure ReadAppSettings(AMachine: TMachineSettings; ASettings: Settings_v1);
var
    Section: TJSONObject;
    Value: TJSONData;
    Info: PPropInfo;
    i: longint;
begin
    Section := AMachine.Section(AppSettingsSection);
    try
        for i := 0 to Section.Count - 1 do
        begin
            //  In any case, as Pascal names a property.
            Info := GetPropInfo(ASettings, Section.Names[i]);
            if not Assigned(Info) or not IsSettingsProperty(Info) then
                Continue;
            Value := Section.Items[i];
            case Info^.PropType^.Kind of
                tkInteger, tkInt64:
                    if Value is TJSONIntegerNumber then
                        SetOrdProp(ASettings, Info, Value.AsInteger)
                    else if Value is TJSONInt64Number then
                        SetOrdProp(ASettings, Info, Value.AsInt64);
                tkBool:
                    if Value is TJSONBoolean then
                        SetOrdProp(ASettings, Info, Ord(Value.AsBoolean));
                tkSString, tkLString, tkAString, tkUString, tkWString:
                    if Value is TJSONString then
                        SetStrProp(ASettings, Info, Value.AsString);
            end;
        end;
    finally
        Section.Free;
    end;
end;

initialization

end.
