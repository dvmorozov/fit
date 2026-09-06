// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of settings containers. 
Names of classes have non standard form because they 
are serialized into setting file.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
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
    //  The two weighting names, and what an unrecognised one means.
    fit_weighting;

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
        FViewMode:   longint;
        FViewModeChosenByUser: boolean;
        FMinimizerKind: longint;
        FLossKind:   longint;
        FSelectedCurveType: string;
        FServerUrl:  string;
        FWeighting:  string;
        FCustomAxisName, FCustomAxisUnit: string;
        FLastProjectFile: string;
        FRecentProjects: string;
        FCustomAxisForward, FCustomAxisInverse: string;


    public
        constructor Create(Owner: TComponent); override;
        destructor Destroy; override;
        { Does not work with XML-streams. }
        procedure DefineProperties(Filer: TFiler); override;

        property Curve_types: TComponentList
            read FCurveTypes write FCurveTypes;

    published
        { Dummy property. Prevents exceptions in reading. }
        property Reserved: longint read FReserved write FReserved;
        { Persisted argument axis / display mode (XCM_* constant). }
        property ViewMode: longint read FViewMode write FViewMode;
        { True once the user has picked an axis from the menu. False - including
          in every settings file written before this existed - means "never
          chosen", and then the axis the selected curve type defines is used
          instead of ViewMode. Without this flag an old file's ViewMode = 0 (the
          former hard-coded default) is indistinguishable from a deliberate
          choice of 2*Theta. }
        property ViewModeChosenByUser: boolean
            read FViewModeChosenByUser write FViewModeChosenByUser;
        { Persisted minimizer algorithm (MIN_KIND_* constant). }
        property MinimizerKind: longint read FMinimizerKind write FMinimizerKind;
        { Persisted objective (LOSS_KIND_* constant). 0 is the corrected
          R-factor, so a settings file written before this existed loads onto the
          right objective rather than an absent one. }
        property LossKind: longint read FLossKind write FLossKind;
        { Persisted curve type, as a GUID string.
          A STRING, not the TGuid itself: only simple published types survive the
          settings writer, and a string is also readable in the file and stable
          if the id is ever re-issued. Empty means "never chosen", which is what
          an older settings file says, and the registry's default then applies -
          so upgrading does not silently move a user onto a different model. }
        property SelectedCurveType: string
            read FSelectedCurveType write FSelectedCurveType;
        { Persisted compute-server URL. Empty = fit in-process (default). }
        property ServerUrl: string read FServerUrl write FServerUrl;
        { Persisted residual weighting for the Python backend ('poisson'/'none'). }
        property Weighting: string read FWeighting write FWeighting;
        { The project last open, offered again at start-up.

          HERE AND NOT IN THE PROJECT, obviously - but worth saying why it is in
          the settings at all: it is a per-machine convenience, like the server
          URL beside it. A project that recorded which project to open next
          would be a document with an opinion about the application. Empty means
          none has been opened, which is what every settings file written before
          this existed says. }
        property LastProjectFile: string
            read FLastProjectFile write FLastProjectFile;
        { The projects File > Open Recent offers, most recent first, as ONE
          string - recent_project owns its shape and its separator. One field
          because only simple published types survive this writer, and a list
          of components would put the menu's contents into the class layout,
          which is what the project file itself refuses to do. Empty means
          none has been opened, which is what every settings file written
          before this existed says. }
        property RecentProjects: string
            read FRecentProjects write FRecentProjects;
        { Persisted user-defined (XCM_CUSTOM) axis definition. }
        property CustomAxisName: string read FCustomAxisName write FCustomAxisName;
        property CustomAxisUnit: string read FCustomAxisUnit write FCustomAxisUnit;
        property CustomAxisForward: string
            read FCustomAxisForward write FCustomAxisForward;
        property CustomAxisInverse: string
            read FCustomAxisInverse write FCustomAxisInverse;
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
    //  XCM_CURVE: the axis is the one the selected curve type defines, until the
    //  user overrides it from the menu (which is what the flag below records).
    FViewMode   := XCM_CURVE;
    FViewModeChosenByUser := False;
    FMinimizerKind := 0;   //  MIN_KIND_DHS - original Downhill Simplex by default
    FLossKind := 0;        //  LOSS_KIND_RFACTOR - the corrected, data-normalised form
    //  Empty, not a hard-coded id: "the user has never chosen" must be
    //  distinguishable from "the user chose this one".
    FSelectedCurveType := '';
    FServerUrl := '';      //  in-process by default; no server required
    FWeighting := WEIGHTING_POISSON;
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

initialization
end.
