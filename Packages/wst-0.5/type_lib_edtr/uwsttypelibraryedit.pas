{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit uwsttypelibraryedit;

//{$DEFINE WST_IDE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, ActnList,
  pastree, pascal_parser_intf, logger_intf,
  SynHighlighterPas, SynEdit, StdCtrls, SynHighlighterXML
  {$IFDEF WST_IDE},ProjectIntf{$ENDIF};

type

  { TfWstTypeLibraryEdit }

  TfWstTypeLibraryEdit = class(TForm)
    actExit: TAction;
    actExport: TAction;
    actAbout: TAction;
    actEnumCreate: TAction;
    actCompoundCreate: TAction;
    actIntfCreate: TAction;
    actFullExpand: TAction;
    actFullCollapse: TAction;
    actDelete : TAction;
    actArrayCreate : TAction;
    actTypeALiasCreate : TAction;
    actSave : TAction;
    actNewFile: TAction;
    actRefreshView: TAction;
    actUpdateObject: TAction;
    actSaveAs: TAction;
    actOpenFile: TAction;
    AL: TActionList;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33 : TMenuItem;
    MenuItem34 : TMenuItem;
    MenuItem35 : TMenuItem;
    MenuItem36 : TMenuItem;
    MenuItem37 : TMenuItem;
    MenuItem38 : TMenuItem;
    MenuItem39 : TMenuItem;
    MenuItem40 : TMenuItem;
    MenuItem41 : TMenuItem;
    MenuItem42 : TMenuItem;
    MenuItem43 : TMenuItem;
    MenuItem44 : TMenuItem;
    MenuItem45 : TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7 : TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmoLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OD: TOpenDialog;
    PC: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SD: TSaveDialog;
    Splitter1: TSplitter;
    srcInterface: TSynEdit;
    SB: TStatusBar;
    srcImp: TSynEdit;
    srcBinder: TSynEdit;
    srcProxy: TSynEdit;
    srcWSDL: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    SynXMLSyn1: TSynXMLSyn;
    tsWSDL: TTabSheet;
    tsLog: TTabSheet;
    tsImp: TTabSheet;
    tsBinder: TTabSheet;
    tsInterface: TTabSheet;
    tsProxy: TTabSheet;
    trvSchema: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actArrayCreateExecute(Sender : TObject);
    procedure actCompoundCreateExecute(Sender: TObject);
    procedure actDeleteExecute (Sender : TObject );
    procedure actEnumCreateExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actExportUpdate(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure actFullExpandExecute(Sender: TObject);
    procedure actIntfCreateExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRefreshViewExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute (Sender : TObject );
    procedure actTypeALiasCreateExecute(Sender : TObject);
    procedure actUpdateObjectExecute(Sender: TObject);
    procedure actUpdateObjectUpdate(Sender: TObject);
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormShow(Sender: TObject);
  private
    FSymbolTable : TwstPasTreeContainer;
    FStatusMessageTag : PtrInt;
    FCurrentFileName : string;
    {$IFDEF WST_IDE}FProjectLibrary : TLazProjectFile;{$ENDIF}
  private
    function GetTypeNode() : TTreeNode;
    function GetInterfaceNode() : TTreeNode;
  private
    procedure ShowStatusMessage(const AMsgType : TMessageType;const AMsg : string);
    procedure RenderSymbols();
    procedure RenderSources();
    procedure RenderWSDL();
    procedure OpenFile(const AFileName : string; const AContent : TStream = nil);
    procedure SaveToFile(const AFileName : string);
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
  end; 

var
  fWstTypeLibraryEdit: TfWstTypeLibraryEdit;

implementation
uses view_helper, DOM, XMLRead, XMLWrite, //HeapTrc,
     wsdl2pas_imp, source_utils, command_line_parser, generator, metadata_generator,
     binary_streamer, wst_resources_utils, wsdl_generator,
     uabout, edit_helper, udm, ufrmsaveoption, pparser
     {$IFDEF WST_IDE},LazIDEIntf,IDEMsgIntf{$ENDIF};


{$IFDEF WST_IDE}
function GetCurrentProjectLibraryFile():TLazProjectFile;
var
  i, c : Integer;
begin
  Result := nil;
  c := LazarusIDE.ActiveProject.FileCount;
  for i := 0 to Pred(c) do begin
    if AnsiSameText('.wsdl',ExtractFileExt(LazarusIDE.ActiveProject.Files[i].Filename)) then begin
      Result := LazarusIDE.ActiveProject.Files[i];
      Break;
    end;
  end;
end;
{$ENDIF}

const
  DEF_FILE_NAME = 'library1';
  sWST_META = 'wst_meta';
  
type
  TSourceType = cloInterface .. cloBinder;
  TSourceTypes = set of TSourceType;

function ParsePascalFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
) : TwstPasTreeContainer;overload;
const
  s_ostype =
    {$IFDEF WINDOWS}
      'WINDOWS'
    {$ELSE}
      {$IFDEF LINUX}
        'LINUX'
      {$ELSE}
        ''
      {$ENDIF}
    {$ENDIF};
    
  procedure DoNotify(const AMsgType : TMessageType; const AMsg : string);
  begin
    if ( ANotifier <> nil ) then
      ANotifier(AMsgType,AMsg);
  end;
  
var
  symName : string;
begin
  symName := ChangeFileExt(ExtractFileName(AFileName),'');
  if ( symName[Length(symName)] = '.' ) then begin
    Delete(symName,Length(symName),1);
  end;
  Result := TwstPasTreeContainer.Create();
  try
    DoNotify(mtInfo,Format('Parsing file %s ...',[AFileName]));
    CreateWstInterfaceSymbolTable(Result);
    ParseSource(Result,AFileName,s_ostype,'');
    DoNotify(mtInfo,Format('File parsed %s .',[AFileName]));
  except
    on e : Exception do begin
      FreeAndNil(Result);
      DoNotify(mtError,e.Message);
      raise;
    end;
  end;
end;

function ParseWsdlFile(
  const AFileName : string;
        AContent  : TStream;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
var
  locDoc : TXMLDocument;
  prsr : TWsdlParser;
  symName : string;
begin
  Result := nil;
  symName := ChangeFileExt(ExtractFileName(AFileName),'');
  if ( symName[Length(symName)] = '.' ) then begin
    Delete(symName,Length(symName),1);
  end;
  prsr := nil;
  ReadXMLFile(locDoc,AContent);
  try
    Result := TwstPasTreeContainer.Create();
    try
      prsr := TWsdlParser.Create(locDoc,Result);
      prsr.OnMessage := ANotifier;
      prsr.Parse(pmAllTypes,symName);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    FreeAndNil(prsr);
    FreeAndNil(locDoc);
  end;
end;

function ParseWsdlFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
var
  locContent : TMemoryStream;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    locContent := TMemoryStream.Create();
    try
      locContent.LoadFromFile(AFileName);
      locContent.Position := 0;
      Result := ParseWsdlFile(AFileName,locContent,ANotifier);
    finally
      FreeAndNil(locContent);
    end;
  end;
end;

type TOutputType = ( otMemory, otFileSystem );
function GenerateSource(
        ASymbolTable : TwstPasTreeContainer;
        AOptions     : TSourceTypes;
  const AOutputType  : TOutputType;
  const AOutPath     : string;
  const ANotifier    : TOnParserMessage
) : ISourceManager;

  procedure Notify(const AMsg : string);
  begin
    if Assigned(ANotifier) then begin
      ANotifier(mtInfo, AMsg);
    end;
  end;
  
var
  mtdaFS: TMemoryStream;
  g : TBaseGenerator;
  mg : TMetadataGenerator;
  rsrcStrm : TMemoryStream;
begin
  Result := CreateSourceManager();
  rsrcStrm := nil;
  mtdaFS := nil;
  mg := nil;
  g := Nil;
  try
  
    if ( cloInterface in AOptions ) then begin
      Notify('Interface file generation...');
      g := TInftGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloProxy in AOptions ) then begin
      Notify('Proxy file generation...');
      g := TProxyGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloBinder in AOptions ) then begin
      Notify('Binder file generation...');
      g := TBinderGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloImp in AOptions ) then begin
      Notify('Implementation file generation...');
      g := TImplementationGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( AOutputType = otFileSystem ) and ( [cloBinder,cloProxy]*AOptions  <> [] ) then begin
      Notify('Metadata file generation...');
      mtdaFS := TMemoryStream.Create();
      mg := TMetadataGenerator.Create(ASymbolTable,CreateBinaryWriter(mtdaFS));
      mg.Execute();
      mtdaFS.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_META]));
      rsrcStrm := TMemoryStream.Create();
      mtdaFS.Position := 0;
      BinToWstRessource(UpperCase(ASymbolTable.CurrentModule.Name),mtdaFS,rsrcStrm);
      rsrcStrm.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_EXTENSION]));
    end;
    
    if ( AOutputType = otFileSystem ) then begin
      Result.SaveToFile(AOutPath);
    end;
  finally
    rsrcStrm.Free();
    mg.Free();;
    mtdaFS.Free();;
    g.Free();
  end;
end;

procedure GenerateWSDL_ToStream(ASymbol : TwstPasTreeContainer; ADest : TStream);
var
  doc : TXMLDocument;
begin
  doc := TXMLDocument.Create();
  try
    GenerateWSDL(ASymbol,doc);
    WriteXML(doc,ADest);
  finally
    FreeAndNil(doc);
  end;
end;

function CreateSymbolTable(const AName : string):TwstPasTreeContainer ;
begin
  Result := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(Result);
    Result.CreateElement(TPasModule,AName,Result.Package,visDefault,'',0);
    Result.CurrentModule.InterfaceSection := TPasSection(Result.CreateElement(TPasSection,'',Result.CurrentModule,visDefault,'',0));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TfWstTypeLibraryEdit }

procedure TfWstTypeLibraryEdit.actOpenFileExecute(Sender: TObject);
begin
  if OD.Execute() then begin
    OpenFile(OD.FileName);
  end;
end;

procedure TfWstTypeLibraryEdit.actRefreshViewExecute(Sender: TObject);
begin
  RenderSymbols();
end;

procedure TfWstTypeLibraryEdit.actSaveAsExecute(Sender: TObject);
begin
  if SD.Execute() then begin
    SaveToFile(SD.FileName);
    FCurrentFileName := SD.FileName;
  end;
end;

procedure TfWstTypeLibraryEdit.actSaveExecute (Sender : TObject );
begin
  if FileExists(FCurrentFileName) then
    SaveToFile(FCurrentFileName)
  else
    actSaveAs.Execute() ;
end;

procedure TfWstTypeLibraryEdit.actTypeALiasCreateExecute(Sender : TObject);
var
  e : TPasAliasType;
begin
  e := CreateAliasType(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actUpdateObjectExecute(Sender: TObject);
var
  o : TPasElement;
  nd, nd_1 : TTreeNode;
begin
  nd := trvSchema.Selected;
  if Assigned(nd) and Assigned(nd.Data) then begin
    o := TPasElement(nd.Data);
    if HasEditor(o) then begin
      UpdateObject(o,FSymbolTable);
      nd_1  := nd;
      trvSchema.BeginUpdate();
      try
        nd := FindPainter(o).Paint(FSymbolTable,o,GetTypeNode());
        nd.MoveTo(nd_1,naInsertBehind);
        FreeAndNil(nd_1);
      finally
        trvSchema.EndUpdate();
      end;
    end;
  end;
end;

procedure TfWstTypeLibraryEdit.actUpdateObjectUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(trvSchema.Selected) and
    Assigned(trvSchema.Selected.Data) and
    HasEditor(TPasElement(trvSchema.Selected.Data));
end;

procedure TfWstTypeLibraryEdit.FormClose (Sender : TObject; var CloseAction : TCloseAction );
var
  dlgRes : Integer;
  {$IFDEF WST_IDE}
  prjFile : TLazProjectFile;
  {$ENDIF}
begin
  dlgRes := MessageDlg(Self.Caption,'Save the file before exit ?',mtConfirmation,mbYesNo,0);
  if ( dlgRes = mrYes ) then begin
    actSave.Execute();
  end;
  {$IFDEF WST_IDE}
  if ( FProjectLibrary = nil ) then begin
    prjFile := GetCurrentProjectLibraryFile();
    if ( prjFile = nil ) then begin
      dlgRes := MessageDlg(Self.Caption,'Add this type library to the current project ?',mtConfirmation,mbYesNo,0);
      if ( dlgRes = mrYes ) then begin
        LazarusIDE.DoOpenEditorFile(FCurrentFileName,-1,[ofAddToProject]);
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TfWstTypeLibraryEdit.FormShow(Sender: TObject);
{$IFDEF WST_IDE}
var
  prjFile : TLazProjectFile;
  locContent : TMemoryStream;
{$ENDIF}
begin
{$IFDEF WST_IDE}
  prjFile := GetCurrentProjectLibraryFile();
  if ( prjFile <> nil ) then begin
    locContent := TMemoryStream.Create();
    try
      locContent.LoadFromFile(prjFile.FileName);
      if ( locContent.Size > 0 ) then begin
        locContent.Position := 0;
        IDEMessagesWindow.AddMsg(Format('Parsing %s...',[prjFile.Filename]),ExtractFileDir(prjFile.Filename),0);
        OpenFile(prjFile.Filename,locContent);
        IDEMessagesWindow.AddMsg(Format('File Parsed %s.',[prjFile.Filename]),ExtractFileDir(prjFile.Filename),0);
        FProjectLibrary := prjFile;
      end;
    finally
      FreeAndNil(locContent);
    end;
  end;
{$ENDIF}
  RenderSymbols();
end;

function TfWstTypeLibraryEdit.GetTypeNode(): TTreeNode;
begin
  Result := trvSchema.TopItem.GetFirstChild().Items[0];
end;

function TfWstTypeLibraryEdit.GetInterfaceNode(): TTreeNode;
begin
  Result := trvSchema.TopItem.GetFirstChild().Items[1];
end;

procedure TfWstTypeLibraryEdit.ShowStatusMessage(const AMsgType : TMessageType;const AMsg: string);
begin
  mmoLog.Lines.Add(Format('%s : %s',[MessageTypeNames[AMsgType],AMsg]));
  SB.Panels[1].Text := AMsg;
  Inc(FStatusMessageTag);
  if ( (FStatusMessageTag) > 23 ) then begin
    FStatusMessageTag := 0;
    Application.ProcessMessages();
  end;
end;

procedure TfWstTypeLibraryEdit.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TfWstTypeLibraryEdit.actAboutExecute(Sender: TObject);
var
  fa : TfAbout;
begin
  fa := TfAbout.Create(Self);
  try
    fa.ShowModal();
  finally
    fa.Release();
  end;
end;

procedure TfWstTypeLibraryEdit.actArrayCreateExecute(Sender : TObject);
var
  e : TPasArrayType;
begin
  e := CreateArray(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actCompoundCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateCompoundObject(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actDeleteExecute (Sender : TObject );
var
  o : TPasElement;
  nd : TTreeNode;
begin
  if ( MessageDlg('Delete the select object ?',mtConfirmation,mbYesNo,0) = mrYes ) then begin
    nd := trvSchema.Selected;
    if Assigned(nd) and Assigned(nd.Data) then begin
      o := TPasElement(nd.Data);
      if HasEditor(o) then begin
        DeleteObject(o,FSymbolTable);
        trvSchema.BeginUpdate();
        try
          FreeAndNil(nd);
        finally
          trvSchema.EndUpdate();
        end;
      end;
    end;
  end;
end;

procedure TfWstTypeLibraryEdit.actEnumCreateExecute(Sender: TObject);
var
  e : TPasEnumType;
begin
  e := CreateEnum(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actExportExecute(Sender: TObject);
var
  curLok : IInterface;
  saveOpts : TComandLineOptions;
  f : TfrmSaveOptions;
begin
  f := TfrmSaveOptions.Create(nil);
  try
    if ( f.ShowModal() = mrOK ) then begin
      saveOpts := [];
      if f.edtBinder.Checked then
        Include(saveOpts,cloBinder);
      if f.edtInterface.Checked then
        Include(saveOpts,cloInterface);
      if f.edtImplementation.Checked then
        Include(saveOpts,cloImp);
      if f.edtProxy.Checked then
        Include(saveOpts,cloProxy);

      curLok := SetCursorHourGlass();
      GenerateSource(
        FSymbolTable,
        saveOpts,
        otFileSystem,
        IncludeTrailingBackslash(f.edtOutputDir.Text),
        nil
      );
      curLok := nil;
    end;
  finally
    f.Release();
  end;
end;

procedure TfWstTypeLibraryEdit.actExportUpdate(Sender: TObject);
begin
  //TAction(Sender).Enabled := Assigned(FSymbolTable) and ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 );
end;

procedure TfWstTypeLibraryEdit.actFullCollapseExecute(Sender: TObject);
begin
  trvSchema.FullCollapse();
end;

procedure TfWstTypeLibraryEdit.actFullExpandExecute(Sender: TObject);
begin
  trvSchema.FullExpand();
end;

procedure TfWstTypeLibraryEdit.actIntfCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateInterface(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetInterfaceNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actNewFileExecute(Sender: TObject);
var
  res : Integer;
begin
  res := MessageDlg(Application.Title,'Save the current file before ?',mtConfirmation,mbYesNoCancel,0,mbYes);
  if ( res = mrCancel ) then begin
    Exit;
  end;
  if ( res = mrYes ) then begin
    actSave.Execute();
  end;
  FCurrentFileName := DEF_FILE_NAME;
  {$IFDEF WST_IDE}
  FProjectLibrary := nil;
  {$ENDIF}
  FreeAndNil(FSymbolTable);
  FSymbolTable := CreateSymbolTable(ExtractFileName(FCurrentFileName));
  RenderSymbols();
end;

procedure TfWstTypeLibraryEdit.RenderSymbols();
var
  objPtr : ISymbolPainter;
  nd : TTreeNode;
begin
  trvSchema.BeginUpdate();
  try
    trvSchema.Items.Clear();
    srcInterface.ClearAll();
    nd := trvSchema.Items.AddFirst(nil,'Type Library Editor');
    nd.ImageIndex := -1;
    nd.StateIndex := -1;
    nd.SelectedIndex := -1;
    if Assigned(FSymbolTable) then begin
      objPtr := FindPainter(FSymbolTable.Package);
      if Assigned(objPtr) then begin
        objPtr.Paint(FSymbolTable,FSymbolTable.Package,trvSchema.TopItem);
      end;
      RenderSources();
      RenderWSDL();
    end;
    trvSchema.Items[0].Expand(False);
    trvSchema.Items[0].Items[0].Expand(False);
  finally
    trvSchema.EndUpdate();
  end;
  ShowStatusMessage(mtInfo,'');
end;

procedure TfWstTypeLibraryEdit.RenderSources();

  procedure LoadText(const AList : TStrings; ASrc : ISourceStream);
  var
    srcItemSV : ISavableSourceStream;
  begin
    if Supports(ASrc,ISavableSourceStream,srcItemSV) then begin
      srcItemSV.GetStream().Position := 0;
      AList.LoadFromStream(srcItemSV.GetStream());
    end;
  end;

var
  srcMngr : ISourceManager;
begin
  if Assigned(FSymbolTable) then begin
    if ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 ) then begin
      srcMngr := GenerateSource(FSymbolTable,[cloInterface,cloProxy,cloBinder,cloImp],otMemory,'',@ShowStatusMessage);
      if Assigned(srcMngr) and ( srcMngr.GetCount() > 0 ) then begin
        LoadText(srcInterface.Lines,srcMngr.GetItem(0));
        LoadText(srcProxy.Lines,srcMngr.GetItem(1));
        LoadText(srcBinder.Lines,srcMngr.GetItem(2));
        LoadText(srcImp.Lines,srcMngr.GetItem(3));
      end;
    end else begin
      srcInterface.ClearAll();
      srcProxy.ClearAll();
      srcBinder.ClearAll();
      srcImp.ClearAll();
    end;
  end;
  ShowStatusMessage(mtInfo,'');
end;

procedure TfWstTypeLibraryEdit.RenderWSDL();
var
  mstrm : TMemoryStream;
begin
  mstrm := TMemoryStream.Create();
  try
    GenerateWSDL_ToStream(FSymbolTable,mstrm);
    mstrm.Position := 0;
    srcWSDL.Lines.LoadFromStream(mstrm);
  finally
    FreeAndNil(mstrm);
  end;
end;

procedure TfWstTypeLibraryEdit.OpenFile (const AFileName : string; const AContent : TStream);
var
  tmpTable : TwstPasTreeContainer;
  curLok : IInterface;
begin
  {$IFDEF WST_IDE}
  FProjectLibrary := nil;
  {$ENDIF}
  FCurrentFileName := '';
  mmoLog.Clear();
  PC.ActivePage := tsLog;
  curLok := SetCursorHourGlass();
  if AnsiSameText('.pas',ExtractFileExt(AFileName)) then begin
    tmpTable := ParsePascalFile(AFileName,@ShowStatusMessage);
  end else begin
    if ( AContent = nil ) then
      tmpTable := ParseWsdlFile(AFileName,@ShowStatusMessage)
    else
      tmpTable := ParseWsdlFile(AFileName,AContent,@ShowStatusMessage);
  end;
  if Assigned(tmpTable) then begin
    if AnsiSameText('.pas',ExtractFileExt(AFileName)) then
      FCurrentFileName := ChangeFileExt(AFileName,'.wsdl')
    else
      FCurrentFileName := AFileName;
    trvSchema.Items.Clear();
    FreeAndNil(FSymbolTable);
    FSymbolTable := tmpTable;
    RenderSymbols();
    PC.ActivePage := tsInterface;
  end;
  curLok := nil;
  SB.Panels[0].Text := FCurrentFileName;
end;

procedure TfWstTypeLibraryEdit.SaveToFile (const AFileName : string );
var
  mstrm : TMemoryStream;
begin
  mstrm := TMemoryStream.Create();
  try
    GenerateWSDL_ToStream(FSymbolTable,mstrm);
    mstrm.SaveToFile(AFileName);
  finally
    FreeAndNil(mstrm);
  end;
end;

constructor TfWstTypeLibraryEdit.Create(AOwner: TComponent);
begin
  if ( DM = nil ) then begin
    DM := TDM.Create(Application);
  end;
  inherited Create(AOwner);
  FSymbolTable := CreateSymbolTable(ExtractFileName(DEF_FILE_NAME));
  trvSchema.Images := DM.IM;
end;

destructor TfWstTypeLibraryEdit.Destroy();
begin
  trvSchema.Items.Clear();
  FreeAndNil(FSymbolTable);
  inherited Destroy();
  FreeAndNil(DM);
end;

initialization
  //SetHeapTraceOutput('heap_trace.txt');
  
  {$I uwsttypelibraryedit.lrs}

end.

