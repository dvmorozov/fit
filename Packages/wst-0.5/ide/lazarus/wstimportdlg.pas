
unit wstimportdlg;

{$mode objfpc}{$H+}
{$DEFINE WST_IDE}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ActnList, logger_intf;

type

  TGenOption = (
    goInterface, goInterfaceALL,
    goProxy, goImp, goBinder
  );
  TGenOptions = set of TGenOption;
  
  TOnParserMessage = procedure (const AMsgType : TMessageType; const AMsg : string) of object;
  
  { TformImport }

  TformImport = class(TForm)
    actOpenDir: TAction;
    actOpenFile: TAction;
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtAddToProject : TCheckBox;
    edtOptionIntfALL: TCheckBox;
    edtOptionIntf: TCheckBox;
    edtOptionProxy: TCheckBox;
    edtOptionBinder: TCheckBox;
    edtOptionImp: TCheckBox;
    edtInputFile: TEdit;
    edtOutputDir: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    mmoLog: TMemo;
    OD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SDD: TSelectDirectoryDialog;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure actOpenDirExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure edtOptionIntfALLClick(Sender: TObject);
    procedure edtOptionIntfClick(Sender: TObject);
  private
    FStatusMessageTag : Integer;
    procedure ShowStatusMessage(const AMsgType : TMessageType;const AMsg : string);
  public
    function GetOptions() : TGenOptions;
  end; 

var
  formImport: TformImport;

implementation
uses DOM, XMLRead, pastree, pascal_parser_intf, wsdl2pas_imp, source_utils,
     generator, metadata_generator, binary_streamer, wst_resources_utils
     {$IFDEF WST_IDE},LazIDEIntf{$ENDIF};

type
  TSourceType = goInterface .. goBinder;
  TSourceTypes = set of TSourceType;

function ParseWsdlFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prsr : TWsdlParser;
  symName : string;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    symName := ChangeFileExt(ExtractFileName(AFileName),'');
    if ( symName[Length(symName)] = '.' ) then begin
      Delete(symName,Length(symName),1);
    end;
    prsr := nil;
    ReadXMLFile(locDoc,AFileName);
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

    if ( ( [goInterface,goInterfaceALL] * AOptions ) <> [] ) then begin
      Notify('Interface file generation...');
      g := TInftGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( goProxy in AOptions ) then begin
      Notify('Proxy file generation...');
      g := TProxyGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( goBinder in AOptions ) then begin
      Notify('Binder file generation...');
      g := TBinderGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( goImp in AOptions ) then begin
      Notify('Implementation file generation...');
      g := TImplementationGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( AOutputType = otFileSystem ) and ( [goBinder,goProxy]*AOptions  <> [] ) then begin
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

{ TformImport }

procedure TformImport.actOpenFileExecute(Sender: TObject);
begin
  if OD.Execute() then begin
    edtInputFile.Text := OD.FileName;
  end;
end;

procedure TformImport.edtOptionIntfALLClick(Sender: TObject);
begin
  if edtOptionIntfALL.Checked and ( not edtOptionIntf.Checked ) then
    edtOptionIntf.Checked := True;
end;

procedure TformImport.edtOptionIntfClick(Sender: TObject);
begin
  if ( not edtOptionIntf.Checked ) and edtOptionIntfALL.Checked then
    edtOptionIntfALL.Checked := False;
end;

procedure TformImport.ShowStatusMessage(const AMsgType: TMessageType;const AMsg: string);
begin
  mmoLog.Lines.Add(Format('%s : %s',[MessageTypeNames[AMsgType],AMsg]));
  Inc(FStatusMessageTag);
  if ( (FStatusMessageTag) > 23 ) then begin
    FStatusMessageTag := 0;
    Application.ProcessMessages();
  end;
end;

function TformImport.GetOptions(): TGenOptions;
begin
  Result := [];
  if edtOptionIntf.Checked then begin
    Result := Result + [goInterface];
    if edtOptionIntfALL.Checked then begin
      Result := Result + [goInterfaceALL];
    end;
  end;
  if edtOptionProxy.Checked then
    Include(Result,goProxy);
  if edtOptionBinder.Checked then
    Include(Result,goBinder);
  if edtOptionImp.Checked then
    Include(Result,goImp);
end;

procedure TformImport.actOpenDirExecute(Sender: TObject);
begin
  if SDD.Execute() then begin
    if not DirectoryExists(SDD.FileName) then
      ForceDirectories(SDD.FileName);
    edtOutputDir.Text := SDD.FileName;
  end;
end;

procedure TformImport.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FileExists(edtInputFile.Text) and
                             DirectoryExists(edtOutputDir.Text) and
                             ( GetOptions() <> [] );
end;

procedure TformImport.actOKExecute(Sender: TObject);
var
  tree : TwstPasTreeContainer;
  oldCursor : TCursor;
  srcMgnr : ISourceManager;
  i : Integer;
  {$IFDEF WST_IDE}
  j, c : Integer;
  srcItm : ISourceStream;
  trueOpenFlags, openFlags : TOpenFlags;
  destPath : string;
  {$ENDIF}
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    tree := ParseWsdlFile(edtInputFile.Text,@ShowStatusMessage);
    try
      srcMgnr := GenerateSource(tree,GetOptions(),otFileSystem,IncludeTrailingPathDelimiter(edtOutputDir.Text),@ShowStatusMessage);
      ShowStatusMessage(mtInfo,'');
      {$IFDEF WST_IDE}
      openFlags := [];
      if edtAddToProject.Checked then begin
        Include(openFlags,ofAddToProject);
      end;
      destPath := IncludeTrailingPathDelimiter(edtOutputDir.Text);
      c := srcMgnr.GetCount();
      for i := 0 to Pred(c) do begin
        srcItm := srcMgnr.GetItem(i);
        trueOpenFlags := openFlags;
        for j := 0 to Pred(LazarusIDE.ActiveProject.FileCount) do begin
          if AnsiSameText(srcItm.GetFileName(),ExtractFileName(LazarusIDE.ActiveProject.Files[j].Filename)) then
            trueOpenFlags := trueOpenFlags - [ofAddToProject];
        end;
        LazarusIDE.DoOpenEditorFile(destPath + srcItm.GetFileName(),-1,trueOpenFlags);
      end;
      {$ENDIF}
    finally
      srcMgnr := nil;
      tree.Free();
    end;
  finally
    Screen.Cursor := oldCursor;
  end;
  ShowMessage('File parsed succefully.');
  Self.Close();
  ModalResult := mrOK;
end;

initialization
  {$I wstimportdlg.lrs}

end.

