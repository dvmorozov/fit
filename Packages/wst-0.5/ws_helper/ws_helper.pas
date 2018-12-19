{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


program ws_helper;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, wst_resources_utils,
  //parserdefs, ws_parser,
  generator, parserutils, source_utils,
  command_line_parser, metadata_generator, binary_streamer,
  DOM, xmlread, wsdl2pas_imp,

  pastree, pparser, pascal_parser_intf, logger_intf;

resourcestring
  sUSAGE = 'ws_helper [-uMODE] [-p] [-b] [-i] [-oPATH] inputFilename' + sNEW_LINE +
           '  -u MODE Generate the pascal translation of the WSDL input file ' + sNEW_LINE +
           '       MODE value may be U for used types or A for all types' + sNEW_LINE +
           '  -p  Generate service proxy' + sNEW_LINE +
           '  -b  Generate service binder' + sNEW_LINE +
           '  -i  Generate service minimal implementation' + sNEW_LINE +
           '  -o  PATH  Relative output directory' + sNEW_LINE +
           '  -a  PATH  Absolute output directory' + sNEW_LINE;
  sCOPYRIGHT = 'ws_helper, Web Service Toolkit 0.4 Copyright (c) 2006, 2007 by Inoussa OUEDRAOGO';

const
  sWST_META = 'wst_meta';
  
type
  TSourceFileType = ( sftPascal, sftWSDL );
  
var
  inFileName,outPath,errStr : string;
  srcMngr : ISourceManager;
  AppOptions : TComandLineOptions;
  NextParam : Integer;
  sourceType : TSourceFileType;
  symtable : TwstPasTreeContainer;
  parserMode : TParserMode;
  
  osParam, targetParam : string;

  function ProcessCmdLine():boolean;
  begin
    NextParam := ParseCmdLineOptions(AppOptions);
    if ( NextParam <= Paramcount ) then begin
      inFileName := ParamStr(NextParam);
    end;
    Result := FileExists(ExpandFileName(inFileName));
    if AnsiSameText(ExtractFileExt(inFileName),'.PAS') or
       AnsiSameText(ExtractFileExt(inFileName),'.PP')
    then begin
      sourceType := sftPascal;
    end else if AnsiSameText(ExtractFileExt(inFileName),'.WSDL') then begin
      sourceType := sftWSDL;
    end;
    if Result then begin
      if ( AppOptions = [] ) then begin
        Include(AppOptions,cloProxy);
      end;
    end else begin
      errStr := Format('File not Found : "%s"',[inFileName]);
    end;
    if ( cloOutPutDirAbsolute in AppOptions ) then begin
      outPath := Trim(GetOptionArg(cloOutPutDirAbsolute));
    end else begin
      outPath := ExtractFilePath(inFileName);
      if ( cloOutPutDirRelative in AppOptions ) then begin
        outPath := outPath + Trim(GetOptionArg(cloOutPutDirRelative));
      end;
    end;
    outPath := IncludeTrailingPathDelimiter(outPath);
    parserMode := pmUsedTypes;
    if AnsiSameText('A',Trim(GetOptionArg(cloInterface))) then begin
      parserMode := pmAllTypes;
    end;
  end;

  function GenerateSymbolTable() : Boolean ;

    procedure ParsePascalFile();
    begin
      ParseSource(symtable,inFileName,osParam,targetParam);
    end;
    
    procedure ParseWsdlFile();
    var
      locDoc : TXMLDocument;
      prsr : TWsdlParser;
    begin
      prsr := nil;
      ReadXMLFile(locDoc,inFileName);
      try
        prsr := TWsdlParser.Create(locDoc,symtable);
        prsr.Parse(parserMode,ChangeFileExt(ExtractFileName(inFileName),''));
      finally
        FreeAndNil(prsr);
        FreeAndNil(locDoc);
      end;
    end;
    
  begin
    try
      WriteLn('Parsing the file : ', inFileName);
      case sourceType of
        sftPascal : ParsePascalFile();
        sftWSDL   : ParseWsdlFile();
      end;
      Result := True;
    except
      on e : Exception do begin
        Result := False;
        errStr := e.Message;
      end;
    end;
  end;
  
  function ProcessFile():Boolean;
  Var
    mtdaFS: TMemoryStream;
    g : TBaseGenerator;
    mg : TMetadataGenerator;
    rsrcStrm : TMemoryStream;
  begin
    Result := False;
    rsrcStrm := nil;
    mtdaFS := nil;
    mg := nil;
    g := Nil;
    try
      try
        if ( cloInterface in AppOptions ) then begin
          WriteLn('Interface file generation...');
          g := TInftGenerator.Create(symtable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        end;

        If ( cloProxy in AppOptions ) Then Begin
          WriteLn('Proxy file generation...');
          g := TProxyGenerator.Create(symtable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;

        If ( cloBinder in AppOptions ) Then Begin
          WriteLn('Binder file generation...');
          g := TBinderGenerator.Create(symtable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;

        If ( cloImp in AppOptions ) Then Begin
          WriteLn('Implementation file generation...');
          g := TImplementationGenerator.Create(symtable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;

        if ( [cloBinder,cloProxy]*AppOptions  <> [] ) then begin
          WriteLn('Metadata file generation...');
          mtdaFS := TMemoryStream.Create();
          mg := TMetadataGenerator.Create(symtable,CreateBinaryWriter(mtdaFS));
          mg.Execute();
          mtdaFS.SaveToFile(ChangeFileExt(inFileName,'.' + sWST_META));
          rsrcStrm := TMemoryStream.Create();
          mtdaFS.Position := 0;
          BinToWstRessource(UpperCase(symtable.CurrentModule.Name),mtdaFS,rsrcStrm);
          rsrcStrm.SaveToFile(outPath + ChangeFileExt(ExtractFileName(inFileName),'.' + sWST_EXTENSION));
        end;
        
        Result := True;
      except
        on E : Exception do begin
          Result := False;
          errStr := E.Message;
        end;
      end;
    finally
      rsrcStrm.Free();
      mg.Free();;
      mtdaFS.Free();;
      g.Free();
    end;
  end;


begin
  osParam := 'windows';
  targetParam := 'x86';
  
  SetLogger(TSimpleConsoleLogger.Create());
  
  symtable := nil;
  try
    try
      Writeln(sCOPYRIGHT);
      If ( ParamCount = 0 ) Then Begin
        WriteLn(sUSAGE);
        Exit;
      End;

      if not ProcessCmdLine() then begin
        WriteLn(errStr);
        Exit;
      end;
      symtable := TwstPasTreeContainer.Create();//ChangeFileExt(ExtractFileName(inFileName),'')
      srcMngr := CreateSourceManager();

      if not GenerateSymbolTable() then begin
        WriteLn(errStr);
        Exit;
      end;
      
      If Not ProcessFile() Then Begin
        WriteLn(errStr);
        Exit;
      End;

      srcMngr.SaveToFile(outPath);
      if ( GetLogger().GetMessageCount(mtError) = 0 ) then begin
        WriteLn(Format('File "%s" parsed succesfully.',[inFileName]));
      end else begin
        WriteLn(Format('Paring complete with %d error(s).',[GetLogger().GetMessageCount(mtError)]));
      end;
    except
      on e:exception Do
        Writeln('Exception : ' + e.Message)
    end;
  finally
    FreeAndNil(symtable);
    SetLogger(nil);
  end;
end.
