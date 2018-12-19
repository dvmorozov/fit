{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit testmetadata_unit;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XMLWrite,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  metadata_generator, binary_streamer, metadata_repository, pastree,
  pascal_parser_intf,
  metadata_wsdl;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  { TTestMetadata }

  TTestMetadata= class(TTestCase)
  protected
    function CreateSymbolTable():TwstPasTreeContainer;
  published
    procedure test_Metadata();
  end;


implementation

{ TTestMetadata }

function TTestMetadata.CreateSymbolTable(): TwstPasTreeContainer;

  function CreateProc(
    const AName : string;
          AClass : TPasClassType;
          AContainer : TwstPasTreeContainer
  ) : TPasProcedure ;
  begin
    Result := TPasProcedure(AContainer.CreateElement(TPasProcedure,AName,AContainer.CurrentModule.InterfaceSection,visDefault,'',0));
    Result.ProcType := TPasProcedureType(AContainer.CreateElement(TPasProcedureType,'',Result,visDefault,'',0));
    AClass.Members.Add(Result);
  end;

  function CreateFunc(
    const AName, AResultTypeName : string;
          AClass : TPasClassType;
          AContainer : TwstPasTreeContainer
  ) : TPasFunction ;
  begin
    Result := TPasFunction(AContainer.CreateElement(TPasFunction,AName,AContainer.CurrentModule.InterfaceSection,visDefault,'',0));
    Result.ProcType := AContainer.CreateFunctionType('','result',Result,True,'',0);
    AClass.Members.Add(Result);
    TPasFunctionType(Result.ProcType).ResultEl.ResultType := AContainer.FindElement(AResultTypeName) as TPasType;
    TPasFunctionType(Result.ProcType).ResultEl.ResultType.AddRef();
  end;

  function CreateParam(
    const AName, ATypeName : string;
    const AAccess : TArgumentAccess;
          AProc : TPasProcedure;
          AContainer : TwstPasTreeContainer
  ) : TPasArgument ;
  begin
    Result := TPasArgument(AContainer.CreateElement(TPasArgument,AName,AProc,visDefault,'',0));
    Result.ArgType := AContainer.FindElement(ATypeName) as TPasType;
    Result.ArgType.AddRef();
    Result.Access := AAccess;
  end;
  
var
  inft : TPasClassType;
  sct : TPasSection;
  locProc : TPasProcedure;
begin
  Result := TwstPasTreeContainer.Create();
  CreateWstInterfaceSymbolTable(Result);
  Result.CreateElement(TPasModule,'test_unit_name',Result.Package,visDefault,'',0);
  sct := TPasSection(Result.CreateElement(TPasSection,'',Result.CurrentModule,visDefault,'',0));
  Result.CurrentModule.InterfaceSection := sct;

  inft := TPasClassType(Result.CreateElement(TPasClassType,'service_1',sct,visDefault,'',0));
  inft.ObjKind := okInterface;
  sct.Declarations.Add(inft);
  sct.Types.Add(inft);
    CreateProc('void_operation_proc',inft,Result);
    CreateFunc('void_operation_func','Integer',inft,Result);

  inft := TPasClassType(Result.CreateElement(TPasClassType,'service_2',sct,visDefault,'',0));
  inft.ObjKind := okInterface;
  sct.Declarations.Add(inft);
  sct.Types.Add(inft);
    locProc := CreateProc('dis_proc',inft,Result);
      CreateParam('d','double',argDefault,locProc,Result);
      CreateParam('i','Integer',argConst,locProc,Result);
      CreateParam('s','string',argOut,locProc,Result);
    locProc := CreateFunc('sid_func','double',inft,Result);
      CreateParam('s','string',argConst,locProc,Result);
      CreateParam('i','Integer',argVar,locProc,Result);
end;

procedure PrintWSDL(ARep : PServiceRepository);
var
  locDoc : TXMLDocument;
  strm : TMemoryStream;
  s : string;
begin
  strm := nil;;
  locDoc := TXMLDocument.Create();
  try
    GenerateWSDL(ARep,locDoc);
    strm := TMemoryStream.Create();
    WriteXMLFile(locDoc,strm);
    SetLength(s,strm.Size);
    Move(strm.Memory^,s[1],strm.Size);
    WriteLn('*******************************************************');
    WriteLn(s);
    WriteLn('*******************************************************');
  finally
    locDoc.Free();
    strm.Free();
  end;
end;

procedure TTestMetadata.test_Metadata();
var
  st : TwstPasTreeContainer;
  mg : TMetadataGenerator;
  wtr : IDataStore;
  strm : TMemoryStream;

  rp : PServiceRepository;
  ps : PService;
  po : PServiceOperation;
  pop : POperationParam;
begin
  strm := nil;
  mg := nil;
  rp := nil;
  st := CreateSymbolTable();
  try
    strm := TMemoryStream.Create();
    wtr := CreateBinaryWriter(strm);
    mg := TMetadataGenerator.Create(st,wtr);
    mg.Execute();
    wtr := nil;
    strm.Position := 0;
    
    AssertTrue(strm.Size>10);
    AssertEquals('symbol count',2,LoadRepositoryData(strm,rp));
    AssertEquals('unit name','test_unit_name',rp^.Name);
    AssertEquals('services count',2,rp^.ServicesCount);
    AssertNotNull('services pointer',rp^.Services);
    
    ps := rp^.Services;
    AssertEquals('service name','service_1',ps^.Name);
    AssertEquals('operations count',2,ps^.OperationsCount);
    AssertNotNull('operations pointer',ps^.Operations);
      po := ps^.Operations;
      AssertEquals('operation name','void_operation_proc',po^.Name);
      AssertEquals('params count',0,po^.ParamsCount);
      AssertNull('params pointer',po^.Params);
      Inc(po);
      AssertEquals('operation name','void_operation_func',po^.Name);
      AssertEquals('params count',1,po^.ParamsCount);
      AssertNotNull('params pointer',po^.Params);
        pop := po^.Params;
        AssertEquals('param name','result',pop^.Name);
        AssertEquals('param type name','integer',pop^.TypeName);
        AssertEquals('param modifier',ord(argOut),ord(pop^.Modifier));
        
     rp^.NameSpace := 'http://test_name_space/';
     //PrintWSDL(rp);
  finally
    mg.Free();
    st.Free();
    strm.Free();
    ClearRepositoryData(rp);
  end;
end;

initialization
  RegisterTest(TTestMetadata);
  
end.
