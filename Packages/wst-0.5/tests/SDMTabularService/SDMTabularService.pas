{
This unit has been produced by ws_helper.
  Input unit name : "SDMTabularService".
  This unit name  : "SDMTabularService".
  Date            : "11/07/2007 23:11:05".
}
unit SDMTabularService;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx';
  sUNIT_NAME = 'SDMTabularService';

type

  schema_Type = class(TStringBufferRemotable) end;
  //diffgram_Type = class(TStringBufferRemotable);
  
  RunQueryType = class;
  RunQueryResponse_RunQueryResult_Type = class;
  RunQueryResponse = class;

  RunQueryType = class(TBaseComplexRemotable)
  private
    FQuery : string;
  private
    function HasQuery() : Boolean;
  published
    property Query : string read FQuery write FQuery stored HasQuery;
  end;

  { RunQueryResponse_RunQueryResult_Type }

  RunQueryResponse_RunQueryResult_Type = class(TBaseComplexRemotable)
  private
    Fdiffgram : schema_Type;
    Fschema : schema_Type;
    function Hasdiffgram : boolean;
  public
    constructor Create();override;
    Destructor Destroy();override;
  published
    property schema : schema_Type read Fschema write Fschema;
    property diffgram : schema_Type read Fdiffgram write Fdiffgram stored Hasdiffgram;
  end;

  RunQueryResponse = class(TBaseComplexRemotable)
  private
    FRunQueryResult : RunQueryResponse_RunQueryResult_Type;
  private
    function HasRunQueryResult() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property RunQueryResult : RunQueryResponse_RunQueryResult_Type read FRunQueryResult write FRunQueryResult stored HasRunQueryResult;
  end;

  SDMTabularServiceSoap = interface(IInvokable)
    ['{6F9CD0B5-85E3-43A5-9265-5F6AD11B3742}']
    function RunQuery(
      Const RunQueryParam : RunQueryType
    ):RunQueryResponse;
  end;

  procedure Register_SDMTabularService_ServiceMetadata();

Implementation
uses metadata_repository;

{ RunQueryType }

function RunQueryType.HasQuery() : Boolean;
begin
  Result := True;
end;

{ RunQueryResponse }

destructor RunQueryResponse.Destroy();
begin
  if Assigned(FRunQueryResult) then
    FreeAndNil(FRunQueryResult);
  inherited Destroy();
end;

function RunQueryResponse.HasRunQueryResult() : Boolean;
begin
  Result := True;
end;

constructor RunQueryResponse.Create();
begin
  inherited Create();
  FRunQueryResult := RunQueryResponse_RunQueryResult_Type.Create();
end;


procedure Register_SDMTabularService_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'SDMTabularServiceSoap',
    'TRANSPORT_Address',
    'http://sdmdataaccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'SDMTabularServiceSoap',
    'FORMAT_Style',
    'document'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'SDMTabularServiceSoap',
    'RunQuery',
    'TRANSPORT_soapAction',
    'http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'SDMTabularServiceSoap',
    'RunQuery',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'SDMTabularServiceSoap',
    'RunQuery',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


{ RunQueryResponse_RunQueryResult_Type }

function RunQueryResponse_RunQueryResult_Type.Hasdiffgram : boolean;
begin
  Result := ( diffgram <> nil );
end;

constructor RunQueryResponse_RunQueryResult_Type.Create();
begin
  inherited Create();
  Fschema := schema_Type.Create();
  Fdiffgram := schema_Type.Create();
end;

destructor RunQueryResponse_RunQueryResult_Type.Destroy();
begin
  FreeAndNil(Fdiffgram);
  FreeAndNil(Fschema);
  inherited Destroy();
end;

initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RunQueryType),'RunQuery');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RunQueryResponse_RunQueryResult_Type),'RunQueryResponse_RunQueryResult_Type').RegisterExternalPropertyName('schema','xs:schema');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RunQueryResponse_RunQueryResult_Type),'RunQueryResponse_RunQueryResult_Type').RegisterExternalPropertyName('diffgram','diffgr:diffgram');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(RunQueryResponse),'RunQueryResponse');


End.
