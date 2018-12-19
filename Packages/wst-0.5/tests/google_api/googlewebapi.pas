Unit googlewebapi;
{$mode objfpc}{$H+}
interface
uses SysUtils, Classes, base_service_intf;

Type

  TResultElement = class(TBaseComplexRemotable)
  private
    FcachedSize: string;
    FdirectoryTitle: string;
    FhostName: string;
    FrelatedInformationPresent: boolean;
    Fsnippet: string;
    Fsummary: string;
    Ftitle: string;
    FURL: string;
  Published
    property summary : string Read Fsummary Write Fsummary;
    property URL : string Read FURL Write FURL;
    property snippet: string Read Fsnippet Write Fsnippet;
    property title : string Read Ftitle Write Ftitle;
    property cachedSize : string Read FcachedSize Write FcachedSize;
    property relatedInformationPresent : boolean Read FrelatedInformationPresent Write FrelatedInformationPresent;
    property hostName : string Read FhostName Write FhostName;
    property directoryTitle : string Read FdirectoryTitle Write FdirectoryTitle;
  End;

  TResultElementArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TResultElement;
  Public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TResultElement Read GetItem;Default;
  End;
  
  TDirectoryCategory = class(TBaseComplexRemotable)
  private
    FfullViewableName: string;
    FspecialEncoding: string;
  Published
    property fullViewableName : string Read FfullViewableName Write FfullViewableName;
    property specialEncoding : string Read FspecialEncoding Write FspecialEncoding;
  End;

  TDirectoryCategoryArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TDirectoryCategory;
  Public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TDirectoryCategory Read GetItem;Default;
  End;

  TGoogleSearchResult = class(TBaseComplexRemotable)
  private
    FdirectoryCategories: TDirectoryCategoryArray;
    FdocumentFiltering: Boolean;
    FendIndex: Integer;
    FestimatedTotalResultsCount: Integer;
    FestimateIsExact: Boolean;
    FresultElements: TResultElementArray;
    FsearchComments: string;
    FsearchQuery: string;
    FsearchTime: Double;
    FsearchTips: string;
    FstartIndex: Integer;
  Public
    constructor Create();override;
    destructor Destroy();override;
  Published
    property documentFiltering : Boolean Read FdocumentFiltering Write FdocumentFiltering;
    property searchComments :string Read FsearchComments Write FsearchComments;
    property estimatedTotalResultsCount: Integer Read FestimatedTotalResultsCount Write FestimatedTotalResultsCount;
    property estimateIsExact : Boolean Read FestimateIsExact Write FestimateIsExact;
    property searchQuery : string Read FsearchQuery Write FsearchQuery;
    property startIndex : Integer Read FstartIndex Write FstartIndex;
    property endIndex : Integer Read FendIndex Write FendIndex;
    property searchTips :string Read FsearchTips Write FsearchTips;
    property searchTime : Double Read FsearchTime Write FsearchTime;
    property resultElements : TResultElementArray Read FresultElements Write FresultElements;
    property directoryCategories : TDirectoryCategoryArray Read FdirectoryCategories Write FdirectoryCategories;
  End;
  
  IGoogleSearch = Interface
    ['{17FCCC65-4A0B-4D19-93F6-F69EAA719CA3}']
    function doSpellingSuggestion(
      const key:string;
      const phrase:string
    ):string;
    function doGoogleSearch(
      Const key          : string;
      Const q            : string;
      Const start        : Integer;
      Const maxResults   : Integer;
      Const filter       : Boolean;
      Const restrict     : string;
      Const safeSearch   : Boolean;
      Const lr           : string;
      Const ie           : string;
      Const oe           : string
    ) : TGoogleSearchResult ;
  End;

Implementation
uses base_soap_formatter;

Const
  GOOGLE_NAMESPACE = 'urn:GoogleSearch';
  
{ TResultElementArray }

function TResultElementArray.GetItem(AIndex: Integer): TResultElement;
begin
  Result := Inherited GetItem(AIndex) As TResultElement;
end;

class function TResultElementArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TResultElement;
end;

{ TGoogleSearchResult }

constructor TGoogleSearchResult.Create();
begin
  inherited Create();
  FresultElements := TResultElementArray.Create();
  FdirectoryCategories := TDirectoryCategoryArray.Create();
end;

destructor TGoogleSearchResult.Destroy();
begin
  FdirectoryCategories.Free();
  FresultElements.Free();
  inherited Destroy();
end;

{ TDirectoryCategoryArray }

function TDirectoryCategoryArray.GetItem(AIndex: Integer): TDirectoryCategory;
begin
  Result := Inherited GetItem(AIndex) As TDirectoryCategory;
end;

class function TDirectoryCategoryArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TDirectoryCategory;
end;

Initialization
  GetTypeRegistry().Register(GOOGLE_NAMESPACE,TypeInfo(TDirectoryCategory),'DirectoryCategory');
  GetTypeRegistry().Register(GOOGLE_NAMESPACE,TypeInfo(TDirectoryCategoryArray),'DirectoryCategoryArray');
  GetTypeRegistry().Register(GOOGLE_NAMESPACE,TypeInfo(TResultElement),'ResultElement');
  GetTypeRegistry().Register(GOOGLE_NAMESPACE,TypeInfo(TResultElementArray),'ResultElementArray');
  GetTypeRegistry().Register(GOOGLE_NAMESPACE,TypeInfo(TGoogleSearchResult),'GoogleSearchResult');
  
End.
