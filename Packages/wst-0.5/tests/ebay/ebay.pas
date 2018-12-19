unit ebay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base_service_intf;

const
  sAPP_ID = 'INOUSSAOUEU258CIC9Z5E83UXC1BE5';//'<your AppId>';
  sEBAY_VERSION = '467';

type

  TAckCodeType = ( Success, Failure, Warning, PartialFailure, CustomCode );
  
{  TErrorType = class(TBaseComplexRemotable)
  published
    property ShortMessage : string read FShortMessage write FShortMessage stored HasShortMessage;
    property LongMessage : string read FLongMessage write FLongMessage stored HasLongMessage;
  end;
}

  { TPaginationType }

  TPaginationType = class(TBaseComplexRemotable)
  private
    FEntriesPerPage: Integer;
    FPageNumber: Integer;
    function HasEntriesPerPage: boolean;
    function HasPageNumber: boolean;
  published
    property EntriesPerPage : Integer read FEntriesPerPage write FEntriesPerPage stored HasEntriesPerPage;
    property PageNumber : Integer read FPageNumber write FPageNumber stored HasPageNumber;
  end;

  { TPaginationResultType }

  TPaginationResultType = class(TBaseComplexRemotable)
  private
    FTotalNumberOfEntries: Integer;
    FTotalNumberOfPages: Integer;
    function HasTotalNumberOfEntries: boolean;
    function HasTotalNumberOfPages: boolean;
  published
    property TotalNumberOfPages : Integer read FTotalNumberOfPages write FTotalNumberOfPages stored HasTotalNumberOfPages;
    property TotalNumberOfEntries : Integer read FTotalNumberOfEntries write FTotalNumberOfEntries stored HasTotalNumberOfEntries;
  end;
  
  { TCategoryType }

  TCategoryType = class(TBaseComplexRemotable)
  private
    FAutoPayEnabled: Boolean;
    FB2BVATEnabled: Boolean;
    FBestOfferEnabled: Boolean;
    FCatalogEnabled: Boolean;
    FCategoryID: string;
    FCategoryLevel: Integer;
    FCategoryName: string;
    FCategoryParentID: string;
    FCategoryParentName: string;
    FKeywords: string;
    FProductFinderAvailable: Boolean;
    FProductFinderID: Integer;
    FProductSearchPageAvailable: Boolean;
    function HasCategoryID: boolean;
    function HasCategoryLevel: boolean;
    function HasCategoryName: boolean;
    function HasCategoryParentID: boolean;
    function HasCategoryParentName: boolean;
    function HasKeywords: boolean;
    function HasProductFinderID: boolean;
  published
    property BestOfferEnabled : Boolean read FBestOfferEnabled write FBestOfferEnabled stored FBestOfferEnabled;
    property AutoPayEnabled : Boolean read FAutoPayEnabled write FAutoPayEnabled stored FAutoPayEnabled;
    property B2BVATEnabled : Boolean read FB2BVATEnabled write FB2BVATEnabled stored FB2BVATEnabled;
    property CatalogEnabled : Boolean read FCatalogEnabled write FCatalogEnabled stored FCatalogEnabled;
    property CategoryID : string read FCategoryID write FCategoryID stored HasCategoryID;
    property CategoryLevel : Integer read FCategoryLevel write FCategoryLevel stored HasCategoryLevel;
    property CategoryName : string read FCategoryName write FCategoryName stored HasCategoryName;
    property CategoryParentID : string read FCategoryParentID write FCategoryParentID stored HasCategoryParentID;
    property CategoryParentName : string read FCategoryParentName write FCategoryParentName stored HasCategoryParentName;
    property ProductFinderID : Integer read FProductFinderID write FProductFinderID stored HasProductFinderID;
    property ProductSearchPageAvailable : Boolean read FProductSearchPageAvailable write FProductSearchPageAvailable stored FProductSearchPageAvailable;
    property ProductFinderAvailable : Boolean read FProductFinderAvailable write FProductFinderAvailable stored FProductFinderAvailable;
    property Keywords : string read FKeywords write FKeywords stored HasKeywords;
  end;
  
  { TCategoryArrayType }

  TCategoryArrayType = class(TBaseObjectArrayRemotable)
  private
    function GetCategoryItem(AIndex: Integer): TCategoryType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TCategoryType read GetCategoryItem;
    property Category[AIndex:Integer] : TCategoryType read GetCategoryItem;default;
  end;
  
  { TUserIdPasswordType }

  TUserIdPasswordType = class(TBaseComplexRemotable)
  private
    FAppId: string;
    FAuthCert: string;
    FDevId: string;
    FPassword: string;
    FUsername: string;
    function HasAppId: boolean;
    function HasAuthCert: boolean;
    function HasDevId: boolean;
    function HasPassword: boolean;
    function HasUsername: boolean;
  published
    property AppId : string read FAppId write FAppId stored HasAppId;
    property DevId : string read FDevId write FDevId stored HasDevId;
    property AuthCert : string read FAuthCert write FAuthCert stored HasAuthCert;
    property Username : string read FUsername write FUsername stored HasUsername;
    property Password : string read FPassword write FPassword stored HasPassword;
  end;
  
  { TCustomSecurityHeaderType }

  TCustomSecurityHeaderType = class(THeaderBlock)
  private
    FCredentials: TUserIdPasswordType;
    FeBayAuthToken: string;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property eBayAuthToken : string read FeBayAuthToken write FeBayAuthToken;
    property Credentials : TUserIdPasswordType read FCredentials write FCredentials;
  end;

  { TAbstractRequestType }

  TAbstractRequestType = class(TBaseComplexRemotable)
  private
    FVersion: string;
  published
    property Version : string read FVersion write FVersion;
  end;
  
  { TAbstractResponseType }

  TAbstractResponseType = class(TBaseComplexRemotable)
  private
    FAck: TAckCodeType;
    FCorrelationID: string;
    FMessage: string;
    FVersion: string;
    function HasAck: boolean;
    function HasCorrelationID: boolean;
    function HasMessage: boolean;
    function HasVersion: boolean;
  published
    property CorrelationID : string read FCorrelationID write FCorrelationID stored HasCorrelationID;
    property Message : string read FMessage write FMessage stored HasMessage;
    property Version : string read FVersion write FVersion stored HasVersion;
    property Ack : TAckCodeType read FAck write FAck stored HasAck;
  end;
  
  { TGetCategoriesRequestType }

  TGetCategoriesRequestType = class(TAbstractRequestType)
  private
    FCategorySiteID: string;
    function HasCategorySiteID: boolean;
  published
    property CategorySiteID : string read FCategorySiteID write FCategorySiteID stored HasCategorySiteID;
  end;

  { TGetCategoriesResponseType }

  TGetCategoriesResponseType = class(TAbstractResponseType)
  private
    FCategoryCount: Integer;
    FCategoryVersion: string;
    FMinimumReservePrice: Double;
    FReservePriceAllowed: Boolean;
    function HasCategoryCount: boolean;
    function HasCategoryVersion: boolean;
    function HasMinimumReservePrice: boolean;
    function HasReservePriceAllowed: boolean;
  published
    property CategoryCount : Integer read FCategoryCount write FCategoryCount stored HasCategoryCount;
    property CategoryVersion : string read FCategoryVersion write FCategoryVersion stored HasCategoryVersion;
    property ReservePriceAllowed : Boolean read FReservePriceAllowed write FReservePriceAllowed stored HasReservePriceAllowed;
    property MinimumReservePrice : Double read FMinimumReservePrice write FMinimumReservePrice stored HasMinimumReservePrice;
  end;
  
  { TGetPopularKeywordsRequestType }

  TGetPopularKeywordsRequestType = class(TAbstractRequestType)
  private
    FCategoryID: string;
    FIncludeChildCategories: Boolean;
    FMaxKeywordsRetrieved: Integer;
    FPagination: TPaginationType;
    function HasCategoryID: boolean;
    function HasIncludeChildCategories: boolean;
    function HasMaxKeywordsRetrieved: boolean;
    function HasPagination: boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CategoryID : string read FCategoryID write FCategoryID stored HasCategoryID;
    property IncludeChildCategories : Boolean read FIncludeChildCategories write FIncludeChildCategories stored HasIncludeChildCategories;
    property MaxKeywordsRetrieved : Integer read FMaxKeywordsRetrieved write FMaxKeywordsRetrieved stored HasMaxKeywordsRetrieved;
    property Pagination : TPaginationType read FPagination write FPagination stored HasPagination;
  end;

  { TGetPopularKeywordsResponseType }

  TGetPopularKeywordsResponseType = class(TAbstractResponseType)
  private
    FCategoryArray: TCategoryArrayType;
    FHasMore: Boolean;
    FPaginationResult: TPaginationResultType;
    function HasCategoryArray: boolean;
    function HasPaginationResult: boolean;
    procedure SetCategoryArray(const AValue: TCategoryArrayType);
    procedure SetPaginationResult(const AValue: TPaginationResultType);
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property HasMore : Boolean read FHasMore write FHasMore stored FHasMore;
    property CategoryArray : TCategoryArrayType read FCategoryArray write SetCategoryArray stored HasCategoryArray;
    property PaginationResult : TPaginationResultType read FPaginationResult write SetPaginationResult stored HasPaginationResult;
  end;
  
  IeBayAPIInterfaceService = interface
    {function GetCategories(GetCategoriesRequest : TGetCategoriesRequestType ) : TGetCategoriesResponseType;}
    procedure GetCategories(
      const GetCategoriesRequest : TGetCategoriesRequestType;
      out   GetCategoriesResponse : TGetCategoriesResponseType
    );
    procedure GetPopularKeywords(
      const GetPopularKeywordsRequest : TGetPopularKeywordsRequestType;
      out   GetPopularKeywordsResponse : TGetPopularKeywordsResponseType
    );
  end;

  procedure Register_ebay_ServiceMetadata();
  
implementation
uses imp_utils, metadata_repository;

const
  sE_BAY_API_VERSION = 'Version 467';
  sE_BAY_NAME_SPACE = 'urn:ebay:apis:eBLBaseComponents';

{ TAbstractResponseType }

function TAbstractResponseType.HasCorrelationID: boolean;
begin
  Result := not IsStrEmpty(FCorrelationID);
end;

function TAbstractResponseType.HasAck: boolean;
begin
  Result := FAck > Success;
end;

function TAbstractResponseType.HasMessage: boolean;
begin
  Result := not IsStrEmpty(FMessage);
end;

function TAbstractResponseType.HasVersion: boolean;
begin
  Result := not IsStrEmpty(FVersion);
end;

{ TGetCategoriesRequestType }

function TGetCategoriesRequestType.HasCategorySiteID: boolean;
begin
  Result := not IsStrEmpty(FCategorySiteID);
end;

{ TGetCategoriesResponseType }

function TGetCategoriesResponseType.HasCategoryCount: boolean;
begin
  Result := ( FCategoryCount > 0 );
end;

function TGetCategoriesResponseType.HasCategoryVersion: boolean;
begin
  Result := not IsStrEmpty(FCategoryVersion);
end;

function TGetCategoriesResponseType.HasMinimumReservePrice: boolean;
begin
  Result := ( MinimumReservePrice <> 0 );
end;

function TGetCategoriesResponseType.HasReservePriceAllowed: boolean;
begin
  Result := FReservePriceAllowed;
end;

{ TUserIdPasswordType }

function TUserIdPasswordType.HasAppId: boolean;
begin
  Result := not IsStrEmpty(FAppId);
end;

function TUserIdPasswordType.HasAuthCert: boolean;
begin
  Result := not IsStrEmpty(FAuthCert);
end;

function TUserIdPasswordType.HasDevId: boolean;
begin
  Result := not IsStrEmpty(FDevId);
end;

function TUserIdPasswordType.HasPassword: boolean;
begin
  Result := not IsStrEmpty(FPassword);
end;

function TUserIdPasswordType.HasUsername: boolean;
begin
  Result := not IsStrEmpty(FUsername);
end;

{ TCustomSecurityHeaderType }

constructor TCustomSecurityHeaderType.Create();
begin
  inherited Create();
  FCredentials := TUserIdPasswordType.Create();
end;

destructor TCustomSecurityHeaderType.Destroy();
begin
  FreeAndNil(FCredentials);
  inherited Destroy();
end;

{ TPaginationType }

function TPaginationType.HasEntriesPerPage: boolean;
begin
  Result := HasEntriesPerPage;
end;

function TPaginationType.HasPageNumber: boolean;
begin
  Result := ( FPageNumber <> 0 );
end;

{ TGetPopularKeywordsRequestType }

function TGetPopularKeywordsRequestType.HasCategoryID: boolean;
begin
  Result := not IsStrEmpty(FCategoryID);
end;

function TGetPopularKeywordsRequestType.HasIncludeChildCategories: boolean;
begin
  Result := IncludeChildCategories;
end;

function TGetPopularKeywordsRequestType.HasMaxKeywordsRetrieved: boolean;
begin
  Result := ( MaxKeywordsRetrieved <> 0 );
end;

function TGetPopularKeywordsRequestType.HasPagination: boolean;
begin
  Result := Assigned(FPagination) and
            ( FPagination.HasEntriesPerPage or FPagination.HasPageNumber);
end;

constructor TGetPopularKeywordsRequestType.Create();
begin
  inherited Create();
end;

destructor TGetPopularKeywordsRequestType.Destroy();
begin
  FreeAndNil(FPagination);
  inherited Destroy();
end;

{ TCategoryType }

function TCategoryType.HasCategoryID: boolean;
begin
  Result := not IsStrEmpty(FCategoryID);
end;

function TCategoryType.HasCategoryLevel: boolean;
begin
  Result := ( FCategoryLevel <> 0 );
end;

function TCategoryType.HasCategoryName: boolean;
begin
  Result := not IsStrEmpty(FCategoryName);
end;

function TCategoryType.HasCategoryParentID: boolean;
begin
  Result := not IsStrEmpty(FCategoryParentID);
end;

function TCategoryType.HasCategoryParentName: boolean;
begin
  Result := not IsStrEmpty(FCategoryParentName);
end;

function TCategoryType.HasKeywords: boolean;
begin
  Result := not IsStrEmpty(FKeywords);
end;

function TCategoryType.HasProductFinderID: boolean;
begin
  Result := ( FProductFinderID > 0 );
end;

{ TCategoryArrayType }

function TCategoryArrayType.GetCategoryItem(AIndex: Integer): TCategoryType;
begin
  Result := inherited GetItem(AIndex) as TCategoryType;
end;

class function TCategoryArrayType.GetItemClass(): TBaseRemotableClass;
begin
  Result := TCategoryType;
end;

{ TGetPopularKeywordsResponseType }

function TGetPopularKeywordsResponseType.HasCategoryArray: boolean;
begin
  Result := ( FCategoryArray.Length > 0 );
end;

function TGetPopularKeywordsResponseType.HasPaginationResult: boolean;
begin
  Result := ( FPaginationResult.TotalNumberOfEntries <> 0 ) or
            ( FPaginationResult.TotalNumberOfPages <> 0 ) ;
end;

procedure TGetPopularKeywordsResponseType.SetCategoryArray(
  const AValue: TCategoryArrayType
);
begin
  if ( FCategoryArray = AValue ) then
    exit;
  FCategoryArray.Assign(AValue) ;
end;

procedure TGetPopularKeywordsResponseType.SetPaginationResult(
  const AValue: TPaginationResultType
);
begin
  if ( FPaginationResult = AValue ) then
    exit;
  FPaginationResult.Assign(AValue);
end;

constructor TGetPopularKeywordsResponseType.Create();
begin
  FCategoryArray := TCategoryArrayType.Create();
  FPaginationResult := TPaginationResultType.Create();
  inherited Create();
end;

destructor TGetPopularKeywordsResponseType.Destroy();
begin
  FreeAndNil(FPaginationResult);
  FreeAndNil(FCategoryArray);
  inherited Destroy();
end;

procedure RegisterEbayTypes();
Var
  r : TTypeRegistry;
  ri : TTypeRegistryItem;
begin
  r := GetTypeRegistry();

  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TAckCodeType),'AckCodeType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TCategoryType),'CategoryType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TCategoryArrayType),'CategoryArrayType');


  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TPaginationType),'PaginationType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TPaginationResultType),'PaginationResultType');

  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TGetPopularKeywordsRequestType),'GetPopularKeywordsRequestType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TGetPopularKeywordsResponseType),'GetPopularKeywordsResponseType');

  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TUserIdPasswordType),'UserIdPasswordType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TCustomSecurityHeaderType),'RequesterCredentials');//'CustomSecurityHeaderType');

  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TGetCategoriesRequestType),'GetCategoriesRequestType');
  r.Register(sE_BAY_NAME_SPACE,TypeInfo(TGetCategoriesResponseType),'GetCategoriesResponseType');
end;

procedure Register_ebay_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetOperationCustomData(
    'ebay',
    'IeBayAPIInterfaceService',
    'GetCategories',
    'Address',
        'https://api.sandbox.ebay.com/wsapi?' +
        'callname=GetCategories' +
        '&siteid=0' +
        '&appid=' + sAPP_ID +
        '&version=' + sEBAY_VERSION
  );

  mm.SetOperationCustomData(
    'ebay',
    'IeBayAPIInterfaceService',
    'GetPopularKeywords',
    'Address',
      'https://api.sandbox.ebay.com/wsapi?' +
      'callname=GetPopularKeywords' +
      '&siteid=0' +
      '&appid=' + sAPP_ID +
      '&version=' + sEBAY_VERSION

  );

end;

{ TPaginationResultType }

function TPaginationResultType.HasTotalNumberOfEntries: boolean;
begin
  Result := ( FTotalNumberOfEntries <> 0 );
end;

function TPaginationResultType.HasTotalNumberOfPages: boolean;
begin
  Result := ( FTotalNumberOfPages <> 0 );
end;

initialization
  //RegisterEbayTypes();
  
end.
