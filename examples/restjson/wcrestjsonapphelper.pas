unit WCRESTJsonAppHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  wcApplication,
  ECommonObjs;

type

  { TRESTJsonConfigInitHelper }

  TRESTJsonConfigInitHelper = class(TWCHTTPAppConfigInitHelper)
  public
    procedure  DoHelp(AData : TObject); override;
  end;

  { TRESTJsonConfigHelper }

  TRESTJsonConfigHelper = class(TWCHTTPAppConfigRecordHelper)
  private
    FItemsDB : TThreadUtf8String;
    function GetItemsDb : UTF8String;
  public
    constructor Create;
    procedure  DoHelp(AData : TObject); override;
    destructor Destroy; override;

    property   ItemsDB : UTF8String read GetItemsDb;

    class function Config : TRESTJsonConfigHelper;
  end;

implementation

uses wcConfig;

const CFG_RESTJSON_SEC = $2000;
      CFG_RESTJSON_DB  = $2001;

      cItemsDb = 'items.db';

      RESTJSON_CFG_CONFIGURATION : TWCConfiguration = (
        (ParentHash:CFG_ROOT_HASH;   Hash:CFG_RESTJSON_SEC; Name:'RESTServer'),
        (ParentHash:CFG_RESTJSON_SEC; Hash:CFG_RESTJSON_DB; Name:'ItemsDB')
        );

var vRJServerConfigHelper : TRESTJsonConfigHelper = nil;

{ TRESTJsonConfigInitHelper }

procedure TRESTJsonConfigInitHelper.DoHelp(AData : TObject);
var
  RJSection : TWCConfigRecord;
begin
  AddWCConfiguration(RESTJSON_CFG_CONFIGURATION);

  with TWCConfig(AData) do begin
    RJSection := Root.AddSection(HashToConfig(CFG_RESTJSON_SEC)^.NAME_STR);
    RJSection.AddValue(CFG_RESTJSON_DB, wccrString);
  end;
end;

{ TRESTJsonConfigHelper }

function TRESTJsonConfigHelper.GetItemsDb : UTF8String;
begin
  Result := FItemsDB.Value;
end;

constructor TRESTJsonConfigHelper.Create;
begin
  FItemsDB := TThreadUtf8String.Create(cItemsDb);
end;

procedure TRESTJsonConfigHelper.DoHelp(AData : TObject);
begin
  case TWCConfigRecord(AData).HashName of
    CFG_RESTJSON_DB :
      FItemsDB.Value := TWCConfigRecord(AData).Value;
  end;
end;

destructor TRESTJsonConfigHelper.Destroy;
begin
  FItemsDB.Free;
  inherited Destroy;
end;

class function TRESTJsonConfigHelper.Config : TRESTJsonConfigHelper;
begin
  if not assigned(vRJServerConfigHelper) then
    vRJServerConfigHelper := TRESTJsonConfigHelper.Create;
  Result := vRJServerConfigHelper;
end;

end.

