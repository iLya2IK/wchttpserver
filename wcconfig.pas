unit wcconfig;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, DateUtils,
  OGLFastList,ECommonObjs,
  jsonscanner, jsonparser, fpjson, Variants;

type
  TWCConfigRec = record
    Name : PChar;
    Hash : Cardinal;
  public
    function NAME_STR : String;
  end;
  PWCConfigRec = ^TWCConfigRec;

  TWCConfigRecordKind = (wccrUnknown, wccrRoot, wccrSection,
                         wccrString, wccrBoolean, wccrInteger);

  TWCConfigRecord = class;

  TWCConfigValueEvent = procedure (Sender : TWCConfigRecord) of object;

  { TWCConfigRecords }

  TWCConfigRecords = class(TFastCollection)
  private
    function GetRecord(index : integer): TWCConfigRecord;
    procedure SetRecord(index : integer; AValue: TWCConfigRecord);
  public
    property Records[index : integer] : TWCConfigRecord read GetRecord
                                                 write SetRecord;
  end;

  { TWCConfigRecord }

  TWCConfigRecord = class
  private
    FName : String;
    FHashName : Cardinal;
    FValue : Variant;
    FDefaultValue : Variant;
    FKind : TWCConfigRecordKind;
    FChildren : TWCConfigRecords;
    function GetByHash(hash : Cardinal): TWCConfigRecord;
    function GetByName(const aName : String): TWCConfigRecord;
    function GetCount: Integer;
    function GetRecord(index : integer): TWCConfigRecord;
  protected
    property Count : Integer read GetCount;
  public
    constructor Create(Rec : PWCConfigRec; aKind : TWCConfigRecordKind);
    constructor Create(const aName : String; aKind : TWCConfigRecordKind);
    constructor Create(const aName : String; aHash : Cardinal; aKind : TWCConfigRecordKind);
    destructor Destroy; override;
    procedure AddChild(AChild : TWCConfigRecord);
    property Name : String read FName;
    property HashName : Cardinal read FHashName write FHashName;
    property Value : Variant read FValue;
    property DefaultValue : Variant read FDefaultValue;
    function AddSection(const aName : String) : TWCConfigRecord;
    function AddValue(const aName : String; aDefault : Variant) : TWCConfigRecord; overload;
    function AddValue(const aName: String; aKind: TWCConfigRecordKind
      ): TWCConfigRecord; overload;
    function AddValue(Hash: Cardinal; aKind: TWCConfigRecordKind
      ): TWCConfigRecord; overload;
    property Records[index : integer] : TWCConfigRecord read GetRecord; default;
    property ByHash[hash : Cardinal] : TWCConfigRecord read GetByHash;
    property ByName[const aName : String] : TWCConfigRecord read GetByName;
    function SetNewValue(const ANewValue : Variant) : Boolean;
    procedure SetDefaultValue(const ANewValue : Variant);
  end;

  { TWCConfig }

  TWCConfig = class(TThreadSafeObject)
  private
    FDataTime   : TDateTime;
    FConfigFile : String;
    FJsonConfig : TJSONObject;
    FRootConfig : TWCConfigRecord;
    FOnChangeValue : TWCConfigValueEvent;
    procedure Reload;
    procedure SetConfigFileName(AValue: String);
    procedure SyncConfigRecord(aRecord: TWCConfigRecord; Parent: TJSONObject;
                                        Forced : Boolean);
  public
    constructor Create(const aFileName : String);
    destructor Destroy; override;
    procedure Sync(Forced : Boolean);
    property Root : TWCConfigRecord read FRootConfig;
    procedure SetDefaultValue(aHash : Cardinal; const ANewValue : Variant); overload;
    procedure SetDefaultValue(const aName : String; const ANewValue : Variant); overload;
    property FileName : String read FConfigFile write SetConfigFileName;
    property OnChangeValue : TWCConfigValueEvent read FOnChangeValue write
                                                      FOnChangeValue;
  end;

const
  MAX_OPTS = 14;

type
  TWCConfiguration = packed record
    case byte of
    0:(
    CFG_SITE_FOLDER    : TWCConfigRec;
    CFG_SERVER_NAME    : TWCConfigRec;
    CFG_MAIN_URI       : TWCConfigRec;
    CFG_SESSIONS_LOC   : TWCConfigRec;
    CFG_CLIENTS_DB     : TWCConfigRec;
    CFG_LOG_DB         : TWCConfigRec;
    CFG_MIME_NAME      : TWCConfigRec;
    CFG_USE_SSL        : TWCConfigRec;
    CFG_HOST_NAME      : TWCConfigRec;
    CFG_SSL_LOC        : TWCConfigRec;
    CFG_SSL_CIPHER     : TWCConfigRec;
    CFG_PRIVATE_KEY    : TWCConfigRec;
    CFG_CERTIFICATE    : TWCConfigRec;
    CFG_TLSKEY_LOG     : TWCConfigRec;
    CFG_ALPN_USE_HTTP2 : TWCConfigRec;
    );
    1 : (CNFGS : Array [0..MAX_OPTS] of TWCConfigRec);
  end;

const
  CFG_MAIN_SEC       = 'Main';
  CFG_OPENSSL_SEC    = 'OpenSSL';

  CFG_SITE_FOLDER_HASH    = $10;
  CFG_SERVER_NAME_HASH    = $20;
  CFG_MAIN_URI_HASH       = $30;
  CFG_SESSIONS_LOC_HASH   = $40;
  CFG_CLIENTS_DB_HASH     = $50;
  CFG_LOG_DB_HASH         = $60;
  CFG_MIME_NAME_HASH      = $70;
  CFG_USE_SSL_HASH        = $11;
  CFG_HOST_NAME_HASH      = $21;
  CFG_SSL_LOC_HASH        = $31;
  CFG_SSL_CIPHER_HASH     = $41;
  CFG_PRIVATE_KEY_HASH    = $51;
  CFG_CERTIFICATE_HASH    = $61;
  CFG_TLSKEY_LOG_HASH     = $71;
  CFG_ALPN_USE_HTTP2_HASH = $81;

  CFG_CONFIGURATION : TWCConfiguration = (
    CFG_SITE_FOLDER    : (Name:'SiteFolder';    Hash:CFG_SITE_FOLDER_HASH    );
    CFG_SERVER_NAME    : (Name:'ServerName';    Hash:CFG_SERVER_NAME_HASH    );
    CFG_MAIN_URI       : (Name:'MainURI';       Hash:CFG_MAIN_URI_HASH       );
    CFG_SESSIONS_LOC   : (Name:'SessionsLoc';   Hash:CFG_SESSIONS_LOC_HASH   );
    CFG_CLIENTS_DB     : (Name:'ClientsDb';     Hash:CFG_CLIENTS_DB_HASH     );
    CFG_LOG_DB         : (Name:'LogDb';         Hash:CFG_LOG_DB_HASH         );
    CFG_MIME_NAME      : (Name:'MimeName';      Hash:CFG_MIME_NAME_HASH      );
    CFG_USE_SSL        : (Name:'UseSSL';        Hash:CFG_USE_SSL_HASH        );
    CFG_HOST_NAME      : (Name:'HostName';      Hash:CFG_HOST_NAME_HASH      );
    CFG_SSL_LOC        : (Name:'SSLLoc';        Hash:CFG_SSL_LOC_HASH        );
    CFG_SSL_CIPHER     : (Name:'SSLCipherList'; Hash:CFG_SSL_CIPHER_HASH     );
    CFG_PRIVATE_KEY    : (Name:'PrivateKeyLoc'; Hash:CFG_PRIVATE_KEY_HASH    );
    CFG_CERTIFICATE    : (Name:'CertificateLoc';Hash:CFG_CERTIFICATE_HASH    );
    CFG_TLSKEY_LOG     : (Name:'TLSKeyLog';     Hash:CFG_TLSKEY_LOG_HASH     );
    CFG_ALPN_USE_HTTP2 : (Name:'UseHTTP2';      Hash:CFG_ALPN_USE_HTTP2_HASH )
    );

function  StrToConfig(const str : String) : PWCConfigRec;
function  HashToConfig(hash : Cardinal) : PWCConfigRec;
function  StrConfigToHash(const str : String) : Cardinal;

implementation

function StrToConfig(const str: String): PWCConfigRec;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION.CNFGS) do
  begin
    if SameStr(str, CFG_CONFIGURATION.CNFGS[i].NAME_STR) then
    begin
      Result := @(CFG_CONFIGURATION.CNFGS[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function HashToConfig(hash: Cardinal): PWCConfigRec;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION.CNFGS) do
  begin
    if hash = CFG_CONFIGURATION.CNFGS[i].Hash then
    begin
      Result := @(CFG_CONFIGURATION.CNFGS[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function StrConfigToHash(const str: String): Cardinal;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION.CNFGS) do
  begin
    if SameStr(str, CFG_CONFIGURATION.CNFGS[i].NAME_STR) then
    begin
      Result := CFG_CONFIGURATION.CNFGS[i].Hash;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TWCConfigRec }

function TWCConfigRec.NAME_STR: String;
begin
  Result := StrPas(Name);
end;

{ TWCConfigRecords }

function TWCConfigRecords.GetRecord(index : integer): TWCConfigRecord;
begin
  Result := TWCConfigRecord(Item[index]);
end;

procedure TWCConfigRecords.SetRecord(index : integer; AValue: TWCConfigRecord);
begin
  Item[index] := AValue;
end;

{ TWCConfigRecord }

function TWCConfigRecord.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TWCConfigRecord.GetByHash(hash : Cardinal): TWCConfigRecord;
var i : integer;
begin
  if FHashName = hash then Result := Self else
  begin
    Result := nil;
    for i := 0 to Count-1 do
     if not Assigned(Result) then
       Result := Records[i].GetByHash(hash) else
       Break;
  end;
end;

function TWCConfigRecord.GetByName(const aName : String): TWCConfigRecord;
var i : integer;
begin
  if SameText(FName, aName) then Result := Self else
  begin
    Result := nil;
    for i := 0 to Count-1 do
     if not Assigned(Result) then
       Result := Records[i].GetByName(aName) else
       Break;
  end;
end;

function TWCConfigRecord.GetRecord(index : integer): TWCConfigRecord;
begin
  Result := FChildren.Records[index];
end;

constructor TWCConfigRecord.Create(Rec: PWCConfigRec; aKind: TWCConfigRecordKind
  );
begin
  Create(Rec^.NAME_STR, Rec^.Hash, aKind);
end;

constructor TWCConfigRecord.Create(const aName: String;
  aKind: TWCConfigRecordKind);
var Rec : PWCConfigRec;
begin
  if aKind in [wccrRoot, wccrSection] then
  begin
    Create(aName, 0, aKind);
  end else begin
    Rec := StrToConfig(aName);
    Create(Rec, aKind);
  end;
end;

constructor TWCConfigRecord.Create(const aName: String; aHash: Cardinal;
  aKind: TWCConfigRecordKind);
begin
  FChildren := TWCConfigRecords.Create;
  FName:= aName;
  FHashName:= aHash;
  FKind:= aKind;
  FValue:= Null;
  FDefaultValue:= Null;
end;

destructor TWCConfigRecord.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

procedure TWCConfigRecord.AddChild(AChild: TWCConfigRecord);
begin
  FChildren.Add(AChild);
end;

function TWCConfigRecord.AddSection(const aName: String): TWCConfigRecord;
begin
  Result := TWCConfigRecord.Create(aName, wccrSection);
  AddChild(Result);
end;

function TWCConfigRecord.AddValue(const aName: String; aDefault: Variant
  ): TWCConfigRecord;
var aKind : TWCConfigRecordKind;
begin
  aKind := wccrUnknown;
  if VarIsStr(aDefault) then aKind:= wccrString else
  if VarIsBool(aDefault) then aKind:= wccrBoolean else
  if VarIsOrdinal(aDefault) then aKind:= wccrInteger;
  if aKind > wccrUnknown then
  begin
    Result := TWCConfigRecord.Create(aName, aKind);
    Result.SetDefaultValue(aDefault);
    AddChild(Result);
  end else
    Result := nil;
end;

function TWCConfigRecord.AddValue(const aName: String;
                                  aKind : TWCConfigRecordKind): TWCConfigRecord;
begin
  if aKind > wccrUnknown then
  begin
    Result := TWCConfigRecord.Create(aName, aKind);
    AddChild(Result);
  end else
    Result := nil;
end;

function TWCConfigRecord.AddValue(Hash: Cardinal; aKind: TWCConfigRecordKind
  ): TWCConfigRecord;
var R : PWCConfigRec;
begin
  if aKind > wccrUnknown then
  begin
    R := HashToConfig(Hash);
    if Assigned(R) then begin
      Result := TWCConfigRecord.Create(R, aKind);
      AddChild(Result);
    end else Result := nil;
  end else
    Result := nil;
end;

function TWCConfigRecord.SetNewValue(const ANewValue: Variant): Boolean;
begin
  if VarCompareValue(FValue, ANewValue) <> vrEqual then
  begin
    Result := true;
    FValue:= ANewValue;
  end else Result := false;
end;

procedure TWCConfigRecord.SetDefaultValue(const ANewValue: Variant);
begin
  FDefaultValue:= ANewValue;
end;

{ TWCConfig }

procedure TWCConfig.Reload;
var SL : TStringList;
    jsonData : TJSONData;
begin
 if FileExists(FConfigFile) then
 begin
   SL := TStringList.Create;
   try
     SL.LoadFromFile(FConfigFile);
     try
       jsonData := GetJSON(SL.Text);
       if Assigned(jsonData) and
          (jsonData.JSONType = jtObject) then
       begin
         if Assigned(FJsonConfig) then FreeAndNil(FJsonConfig);
         FJsonConfig := TJSONObject(jsonData);
       end;
     except
       // do nothing. maybe broken json file
     end;
   finally
     SL.Free;
   end;
 end;
end;

procedure TWCConfig.SetConfigFileName(AValue: String);
begin
  Lock;
  try
    if FConfigFile=AValue then Exit;
    FConfigFile:=AValue;
    FDataTime := EncodeDate(1990, 1, 1);
    Reload;
  finally
    UnLock;
  end;
end;

procedure TWCConfig.SyncConfigRecord(aRecord: TWCConfigRecord;
  Parent: TJSONObject; Forced : Boolean);
var i : integer;
    aValue : Variant;
    ParentData : TJSONData;
begin
  for i := 0 to aRecord.Count-1 do
  begin
    if aRecord[i].FKind in [wccrString, wccrBoolean,
                         wccrInteger] then
    begin
      if VarIsNull(aRecord[i].DefaultValue) then
      begin
        ParentData := Parent.Find(aRecord[i].Name);
        if assigned(ParentData) then
        begin
          aValue := ParentData.Value;
        end else begin
          aValue := Null;
        end;
      end else begin
        case aRecord[i].FKind of
        wccrString  : aValue := Parent.Get(aRecord[i].Name,
                                           String(aRecord[i].DefaultValue));
        wccrBoolean : aValue := Parent.Get(aRecord[i].Name,
                                           Boolean(aRecord[i].DefaultValue));
        wccrInteger : aValue := Parent.Get(aRecord[i].Name,
                                           Integer(aRecord[i].DefaultValue));
        end;
      end;
      if (aRecord[i].SetNewValue(aValue) or forced) and
          assigned(FOnChangeValue) then
      begin
        FOnChangeValue(aRecord[i]);
      end;
    end else begin
      if assigned(Parent) then
        ParentData := Parent.Find(aRecord[i].Name) else
        ParentData := FJsonConfig.Find(aRecord[i].Name);
      if ParentData.JSONType = jtObject then
        SyncConfigRecord(aRecord[i], TJSONObject(ParentData), Forced);
    end;
  end;
end;

constructor TWCConfig.Create(const aFileName: String);
begin
  inherited Create;
  FJsonConfig := nil;
  FConfigFile := '';
  if assigned(FRootConfig) then FreeAndNil(FRootConfig);
  FRootConfig := TWCConfigRecord.Create('', wccrRoot);
  SetConfigFileName(aFileName);
end;

destructor TWCConfig.Destroy;
begin
  if assigned(FJsonConfig) then FreeAndNil(FJsonConfig);
  FRootConfig.Free;
  inherited Destroy;
end;

procedure TWCConfig.Sync(Forced: Boolean);
var cDT : TDateTime;
begin
  Lock;
  try
    if FileExists(FConfigFile) then
    begin
      FileAge(FConfigFile, cDT);
      if SecondsBetween(cDT, FDataTime) > 0 then
      begin
        FDataTime := cDT;
        Reload;
        if assigned(FJsonConfig) then
          SyncConfigRecord(FRootConfig, FJsonConfig, Forced);
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCConfig.SetDefaultValue(aHash: Cardinal; const ANewValue: Variant);
var R : TWCConfigRecord;
begin
  Lock;
  try
    R := FRootConfig.ByHash[aHash];
    if assigned(R) then begin
      if VarIsNull(R.Value) and Assigned(FOnChangeValue) then
      begin
        R.SetNewValue(ANewValue);
        FOnChangeValue(R);
      end;
      R.SetDefaultValue(ANewValue);
    end;
  finally
    UnLock;
  end;
end;

procedure TWCConfig.SetDefaultValue(const aName: String;
  const ANewValue: Variant);
var R : TWCConfigRecord;
begin
  Lock;
  try
    R := FRootConfig.ByName[aName];
    if assigned(R) then begin
      if VarIsNull(R.Value) and Assigned(FOnChangeValue) then
      begin
        R.SetNewValue(ANewValue);
        FOnChangeValue(R);
      end;
      R.SetDefaultValue(ANewValue);
    end;
  finally
    UnLock;
  end;
end;

end.

