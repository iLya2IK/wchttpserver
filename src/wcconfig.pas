{
 wcConfig:
   Custom application config class, structures and utils

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcConfig;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  {$ifdef wiki_docs}
  commonutils,
  {$endif}
  Classes, SysUtils, DateUtils,
  OGLFastList,ECommonObjs,
  jsonscanner, jsonparser, fpjson, Variants;

type
  TWCConfigRec = record
    ParentHash : Cardinal;
    Hash       : Cardinal;
    Name       : PChar;
  public
    function NAME_STR : String;
  end;
  PWCConfigRec = ^TWCConfigRec;

  TWCConfigRecordKind = (wccrUnknown, wccrRoot, wccrSection,
                         wccrString, wccrBoolean, wccrInteger);

  TWCConfigRecord = class;

  TWCConfigValueEvent = procedure (Sender : TWCConfigRecord) of object;
  TWCConfigLoadedEvent = procedure (Config : TJSONObject) of object;

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
    FParent :  TWCConfigRecord;
    function GetByHash(hash : Cardinal): TWCConfigRecord;
    function GetByName(const aName : String): TWCConfigRecord;
    function GetCount: Integer;
    function GetRecord(index : integer): TWCConfigRecord;
  protected
    property Count : Integer read GetCount;
  public
    constructor Create(aParent : TWCConfigRecord; Rec : PWCConfigRec; aKind : TWCConfigRecordKind);
    constructor Create(aParent : TWCConfigRecord; const aName : String; aKind : TWCConfigRecordKind);
    constructor Create(aParent : TWCConfigRecord; const aName : String; aHash : Cardinal; aKind : TWCConfigRecordKind);
    destructor Destroy; override;
    procedure AddChild(AChild : TWCConfigRecord);
    property Name : String read FName;
    property HashName : Cardinal read FHashName write FHashName;
    property Value : Variant read FValue;
    property DefaultValue : Variant read FDefaultValue;
    function AddSection(const aName : String) : TWCConfigRecord;
    function AddValue(const aName : String; aDefault : Variant) : TWCConfigRecord; overload;
    function AddValue(Hash: Cardinal; aDefault : Variant) : TWCConfigRecord; overload;
    function AddValue(const aName : String; aKind: TWCConfigRecordKind
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
    FOnConfigLoaded : TWCConfigLoadedEvent;
    procedure Reload;
    procedure SetConfigFileName(AValue: String);
    procedure SyncConfigRecord(aRecord: TWCConfigRecord; Parent: TJSONObject;
                                        Forced : Boolean);
  protected
    procedure DoInitialize; virtual; abstract;
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
    property OnConfigLoaded : TWCConfigLoadedEvent read FOnConfigLoaded write
                                                      FOnConfigLoaded;
  end;

type
  TWCConfiguration = Array of TWCConfigRec;

const  CFG_ROOT_HASH = $00;

procedure AddWCConfiguration(const cfg : TWCConfiguration);
function  StrToConfig(Parent : Cardinal; const str : String) : PWCConfigRec;
function  HashToConfig(hash : Cardinal) : PWCConfigRec;
function  StrConfigToHash(Parent : Cardinal; const str : String) : Cardinal;
function  VarToConfigKind(const V : Variant) : TWCConfigRecordKind;

{$ifndef wiki_docs}
var CFG_CONFIGURATION : TWCConfiguration = ();
{$endif}

implementation

procedure AddWCConfiguration(const cfg : TWCConfiguration);
var l1, l2, i : integer;
begin
  l1 := Length(CFG_CONFIGURATION);
  l2 := Length(cfg);
  SetLength(CFG_CONFIGURATION, l1 + l2);
  for i := l1 to l1 + l2 - 1 do
  begin
    CFG_CONFIGURATION[i] := cfg[i - l1];
  end;
end;

function StrToConfig(Parent : Cardinal; const str: String): PWCConfigRec;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION) do
  begin
    if (CFG_CONFIGURATION[i].ParentHash = Parent) and
        SameStr(str, CFG_CONFIGURATION[i].NAME_STR) then
    begin
      Result := @(CFG_CONFIGURATION[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function HashToConfig(hash: Cardinal): PWCConfigRec;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION) do
  begin
    if hash = CFG_CONFIGURATION[i].Hash then
    begin
      Result := @(CFG_CONFIGURATION[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function StrConfigToHash(Parent: Cardinal; const str: String): Cardinal;
var i : integer;
begin
  for i := 0 to High(CFG_CONFIGURATION) do
  begin
    if (CFG_CONFIGURATION[i].ParentHash = Parent) and
            SameStr(str, CFG_CONFIGURATION[i].NAME_STR) then
    begin
      Result := CFG_CONFIGURATION[i].Hash;
      Exit;
    end;
  end;
  Result := 0;
end;

function VarToConfigKind(const V: Variant): TWCConfigRecordKind;
begin
  Result := wccrUnknown;
  if VarIsStr(V) then Result:= wccrString else
  if VarIsBool(V) then Result:= wccrBoolean else
  if VarIsOrdinal(V) then Result:= wccrInteger;
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

constructor TWCConfigRecord.Create(aParent : TWCConfigRecord; Rec: PWCConfigRec;
                                    aKind: TWCConfigRecordKind
  );
begin
  Create(aParent, Rec^.NAME_STR, Rec^.Hash, aKind);
end;

constructor TWCConfigRecord.Create(aParent : TWCConfigRecord; const aName: String;
  aKind: TWCConfigRecordKind);
var Rec : PWCConfigRec;
begin
  if aKind in [wccrRoot] then
  begin
    Create(aParent, aName, CFG_ROOT_HASH, aKind);
  end else begin
    Rec := StrToConfig(aParent.HashName, aName);
    Create(aParent, Rec, aKind);
  end;
end;

constructor TWCConfigRecord.Create(aParent : TWCConfigRecord;
  const aName: String; aHash: Cardinal;
  aKind: TWCConfigRecordKind);
begin
  FChildren := TWCConfigRecords.Create;
  FParent := aParent;
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
  Result := TWCConfigRecord.Create(Self, aName, wccrSection);
  AddChild(Result);
end;

function TWCConfigRecord.AddValue(const aName: String; aDefault: Variant
  ): TWCConfigRecord;
var aKind : TWCConfigRecordKind;
begin
  aKind := VarToConfigKind(aDefault);
  if aKind > wccrUnknown then
  begin
    Result := TWCConfigRecord.Create(Self, aName, aKind);
    Result.SetDefaultValue(aDefault);
    AddChild(Result);
  end else
    Result := nil;
end;

function TWCConfigRecord.AddValue(Hash: Cardinal; aDefault: Variant
  ): TWCConfigRecord;
var R : PWCConfigRec;
    aKind : TWCConfigRecordKind;
begin
  aKind := VarToConfigKind(aDefault);
  if aKind > wccrUnknown then
  begin
    R := HashToConfig(Hash);
    if Assigned(R) then begin
      Result := TWCConfigRecord.Create(Self, R, aKind);
      Result.SetDefaultValue(aDefault);
      AddChild(Result);
    end else Result := nil;
  end else
    Result := nil;
end;

function TWCConfigRecord.AddValue(const aName: String;
                                  aKind : TWCConfigRecordKind): TWCConfigRecord;
begin
  if aKind > wccrUnknown then
  begin
    Result := TWCConfigRecord.Create(Self, aName, aKind);
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
      Result := TWCConfigRecord.Create(Self, R, aKind);
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
       end else
         if assigned(jsonData) then FreeAndNil(jsonData);
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
    if aRecord[i].FKind in [wccrString, wccrBoolean, wccrInteger] then
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
      if assigned(ParentData) then
        if ParentData.JSONType = jtObject then
          SyncConfigRecord(aRecord[i], TJSONObject(ParentData), Forced);
    end;
  end;
end;

constructor TWCConfig.Create(const aFileName: String);
begin
  inherited Create;

  FOnConfigLoaded := nil;
  FOnChangeValue := nil;
  FJsonConfig := nil;
  FConfigFile := '';
  if assigned(FRootConfig) then FreeAndNil(FRootConfig);
  FRootConfig := TWCConfigRecord.Create(nil, '', wccrRoot);

  DoInitialize;

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
        begin
          SyncConfigRecord(FRootConfig, FJsonConfig, Forced);
          if assigned(FOnConfigLoaded) then
          begin
            FOnConfigLoaded(FJsonConfig);
          end;
        end;
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

