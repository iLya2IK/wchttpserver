{
  This file is a part of example.
  look more in WCRESTJsonDemo.lpr
}

unit WCRESTJsonJobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants,
  httpdefs, httpprotocol,
  jsonscanner, jsonparser, fpjson,
  ExtSqlite3DS,
  wcApplication;

type

  { TWCAddClient }

  TWCAddClient = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCGetClientByName }

  TWCGetClientByName = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCGetItem }

  TWCGetItem = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCAddItem }

  TWCAddItem = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCAddToBasket }

  TWCAddToBasket = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCRemoveFromBasket }

  TWCRemoveFromBasket = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCGetBasket }

  TWCGetBasket = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TRESTJsonItemsDB }

  TRESTJsonItemsDB = class(TWCHTTPAppInitHelper)
  private
    FItemsDB : TExtSqlite3Dataset;
  public
    PREP_AddClient,
    PREP_GetClientByName,
    PREP_AddItem,
    PREP_GetItem,
    PREP_AddToBasket,
    PREP_RemoveFromBasket,
    PREP_GetBasket : TSqlite3Prepared;
    constructor Create;
    procedure DoHelp({%H-}aData : TObject); override;
    destructor Destroy; override;

    class function ItemsDB : TRESTJsonItemsDB;
  end;

implementation

uses wcutils, WCRESTJsonAppHelper;

const BAD_JSON = '{"result":"BAD"}';
      OK_JSON  = '{"result":"OK"}';
      cOK = 'OK';

      cNAME      = 'name';
      cSHRT_NAME = 'shrtName';
      cFULL_NAME = 'fullName';
      cDESCR     = 'descr';
      cCOST      = 'cost';
      cRESULT    = 'result';
      cCID       = 'cId';
      cIID       = 'iId';

var vItemsDB : TRESTJsonItemsDB = nil;

function AddClient(const Name : String) : String;
begin
  try
    vItemsDB.PREP_AddClient.Execute([Name]);
    Result := OK_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function GetClientByName(const Name : String) : String;
var
  jsonObj : TJSONObject;
begin
  try
    Result := vItemsDB.PREP_GetClientByName.QuickQuery([Name], nil, false);
    if Length(Result) > 0 then
    begin
      jsonObj := TJSONObject.Create([cCID,    StrToInt(Result),
                                     cRESULT, cOK
                                     ]);
      try
        jsonObj.CompressedJSON:=true;
        Result := jsonObj.AsJSON;
      finally
        jsonObj.Free;
      end;
    end else
      Result := BAD_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function GetItem(iid : integer) : String;
var
  jsonObj : TJSONObject;
begin
  vItemsDB.PREP_GetItem.Lock;
  try
    if vItemsDB.PREP_GetItem.Open([iid]) then
    begin
      jsonObj := TJSONObject.Create([cSHRT_NAME, vItemsDB.PREP_GetItem.Columns[0],
                                     cFULL_NAME, vItemsDB.PREP_GetItem.Columns[1],
                                     cDESCR,     vItemsDB.PREP_GetItem.Columns[2],
                                     cCOST,      StrToInt(vItemsDB.PREP_GetItem.Columns[3]),
                                     cRESULT,    cOK
                                     ]);
      try
        jsonObj.CompressedJSON:=true;
        Result := jsonObj.AsJSON;
      finally
        jsonObj.Free;
      end;
    end else
      Result := BAD_JSON;
    vItemsDB.PREP_GetItem.Close;
  finally
    vItemsDB.PREP_GetItem.UnLock;
  end;
end;

function AddItem(iid : integer;  //set iid < 0 to add new item
                                 //set iid > 0 to set new properties for existing item
                   const itShrtName, itFullName, itDescr : String;
                                 //set strings to empty to leave text props unchanged
                   itCost : integer //set itCost < 0 to leave cost unchanged
                   ) : String;
begin
  try
    vItemsDB.PREP_AddItem.Execute([iid, itShrtName, itFullName, itDescr, itCost]);
    Result := OK_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function AddToBasket(cid, iid, cost : integer) : String;
begin
  try
    vItemsDB.PREP_AddToBasket.Execute([cid, iid, cost]);
    Result := OK_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function RemoveFromBasket(cid, iid : integer) : String;
begin
  try
    vItemsDB.PREP_RemoveFromBasket.Execute([cid, iid]);
    Result := OK_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function GetBasket(cid : integer) : String;
var
  jsonElement : TJSONObject;
  jsonResponse : TJSONArray;
  amount : integer;
begin
  jsonResponse := TJSONArray.Create;
  try
   vItemsDB.PREP_GetBasket.Lock;
    try
      amount := 0;
      if vItemsDB.PREP_GetBasket.Open([cid]) then
      repeat
        inc(amount);

        jsonElement := TJSONObject.Create([cIID,  StrToInt(vItemsDB.PREP_GetBasket.Columns[0]),
                                           cCOST, StrToInt(vItemsDB.PREP_GetBasket.Columns[1])]);
        jsonElement.CompressedJSON:=true;
        jsonResponse.Add(jsonElement);
      until (not vItemsDB.PREP_GetBasket.Step) or (amount > 100);
      vItemsDB.PREP_GetBasket.Close;
    finally
      vItemsDB.PREP_GetBasket.UnLock;
    end;
    jsonResponse.CompressedJSON := true;
    Result := jsonResponse.AsJSON;
  finally
    jsonResponse.Free;
  end;
end;

{ TRESTJsonItemsDB }

constructor TRESTJsonItemsDB.Create;
begin
  FItemsDB := TExtSqlite3Dataset.Create(nil);
end;

procedure TRESTJsonItemsDB.DoHelp({%H-}aData : TObject);
begin
  try
    FItemsDB.FileName := Application.SitePath + TRESTJsonConfigHelper.Config.ItemsDB;
    FItemsDB.ExecSQL(
    'create table if not exists clients'+
      '(id integer primary key autoincrement, '+
       'name text);');
    FItemsDB.ExecSQL(
    'create table if not exists items'+
      '(id integer primary key autoincrement, '+
       'shrtname text,'+
       'fullname text,'+
       'descr text,'+
       'cost integer);');
    FItemsDB.ExecSQL(
    'create table if not exists baskets'+
      '(id integer primary key autoincrement, '+
       'cid integer references clients(id),'+
       'iid integer references items(id),'+
       'cost integer);');

    PREP_AddClient := FItemsDB.AddNewPrep('WITH new (name) AS ( VALUES(?1) ) '+
                        'INSERT OR REPLACE INTO clients (id, name) '+
                        'SELECT old.id, new.name '+
                        'FROM new LEFT JOIN clients AS old ON '+
                        'new.name = old.name;');
    PREP_GetClientByName := FItemsDB.AddNewPrep('select id from clients where name == ?1;');
    PREP_AddItem := FItemsDB.AddNewPrep('WITH new (id, shrtname, fullname, descr, cost) AS ( VALUES(?1, ?2, ?3, ?4, ?5) ) '+
                        'INSERT OR REPLACE INTO items (id, shrtname, fullname, descr, cost) '+
                        'SELECT old.id, '+
                        'case when length(new.shrtname)=0 then IFNULL(old.shrtname,"") else new.shrtname end, '+
                        'case when length(new.fullname)=0 then IFNULL(old.fullname,"") else new.fullname end, '+
                        'case when length(new.descr)=0 then IFNULL(old.descr,"") else new.descr end, '+
                        'case when new.cost<0 then IFNULL(old.cost,0) else new.cost end '+
                        'FROM new LEFT JOIN items AS old ON '+
                        'new.id == old.id;');
    PREP_GetItem := FItemsDB.AddNewPrep('select shrtname, fullname, descr, cost from items where id == ?1;');
    PREP_AddToBasket := FItemsDB.AddNewPrep('WITH new (cid, iid, cost) AS ( VALUES(?1, ?2, ?3) ) '+
                        'INSERT OR REPLACE INTO baskets (id, cid, iid, cost) '+
                        'SELECT old.id, new.cid, new.iid, '+
                        'ifnull(case when new.cost <= 0 then (select cost from items where items.id == new.iid limit 1) else new.cost end, 0) '+
                        'FROM new LEFT JOIN baskets AS old ON '+
                        'new.cid = old.cid AND '+
                        'new.iid = old.iid;');
    PREP_RemoveFromBasket := FItemsDB.AddNewPrep('DELETE FROM baskets WHERE cid == ?1 and iid == ?2');
    PREP_GetBasket := FItemsDB.AddNewPrep('SELECT iid, cost FROM baskets where cid == ?1;');
  except
    on E : Exception do
    begin
      Application.DoError(E.ToString);
      Application.NeedShutdown := true;
    end;
  end;
end;

destructor TRESTJsonItemsDB.Destroy;
begin
  FItemsDB.Free;
  inherited Destroy;
end;

class function TRESTJsonItemsDB.ItemsDB : TRESTJsonItemsDB;
begin
  if not assigned(vItemsDB) then
    vItemsDB := TRESTJsonItemsDB.Create;
  Result := vItemsDB;
end;

{ TWCRemoveFromBasket }

procedure TWCRemoveFromBasket.Execute;
begin
  if DecodeParamsWithDefault(Request.QueryFields, [cCID, cIID],
                             Request.Content, Params, [-1, -1]) then
  begin
    if (Params[0] > 0) and (Params[1] > 0) then
      Response.Content := RemoveFromBasket(Params[0], Params[1]) else
      Response.Content := BAD_JSON;
  end else Response.Content := BAD_JSON;
  inherited Execute;
end;

{ TWCGetBasket }

procedure TWCGetBasket.Execute;
var cid : integer;
begin
  if TryStrToInt(Request.QueryFields.Values[cCID], cid) then
    Response.Content := GetBasket(cid) else
    Response.Content := BAD_JSON;
  inherited Execute;
end;

{ TWCAddToBasket }

procedure TWCAddToBasket.Execute;
begin
  if DecodeParamsWithDefault(Request.QueryFields, [cCID, cIID, cCOST],
                             Request.Content, Params, [-1, -1, -1]) then
  begin
    if (Params[0] > 0) and (Params[1] > 0) then
      Response.Content := AddToBasket(Params[0], Params[1], Params[2]) else
      Response.Content := BAD_JSON;
  end else Response.Content := BAD_JSON;
  inherited Execute;
end;

{ TWCAddItem }

procedure TWCAddItem.Execute;
begin
  if DecodeParamsWithDefault(Request.QueryFields, [cIID, cCOST, cSHRT_NAME,
                                                   cFULL_NAME, cDESCR],
                             Request.Content, Params, [-1, -1, '', '', '']) then
  begin
    Response.Content := AddItem(Params[0], Params[2], Params[3],
                                           Params[4], Params[1]);
  end else Response.Content := BAD_JSON;
  inherited Execute;
end;

{ TWCGetClientByName }

procedure TWCGetClientByName.Execute;
begin
  Response.Content := GetClientByName(Request.QueryFields.Values[cNAME]);
  inherited Execute;
end;

{ TWCAddClient }

procedure TWCAddClient.Execute;
begin
  Response.Content := AddClient(Request.QueryFields.Values[cNAME]);
  inherited Execute;
end;

{ TWCGetItem }

procedure TWCGetItem.Execute;
var iid : integer;
begin
  if TryStrToInt(Request.QueryFields.Values[cIID], iid) then
    Response.Content := GetItem(iid) else
    Response.Content := BAD_JSON;
  inherited Execute;
end;

end.

