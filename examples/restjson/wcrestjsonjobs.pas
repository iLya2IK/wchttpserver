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

  { TWCGetBasket }

  TWCGetBasket = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

procedure InitializeItemsDb;
procedure DoneItemsDb;

implementation

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

      cItemsDb = 'items.db';

var ItemsDB : TExtSqlite3Dataset;
    PREP_AddClient,
    PREP_GetClientByName,
    PREP_AddItem,
    PREP_GetItem,
    PREP_AddToBasket,
    PREP_GetBasket : TSqlite3Prepared;

procedure InitializeItemsDb;
begin
  ItemsDB := TExtSqlite3Dataset.Create(nil);
  try
    ItemsDB.FileName := Application.SitePath + cItemsDb;
    ItemsDB.ExecSQL(
    'create table if not exists clients'+
      '(id integer primary key autoincrement, '+
       'name text);');
    ItemsDB.ExecSQL(
    'create table if not exists items'+
      '(id integer primary key autoincrement, '+
       'shrtname text,'+
       'fullname text,'+
       'descr text,'+
       'cost integer);');
    ItemsDB.ExecSQL(
    'create table if not exists baskets'+
      '(id integer primary key autoincrement, '+
       'cid integer references clients(id),'+
       'iid integer references items(id),'+
       'cost integer);');

    PREP_AddClient := ItemsDB.AddNewPrep('WITH new (name) AS ( VALUES(?1) ) '+
                        'INSERT OR REPLACE INTO clients (id, name) '+
                        'SELECT old.id, new.name '+
                        'FROM new LEFT JOIN clients AS old ON '+
                        'new.name = old.name;');
    PREP_GetClientByName := ItemsDB.AddNewPrep('select id from clients where name == ?1;');
    PREP_AddItem := ItemsDB.AddNewPrep('WITH new (id, shrtname, fullname, descr, cost) AS ( VALUES(?1, ?2, ?3, ?4, ?5) ) '+
                        'INSERT OR REPLACE INTO items (id, shrtname, fullname, descr, cost) '+
                        'SELECT old.id, '+
                        'case when length(new.shrtname)=0 then IFNULL(old.shrtname,"") else new.shrtname end, '+
                        'case when length(new.fullname)=0 then IFNULL(old.fullname,"") else new.fullname end, '+
                        'case when length(new.descr)=0 then IFNULL(old.descr,"") else new.descr end, '+
                        'case when new.cost<0 then IFNULL(old.cost,0) else new.cost end '+
                        'FROM new LEFT JOIN items AS old ON '+
                        'new.id == old.id;');
    PREP_GetItem := ItemsDB.AddNewPrep('select shrtname, fullname, descr, cost from items where id == ?1;');
    PREP_AddToBasket := ItemsDB.AddNewPrep('WITH new (cid, iid, cost) AS ( VALUES(?1, ?2, ?3) ) '+
                        'INSERT OR REPLACE INTO baskets (id, cid, iid, cost) '+
                        'SELECT old.id, new.cid, new.iid, new.cost '+
                        'FROM new LEFT JOIN baskets AS old ON '+
                        'new.cid = old.cid AND '+
                        'new.iid = old.iid;');
    PREP_GetBasket := ItemsDB.AddNewPrep('SELECT iid, cost FROM baskets where cid == ?1;');
  except
    on E : Exception do
    begin
      Application.DoError(E.ToString);
      Application.NeedShutdown := true;
    end;
  end;
end;

procedure DoneItemsDb;
begin
  ItemsDB.Free;
end;

function AddClient(const Name : String) : String;
begin
  try
    PREP_AddClient.Execute([Name]);
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
    Result := PREP_GetClientByName.QuickQuery([Name], nil, false);
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
  PREP_GetItem.Lock;
  try
    if PREP_GetItem.Open([iid]) then
    begin
      jsonObj := TJSONObject.Create([cSHRT_NAME, PREP_GetItem.Columns[0],
                                     cFULL_NAME, PREP_GetItem.Columns[1],
                                     cDESCR,     PREP_GetItem.Columns[2],
                                     cCOST,      StrToInt(PREP_GetItem.Columns[3]),
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
    PREP_GetItem.Close;
  finally
    PREP_GetItem.UnLock;
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
    PREP_AddItem.Execute([iid, itShrtName, itFullName, itDescr, itCost]);
    Result := OK_JSON;
  except
    Result := BAD_JSON;
  end;
end;

function AddToBasket(cid, iid, cost : integer) : String;
begin
  try
    PREP_AddToBasket.Execute([cid, iid, cost]);
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
    PREP_GetBasket.Lock;
    try
      amount := 0;
      if PREP_GetBasket.Open([cid]) then
      repeat
        inc(amount);

        jsonElement := TJSONObject.Create([cIID,  StrToInt(PREP_GetBasket.Columns[0]),
                                           cCOST, StrToInt(PREP_GetBasket.Columns[1])]);
        jsonElement.CompressedJSON:=true;
        jsonResponse.Add(jsonElement);
      until (not PREP_GetBasket.Step) or (amount > 100);
      PREP_GetBasket.Close;
    finally
      PREP_GetBasket.UnLock;
    end;
    jsonResponse.CompressedJSON := true;
    Result := jsonResponse.AsJSON;
  finally
    jsonResponse.Free;
  end;
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
var cid, iid, cost : integer;
    requestContent : TJSONObject;
    ele : TJSONData;
begin
  cid := -1;
  iid := -1;
  cost := -1;
  try
    requestContent:= TJSONObject(GetJSON(Request.Content));
    if assigned(requestContent) then
    begin
      try
        ele := requestContent.Find(cIID);
        if assigned(ele) then iid := ele.AsInteger;
        ele := requestContent.Find(cCOST);
        if assigned(ele) then cost := ele.AsInteger;
        ele := requestContent.Find(cCID);
        if assigned(ele) then cid := ele.AsInteger;
      finally;
        requestContent.Free;
      end;
    end;
  except
    //do nothing
  end;

  if Length(Request.QueryFields.Values[cIID]) > 0 then
     TryStrToInt(Request.QueryFields.Values[cIID], iid);
  if Length(Request.QueryFields.Values[cCOST]) > 0 then
     TryStrToInt(Request.QueryFields.Values[cCOST], cost);
  if Length(Request.QueryFields.Values[cCID]) > 0 then
     TryStrToInt(Request.QueryFields.Values[cCID], cid);

  if (cid > 0) and (iid > 0) and (cost > 0) then
    Response.Content := AddToBasket(cid, iid, cost) else
    Response.Content := BAD_JSON;
  inherited Execute;
end;

{ TWCAddItem }

procedure TWCAddItem.Execute;
var iid, cost : integer;
    s1, s2, s3 : string;
    requestContent : TJSONObject;
    ele : TJSONData;
begin
  iid := -1;
  cost := -1;
  s1 := '';
  s2 := '';
  s3 := '';
  try
    requestContent:= TJSONObject(GetJSON(Request.Content));
    if assigned(requestContent) then
    begin
      try
        ele := requestContent.Find(cIID);
        if assigned(ele) then iid := ele.AsInteger;
        ele := requestContent.Find(cCOST);
        if assigned(ele) then cost := ele.AsInteger;
        ele := requestContent.Find(cSHRT_NAME);
        if assigned(ele) then s1 := ele.AsString;
        ele := requestContent.Find(cFULL_NAME);
        if assigned(ele) then s2 := ele.AsString;
        ele := requestContent.Find(cDESCR);
        if assigned(ele) then s3 := ele.AsString;
      finally;
        requestContent.Free;
      end;
    end;
  except
    //do nothing
  end;

  if Length(Request.QueryFields.Values[cIID]) > 0 then
     TryStrToInt(Request.QueryFields.Values[cIID], iid);
  if Length(Request.QueryFields.Values[cCOST]) > 0 then
     TryStrToInt(Request.QueryFields.Values[cCOST], cost);
  if Length(Request.QueryFields.Values[cSHRT_NAME]) > 0 then
     s1 := Request.QueryFields.Values[cSHRT_NAME];
  if Length(Request.QueryFields.Values[cFULL_NAME]) > 0 then
     s2 := Request.QueryFields.Values[cFULL_NAME];
  if Length(Request.QueryFields.Values[cDESCR]) > 0 then
     s3 := Request.QueryFields.Values[cDESCR];

  Response.Content := AddItem(iid, s1, s2, s3, cost);

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

