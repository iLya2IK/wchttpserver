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
  { TWCMyItem }

  TWCMyItem = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

implementation

uses wcutils;

const BAD_JSON = '{"result":"BAD"}';
var ItemsDB : TExtSqlite3Dataset;

procedure InitializeItemsDb;
begin

end;

procedure DoneItemsDb;
begin

end;

function GetItem(const id, name, itName, itDescr : String) : String;
var
  jsonObj : TJSONObject;
begin
  jsonObj := TJSONObject.Create(['id', id,
                                 'name', name,
                                 'itemName', itName,
                                 'ItemDescriprion', itDescr]);
  try
    jsonObj.CompressedJSON := true;
    Result := jsonObj.AsJSON;
  finally
    jsonObj.Free;
  end;
end;

procedure TWCMyItem.Execute;
var
  s1, s2 : string;
begin
  s1:=Request.QueryFields.Values['id'];
  s2:=Request.QueryFields.Values['name'];
  if DecodeJsonParams(Request.Content, ['ItemName', 'ItemDescriprion'], Params,
                                       ['',''] ) then
  begin
    Response.Content := GetItem(s1, s2, Param[0], Param[1]);
  end else
    Response.Content := BAD_JSON;
  inherited Execute;
end;

end.

