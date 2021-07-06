{
  This file is a part of example.
  look more in WCHTTPServerDemo.lpr
}

unit WCServerTestJobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants,
  httpdefs, httpprotocol,
  jsonscanner, jsonparser, fpjson,
  WCMainTest, WCTestClient, wcApplication;

type
  { TESConnectToServer }

  TWCConnectToServer = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCDisconnect }

  TWCDisconnect = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCGetInteger }

  TWCGetInteger = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCLaunch }

  TWCLaunch = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCStop }

  TWCStop = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  { TWCTryToStartSynchroThread }

  TWCTryToStartSynchroThread = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

const
  cSStatusBADJSON  = '{"status":"BAD"}';
  cSStatusOKJSON   = '{"status":"OK"}';
  cSStatusNEWJSON  = '{"status":"NEW"}';
  cSStatusSAMEJSON = '{"status":"SAME"}';
  cSPos = 'pos';

implementation

uses wcutils;

{ TWCStop }

procedure TWCStop.Execute;
begin
  if TWCTestWebClient(Client).Stop then
    Response.Content := cSStatusOKJSON else
    Response.Content := cSStatusBADJSON;
  inherited Execute;
end;

{ TWCLaunch }

procedure TWCLaunch.Execute;
begin
  if TWCTestWebClient(Client).Launch then
    Response.Content := cSStatusOKJSON else
    Response.Content := cSStatusBADJSON;
  inherited Execute;
end;

{ TWCTryToStartSynchroThread }

procedure TWCTryToStartSynchroThread.Execute;
var SynLastIndx : integer;
  FN : String;
begin
  if Client.HasSynConnection then
  begin
    // additional connection. refuse old one
    Application.SendError(Response, 405);
  end else begin
    FN := Request.GetCustomHeader('Last-Event-ID');
    if Length(FN) = 0 then SynLastIndx := 0 else
                           SynLastIndx := StrToInt(FN);
    Application.WCServer.AddSortedJob(TWCSynchroJob.Create(Connection, SynLastIndx));
    ReleaseConnection;
  end;
  ResponseReadyToSend := false; // prevent to send response
  inherited Execute;
end;

{ TWCDisconnect }

procedure TWCDisconnect.Execute;
begin
  try
    TWCTestWebClient(Client).Disconnect();
    Response.Content := cSStatusOKJSON;
  finally
    //
  end;
  inherited Execute;
end;

{ TWCConnectToServer }

procedure TWCConnectToServer.Execute;
var jsonObj : TJSONObject;
begin
  try
    jsonObj:= TJSONObject(GetJSON(Request.Content));
    if assigned(jsonObj) then
    begin
      TWCTestWebClient(Client).ConnectToServer(jsonObj.Get('VAL_USERNAME', ''),
                                               jsonObj.Get('VAL_USERPSW', ''),
                                               jsonObj.Get('content', ''));
      Response.Content := cSStatusOKJSON;
    end else
      Response.Content := cSStatusBADJSON;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
  inherited Execute;
end;

{ TWCGetInteger }

procedure TWCGetInteger.Execute;
begin
  if DecodeJsonParams(Request.Content, cSPos, ParPtr[1]^, 0) then
  begin
    Response.Content:= IntToStr(TWCTestWebClient(Client).GetClientInt(Param[1]));
  end else
    Response.Content := '0';
  inherited Execute;
end;

end.

