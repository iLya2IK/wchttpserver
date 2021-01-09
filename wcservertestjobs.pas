unit WCServerTestJobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants,
  httpdefs, httpprotocol,
  jsonscanner, jsonparser, fpjson,
  WCMainTest, WCTestClient, wcapplication;

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

  { TWCSendServerFile }

  TWCSendServerFile = class(TWCMainClientJob)
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
    StartSynchroTimer(Connection, SynLastIndx);
    ReleaseConnection;
  end;
  ResponseReadyToSend := false; // prevent to send response
  inherited Execute;
end;

{ TWCSendServerFile }

procedure TWCSendServerFile.Execute;
var FN, FE, aURI : String;
  aCachedFile : TWebCachedItem;
begin
  ResponseReadyToSend := false; // prevent to send response

  aURI := Request.PathInfo;
  if (aURI = '/') or (Length(aURI) = 0) then
  begin
    aURI := Application.MainURI;
  end;
  FN := StringReplace(aURI, cNonSysDelimiter, cSysDelimiter, [rfReplaceAll]);
  if (Pos(FN, '..') = 0) and (Length(FN) > 0) then
  begin
    if FN[1] = cSysDelimiter then
      Delete(FN, 1, 1);

    FN := UTF8Encode(vPath) + FN;

    FE := ExtractFileExt(FN);
    if SameText(FE, '.svgz') then begin
      if Client.AcceptGzip then
        Response.SetHeader(hhContentEncoding, cSgzip) else
        FN := ChangeFileExt(FN, '.svg');
    end;

    aCachedFile := WebContainer.GetWebCachedItem(FN);
    if assigned(aCachedFile) and
       (aCachedFile.Size > 0) then
    begin
      aCachedFile.Lock;
      try
        Response.ContentType := aCachedFile.MimeType;
        Response.CacheControl:= aCachedFile.CacheControl;

        {$IFDEF ALLOW_STREAM_GZIP}
        if Client.AcceptGzip and aCachedFile.GzipReady then
        begin
          Response.ContentLength:=aCachedFile.GzipSize;
          Response.ContentStream:=aCachedFile.GzipCache;
          Response.SetHeader(hhContentEncoding, cSgzip)
        end else
        {$ENDIF}
        if Client.AcceptDeflate and aCachedFile.DeflateReady then
        begin
          Response.ContentLength:=aCachedFile.DeflateSize;
          Response.ContentStream:=aCachedFile.DeflateCache;
          Response.SetHeader(hhContentEncoding, cSdeflate)
        end else
        begin
          Response.ContentLength:=aCachedFile.Size;
          Response.ContentStream:=aCachedFile.Cache;
        end;

        Response.SendContent;
      finally
        aCachedFile.UnLock;
      end;
      Response.ContentStream:=Nil;
    end else
    begin
      Application.SendError(Response, 404);
    end;
  end else
  begin
    Application.SendError(Response, 406);
  end;
  // inherited Execute;
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
                                               jsonObj.Get('VAL_USERPSW', ''));
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
  if ESWGetHeaderContent(Request, cSPos, P1, 0) then
  begin
    Response.Content:= IntToStr(TWCTestWebClient(Client).GetClientInt(P1));
  end else
    Response.Content := '0';
  inherited Execute;
end;

end.

