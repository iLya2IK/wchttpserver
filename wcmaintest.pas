{
  This file is a part of example.
  look more in WCHTTPServer.lpr
}

unit WCMainTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  wcapplication,
  wchttp2server,
  SortedThreadPool,
  httpdefs, fpHTTP, httpprotocol,
  WCTestClient;

type

  { TWCPreThread }

  TWCPreThread = class(TWCPreAnalizeClientJob)
  public
    function GenerateClientJob: TWCMainClientJob; override;
  end;

  { TWCSynchroJob }

  TWCSynchroJob = class(TWCMainClientJob)
  private
    lastEventID: Cardinal;
    curEventID : Cardinal;
    FStage : Byte;
    {$IFDEF WC_WEB_SOCKETS}
    FRID : cardinal;
    {$ENDIF}
  public
    constructor Create(aConnection : TWCConnection;
      aLastEventID: Cardinal); overload;
    procedure Execute; override;
    procedure UpdateScore; override;
  end;

  {$IFDEF WC_WEB_SOCKETS}

  { TJSONRPCWrapperJob }

  TJSONRPCWrapperJob = class(TWCMainClientWrapperJob)
  private
    FRID : Cardinal;
  public
    constructor Create(aConnection : TWCConnection; aRID: Cardinal;
      aWrappedJobClass: TWCMainClientJobClass); overload;
    procedure DoWrappedExecute; override;
  end;
  {$ENDIF}

procedure InitializeJobsTree;
procedure DisposeJobsTree;

implementation

uses WCServerTestJobs,
  {$IFDEF WC_WEB_SOCKETS}
  jsonscanner, jsonparser, fpjson, Variants,
  {$ENDIF}
  AvgLvlTree;

var WCJobsTree : TStringToPointerTree;

procedure InitializeJobsTree;
begin
  WCJobsTree := TStringToPointerTree.Create(true);
  WCJobsTree.Values['/wcConnectToServer.json'] := TWCConnectToServer;
  WCJobsTree.Values['/wcDisconnect.json'] := TWCDisconnect;
  WCJobsTree.Values['/wcLaunch.json'] := TWCLaunch;
  WCJobsTree.Values['/wcStop.json'] := TWCStop;
  WCJobsTree.Values['/wcGetClientInt.json'] := TWCGetInteger;
end;

procedure DisposeJobsTree;
begin
  FreeAndNil(WCJobsTree);
end;


{$IFDEF WC_WEB_SOCKETS}
{ TJSONRPCWrapperJob }

constructor TJSONRPCWrapperJob.Create(aConnection : TWCConnection;
  aRID : Cardinal; aWrappedJobClass : TWCMainClientJobClass);
begin
  inherited Create(aConnection, aWrappedJobClass);
  FRID := aRID;
end;

procedure TJSONRPCWrapperJob.DoWrappedExecute;
var j : TJSONObject;
begin
  J := TJSONObject.Create(['rid', FRID, 'data', WrappedJob.Response.Content]);
  try
    Response.Content := J.AsJSON;
  finally
    J.Free;
  end;
end;
{$ENDIF}

{ TWCPreThread }

function TWCPreThread.GenerateClientJob: TWCMainClientJob;
var ResultClass : TWCMainClientJobClass;
    {$IFDEF WC_WEB_SOCKETS}
    jsonObj : TJSONData;
    jsonData : TJSONVariant;
    jsonDataObj : TJSONObject;
    rid : Cardinal;
    uri : string;
    {$ENDIF}
begin
  Result := nil;
  {$IFDEF WC_WEB_SOCKETS}
  if Connection.HTTPVersion = wcWebSocket then
  begin
    if Connection.Request.ContentLength > 0 then
    begin
      try
        jsonObj := GetJSON(Connection.Request.Content);
      except
        jsonObj := nil;
      end;
      if Assigned(jsonObj) then
      begin
        try
          if jsonObj is TJSONObject then
          begin
           rid := TJSONObject(jsonObj).Get('rid', 0);
           uri := TJSONObject(jsonObj).Get('uri', '');
           jsonDataObj := TJSONObject(jsonObj).Get('data', TJSONObject(nil));
           jsonData := null;
           if not assigned(jsonDataObj) then
             jsonData := TJSONObject(jsonObj).Get('data');
           Request.SetCustomHeader('JSONRPC_RID', inttostr(rid));
           if (length(uri) > 0) and (uri[1] = '.') then
             uri := Copy(uri, 2, Length(uri));
           ResultClass := TWCMainClientJobClass(WCJobsTree.Values[uri]);
           if assigned(ResultClass) then begin
              if (not VarIsNull(jsonData)) or assigned(jsonDataObj) then
              begin
                if assigned(jsonDataObj) then
                  Request.Content := jsonDataObj.AsJSON else
                  Request.Content := VarToStr(jsonData);
              end else
                Request.Content := '';
              Result := TJSONRPCWrapperJob.Create(Connection, rid, ResultClass);
           end else
           if CompareStr(uri, '/wcDoSynchronizeWebSocket.json')=0 then
           begin
             if Assigned(jsonDataObj) then
             begin
               Request.SetCustomHeader('Last-Event-ID',
                                       inttostr(jsonDataObj.Get('lstId', 0)));
             end;
             Result := TWCTryToStartSynchroThread.Create(Connection);
           end;
          end;
        finally
          jsonObj.Free;
        end;
      end;
    end;
  end else
  {$ENDIF}
  begin
  if CompareText(Request.Method,'POST')=0 then
  begin
    ResultClass := TWCMainClientJobClass(WCJobsTree.Values[Request.PathInfo]);
    if assigned(ResultClass) then
       Result := ResultClass.Create(Connection);
  end else
  begin
    if CompareStr(Request.PathInfo, '/wcDoSynchronizeEventSource.json')=0 then
    begin
      Result := TWCTryToStartSynchroThread.Create(Connection);
    end else
    begin
      Result := TWCSendServerFile.Create(Connection);
    end;
  end;
  end;
end;

{ TWCSynchroJob }

constructor TWCSynchroJob.Create(aConnection: TWCConnection;
  aLastEventID: Cardinal);
begin
  inherited Create(aConnection);
  lastEventID:= aLastEventID;
end;

procedure TWCSynchroJob.UpdateScore;
begin
  Connection.Client.UpdateScore;
  Score := Connection.Client.Score;
end;

procedure TWCSynchroJob.Execute;
var S : String;
function SendResponse : Boolean;
begin
  try
    if not Response.HeadersSent then
      Response.SendHeaders;
    {$IFDEF WC_WEB_SOCKETS}
    if Connection.HTTPVersion = wcWebSocket then
    begin
      s := '{"rid":' + inttostr(FRID) + ',"data":"' + StringToJSONString(s) + '"}';
    end;
    {$ENDIF}
    Response.SendUtf8String(S);
    Result := True;
  except
    //maybe connection dropped or other write error
    on e : Exception do
    begin
      Application.DoError(e.Message);
      Result := false;
    end;
  end;
end;
var aClient : TWCTestWebClient;
begin
  ResponseReadyToSend := false;
  if FStage = 0 then
  begin
    {$IFDEF WC_WEB_SOCKETS}
    if Connection.HTTPVersion = wcWebSocket then
    begin
      s := Request.GetCustomHeader('JSONRPC_RID');
      if length(s) > 0 then FRID := StrToInt(s) else FRID := 0;
    end else
    {$ENDIF}
    begin
      Response.Code:=200;
      Response.ContentType:='text/event-stream; charset=utf-8';
      Response.CacheControl:='no-cache';
    end;
    Response.KeepStreamAlive:=true;
    FStage := 1;
  end;
  try
    try
      if FStage = 1 then
      begin
        FStage := 2; // set here to react on exceptions and exits
        if not Connection.HTTPRefCon.ConnectionAvaible then Exit;
        aClient := TWCTestWebClient(Client);
        if not assigned(aClient) then begin
          Application.SendError(Response, 405);
          Exit;
        end else begin
          if (aClient.State = csDisconnected) then
          begin
            {$IFDEF WC_WEB_SOCKETS}
            if Connection.HTTPVersion = wcWebSocket then
            begin
              S := '{"event":"disconnect"}';
            end else
            {$ENDIF}
            begin
              S := 'event: disconnect' + #10;
            end;
            SendResponse;
            Exit;
          end;
          Client.HasSynConnection := True;

          curEventID := aClient.LastBackupFrame;
          if (lastEventID > curEventID) or
              ((curEventID - lastEventID) > 1) or
               (lastEventID = 0) then
          begin
             // restore state here
             aClient.LastBackupFrame := lastEventID;
          end else
          if (curEventID > lastEventID) then
          begin
             // rollback here
             aClient.LastBackupFrame := lastEventID;
          end;

          S := '';

          if aClient.LaunchChanged then
            S := S + inttostr(VAL_LAUNCH_STATE);
          if aClient.ConnectionChanged then
          begin
            if Length(S) > 0 then S := S + ',';
            S := S + inttostr(VAL_CONNECTION_STATE);
          end;
          if aClient.Launched then
          begin
            if Length(S) > 0 then S := S + ',';
            S := S + inttostr(VAL_LIFETIME_VALUE);
          end;

          curEventID := aClient.LastBackupFrame;
          if (curEventID = 0) then begin
            if length(S) = 0 then
              S := inttostr(VAL_CONNECTION_STATE);
            inc(curEventID); //some reconnection issue
          end;
          if length(S) > 0 then
            S := '{"events":[' + S + ']}';
          lastEventID := curEventID;
        end;
        if Length(S) > 0 then begin
          {$IFDEF WC_WEB_SOCKETS}
          if Connection.HTTPVersion = wcWebSocket then
          begin
            S := '{"data":' + S + ',"id":' + inttostr(curEventID) + '}';
          end else
          {$ENDIF}
          begin
          S := 'data: ' + S + #10 + 'id: ' + inttostr(curEventID) + #10#10;
          end;
          if not SendResponse then Exit;
        end;
        FStage := 1;
        RestartJob(500, GetTickCount64);
      end;
    except
      FStage := 2;
    end;
  finally
    if FStage = 2 then begin
      Response.CloseStream;
      aClient := TWCTestWebClient(Client);
      if assigned(aClient) then
        aClient.HasSynConnection:=false;
    end;
  end;
end;

end.
