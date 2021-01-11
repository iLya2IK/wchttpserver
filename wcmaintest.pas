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
  httpdefs, fpHTTP, httpprotocol,
  WCTestClient;

type

  { TWCPreThread }

  TWCPreThread = class(TWCPreAnalizeClientJob)
  public
    function GenerateClientJob: TWCMainClientJob; override;
  end;

  { TESWCSynchroThread }

  TWCSynchroThread = class(TThread)
  private
    CID : String;
    lastEventID: Cardinal;
    FConn : TWCConnection;
    function GetResponse: TWCResponse;
  public
    constructor Create(aConnection : TWCConnection;
      aLastEventID: Cardinal);
    destructor Destroy; override;
    procedure Execute; override;
    property Response : TWCResponse read GetResponse;
  end;

procedure StartSynchroTimer(aConnection : TWCConnection; lastEventID: Cardinal);
procedure InitializeJobsTree;
procedure DisposeJobsTree;

implementation

uses WCServerTestJobs, AvgLvlTree;

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

{ TWCPreThread }

function TWCPreThread.GenerateClientJob: TWCMainClientJob;
var ResultClass : TWCMainClientJobClass;
begin
  Result := nil;
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

{ TWCSynchroThread }

function TWCSynchroThread.GetResponse: TWCResponse;
begin
  Result := FConn.Response;
end;

constructor TWCSynchroThread.Create(aConnection: TWCConnection;
  aLastEventID: Cardinal);
begin
  FConn := aConnection;
  CID := aConnection.Client.CUID;
  lastEventID:= aLastEventID;
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TWCSynchroThread.Destroy;
begin
  FreeAndNil(FConn);
  inherited Destroy;
end;

procedure TWCSynchroThread.Execute;
var aClient : TWCTestWebClient;
    S : String;
    curEventID : Cardinal;
function SendResponse : Boolean;
begin
  try
    if not Response.HeadersSent then
    begin
       Response.SendHeaders;
       Sleep(1000); // timeout 1s
    end;
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
begin
  Response.Code:=200;
  Response.ContentType:='text/event-stream; charset=utf-8';
  Response.CacheControl:='no-cache';
  Response.KeepStreamAlive:=true;
  aClient := nil;
  try
    while not Terminated do
    begin
      WebContainer.Clients.Lock;
      try
        aClient := TWCTestWebClient(WebContainer.GetClient(CID));
        if not assigned(aClient) then begin
          Application.SendError(Response, 405);
          Exit;
        end else begin
          aClient.IncReference;
          try
            if (aClient.State = csDisconnected) then
            begin
              S := 'event: disconnect' + #10;
              SendResponse;
              Exit;
            end;
            aClient.HasSynConnection := True;

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
          finally
            aClient.DecReference
          end;
        end;
      finally
        WebContainer.Clients.UnLock;
      end;
      if Length(S) > 0 then begin
        S := 'data: ' + S + #10 + 'id: ' + inttostr(curEventID) + #10#10;
        if not SendResponse then Exit;
      end;
      Sleep(500);
    end;
  finally
    Response.CloseStream;
    WebContainer.Clients.Lock;
    try
      aClient := TWCTestWebClient(WebContainer.GetClient(CID));
      if assigned(aClient) then
        aClient.HasSynConnection:=false;
    finally
      WebContainer.Clients.UnLock;
    end;
  end;
end;

procedure StartSynchroTimer(aConnection : TWCConnection; lastEventID: Cardinal);
begin
  TWCSynchroThread.Create(aConnection, lastEventID);
end;

end.
