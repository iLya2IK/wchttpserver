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
  public
    constructor Create(aConnection : TWCConnection;
      aLastEventID: Cardinal); overload;
    procedure Execute; override;
    procedure UpdateScore; override;
  end;

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
    Response.Code:=200;
    Response.ContentType:='text/event-stream; charset=utf-8';
    Response.CacheControl:='no-cache';
    Response.KeepStreamAlive:=true;
    FStage := 1;
  end;
  try
    try
      if FStage = 1 then
      begin
        FStage := 2; // set here to react on exceptions and exits
        aClient := TWCTestWebClient(Client);
        if not assigned(aClient) then begin
          Application.SendError(Response, 405);
          Exit;
        end else begin
          if (aClient.State = csDisconnected) then
          begin
            S := 'event: disconnect' + #10;
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
          S := 'data: ' + S + #10 + 'id: ' + inttostr(curEventID) + #10#10;
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
