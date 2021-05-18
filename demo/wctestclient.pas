{
  This file is a part of example.
  look more in WCHTTPServerDemo.lpr
}

unit WCTestClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  wcapplication;

type
  TWCTestClientState = (csDisconnected, csConnected);

  { TWCTestWebClient }

  TWCTestWebClient = class(TWebClient)
  private
    FLifeTime : QWord;
    FStartAt  : QWord;
    FLastBackupFrame : integer;
    FClientState : TWCTestClientState;
    FLaunched, FLaunchChanged, FConnectionChanged : Boolean;
  public
    constructor Create(AOwner : TWebClients; const aCUID : String); override;
    procedure DoIdle; override;
    procedure Initialize; override;
    procedure ConnectToServer(const UN, PWD, aContent : String);
    procedure Disconnect;
    function  GetClientInt(pos : Integer) : QWord;
    function  Launch : Boolean;
    function  Stop : Boolean;
    function  LaunchChanged : Boolean;
    function  ConnectionChanged : Boolean;
    function  Launched : Boolean;
    property  LifeTime : QWord read FLifeTime;
    property  State : TWCTestClientState read FClientState;
    property  LastBackupFrame : Integer read FLastBackupFrame write
                                             FLastBackupFrame;
  end;

const
VAL_CONNECTION_STATE = 1;
VAL_LIFETIME_VALUE = 2;
VAL_LAUNCH_STATE = 3;
CCS_CONNECTED = 1;
CLS_RUN = 1;

implementation

{ TWCTestWebClient }

constructor TWCTestWebClient.Create(AOwner: TWebClients; const aCUID: String);
begin
  inherited Create(AOwner, aCUID);
  FClientState:=csDisconnected;
  FStartAt := GetTickCount64;
  FLifeTime:= 0;
  FLastBackupFrame:=0;
end;

procedure TWCTestWebClient.DoIdle;
begin
  inherited DoIdle;
end;

procedure TWCTestWebClient.Initialize;
begin
  //do nothing
end;

procedure TWCTestWebClient.ConnectToServer(const UN, PWD, aContent : String);
begin
  Lock;
  try
    if FClientState = csDisconnected then
    begin
      FClientState:=csConnected;
      FConnectionChanged:=true;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCTestWebClient.Disconnect;
begin
  Lock;
  try
    if FClientState = csConnected then
    begin
      FClientState:=csDisconnected;
      FConnectionChanged:=true;
    end;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.GetClientInt(pos: Integer): QWord;
begin
  Lock;
  try
    case pos of
    VAL_CONNECTION_STATE : begin
         Result := 0;
         if FClientState = csConnected then Result := CCS_CONNECTED;
      end;
    VAL_LAUNCH_STATE : begin
         Result := 0;
         if FLaunched then Result := CLS_RUN;
      end;
    VAL_LIFETIME_VALUE : begin
         FLifeTime := GetTickCount64 - FStartAt;
         Result := LifeTime;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.Launch: Boolean;
begin
  Lock;
  try
    if not FLaunched then
    begin
      FLaunched:=True;
      FLaunchChanged:=true;
    end;
    Result := FLaunched;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.Stop: Boolean;
begin
  Lock;
  try
    if FLaunched then
    begin
      FLaunched:=false;
      FLaunchChanged:=true;
    end;
    Result := not FLaunched;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.LaunchChanged: Boolean;
begin
  Lock;
  try
    Result := FLaunchChanged;
    FLaunchChanged := false;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.ConnectionChanged: Boolean;
begin
  Lock;
  try
    Result := FConnectionChanged;
    FConnectionChanged := false;
  finally
    UnLock;
  end;
end;

function TWCTestWebClient.Launched: Boolean;
begin
  Lock;
  try
    Result := FLaunched;
  finally
    UnLock;
  end;
end;

end.
