{
 SqliteWebSession:
   Session Factory based on sqlite database routings

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit sqlitewebsession;

{$mode objfpc}{$H+}
{ $define cgidebug}
interface

uses
  Classes, SysUtils, fphttp, httpdefs, ExtSqlite3DS;

Type

  TSqliteSessionFactory = class;

  { TSqliteWebSession }

  TSqliteWebSession = Class(TCustomSession)
  Private
    FSessionStarted : Boolean;
    FTerminated :Boolean;
    FSID : String;
    FRequest : TRequest;
    FSqliteFactory : TSqliteSessionFactory;
  Protected
    procedure CreateRecord(Const AFN : String);
    Procedure RemoveRecord;
    Procedure CheckSession;
    Function GetSessionID : String; override;
    Function GetSessionVariable(VarName : String) : String; override;
    procedure SetSessionVariable(VarName : String; const AValue: String); override;
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Terminate; override;
    Procedure UpdateResponse(AResponse : TResponse); override;
    Procedure InitSession(ARequest : TRequest; OnNewSession, OnExpired: TNotifyEvent); override;
    Procedure InitResponse(AResponse : TResponse); override;
    Procedure RemoveVariable(VariableName : String); override;
    property SID : String read FSID;
    property Request : TRequest read FRequest;
  end;
  TSqliteWebSessionClass = Class of TSqliteWebSession;

  TGetSessionID = function (aSession : TSqliteWebSession) : String of object;

  { TSqliteSessionFactory }

  TSqliteSessionFactory = Class(TSessionFactory)
  private
    PREP_AddNewClient,
      PREP_GetClientExpired,
      PREP_GetClientExists,
      PREP_DoCleanUp,
      PREP_RemoveClient,
      PREP_ClearClientsVariables,
      PREP_GetClientsVariable,
      PREP_SetClientsVariable,
      PREP_RemoveVariable,
      PREP_RefreshClient: TSqlite3Prepared;
    FClientsDB: TExtSqlite3Dataset;
    FOnGenSessionID : TGetSessionID;
    procedure SetClientsDB(const AValue: TExtSqlite3Dataset);
  protected
    Procedure DeleteSessionRecord(const ARecordName : String);virtual;
    Function SessionExpired(const ARecordName : String) : boolean;
    Function SessionExists(const ARecordName : String) : boolean;
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
    // Delete expired records.
    procedure DoCleanupSessions; override;
    Procedure DoDoneSession(Var ASession : TCustomSession); override;
  Public
    procedure InitializeDB(aDB : TExtSqlite3Dataset);
    procedure RefreshClient(const ARecordName : String);
    function  IsActiveSession(const ARecordName : String) : Boolean;
    Property ClientsDB : TExtSqlite3Dataset Read FClientsDB Write SetClientsDB;
    property OnGenSessionID : TGetSessionID read FOnGenSessionID write FOnGenSessionID;
  end;

Var
  SqliteWebSessionClass : TSqliteWebSessionClass = Nil;

implementation

{$ifdef cgidebug}
uses dbugintf;
{$endif}

Const
  // Sections in sqlite file
  SSession   = 'session';
  SData      = 'data';

  KeyStart   = 'start';         // Start time of session
  KeyLast    = 'last';          // Last seen time of session
  KeyTimeOut = 'timeout';       // Timeout in seconds;

resourcestring
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession         = 'No web session active: Session was not started';

{ TSqliteSessionFactory }

procedure TSqliteSessionFactory.InitializeDB(aDB : TExtSqlite3Dataset);
begin
  if not assigned(aDB) then Exit;

  ClientsDB := aDB;

  aDB.ExecSQL(
  'create table if not exists clients'+
    '(id text primary key, '+
     'start timestamp default current_timestamp,'+
     'last timestamp default current_timestamp,'+
     'timeout integer);');

  aDB.ExecSQL(
  'create table if not exists variables'+
    '(id integer primary key autoincrement, '+
     'client_id text, '+
     'var text,'+
     'data text);');

  PREP_AddNewClient := aDB.AddNewPrep('INSERT INTO clients (id, timeout) values(?1, ?2);');
  PREP_GetClientExpired := aDB.AddNewPrep('select count(*) from clients where (id = ?1) and (julianday(current_timestamp)-julianday(last)) * 1440.0 > timeout;');
  PREP_GetClientExists := aDB.AddNewPrep('select count(*) from clients where (id = ?1);');
  PREP_DoCleanUp := aDB.AddNewPrep('delete from clients where (julianday(current_timestamp)-julianday(last)) * 1440.0 > timeout;');
  PREP_RemoveClient := aDB.AddNewPrep('delete from clients where id = ?1;');
  PREP_ClearClientsVariables := aDB.AddNewPrep('delete from variables where client_id = ?1;');
  PREP_GetClientsVariable := aDB.AddNewPrep('select data from variables where client_id = ?1 and var = ?2;');
  PREP_SetClientsVariable := aDB.AddNewPrep('WITH new (clid, varid, data) AS ( VALUES(?1, ?2, ?3) ) '+
                            'INSERT OR REPLACE INTO variables (id, client_id, var, data) '+
                            'SELECT old.id, old.client_id, old.var, new.data '+
                            'FROM new LEFT JOIN variables AS old ON '+
                            'new.clid = old.client_id AND '+
                            'new.varid = old.var;');
  PREP_RemoveVariable := aDB.AddNewPrep('delete from variables where client_id = ?1 and var = ?2;');
  PREP_RefreshClient := aDB.AddNewPrep('update clients set last = current_timestamp where clients.id = ?1;');

  PREP_DoCleanUp.Execute([]);
end;

procedure TSqliteSessionFactory.RefreshClient(const ARecordName: String);
begin
  PREP_RefreshClient.Execute([ARecordName]);
end;

function TSqliteSessionFactory.IsActiveSession(const ARecordName: String
  ): Boolean;
begin
  Result := SessionExists(ARecordName);
end;

procedure TSqliteSessionFactory.SetClientsDB(const AValue: TExtSqlite3Dataset);
begin
  if FClientsDB=AValue then exit;
  FClientsDB:=AValue;
end;

procedure TSqliteSessionFactory.DeleteSessionRecord(const ARecordName : String);
begin
  PREP_RemoveClient.Execute([ARecordName]);
  PREP_ClearClientsVariables.Execute([ARecordName]);
end;

function TSqliteSessionFactory.SessionExpired(const ARecordName : String): boolean;
Var
  S : String;
begin
  S := PREP_GetClientExpired.QuickQuery([ARecordName], nil, false);
  Result:=not SameStr(S, '0');
end;

function TSqliteSessionFactory.SessionExists(const ARecordName: String
  ): boolean;
Var
  S : String;
begin
  S := PREP_GetClientExists.QuickQuery([ARecordName], nil, false);
  Result:=not SameStr(S, '0');
end;

function TSqliteSessionFactory.DoCreateSession(ARequest: TRequest): TCustomSession;
Var
  S : TSqliteWebSession;
begin
  if SqliteWebSessionClass=Nil then
    S:=TSqliteWebSession.Create(Nil)
  else
    S:=SqliteWebSessionClass.Create(Nil);
  S.SessionCookie:=SessionCookie;
  S.SessionCookiePath:=SessionCookiePath;
  S.TimeOutMinutes:= DefaultTimeOutMinutes;
  Result:=S;
end;

procedure TSqliteSessionFactory.DoCleanupSessions;
begin
  PREP_DoCleanUp.Execute([]);
end;

procedure TSqliteSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;

{ TSqliteWebSession }

function TSqliteWebSession.GetSessionID: String;
begin
  If (FSID='') then
  repeat
    if Assigned(FSqliteFactory.OnGenSessionID) then
       FSID := FSqliteFactory.OnGenSessionID(Self) else
       FSID := Inherited GetSessionID;
    Sleep(1);
  until not FSqliteFactory.SessionExists(FSID);
  Result:=SID;
end;

procedure TSqliteWebSession.CreateRecord(const AFN: String);
begin
  FSqliteFactory := TSqliteSessionFactory(SessionFactory);
  if assigned(FSqliteFactory) then
     FSqliteFactory.PREP_AddNewClient.Execute([AFN, TimeOutMinutes]);
end;

procedure TSqliteWebSession.RemoveRecord;
begin
  if assigned(FSqliteFactory) then
     FSqliteFactory.DeleteSessionRecord(SID);
end;

procedure TSqliteWebSession.CheckSession;
begin
  if FTerminated then
     Raise EWebSessionError.Create(SErrSessionTerminated)
end;

function TSqliteWebSession.GetSessionVariable(VarName: String): String;
begin
  CheckSession;
  if assigned(FSqliteFactory) then
    Result := FSqliteFactory.PREP_GetClientsVariable.QuickQuery([SID, VarName], nil, false) else
      Result := '';
end;

procedure TSqliteWebSession.SetSessionVariable(VarName: String;
  const AValue: String);
begin
  CheckSession;
  if assigned(FSqliteFactory) then
    FSqliteFactory.PREP_SetClientsVariable.Execute([SID, VarName, AValue]);
end;

constructor TSqliteWebSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSqliteFactory := nil;
end;

destructor TSqliteWebSession.Destroy;
begin
  inherited Destroy;
end;

procedure TSqliteWebSession.Terminate;
begin
  FTerminated:=True;
  RemoveFromSessionState(ssActive);
  RemoveFromSessionState(ssNew);
  RemoveFromSessionState(ssExpired);
end;

procedure TSqliteWebSession.UpdateResponse(AResponse: TResponse);
begin
  //
end;

procedure TSqliteWebSession.InitSession(ARequest: TRequest; OnNewSession,OnExpired: TNotifyEvent);

Var
  S : String;
begin
  FRequest := ARequest;
  FSqliteFactory:=SessionFactory as TSqliteSessionFactory;
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.InitSession');{$endif}
  // First initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  FSID := '';
  FSessionStarted := False;
  FTerminated := False;
  If (SessionCookie='') then SessionCookie:='cid';
  S:=ARequest.CookieFields.Values[SessionCookie];
  // have session cookie ?
  if not assigned(FSqliteFactory) then Exit;
  If (Length(S) > 0) then
  begin
{$ifdef cgidebug}SendDebug('Existing session. Reading ini file:'+FN);{$endif}
    if FSqliteFactory.SessionExpired(S) then
    begin
      AddToSessionState(ssExpired);
      // Expire session.
      If Assigned(OnExpired) then
        OnExpired(Self);
      RemoveRecord;
    end;
    FSID:=S;
    if (not FSqliteFactory.SessionExists(S)) then
       CreateRecord(S);
  end else
  begin
    AddToSessionState(ssNew);
    GetSessionID;
    S:=SessionID;
{$ifdef cgidebug}SendDebug('Expired or new session. Creating new Ini file : '+S);{$endif}
    if (not FSqliteFactory.SessionExists(S)) then
       CreateRecord(S);
    FSessionStarted:=True;
    If Assigned(OnNewSession) then
      OnNewSession(Self);
  end;
  if assigned(FSqliteFactory) then
     FSqliteFactory.RefreshClient(S);
  AddToSessionState(ssActive);
{$ifdef cgidebug}SendMethodExit('TIniWebSession.InitSession');{$endif}
end;

procedure TSqliteWebSession.InitResponse(AResponse: TResponse);

Var
  C : TCookie;

begin
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.InitResponse: '+SID);{$endif}
  C:=AResponse.Cookies.FindCookie(SessionCookie);
  If (C=Nil) then
    begin
    C:=AResponse.Cookies.Add;
    C.Name:=SessionCookie;
    C.HttpOnly:= true;
    end;
  If FTerminated then
    begin
{$ifdef cgidebug}SendDebug('Session terminated');{$endif}
    C.Value:='';
    end
  else
    begin
{$ifdef cgidebug}SendDebug('Existing session or Session started');{$endif}
    C.Value:=SID;
    C.Path:=SessionCookiePath;
    end;
{$ifdef cgidebug}SendMethodExit('TIniWebSession.InitResponse');{$endif}
  AddToSessionState(ssResponseInitialized);
end;

procedure TSqliteWebSession.RemoveVariable(VariableName: String);
begin
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.RemoveVariable');{$endif}
  CheckSession;
  FSqliteFactory.PREP_RemoveVariable.Execute([SessionID, VariableName])
{$ifdef cgidebug}SendMethodExit('TIniWebSession.RemoveVariable');{$endif}
end;


initialization
  SessionFactoryClass:=TSqliteSessionFactory;
end.

