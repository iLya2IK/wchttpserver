{
 CustAbsHTTPApp:
   Abstract implementation of TCustomHttpApplication
   based on custhttpapp and replace it.

   Part of WCHTTP2Server project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit custabshttpapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, custweb, ssockets, abstracthttpserver;

type
  TCustomAbsHTTPApplication = Class;
  TAbsHTTPServerHandler = Class;

  { TEmbeddedAbsHttpServer }

  TEmbeddedAbsHttpServer = Class(TAbsCustomHttpServer)
  Private
    FWebHandler: TAbsHTTPServerHandler;
  protected
    Procedure InitRequest(ARequest : TAbsHTTPConnectionRequest); override;
    Procedure InitResponse(AResponse : TAbsHTTPConnectionResponse); override;
    Property WebHandler : TAbsHTTPServerHandler Read FWebHandler;
    Property Active;
    Property OnAcceptIdle;
    Property AcceptIdleTimeout;
  end;

  { TAbsHTTPServerHandler }

  TAbsHTTPServerHandler = class(TWebHandler)
  Private
    FOnRequestError: TRequestErrorHandler;
    FServer: TEmbeddedAbsHttpServer;
    function GetAllowConnect: TConnectQuery;
    function GetAddress: string;
    function GetHostName: String;
    function GetIdle: TNotifyEvent;
    function GetIDleTimeOut: Cardinal;
    function GetPort: Word;
    function GetQueueSize: Word;
    function GetThreaded: Boolean;
    function GetUseSSL: Boolean;
    procedure SetHostName(AValue: String);
    procedure SetIdle(AValue: TNotifyEvent);
    procedure SetIDleTimeOut(AValue: Cardinal);
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    function GetLookupHostNames : Boolean;
    Procedure SetLookupHostnames(Avalue : Boolean);
    procedure SetUseSSL(AValue: Boolean);
  protected
    procedure HTTPHandleRequest(Sender: TObject; var ARequest: TAbsHTTPConnectionRequest; var AResponse: TAbsHTTPConnectionResponse); virtual;
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    Procedure InitRequest(ARequest : TRequest); override;
    Procedure InitResponse(AResponse : TResponse); override;
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    Function CreateServer : TEmbeddedAbsHttpServer; virtual; abstract;
    Property HTTPServer : TEmbeddedAbsHttpServer Read FServer;
  Public
    Procedure Run; override;
    Procedure Terminate; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Address to listen on.
    Property Address : string Read GetAddress Write SetAddress;
    // Port to listen on.
    Property Port : Word Read GetPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read GetQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read GetAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read GetThreaded Write SetThreaded;
    // Handle On Request error. If not set, error is logged.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Should addresses be matched to hostnames ? (expensive)
    Property LookupHostNames : Boolean Read GetLookupHostNames Write SetLookupHostNames;
    // Event handler called when going Idle while waiting for a connection
    Property OnAcceptIdle : TNotifyEvent Read GetIdle Write SetIdle;
    // If >0, when no new connection appeared after timeout, OnAcceptIdle is called.
    Property AcceptIdleTimeout : Cardinal Read GetIDleTimeOut Write SetIDleTimeOut;
    // Use SSL or not ?
    Property UseSSL : Boolean Read GetUseSSL Write SetUseSSL;
    // HostName to use when using SSL
    Property HostName : String Read GetHostName Write SetHostName;
  end;

  { TCustomAbsHTTPApplication }

  TCustomAbsHTTPApplication = Class(TCustomWebApplication)
  private
    procedure FakeConnect;
    function GetHostName: String;
    function GetIdle: TNotifyEvent;
    function GetIDleTimeOut: Cardinal;
    function GetLookupHostNames : Boolean;
    function GetUseSSL: Boolean;
    procedure SetHostName(AValue: String);
    procedure SetIdle(AValue: TNotifyEvent);
    procedure SetIDleTimeOut(AValue: Cardinal);
    Procedure SetLookupHostnames(Avalue : Boolean);
    function GetAllowConnect: TConnectQuery;
    function GetAddress: String;
    function GetPort: Word;
    function GetQueueSize: Word;
    function GetThreaded: Boolean;
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    procedure SetUseSSL(AValue: Boolean);
  protected
    function InitializeWebHandler: TWebHandler; override;
    function InitializeAbstractWebHandler : TWebHandler; virtual; abstract;
    Function HTTPHandler : TAbsHTTPServerHandler;
  Public
    procedure Terminate; override;
    Property Address : string Read GetAddress Write SetAddress;
    Property Port : Word Read GetPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read GetQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read GetAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read GetThreaded Write SetThreaded;
    // Should addresses be matched to hostnames ? (expensive)
    Property LookupHostNames : Boolean Read GetLookupHostNames Write SetLookupHostNames;
    // Event handler called when going Idle while waiting for a connection
    Property OnAcceptIdle : TNotifyEvent Read GetIdle Write SetIdle;
    // If >0, when no new connection appeared after timeout, OnAcceptIdle is called.
    Property AcceptIdleTimeout : Cardinal Read GetIDleTimeOut Write SetIDleTimeOut;
    // Use SSL ?
    Property UseSSL : Boolean Read GetUseSSL Write SetUseSSL;
    // Hostname to use when using SSL
    Property HostName : String Read GetHostName Write SetHostName;
  end;

Implementation

{ TEmbeddedAbsHttpServer }

procedure TEmbeddedAbsHttpServer.InitRequest(ARequest: TAbsHTTPConnectionRequest);
begin
  WebHandler.InitRequest(ARequest);
end;

procedure TEmbeddedAbsHttpServer.InitResponse(AResponse: TAbsHTTPConnectionResponse
  );
begin
  WebHandler.InitResponse(AResponse);
end;

{$ifdef CGIDEBUG}
uses
  dbugintf;
{$endif}

{ TCustomAbsHTTPApplication }

function TCustomAbsHTTPApplication.GetIdle: TNotifyEvent;
begin
  Result:=HTTPHandler.OnAcceptIdle;
end;

function TCustomAbsHTTPApplication.GetIDleTimeOut: Cardinal;
begin
  Result:=HTTPHandler.AcceptIdleTimeout;
end;

function TCustomAbsHTTPApplication.GetLookupHostNames : Boolean;

begin
  Result:=HTTPHandler.LookupHostNames;
end;

function TCustomAbsHTTPApplication.GetUseSSL: Boolean;
begin
  Result:=HTTPHandler.UseSSL;
end;

procedure TCustomAbsHTTPApplication.SetHostName(AValue: String);
begin
  HTTPHandler.HostName:=aValue;
end;

procedure TCustomAbsHTTPApplication.SetIdle(AValue: TNotifyEvent);
begin
  HTTPHandler.OnAcceptIdle:=AValue;
end;

procedure TCustomAbsHTTPApplication.SetIDleTimeOut(AValue: Cardinal);
begin
  HTTPHandler.AcceptIdleTimeOut:=AValue;
end;

procedure TCustomAbsHTTPApplication.SetLookupHostnames(Avalue: Boolean);

begin
  HTTPHandler.LookupHostNames:=AValue;
end;

function TCustomAbsHTTPApplication.GetAllowConnect: TConnectQuery;
begin
  Result:=HTTPHandler.OnAllowConnect;
end;

function TCustomAbsHTTPApplication.GetAddress: String;
begin
  Result:=HTTPHandler.Address;
end;

function TCustomAbsHTTPApplication.GetPort: Word;
begin
  Result:=HTTPHandler.Port;
end;

function TCustomAbsHTTPApplication.GetQueueSize: Word;
begin
  Result:=HTTPHandler.QueueSize;
end;

function TCustomAbsHTTPApplication.GetThreaded: Boolean;
begin
  Result:=HTTPHandler.Threaded;
end;

procedure TCustomAbsHTTPApplication.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  HTTPHandler.OnAllowConnect:=AValue;
end;

procedure TCustomAbsHTTPApplication.SetAddress(const AValue: string);
begin
  HTTPHandler.Address:=Avalue;
end;

procedure TCustomAbsHTTPApplication.SetPort(const AValue: Word);
begin
  HTTPHandler.Port:=Avalue;
end;

procedure TCustomAbsHTTPApplication.SetQueueSize(const AValue: Word);
begin
  HTTPHandler.QueueSize:=Avalue;
end;

procedure TCustomAbsHTTPApplication.SetThreaded(const AValue: Boolean);
begin
  HTTPHandler.Threaded:=Avalue;
end;

procedure TCustomAbsHTTPApplication.SetUseSSL(AValue: Boolean);
begin
  HTTPHandler.UseSSL:=aValue;
end;

function TCustomAbsHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result:=InitializeAbstractWebHandler;
end;

function TCustomAbsHTTPApplication.HTTPHandler: TAbsHTTPServerHandler;
begin
  Result:=Webhandler as TAbsHTTPServerHandler;
end;

procedure TCustomAbsHTTPApplication.FakeConnect;
begin
  try
    TInetSocket.Create('localhost',Self.Port).Free;
  except
    // Ignore errors this may raise.
  end
end;

function TCustomAbsHTTPApplication.GetHostName: String;
begin
  Result:=HTTPHandler.HostName;
end;

procedure TCustomAbsHTTPApplication.Terminate;
begin
  inherited Terminate;
  // We need to break the accept loop. Do a fake connect.
  if Threaded And (AcceptIdleTimeout=0) then
    FakeConnect;
end;

{ TAbsHTTPServerHandler }

procedure TAbsHTTPServerHandler.HandleRequestError(Sender: TObject; E: Exception
  );
begin
  Try
    If Assigned(FOnRequestError) then
      FOnRequestError(Sender,E)
    else
      Log(etError,Format('Error (%s) handling request : %s',[E.ClassName,E.Message]));
  except
    // Do not let errors escape
  end;
end;

procedure TAbsHTTPServerHandler.HTTPHandleRequest(Sender: TObject;
  var ARequest: TAbsHTTPConnectionRequest;
  var AResponse: TAbsHTTPConnectionResponse);
begin
  // Exceptions are handled by (Do)HandleRequest. It also frees the response/request
  try
    DoHandleRequest(ARequest,AResponse);
  finally
    ARequest:=Nil;
    AResponse:=Nil;
  end;
  if Assigned(OnIdle) then
    OnIdle(Self);
end;

function TAbsHTTPServerHandler.GetLookupHostNames : Boolean;

begin
  Result:=FServer.LookupHostNames;
end;

procedure TAbsHTTPServerHandler.SetLookupHostnames(Avalue: Boolean);

begin
  FServer.LookupHostNames:=AValue;
end;

procedure TAbsHTTPServerHandler.SetUseSSL(AValue: Boolean);
begin
  FServer.UseSSL:=aValue;
end;

function TAbsHTTPServerHandler.GetAllowConnect: TConnectQuery;
begin
  Result:=FServer.OnAllowConnect;
end;

function TAbsHTTPServerHandler.GetAddress: string;
begin
  Result:=FServer.Address;
end;

function TAbsHTTPServerHandler.GetHostName: String;
begin
  Result:=FServer.CertificateData.HostName;
end;

function TAbsHTTPServerHandler.GetIdle: TNotifyEvent;
begin
  Result:=FServer.OnAcceptIdle;
end;

function TAbsHTTPServerHandler.GetIDleTimeOut: Cardinal;
begin
  Result:=FServer.AcceptIdleTimeout;
end;

function TAbsHTTPServerHandler.GetPort: Word;
begin
  Result:=FServer.Port;
end;

function TAbsHTTPServerHandler.GetQueueSize: Word;
begin
  Result:=FServer.QueueSize;
end;

function TAbsHTTPServerHandler.GetThreaded: Boolean;
begin
  Result:=FServer.Threaded;
end;

function TAbsHTTPServerHandler.GetUseSSL: Boolean;
begin
  Result:=FServer.UseSSL;
end;

procedure TAbsHTTPServerHandler.SetHostName(AValue: String);
begin
  FServer.CertificateData.HostName:=aValue;
end;

procedure TAbsHTTPServerHandler.SetIdle(AValue: TNotifyEvent);
begin
  FServer.OnAcceptIdle:=AValue;
end;

procedure TAbsHTTPServerHandler.SetIDleTimeOut(AValue: Cardinal);
begin
  FServer.AcceptIdleTimeOut:=AValue;
end;

procedure TAbsHTTPServerHandler.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  FServer.OnAllowConnect:=Avalue
end;

procedure TAbsHTTPServerHandler.SetAddress(const AValue: string);
begin
  FServer.Address:=AValue
end;

procedure TAbsHTTPServerHandler.SetPort(const AValue: Word);
begin
  FServer.Port:=Avalue
end;

procedure TAbsHTTPServerHandler.SetQueueSize(const AValue: Word);
begin
  FServer.QueueSize:=Avalue
end;

procedure TAbsHTTPServerHandler.SetThreaded(const AValue: Boolean);
begin
  FServer.Threaded:=AValue;
end;

procedure TAbsHTTPServerHandler.InitRequest(ARequest: TRequest);
begin
  inherited InitRequest(ARequest);
end;

procedure TAbsHTTPServerHandler.InitResponse(AResponse: TResponse);
begin
  inherited InitResponse(AResponse);
end;

function TAbsHTTPServerHandler.WaitForRequest(out ARequest: TRequest;
  out AResponse: TResponse): boolean;
begin
  Result:=False;
  ARequest:=Nil;
  AResponse:=Nil;
end;

procedure TAbsHTTPServerHandler.Run;
begin
  Fserver.Active:=True;
end;

procedure TAbsHTTPServerHandler.Terminate;
begin
  Inherited;
  if Assigned(FServer) then
    Fserver.Active:=False;
end;

constructor TAbsHTTPServerHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:=CreateServer;
  FServer.FWebHandler:=Self;
  FServer.OnRequest:=@HTTPHandleRequest;
  Fserver.OnRequestError:=@HandleRequestError;
end;

destructor TAbsHTTPServerHandler.Destroy;
begin
  if Assigned(FServer) then
  begin
    FServer.Active:=False;
    FreeAndNil(FServer);
  end;
  inherited Destroy;
end;


end.

