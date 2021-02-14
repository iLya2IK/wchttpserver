{
 AbstractHTTPServer:
   Abstract implementation of THttpServer
   based on fphttpserver and replace it.

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit abstracthttpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, sslbase, sslsockets, ssockets, resolve, httpdefs;

Type
  TAbsHTTPConnection = Class;
  TAbsCustomHttpServer = Class;

  TRequestErrorHandler = Procedure (Sender : TObject; E : Exception) of object;
  TGetSocketHandlerEvent = Procedure (Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler) of object;
  TSocketHandlerCreatedEvent = Procedure (Sender : TObject; AHandler : TSocketHandler) of object;

  { TAbsHTTPConnectionRequest }

  TAbsHTTPConnectionRequest = Class(TRequest)
  private
    FConnection: TAbsHTTPConnection;
  protected
    Procedure InitRequestVars; override;
    Procedure SetConnection(AConnection : TAbsHTTPConnection);
  public
    Property Connection : TAbsHTTPConnection Read FConnection;
  end;

  { TAbsHTTPConnectionResponse }

  TAbsHTTPConnectionResponse = Class(TResponse)
  private
    FConnection: TAbsHTTPConnection;
  Protected
    Procedure SetConnection(AConnection : TAbsHTTPConnection);
  public
    Property Connection : TAbsHTTPConnection Read FConnection;
  end;

  { TAbsHTTPConnection }

  TAbsHTTPConnection = Class(TObject)
  private
    FOnError: TRequestErrorHandler;
    FServer: TAbsCustomHttpServer;
    Function GetLookupHostNames : Boolean;
  Protected
    procedure UnknownHeader(ARequest: TRequest; const AHeader: String); virtual; abstract;
    procedure HandleRequestError(E : Exception); virtual;
    Procedure SetupSocket; virtual;
    Procedure ConsumeHeader(ARequest: TRequest; AHeader: String); virtual; abstract;
    procedure DoSocketAttach(ASocket : TSocketStream); virtual; abstract;
    function  GetSocket : TSocketStream; virtual; abstract;
  Public
    Constructor Create(AServer : TAbsCustomHttpServer; ASocket : TSocketStream); virtual;
    Destructor Destroy; override;
    Property Socket : TSocketStream Read GetSocket;
    Property Server : TAbsCustomHttpServer Read FServer;
    Property OnRequestError : TRequestErrorHandler Read FOnError Write FOnError;
    Property LookupHostNames : Boolean Read GetLookupHostNames;
  end;

  { TAbsHTTPServer }
  THTTPServerRequestHandler = Procedure (Sender: TObject;
      Var ARequest: TAbsHTTPConnectionRequest;
      Var AResponse : TAbsHTTPConnectionResponse) of object;

  { TAbsInetServer }

  TAbsInetServer = Class(TInetServer)
  public
//    Function SockToStream (ASocket : Longint) : TSocketStream;Override;
  end;

  { TAbsCustomHttpServer }

  TAbsCustomHttpServer = Class(TComponent)
  Private
    FAcceptIdleTimeout: Cardinal;
    FAdminMail: string;
    FAdminName: string;
    FAfterSocketHandlerCreated: TSocketHandlerCreatedEvent;
    FCertificateData: TCertificateData;
    FOnAcceptIdle: TNotifyEvent;
    FOnAllowConnect: TConnectQuery;
    FOnGetSocketHandler: TGetSocketHandlerEvent;
    FOnRequest: THTTPServerRequestHandler;
    FOnRequestError: TRequestErrorHandler;
    FAddress: string;
    FPort: Word;
    FQueueSize: Word;
    FServer : TInetServer;
    FLoadActivate : Boolean;
    FServerBanner: string;
    FLookupHostNames,
    FThreaded: Boolean;
    FConnectionCount : Integer;
    FUseSSL: Boolean;
    FSSLType : TSSLType;
    FAlpnList : TStringList;
    FSSLLoc, FSSLMasterKeyLog : String;
    procedure DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler);
    function GetActive: Boolean;
    function GetCertificate: String;
    function GetHostName: string;
    function GetPrivateKey: String;
    function GetSSLMasterKeyLog: String;
    procedure SetAcceptIdleTimeout(AValue: Cardinal);
    procedure SetActive(const AValue: Boolean);
    procedure SetIdle(AValue: TNotifyEvent);
    procedure SetOnAllowConnect(const AValue: TConnectQuery);
    procedure SetAddress(const AValue: string);
    procedure SetPort(const AValue: Word);
    procedure SetQueueSize(const AValue: Word);
    procedure SetThreaded(const AValue: Boolean);
    procedure SetupSocket;
    procedure WaitForRequests;
  Protected
    procedure SetSSLMasterKeyLog(AValue: String); virtual;
    procedure SetCertificateData(AValue: TCertificateData); virtual;
    procedure SetHostName(AValue: string); virtual;
    procedure SetCertificate(AValue: String); virtual;
    procedure SetPrivateKey(AValue: String); virtual;
    // Override this to create descendent
    function CreateSSLSocketHandler: TSocketHandler; virtual;
    // Override this to create descendent
    Function CreateCertificateData : TCertificateData; virtual;
    // Override this to create descendent
    Function GetSocketHandler(Const UseSSL : Boolean) : TSocketHandler;  virtual;
    // Override these to create descendents of the request/response instead.
    Function CreateRequest : TAbsHTTPConnectionRequest; virtual; abstract;
    Function CreateResponse(ARequest : TAbsHTTPConnectionRequest) : TAbsHTTPConnectionResponse; virtual; abstract;
    Procedure InitRequest(ARequest : TAbsHTTPConnectionRequest); virtual; abstract;
    Procedure InitResponse(AResponse : TAbsHTTPConnectionResponse); virtual; abstract;
    // Called on accept errors
    procedure DoAcceptError(Sender: TObject; {%H-}ASocket: Longint; {%H-}E: Exception;  var ErrorAction: TAcceptErrorAction);
    // Create a connection handling object.
    function CreateConnection(Data : TSocketStream) : TAbsHTTPConnection; virtual; abstract;
    procedure CreateConnectionThread(Conn: TAbsHTTPConnection); virtual; abstract;
    // Check if server is inactive
    Procedure CheckInactive;
    // Called by TInetServer when a new connection is accepted.
    Procedure DoConnect(Sender : TObject; Data : TSocketStream); virtual;
    // Create and configure TInetServer
    Procedure CreateServerSocket; virtual;
    // Start server socket
    procedure StartServerSocket; virtual;
    // Stop server stocket
    procedure StopServerSocket; virtual;
    // free server socket instance
    Procedure FreeServerSocket; virtual;
    // Handle request. This calls OnRequest. It can be overridden by descendants to provide standard handling.
    procedure HandleRequest(Var ARequest: TAbsHTTPConnectionRequest;
                            Var AResponse : TAbsHTTPConnectionResponse); virtual;
    // Called when a connection encounters an unexpected error. Will call OnRequestError when set.
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    // Connection count
    Property ConnectionCount : Integer Read FConnectionCount;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  protected
    // Set to true to start listening.
    Property Active : Boolean Read GetActive Write SetActive Default false;
    // Address to listen on.
    Property Address : string Read FAddress Write SetAddress;
    // Port to listen on.
    Property Port : Word Read FPort Write SetPort Default 80;
    // Max connections on queue (for Listen call)
    Property QueueSize : Word Read FQueueSize Write SetQueueSize Default 5;
    // Called when deciding whether to accept a connection.
    Property OnAllowConnect : TConnectQuery Read FOnAllowConnect Write SetOnAllowConnect;
    // Use a thread to handle a connection ?
    property Threaded : Boolean read FThreaded Write SetThreaded;
    // Called to handle the request. If Threaded=True, it is called in a the connection thread.
    Property OnRequest : THTTPServerRequestHandler Read FOnRequest Write FOnRequest;
    // Called when an unexpected error occurs during handling of the request. Sender is the TAbsHTTPConnection.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Called when there are no connections waiting.
    Property OnAcceptIdle : TNotifyEvent Read FOnAcceptIdle Write SetIdle;
    // If >0, when no new connection appeared after timeout, OnAcceptIdle is called.
    Property AcceptIdleTimeout : Cardinal Read FAcceptIdleTimeout Write SetAcceptIdleTimeout;
  published
    //aditional server information
    property AdminMail: string read FAdminMail write FAdminMail;
    property AdminName: string read FAdminName write FAdminName;
    property ServerBanner: string read FServerBanner write FServerBanner;
    Property LookupHostNames : Boolean Read FLookupHostNames Write FLookupHostNames;
    // You need to set this if you want to use SSL
    property HostName : string Read GetHostName Write SetHostName; deprecated 'Use certificatedata instead';
    // Properties to use when doing SSL handshake
    Property CertificateData  : TCertificateData Read FCertificateData Write SetCertificateData;
    // Set to true if you want to use SSL
    Property UseSSL : Boolean Read FUseSSL Write FUseSSL;
    Property SSLType : TSSLType read FSSLType Write FSSLType;
    Property Certificate : String read GetCertificate write SetCertificate;
    Property PrivateKey : String read GetPrivateKey write SetPrivateKey;
    Property SSLMasterKeyLog : String read GetSSLMasterKeyLog write SetSSLMasterKeyLog;
    Property SSLLoc : String read FSSLLoc write FSSLLoc;
    Property AlpnList : TStringList read FAlpnList;
    // Called to create socket handler. If not set, or Nil is returned, a standard socket handler is created.
    Property OnGetSocketHandler : TGetSocketHandlerEvent Read FOnGetSocketHandler Write FOnGetSocketHandler;
    // Called after create socket handler was created, with the created socket handler.
    Property AfterSocketHandlerCreate : TSocketHandlerCreatedEvent Read FAfterSocketHandlerCreated Write FAfterSocketHandlerCreated;

  end;

  TAbsHTTPServer = Class(TAbsCustomHttpServer)
  Published
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    property Threaded;
    Property OnRequest;
    Property OnRequestError;
    Property OnAcceptIdle;
    Property AcceptIdleTimeout;
  end;

  EHTTPServer = Class(EHTTP);

  Function GetStatusCode (ACode: Integer) : String;

implementation


resourcestring
  SErrSocketActive    =  'Operation not allowed while server is active';

{ TAbsHTTPConnectionRequest }
Function GetStatusCode (ACode: Integer) : String;
begin
  Case ACode of
    100 :  Result:='Continue';
    101 :  Result:='Switching Protocols';
    200 :  Result:='OK';
    201 :  Result:='Created';
    202 :  Result:='Accepted';
    203 :  Result:='Non-Authoritative Information';
    204 :  Result:='No Content';
    205 :  Result:='Reset Content';
    206 :  Result:='Partial Content';
    300 :  Result:='Multiple Choices';
    301 :  Result:='Moved Permanently';
    302 :  Result:='Found';
    303 :  Result:='See Other';
    304 :  Result:='Not Modified';
    305 :  Result:='Use Proxy';
    307 :  Result:='Temporary Redirect';
    400 :  Result:='Bad Request';
    401 :  Result:='Unauthorized';
    402 :  Result:='Payment Required';
    403 :  Result:='Forbidden';
    404 :  Result:='Not Found';
    405 :  Result:='Method Not Allowed';
    406 :  Result:='Not Acceptable';
    407 :  Result:='Proxy Authentication Required';
    408 :  Result:='Request Time-out';
    409 :  Result:='Conflict';
    410 :  Result:='Gone';
    411 :  Result:='Length Required';
    412 :  Result:='Precondition Failed';
    413 :  Result:='Request Entity Too Large';
    414 :  Result:='Request-URI Too Large';
    415 :  Result:='Unsupported Media Type';
    416 :  Result:='Requested range not satisfiable';
    417 :  Result:='Expectation Failed';
    418 :  Result:='I''m a teapot';
    421 :  Result:='Misdirected Request';
    422 :  Result:='Unprocessable Entity';
    423 :  Result:='Locked';
    424 :  Result:='Failed Dependency';
    425 :  Result:='Too Early';
    426 :  Result:='Upgrade Required';
    428 :  Result:='Precondition Required';
    429 :  Result:='Too Many Requests';
    431 :  Result:='Request Header Fields Too Large';
    451 :  Result:='Unavailable For Legal Reasons';

    500 :  Result:='Internal Server Error';
    501 :  Result:='Not Implemented';
    502 :  Result:='Bad Gateway';
    503 :  Result:='Service Unavailable';
    504 :  Result:='Gateway Time-out';
    505 :  Result:='HTTP Version not supported';
  else
    Result:='Unknown status';
  end;
end;

Function GetHostNameByAddress(const AnAddress: String): String;
var
  Resolver: THostResolver;
begin
  Result := '';
  if AnAddress = '' then exit;
  Resolver := THostResolver.Create(nil);
  try
    if Resolver.AddressLookup(AnAddress) then
      Result := Resolver.ResolvedName
  finally
    FreeAndNil(Resolver);
  end;
end;

{ TAbsHTTPConnectionResponse }

procedure TAbsHTTPConnectionResponse.SetConnection(
  AConnection: TAbsHTTPConnection);
begin
  FConnection := AConnection;
end;

procedure TAbsHTTPConnectionRequest.InitRequestVars;
Var
  P : Integer;
  S : String;
begin
  S:=URL;
  P:=Pos('?',S);
  if (P<>0) then
    SetHTTPVariable(hvQuery,Copy(S,P+1,Length(S)-P));
  if Assigned(FConnection) and FConnection.LookupHostNames then
    SetHTTPVariable(hvRemoteHost,GetHostNameByAddress(RemoteAddress));
  inherited InitRequestVars;
end;

procedure TAbsHTTPConnectionRequest.SetConnection(
  AConnection: TAbsHTTPConnection);
begin
  FConnection := AConnection;
end;

Function SocketAddrToString(ASocketAddr: TSockAddr): String;
begin
  if ASocketAddr.sa_family = AF_INET then
    Result := NetAddrToStr(ASocketAddr.sin_addr)
  else // no ipv6 support yet
    Result := '';
end;

procedure TAbsHTTPConnection.HandleRequestError(E: Exception);
begin
  If Assigned(FOnError) then
    try
      FOnError(Self,E);
    except
      // We really cannot handle this...
    end;
end;

procedure TAbsHTTPConnection.SetupSocket;
begin
{$if defined(FreeBSD) or defined(Linux)}
  Socket.ReadFlags:=MSG_NOSIGNAL;
  Socket.WriteFlags:=MSG_NOSIGNAL;
{$endif}
end;

constructor TAbsHTTPConnection.Create(AServer: TAbsCustomHttpServer; ASocket: TSocketStream);
begin
  if assigned(ASocket) then DoSocketAttach(ASocket);
  FServer:=AServer;
  If Assigned(FServer) then
    InterLockedIncrement(FServer.FConnectionCount)
end;

destructor TAbsHTTPConnection.Destroy;
begin
  If Assigned(FServer) then
    InterLockedDecrement(FServer.FConnectionCount);
  Inherited;
end;

Function TAbsHTTPConnection.GetLookupHostNames : Boolean;
begin
  if Assigned(FServer) then
    Result:=FServer.LookupHostNames
  else
    Result:=False;
end;

{ TAbsCustomHttpServer }

procedure TAbsCustomHttpServer.HandleRequestError(Sender: TObject; E: Exception);
begin
  If Assigned(FOnRequestError) then
    try
      FOnRequestError(Sender,E);
    except
      // Do not let errors in user code escape.
    end
end;

procedure TAbsCustomHttpServer.DoAcceptError(Sender: TObject;
  {%H-}ASocket: Longint;
  {%H-}E: Exception; var ErrorAction: TAcceptErrorAction);
begin
  If Not Active then
    ErrorAction:=AEAStop
  else
    ErrorAction:=AEARaise
end;

function TAbsCustomHttpServer.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result:=FLoadActivate
  else
    Result:=Assigned(FServer);
end;

function TAbsCustomHttpServer.GetCertificate: String;
begin
  Result := CertificateData.Certificate.FileName;
end;

procedure TAbsCustomHttpServer.DoCreateClientHandler(Sender: TObject; out AHandler: TSocketHandler);
begin
  AHandler:=GetSocketHandler(UseSSL);
end;

function TAbsCustomHttpServer.GetHostName: string;
begin
  Result:=FCertificateData.HostName;
end;

function TAbsCustomHttpServer.GetPrivateKey: String;
begin
  Result := CertificateData.PrivateKey.FileName;
end;

function TAbsCustomHttpServer.GetSSLMasterKeyLog: String;
begin
  Result := FSSLMasterKeyLog;
end;

procedure TAbsCustomHttpServer.SetAcceptIdleTimeout(AValue: Cardinal);
begin
  if FAcceptIdleTimeout=AValue then Exit;
  FAcceptIdleTimeout:=AValue;
  If Assigned(FServer) then
    FServer.AcceptIdleTimeOut:=AValue;
end;

procedure TAbsCustomHttpServer.StopServerSocket;
begin
  FServer.StopAccepting(False);
end;

procedure TAbsCustomHttpServer.SetActive(const AValue: Boolean);
begin
  If AValue=GetActive then exit;
  FLoadActivate:=AValue;
  if not (csDesigning in Componentstate) then
    if AValue then
    begin
      CreateServerSocket;
      SetupSocket;
      StartServerSocket;
      FreeServerSocket;
    end
    else
      StopServerSocket;
end;

procedure TAbsCustomHttpServer.SetCertificate(AValue: String);
begin
  CertificateData.Certificate.FileName:= FSSLLoc + AValue;
end;

procedure TAbsCustomHttpServer.SetCertificateData(AValue: TCertificateData);
begin
  if FCertificateData=AValue then Exit;
  FCertificateData:=AValue;
end;

procedure TAbsCustomHttpServer.SetHostName(AValue: string);
begin
  FCertificateData.HostName:=aValue;
end;

procedure TAbsCustomHttpServer.SetIdle(AValue: TNotifyEvent);
begin
  FOnAcceptIdle:=AValue;
  if Assigned(FServer) then
    FServer.OnIdle:=AValue;
end;

procedure TAbsCustomHttpServer.SetOnAllowConnect(const AValue: TConnectQuery);
begin
  if FOnAllowConnect=AValue then exit;
  CheckInactive;
  FOnAllowConnect:=AValue;
end;

procedure TAbsCustomHttpServer.SetAddress(const AValue: string);
begin
  if FAddress=AValue then exit;
  CheckInactive;
  FAddress:=AValue;
end;

procedure TAbsCustomHttpServer.SetPort(const AValue: Word);
begin
  if FPort=AValue then exit;
  CheckInactive;
  FPort:=AValue;
end;

procedure TAbsCustomHttpServer.SetPrivateKey(AValue: String);
begin
  CertificateData.PrivateKey.FileName:= FSSLLoc + AValue;
end;

procedure TAbsCustomHttpServer.SetQueueSize(const AValue: Word);
begin
  if FQueueSize=AValue then exit;
  CheckInactive;
  FQueueSize:=AValue;
end;

procedure TAbsCustomHttpServer.SetSSLMasterKeyLog(AValue: String);
begin
  if Length(AValue) > 0 then
    FSSLMasterKeyLog:= FSSLLoc + AValue else
      FSSLMasterKeyLog:= '';
end;

procedure TAbsCustomHttpServer.SetThreaded(const AValue: Boolean);
begin
  if FThreaded=AValue then exit;
  CheckInactive;
  FThreaded:=AValue;
end;

procedure TAbsCustomHttpServer.CheckInactive;
begin
  If GetActive then
    Raise EHTTPServer.Create(SErrSocketActive);
end;

procedure TAbsCustomHttpServer.DoConnect(Sender: TObject; Data: TSocketStream);
Var
  Con : TAbsHTTPConnection;
begin
  Con:=CreateConnection(Data);
  try
    Con.FServer:=Self;
    Con.OnRequestError:=@HandleRequestError;
    CreateConnectionThread(Con);
  finally
  end;
end;

procedure TAbsCustomHttpServer.SetupSocket;
begin
  FServer.QueueSize:=Self.QueueSize;
  FServer.ReuseAddress:=true;
end;

procedure TAbsCustomHttpServer.CreateServerSocket;
begin
  if FAddress='' then
    FServer:=TAbsInetServer.Create(FPort)
  else
    FServer:=TAbsInetServer.Create(FAddress,FPort);
  FServer.OnCreateClientSocketHandler:=@DoCreateClientHandler;
  FServer.MaxConnections:=-1;
  FServer.OnConnectQuery:=OnAllowConnect;
  FServer.OnConnect:=@DOConnect;
  FServer.OnAcceptError:=@DoAcceptError;
  FServer.OnIdle:=OnAcceptIdle;
  FServer.AcceptIdleTimeOut:=AcceptIdleTimeout;
  FServer.SetNonBlocking;
end;

procedure TAbsCustomHttpServer.StartServerSocket;
begin
  FServer.Bind;
  FServer.Listen;
  FServer.StartAccepting;
end;

procedure TAbsCustomHttpServer.FreeServerSocket;
begin
  FreeAndNil(FServer);
end;

procedure TAbsCustomHttpServer.HandleRequest(var ARequest: TAbsHTTPConnectionRequest;
  var AResponse: TAbsHTTPConnectionResponse);
begin
  If Assigned(FOnRequest) then
    FonRequest(Self,ARequest,AResponse);
end;

constructor TAbsCustomHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort:=80;
  FQueueSize:=5;
  FServerBanner := 'FreePascal';
  FCertificateData:=CreateCertificateData;
  FAlpnList := TStringList.Create;
  FSSLMasterKeyLog := '';
end;

procedure TAbsCustomHttpServer.WaitForRequests;
Var
  FLastCount,ACount : Integer;
begin
  ACount:=0;
  FLastCount:=FConnectionCount;
  While (FConnectionCount>0) and (ACount<10) do
  begin
    Sleep(100);
    if (FConnectionCount=FLastCount) then
      Dec(ACount)
    else
      FLastCount:=FConnectionCount;
  end;
end;

function TAbsCustomHttpServer.CreateCertificateData: TCertificateData;
begin
  Result:=TCertificateData.Create;
end;

function TAbsCustomHttpServer.CreateSSLSocketHandler : TSocketHandler;
Var
  S : TSSLSocketHandler;
  CK : TCertAndKey;
begin
  S:=TSSLSocketHandler.GetDefaultHandler;
  try
    S.SSLType:= SSLType;
    // We must create the certificate once in our global copy of CertificateData !
    if CertificateData.NeedCertificateData then
    begin
      S.CertGenerator.HostName:=CertificateData.Hostname;
      CK:=S.CertGenerator.CreateCertificateAndKey;
      CertificateData.Certificate.Value:=CK.Certificate;
      CertificateData.PrivateKey.Value:=CK.PrivateKey;
    end;
    S.CertificateData:=Self.CertificateData;
    S.SendHostAsSNI:=true;
    Result:=S;
  except
    S.free;
    Result := nil;
    Raise;
  end;
end;

function TAbsCustomHttpServer.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
begin
  Result:=Nil;
  if Assigned(FonGetSocketHandler) then
    FOnGetSocketHandler(Self,UseSSL,Result);
  if (Result=Nil) then
    If UseSSL then
      Result:=CreateSSLSocketHandler
    else
      Result:=TSocketHandler.Create;
  if Assigned(FAfterSocketHandlerCreated) then
    FAfterSocketHandlerCreated(Self,Result);
end;

destructor TAbsCustomHttpServer.Destroy;
begin
  Active:=False;
  if Threaded and (FConnectionCount>0) then
    WaitForRequests;
  FreeAndNil(FCertificateData);
  FreeAndNil(FAlpnList);
  inherited Destroy;
end;

end.
