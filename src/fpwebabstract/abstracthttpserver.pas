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

unit AbstractHTTPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets,
  sslbase, ExtOpenSSL,
  sslsockets, ssockets, resolve,
  httpdefs;

Type
  TAbsHTTPConnection = Class;
  TAbsCustomHttpServer = Class;

  TRequestErrorHandler = Procedure (Sender : TObject; E : Exception) of object;
  TGetSocketHandlerEvent = Procedure (Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler) of object;
  TSocketHandlerCreatedEvent = Procedure (Sender : TObject; AHandler : TSocketHandler) of object;

  { TAbsHTTPContent }

  TAbsHTTPContent = class
  protected
    function  GetRawStringValue: RawByteString; virtual; abstract;
    function  GetStreamValue : TStream; virtual; abstract;
    procedure SetStreamValue(Str : TStream); virtual; abstract;
    procedure SetRawStringValue(AValue: RawByteString ); virtual; abstract;
  public
    property RawString : RawByteString  read GetRawStringValue write
                                             SetRawStringValue;
    { Pointer to Stream. Nb: do not forget that TStream should be returned with
      reseted position }
    property Stream : TStream read GetStreamValue write SetStreamValue;
  end;

  TAbsHTTPContentClass = class of TAbsHTTPContent;

  { TAbsHTTPRawStringContent }

  TAbsHTTPRawStringContent = Class(TAbsHTTPContent)
  private
    FStringContent : RawByteString;
  protected
    function  GetRawStringValue: RawByteString; override;
    procedure SetRawStringValue(AValue: RawByteString ); override;
    function  GetStreamValue : TStream; override;
    procedure SetStreamValue({%H-}Str : TStream); override;
  public
    constructor Create; virtual;
  end;

  { TAbsHTTPCombinedContent }

  TAbsHTTPCombinedContent = Class(TAbsHTTPContent)
  private
    FStream : TStream;
    FOwnStream : Boolean;
    procedure ReleaseStream;
  protected
    function  GetRawStringValue: RawByteString; override;
    procedure SetRawStringValue(AValue: RawByteString ); override;
    function  GetStreamValue : TStream; override;
    procedure SetStreamValue(Str : TStream); override;
    function  StreamAssigned : Boolean;
    function  UpdateStreamValue : TStream; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OwnStream : Boolean read FOwnStream write FOwnStream;
  end;

  { TErrContentDataTypeNotProvided }

  TErrContentDataTypeNotProvided = class(Exception)
  public
    constructor Create; overload;
  end;

  { TAbsHTTPConnectionRequest }

  TAbsHTTPConnectionRequest = Class(TRequest)
  private
    FConnection   : TAbsHTTPConnection;
    FAbsContent   : TAbsHTTPContent;
    FContentClass : TAbsHTTPContentClass;
    function GetContent: RawByteString;
    function GetContentStream : TStream;
    procedure SetContentClass(AValue: TAbsHTTPContentClass);
    procedure SetContent(AValue: RawByteString);
  protected
    Procedure InitRequestVars; override;
    Procedure SetConnection(AConnection : TAbsHTTPConnection);
  public
    Constructor Create; override;
    Constructor Create(aContentClass : TAbsHTTPContentClass); overload;
    Destructor Destroy; override;
    Property Connection : TAbsHTTPConnection Read FConnection;
    //we should override native TRequest::Content property
    Property Content : RawByteString read GetContent write SetContent;
    Property ContentStream : TStream read GetContentStream;
    Property ContentObject : TAbsHTTPContent read FAbsContent;
    Property ContentClass : TAbsHTTPContentClass read FContentClass write
                                                                SetContentClass;
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
    procedure UnknownHeader(ARequest: TAbsHTTPConnectionRequest; const AHeader: String); virtual; abstract;
    procedure HandleRequestError(E : Exception); virtual;
    Procedure SetupSocket; virtual;
    Procedure ConsumeHeader(ARequest: TAbsHTTPConnectionRequest; AHeader: String); virtual; abstract;
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
      Var ARequest  : TAbsHTTPConnectionRequest;
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
    FServer : TAbsInetServer;
    FLoadActivate : Boolean;
    FServerBanner: string;
    FLookupHostNames,
    FThreaded : Boolean;
    FConnectionCount : Integer;
    FUseSSL   : Boolean;
    FSSLType  : TExSSLType;
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
    Property SSLType : TExSSLType read FSSLType Write FSSLType;
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

implementation


resourcestring
  SErrSocketActive       =  'Operation not allowed while server is active';
  SErrContentDataTypeNotProvided =  'This data type not provided for content';


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

{ TErrContentDataTypeNotProvided }

constructor TErrContentDataTypeNotProvided.Create;
begin
  inherited Create(SErrContentDataTypeNotProvided);
end;

{ TAbsHTTPCombinedContent }

procedure TAbsHTTPCombinedContent.ReleaseStream;
begin
  if assigned(FStream) then
  begin
    if OwnStream then
      FreeAndNil(FStream) else
      FStream := nil;
  end;
end;

function TAbsHTTPCombinedContent.GetRawStringValue: RawByteString;
begin
  if assigned(Stream) then
  begin
    SetLength(Result, FStream.Size);
    FStream.Position := 0;
    FStream.ReadBuffer(Result[1], FStream.Size);
  end else Result := '';
end;

procedure TAbsHTTPCombinedContent.SetRawStringValue(AValue: RawByteString);
begin
  if assigned(Stream) then
  begin
    FStream.Size := Length(AValue);
    FStream.Position := 0;
    FStream.WriteBuffer(AValue[1], FStream.Size);
  end else
    raise TErrContentDataTypeNotProvided.Create;
end;

function TAbsHTTPCombinedContent.GetStreamValue: TStream;
begin
  if not Assigned(FStream) then FStream := UpdateStreamValue;
  Result := FStream;
end;

procedure TAbsHTTPCombinedContent.SetStreamValue(Str: TStream);
begin
  ReleaseStream;
  FStream := Str;
end;

function TAbsHTTPCombinedContent.StreamAssigned: Boolean;
begin
  Result := Assigned(FStream);
end;

function TAbsHTTPCombinedContent.UpdateStreamValue: TStream;
begin
  Result := TMemoryStream.Create;
  FOwnStream := true;
end;

constructor TAbsHTTPCombinedContent.Create;
begin
  FStream := nil;
end;

destructor TAbsHTTPCombinedContent.Destroy;
begin
  ReleaseStream;
  inherited Destroy;
end;

{ TAbsHTTPRawStringContent }

constructor TAbsHTTPRawStringContent.Create;
begin
  FStringContent := '';
end;

function TAbsHTTPRawStringContent.GetRawStringValue: RawByteString;
begin
  Result := FStringContent;
end;

procedure TAbsHTTPRawStringContent.SetRawStringValue(AValue: RawByteString);
begin
  FStringContent := AValue;
end;

function TAbsHTTPRawStringContent.GetStreamValue: TStream;
begin
  Result := nil;
end;

procedure TAbsHTTPRawStringContent.SetStreamValue({%H-}Str: TStream);
begin
  raise TErrContentDataTypeNotProvided.Create;
end;

{ TAbsHTTPConnectionResponse }

procedure TAbsHTTPConnectionResponse.SetConnection(
  AConnection: TAbsHTTPConnection);
begin
  FConnection := AConnection;
end;

function TAbsHTTPConnectionRequest.GetContent: RawByteString;
begin
  if Assigned(FAbsContent) then
    Result := FAbsContent.RawString else
    Result := '';
end;

function TAbsHTTPConnectionRequest.GetContentStream: TStream;
begin
  if Assigned(FAbsContent) then
    Result := FAbsContent.Stream else
    Result := nil;
end;

procedure TAbsHTTPConnectionRequest.SetContentClass(AValue: TAbsHTTPContentClass
  );
begin
  if FContentClass=AValue then Exit;
  FContentClass:=AValue;
  if Assigned(FAbsContent) then FAbsContent.Free;
  FAbsContent := FContentClass.Create;
end;

procedure TAbsHTTPConnectionRequest.SetContent(AValue: RawByteString);
begin
  if Assigned(FAbsContent) then
    FAbsContent.RawString := AValue;
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

constructor TAbsHTTPConnectionRequest.Create;
begin
  inherited Create;
  ContentClass := TAbsHTTPRawStringContent;
end;

constructor TAbsHTTPConnectionRequest.Create(aContentClass: TAbsHTTPContentClass
  );
begin
  inherited Create;
  ContentClass := aContentClass;
end;

destructor TAbsHTTPConnectionRequest.Destroy;
begin
  if Assigned(FAbsContent) then FAbsContent.Free;
  inherited Destroy;
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

function TAbsHTTPConnection.GetLookupHostNames: Boolean;
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
      try
        SetupSocket;
        StartServerSocket;
      finally
        FreeServerSocket;
      end;
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
  if assigned(FServer) then FreeAndNil(FServer);
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
    if S is TExtSSLSocketHandler then
      TExtSSLSocketHandler(S).ExSSLType:= SSLType else
      S.SSLType := ExSSLTypeToSSLType(SSLType);
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
