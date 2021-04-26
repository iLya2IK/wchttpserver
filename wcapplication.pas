{
 WCApplication:
   Custom application class to integrate with LCL

   Part of WCHTTPServer project

   Copyright (c) 2020-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcapplication;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  StringHashList,
  SysUtils, DateUtils,
  ECommonObjs,
  fpwebfile, fpmimetypes,
  fphttp, HTTPDefs, httpprotocol, http2consts,
  abstracthttpserver, wchttp2server,
  extopenssl,
  custweb, custabshttpapp,
  sqlitewebsession,
  sqlitelogger,
  jsonscanner, jsonparser, fpjson,
  wcconfig,
  ExtSqlite3DS,
  variants,
  sockets,
  ssockets,
  gzstream,
  wcdecoders,
  BufferedStream,
  SortedThreadPool,
  RegExpr,
  {$ifdef DEBUG_STAT}
  wcdebug_vars,
  {$endif}
  AvgLvlTree
  {$ifdef unix}
  , BaseUnix
  {$ifdef NOGUI}
  , gwidgetsethelper
  {$endif}
  {$endif}    ;

type
  TWebClient = class;
  TWebClients = class;
  TWCResponse = class;
  TWCRequest = class;
  TWCConnection = class;
  TWCHTTPServer = class;
  TWebClientClass = class of TWebClient;

  { TWCConnection }

  TWCConnection =  class(TWCHTTPConnection)
  private
    FInputBuf : Pointer;
    FInput    : TBufferedStream;
    FResponse : TWCResponse;
    FRequest  : TWCRequest;
    FClient   : TWebClient;
    FSession  : TSqliteWebSession;
    FProtocolVersion : TWCProtocolVersion;
    {$ifdef DEBUG_STAT}
    FDStartStamp, FDWaitStamp, FDStopStamp, FDReadStamp : QWord;
    {$endif}
    procedure DoInitialize;
  protected
    function ReadLine : String;
    Function ReadReqHeaders : TWCRequest;
    function ConvertFromHTTP2Req(AReq2 : TWCHTTP2Request) : TWCRequest;
    procedure ReadReqContent(ARequest: TWCRequest);
    procedure ConsumeHeader(ARequest: TRequest; AHeader: String); override;
    procedure UnknownHeader({%H-}ARequest: TRequest; const {%H-}AHeader: String); override;
    procedure DoSocketAttach(ASocket : TSocketStream); override;
  public
    Constructor Create(AServer : TAbsCustomHTTPServer; ASocket : TSocketStream); override;
    Constructor CreateRefered(AServer : TAbsCustomHTTPServer; ASocketRef : TWCHTTPSocketReference); override;
    function ConsumeSocketData : Cardinal;
    procedure SetSessionParams(aClient : TWebClient; aSession : TSqliteWebSession);
    destructor Destroy; override;
    property Response : TWCResponse read FResponse;
    property Request : TWCRequest read FRequest;
    property Client : TWebClient read FClient;
    property Session : TSqliteWebSession read FSession;
    property HTTPVersion : TWCProtocolVersion read FProtocolVersion;
  end;

  { TWCMainClientJob }

  TWCMainClientJob = class(TSortedJob)
  private
    FConn : TWCConnection;
    FResponseReadyToSend : Boolean;
    function GetClient: TWebClient;
    function GetRequest: TWCRequest;
    function GetResponse: TWCResponse;
  public
    P1, P2 : Variant;
    constructor Create(aConn : TWCConnection); overload;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ReleaseConnection;
    property  Connection : TWCConnection read FConn;
    property  Request : TWCRequest read GetRequest;
    property  Response : TWCResponse read GetResponse;
    property  Client : TWebClient read GetClient;
    property  ResponseReadyToSend : Boolean read FResponseReadyToSend write
                                                 FResponseReadyToSend;
  end;
  TWCMainClientJobClass = class of TWCMainClientJob;

  { TWCPreAnalizeClientJob }

  TWCPreAnalizeClientJob = class(TLinearJob)
  private
    FConn : TWCConnection;
    function GetRequest: TWCRequest;
    function GetResponse: TWCResponse;
  public
    constructor Create(aConn : TWCConnection);
    destructor Destroy; override;
    procedure Execute; override;
    function GenerateClientJob : TWCMainClientJob; virtual;
    property Connection : TWCConnection read FConn;
    property Request : TWCRequest read GetRequest;
    property Response : TWCResponse read GetResponse;
  end;
  TWCPreAnalizeClientJobClass = class of TWCPreAnalizeClientJob;

  { TWCHttpServer }

  TWCHttpServer = class(TEmbeddedAbsHttpServer)
  private
    FThreadPool : TSortedThreadPool;
    FPoolsLocker : TNetCustomLockedObject;
    FSSLLocker : TNetCustomLockedObject;
    FHTTPRefConnections : TWCHTTP2ServerRefConnections;
    function CompareMainJobs({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer) : Integer;
    procedure AddToMainPool(AJob : TWCMainClientJob);
    procedure CheckThreadPool;
    function GetMaxMainClientsThreads: Integer;
    function GetMaxPreClientsThreads: Integer;
    procedure StopThreads;
  protected
    procedure SetSSLMasterKeyLog(AValue: String); override;
    procedure SetHostName(AValue: string); override;
    procedure SetCertificate(AValue: String); override;
    procedure SetPrivateKey(AValue: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    function  CreateSSLSocketHandler: TSocketHandler; override;
    Procedure CreateConnectionThread(Conn : TAbsHTTPConnection); override;
    function  CreateConnection(Data : TSocketStream) : TAbsHTTPConnection; override;
    Function  CreateRequest : TAbsHTTPConnectionRequest; override;
    Function  CreateResponse(ARequest : TAbsHTTPConnectionRequest) : TAbsHTTPConnectionResponse; override;
    Procedure InitRequest(ARequest : TAbsHTTPConnectionRequest); override;
    Procedure InitResponse(AResponse : TAbsHTTPConnectionResponse); override;
    function AttachNewHTTP2Con(aSocket: TWCHTTPSocketReference;
      aOpenMode: THTTP2OpenMode; aServerDoConsume: THttpRefSocketConsume;
      aSendData: THttpRefSendData): TWCHTTP2Connection;
    function AttachNewHTTP11Con(aSocket: TWCHTTPSocketReference;
      aServerDoConsume: THttpRefSocketConsume;
      aSendData: THttpRefSendData): TWCHTTP11Connection;
    property  MaxPreClientsThreads : Integer read GetMaxPreClientsThreads;
    property  MaxMainClientsThreads : Integer read GetMaxMainClientsThreads;
    procedure AddLinearJob(AJob : TLinearJob);
    procedure AddSortedJob(AJob : TSortedJob);
    function  ServerActive : Boolean;
    procedure DoConnectToSocketRef(SockRef : TWCHTTPSocketReference);
    procedure DoSendData(aConnection : TWCHTTPRefConnection);
    destructor Destroy; override;

    property  HTTPRefConnections : TWCHTTP2ServerRefConnections read FHTTPRefConnections;
  end;

  { TWCHttpServerHandler }

  TWCHttpServerHandler = class(TAbsHTTPServerHandler)
  protected
    procedure HandleRequestError(Sender: TObject; E: Exception); override;
  public
    Function CreateServer : TEmbeddedAbsHttpServer; override;
    function GetESServer : TWCHttpServer;
  end;

  { TWCHTTPTemplate }

  TWCHTTPTemplateRecord = record
    Compress : Boolean;
    Cache    : String;
    Charset  : String;
  end;

  TWCHTTPTemplate = class
  private
    FMimeRegExp : TRegExpr;
    FURIRegExp : TRegExpr;
    FComplete   : Boolean;
    FCompress   : PBoolean;
    FCache      : PChar;
    FCharset    : PChar;
    FPriority   : Integer;
  public
    constructor Create(obj : TJSONObject);
    procedure   Rebuild(obj : TJSONObject);
    destructor  Destroy; override;
    function Check(const aMime, aURI : String) : Boolean;
    function GetCompress(lV : Boolean) : Boolean;
    procedure GetCache(var lV: String);
    procedure GetCharset(var lV: String);
    procedure GetStatus(var R : TWCHTTPTemplateRecord);
    property Priority : Integer read FPriority;
    property Complete : Boolean read FComplete;
  end;

  { TWCHTTPMimeTemplates }

  TWCHTTPMimeTemplates = class(TThreadSafeFastCollection)
  private
    function GetTemplateInd(index : Integer): TWCHTTPTemplate;
  public
    procedure SortTemplates;
    procedure Rebuild(aTemplates : TJSONArray);
    function  GetTemplate(const aMime, aURI : String) : TWCHTTPTemplateRecord;
    property Template[index : Integer] : TWCHTTPTemplate read
                                                 GetTemplateInd; default;
  end;

  { TWCHTTPConfig }

  TWCHTTPConfig = class(TWCConfig)
  private
    FMimeTemplates : TWCHTTPMimeTemplates;
    procedure  DoConfigLoaded(aConfig : TJSONObject);
  protected
    procedure  DoInitialize(); override;
  public
    destructor Destroy; override;
  end;

  { TWCHTTPApplication }

  TWCHTTPApplication = Class(TCustomAbsHTTPApplication)
  private
    FMTime : QWord;
    FLogDB : TSqliteLogger;
    FSocketsReferences, FReferences : TNetReferenceList;
    FStartStamp : QWord;
    FNetDebugMode : Boolean;
    FWebClientClass :  TWebClientClass;
    FServerAnalizeJobClass : TWCPreAnalizeClientJobClass;

    FConfig : TWCHTTPConfig;
    FMaxMainThreads: TThreadInteger;
    FMaxPrepareThreads: TThreadInteger;
    FThreadJobToJobWait : TThreadJobToJobWait;
    FCompressLimit: TThreadInteger;
    FClientCookieMaxAge : TThreadInteger;
    FClientTimeOut : TThreadInteger;
    FClientDecoders : TThreadSafeDecoders;
    FVPath, FMainHTTP, FSessionsLoc,
    FSessionsDb, FLogDbLoc,
    FWebFilesLoc, FSSLLoc, FMimeLoc : TThreadUtf8String;
    FWebFilesIgnore, FWebFilesExceptIgnore : TThreadUtf8String;
    FWebFilesIgnoreRx, FWebFilesExceptIgnoreRx : TRegExpr;

    FNeedShutdown : TThreadBoolean;

    procedure DoOnConfigChanged(Sender : TWCConfigRecord);
    procedure DoOnLoggerException(Sender : TObject; E : Exception);
    procedure DoOnException(Sender : TObject; E : Exception);
    Procedure DoGetModule(Sender : TObject; {%H-}ARequest : TRequest;
                               Var ModuleClass : TCustomHTTPModuleClass);
    procedure DoOnIdle({%H-}sender : TObject);
    function GetMimeTemplates: TWCHTTPMimeTemplates;
    function GetClientAllowEncode : String;
    function GetClientCookieMaxAge: Integer;
    function GetClientTimeOut: Integer;
    function GetCompressLimit: Cardinal;
    function GetConfigFileName: String;
    function GetESServer: TWCHttpServer;
    function GetJobToJobWait: TJobToJobWait;
    function GetLogDbLoc: String;
    function GetMainHTTP: String;
    function GetMaxMainThreads: Byte;
    function GetMaxPrepareThreads: Byte;
    function GetMimeLoc: String;
    function GetNeedShutdown: Boolean;
    function GetSessionsDb: String;
    function GetSessionsLoc: String;
    function GetSitePath: String;
    function GetSSLLoc: String;
    function GetWebFilesExcludeIgnore: String;
    function GetWebFilesIgnore: String;
    function getWebFilesLoc: String;
    procedure SetClientCookieMaxAge(AValue: Integer);
    procedure SetClientTimeOut(AValue: Integer);
    procedure SetCompressLimit(AValue: Cardinal);
    procedure SetConfigFileName(AValue: String);
    procedure SetJobToJobWait(AValue: TJobToJobWait);
    procedure SetLogDbLoc(AValue: String);
    procedure SetMainHTTP(AValue: String);
    procedure SetMaxMainThreads(AValue: Byte);
    procedure SetMaxPrepareThreads(AValue: Byte);
    procedure SetMimeLoc(AValue: String);
    procedure SetNeedShutdown(AValue: Boolean);
    procedure SetSessionsDb(AValue: String);
    procedure SetSessionsLoc(AValue: String);
    procedure SetSSLLoc(AValue: String);
    procedure SetWebFilesExcludeIgnore(AValue: String);
    procedure SetWebFilesIgnore(AValue: String);
    procedure SetWebFilesLoc(AValue: String);
    procedure SetClientAllowEncode(AValue : String);
    procedure StopThreads;
    function  Initialized : Boolean;
    function ConfigChangeHalt : Boolean;
  protected
    Procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoInfo(const V : String); overload;
    procedure DoInfo(const V : String; const aParams : Array Of const); overload;
    procedure DoError(const V : String); overload;
    procedure DoError(const V : String; const aParams : Array Of const); overload;
    procedure SendError(AResponse: TResponse; errno: Integer);
    Procedure Initialize; override;
    function GetWebHandler: TWCHttpServerHandler;
    function InitializeAbstractWebHandler : TWebHandler; override;
    function GetTimeSecFromStart : Cardinal;
    function CreateReferedMemoryStream : TRefMemoryStream;
    property ESServer : TWCHttpServer read GetESServer;
    property GarbageCollector : TNetReferenceList read FReferences;
    property SocketsCollector : TNetReferenceList read FSocketsReferences;

    //configurations
    property NetDebugMode : Boolean read FNetDebugMode;
    property WebClientClass : TWebClientClass read FWebClientClass write FWebClientClass;
    property ServerAnalizeJobClass : TWCPreAnalizeClientJobClass read FServerAnalizeJobClass write FServerAnalizeJobClass;
    property SitePath : String read GetSitePath;
    property MainURI : String read GetMainHTTP write SetMainHTTP;
    property SessionsLoc : String read GetSessionsLoc write SetSessionsLoc;
    property SessionsDb : String read GetSessionsDb write SetSessionsDb;
    property LogDb : String read GetLogDbLoc write SetLogDbLoc;
    property MimeLoc : String read GetMimeLoc write SetMimeLoc;
    //webfiles
    property WebFilesLoc : String read getWebFilesLoc write SetWebFilesLoc;
    property CompressLimit : Cardinal read GetCompressLimit write SetCompressLimit;
    property WebFilesIgnore : String read GetWebFilesIgnore write SetWebFilesIgnore;
    property WebFilesExcludeIgnore : String read GetWebFilesExcludeIgnore write SetWebFilesExcludeIgnore;
    function IsAcceptedWebFile(const FN: String) : Boolean;
    property MimeTemplates : TWCHTTPMimeTemplates read GetMimeTemplates;
    //threads
    property MaxPrepareThreads : Byte read GetMaxPrepareThreads write SetMaxPrepareThreads;
    property MaxMainThreads : Byte read GetMaxMainThreads write SetMaxMainThreads;
    property ThreadPoolJobToJobWait : TJobToJobWait read GetJobToJobWait write SetJobToJobWait;
    //clients
    property ClientTimeOut : Integer read GetClientTimeOut write SetClientTimeOut;
    property ClientCookieMaxAge : Integer read GetClientCookieMaxAge write SetClientCookieMaxAge;
    property ClientAllowEncode : String read GetClientAllowEncode write SetClientAllowEncode;
    property ClientDecoders : TThreadSafeDecoders read FClientDecoders;
    //openssl
    property SSLLoc : String read GetSSLLoc write SetSSLLoc;
    //main config
    property Config : TWCHTTPConfig read FConfig;
    property ConfigFileName : String read GetConfigFileName write SetConfigFileName;
    //maintaining
    property NeedShutdown : Boolean read GetNeedShutdown write SetNeedShutdown;
  end;

  { TWebCachedItem }

  TWebCachedItem = class(TNetCustomLockedObject)
  private
    FDataTime : TDateTime;
    FCache, FDeflateCache : TRefMemoryStream;
    FNeedToCompress : TThreadBoolean;
    FDeflateSize: QWord;
    {$IFDEF ALLOW_STREAM_GZIP}
    FGzipSize: QWord;
    FGzipCache : TRefMemoryStream;
    {$ENDIF}
    FSize : QWord;
    FLoc, FURI : String;
    FMimeType : String;
    FCharset :  TThreadUtf8String;
    FCacheControl : TThreadUtf8String;
    function GetCache: TRefMemoryStream;
    function GetCacheControl : String;
    function GetCharset : String;
    function GetDeflateCache: TRefMemoryStream;
    function GetDeflateReady: Boolean;
    function GetDeflateSize: QWord;
    {$IFDEF ALLOW_STREAM_GZIP}
    function GetGzipCache: TRefMemoryStream;
    function GetGzipReady: Boolean;
    function GetGzipSize: QWord;
    {$ENDIF}
    function GetMimeType: String;
    function GetSize: QWord;
    procedure UpdateFromTemplate;
  public
    constructor Create(const aLoc, aURI : String);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    property Cache : TRefMemoryStream read GetCache;
    property Size : QWord read GetSize;
    property DeflateCache : TRefMemoryStream read GetDeflateCache;
    property DeflateSize : QWord read GetDeflateSize;
    property DeflateReady : Boolean read GetDeflateReady;
    {$IFDEF ALLOW_STREAM_GZIP}
    property GzipCache : TRefMemoryStream read GetGzipCache;
    property GzipSize : QWord read GetGzipSize;
    property GzipReady : Boolean read GetGzipReady;
    {$ENDIF}
    property MimeType : String read GetMimeType;
    property Charset : String read GetCharset;
    property CacheControl : String read GetCacheControl;
  end;


  { TWebCacheCollection }

  TWebCacheCollection = class(TThreadSafeFastCollection)
  private
    FHash : TStringHashList;
    function GetCache(const index : String): TWebCachedItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddNew(const aName: String; aData: TWebCachedItem);
    property CacheItem[const index : String] : TWebCachedItem read GetCache; default;
  end;

  TWebHashState = record
    SavedState : String;
    HashValue  : Cardinal;
  end;

  TWebClientState = Byte;

  { TWebThreadSafeCacheStates }

  TWebThreadSafeCacheStates = class(TNetCustomLockedObject)
  private
    FCurState : Array of TWebHashState;
    function GetStateHash(index : TWebClientState): Cardinal;
    function GetStateValue(index : TWebClientState): String;
    procedure SetStateHash(index : TWebClientState; AValue: Cardinal);
    procedure SetStateValue(index : TWebClientState; const AValue: String);
  public
    procedure SetStatesLength(Len : integer);
    property Hash[index : TWebClientState] : Cardinal read GetStateHash
                                                 write SetStateHash;
    property Value[index : TWebClientState] : String read GetStateValue
                                                 write SetStateValue;
  end;

  { TWebClient }

  TWebClient = class(TNetReferencedObject)
  private
    FCUID : String;
    FCurStates : TWebThreadSafeCacheStates;
    FOwner : TWebClients;
    FAcceptGZip : TThreadBoolean;
    FAcceptDeflate : TThreadBoolean;
    FHasSynConnection : TThreadBoolean;
    FOnRemove : TNotifyEvent;
    FStartStamp : QWord;
    FScore : TThreadInteger;
    FLastConnection : TThreadInteger;
    function GetAcceptGZip: Boolean;
    function GetLastConnection: Integer;
    function GetScore: Integer;
    function GetStateHash(index : TWebClientState): Cardinal;
    procedure SetAcceptGZip(AValue: Boolean);
    function GetAcceptDeflate: Boolean;
    procedure SetAcceptDeflate(AValue: Boolean);
    function GetHasSynConnection: Boolean;
    procedure SetHasSynConnection(AValue: Boolean);
    procedure SetScore(AValue: Integer);
  protected
    function GenerateNewStateHash : Cardinal;
    property CurStates : TWebThreadSafeCacheStates read FCurStates;
  public
    constructor Create(AOwner : TWebClients; const aCUID : String); virtual;
    destructor Destroy; override;
    procedure DoIdle; virtual;
    procedure UpdateScore; virtual;
    procedure RelaxScore;  virtual;
    procedure Initialize;  virtual;
    function SaveNewState(aState: TWebClientState; const aNewState: String;
      oldHash: Cardinal; ignoreHash: boolean=false): Boolean;
    procedure ResponseString(AResponse : TResponse; const S : String); virtual;
    procedure ResponseStream(AResponse : TResponse; Str : TStream; StrSize : Int64;
      OwnStream : Boolean); virtual;
    property CUID : String read FCUID;
    property StateHash[index : TWebClientState] : Cardinal read GetStateHash;
    property AcceptGzip : Boolean read GetAcceptGZip write SetAcceptGZip;
    property AcceptDeflate : Boolean read GetAcceptDeflate write SetAcceptDeflate;
    property HasSynConnection : Boolean read GetHasSynConnection write SetHasSynConnection;
    property OnRemove : TNotifyEvent read FOnRemove write FOnRemove;
    property LastConnectionInSec : Integer read GetLastConnection;
    property Score : Integer read GetScore write SetScore;
  end;

  TWebClientsContainer = class;

  { TWebClients }

  TWebClients = class(TThreadSafeFastSeq)
  private
    FContainer : TWebClientsContainer;
    FHash : TStringHashList;
    function  GetClient(const cUID : String): TWebClient;
    function  IsClientDead(aClient : TObject; {%H-}data : pointer) : Boolean;
    procedure AfterClientExtracted(aObj : TObject);
    procedure IdleClient(aObj : TObject);
  public
    constructor Create(aContainer : TWebClientsContainer);
    destructor Destroy; override;
    procedure Clear;
    procedure AddNew(const cUID: String; aClient: TWebClient);
    procedure RemoveClient(aClient : TWebClient); overload;
    procedure RemoveClient(const cUID: String); overload;
    procedure RemoveClientNotDestroy(const cUID: String);
    procedure ClearDeadClients;
    procedure IdleLiveClients;
    property  Client[const cUID : String] : TWebClient read GetClient; default;
    property  Container : TWebClientsContainer read FContainer;
  end;


  { TWebClientsContainer }

  TWebClientsContainer = class(TNetCustomLockedObject)
  private
    FSessions : TSqliteSessionFactory;
    FCachedPages : TWebCacheCollection;
    FClientsDB : TExtSqlite3Dataset;
    PREP_AddClientToBase,
    PREP_ClientStop,
    PREP_ClientSetLastCUID,
    PREP_ClientGetLastCUID,
    PREP_OnCreateSession,
    PREP_DeleteOldNetRequests : TSqlite3Prepared;
    //
    FCurCID : TThreadSafeAutoIncrementCardinal;
    FConnectedClients : TWebClients;
    procedure ClientRemove(Sender : TObject);
    function OnGenSessionID({%H-}aSession : TSqliteWebSession) : String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ClearCache;
    function CreateSession(ARequest : TWCRequest) : TSqliteWebSession;
    //
    procedure OnCreateNewSession(Sender : TObject);
    function  AddClient(ARequest : TRequest; const ClientID : String): TWebClient;
    function  GetClient(const ClientID : String) : TWebClient;
    procedure RemoveClient(const ClientID : String); overload;
    procedure RemoveClient(mClient : TWebClient); overload;
    procedure ClearDeadClients;
    procedure DoMaintainingStep;
    //
    function GetWebCachedItem(const aURI : String) : TWebCachedItem;
    procedure UpdateCachedWebItemsWithTemplates;
    //
    property  Sessions : TSqliteSessionFactory read FSessions;
    property  Clients  : TWebClients read FConnectedClients;
  end;

  { TWCResponse }

  TWCResponse = class (TAbsHTTPConnectionResponse)
  private
    FKeepAlive : boolean;
    FRefStream : TReferencedStream;
    function  GetConnection : TWCConnection;
    procedure SetRefStream(AValue: TReferencedStream);
  public
    constructor Create(ARequest : TWCRequest); overload;
    destructor Destroy; override;
    procedure SendUTf8String(const S: String);
    procedure CloseStream;
    procedure ReleaseRefStream;
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
    property  WCConn : TWCConnection read GetConnection;
    property  RefStream : TReferencedStream read FRefStream write SetRefStream;
    property  KeepStreamAlive : Boolean read FKeepAlive write FKeepAlive;
  end;

  { TWCRequest }

  TWCRequest = class (TAbsHTTPConnectionRequest)
  private
    function GetSocket: TSocketStream;
    function GetConnection : TWCConnection;
  protected
    procedure DecodeContent;
  public
    procedure CollectHeaders(Headers : TStringList);
    property  WCConn : TWCConnection read GetConnection;
    property  Socket : TSocketStream read GetSocket;
  end;

  { TWCHttpRefSendDataJob }

  TWCHttpRefSendDataJob = class(TLinearJob)
  private
    FConnection : TWCHTTPRefConnection;
    {$ifdef DEBUG_STAT}
    FDStartStamp : QWord;
    {$endif}
  public
    constructor Create(aConnection : TWCHTTPRefConnection);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TWCSendServerFile }

  TWCSendServerFile = class(TWCMainClientJob)
  public
    procedure Execute; override;
  end;

  EPipeException = class(ESocketError)
  end;

var
  WebContainer : TWebClientsContainer;

const
{$ifdef unix}
cSysDelimiter = '/';
cNonSysDelimiter = '\';
{$else}
cSysDelimiter = '\';
cNonSysDelimiter = '/';
{$endif}

Var
  Application : TWCHTTPApplication;
  ShowCleanUpErrors : Boolean = False;

Function ESSocketAddrToString(ASocketAddr: TSockAddr): String;
function ESGetGetRemoteAddress(Handle : Cardinal): sockets.TSockAddr;

const
  cSgzip = 'gzip';
  cSdeflate = 'deflate';

  WC_MAX_MAIN_THREADS = 32;
  WC_MAX_PREP_THREADS = 32;

  WC_CONSUME_OK              = 0;
  WC_CONSUME_PROTOCOL_ERROR  = 1;
  WC_CONSUME_NO_DATA         = 2;
  WC_CONSUME_WRONG_PROTOCOL  = 3;
  WC_CONSUME_SOCKET_ERROR    = 4;

{$I wcappconfig.inc}

Implementation

uses  CustApp, extopensslsockets, wcutils, math;
const WCSocketReadError = 'Socket read error';

function ParseStartLine(Request : TWCRequest; AStartLine : String) : Boolean;

Function GetNextWord(Var S : String) : string;
Var
  P : Integer;
begin
  P:=Pos(' ',S);
  If (P=0) then
    P:=Length(S)+1;
  Result:=Copy(S,1,P-1);
  Delete(S,1,P);
end;

Var
  S : String;
  I : Integer;

begin
  if aStartLine='' then
    exit(false);
  Result := True;
  Request.Method:=GetNextWord(AStartLine);
  Request.URL:=GetNextWord(AStartLine);
  S:=Request.URL;
  I:=Pos('?',S);
  if (I>0) then
    S:=Copy(S,1,I-1);
  If (Length(S)>1) and (S[1]<>'/') then
    S:='/'+S
  else if S='/' then
    S:='';
  Request.PathInfo:=S;
  S:=GetNextWord(AStartLine);
  If (S<>'') and (Pos('HTTP/',S)<>1) then
    Exit(false);
  Delete(S,1,5);
  Request.ProtocolVersion:=trim(S);
end;

{$IFDEF UNIX}
Procedure SignalHandler(SigNo: cint); cdecl;
Begin
    If SigNo = SIGPIPE Then
     raise EPipeException.Create('SIGPIPE catched');
End;
{$ENDIF}

Procedure InitHTTP;
begin
  {$IFDEF UNIX}
  fpSignal(SIGPIPE, @SignalHandler);
  {$ENDIF}
  CFG_CONFIGURATION := WC_CFG_CONFIGURATION;
  Application:=TWCHTTPApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneHTTP;
begin
  if CustomApplication=Application then
    CustomApplication := nil;
  try
    Application.Free;
    Application := nil;
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

{ TWCHTTPMimeTemplates }

function TWCHTTPMimeTemplates.GetTemplateInd(index : Integer): TWCHTTPTemplate;
begin
  Result := TWCHTTPTemplate(Self.Item[index]);
end;

function TWCHTTPMimeTemplatesCompare(obj1: TObject; obj2 : TObject) : Integer;
begin
  Result := Integer(CompareValue(TWCHTTPTemplate(obj1).Priority,
                                 TWCHTTPTemplate(obj2).Priority));
end;

procedure TWCHTTPMimeTemplates.SortTemplates;
begin
  SortList(@TWCHTTPMimeTemplatesCompare);
end;

procedure TWCHTTPMimeTemplates.Rebuild(aTemplates: TJSONArray);
var i : integer;
    d : TJSONData;
begin
  if not assigned(aTemplates) then Exit;
  Lock;
  try
    Clear;
    for i := 0 to aTemplates.Count-1 do
    begin
      d := aTemplates[i];
      if d is TJSONObject then
      begin
        Add(TWCHTTPTemplate.Create(TJSONObject(d)));
      end;
    end;
    SortTemplates;
  finally
    UnLock;
  end;
end;

function TWCHTTPMimeTemplates.GetTemplate(const aMime, aURI: String
  ): TWCHTTPTemplateRecord;
var i : integer;
    T : TWCHTTPTemplate;
begin
  Result.Cache := 'no-cache';
  Result.Compress := false;
  Result.Charset := '';
  Lock;
  try
    for i := 0 to Count-1 do
    begin
      T := Template[i];
      if T.Check(aMime, aURI) then
      begin
        T.GetStatus(Result);
      end;
    end;
  finally
    UnLock;
  end;
end;

{ TWCHTTPTemplate }

constructor TWCHTTPTemplate.Create(obj: TJSONObject);
begin
  FComplete := false;
  FPriority := 0;
  FCache:= nil;
  FCharset := nil;
  FCompress:=nil;
  FMimeRegExp := nil;
  FURIRegExp := nil;
  Rebuild(obj);
end;

procedure TWCHTTPTemplate.Rebuild(obj: TJSONObject);
var d : TJSONData;
    S : String;
begin
  if assigned(FCache) then begin StrDispose(FCache); FCache := nil; end;
  if assigned(FCharset) then begin StrDispose(FCharset); FCharset := nil; end;
  if assigned(FCompress) then FreeMemAndNil(FCompress);
  if assigned(FMimeRegExp) then FreeAndNil(FMimeRegExp);
  if assigned(FURIRegExp) then FreeAndNil(FURIRegExp);

  try
    FComplete := true;
    d := obj.Find('mime');
    if assigned(d) and (d is TJSONString) then
    begin
      FMimeRegExp := TRegExpr.Create(d.AsString);
    end;
    d := obj.Find('uri');
    if assigned(d) and (d is TJSONString) then
    begin
      FURIRegExp := TRegExpr.Create(d.AsString);
    end;
    if not (assigned(FMimeRegExp) or assigned(FURIRegExp)) then
      FComplete := false;
    if FComplete then
    begin
      d := obj.Find('prior');
      if assigned(d) and (d is TJSONNumber) then
        FPriority := d.AsInteger;
      d := obj.Find('cache');
      if assigned(d) and (d is TJSONString) then
      begin
        S := UTF8Encode(d.AsString);
        FCache := StrAlloc(Length(S) + 1);
        StrPCopy(FCache, S);
      end;
      d := obj.Find('charset');
      if assigned(d) and (d is TJSONString) then
      begin
        S := UTF8Encode(d.AsString);
        FCharset := StrAlloc(Length(S) + 1);
        StrPCopy(FCharset, S);
      end;
      d := obj.Find('compress');
      if assigned(d) and (d is TJSONBoolean) then
      begin
        FCompress:= GetMem(Sizeof(Boolean));
        FCompress^:=d.AsBoolean;
      end;
    end;
  except
    FComplete := false;
  end;
end;

destructor TWCHTTPTemplate.Destroy;
begin
  if assigned(FCache) then begin StrDispose(FCache); FCache := nil; end;
  if assigned(FCharset) then begin StrDispose(FCharset); FCharset := nil; end;
  if assigned(FCompress) then FreeMem(FCompress);
  if assigned(FMimeRegExp) then FMimeRegExp.Free;
  if assigned(FURIRegExp) then FURIRegExp.Free;
  inherited Destroy;
end;

function TWCHTTPTemplate.Check(const aMime, aURI : String) : Boolean;
begin
  if Complete then
  begin
    try
      Result := true;
      if assigned(FMimeRegExp) then
        Result := FMimeRegExp.Exec(aMime);
      if Result and assigned(FURIRegExp) then
        Result := FURIRegExp.Exec(aURI);
    except
      Result := false;
    end;
  end else Result := false;
end;

function TWCHTTPTemplate.GetCompress(lV: Boolean): Boolean;
begin
  if assigned(FCompress) then Result := FCompress^ else
                              Result := lV;
end;

procedure TWCHTTPTemplate.GetCache(var lV: String);
begin
  if assigned(FCache) then lV := StrPas(FCache);
end;

procedure TWCHTTPTemplate.GetCharset(var lV : String);
begin
  if assigned(FCharset) then lV := StrPas(FCharset);
end;

procedure TWCHTTPTemplate.GetStatus(var R: TWCHTTPTemplateRecord);
begin
  if assigned(FCompress) then R.Compress := FCompress^;
  if assigned(FCache) then    R.Cache := StrPas(FCache);
  if assigned(FCharset) then  R.Charset := StrPas(FCharset);
end;

{ TWCHTTPConfig }

procedure TWCHTTPConfig.DoConfigLoaded(aConfig: TJSONObject);
var d : TJSONData;
begin
  d := aConfig.FindPath(HashToConfig(CFG_WEBFILES_SEC)^.NAME_STR + '.' +
                        HashToConfig(CFG_MIME_TEMPLATES)^.NAME_STR);
  if assigned(d) and (d is TJSONArray) then begin
    FMimeTemplates.Rebuild(TJSONArray(d));
    if assigned(WebContainer) then
      WebContainer.UpdateCachedWebItemsWithTemplates;
  end;
end;

destructor TWCHTTPConfig.Destroy;
begin
  FMimeTemplates.Free;
  inherited Destroy;
end;

procedure TWCHTTPConfig.DoInitialize();
var MainSec, SSLSec, WFSec, ClientsSec, Http2Sec, MaintSec : TWCConfigRecord;
begin
  FMimeTemplates := TWCHTTPMimeTemplates.Create;
  OnConfigLoaded := @DoConfigLoaded;

  MainSec := Root.AddSection(HashToConfig(CFG_MAIN_SEC)^.NAME_STR);
  MainSec.AddValue(CFG_SITE_FOLDER, wccrString);
  MainSec.AddValue(CFG_SERVER_NAME, wccrString);
  MainSec.AddValue(CFG_MAIN_URI, wccrString);
  MainSec.AddValue(CFG_SESSIONS_LOC, wccrString);
  MainSec.AddValue(CFG_CLIENTS_DB, wccrString);
  MainSec.AddValue(CFG_LOG_DB, wccrString);
  MainSec.AddValue(CFG_MIME_NAME, wccrString);
  MainSec.AddValue(CFG_MAIN_THREAD_CNT, wccrInteger);
  MainSec.AddValue(CFG_PRE_THREAD_CNT, wccrInteger);
  MainSec.AddValue(CFG_JOB_TO_JOB_WAIT, wccrInteger);
  MainSec.AddValue(CFG_JOB_TO_JOB_WAIT_ADAPT_MIN, wccrInteger);
  MainSec.AddValue(CFG_JOB_TO_JOB_WAIT_ADAPT_MAX, wccrInteger);

  WFSec := Root.AddSection(HashToConfig(CFG_WEBFILES_SEC)^.NAME_STR);
  WFSec.AddValue(CFG_COMPRESS_LIMIT, wccrInteger);
  WFSec.AddValue(CFG_IGNORE_FILES, wccrString);
  WFSec.AddValue(CFG_EXCLUDE_IGNORE_FILES, wccrString);

  SSLSec := Root.AddSection(HashToConfig(CFG_OPENSSL_SEC)^.NAME_STR);
  SSLSec.AddValue(CFG_USE_SSL, wccrBoolean);
  SSLSec.AddValue(CFG_HOST_NAME, wccrString);
  SSLSec.AddValue(CFG_SSL_LOC, wccrString);
  SSLSec.AddValue(CFG_SSL_VER, wccrString);
  SSLSec.AddValue(CFG_SSL_CIPHER, wccrString);
  SSLSec.AddValue(CFG_PRIVATE_KEY, wccrString);
  SSLSec.AddValue(CFG_CERTIFICATE, wccrString);
  SSLSec.AddValue(CFG_TLSKEY_LOG, wccrString);
  SSLSec.AddValue(CFG_ALPN_USE_HTTP2, wccrBoolean);

  ClientsSec := Root.AddSection(HashToConfig(CFG_CLIENTS_SEC)^.NAME_STR);
  ClientsSec.AddValue(CFG_CLIENT_TIMEOUT, wccrInteger);
  ClientsSec.AddValue(CFG_CLIENT_COOKIE_MAX_AGE, wccrInteger);
  ClientsSec.AddValue(CFG_CLIENT_ALLOW_ENCODE, wccrString);

  Http2Sec := Root.AddSection(HashToConfig(CFG_HTTP2_SEC)^.NAME_STR);
  Http2Sec.AddValue(CFG_H2SET_HEADER_TABLE_SIZE     , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_ENABLE_PUSH           , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_CONCURRENT_STREAMS, wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_INITIAL_WINDOW_SIZE   , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_FRAME_SIZE        , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_HEADER_LIST_SIZE  , wccrInteger);

  MaintSec := Root.AddSection(HashToConfig(CFG_MAINTAIN_SEC)^.NAME_STR);
  MaintSec.AddValue(CFG_SHUTDOWN, False);
end;

{ TWCHttpRefSendDataJob }

constructor TWCHttpRefSendDataJob.Create(aConnection: TWCHTTPRefConnection);
begin
  inherited Create;
  FConnection := aConnection;
  FConnection.IncReference;
  {$ifdef DEBUG_STAT}
  FDStartStamp:= GetTickCount64;
  {$endif}
end;

destructor TWCHttpRefSendDataJob.Destroy;
{$ifdef DEBUG_STAT}
var DT: Integer;
{$endif}
begin
  FConnection.DecReference;
  {$ifdef DEBUG_STAT}
  DT := Int64(GetTickCount64) - Int64(FDStartStamp);
  if DT < 0 then DT := 0;
  if DEBUG_GLOBALS_LONGWORD[DG_WRITE_DELTA_TIME_MAX] < DT then DEBUG_GLOBALS_LONGWORD[DG_WRITE_DELTA_TIME_MAX] := DT;
  DEBUG_GLOBALS_QWORD[DG_WRITE_DELTA_TIME_SUM] := DEBUG_GLOBALS_QWORD[DG_WRITE_DELTA_TIME_SUM] + DT;
  Inc(DEBUG_GLOBALS_QWORD[DG_WRITE_DELTA_TIME_CNT]);
  DEBUG_GLOBALS_LONGWORD[DG_WRITE_DELTA_TIME_AV] := DEBUG_GLOBALS_QWORD[DG_WRITE_DELTA_TIME_SUM] div DEBUG_GLOBALS_QWORD[DG_WRITE_DELTA_TIME_CNT];
  {$endif}
  inherited Destroy;
end;

procedure TWCHttpRefSendDataJob.Execute;
begin
  try
    FConnection.SendFrames;
  except
    on e: Exception do FConnection.ConnectionState:= wcDROPPED;
  end;
end;

{ TWCSendServerFile }

procedure TWCSendServerFile.Execute;
var FE, aURI : String;
  aCachedFile : TWebCachedItem;
begin
  ResponseReadyToSend := false; // prevent to send response

  aURI := Request.PathInfo;
  if (aURI = '/') or (Length(aURI) = 0) then
  begin
    aURI := Application.MainURI;
  end;
  if (Pos(aURI, '..') = 0) and (Length(aURI) > 0) and
     Application.IsAcceptedWebFile(aURI) then
  begin
    FE := ExtractFileExt(aURI);
    if SameText(FE, '.svgz') then begin
      if Client.AcceptGzip then
        Response.SetHeader(hhContentEncoding, cSgzip) else
        aURI := ChangeFileExt(aURI, '.svg');
    end;

    aCachedFile := WebContainer.GetWebCachedItem(aURI);
    if assigned(aCachedFile) and
       (aCachedFile.Size > 0) then
    begin
      aCachedFile.Lock;
      try
        Response.ContentType := aCachedFile.MimeType;
        if Length(aCachedFile.Charset) > 0 then
          Response.ContentType := Response.ContentType + '; charset='+aCachedFile.Charset;
        Response.CacheControl:= aCachedFile.CacheControl;

        {$IFDEF ALLOW_STREAM_GZIP}
        if Client.AcceptGzip and aCachedFile.GzipReady then
        begin
          Response.ContentLength:=aCachedFile.GzipSize;
          Response.RefStream:=aCachedFile.GzipCache;
          Response.SetHeader(hhContentEncoding, cSgzip)
        end else
        {$ENDIF}
        if Client.AcceptDeflate and aCachedFile.DeflateReady then
        begin
          Response.ContentLength:=aCachedFile.DeflateSize;
          Response.RefStream:=aCachedFile.DeflateCache;
          Response.SetHeader(hhContentEncoding, cSdeflate)
        end else
        begin
          Response.ContentLength:=aCachedFile.Size;
          Response.RefStream:=aCachedFile.Cache;
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
    Application.SendError(Response, 404);
  end;
  // inherited Execute;
end;

{ TWCRequest }

function TWCRequest.GetSocket: TSocketStream;
begin
  Result := Connection.Socket;
end;

function TWCRequest.GetConnection: TWCConnection;
begin
  Result := TWCConnection(Connection);
end;

procedure TWCRequest.DecodeContent;
var aDecoder : TWCClientDecoderClass;
begin
  // check if compression applyed
  if Length(ContentEncoding) > 0 then
  begin
    aDecoder := Application.ClientDecoders[ContentEncoding];
    if assigned(aDecoder) then
    begin
      // maybe not best solution for large content blocks
      // Initial Content -> DecodedBytes -> Output Content
      // utf8 charset means as default on then client-side
      // need to parse Content-Type string to determinate
      // initial client-side charset and convert result string to utf8
      Content := aDecoder.DecodeString(Content);
    end;
  end;
end;

procedure TWCRequest.CollectHeaders(Headers: TStringList);
var H : THeader;
begin
 For H in THeader do
   if HeaderIsSet(H) then
     Headers.Add(HTTPHeaderNames[H]+': '+GetHeader(H));
end;

{ TWebThreadSafeCacheStates }

function TWebThreadSafeCacheStates.GetStateHash(index: TWebClientState
  ): Cardinal;
begin
  Lock;
  try
    Result := FCurState[index].HashValue;
  finally
    UnLock;
  end;
end;

function TWebThreadSafeCacheStates.GetStateValue(index: TWebClientState
  ): String;
begin
  Lock;
  try
    Result := FCurState[index].SavedState;
  finally
    UnLock;
  end;
end;

procedure TWebThreadSafeCacheStates.SetStateHash(index : TWebClientState;
  AValue: Cardinal);
begin
  Lock;
  try
    FCurState[index].HashValue := AValue;
  finally
    UnLock;
  end;
end;

procedure TWebThreadSafeCacheStates.SetStateValue(index: TWebClientState;
  const AValue: String);
begin
  Lock;
  try
    FCurState[index].SavedState := AValue;
  finally
    UnLock;
  end;
end;

procedure TWebThreadSafeCacheStates.SetStatesLength(Len: integer);
begin
  SetLength(FCurState, Len);
end;

{ TWCResponse }

function TWCResponse.GetConnection: TWCConnection;
begin
  Result := TWCConnection(Connection);
end;

procedure TWCResponse.SetRefStream(AValue: TReferencedStream);
begin
  if FRefStream=AValue then Exit;
  ReleaseRefStream;
  FRefStream:=AValue;
  FRefStream.IncReference;
end;

constructor TWCResponse.Create(ARequest: TWCRequest);
begin
  inherited Create(ARequest);
  FKeepAlive:= false;
  FRefStream := nil;
end;

destructor TWCResponse.Destroy;
begin
  ReleaseRefStream;
  inherited Destroy;
end;

procedure TWCResponse.SendUTf8String(const S: String);
var L, C : integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    if WCConn.HTTPVersion = wcHTTP2 then
    begin
      WCConn.HTTP2Str.Request.Response.PushData(Pointer(@(S[1])), L);
      WCConn.HTTP2Str.Request.Response.SerializeData(not KeepStreamAlive);
    end else
    begin
      if Assigned(WCConn.HTTPRefCon) then
      begin
        WCConn.HTTPRefCon.PushFrame(S);
      end else
      begin
        C := WCConn.Socket.Write(S[1], L);
        if C < 0 then
        begin
          // do nothing
          raise ESocketError.CreateFmt('Socket write error %d', [GetConnection.Socket.LastError]);
        end;
      end;
    end;
  end;
end;

procedure TWCResponse.CloseStream;
begin
  if WCConn.HTTPVersion = wcHTTP2 then
  begin
    WCConn.HTTP2Str.Request.Response.Close;
  end else
  begin
    // do nothing
  end;
end;

procedure TWCResponse.ReleaseRefStream;
begin
  if assigned(FRefStream) then begin
    FRefStream.DecReference;
    FRefStream := nil;
  end;
end;

procedure TWCResponse.DoSendHeaders(Headers: TStrings);
Var
  S : String;
  I : Integer;
begin
  if Assigned(WCConn.HTTP2Str) then
  begin
    WCConn.HTTP2Str.Request.
               Response.SerializeResponseHeaders(Self,
                                                       (ContentLength = 0) and
                                                       (not FKeepAlive));
  end else
  begin
    S:=Format('HTTP/1.1 %3d %s'#13#10,[Code,GetStatusCode(Code)]);
    For I:=0 to Headers.Count-1 do
      S:=S+Headers[i]+#13#10;
    // Last line in headers is empty.
    if Assigned(WCConn.HTTPRefCon) then
    begin
      WCConn.HTTPRefCon.PushFrame(S);
    end else begin
      WCConn.Socket.WriteBuffer(S[1],Length(S));
    end;
  end;
end;

procedure TWCResponse.DoSendContent;
begin
  if Assigned(WCConn.HTTP2Str) then
  begin
    if Assigned(FRefStream) then begin
      WCConn.HTTP2Str.Request.Response.SerializeRefStream(FRefStream, true); //close stream
      ReleaseRefStream;
    end
    else
      WCConn.HTTP2Str.Request.Response.SerializeResponseData(Self, true); //close stream
  end else
  begin
    if Assigned(WCConn.HTTPRefCon) then
    begin
      if Assigned(FRefStream) then
      begin
        WCConn.HTTPRefCon.PushFrame(FRefStream);
        ReleaseRefStream;
      end else
      begin
        If Assigned(ContentStream) then begin
          WCConn.HTTPRefCon.PushFrame(ContentStream, 0, FreeContentStream);
          FreeContentStream := false;
        end
        else
          WCConn.HTTPRefCon.PushFrame(Contents);
      end;
    end else begin
      If Assigned(ContentStream) then
        WCConn.Socket.CopyFrom(ContentStream,0)
      else
        Contents.SaveToStream(WCConn.Socket);
    end;
  end;
end;

{ TWCConnection }

procedure TWCConnection.DoInitialize;
begin
  FProtocolVersion := wcUNK;
  FInputBuf := GetMem(WC_INITIAL_READ_BUFFER_SIZE);
  FInput  := TBufferedStream.Create;
  FInput.SetPointer(FInputBuf, WC_INITIAL_READ_BUFFER_SIZE);
  FRequest := nil;
  FResponse := nil;
  FClient := nil;
  FSession := nil;
  {$ifdef DEBUG_STAT}
  FDStartStamp := GetTickCount64;
  FDWaitStamp := FDStartStamp;
  FDStopStamp := FDStartStamp;
  FDReadStamp := FDStartStamp;
  {$endif}
end;

function TWCConnection.ReadLine: String;
var aBuffer : AnsiString;
    OFFSET : Integer;

Procedure FillBuffer;
Var
  R : Integer;
begin
  R := 512;
  if (FInput.Size - OFFSET) < R then R := FInput.Size - OFFSET;
  SetLength(aBuffer,R);
  FInput.Read(aBuffer[1], R);
end;

Var
CheckLF,Done : Boolean;
P,L: integer;
begin
  aBuffer := '';
  Result:='';
  Done:=False;
  CheckLF:=False;
  OFFSET := FInput.Position;
  Repeat
    if Length(aBuffer)=0 then
      FillBuffer;
    if Length(aBuffer)=0 then
      Done:=True
    else if CheckLF then
    begin
      If (aBuffer[1]<>#10) then
        Result:=Result+#13
      else
      begin
        Delete(aBuffer,1,1);
        Done:=True;
      end;
      CheckLF:=False;
    end;
    if not Done then
    begin
      P:=Pos(#13#10,aBuffer);
      If P=0 then
      begin
        L:=Length(aBuffer);
        CheckLF:=aBuffer[L]=#13;
        if CheckLF then
          Result:=Result+Copy(aBuffer,1,L-1)
        else
          Result:=Result+aBuffer;
        aBuffer:='';
      end
      else
      begin
        Result:=Result+Copy(aBuffer,1,P-1);
        Delete(aBuffer,1,P+1);
        Done:=True;
      end;
    end;
  until Done;
  FInput.Position:= FInput.Position - Length(aBuffer);
end;

function TWCConnection.ReadReqHeaders: TWCRequest;
Var
  S, StartLine : String;
begin
  Result:=TWCRequest(TWCHttpServer(Server).CreateRequest);
  try
    TWCHttpServer(Server).InitRequest(Result);
    Result.SetConnection(Self);
    StartLine := ReadLine;
    if ParseStartLine(Result,StartLine) then
    begin
      Repeat
        S := ReadLine;
        if (S<>'') then
          ConsumeHeader(Result, S);
      Until (S='');
      Result.RemoteAddress := ESSocketAddrToString(Socket.RemoteAddress);
      Result.ServerPort := TWCHttpServer(Server).Port;
    end else
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TWCConnection.ConvertFromHTTP2Req(AReq2: TWCHTTP2Request): TWCRequest;
begin
  if not assigned(AReq2) then Exit(nil);
  Result:=TWCRequest(TWCHttpServer(Server).CreateRequest);
  try
    TWCHttpServer(Server).InitRequest(Result);
    Result.SetConnection(Self);
    AReq2.CopyToHTTP1Request(Result);
    Result.InitRequestVars;
    Result.RemoteAddress := ESSocketAddrToString(Socket.RemoteAddress);
    Result.ServerPort := TWCHttpServer(Server).Port;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TWCConnection.ReadReqContent(ARequest: TWCRequest);
Var
  P,L,R : integer;
  S : String;
begin
  S:='';
  L:=ARequest.ContentLength;
  If (L>0) then
  begin
    SetLength(S,L);
    P:=(FInput.Size - FInput.Position);
    if (P>0) then
    begin
      FInput.Read(S[1],P);
      L:=L-P;
    end;
    P:=P+1;
    R:=1;
    While (L<>0) and (R>0) do
    begin
      R:=Socket.Read(S[p],L);
      If R<0 then
        Raise ESocketError.Create(WCSocketReadError);
      if (R>0) then
      begin
        P:=P+R;
        L:=L-R;
      end;
    end;
  end;
  ARequest.InitContent(S);
end;

procedure TWCConnection.ConsumeHeader(ARequest: TRequest;
  AHeader: String);
Var
  P : Integer;
  N,V : String;
begin
  V:=AHeader;
  P:=Pos(':',V);
  if (P=0) then
  begin
    UnknownHeader(ARequest,Aheader);
    Exit;
  end;
  N:=Copy(V,1,P-1);
  Delete(V,1,P);
  V:=Trim(V);
  ARequest.SetFieldByName(N,V);
end;

procedure TWCConnection.UnknownHeader({%H-}ARequest: TRequest;
  const {%H-}AHeader: String);
begin
  // do nothing
end;

procedure TWCConnection.DoSocketAttach(ASocket: TSocketStream);
begin
  inherited DoSocketAttach(ASocket);
  Application.SocketsCollector.Add(SocketReference);
end;

constructor TWCConnection.Create(AServer: TAbsCustomHTTPServer;
  ASocket: TSocketStream);
begin
  inherited Create(AServer, ASocket);
  DoInitialize;
end;

constructor TWCConnection.CreateRefered(AServer: TAbsCustomHTTPServer;
  ASocketRef: TWCHTTPSocketReference);
begin
  inherited CreateRefered(AServer, ASocketRef);
  DoInitialize;
end;

function TWCConnection.ConsumeSocketData: Cardinal;
var r : integer;
    aHTTPRefCon : TWCHTTPRefConnection;
    h2openmode : THTTP2OpenMode;
begin
  Result := WC_CONSUME_OK;
  try
    aHTTPRefCon := TWCHttpServer(Server).HTTPRefConnections.GetByHandle(Socket.Handle);
    // if HTTPRefCon not nil, then reference to httprefcon automatically incremented
    // NOT need To increment here
    if Assigned(aHTTPRefCon) then
    begin
      HTTPRefCon := aHTTPRefCon;
      FProtocolVersion := aHTTPRefCon.Protocol;
    end;

    try
      r:=SocketReference.Read(FInputBuf^, WC_INITIAL_READ_BUFFER_SIZE);
      If r < 0 then begin
        Result := WC_CONSUME_SOCKET_ERROR;
        Raise ESocketError.Create(WCSocketReadError);
      end;
      FInput.SetPointer(FInputBuf, r);  //resize buffered stream

      if FInput.Size > 0 then
      begin
        if FProtocolVersion = wcUNK then
        begin
          FProtocolVersion := TWCHTTP2Connection.CheckProtocolVersion(FInput.Memory, FInput.Size);
          if FProtocolVersion = wcHTTP2 then begin
             h2openmode := h2oPrefaceMode;
             FInput.Position:= H2P_PREFACE_SIZE;
          end else begin
             FInput.Position:= 0;
          end;
        end;
        if FProtocolVersion in [wcHTTP1, wcHTTP1_1] then
        begin
          // Request headers and content reading on one round
          // Read headers.
          FRequest:= ReadReqHeaders;
          if Assigned(FRequest) then
          begin
            // Read content, if any
            If FRequest.ContentLength>0 then begin
              ReadReqContent(FRequest);
              FRequest.DecodeContent;
            end;
            FRequest.InitRequestVars;
            //check here if http1.1 upgrade to http2
            //here can be implemented simple mechanism for transitioning
            //from HTTP/1.1 to HTTP/2 according RFC 7540 (Section 3.2)
            //
            //this mechanism is not implemented due to its rare use
            if not Assigned(HTTPRefCon) then
            begin
              if SameText(FRequest.ProtocolVersion, '1.1') and
                 SameText(FRequest.GetHeader(hhConnection), 'keep-alive') then
              begin
                HTTPRefCon := TWCHttpServer(Server).AttachNewHTTP11Con(SocketReference,
                                                                       @(TWCHttpServer(Server).DoConnectToSocketRef),
                                                                       @(TWCHttpServer(Server).DoSendData));
                HTTPRefCon.IncReference; // reference not incremented here, need to increment
              end;
            end;
            if FProtocolVersion in [wcHTTP1, wcHTTP1_1] then
              Result := WC_CONSUME_OK else
              Result := WC_CONSUME_WRONG_PROTOCOL;
          end else begin
            FProtocolVersion:= wcUNK;
            Result := WC_CONSUME_WRONG_PROTOCOL;
          end;
        end else Result := WC_CONSUME_WRONG_PROTOCOL;
      end else Result := WC_CONSUME_NO_DATA;
      if FProtocolVersion = wcHTTP2 then
      begin
        // read http/2 frames
        // RFC 7540
        // consume socket data, pop new request
        Result := WC_CONSUME_OK;
        if not Assigned(HTTPRefCon) then
        begin
          HTTPRefCon := TWCHttpServer(Server).AttachNewHTTP2Con(SocketReference,
                                                                h2openmode,
                                                                @(TWCHttpServer(Server).DoConnectToSocketRef),
                                                                @(TWCHttpServer(Server).DoSendData));
          HTTPRefCon.IncReference; // reference not incremented here, need to increment
        end;
        if FInput.Size > 0 then
           HTTPRefCon.ConsumeNextFrame(FInput);
        HTTP2Str := TWCHTTP2Connection(HTTPRefCon).PopRequestedStream;
        if Assigned(HTTP2Str) then begin
          FRequest := ConvertFromHTTP2Req(HTTP2Str.Request);
          // check Malformed Requests
          if FRequest.ContentLength <> HTTP2Str.Request.DataBlockSize then
          begin
            TWCHTTP2Connection(HTTPRefCon).ResetStream(HTTP2Str.ID,
                                                       H2E_PROTOCOL_ERROR);
            TWCHTTP2Connection(HTTPRefCon).GoAway(H2E_PROTOCOL_ERROR);
            Result := WC_CONSUME_PROTOCOL_ERROR;
          end;
          FRequest.DecodeContent;
        end else
          Result := WC_CONSUME_NO_DATA;
      end;
    finally
      if Assigned(HTTPRefCon) then
         HTTPRefCon.ReleaseRead(not (Result in [WC_CONSUME_PROTOCOL_ERROR,
                                                WC_CONSUME_WRONG_PROTOCOL,
                                                WC_CONSUME_SOCKET_ERROR]));
    end;
    if Result = WC_CONSUME_OK then
    begin
      // Create Response
      FResponse:= TWCResponse(TWCHttpServer(Server).CreateResponse(FRequest));
      TWCHttpServer(Server).InitResponse(FResponse);
      FResponse.SetConnection(Self);
    end;
  Except
    On E : Exception do begin
      if Result in [WC_CONSUME_NO_DATA, WC_CONSUME_OK] then
        Result := WC_CONSUME_PROTOCOL_ERROR;
      if Assigned(HTTPRefCon) then HTTPRefCon.ConnectionState:=wcDROPPED;
      if Assigned(FRequest) then FreeAndNil(FRequest);
      if Assigned(FResponse) then FreeAndNil(FResponse);
      HandleRequestError(E);
    end;
  end;
end;

procedure TWCConnection.SetSessionParams(aClient: TWebClient;
  aSession: TSqliteWebSession);
begin
  FClient := aClient;
  FClient.IncReference;
  FSession := aSession;
end;

destructor TWCConnection.Destroy;
{$ifdef DEBUG_STAT}
var DT : Integer;
{$endif}
begin
  if Assigned(FRequest) then FreeAndNil(FRequest);
  if Assigned(FResponse) then FreeAndNil(FResponse);
  if Assigned(FSession) then FreeAndNil(FSession);
  if Assigned(FClient) then FClient.DecReference;
  FInput.Free;
  FreeMem(FInputBuf);

  {$ifdef DEBUG_STAT}
  FDStopStamp := GetTickCount64;
  DT := Int64(FDStopStamp) - Int64(FDReadStamp);
  if DT < 0 then DT := 0;
  if DEBUG_GLOBALS_LONGWORD[DG_WORK_DELTA_TIME_MAX] < DT then DEBUG_GLOBALS_LONGWORD[DG_WORK_DELTA_TIME_MAX] := DT;
  DEBUG_GLOBALS_QWORD[DG_WORK_DELTA_TIME_SUM] := DEBUG_GLOBALS_QWORD[DG_WORK_DELTA_TIME_SUM] + DT;
  Inc(DEBUG_GLOBALS_QWORD[DG_WORK_DELTA_TIME_CNT]);
  DEBUG_GLOBALS_LONGWORD[DG_WORK_DELTA_TIME_AV] := DEBUG_GLOBALS_QWORD[DG_WORK_DELTA_TIME_SUM] div DEBUG_GLOBALS_QWORD[DG_WORK_DELTA_TIME_CNT];
  DT := Int64(FDReadStamp) - Int64(FDWaitStamp);
  if DT < 0 then DT := 0;
  if DEBUG_GLOBALS_LONGWORD[DG_READ_DELTA_TIME_MAX] < DT then DEBUG_GLOBALS_LONGWORD[DG_READ_DELTA_TIME_MAX] := DT;
  DEBUG_GLOBALS_QWORD[DG_READ_DELTA_TIME_SUM] := DEBUG_GLOBALS_QWORD[DG_READ_DELTA_TIME_SUM] + DT;
  Inc(DEBUG_GLOBALS_QWORD[DG_READ_DELTA_TIME_CNT]);
  DEBUG_GLOBALS_LONGWORD[DG_READ_DELTA_TIME_AV] := DEBUG_GLOBALS_QWORD[DG_READ_DELTA_TIME_SUM] div DEBUG_GLOBALS_QWORD[DG_READ_DELTA_TIME_CNT];
  DT := Int64(FDWaitStamp) - Int64(FDStartStamp);
  if DT < 0 then DT := 0;
  if DEBUG_GLOBALS_LONGWORD[DG_WAIT_DELTA_TIME_MAX] < DT then DEBUG_GLOBALS_LONGWORD[DG_WAIT_DELTA_TIME_MAX] := DT;
  DEBUG_GLOBALS_QWORD[DG_WAIT_DELTA_TIME_SUM] := DEBUG_GLOBALS_QWORD[DG_WAIT_DELTA_TIME_SUM] + DT;
  Inc(DEBUG_GLOBALS_QWORD[DG_WAIT_DELTA_TIME_CNT]);
  DEBUG_GLOBALS_LONGWORD[DG_WAIT_DELTA_TIME_AV] := DEBUG_GLOBALS_QWORD[DG_WAIT_DELTA_TIME_SUM] div DEBUG_GLOBALS_QWORD[DG_WAIT_DELTA_TIME_CNT];
  {$endif}

  inherited Destroy;
end;

{ TWCPreAnalizeClientJob }

function TWCPreAnalizeClientJob.GetRequest: TWCRequest;
begin
  Result := Connection.Request;
end;

function TWCPreAnalizeClientJob.GetResponse: TWCResponse;
begin
  Result := Connection.Response;
end;

constructor TWCPreAnalizeClientJob.Create(aConn: TWCConnection);
begin
  FConn := aConn;
end;

destructor TWCPreAnalizeClientJob.Destroy;
begin
  if assigned(FConn) then begin
    FConn.Free;
  end;
  inherited Destroy;
end;

procedure TWCPreAnalizeClientJob.Execute;
var ASynThread : TWCMainClientJob;
    aClient : TWebClient;
    aSession : TSqliteWebSession;
    aConsumeResult : Cardinal;
begin
  try
     {$ifdef DEBUG_STAT}
     FConn.FDWaitStamp := GetTickCount64;
     {$endif}
     if not (Assigned(FConn) and TWCHttpServer(FConn.Server).ServerActive) then
       Exit;
     aConsumeResult := FConn.ConsumeSocketData;
     if aConsumeResult = WC_CONSUME_OK then begin
       aSession := WebContainer.CreateSession(Request);
       if Assigned(aSession) then
       begin
         aSession.InitSession(Request, @(WebContainer.OnCreateNewSession), nil);
         if ssNew in aSession.SessionState then
           aSession.InitResponse(Response); // fill cookies
         if ssExpired in aSession.SessionState then
         begin
           Application.SendError(Response, 205);
           Exit;
         end else
         begin
           //try to find client
           //or
           //if new session then try to create client in clients pool
           //using ARequest to deteminate some additional data
           aClient := WebContainer.AddClient(Request, aSession.SessionID);
           if not assigned(aClient) then begin
             Application.SendError(Response, 405);
             Exit;
           end else begin
             aClient.Initialize;
           end;
         end;
         FConn.SetSessionParams(aClient, aSession);
       end else aClient := nil;
       {$ifdef DEBUG_STAT}
       FConn.FDReadStamp := GetTickCount64;
       {$endif}
       //
       if assigned(aClient) then begin
         ASynThread := GenerateClientJob;
         if Assigned(ASynThread) then
         begin
           FConn := nil; //now fconn is part of ASynThread job
           Application.ESServer.AddToMainPool(ASynThread);
         end;
       end;
     end
     else
     begin
       {$ifdef DEBUG_STAT}
       if aConsumeResult <> WC_CONSUME_NO_DATA then begin
         Inc(DEBUG_GLOBALS_LONGWORD[DG_FAILED_PREP_CNT]);
       end;
       {$endif}
     end;
  except
    on E: Exception do ; // catch errors. jail them in thread
  end;
end;

function TWCPreAnalizeClientJob.GenerateClientJob: TWCMainClientJob;
begin
  Result := nil;
end;

{ TWCMainClientJob }

function TWCMainClientJob.GetClient: TWebClient;
begin
  Result := Connection.Client;
end;

function TWCMainClientJob.GetRequest: TWCRequest;
begin
  Result := Connection.Request;
end;

function TWCMainClientJob.GetResponse: TWCResponse;
begin
  Result := Connection.Response;
end;

constructor TWCMainClientJob.Create(aConn: TWCConnection);
begin
  inherited Create(aConn.Client.Score);
  aConn.Client.UpdateScore;
  FConn := aConn;
  FResponseReadyToSend := true;
end;

destructor TWCMainClientJob.Destroy;
begin
  if Assigned(FConn) then begin
    FConn.Free;
    FConn := nil;
  end;
  inherited Destroy;
end;

procedure TWCMainClientJob.Execute;
begin
  // do something in descendant class
  if ResponseReadyToSend then
    Connection.Response.SendContent;
end;

procedure TWCMainClientJob.ReleaseConnection;
begin
  FConn := nil; // clear reference to connection
end;

{ TWCHttpServer }

function TWCHttpServer.CompareMainJobs(Tree: TAvgLvlTree; Data1, Data2: Pointer
  ): Integer;
begin
  Result := CompareValue(TWCMainClientJob(Data1).Score,
                         TWCMainClientJob(Data2).Score);
end;

constructor TWCHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadPool := nil;
  FPoolsLocker := TNetCustomLockedObject.Create;
  FSSLLocker := TNetCustomLockedObject.Create;
  FHTTPRefConnections := TWCHTTP2ServerRefConnections.Create(nil);
end;

function TWCHttpServer.CreateSSLSocketHandler: TSocketHandler;
begin
  FSSLLocker.Lock;
  try
    Result:=inherited CreateSSLSocketHandler;
    if assigned(Result) then
    begin
      TExtOpenSSLSocketHandler(Result).AlpnList := AlpnList.Text;
      TExtOpenSSLSocketHandler(Result).SSLMasterKeyLog:= SSLMasterKeyLog;
    end;
  finally
    FSSLLocker.UnLock;
  end;
end;

procedure TWCHttpServer.CreateConnectionThread(Conn: TAbsHTTPConnection);
var PreJob : TWCPreAnalizeClientJob;
begin
  if assigned(Application.ServerAnalizeJobClass) then
     PreJob := Application.ServerAnalizeJobClass.Create(TWCConnection(Conn))
  else
     PreJob := TWCPreAnalizeClientJob.Create(TWCConnection(Conn));

  CheckThreadPool;

  AddLinearJob(PreJob);
end;

function TWCHttpServer.CreateConnection(Data: TSocketStream): TAbsHTTPConnection;
begin
  Result:= TWCConnection.Create(Self, Data);
  Data.IOTimeout := 10000;
end;

function TWCHttpServer.CreateRequest: TAbsHTTPConnectionRequest;
begin
  Result:=TWCRequest.Create;
end;

function TWCHttpServer.CreateResponse(ARequest: TAbsHTTPConnectionRequest
  ): TAbsHTTPConnectionResponse;
begin
  Result:=TWCResponse.Create(ARequest);
  TWCResponse(Result).KeepStreamAlive:=false;
end;

procedure TWCHttpServer.InitRequest(ARequest: TAbsHTTPConnectionRequest);
begin
  inherited InitRequest(ARequest);
end;

procedure TWCHttpServer.InitResponse(AResponse: TAbsHTTPConnectionResponse);
begin
  inherited InitResponse(AResponse);
end;

function TWCHttpServer.AttachNewHTTP2Con(aSocket: TWCHTTPSocketReference;
  aOpenMode: THTTP2OpenMode; aServerDoConsume: THttpRefSocketConsume;
  aSendData: THttpRefSendData): TWCHTTP2Connection;
begin
  Result := TWCHTTP2Connection.Create(FHTTPRefConnections,
                                      aSocket,
                                      aOpenMode,
                                      aServerDoConsume,
                                      aSendData);
  Application.GarbageCollector.Add(Result);
  FHTTPRefConnections.AddConnection(Result);
end;

function TWCHttpServer.AttachNewHTTP11Con(aSocket: TWCHTTPSocketReference;
  aServerDoConsume: THttpRefSocketConsume; aSendData: THttpRefSendData
  ): TWCHTTP11Connection;
begin
 Result := TWCHTTP11Connection.Create(FHTTPRefConnections,
                                     aSocket,
                                     aServerDoConsume,
                                     aSendData);
 Application.GarbageCollector.Add(Result);
 FHTTPRefConnections.AddConnection(Result);
end;

procedure TWCHttpServer.AddToMainPool(AJob: TWCMainClientJob);
var SC, LC : Integer;
begin
  SC := FThreadPool.SortedJobsCount * MaxPreClientsThreads div
                                      MaxMainClientsThreads shr 1;
  LC := FThreadPool.LinearJobsCount;
  {if ((SC > (FMaxMainClientsThreads shl 2)) and
      (LC < (FMaxPreClientsThreads shl 1))) or
     ((SC > (FMaxMainClientsThreads shl 1)) and
      (LC <  FMaxPreClientsThreads)) then}
  if SC > LC then
     AddLinearJob(AJob) else
     AddSortedJob(AJob);
end;

procedure TWCHttpServer.CheckThreadPool;
begin
   FPoolsLocker.Lock;
   try
     if not assigned(FThreadPool) then
     begin
       FThreadPool := TSortedThreadPool.Create(@CompareMainJobs,
                                               Application.ThreadPoolJobToJobWait,
                                               Application.MaxMainThreads,
                                               Application.MaxPrepareThreads);
       FThreadPool.Running := true;
     end;
   finally
     FPoolsLocker.UnLock;
   end;
end;

procedure TWCHttpServer.AddLinearJob(AJob: TLinearJob);
begin
  FThreadPool.AddLinear(AJob);
  {$ifdef DEBUG_STAT}
  if DEBUG_GLOBALS_LONGWORD[DG_MAX_LIVE_LINEAR_JOBS] < FThreadPool.LinearJobsCount then
  begin
    DEBUG_GLOBALS_LONGWORD[DG_MAX_LIVE_LINEAR_JOBS] := FThreadPool.LinearJobsCount;
  end;
  Inc(DEBUG_GLOBALS_LONGWORD[DG_COUNT_LINEAR_JOBS]);
  {$endif}
end;

procedure TWCHttpServer.AddSortedJob(AJob: TSortedJob);
begin
  FThreadPool.AddSorted(AJob);
  {$ifdef DEBUG_STAT}
  if DEBUG_GLOBALS_LONGWORD[DG_MAX_LIVE_SORTED_JOBS] < FThreadPool.SortedJobsCount then
  begin
    DEBUG_GLOBALS_LONGWORD[DG_MAX_LIVE_SORTED_JOBS] := FThreadPool.SortedJobsCount;
  end;
  Inc(DEBUG_GLOBALS_LONGWORD[DG_COUNT_SORTED_JOBS]);
  {$endif}
end;

function TWCHttpServer.GetMaxMainClientsThreads: Integer;
begin
  Result := FThreadPool.SortedThreadsCount;
end;

function TWCHttpServer.GetMaxPreClientsThreads: Integer;
begin
  Result := FThreadPool.LinearThreadsCount;
end;

procedure TWCHttpServer.StopThreads;
begin
  if Assigned(FThreadPool) then FreeAndNil(FThreadPool);
end;

procedure TWCHttpServer.SetSSLMasterKeyLog(AValue: String);
begin
  FSSLLocker.Lock;
  try
    inherited SetSSLMasterKeyLog(AValue);
  finally
    FSSLLocker.UnLock;
  end;
end;

procedure TWCHttpServer.SetHostName(AValue: string);
begin
  FSSLLocker.Lock;
  try
    inherited SetHostName(AValue);
  finally
    FSSLLocker.UnLock;
  end;
end;

procedure TWCHttpServer.SetCertificate(AValue: String);
begin
  FSSLLocker.Lock;
  try
    inherited SetCertificate(AValue);
  finally
    FSSLLocker.UnLock;
  end;
end;

procedure TWCHttpServer.SetPrivateKey(AValue: String);
begin
  FSSLLocker.Lock;
  try
    inherited SetPrivateKey(AValue);
  finally
    FSSLLocker.UnLock;
  end;
end;

function TWCHttpServer.ServerActive: Boolean;
begin
  Result := Active;
end;

procedure TWCHttpServer.DoConnectToSocketRef(SockRef: TWCHTTPSocketReference);
Var
  Con : TWCConnection;
begin
  SockRef.Lock;
  try
    Con:=TWCConnection.CreateRefered(Self, SockRef);
  finally
    SockRef.UnLock;
  end;
  Con.OnRequestError:=@HandleRequestError;
  CreateConnectionThread(Con);
end;

procedure TWCHttpServer.DoSendData(aConnection: TWCHTTPRefConnection);
begin
  AddLinearJob(TWCHttpRefSendDataJob.Create(aConnection));
end;

destructor TWCHttpServer.Destroy;
begin
  if Assigned(FThreadPool) then FThreadPool.Free;
  FPoolsLocker.Free;
  FSSLLocker.Free;
  FHTTPRefConnections.Free;
  inherited Destroy;
end;

{ TWCHttpServerHandler }

procedure TWCHttpServerHandler.HandleRequestError(Sender: TObject; E: Exception
  );
begin
  Try
    Application.DoError('Error (%s) handling request : %s',[E.ClassName,E.Message]);
  except
    // Do not let errors escape
  end;
end;

function TWCHttpServerHandler.CreateServer: TEmbeddedAbsHttpServer;
begin
  Result:=TWCHttpServer.Create(Self);
end;

function TWCHttpServerHandler.GetESServer: TWCHttpServer;
begin
  Result := TWCHttpServer(HTTPServer);
end;

{ TWebClient }

function TWebClient.GetAcceptGZip: Boolean;
begin
  Result := FAcceptGZip.Value;
end;

function TWebClient.GetLastConnection: Integer;
begin
  Result := FLastConnection.Value;
end;

function TWebClient.GetScore: Integer;
begin
  Result := FScore.Value;
end;

function TWebClient.GetStateHash(index : TWebClientState): Cardinal;
begin
  Result := FCurStates.Hash[index];
end;

function TWebClient.GetAcceptDeflate: Boolean;
begin
  Result := FAcceptDeflate.Value;
end;

function TWebClient.GetHasSynConnection: Boolean;
begin
  Result := FHasSynConnection.Value;
end;

procedure TWebClient.SetAcceptDeflate(AValue: Boolean);
begin
  FAcceptDeflate.Value:= AValue;
end;

procedure TWebClient.SetAcceptGZip(AValue: Boolean);
begin
  FAcceptGZip.Value := AValue;
end;

procedure TWebClient.SetHasSynConnection(AValue: Boolean);
begin
  FHasSynConnection.Value:= AValue;
end;

procedure TWebClient.SetScore(AValue: Integer);
begin
  FScore.Value := AValue;
end;

function TWebClient.GenerateNewStateHash: Cardinal;
var cT : Qword;
begin
  cT := GetTickCount64;
  Result := Cardinal(cT - FStartStamp);
end;

constructor TWebClient.Create(AOwner: TWebClients; const aCUID: String);
begin
  inherited Create;
  FCUID := aCUID;
  FOwner := AOwner;
  FStartStamp := GetTickCount64;
  FCurStates := TWebThreadSafeCacheStates.Create;
  FHasSynConnection := TThreadBoolean.Create(false);
  FAcceptGZip := TThreadBoolean.Create(false);
  FAcceptDeflate := TThreadBoolean.Create(false);
  FOnRemove:=nil;
  FScore := TThreadInteger.Create(0);
  FLastConnection := TThreadInteger.Create(0);
  FOwner.AddNew(aCUID, Self);

  Application.GarbageCollector.Add(Self);
end;

destructor TWebClient.Destroy;
begin
  if assigned(FOnRemove) then
     FOnRemove(Self);
  FOwner.RemoveClientNotDestroy(FCUID);

  FAcceptGZip.Free;
  FAcceptDeflate.Free;
  FHasSynConnection.Free;

  FLastConnection.Free;
  FScore.Free;

  FCurStates.Free;

  inherited Destroy;
end;

procedure TWebClient.DoIdle;
begin
  RelaxScore;
end;

procedure TWebClient.UpdateScore;
var CTS : Cardinal;
begin
  CTS := Application.GetTimeSecFromStart;
  if (CTS - FLastConnection.Value) < 10 then FScore.IncValue;
  FLastConnection.Value := CTS;
end;

procedure TWebClient.RelaxScore;
var CTS : Cardinal;
begin
  CTS := Application.GetTimeSecFromStart;
  if (FScore.Value > 0) and ((CTS - FLastConnection.Value) > 10) then
     FScore.DecValue;
end;

procedure TWebClient.Initialize;
begin
  //override this
end;

function TWebClient.SaveNewState(aState: TWebClientState;
  const aNewState: String; oldHash: Cardinal;
  ignoreHash : boolean = false): Boolean;
begin
  if ignoreHash or
     (oldHash = 0) or
     (oldHash <> FCurStates.Hash[aState]) or
     (not SameStr(FCurStates.Value[aState], aNewState)) then
  begin
    FCurStates.Value[aState] := aNewState;
    FCurStates.Hash[aState]  := GenerateNewStateHash;
    Result := True;
  end else
    Result := False;
end;

procedure TWebClient.ResponseString(AResponse: TResponse; const S: String);
var
  StrBuffer : TBufferedStream;
  L : Longint;
begin
  L := Length(S);
  if (L > Application.CompressLimit) and
     ({$IFDEF ALLOW_STREAM_GZIP} AcceptGzip or {$ENDIF}AcceptDeflate) then
  begin
    StrBuffer := TBufferedStream.Create;
    StrBuffer.SetPointer(@(S[1]), L);
    ResponseStream(AResponse, StrBuffer, L, true);
  end  else
    AResponse.Content:=S;
end;

procedure TWebClient.ResponseStream(AResponse : TResponse; Str : TStream;
  StrSize : Int64;
  OwnStream : Boolean);
var
  deflateStream : TDefcompressionstream;
  NeedCompress : Boolean;
  {$IFDEF ALLOW_STREAM_GZIP}
  gzStream : TGzCompressionstream;
  {$ENDIF}
begin
  NeedCompress:= StrSize > Application.CompressLimit;
  {$IFDEF ALLOW_STREAM_GZIP}
  if AcceptGzip and NeedCompress then
  begin
    AResponse.ContentStream := TMemoryStream.Create;
    gzStream := Tgzcompressionstream.create(cldefault, AResponse.ContentStream);
    try
      gzStream.CopyFrom(Str, StrSize);
    finally
      gzStream.Free;
    end;
    AResponse.FreeContentStream:=true;
    AResponse.ContentStream.Position:=0;
    AResponse.ContentLength := AResponse.ContentStream.Size;
    AResponse.SetHeader(hhContentEncoding, cSgzip);
  end else
  {$ENDIF}
  if AcceptDeflate and NeedCompress then
  begin
    AResponse.ContentStream := TMemoryStream.Create;
    deflateStream := Tdefcompressionstream.create(cldefault, AResponse.ContentStream);
    try
      deflateStream.CopyFrom(Str, StrSize);
    finally
      deflateStream.Free;
    end;
    AResponse.FreeContentStream:=true;
    AResponse.ContentStream.Position:=0;
    AResponse.ContentLength := AResponse.ContentStream.Size;
    AResponse.SetHeader(hhContentEncoding, cSdeflate);
  end else
  begin
    AResponse.ContentStream := Str;
    AResponse.FreeContentStream := OwnStream;
    Str := nil;
  end;
  if OwnStream and Assigned(Str) then Str.Free;
end;

{ TWebClients }

function TWebClients.GetClient(const cUID: String): TWebClient;
begin
  Lock;
  try
    Result := TWebClient(FHash[cUID]);
  finally
    UnLock;
  end;
end;

function TWebClients.IsClientDead(aClient: TObject; {%H-}data: pointer): Boolean;
begin
  // TWebClients already locked here
  Result := (not Container.Sessions.IsActiveSession(TWebClient(aClient).FCUID)) or
             Application.Terminated;
  if Result then
    Application.DoInfo('Client is dead ' + TWebClient(aClient).CUID);
end;

procedure TWebClients.AfterClientExtracted(aObj: TObject);
begin
  TWebClient(aObj).DecReference;
end;

procedure TWebClients.IdleClient(aObj: TObject);
var aClient : TWebClient;
begin
  aClient := TWebClient(aObj);
  aClient.DoIdle;
end;

constructor TWebClients.Create(aContainer: TWebClientsContainer);
begin
  inherited Create;
  FContainer := aContainer;
  FHash := TStringHashList.Create(true);
end;

destructor TWebClients.Destroy;
begin
  FHash.Free;
  inherited Destroy;
end;

procedure TWebClients.Clear;
begin
  Lock;
  try
    Clean;
  finally
    Unlock;
  end;
end;

procedure TWebClients.AddNew(const cUID: String; aClient: TWebClient);
begin
  Lock;
  try
    FHash.Add(cUID, aClient);
    Push_back(aClient);
  finally
    UnLock;
  end;
end;

procedure TWebClients.RemoveClient(aClient: TWebClient);
begin
  Lock;
  try
    ExtractObject(aClient);
    aClient.DecReference;
  finally
    UnLock;
  end;
end;

procedure TWebClients.RemoveClient(const cUID: String);
var aClient : TWebClient;
begin
  Lock;
  try
    aClient := Client[cUID];
    RemoveClient(aClient);
  finally
    UnLock;
  end;
end;

procedure TWebClients.RemoveClientNotDestroy(const cUID: String);
begin
  Lock;
  try
    FHash.Remove(cUID);
  finally
    UnLock;
  end;
end;

procedure TWebClients.ClearDeadClients;
begin
  ExtractObjectsByCriteria(@IsClientDead, @AfterClientExtracted, nil);
end;

procedure TWebClients.IdleLiveClients;
begin
  DoForAll(@IdleClient)
end;

{ TWebCacheCollection }

function TWebCacheCollection.GetCache(const index: String): TWebCachedItem;
begin
  Result := TWebCachedItem(FHash[index]);
end;

constructor TWebCacheCollection.Create;
begin
  inherited Create;
  FHash := TStringHashList.Create(true);
end;

destructor TWebCacheCollection.Destroy;
begin
  FHash.Free;
  inherited Destroy;
end;

procedure TWebCacheCollection.Clear;
begin
  Lock;
  try
    FHash.Clear;
  finally
    Unlock;
  end;
  inherited Clear;
end;

procedure TWebCacheCollection.AddNew(const aName: String; aData: TWebCachedItem
  );
begin
  Lock;
  try
    FHash.Add(aName, aData);
  finally
    UnLock;
  end;
  Add(aData);
end;

{ TWCHTTPApplication }

procedure TWCHTTPApplication.DoOnException(Sender: TObject; E: Exception);
begin
  WriteLn('An error handled: ' + E.Message);
end;

procedure TWCHTTPApplication.StopThreads;
begin
  if Assigned(ESServer) then ESServer.StopThreads;
end;

function TWCHTTPApplication.GetMimeTemplates: TWCHTTPMimeTemplates;
begin
  Result := FConfig.FMimeTemplates;
end;

function TWCHTTPApplication.GetClientAllowEncode : String;
begin
  Result := FClientDecoders.GetListOfDecoders;
end;

procedure TWCHTTPApplication.SetClientAllowEncode(AValue : String);
begin
  FClientDecoders.RebuildListOfDecoders(AValue);
end;

procedure TWCHTTPApplication.DoOnConfigChanged(Sender: TWCConfigRecord);
var JTJ : TJobToJobWait;
begin
  if not VarIsNull(Sender.Value) then
  case Sender.HashName of
    CFG_SHUTDOWN :
      NeedShutdown := Sender.Value;
    CFG_SITE_FOLDER :
      WebFilesLoc := Sender.Value + cSysDelimiter;
    CFG_SERVER_NAME :
      Title := Sender.Value;
    CFG_MAIN_URI :
      MainURI := Sender.Value;
    CFG_SESSIONS_LOC :
      SessionsLoc := Sender.Value;
    CFG_CLIENTS_DB :
      SessionsDb := Sender.Value;
    CFG_LOG_DB :
      LogDb := Sender.Value;
    CFG_MIME_NAME :
      MimeLoc := Sender.Value;
    CFG_USE_SSL :
      UseSSL := Sender.Value;
    CFG_HOST_NAME :
      HostName := Sender.Value;
    CFG_SSL_LOC :
      SSLLoc := Sender.Value + cSysDelimiter;
    CFG_JOB_TO_JOB_WAIT,
    CFG_JOB_TO_JOB_WAIT_ADAPT_MIN,
    CFG_JOB_TO_JOB_WAIT_ADAPT_MAX :
    begin
      JTJ := ThreadPoolJobToJobWait;
      case Sender.HashName of
        CFG_JOB_TO_JOB_WAIT :        JTJ.DefaultValue:= Sender.Value;
        CFG_JOB_TO_JOB_WAIT_ADAPT_MIN : JTJ.AdaptMin := Sender.Value;
        CFG_JOB_TO_JOB_WAIT_ADAPT_MAX : JTJ.AdaptMax := Sender.Value;
      end;
      ThreadPoolJobToJobWait := JTJ;
    end;
    CFG_MAIN_THREAD_CNT :
      MaxMainThreads:= Sender.Value;
    CFG_PRE_THREAD_CNT :
      MaxPrepareThreads:= Sender.Value;
    //openssl
    CFG_SSL_VER : begin
      if ConfigChangeHalt then
      begin
        ESServer.FSSLLocker.Lock;
        try
          ESServer.SSLType := ExSSLStringToType(Sender.Value);
        finally
          ESServer.FSSLLocker.UnLock;
        end;
      end;
    end;
    CFG_SSL_CIPHER : begin
      ESServer.FSSLLocker.Lock;
      try
        ESServer.CertificateData.CipherList := Sender.Value;
      finally
        ESServer.FSSLLocker.UnLock;
      end;
    end;
    CFG_PRIVATE_KEY    :
      ESServer.PrivateKey := Sender.Value;
    CFG_CERTIFICATE    :
      ESServer.Certificate := Sender.Value;
    CFG_TLSKEY_LOG     :
      ESServer.SSLMasterKeyLog := Sender.Value;
    CFG_ALPN_USE_HTTP2 : begin
      ESServer.FSSLLocker.Lock;
      try
        ESServer.AlpnList.Clear;
        if Sender.Value then ESServer.AlpnList.Add('h2');
        ESServer.AlpnList.Add('http/1.1');
      finally
        ESServer.FSSLLocker.UnLock;
      end;
    end;
    //clients
    CFG_CLIENT_COOKIE_MAX_AGE :
       ClientCookieMaxAge := Sender.Value;
    CFG_CLIENT_TIMEOUT :
       ClientTimeOut := Sender.Value;
    CFG_CLIENT_ALLOW_ENCODE :
       ClientAllowEncode := Sender.Value;
    //http2
    CFG_H2SET_HEADER_TABLE_SIZE,
    CFG_H2SET_MAX_CONCURRENT_STREAMS,
    CFG_H2SET_INITIAL_WINDOW_SIZE,
    CFG_H2SET_MAX_FRAME_SIZE,
    CFG_H2SET_MAX_HEADER_LIST_SIZE: begin
      ESServer.HTTPRefConnections.HTTP2Settings.Add((Sender.HashName shr 4) and $0f,
                                                              Sender.Value);
    end;
    //Web files
    CFG_COMPRESS_LIMIT :
      CompressLimit:= Sender.Value;
    CFG_IGNORE_FILES :
      WebFilesIgnore := Sender.Value;
    CFG_EXCLUDE_IGNORE_FILES :
      WebFilesExcludeIgnore := Sender.Value;
  end;
end;

procedure TWCHTTPApplication.DoOnLoggerException(Sender: TObject; E: Exception);
begin
  DoError(E.Message);
end;

procedure TWCHTTPApplication.DoGetModule(Sender: TObject; {%H-}ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := nil;
end;

constructor TWCHTTPApplication.Create(AOwner: TComponent);
var I : integer;
begin
  inherited Create(AOwner);

  FStartStamp := 0;
  FConfig := nil;
  FWebFilesIgnoreRx := nil;
  FWebFilesExceptIgnoreRx := nil;

  FMaxMainThreads:= TThreadInteger.Create(1);
  FMaxPrepareThreads:= TThreadInteger.Create(1);
  FCompressLimit:= TThreadInteger.Create(500);
  FClientCookieMaxAge := TThreadInteger.Create(86400);
  FClientTimeOut := TThreadInteger.Create(10);
  FVPath := TThreadUtf8String.Create('');
  FMainHTTP := TThreadUtf8String.Create('');
  FSessionsLoc := TThreadUtf8String.Create('');
  FSessionsDb := TThreadUtf8String.Create('');
  FLogDbLoc := TThreadUtf8String.Create('');
  FWebFilesLoc := TThreadUtf8String.Create('');
  FWebFilesIgnore := TThreadUtf8String.Create('');
  FWebFilesExceptIgnore := TThreadUtf8String.Create('');
  FSSLLoc := TThreadUtf8String.Create('');
  FMimeLoc := TThreadUtf8String.Create('');
  FClientDecoders := TThreadSafeDecoders.Create;
  FThreadJobToJobWait := TThreadJobToJobWait.Create(DefaultJobToJobWait);
  FNeedShutdown := TThreadBoolean.Create(False);
  OnException:=@DoOnException;

  FNetDebugMode:=False;

  {$ifdef NOGUI}{$IFDEF UNIX}
  GWidgetHelper := TGWidgetSetHelper.Create;
  {$endif}{$ENDIF}

  OnGetModule:= @DoGetModule;
  //
  FMTime := GetTickCount64;

  if not TryStrToInt(ParamStr(1), i) then
  begin
    raise EHTTPServer.CreateFmt('Wrong port number "%s"', [ParamStr(1)]);
  end else
    Port := I;

  for I := 2 to ParamCount do
  begin
    if SameText(ParamStr(i), '-debug') or SameText(ParamStr(i), 'debug') then
       FNetDebugMode:=true;
  end;
end;

destructor TWCHTTPApplication.Destroy;
begin
  DoInfo('Server stopped');
  {$ifdef NOGUI}{$IFDEF UNIX}
  GWidgetHelper.Free;
  {$endif}{$endif}
  // first we need to dec references to all referenced objects
  // wait all jobs and kill threads
  StopThreads;
  // dec references to all clients
  WebContainer.ClearDeadClients;
  // dec references to all connections
  if assigned(ESServer) then
    ESServer.HTTPRefConnections.RemoveDeadConnections(GetTickCount64, 0);
  // clear cache (referenced streams)
  WebContainer.ClearCache;
  // finally clean lists of references
  if assigned(FReferences) then FreeAndNil(FReferences);
  if assigned(FSocketsReferences) then FreeAndNil(FSocketsReferences);
  if assigned(WebContainer) then FreeAndNil(WebContainer);
  // normal destruction step then
  Application := nil;
  OnException:=@DoOnException;
  FLogDB.Free;
  if assigned(FConfig) then FreeAndNil(FConfig);
  FMaxMainThreads.Free;
  FMaxPrepareThreads.Free;
  FCompressLimit.Free;
  FVPath.Free;
  FMainHTTP.Free;
  FSessionsLoc.Free;
  FSessionsDb.Free;
  FLogDbLoc.Free;
  FWebFilesLoc.Free;
  FWebFilesIgnore.Free;
  FWebFilesExceptIgnore.Free;
  FSSLLoc.Free;
  FMimeLoc.Free;
  FClientCookieMaxAge.Free;
  FClientTimeOut.Free;
  FThreadJobToJobWait.Free;
  FClientDecoders.Free;
  if Assigned(FWebFilesIgnoreRx) then  FWebFilesIgnoreRx.Free;
  if Assigned(FWebFilesExceptIgnoreRx) then FWebFilesExceptIgnoreRx.Free;
  FNeedShutdown.Free;
  inherited Destroy;
end;

procedure TWCHTTPApplication.DoOnIdle({%H-}sender: TObject);
var T : QWord;
begin
  {$ifdef NOGUI} {$IFDEF UNIX}
  GWidgetHelper.ProcessMessages;
  {$endif}  {$endif}
  T := GetTickCount64;
  if (T - FMTime) >= 10000 then  //every 10 secs
  begin
    FMTime := T;
    if assigned(FConfig) then FConfig.Sync(false);
    WebContainer.DoMaintainingStep;
    GarbageCollector.CleanDead;
    FSocketsReferences.CleanDead;
  end;
  //
  ESServer.HTTPRefConnections.Idle(T);
  //
  Sleep(5);
  if FNeedShutdown.Value then
  begin
    ESServer.Active := false;
    Terminate;
  end;
end;

function TWCHTTPApplication.GetClientCookieMaxAge: Integer;
begin
  Result := FClientCookieMaxAge.Value;
end;

function TWCHTTPApplication.GetClientTimeOut: Integer;
begin
  Result := FClientTimeOut.Value;
end;

function TWCHTTPApplication.GetCompressLimit: Cardinal;
begin
  Result := FCompressLimit.Value;
end;

function TWCHTTPApplication.GetConfigFileName: String;
begin
  if assigned(FConfig) then
    Result := FConfig.FileName else
    Result := '';
end;

function TWCHTTPApplication.GetESServer: TWCHttpServer;
begin
  Result := GetWebHandler.GetESServer;
end;

function TWCHTTPApplication.GetJobToJobWait: TJobToJobWait;
begin
  Result := FThreadJobToJobWait.Value;
end;

function TWCHTTPApplication.GetLogDbLoc: String;
begin
  Result := FLogDbLoc.Value;
end;

function TWCHTTPApplication.GetMainHTTP: String;
begin
  Result := FMainHTTP.Value;
end;

function TWCHTTPApplication.GetMaxMainThreads: Byte;
begin
  Result := FMaxMainThreads.Value;
end;

function TWCHTTPApplication.GetMaxPrepareThreads: Byte;
begin
  Result := FMaxPrepareThreads.Value;
end;

function TWCHTTPApplication.GetMimeLoc: String;
begin
  Result := FMimeLoc.Value;
end;

function TWCHTTPApplication.GetNeedShutdown: Boolean;
begin
  Result := FNeedShutdown.Value;
end;

function TWCHTTPApplication.GetSessionsDb: String;
begin
  Result := FSessionsDb.Value;
end;

function TWCHTTPApplication.GetSessionsLoc: String;
begin
  Result := FSessionsLoc.Value;
end;

function TWCHTTPApplication.GetSitePath: String;
begin
  Result := FVPath.Value;
end;

function TWCHTTPApplication.GetSSLLoc: String;
begin
  Result := FSSLLoc.Value;
end;

function TWCHTTPApplication.GetWebFilesExcludeIgnore: String;
begin
  Result := FWebFilesExceptIgnore.Value;
end;

function TWCHTTPApplication.GetWebFilesIgnore: String;
begin
  Result := FWebFilesIgnore.Value
end;

function TWCHTTPApplication.getWebFilesLoc: String;
begin
  Result := FWebFilesLoc.Value;
end;

procedure TWCHTTPApplication.SetClientCookieMaxAge(AValue: Integer);
begin
  FClientCookieMaxAge.Value:=AValue;
end;

procedure TWCHTTPApplication.SetClientTimeOut(AValue: Integer);
begin
  FClientTimeOut.Lock;
  try
    FClientTimeOut.Value:=AValue;
    if assigned(WebContainer) then
      WebContainer.Sessions.DefaultTimeOutMinutes:= AValue;
  finally
    FClientTimeOut.UnLock;
  end;
end;

procedure TWCHTTPApplication.SetCompressLimit(AValue: Cardinal);
begin
  if (AValue < 128) then Exit;
  FCompressLimit.Value := AValue;
end;

procedure TWCHTTPApplication.SetConfigFileName(AValue: String);
begin
  if Assigned(FConfig) then
  begin
    FConfig.FileName := AValue;
  end else begin
    FConfig := TWCHTTPConfig.Create(AValue);
    FConfig.OnChangeValue := @DoOnConfigChanged;
  end;
  FConfig.Sync(true);
end;

procedure TWCHTTPApplication.SetJobToJobWait(AValue: TJobToJobWait);
begin
  if assigned(ESServer) and
     Assigned(ESServer.FThreadPool) then begin
     ESServer.FThreadPool.ThreadJobToJobWait := AValue;
     FThreadJobToJobWait.Value := ESServer.FThreadPool.ThreadJobToJobWait;
  end else
     FThreadJobToJobWait.Value := AValue;
end;

procedure TWCHTTPApplication.SetLogDbLoc(AValue: String);
var loc : String;
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FLogDbLoc.Value) then Exit;
  if ConfigChangeHalt then begin
    FLogDbLoc.Value:= AValue;
    loc := ExtractFilePath(ExeName) + LogDb;
    if not Assigned(FLogDB) then begin
      FLogDB := TSqliteLogger.Create(loc);
      OnException:=@DoOnLoggerException;
      DoInfo('Server started');
    end;
  end;
end;

procedure TWCHTTPApplication.SetMainHTTP(AValue: String);
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FMainHTTP.Value) then Exit;

  FMainHTTP.Value := AValue;
end;

procedure TWCHTTPApplication.SetMaxMainThreads(AValue: Byte);
begin
  if AValue = 0 then AValue := 1;
  if AValue > WC_MAX_MAIN_THREADS then AValue := WC_MAX_MAIN_THREADS;
  if FMaxMainThreads.Value=AValue then Exit;


  FMaxMainThreads.Value:=AValue;
  if assigned(ESServer) and
     assigned(ESServer.FThreadPool) then
     ESServer.FThreadPool.SortedThreadsCount:=aValue;
end;

procedure TWCHTTPApplication.SetMaxPrepareThreads(AValue: Byte);
begin
  if AValue = 0 then AValue := 1;
  if AValue > WC_MAX_PREP_THREADS then AValue := WC_MAX_PREP_THREADS;
  if FMaxPrepareThreads.Value=AValue then Exit;

  FMaxPrepareThreads.Value:=AValue;
  if assigned(ESServer) and
     assigned(ESServer.FThreadPool) then
     GetESServer.FThreadPool.LinearThreadsCount:=aValue;
end;

procedure TWCHTTPApplication.SetMimeLoc(AValue: String);
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FMimeLoc.Value) then Exit;

  FMimeLoc.Value:= AValue;

  MimeTypesFile := SitePath + AValue;
  if FileExists(MimeTypesFile) then
    MimeTypes.LoadFromFile(MimeTypesFile);
end;

procedure TWCHTTPApplication.SetNeedShutdown(AValue: Boolean);
begin
  FNeedShutdown.Value := AValue;
end;

procedure TWCHTTPApplication.SetSessionsDb(AValue: String);
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FSessionsDb.Value) then Exit;

  if ConfigChangeHalt then
    FSessionsDb.Value := AValue;
end;

procedure TWCHTTPApplication.SetSessionsLoc(AValue: String);
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FSessionsLoc.Value) then Exit;

  if ConfigChangeHalt then
    FSessionsLoc.Value := AValue;
end;

procedure TWCHTTPApplication.SetSSLLoc(AValue: String);
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FSSLLoc.Value) then Exit;

  if ConfigChangeHalt then begin
    FSSLLoc.Value := AValue;
    if assigned(ESServer) then
      ESServer.SSLLoc := ExtractFilePath(ExeName) + AValue;
  end;
end;

procedure TWCHTTPApplication.SetWebFilesExcludeIgnore(AValue: String);
begin
  FWebFilesExceptIgnore.Lock;
  try
    if Length(AValue) = 0 then begin
      FWebFilesExceptIgnore.Value:= '';
      if Assigned(FWebFilesExceptIgnoreRx) then
        FreeAndNil(FWebFilesExceptIgnoreRx);
      Exit;
    end;
    if SameText(AValue, FWebFilesExceptIgnore.Value) then Exit;

    FWebFilesExceptIgnore.Value:= AValue;
    if assigned(FWebFilesExceptIgnoreRx) then
      FreeAndNil(FWebFilesExceptIgnoreRx);
    try
      FWebFilesExceptIgnoreRx := TRegExpr.Create(AValue);
    except
      if assigned(FWebFilesExceptIgnoreRx) then
        FreeAndNil(FWebFilesExceptIgnoreRx);
    end;
  finally
    FWebFilesExceptIgnore.UnLock;
  end;
end;

procedure TWCHTTPApplication.SetWebFilesIgnore(AValue: String);
begin
 FWebFilesIgnore.Lock;
 try
   if Length(AValue) = 0 then begin
     FWebFilesIgnore.Value:= '';
     if assigned(FWebFilesIgnoreRx) then
       FreeAndNil(FWebFilesIgnoreRx);
     Exit;
   end;
   if SameText(AValue, FWebFilesIgnore.Value) then Exit;

   FWebFilesIgnore.Value:= AValue;
   if assigned(FWebFilesIgnoreRx) then
     FreeAndNil(FWebFilesIgnoreRx);
   try
     FWebFilesIgnoreRx := TRegExpr.Create(AValue);
   except
     if assigned(FWebFilesIgnoreRx) then
       FreeAndNil(FWebFilesIgnoreRx);
   end;
 finally
   FWebFilesIgnore.UnLock;
 end;
end;

procedure TWCHTTPApplication.SetWebFilesLoc(AValue: String);
var loc : String;
begin
  if Length(AValue) = 0 then Exit;
  if SameText(AValue, FWebFilesLoc.Value) then Exit;

  if ConfigChangeHalt then begin
    loc := ExtractFilePath(ExeName) + AValue;
    if DirectoryExists(loc) then
    begin
      FWebFilesLoc.Value:= AValue;
      FVPath.Value := loc;
      if Length(MimeLoc) > 0 then begin
        MimeTypesFile := loc + MimeLoc;
        if FileExists(MimeTypesFile) then
          MimeTypes.LoadFromFile(MimeTypesFile);
      end;
      if Length(SSLLoc) > 0 then
        if assigned(ESServer) then
          ESServer.SSLLoc:= loc + FSSLLoc.Value;
    end;
  end;
end;

function TWCHTTPApplication.Initialized: Boolean;
begin
  Result := FStartStamp > 0;
end;

function TWCHTTPApplication.ConfigChangeHalt: Boolean;
begin
  Result := not Initialized;
  if not Result then
    DoError('Config value changed in runtime. Denied.');
end;

procedure TWCHTTPApplication.DoRun;
begin
  try
    inherited DoRun;
  except
    on EServerStopped do ;
    else
    raise;
  end;
end;

procedure TWCHTTPApplication.DoInfo(const V: String);
begin
  if Assigned(FLogDB) then
    FLogDB.LogAdd(LOG_INFO, V) else
    WriteLn('i: ', V);
end;

procedure TWCHTTPApplication.DoInfo(const V: String;
  const aParams: array of const);
begin
  if Assigned(FLogDB) then
    FLogDB.LogAdd(LOG_INFO, V, aParams) else
    WriteLn('i: ', Format(V, aParams));
end;

procedure TWCHTTPApplication.DoError(const V: String);
begin
  if Assigned(FLogDB) then
    FLogDB.LogAdd(LOG_ERROR, V) else
    WriteLn('e: ', V);
end;

procedure TWCHTTPApplication.DoError(const V: String;
  const aParams: array of const);
begin
  if Assigned(FLogDB) then
    FLogDB.LogAdd(LOG_ERROR, V, aParams) else
    WriteLn('e: ', Format(V, aParams));
end;

procedure TWCHTTPApplication.SendError(AResponse: TResponse; errno: Integer);
begin
  AResponse.Code:=errno;
  AResponse.Content:='';
  if not AResponse.HeadersSent then
    AResponse.SendHeaders;
end;

procedure TWCHTTPApplication.Initialize;
begin
  inherited Initialize;

  FReferences := TNetReferenceList.Create;
  FSocketsReferences :=TNetReferenceList.Create;
  WebContainer := TWebClientsContainer.Create;
  GetWebHandler.OnAcceptIdle:= @DoOnIdle;
  GetWebHandler.AcceptIdleTimeout:=1;
  ESServer.HTTPRefConnections.GarbageCollector := FReferences;

  FStartStamp:= GetTickCount64;
  DoInfo('Server initialized');
end;

function TWCHTTPApplication.GetWebHandler: TWCHttpServerHandler;
begin
  Result := TWCHttpServerHandler(WebHandler);
end;

function TWCHTTPApplication.InitializeAbstractWebHandler: TWebHandler;
begin
  Result:=TWCHttpServerHandler.Create(Self);
end;

function TWCHTTPApplication.GetTimeSecFromStart: Cardinal;
begin
  Result := (GetTickCount64 - FStartStamp) div 1000;
end;

function TWCHTTPApplication.CreateReferedMemoryStream: TRefMemoryStream;
begin
  Result := TRefMemoryStream.Create;
  Application.GarbageCollector.Add(Result);
end;

function TWCHTTPApplication.IsAcceptedWebFile(const FN: String): Boolean;
begin
  Result := true;
  //regexp here to ignore files
  //exclude from ignore exceptions
  //example: ignore_files:   ".*wec\\.jpg|loot.*";
  //         exclude_ignore: "chelowec\\.jpg|looter\\.java"
  //         will ignore all *wec.jpg and loot*.* files
  //         except chelowec.jpg and looter.java
  //nb: double escape \\ needed for json format
  try
    FWebFilesIgnore.Lock;
    try
      if assigned(FWebFilesIgnoreRx) and
         FWebFilesIgnoreRx.Exec(RegExprString(FN)) then
      begin
        //ignore file finded
        Result := False;
      end;
    finally
      FWebFilesIgnore.UnLock;
    end;
    if not Result then begin
      //check exclude
      FWebFilesExceptIgnore.Lock;
      try
        if assigned(FWebFilesExceptIgnoreRx) and
           FWebFilesExceptIgnoreRx.Exec(RegExprString(FN)) then
        begin
          //exclude finded ignore
          Result := True;
        end;
      finally
        FWebFilesExceptIgnore.UnLock;
      end;
    end;
  except
    // ignore parse exceptions
  end;
end;

{ TWebCachedItem }

function TWebCachedItem.GetCache: TRefMemoryStream;
begin
  Result := FCache;
end;

function TWebCachedItem.GetCacheControl : String;
begin
  Result := FCacheControl.Value;
end;

function TWebCachedItem.GetCharset : String;
begin
  Result := FCharset.Value;
end;

function TWebCachedItem.GetDeflateCache: TRefMemoryStream;
begin
  Result := FDeflateCache;
end;

function TWebCachedItem.GetDeflateReady: Boolean;
begin
  Lock;
  try
    Result := Assigned(FDeflateCache);
  finally
    UnLock;
  end;
end;

function TWebCachedItem.GetDeflateSize: QWord;
begin
  Lock;
  try
    Result := FDeflateSize;
  finally
    UnLock;
  end;
end;

{$IFDEF ALLOW_STREAM_GZIP}
function TWebCachedItem.GetGzipCache: TRefMemoryStream;
begin
  Result := FGzipCache;
end;

function TWebCachedItem.GetGzipReady: Boolean;
begin
  Lock;
  try
    Result := Assigned(FGzipCache);
  finally
    UnLock;
  end;
end;

function TWebCachedItem.GetGzipSize: QWord;
begin
  Lock;
  try
    Result := FGzipSize;
  finally
    UnLock;
  end;
end;
{$ENDIF}

function TWebCachedItem.GetMimeType: String;
begin
  Lock;
  try
    Result := FMimeType;
  finally
    UnLock;
  end;
end;

function TWebCachedItem.GetSize: QWord;
begin
  Lock;
  try
    Result := FSize;
  finally
    UnLock;
  end;
end;

constructor TWebCachedItem.Create(const aLoc, aURI : String);
begin
  inherited Create;
  FCacheControl := TThreadUtf8String.Create('');
  FCharset := TThreadUtf8String.Create('');
  FNeedToCompress := TThreadBoolean.Create(false);
  FCache := nil;
  FDeflateCache := nil;
  {$IFDEF ALLOW_STREAM_GZIP}
  FGzipCache := nil;
  {$ENDIF}
  Clear;
  FLoc := aLoc;
  FURI := aURI;
  FDataTime := EncodeDate(1990, 1, 1);
  FMimeType := MimeTypes.GetMimeType(ExtractFileExt(aLoc));
  If Length(FMimeType) = 0 then
    FMimeType:='application/octet-stream';

  UpdateFromTemplate;
end;

destructor TWebCachedItem.Destroy;
begin
  if assigned(FCache) then FCache.DecReference;
  if assigned(FDeflateCache) then FDeflateCache.DecReference;
  {$IFDEF ALLOW_STREAM_GZIP}
  if assigned(FGzipCache) then FGzipCache.DecReference;
  {$ENDIF}
  FCharset.Free;
  FCacheControl.Free;
  FNeedToCompress.Free;
  inherited Destroy;
end;

procedure TWebCachedItem.UpdateFromTemplate;
var T : TWCHTTPTemplateRecord;
begin
  T := Application.MimeTemplates.GetTemplate(FMimeType, FURI);
  if (Application.NetDebugMode) then FCacheControl.Value:='no-cache' else
                                     FCacheControl.Value:= T.Cache;
  FNeedToCompress.Value:= T.Compress;
  FCharset.Value := T.Charset;
end;

procedure TWebCachedItem.Clear;
begin
  if Assigned(FCache) then FCache.DecReference;
  {$IFDEF ALLOW_STREAM_GZIP}
  if Assigned(FGzipCache) then FGzipCache.DecReference;
  FGzipCache := nil;
  FGzipSize := 0;
  {$ENDIF}
  if Assigned(FDeflateCache) then FDeflateCache.DecReference;
  FDeflateCache := nil;
  FDeflateSize := 0;
  FCache := Application.CreateReferedMemoryStream;
  FSize := 0;
end;

procedure TWebCachedItem.Refresh;
var F : TFileStream;
  cDT : TDateTime;
  deflateStream : TDefcompressionstream;
  {$IFDEF ALLOW_STREAM_GZIP}
  gzStream : Tgzcompressionstream;
  {$ENDIF}
begin
  Lock;
  try
    if FileExists(FLoc) then
    begin
      FileAge(FLoc, cDT);
      if SecondsBetween(cDT, FDataTime) > 0 then
      begin
        Clear;
        FDataTime := cDT;
        F:=TFileStream.Create(FLoc, fmOpenRead or fmShareDenyWrite);
        try
          FSize:=F.Size;
          TMemoryStream(FCache.Stream).SetSize(FSize);
          FCache.Stream.Position := 0;
          FCache.Stream.CopyFrom(F, FSize);
        finally
          F.Free;
        end;
        if FNeedToCompress.Value then
        begin
          FCache.Lock;
          try
            FDeflateCache := Application.CreateReferedMemoryStream;
            deflateStream := TDefcompressionstream.create(cldefault, FDeflateCache.Stream);
            try
              FCache.Stream.Position:=0;
              deflateStream.CopyFrom(FCache.Stream, FSize);
            finally
              deflateStream.Free;
            end;
            FDeflateSize := FDeflateCache.Stream.Size;
            if FDeflateSize = 0 then begin
               FDeflateCache.DecReference;
               FDeflateCache := nil;
            end;
            {$IFDEF ALLOW_STREAM_GZIP}
            FGzipCache := Application.CreateReferedMemoryStream;
            gzStream := Tgzcompressionstream.create(cldefault, FGzipCache.Stream);
            try
              FCache.Stream.Position:=0;
              gzStream.CopyFrom(FCache.Stream, FSize);
            finally
              gzStream.Free;
            end;
            FGzipSize := FGzipCache.Stream.Size;
            if FGzipSize = 0 then begin
               FGzipCache.DecReference;
               FGzipCache := nil;
            end;
            {$ENDIF}
          finally
            FCache.UnLock;
          end;
        end;
      end;
    end else Clear;
  finally
    UnLock;
  end;
end;

{ TWebClientsContainer }

procedure TWebClientsContainer.ClientRemove(Sender: TObject);
var aClient : TWebClient;
begin
  aClient := TWebClient(Sender);

  PREP_ClientStop.Execute([aClient.CUID]);
  if assigned(Application) then
    Application.DoInfo('Client removed ' + aClient.CUID);
end;

function TWebClientsContainer.OnGenSessionID({%H-}aSession: TSqliteWebSession
  ): String;
var CID : Cardinal;
begin
  CID := FCurCID.ID;
  Result := EncodeIntToSID(CID, 4) + '-' +
            EncodeInt64ToSID(QWord(TimeStampToMSecs(DateTimeToTimeStamp(Now))), 8);
  PREP_ClientSetLastCUID.Execute([CID+1]);
end;

constructor TWebClientsContainer.Create;
begin
  inherited Create;

  FCachedPages := TWebCacheCollection.Create;
  FConnectedClients := TWebClients.Create(Self);

  GetWebCachedItem(Application.MainURI);

  FClientsDB := TExtSqlite3Dataset.Create(nil);
  FClientsDB.FileName := Application.SitePath +
                         Application.SessionsLoc + cSysDelimiter +
                         Application.SessionsDb;

  FCurCID := TThreadSafeAutoIncrementCardinal.Create;

  FSessions := TSqliteSessionFactory(SessionFactory);
  FSessions.DefaultTimeOutMinutes:=Application.ClientTimeOut;
  FSessions.InitializeDB(FClientsDB);
  FSessions.SessionCookiePath:='/; SameSite=Strict; Max-Age=' +
                                   IntToStr(Application.ClientCookieMaxAge);
  FSessions.CleanupInterval:=0; // manual cleanup
  FSessions.OnGenSessionID:= @OnGenSessionID;

  FClientsDB.ExecSQL(
  'create table if not exists clientsbase'+
    '(id integer primary key autoincrement, '+
     'start timestamp default current_timestamp,'+
     'stop timestamp default current_timestamp,'+
     'CUID text,'+
     'IPv4 text,'+
     'IPv6 text,'+
     'httpAccept text,'+
     'httpAcceptCharset text,'+
     'httpAcceptEncoding text,'+
     'httpAcceptLanguage text,'+
     'httpUserAgent text);');

  FClientsDB.ExecSQL(
  'create table if not exists clientcuid'+
  '(id integer default 1);');

  FClientsDB.ExecSQL(
  'create table if not exists netsessions'+
  '(id integer primary key autoincrement, '+
   'start timestamp default current_timestamp,'+
   'request_comm text,'+
   'request_socket int,'+
   'request_header text,'+
   'request_cookies text,'+
   'request_query text,'+
   'request_content text);');

  FClientsDB.ExecSQL('INSERT INTO clientcuid (id) '+
                                  'SELECT 1 WHERE NOT EXISTS (SELECT * FROM clientcuid)');

  PREP_AddClientToBase := FClientsDB.AddNewPrep(
   'INSERT INTO clientsbase '+
   '(CUID,IPv4,IPv6,'+
    'httpAccept,'+
    'httpAcceptCharset,'+
    'httpAcceptEncoding,'+
    'httpAcceptLanguage,'+
    'httpUserAgent) values(?1,?2,?3,?4,?5,?6,?7,?8);');

  PREP_ClientStop := FClientsDB.AddNewPrep(
    'update clientsbase set stop = current_timestamp where CUID = ?1;');

  PREP_ClientSetLastCUID := FClientsDB.AddNewPrep(
    'update clientcuid set id = ?1;');

  PREP_ClientGetLastCUID := FClientsDB.AddNewPrep(
    'select id from clientcuid;');

  PREP_OnCreateSession := FClientsDB.AddNewPrep(
   'INSERT INTO netsessions '+
   '(request_comm,request_socket,request_header,request_cookies,request_query,request_content) values(?1,?2,?3,?4,?5,?6);');

  PREP_DeleteOldNetRequests := FClientsDB.AddNewPrep(
   'delete from netsessions '+
   'where (julianday(current_timestamp)-julianday(start)) > 1.0;');

  FCurCID.SetValue(StrToInt(PREP_ClientGetLastCUID.QuickQuery([], nil, false)));
  PREP_DeleteOldNetRequests.Execute([]);
end;

destructor TWebClientsContainer.Destroy;
begin
  FCachedPages.Free;
  FConnectedClients.Free;
  FClientsDB.Free;
  FCurCID.Free;
  inherited Destroy;
end;

procedure TWebClientsContainer.ClearCache;
begin
  FCachedPages.Clear;
end;

function TWebClientsContainer.CreateSession(ARequest: TWCRequest
  ): TSqliteWebSession;
var reqHeaders  : TStringList;
begin
  if Assigned(ARequest) then
  begin
    Result := TSqliteWebSession(Sessions.CreateSession(ARequest));
    if Application.NetDebugMode then
    begin
      reqHeaders := TStringList.Create;
      try
        ARequest.CollectHeaders(reqHeaders);
        PREP_OnCreateSession.Execute([ARequest.HeaderLine,
                                      ARequest.Socket.Handle,
                                      Trim(reqHeaders.Text),
                                      Trim(ARequest.CookieFields.Text),
                                      Trim(ARequest.Method),
                                      Trim(ARequest.Content)]);
      finally
        reqHeaders.Free;
      end;
    end;
  end else
    Result := nil;
end;

Function ESSocketAddrToString(ASocketAddr: TSockAddr): String;
begin
  if ASocketAddr.sa_family = AF_INET then
    Result := NetAddrToStr(ASocketAddr.sin_addr)
  else // no ipv6 support yet
    Result := '';
end;

function ESGetLocalAddress(Handle : Cardinal): sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(sockets.TSockAddr);
  if fpGetSockName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function ESGetGetRemoteAddress(Handle : Cardinal): sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(sockets.TSockAddr);
  if fpGetPeerName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TWebClientsContainer.OnCreateNewSession(Sender: TObject);
var ARequest : TWCRequest;
  Session : TSqliteWebSession;
  Con : TWCConnection;
  IpV4, IpV6 : String;
  SocketAddr : TSockAddr;
begin
  Session := TSqliteWebSession(Sender);
  try
    ARequest := TWCRequest(Session.Request);
    Con := ARequest.GetConnection;

    SocketAddr := ESGetGetRemoteAddress(Con.Socket.Handle);

    IpV4 := ESSocketAddrToString(SocketAddr);
    IpV6 := '';
    PREP_AddClientToBase.Execute([Session.SID,
                                  IpV4,
                                  IpV6,
                                  ARequest.GetHeader(hhAccept),
                                  ARequest.GetHeader(hhAcceptCharset),
                                  ARequest.GetHeader(hhAcceptEncoding),
                                  ARequest.GetHeader(hhAcceptLanguage),
                                  ARequest.GetHeader(hhUserAgent)]);
  finally
  end;
end;

function TWebClientsContainer.AddClient(ARequest: TRequest;
  const ClientID: String): TWebClient;
begin
  Result := FConnectedClients.GetClient(ClientID);
  if not assigned(Result) then
  begin
    Result := Application.WebClientClass.Create(FConnectedClients, ClientID);
    Result.OnRemove:= @ClientRemove;
    Result.AcceptGzip:=  Pos(cSgzip, ARequest.GetHeader(hhAcceptEncoding)) > 0;
    Result.AcceptDeflate:=Pos(cSdeflate, ARequest.GetHeader(hhAcceptEncoding)) > 0;
    Application.DoInfo('Client added ' + ClientID);
  end;
end;

function TWebClientsContainer.GetClient(const ClientID: String): TWebClient;
begin
  Result := FConnectedClients.GetClient(ClientID);
end;

procedure TWebClientsContainer.RemoveClient(const ClientID: String);
begin
  FConnectedClients.RemoveClient(ClientID);
end;

procedure TWebClientsContainer.RemoveClient(mClient: TWebClient);
begin
  FConnectedClients.RemoveClient(mClient);
end;

procedure TWebClientsContainer.ClearDeadClients;
begin
  Sessions.CleanupSessions;
  FConnectedClients.ClearDeadClients;
end;

procedure TWebClientsContainer.DoMaintainingStep;
begin
  ClearDeadClients; // delete all timeouted clients
  if Application.NetDebugMode then
    PREP_DeleteOldNetRequests.Execute([]);
  Clients.IdleLiveClients;  // refresh all clients with no syn connection to
                            // prevent memory overflows
//  SeqSheduleStep;   // shedule clients seq
end;

function TWebClientsContainer.GetWebCachedItem(const aURI : String
  ) : TWebCachedItem;
var aLoc: String;
begin
  if Length(aURI) = 0 then raise Exception.Create('trying to cache empty uri');
  aLoc := StringReplace(aURI, cNonSysDelimiter, cSysDelimiter, [rfReplaceAll]);
  if aLoc[1] = cSysDelimiter then
    Delete(aLoc, 1, 1);
  aLoc := Application.SitePath + aLoc;
  Result := FCachedPages[aLoc];
  if not Assigned(Result) then
  begin
    Result := TWebCachedItem.Create(aLoc, aURI);
    FCachedPages.AddNew(aLoc, Result);
  end;
  Result.Refresh;
end;

procedure TWebClientsContainer.UpdateCachedWebItemsWithTemplates;
var i : integer;
  It : TWebCachedItem;
begin
  if assigned(FCachedPages) then
  begin
    FCachedPages.Lock;
    try
      for i := 0 to FCachedPages.Count-1 do
      begin
        It := TWebCachedItem(FCachedPages.Item[i]);
        It.UpdateFromTemplate;
      end;
    finally
      FCachedPages.UnLock;
    end;
  end;
end;

Initialization
  InitHTTP;

Finalization
  DoneHTTP;

end.
