{
 wcApplication:
   Custom application class to integrate with LCL

   Part of WCHTTPServer project

   Copyright (c) 2020-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcApplication;

{$mode objfpc}{$H+}

interface


{$IFDEF SERVER_NOT_RPC_MODE}
{$UNDEF SERVER_RPC_MODE}
{$ENDIF}
{$IFDEF SERVER_NOT_REST_MODE}
{$UNDEF SERVER_REST_MODE}
{$ENDIF}


{.$define USE_GLOBAL_SSL_CONTEXT}

uses
  {$ifdef wiki_docs}
  commonutils,fpcweb,
  {$endif}

  Classes,
  StringHashList,
  OGLFastList,
  OGLFastVariantHash,
  SysUtils, DateUtils,
  ECommonObjs,
  wcNetworking,
  fpwebfile, fpmimetypes,
  fphttp, HTTPDefs, httpprotocol, HTTP1Utils,
  HTTP2Consts,
  AbstractHTTPServer,
  wcHTTP2Con,
  ExtOpenSSL,
  custweb, CustAbsHTTPApp,
  {$IFDEF SERVER_RPC_MODE}
  OGLB64Utils,
  SqliteWebSession,
  {$ENDIF}
  SqliteLogger,
  jsonscanner, jsonparser, fpjson,
  wcConfig,
  {$IFDEF SERVER_RPC_MODE}
  ExtSqlite3DS,
  {$ENDIF}
  variants,
  sockets,
  ssockets,
  gzstream,
  wcDecoders,
  ExtMemoryStream,
  BufferedStream,
  SortedThreadPool,
  RegExpr,
  {$ifdef DEBUG_STAT}
  wcDebug_vars,
  {$endif}
  AvgLvlTree
  {$ifdef unix}
  , BaseUnix
  {$ifdef NOGUI}
  , gwidgetsethelper
  {$endif}
  {$endif}
  {$ifdef WC_WEB_SOCKETS}
  , wcWebsocketCon
  , WebsocketConsts
  {$endif}
  ;

type
  {$IFDEF SERVER_RPC_MODE}
  TWebClient = class;
  TWebClients = class;
  {$ENDIF}
  TWCResponse = class;
  TWCRequest = class;
  TWCHTTPServer = class;
  {$IFDEF SERVER_RPC_MODE}
  TWebClientClass = class of TWebClient;
  {$ENDIF}

  { TWCAppConnection }

  TWCAppConnection =  class(TWCConnection)
  private
    FInputBuf : Pointer;
    FInput    : TBufferedStream;
    FResponse : TWCResponse;
    FRequest  : TWCRequest;
    {$IFDEF SERVER_RPC_MODE}
    FClient   : TWebClient;
    FSession  : TSqliteWebSession;
    {$ENDIF}
    FProtocolVersion : TWCProtocolVersion;
    {$ifdef DEBUG_STAT}
    FDStartStamp, FDWaitStamp, FDStopStamp, FDReadStamp : QWord;
    {$endif}
    procedure DoInitialize;
  protected
    function ReadLine : String;
    Function ReadReqHeaders : TWCRequest;
    function ConvertFromRefRequest(AReq: TWCRequestRefWrapper): TWCRequest;
    procedure ReadReqContent(ARequest: TWCRequest);
    procedure ConsumeHeader(ARequest: TAbsHTTPConnectionRequest; AHeader: String); override;
    procedure UnknownHeader({%H-}ARequest: TAbsHTTPConnectionRequest; const {%H-}AHeader: String); override;
    procedure DoSocketAttach(ASocket : TSocketStream); override;
  public
    Constructor Create(AServer : TAbsCustomHTTPServer; ASocket : TSocketStream); override;
    constructor CreateRefered(AServer : TAbsCustomHTTPServer;
      ASocketRef : TWCSocketReference; AConRef : TWCRefConnection); override;
    function ConsumeSocketData : TWCConsumeResult;
    destructor Destroy; override;
    property Response : TWCResponse read FResponse;
    property Request : TWCRequest read FRequest;
    {$IFDEF SERVER_RPC_MODE}
    procedure SetSessionParams(aClient : TWebClient; aSession : TSqliteWebSession);
    property Client : TWebClient read FClient;
    property Session : TSqliteWebSession read FSession;
    {$ENDIF}
    property HTTPVersion : TWCProtocolVersion read FProtocolVersion;
  end;

  { TWCMainClientJob }

  TWCMainClientJob = class(TSortedJob)
  private
    FConn : TWCAppConnection;
    FResponseReadyToSend : Boolean;
    FParams : TFastHashList;
    {$IFDEF SERVER_RPC_MODE}
    function GetClient: TWebClient;
    {$ENDIF}
    function GetParPtr(index : LongWord): PVariant;
    function GetParam(index : LongWord): Variant;
    function GetRequest: TWCRequest;
    function GetResponse: TWCResponse;
    procedure SetParam(index : LongWord; AValue: Variant);
    function FindOrAddParamId(aId : LongWord) : integer;
  public
    constructor Create(aConn : TWCAppConnection); overload;
    destructor Destroy; override;
    procedure Execute; override;
    procedure ReleaseConnection;
    property  Connection : TWCAppConnection read FConn;
    property  Request : TWCRequest read GetRequest;
    property  Response : TWCResponse read GetResponse;
    {$IFDEF SERVER_RPC_MODE}
    property  Client : TWebClient read GetClient;
    {$ENDIF}
    property  Params : TFastHashList read FParams;
    property  Param[index : LongWord] : Variant read GetParam write SetParam;
    property  ParPtr[index : LongWord] : PVariant read GetParPtr;
    property  ResponseReadyToSend : Boolean read FResponseReadyToSend write
                                                 FResponseReadyToSend;
  end;

  TWCMainClientJobClass = class of TWCMainClientJob;

  { TWCMainClientWrapperJob }

  TWCMainClientWrapperJob = class(TWCMainClientJob)
  private
    FWrappedJob : TWCMainClientJob;
  public
    constructor Create(aConnection : TWCAppConnection;
                             aWrappedJobClass: TWCMainClientJobClass); overload;
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoWrappedExecute; virtual; abstract;
    property  WrappedJob : TWCMainClientJob read FWrappedJob;
  end;

  { TWCPreAnalizeJob }

  TWCPreAnalizeJob = class(TLinearJob)
  private
    FConn : TWCAppConnection;
    function GetRequest: TWCRequest;
    function GetResponse: TWCResponse;
  public
    constructor Create(aConn : TWCAppConnection);
    destructor Destroy; override;
    function GenerateClientJob : TWCMainClientJob; virtual;
    property Connection : TWCAppConnection read FConn;
    property Request : TWCRequest read GetRequest;
    property Response : TWCResponse read GetResponse;
  end;

  {$IFDEF SERVER_RPC_MODE}

  { TWCPreAnalizeClientJob }

  TWCPreAnalizeClientJob = class(TWCPreAnalizeJob)
  public
    procedure Execute; override;
  end;

  { TWCPreAnalizeClientNoSessionJob }

  TWCPreAnalizeClientNoSessionJob = class(TWCPreAnalizeJob)
  public
    procedure Execute; override;
    function GenClientID : String; virtual;
  end;

  {$ENDIF}

  { TWCPreAnalizeNoSessionNoClientJob }

  TWCPreAnalizeNoSessionNoClientJob = class(TWCPreAnalizeJob)
  public
    procedure Execute; override;
  end;

  TWCPreAnalizeJobClass = class of TWCPreAnalizeJob;

  { TWCIPBlocked }

  TWCIPBlocked = class(TThreadSafeObject)
  private
    FCoolDownTime : Integer;
    FIP : String; // with or without port
  public
    constructor Create(const IP : String);

    function IsBlocked : Boolean;

    procedure CoolDown(Minutes : Integer);
    procedure Unfreeze(forSecs : Integer);

    property IP : String read FIP;
  end;

  { TWCIPBlockList }

  TWCIPBlockList = class (specialize TThreadSafeFastBaseSeq <TWCIPBlocked>)
  private
    function IsUnblocked(Obj : TObject; {%H-}ptr : Pointer) : Boolean;
    procedure UnfreezeIP(Obj : TObject; forSec : Pointer);
    function CheckIP(Obj : TObject; ptr : Pointer) : Boolean;
  public
    function FindIP(const vIP : String) : TWCIPBlocked;
    procedure Unfreeze(forSec : Integer);
    procedure FreeUnblocked;
  end;

  { TWCHttpServer }

  TWCHttpServer = class(TWCCustomHttpServer)
  private
    FThreadPool : TSortedThreadPool;
    FPoolsLocker : TNetCustomLockedObject;
    FSSLLocker : TNetCustomLockedObject;
    FIPBlocked : TWCIPBlockList;
    {$ifdef USE_GLOBAL_SSL_CONTEXT}
    FSSLContext : TExtSSLContext;
    function AlpnSelect(outv : PPChar; outl : PChar; inv : PChar;
      inlen : Cardinal) : integer;
    Procedure InitGlobalSSLContext;
    {$endif}
    function CompareMainJobs({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer) : Integer;
    procedure AddToMainPool(AJob : TWCMainClientJob);
    procedure CheckThreadPool;
    function GetHTTP2Settings: TWCHTTP2Settings;
    function GetMaxMainClientsThreads: Integer;
    function GetMaxPreClientsThreads: Integer;
    procedure InternalAllowConnect(Sender : TObject;
                             ASocket : Longint; Var Allow : Boolean);
    {$IFDEF WC_WEB_SOCKETS}
    function GetWebSocketSettings: TWCWebSocketSettings;
    {$ENDIF}
    procedure StopThreads;
  protected
    procedure SetSSLMasterKeyLog(AValue: String); override;
    procedure SetHostName(AValue: string); override;
    procedure SetCertificate(AValue: String); override;
    procedure SetPrivateKey(AValue: String); override;
    function AttachRefCon(aRefCon : TWCRefConnection) : TWCRefConnection;
    procedure HandleNetworkError(E : Exception); override;
    procedure HandleNetworkLog(const S : String); override;
    procedure HandleNetworkLog(const S : String; const Params : Array of Const); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateServerSocket; override;
    function  CreateSSLSocketHandler: TSocketHandler; override;
    Procedure CreateConnectionThread(Conn : TAbsHTTPConnection); override;
    function  CreateConnection(Data : TSocketStream) : TAbsHTTPConnection; override;
    Function  CreateRequest : TAbsHTTPConnectionRequest; override;
    Function  CreateResponse(ARequest : TAbsHTTPConnectionRequest) : TAbsHTTPConnectionResponse; override;
    Procedure InitRequest(ARequest : TAbsHTTPConnectionRequest); override;
    Procedure InitResponse(AResponse : TAbsHTTPConnectionResponse); override;

    function AttachNewHTTP2Con(aSocket: TWCSocketReference;
      aOpenMode: THTTP2OpenMode;
      aReadData, aSendData: TRefReadSendData): TWCHTTP2Connection;
    function AttachNewHTTP11Con(aSocket: TWCSocketReference;
      aReadData, aSendData: TRefReadSendData): TWCHTTP11Connection;
    {$IFDEF WC_WEB_SOCKETS}
    function AttachNewWebSocketCon(aSocket: TWCSocketReference;
      aOpenMode : TWebSocketUpgradeOptions;
      aReadData, aSendData: TRefReadSendData): TWCWebSocketConnection;
    {$ENDIF}
    property  MaxPreClientsThreads : Integer read GetMaxPreClientsThreads;
    property  MaxMainClientsThreads : Integer read GetMaxMainClientsThreads;
    procedure AddLinearJob(AJob : TLinearJob);
    procedure AddSortedJob(AJob : TSortedJob);
    function  ServerActive : Boolean;
    procedure DoReadData(aConnection : TWCRefConnection);
    procedure DoSendData(aConnection : TWCRefConnection);
    destructor Destroy; override;

    procedure CoolDownIP(const vIP : String; forMinutes : Integer);
    procedure UnfreezeIPs(forSecs : Integer);

    property  HTTP2Settings : TWCHTTP2Settings read GetHTTP2Settings;
    {$IFDEF WC_WEB_SOCKETS}
    property  WebSocketSettings : TWCWebSocketSettings read GetWebSocketSettings;
    {$ENDIF}
  end;

  { TWCHttpServerHandler }

  TWCHttpServerHandler = class(TAbsHTTPServerHandler)
  protected
    procedure HandleRequestError(Sender: TObject; E: Exception); override;
  public
    Function CreateServer : TEmbeddedAbsHttpServer; override;
    function GetWCServer : TWCHttpServer;
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
    FURIRegExp  : TRegExpr;
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

  TWCHTTPMimeTemplates = class(specialize TThreadSafeFastBaseCollection<TWCHTTPTemplate>)
  public
    procedure SortTemplates;
    procedure Rebuild(aTemplates : TJSONArray);
    function  GetTemplate(const aMime, aURI : String) : TWCHTTPTemplateRecord;
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

  TWCHTTPAppHelper = class
  public
    procedure DoHelp(aData : TObject); virtual; abstract;
  end;

  TWCHTTPAppInitHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppIdleHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppDoneHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppConfigInitHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppConfigLoadedHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppConfigRecordHelper = class(TWCHTTPAppHelper)
  end;

  TWCHTTPAppHelperClass = class of TWCHTTPAppHelper;

  { TWCHTTPAppHelpers }

  TWCHTTPAppHelpers = class(TThreadSafeFastSeq)
  private
    FHelperClass : TWCHTTPAppHelperClass;
    FData : TObject;
    procedure DoOnHelp(aHelper : TObject);
  public
    procedure DoHelp(aHelperClass : TWCHTTPAppHelperClass;
                     aData : TObject);
  end;

  { TWCTimeStampObj }

  TWCTimeStampObj = class
  private
    FTick : QWord;
  public
    constructor Create(aTick : QWord);
    property Tick : Qword read FTick;
  end;

  { TWCTimeStampLog }

  TWCTimeStampLog = class(TThreadSafeObject)
  private
    FPeriod : Integer;
    FLastStamp : QWord;
    FFormat : String;
    FEnabled : Boolean;
    function GetEnabled : Boolean;
    function GetFormat : String;
    function GetPeriod : Integer;
    procedure SetEnabled(AValue : Boolean);
    procedure SetFormat(AValue : String);
    procedure SetPeriod(AValue : Integer);
  public
    constructor Create;
    procedure TryToLog(stmp : TWCTimeStampObj);
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Format : String read GetFormat write SetFormat;
    property Period : Integer read GetPeriod write SetPeriod;
  end;

  { TWCHTTPApplication }

  TWCHTTPApplication = Class(TCustomAbsHTTPApplication)
  private
    FMTime : QWord;
    FLogDB : TSqliteLogger;
    FAppHelpers : TWCHTTPAppHelpers;
    FSocketsReferences, FReferences : TNetReferenceList;
    FStartStamp : QWord;
    FNetDebugMode : Boolean;
    FTimeStampLog : TWCTimeStampLog;
    {$IFDEF SERVER_RPC_MODE}
    FWebClientClass :  TWebClientClass;
    {$ENDIF}
    FServerAnalizeJobClass : TWCPreAnalizeJobClass;

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

    function GetNeedRestartServerSocket : Boolean;
    procedure SetNeedRestartServerSocket(AValue : Boolean);
    procedure SoftCloseIP(Obj : TObject; ptrIP : Pointer);

    procedure DoOnConfigChanged(Sender : TWCConfigRecord);
    procedure DoOnLoggerException(Sender : TObject; E : Exception);
    procedure DoOnException(Sender : TObject; E : Exception);
    Procedure DoGetModule(Sender : TObject; {%H-}ARequest : TRequest;
                               Var ModuleClass : TCustomHTTPModuleClass);
    procedure DoOnIdle({%H-}sender : TObject);
    function GetCacheLimit : QWord;
    function GetMimeTemplates: TWCHTTPMimeTemplates;
    function GetClientAllowEncode : String;
    function GetClientCookieMaxAge: Integer;
    function GetClientTimeOut: Integer;
    function GetCompressLimit: Cardinal;
    function GetConfigFileName: String;
    function GetWCServer: TWCHttpServer;
    function GetJobToJobWait: TJobToJobWait;
    function GetLogDbLoc: String;
    function GetMainHTTP: String;
    function GetMaxMainThreads: Byte;
    function GetMaxPrepareThreads: Byte;
    function GetMaxIOThreads : Byte;
    function GetMimeLoc: String;
    function GetNeedShutdown: Boolean;
    function GetSessionsDb: String;
    function GetSessionsLoc: String;
    function GetSitePath: String;
    function GetSSLLoc: String;
    function GetWebFilesExcludeIgnore: String;
    function GetWebFilesIgnore: String;
    function getWebFilesLoc: String;
    procedure SetCacheLimit(AValue : QWord);
    procedure SetClientCookieMaxAge(AValue: Integer);
    procedure SetClientTimeOut(AValue: Integer);
    procedure SetCompressLimit(AValue: Cardinal);
    procedure SetConfigFileName(AValue: String);
    procedure SetJobToJobWait(AValue: TJobToJobWait);
    procedure SetLogDbLoc(AValue: String);
    procedure SetMainHTTP(AValue: String);
    procedure SetMaxMainThreads(AValue: Byte);
    procedure SetMaxPrepareThreads(AValue: Byte);
    procedure SetMaxIOThreads(AValue : Byte);
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
    procedure SendError(AResponse: TAbsHTTPConnectionResponse; errno: Integer);
    procedure AddHelper(Hlp : TWCHTTPAppHelper);
    procedure CoolDownIP(const vIP : String; forMinutes : Integer);
    Procedure Initialize; override;
    function GetWebHandler: TWCHttpServerHandler;
    function InitializeAbstractWebHandler : TWebHandler; override;
    function GetTimeSecFromStart : Cardinal;
    function CreateRefMemoryStream : TRefMemoryStream;
    function CreateSizedRefMemoryStream(aSz : PtrInt) : TRefMemoryStream;
    property WCServer : TWCHttpServer read GetWCServer;
    property AppHelpers : TWCHTTPAppHelpers read FAppHelpers;
    property GarbageCollector : TNetReferenceList read FReferences;
    property SocketsCollector : TNetReferenceList read FSocketsReferences;

    //configurations
    property NetDebugMode : Boolean read FNetDebugMode;
    {$IFDEF SERVER_RPC_MODE}
    property WebClientClass : TWebClientClass read FWebClientClass write FWebClientClass;
    {$ENDIF}
    property ServerAnalizeJobClass : TWCPreAnalizeJobClass read FServerAnalizeJobClass write FServerAnalizeJobClass;
    property SitePath : String read GetSitePath;
    property MainURI : String read GetMainHTTP write SetMainHTTP;
    property SessionsLoc : String read GetSessionsLoc write SetSessionsLoc;
    property SessionsDb : String read GetSessionsDb write SetSessionsDb;
    property LogDb : String read GetLogDbLoc write SetLogDbLoc;
    property MimeLoc : String read GetMimeLoc write SetMimeLoc;
    //webfiles
    property WebFilesLoc : String read getWebFilesLoc write SetWebFilesLoc;
    property CompressLimit : Cardinal read GetCompressLimit write SetCompressLimit;
    property WebFilesCacheLimit : QWord read GetCacheLimit write SetCacheLimit;
    property WebFilesIgnore : String read GetWebFilesIgnore write SetWebFilesIgnore;
    property WebFilesExcludeIgnore : String read GetWebFilesExcludeIgnore write SetWebFilesExcludeIgnore;
    function IsAcceptedWebFile(const FN: String) : Boolean;
    property MimeTemplates : TWCHTTPMimeTemplates read GetMimeTemplates;
    //threads
    property MaxIOThreads : Byte read GetMaxIOThreads write SetMaxIOThreads;
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
    property NeedRestartServerSocket : Boolean read GetNeedRestartServerSocket
                                               write SetNeedRestartServerSocket;
    property TimeStampLog : TWCTimeStampLog read FTimeStampLog;
  end;

  TWebCacheCollection = class;

  { TWebCachedItem }

  TWebCachedItem = class(TNetCustomLockedObject)
  private
    FOwner : TWebCacheCollection;
    FAccessTime,
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
    function GetAccessAge : Integer;
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
    function GetTotalCachedSize : Qword;
    procedure UpdateFromTemplate;
  public
    constructor Create(aOwner : TWebCacheCollection; const aLoc, aURI : String);
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
    property AccessAge : Integer read GetAccessAge;
    property TotalCachedSize : Qword read GetTotalCachedSize;
  end;


  { TWebCacheCollection }

  TWebCacheCollection = class(TThreadSafeFastCollection)
  private
    FHash : TStringHashList;
    FCacheSizeLimit : TThreadQWord;
    FCachedSize : TThreadQWord;
    function GetCache(const index : String): TWebCachedItem;
    function GetCacheSizeLimit : QWord;
    procedure SetCacheSizeLimit(AValue : QWord);
    procedure PushCachedSize(NewSize : QWord);
    procedure ExtractCachedSize(OldSize : QWord);
    procedure RebuildWebCache;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddNew(const aName: String; aData: TWebCachedItem);
    property CacheItem[const index : String] : TWebCachedItem read GetCache; default;
    property CacheSizeLimit : QWord read GetCacheSizeLimit write SetCacheSizeLimit;
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

  {$IFDEF SERVER_RPC_MODE}

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
    procedure CompressStream(AResponse : TWCResponse; Str : TStream;
      StrSize : Int64; OwnStream : Boolean; ignoreLimits : Boolean = false);
  virtual;
    procedure CompressString(AResponse : TWCResponse;
      const S : String; ignoreLimits : Boolean = false); virtual;
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
  {$ENDIF}

  { TWebClientsContainer }

  TWebClientsContainer = class(TNetCustomLockedObject)
  private
    FCachedPages : TWebCacheCollection;
    {$IFDEF SERVER_RPC_MODE}
    FSessions : TSqliteSessionFactory;
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
    //
    FVerbose : TThreadBoolean;
    procedure ClientRemove(Sender : TObject);
    function GetVerbose : Boolean;
    function OnGenSessionID({%H-}aSession : TSqliteWebSession) : String;
    procedure SetVerbose(AValue : Boolean);
    {$ENDIF}
    function GetCacheSizeLimit : QWord;
    procedure SetCacheSizeLimit(AValue : QWord);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  ClearCache;
    {$IFDEF SERVER_RPC_MODE}
    function   CreateSession(ARequest : TWCRequest) : TSqliteWebSession;
    //
    procedure OnCreateNewSession(Sender : TObject);
    function  AddClient(ARequest : TAbsHTTPConnectionRequest; const ClientID : String): TWebClient;
    function  GetClient(const ClientID : String) : TWebClient;
    procedure RemoveClient(const ClientID : String); overload;
    procedure RemoveClient(mClient : TWebClient); overload;
    procedure ClearDeadClients;
    {$ENDIF}
    procedure DoMaintainingStep;
    //
    function  GetWebCachedItem(const aURI : String) : TWebCachedItem;
    procedure UpdateCachedWebItemsWithTemplates;
    //
    {$IFDEF SERVER_RPC_MODE}
    property  Sessions : TSqliteSessionFactory read FSessions;
    property  Clients  : TWebClients read FConnectedClients;
    property  Verbose  : Boolean read GetVerbose write SetVerbose;
    {$ENDIF}
    property  CacheSizeLimit : QWord read GetCacheSizeLimit write SetCacheSizeLimit;
  end;

  { TWCResponse }

  TWCResponse = class (TAbsHTTPConnectionResponse)
  private
    FKeepAlive : boolean;
    FRefStream : TReferencedStream;
    function  GetConnection : TWCAppConnection;
    procedure SetRefStream(AValue: TReferencedStream);
  public
    function EncodeContent(const aEncoder : AnsiString;
                            Str : TStream;
                            StrSize : Int64;
                            OwnStream : Boolean;
                            ignoreLimits : Boolean = false) : Boolean; virtual;
    function EncodeString(const aEncoder : AnsiString; const Str : String;
      ignoreLimits : Boolean) : Boolean; virtual;
    constructor Create(ARequest : TWCRequest); overload;
    destructor Destroy; override;
    procedure SendUTf8String(const S: String);
    procedure CloseStream;
    procedure ReleaseRefStream;
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
    property  WCConn : TWCAppConnection read GetConnection;
    property  RefStream : TReferencedStream read FRefStream write SetRefStream;
    property  KeepStreamAlive : Boolean read FKeepAlive write FKeepAlive;
  end;

  { TWCRequest }

  TWCRequest = class (TWCConnectionRequest)
  private
    function GetSocket: TSocketStream;
    function GetConnection : TWCAppConnection;
  protected
    procedure DecodeContent;
  public
    procedure CollectHeaders(Headers : TStringList);
    property  WCConn : TWCAppConnection read GetConnection;
    property  Socket : TSocketStream read GetSocket;
  end;

  { TWCHttpRefSendDataJob }

  TWCHttpRefSendDataJob = class(TLinearJob)
  private
    FConnection : TWCRefConnection;
    {$ifdef DEBUG_STAT}
    FDStartStamp : QWord;
    {$endif}
  public
    constructor Create(aConnection : TWCRefConnection);
    destructor  Destroy; override;
    procedure   Execute; override;
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

const
  cSgzip = 'gzip';
  cSdeflate = 'deflate';

  WC_NO_SESSION_CLIENT_ID = 'empty';

  WC_MAX_MAIN_THREADS = 32;
  WC_MAX_PREP_THREADS = 32;
  WC_MAX_IO_THREADS   = 8;

{$ifndef wiki_docs}
{$I wcappconfig.inc}
{$endif}

Implementation

uses  CustApp, extopensslsockets, wcutils, math, LazUTF8;
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
  AddWCConfiguration(WC_CFG_CONFIGURATION);
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

{ TWCTimeStampLog }

function TWCTimeStampLog.GetEnabled : Boolean;
begin
  Lock;
  try
    Result := FEnabled;
  finally
    UnLock;
  end;
end;

function TWCTimeStampLog.GetFormat : String;
begin
  Lock;
  try
    Result := FFormat;
  finally
    UnLock;
  end;
end;

function TWCTimeStampLog.GetPeriod : Integer;
begin
  Lock;
  try
    Result := FPeriod;
  finally
    UnLock;
  end;
end;

procedure TWCTimeStampLog.SetEnabled(AValue : Boolean);
begin
  Lock;
  try
    FEnabled := AValue;
  finally
    UnLock;
  end;
end;

procedure TWCTimeStampLog.SetFormat(AValue : String);
begin
  Lock;
  try
    FFormat := AValue;
  finally
    UnLock;
  end;
end;

procedure TWCTimeStampLog.SetPeriod(AValue : Integer);
begin
  Lock;
  try
    FPeriod := AValue;
  finally
    UnLock;
  end;
end;

constructor TWCTimeStampLog.Create;
begin
  inherited Create;
  FFormat := '%ts%';
  FPeriod := 1800;
  FEnabled := false;
  FLastStamp := GetTickCount64;
end;

procedure TWCTimeStampLog.TryToLog(stmp : TWCTimeStampObj);
var S, S0 : String;
    Fire : Boolean;
begin
  Lock;
  try
    Fire := FEnabled and (((stmp.Tick - FLastStamp) div 1000) >= FPeriod);
    if Fire then
    begin
      FLastStamp := stmp.Tick;
      S := FFormat;
    end;
  finally
    UnLock;
  end;

  if Assigned(Application) and Fire then
  begin
    DateTimeToString(S0, 'YY-MM-DD HH:MM:SS', Now, []);
    S := UTF8StringReplace(S, '%ts%', S0, [rfReplaceAll]);
    S := UTF8StringReplace(S, '%goc%', inttostr(Application.GarbageCollector.Count), [rfReplaceAll]);
    if assigned(Application.WCServer) then
    begin
      S := UTF8StringReplace(S, '%acc%', inttostr(Application.WCServer.RefConnections.Count), [rfReplaceAll]);
      if assigned(Application.WCServer.FThreadPool) then
      with Application.WCServer.FThreadPool do
      begin
        S := UTF8StringReplace(S, '%wtc%', inttostr(SortedThreadsCount), [rfReplaceAll]);
        S := UTF8StringReplace(S, '%ptc%', inttostr(LinearThreadsCount), [rfReplaceAll]);
        S := UTF8StringReplace(S, '%wjc%', inttostr(SortedJobsCount), [rfReplaceAll]);
        S := UTF8StringReplace(S, '%pjc%', inttostr(LinearJobsCount), [rfReplaceAll]);
      end;
    end;
    Application.DoInfo(S);
  end;
end;

{ TWCIPBlockList }

function TWCIPBlockList.IsUnblocked(Obj : TObject; {%H-}ptr : pointer) : Boolean;
begin
  Result := not TWCIPBlocked(Obj).IsBlocked;
end;

procedure TWCIPBlockList.UnfreezeIP(Obj : TObject; forSec : Pointer);
begin
  TWCIPBlocked(Obj).Unfreeze(PInteger(forSec)^);
end;

function TWCIPBlockList.CheckIP(Obj : TObject; ptr : Pointer) : Boolean;
begin
  Result := SameText(TWCIPBlocked(Obj).IP, PChar(ptr));
end;

function TWCIPBlockList.FindIP(const vIP : String) : TWCIPBlocked;
begin
  Result := FindValue(@CheckIP, PChar(vIP));
end;

procedure TWCIPBlockList.Unfreeze(forSec : Integer);
begin
  DoForAllEx(@UnfreezeIP, @forSec);
end;

procedure TWCIPBlockList.FreeUnblocked;
begin
  EraseObjectsByCriteria(@IsUnblocked, nil);
end;

{ TWCIPBlocked }

constructor TWCIPBlocked.Create(const IP : String);
begin
  inherited Create;
  FIP := IP;
  FCoolDownTime := 0;
end;

function TWCIPBlocked.IsBlocked : Boolean;
begin
  Lock;
  try
    Result := FCoolDownTime > 0;
  finally
    UnLock;
  end;
end;

procedure TWCIPBlocked.CoolDown(Minutes : Integer);
begin
  Lock;
  try
    FCoolDownTime := Minutes * 60;
  finally
    UnLock;
  end;
end;

procedure TWCIPBlocked.Unfreeze(forSecs : Integer);
begin
  Lock;
  try
    Dec(FCoolDownTime, forSecs);
  finally
    UnLock;
  end;
end;

{ TWCTimeStampObj }

constructor TWCTimeStampObj.Create(aTick : QWord);
begin
  FTick := aTick;
end;

{ TWCHTTPAppHelpers }

procedure TWCHTTPAppHelpers.DoOnHelp(aHelper : TObject);
begin
  if aHelper is FHelperClass then
  begin
    TWCHTTPAppHelper(aHelper).DoHelp(FData);
  end;
end;

procedure TWCHTTPAppHelpers.DoHelp(aHelperClass : TWCHTTPAppHelperClass;
                                   aData : TObject);
begin
  Lock;
  try
    FHelperClass := aHelperClass;
    FData := aData;
    DoForAll(@DoOnHelp);
  finally
    UnLock;
  end;
end;

{ TWCPreAnalizeJob }

function TWCPreAnalizeJob.GetRequest : TWCRequest;
begin
  Result := Connection.Request;
end;

function TWCPreAnalizeJob.GetResponse : TWCResponse;
begin
  Result := Connection.Response;
end;

constructor TWCPreAnalizeJob.Create(aConn : TWCAppConnection);
begin
  FConn := aConn;
end;

destructor TWCPreAnalizeJob.Destroy;
begin
  if assigned(FConn) then begin
    FConn.Free;
  end;
  inherited Destroy;
end;

function TWCPreAnalizeJob.GenerateClientJob : TWCMainClientJob;
begin
  Result := nil;
end;

{$IFDEF SERVER_RPC_MODE}
{ TWCPreAnalizeClientJob }

procedure TWCPreAnalizeClientJob.Execute;
var ASynThread : TWCMainClientJob;
    aClient : TWebClient;
    aSession : TSqliteWebSession;
    aConsumeResult : TWCConsumeResult;
begin
  try
     {$ifdef DEBUG_STAT}
     FConn.FDWaitStamp := GetTickCount64;
     {$endif}
     if not (Assigned(FConn) and TWCHttpServer(FConn.Server).ServerActive) then
       Exit;
     aConsumeResult := FConn.ConsumeSocketData;
     if aConsumeResult = wccrOK then begin
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
           Application.WCServer.AddToMainPool(ASynThread);
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
{$ENDIF}

{ TWCPreAnalizeNoSessionNoClientJob }

procedure TWCPreAnalizeNoSessionNoClientJob.Execute;
var ASynThread : TWCMainClientJob;
    aConsumeResult : TWCConsumeResult;
begin
  try
    if not (Assigned(FConn) and TWCHttpServer(FConn.Server).ServerActive) then
      Exit;
    aConsumeResult := FConn.ConsumeSocketData;
    if aConsumeResult = wccrOK then begin
      {$IFDEF SERVER_RPC_MODE}
      FConn.SetSessionParams(nil, nil);
      {$ENDIF}
      //
      ASynThread := GenerateClientJob;
      if Assigned(ASynThread) then
      begin
        FConn := nil; //now fconn is part of ASynThread job
        Application.WCServer.AddToMainPool(ASynThread);
      end;
    end;
  except
    on E: Exception do ; // catch errors. jail them in thread
  end;
end;

{$IFDEF SERVER_RPC_MODE}
{ TWCPreAnalizeClientNoSessionJob }

procedure TWCPreAnalizeClientNoSessionJob.Execute;
var ASynThread : TWCMainClientJob;
    aClient : TWebClient;
    aConsumeResult : TWCConsumeResult;
begin
  try
     if not (Assigned(FConn) and TWCHttpServer(FConn.Server).ServerActive) then
       Exit;
     aConsumeResult := FConn.ConsumeSocketData;
     if aConsumeResult = wccrOK then begin
       aClient := WebContainer.AddClient(Request, GenClientID);
       if not assigned(aClient) then begin
         Application.SendError(Response, 405);
         Exit;
       end else begin
         aClient.Initialize;
       end;
       FConn.SetSessionParams(aClient, nil);
       //
       if assigned(aClient) then begin
         ASynThread := GenerateClientJob;
         if Assigned(ASynThread) then
         begin
           FConn := nil; //now fconn is part of ASynThread job
           Application.WCServer.AddToMainPool(ASynThread);
         end;
       end;
     end;
  except
    on E: Exception do ; // catch errors. jail them in thread
  end;
end;

function TWCPreAnalizeClientNoSessionJob.GenClientID : String;
begin
  Result := WebContainer.OnGenSessionID(nil);
end;
{$ENDIF}

{ TWCMainClientWrapperJob }

constructor TWCMainClientWrapperJob.Create(aConnection : TWCAppConnection;
  aWrappedJobClass : TWCMainClientJobClass);
begin
  inherited Create(aConnection);
  if assigned(aWrappedJobClass) then begin
    FWrappedJob := aWrappedJobClass.Create(aConnection);
    FWrappedJob.ResponseReadyToSend := false;
  end else
    FWrappedJob := nil;
end;

destructor TWCMainClientWrapperJob.Destroy;
begin
  if Assigned(FWrappedJob) then FWrappedJob.Free;
  inherited Destroy;
end;

procedure TWCMainClientWrapperJob.Execute;
begin
  try
    WrappedJob.Execute;
    DoWrappedExecute;
  finally
    if Assigned(FWrappedJob) then FWrappedJob.ReleaseConnection;
    inherited Execute;
  end;
end;

{ TWCHTTPMimeTemplates }

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
      T := Self[i];
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
  {$IFDEF WC_WEB_SOCKETS}
  d := aConfig.FindPath(HashToConfig(CFG_WEBSOCKET_SEC)^.NAME_STR + '.' +
                        HashToConfig(CFG_WEBSOCKET_DEFLATE_EXT)^.NAME_STR);
  if assigned(d) and (d is TJSONObject) then begin
    if assigned(Application) then
      Application.WCServer.WebSocketSettings.ConfigExtFromJson(WSEX_PMCEDEFLATE,
                                                           TJSONObject(d));
  end;
  {$ENDIF}

  Application.AppHelpers.DoHelp(TWCHTTPAppConfigLoadedHelper, aConfig);
end;

destructor TWCHTTPConfig.Destroy;
begin
  FMimeTemplates.Free;
  inherited Destroy;
end;

procedure TWCHTTPConfig.DoInitialize();
var MainSec, SSLSec, WFSec, ClientsSec, Http2Sec,
    {$IFDEF WC_WEB_SOCKETS}
    WebSocketSec,
    {$ENDIF}
    MaintSec : TWCConfigRecord;
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
  MainSec.AddValue(CFG_IO_THREAD_CNT, wccrInteger);
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
  ClientsSec.AddValue(CFG_CLIENT_VERBOSE, wccrBoolean);

  Http2Sec := Root.AddSection(HashToConfig(CFG_HTTP2_SEC)^.NAME_STR);
  Http2Sec.AddValue(CFG_H2SET_HEADER_TABLE_SIZE     , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_ENABLE_PUSH           , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_CONCURRENT_STREAMS, wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_INITIAL_WINDOW_SIZE   , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_FRAME_SIZE        , wccrInteger);
  Http2Sec.AddValue(CFG_H2SET_MAX_HEADER_LIST_SIZE  , wccrInteger);

  {$IFDEF WC_WEB_SOCKETS}
  WebSocketSec := Root.AddSection(HashToConfig(CFG_WEBSOCKET_SEC)^.NAME_STR);
  WebSocketSec.AddValue(CFG_WEBSOCKET_SUB_PROTO     , wccrString);
  {$ENDIF}

  MaintSec := Root.AddSection(HashToConfig(CFG_MAINTAIN_SEC)^.NAME_STR);
  MaintSec.AddValue(CFG_SHUTDOWN, False);
  MaintSec.AddValue(CFG_RESTART,  False);
  MaintSec.AddValue(CFG_TIMESTAMPLOG, False);
  MaintSec.AddValue(CFG_TIMESTAMPLOG_FORMAT, '%ts%');
  MaintSec.AddValue(CFG_TIMESTAMPLOG_PERIOD, 1800);
end;

{ TWCHttpRefSendDataJob }

constructor TWCHttpRefSendDataJob.Create(aConnection: TWCRefConnection);
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
  {$IFDEF SERVER_REST_MODE}
  AcceptGzip, AcceptDeflate : Boolean;
  {$ENDIF}
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
    {$IFDEF SERVER_REST_MODE}
    AcceptGzip:=  Pos(cSgzip, Request.GetHeader(hhAcceptEncoding)) > 0;
    AcceptDeflate:=Pos(cSdeflate, Request.GetHeader(hhAcceptEncoding)) > 0;
    {$ENDIF}

    FE := ExtractFileExt(aURI);
    if SameText(FE, '.svgz') then begin
      if {$IFDEF SERVER_RPC_MODE}Client.{$ENDIF}AcceptGzip then
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
        if {$IFDEF SERVER_RPC_MODE}Client.{$ENDIF}AcceptGzip and aCachedFile.GzipReady then
        begin
          Response.ContentLength:=aCachedFile.GzipSize;
          Response.RefStream:=aCachedFile.GzipCache;
          Response.SetHeader(hhContentEncoding, cSgzip)
        end else
        {$ENDIF}
        if {$IFDEF SERVER_RPC_MODE}Client.{$ENDIF}AcceptDeflate and aCachedFile.DeflateReady then
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

function TWCRequest.GetConnection: TWCAppConnection;
begin
  Result := TWCAppConnection(Connection);
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
      if (ContentObject.Stream is TCustomMemoryStream) then
        aDecoder.DecodeStream(ContentObject.Stream) else
      begin
        // maybe not best solution for large content blocks
        // Initial Content -> DecodedBytes -> Output Content.
        // utf8 charset means as default on the client-side.
        // need to parse Content-Type string to determinate
        // initial client-side charset and set Content type
        // ContentType := wctWideString for example
        Content := aDecoder.DecodeString(Content);
      end;
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

function TWCResponse.GetConnection: TWCAppConnection;
begin
  Result := TWCAppConnection(Connection);
end;

procedure TWCResponse.SetRefStream(AValue: TReferencedStream);
begin
  if FRefStream=AValue then Exit;
  ReleaseRefStream;
  FRefStream:=AValue;
  FRefStream.IncReference;
end;

function TWCResponse.EncodeContent(const aEncoder : AnsiString; Str : TStream;
  StrSize : Int64; OwnStream : Boolean; ignoreLimits : Boolean) : Boolean;
var
  deflateStream : TDefcompressionstream;
  NeedCompress : Boolean;
  {$IFDEF ALLOW_STREAM_GZIP}
  gzStream : TGzCompressionstream;
  {$ENDIF}
begin
  NeedCompress:= (StrSize > Application.CompressLimit) or ignoreLimits;
  {$IFDEF ALLOW_STREAM_GZIP}
  if SameStr(aEncoder, cSgzip) and NeedCompress then
  begin
    ContentStream := TExtMemoryStream.Create;
    gzStream := Tgzcompressionstream.create(cldefault, ContentStream);
    try
      gzStream.CopyFrom(Str, StrSize);
    finally
      gzStream.Free;
    end;
    FreeContentStream:=true;
    ContentStream.Position:=0;
    ContentLength := ContentStream.Size;
    SetHeader(hhContentEncoding, cSgzip);
    Result := true;
  end else
  {$ENDIF}
  if SameStr(aEncoder, cSdeflate) and NeedCompress then
  begin
    ContentStream := TExtMemoryStream.Create;
    deflateStream := Tdefcompressionstream.create(cldefault, ContentStream);
    try
      deflateStream.CopyFrom(Str, StrSize);
    finally
      deflateStream.Free;
    end;
    FreeContentStream:=true;
    ContentStream.Position:=0;
    ContentLength := ContentStream.Size;
    SetHeader(hhContentEncoding, cSdeflate);
    Result := true;
  end else
  begin
    ContentStream := Str;
    FreeContentStream := OwnStream;
    Str := nil;
    Result := false;
  end;
  if OwnStream and Assigned(Str) then Str.Free;
end;

function TWCResponse.EncodeString(const aEncoder : AnsiString;
  const Str : String; ignoreLimits : Boolean) : Boolean;
var
  StrBuffer : TBufferedStream;
  L : Longint;
begin
  L := Length(Str);
  if ((L > Application.CompressLimit) or ignoreLimits) then
  begin
    StrBuffer := TBufferedStream.Create;
    StrBuffer.SetPtr(@(Str[1]), L);
    Result := EncodeContent(aEncoder, StrBuffer, L, true, true);
  end  else begin
    Content := Str;
    Result := false;
  end;
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
      TWCHTTP2Stream(WCConn.RefRequest).Response.PushData(Pointer(@(S[1])), L);
      TWCHTTP2Stream(WCConn.RefRequest).Response.SerializeData(not KeepStreamAlive);
    end else
    begin
      if Assigned(WCConn.RefCon) then
      begin
        WCConn.RefCon.PushFrame(S);
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
  {$IFDEF WC_WEB_SOCKETS}
  if WCConn.HTTPVersion = wcWebSocket then
  begin
    WCConn.RefRequest := nil;
  end else
  {$ENDIF}
  if WCConn.HTTPVersion = wcHTTP2 then
  begin
    TWCHTTP2Stream(WCConn.RefRequest).Request.Response.Close;
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
  case WCConn.HTTPVersion of
    wcHTTP2 :
    begin
      TWCHTTP2Stream(WCConn.RefRequest).Request.
                 Response.SerializeResponseHeaders(Self,
                                                         (ContentLength = 0) and
                                                         (not FKeepAlive));
    end;
    wcHTTP1_1, wcHTTP1 :
    begin
      S:=Format('HTTP/1.1 %3d %s'#13#10,[Code,HTTP1GetStatusCode(Code)]);
      For I:=0 to Headers.Count-1 do
        S:=S+Headers[i]+#13#10;
      // Last line in headers is empty.
      if Assigned(WCConn.RefCon) then
      begin
        WCConn.RefCon.PushFrame(S);
      end else begin
        WCConn.Socket.WriteBuffer(S[1],Length(S));
      end;
    end;
  end;
end;

procedure TWCResponse.DoSendContent;
begin
  case WCConn.HTTPVersion of
    wcHTTP2 :
    begin
      if Assigned(FRefStream) then begin
        TWCHTTP2Stream(WCConn.RefRequest).Response.SerializeRefStream(FRefStream, true); //close stream
        ReleaseRefStream;
      end
      else
        TWCHTTP2Stream(WCConn.RefRequest).Response.SerializeResponseData(Self, true); //close stream
    end;
    wcHTTP1_1, wcHTTP1{$IFDEF WC_WEB_SOCKETS}, wcWebSocket {$ENDIF} :
    begin
      if Assigned(WCConn.RefCon) then
      begin
        if Assigned(FRefStream) then
        begin
          WCConn.RefCon.PushFrame(FRefStream);
          ReleaseRefStream;
        end else
        begin
          If Assigned(ContentStream) then begin
            WCConn.RefCon.PushFrame(ContentStream, 0, FreeContentStream);
            FreeContentStream := false;
          end
          else
            WCConn.RefCon.PushFrame(Contents);
        end;
      end else begin
        If Assigned(ContentStream) then
          WCConn.Socket.CopyFrom(ContentStream,0)
        else
          Contents.SaveToStream(WCConn.Socket);
      end;
    end;
  end;
end;

{ TWCAppConnection }

procedure TWCAppConnection.DoInitialize;
begin
  if Assigned(RefCon) then
     FProtocolVersion := RefCon.Protocol else
     FProtocolVersion := wcUNK;
  FInputBuf := GetMem(WC_INITIAL_READ_BUFFER_SIZE);
  FInput  := TBufferedStream.Create;
  FInput.SetPtr(FInputBuf, WC_INITIAL_READ_BUFFER_SIZE);
  FRequest := nil;
  FResponse := nil;
  {$IFDEF SERVER_RPC_MODE}
  FClient := nil;
  FSession := nil;
  {$ENDIF}
  {$ifdef DEBUG_STAT}
  FDStartStamp := GetTickCount64;
  FDWaitStamp := FDStartStamp;
  FDStopStamp := FDStartStamp;
  FDReadStamp := FDStartStamp;
  {$endif}
end;

function TWCAppConnection.ReadLine: String;
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

function TWCAppConnection.ReadReqHeaders: TWCRequest;
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
      Result.RemoteAddress := WCSocketAddrToString(Socket.RemoteAddress);
      Result.ServerPort := TWCHttpServer(Server).Port;
    end else
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TWCAppConnection.ConvertFromRefRequest(AReq: TWCRequestRefWrapper): TWCRequest;
begin
  if not assigned(AReq) then Exit(nil);
  Result:=TWCRequest(TWCHttpServer(Server).CreateRequest);
  try
    TWCHttpServer(Server).InitRequest(Result);
    Result.SetConnection(Self);
    AReq.CopyToHTTP1Request(Result);
    TWCContent(Result.ContentObject).RequestRef := aReq;
    Result.InitRequestVars(False);
    Result.RemoteAddress := WCSocketAddrToString(Socket.RemoteAddress);
    Result.ServerPort := TWCHttpServer(Server).Port;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

procedure TWCAppConnection.ReadReqContent(ARequest: TWCRequest);
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

procedure TWCAppConnection.ConsumeHeader(ARequest: TAbsHTTPConnectionRequest;
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

procedure TWCAppConnection.UnknownHeader({%H-}ARequest: TAbsHTTPConnectionRequest;
  const {%H-}AHeader: String);
begin
  // do nothing
end;

procedure TWCAppConnection.DoSocketAttach(ASocket: TSocketStream);
begin
  inherited DoSocketAttach(ASocket);
  Application.SocketsCollector.Add(SocketReference);
end;

constructor TWCAppConnection.Create(AServer: TAbsCustomHTTPServer;
  ASocket: TSocketStream);
begin
  inherited Create(AServer, ASocket);
  DoInitialize;
end;

constructor TWCAppConnection.CreateRefered(AServer: TAbsCustomHTTPServer;
  ASocketRef: TWCSocketReference; AConRef : TWCRefConnection);
begin
  inherited CreateRefered(AServer, ASocketRef, AConRef);
  DoInitialize;
end;

function TWCAppConnection.ConsumeSocketData: TWCConsumeResult;
var r : integer;
    h2openmode : THTTP2OpenMode;
    {$IFDEF WC_WEB_SOCKETS}
    wsopenmode : TWebSocketUpgradeOptions;
    {$ENDIF}
begin
  if not TAbsSocketStream(Socket).Accepted then
  if not TWCHttpServer(Server).AcceptSocket(TAbsSocketStream(Socket)) then
  begin
    Result := wccrSocketError;
    Exit;
  end;

  Result := wccrOK;
  try
    if assigned(RefCon) then
      FProtocolVersion := RefCon.Protocol;

    try
      r:=SocketReference.Read(FInputBuf^, WC_INITIAL_READ_BUFFER_SIZE);
      If r < 0 then begin
        Result := wccrSocketError;
        Raise ESocketError.Create(WCSocketReadError);
      end;
      FInput.SetPtr(FInputBuf, r);  //resize buffered stream

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
            FRequest.InitRequestVars(True);
            //check here if http1.1 upgrade to other protocol
            if SameStr(FRequest.ProtocolVersion, '1.1') then
            begin
              if Pos(HTTPUpgradeSubHeader,
                     LowerCase(FRequest.GetHeader(hhConnection))) > 0 then
              begin
                FProtocolVersion := wcUNK;
                {$IFDEF WC_WEB_SOCKETS}
                wsopenmode := TWCWebSocketConnection.TryToUpgradeFromHTTP(FRequest,
                                                                          TWCHttpServer(Server).WebSocketSettings);
                if assigned(wsopenmode) then
                begin
                  FProtocolVersion := wcWebSocket;
                  RefCon := TWCHttpServer(Server).AttachNewWebSocketCon(SocketReference,
                                                                        wsopenmode,
                                                                        @(TWCHttpServer(Server).DoReadData),
                                                                        @(TWCHttpServer(Server).DoSendData));
                  RefCon.IncReference; // reference not incremented here, need to increment
                end;
                {$ENDIF}
                //here can be implemented simple mechanism for transitioning
                //from HTTP/1.1 to HTTP/2 according RFC 7540 (Section 3.2)
                //
                //this mechanism is not implemented due to its rare use
              end else
              if not Assigned(RefCon) then
              begin
                if Pos(HTTPKeepAliveSubHeader,
                       LowerCase(FRequest.GetHeader(hhConnection))) > 0 then
                begin
                  RefCon := TWCHttpServer(Server).AttachNewHTTP11Con(SocketReference,
                                                                     @(TWCHttpServer(Server).DoReadData),
                                                                     @(TWCHttpServer(Server).DoSendData));
                  RefCon.IncReference; // reference not incremented here, need to increment
                end;
              end;
            end;

            if FProtocolVersion = wcUNK then
              Result := wccrWrongProtocol else
            begin
              if FProtocolVersion in [wcHTTP1, wcHTTP1_1] then
              begin
                // Read content, if any
                If FRequest.ContentLength>0 then begin
                  ReadReqContent(FRequest);
                  FRequest.DecodeContent;
                end;
              end;
              Result := wccrOK;
            end;
          end else begin
            FProtocolVersion:= wcUNK;
            Result := wccrWrongProtocol;
          end;
        end else Result := wccrWrongProtocol;
      end else Result := wccrNoData;
      if FProtocolVersion = wcHTTP2 then
      begin
        // read http/2 frames
        // RFC 7540
        // consume socket data, pop new request
        Result := wccrOK;
        if not Assigned(RefCon) then
        begin
          RefCon := TWCHttpServer(Server).AttachNewHTTP2Con(SocketReference,
                                                            h2openmode,
                                                            @(TWCHttpServer(Server).DoReadData),
                                                            @(TWCHttpServer(Server).DoSendData));
          RefCon.IncReference; // reference not incremented here, need to increment
        end;
        if (FInput.Size - FInput.Position) > 0 then
           RefCon.ConsumeNextFrame(FInput);
        RefRequest := TWCHTTP2Connection(RefCon).PopRequestedStream;
        if Assigned(RefRequest) then begin
          FRequest := ConvertFromRefRequest(RefRequest);
          // check Malformed Requests
          if FRequest.ContentLength <> TWCHTTP2Stream(RefRequest).Request.DataBlockSize then
          begin
            TWCHTTP2Connection(RefCon).ResetStream(TWCHTTP2Stream(RefRequest).ID,
                                                    H2E_PROTOCOL_ERROR);
            TWCHTTP2Connection(RefCon).GoAway(H2E_PROTOCOL_ERROR);
            Result := wccrProtocolError;
          end else
            FRequest.DecodeContent;
        end else
          Result := wccrNoData;
      end;
      {$IFDEF WC_WEB_SOCKETS}
      if FProtocolVersion = wcWebSocket then
      begin
        Result := wccrOK;
        if (FInput.Size - FInput.Position) > 0 then
           RefCon.ConsumeNextFrame(FInput);
        RefRequest := TWCWebSocketConnection(RefCon).PopReadyChunck;
        if Assigned(RefRequest) then
        begin
          if not assigned(FRequest) then
            FRequest := ConvertFromRefRequest(RefRequest);
        end else
          Result := wccrNoData;
      end;
      {$ENDIF}
    finally
      if Assigned(RefCon) then
         RefCon.ReleaseRead(Result);
    end;
    if Result = wccrOK then
    begin
      // Create Response
      FResponse:= TWCResponse(TWCHttpServer(Server).CreateResponse(FRequest));
      TWCHttpServer(Server).InitResponse(FResponse);
      FResponse.SetConnection(Self);
    end;
  Except
    On E : Exception do begin
      if Result in [wccrNoData, wccrOK] then
        Result := wccrProtocolError;
      if Assigned(RefCon) then RefCon.ConnectionState:=wcDROPPED;
      if Assigned(FRequest) then FreeAndNil(FRequest);
      if Assigned(FResponse) then FreeAndNil(FResponse);
      HandleRequestError(E);
    end;
  end;
end;

{$IFDEF SERVER_RPC_MODE}
procedure TWCAppConnection.SetSessionParams(aClient: TWebClient;
  aSession: TSqliteWebSession);
begin
  FClient := aClient;
  if Assigned(FClient) then FClient.IncReference;
  FSession := aSession;
end;
{$ENDIF}

destructor TWCAppConnection.Destroy;
{$ifdef DEBUG_STAT}
var DT : Integer;
{$endif}
begin
  if Assigned(FRequest) then FreeAndNil(FRequest);
  if Assigned(FResponse) then FreeAndNil(FResponse);
  {$IFDEF SERVER_RPC_MODE}
  if Assigned(FSession) then FreeAndNil(FSession);
  if Assigned(FClient) then FClient.DecReference;
  {$ENDIF}
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

{ TWCMainClientJob }

{$IFDEF SERVER_RPC_MODE}
function TWCMainClientJob.GetClient: TWebClient;
begin
  Result := Connection.Client;
end;
{$ENDIF}

function TWCMainClientJob.GetParPtr(index: LongWord): PVariant;
begin
  index := FindOrAddParamId(index);
  Result := @( FParams.AtPosPt[index]^.Data );
end;

function TWCMainClientJob.GetParam(index: LongWord): Variant;
begin
  index := FindOrAddParamId(index);
  Result := FParams.AtPosPt[index]^.Data;
end;

function TWCMainClientJob.GetRequest: TWCRequest;
begin
  Result := Connection.Request;
end;

function TWCMainClientJob.GetResponse: TWCResponse;
begin
  Result := Connection.Response;
end;

procedure TWCMainClientJob.SetParam(index: LongWord; AValue: Variant);
begin
  index := FindOrAddParamId(index);
  FParams.AtPosPt[index]^.Data := AValue;
end;

function TWCMainClientJob.FindOrAddParamId(aId: LongWord): integer;
begin
  Result := FParams.FindIndexOf(aId);
  if Result < 0 then
    Result := FParams.Add(aId, null);
end;

constructor TWCMainClientJob.Create(aConn: TWCAppConnection);
begin
  {$IFDEF SERVER_RPC_MODE}
  if assigned(aConn.Client) then begin
    inherited Create(aConn.Client.Score);
    aConn.Client.UpdateScore;
  end else
  {$ENDIF}
    inherited Create( GetTickCount64 div 1000 );

  FConn := aConn;
  FResponseReadyToSend := true;
  FParams := TFastHashList.Create;
end;

destructor TWCMainClientJob.Destroy;
begin
  if Assigned(FConn) then begin
    FConn.Free;
    FConn := nil;
  end;
  FParams.Free;
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
  FIPBlocked := TWCIPBlockList.Create;
  OnAllowConnect := @InternalAllowConnect;
  FThreadPool := nil;
  FPoolsLocker := TNetCustomLockedObject.Create;
  FSSLLocker := TNetCustomLockedObject.Create;
  RefConnections.RegisterProtocolHelper(wcHTTP2, TWCHTTP2ServerHelper);
  {$IFDEF WC_WEB_SOCKETS}
  RefConnections.RegisterProtocolHelper(wcWebSocket, TWCWebSocketServerHelper);
  {$ENDIF}
end;

procedure TWCHttpServer.CreateServerSocket;
begin
 {$ifdef USE_GLOBAL_SSL_CONTEXT}
  if UseSSL then
    InitGlobalSSLContext;
 {$ENDIF}
  inherited CreateServerSocket;
end;

{$ifdef USE_GLOBAL_SSL_CONTEXT}
Function HandleSSLPwd(buf : PAnsiChar; len:Integer; {%H-}flags:Integer; UD : Pointer):Integer; cdecl;
var
  Pwd: AnsiString;
  H :  TWCHttpServer;
begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TWCHttpServer(UD);
    Pwd:=H.CertificateData.KeyPassword;
    end;
  if (len<Length(Pwd)+1) then
    SetLength(Pwd,len-1);
  pwd:=pwd+#0;
  Result:=Length(Pwd);
  Move(Pointer(Pwd)^,Buf^,Result);
end;

procedure TWCHttpServer.InitGlobalSSLContext;
var S : String;
begin
  if not assigned(FSSLContext) then
  begin
    FSSLContext:=TExtSSLContext.Create(SSLType);
    S := CertificateData.CipherList;
    FSSLContext.SetCipherList(S);
    FSSLContext.SetVerify(0,Nil);
    FSSLContext.SetDefaultPasswdCb(@HandleSSLPwd);
    FSSLContext.SetDefaultPasswdCbUserdata(self);
    if Assigned(AlpnList) and
       (AlpnList.Count > 0) then
       FSSLContext.SetAlpnSelect(@AlpnSelect);
    if not CertificateData.Certificate.Empty then
      FSSLContext.UseCertificate(CertificateData.Certificate);
    if not CertificateData.PrivateKey.Empty then
      FSSLContext.UsePrivateKey(CertificateData.PrivateKey);
    if (CertificateData.CertCA.FileName<>'') then
      FSSLContext.LoadVerifyLocations(CertificateData.CertCA.FileName,'');
    if not CertificateData.PFX.Empty then
      FSSLContext.LoadPFX(CertificateData.PFX,CertificateData.KeyPassword);
  end;
end;

function TWCHttpServer.AlpnSelect(outv: PPChar; outl: PChar;
  inv: PChar; inlen: Cardinal): integer;
const
  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_NOACK = 3;
var
  protlen : Byte;
  prot : PChar;
  i : integer;
  id : string;
begin
  protlen := 0;
  prot := inv;
  id := '';

  while (prot < pointer(inv + inlen)) do begin
      protlen := PByte(prot)^;
      Inc(prot);
      if (pointer(inv + inlen) < pointer(prot + protlen)) then
          Exit(SSL_TLSEXT_ERR_NOACK);

      SetLength(id, protlen);
      Move(PByteArray(prot)^, id[1], protlen);
      for i := 0 to AlpnList.Count-1 do
      begin
        if SameText(id, AlpnList[i]) then begin
            outv^ := prot;
            PByte(outl)^ := protlen;
            Exit(SSL_TLSEXT_ERR_OK);
        end;
      end;
      Inc(prot, protlen);
  end;

  Result := SSL_TLSEXT_ERR_NOACK;
end;
{$ENDIF}

function TWCHttpServer.CreateSSLSocketHandler: TSocketHandler;
begin
  FSSLLocker.Lock;
  try
    Result:=inherited CreateSSLSocketHandler;
    if assigned(Result) then
    begin
      TExtOpenSSLSocketHandler(Result).AlpnList := AlpnList.Text;
      TExtOpenSSLSocketHandler(Result).SSLMasterKeyLog:= SSLMasterKeyLog;
      {$ifdef USE_GLOBAL_SSL_CONTEXT}
      InitGlobalSSLContext;
      if Assigned(FSSLContext) then
        TExtOpenSSLSocketHandler(Result).GlobalContext := FSSLContext;
      {$ENDIF}
    end;
  finally
    FSSLLocker.UnLock;
  end;
end;

procedure TWCHttpServer.CreateConnectionThread(Conn: TAbsHTTPConnection);
var PreJob : TWCPreAnalizeJob;
begin
  if assigned(Application.ServerAnalizeJobClass) then
     PreJob := Application.ServerAnalizeJobClass.Create(TWCAppConnection(Conn))
  else
  {$IFDEF SERVER_RPC_MODE}
     PreJob := TWCPreAnalizeClientJob.Create(TWCAppConnection(Conn));
  {$ELSE}
     PreJob := TWCPreAnalizeNoSessionNoClientJob.Create(TWCAppConnection(Conn));
  {$ENDIF}

  CheckThreadPool;

  AddLinearJob(PreJob);
end;

function TWCHttpServer.CreateConnection(Data: TSocketStream): TAbsHTTPConnection;
begin
  Result:= TWCAppConnection.Create(Self, Data);
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

function TWCHttpServer.AttachRefCon(aRefCon : TWCRefConnection
  ) : TWCRefConnection;
begin
  Result := aRefCon;
  Application.GarbageCollector.Add(Result);
  RefConnections.AddConnection(Result);
end;

procedure TWCHttpServer.HandleNetworkError(E : Exception);
begin
  Try
    Application.DoError('Error (%s) : %s', [E.ClassName, E.Message]);
  except
    // Do not let errors escape
  end;
end;

procedure TWCHttpServer.HandleNetworkLog(const S : String);
begin
  Try
    Application.DoInfo(S);
  except
    // Do not let errors escape
  end;
end;

procedure TWCHttpServer.HandleNetworkLog(const S : String;
  const Params : array of const);
begin
  Try
    Application.DoInfo(S, Params);
  except
    // Do not let errors escape
  end;
end;

function TWCHttpServer.AttachNewHTTP2Con(aSocket : TWCSocketReference;
  aOpenMode : THTTP2OpenMode; aReadData, aSendData : TRefReadSendData
  ) : TWCHTTP2Connection;
begin
  Result := TWCHTTP2Connection(AttachRefCon(
            TWCHTTP2Connection.Create(RefConnections,
                                      aSocket,
                                      aOpenMode,
                                      aReadData,
                                      aSendData)));
end;

function TWCHttpServer.AttachNewHTTP11Con(aSocket : TWCSocketReference;
  aReadData, aSendData : TRefReadSendData) : TWCHTTP11Connection;
begin
  Result := TWCHTTP11Connection(AttachRefCon(
            TWCHTTP11Connection.Create(RefConnections,
                                       aSocket,
                                       aReadData,
                                       aSendData)));
end;

{$IFDEF WC_WEB_SOCKETS}
function TWCHttpServer.AttachNewWebSocketCon(aSocket : TWCSocketReference;
  aOpenMode : TWebSocketUpgradeOptions; aReadData, aSendData : TRefReadSendData
  ) : TWCWebSocketConnection;
begin
  Result := TWCWebSocketConnection(AttachRefCon(
            TWCWebSocketConnection.Create(RefConnections,
                                          aOpenMode,
                                          aSocket,
                                          aReadData,
                                          aSendData)));
end;
{$ENDIF}

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

function TWCHttpServer.GetHTTP2Settings: TWCHTTP2Settings;
begin
  Result := TWCHTTP2Helper(RefConnections.Protocol[wcHTTP2]).Settings;
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

procedure TWCHttpServer.InternalAllowConnect(Sender : TObject;
  ASocket : Longint; var Allow : Boolean);
var aIP : String;
begin
  // check in blocked list
  aIP := WCSocketAddrToString(WCGetRemoteAddress(ASocket));
  Allow := not Assigned(FIPBlocked.FindIP(aIP));
end;

{$IFDEF WC_WEB_SOCKETS}
function TWCHttpServer.GetWebSocketSettings: TWCWebSocketSettings;
begin
  Result := TWCWebSocketHelper(RefConnections.Protocol[wcWebSocket]).Settings;
end;
{$ENDIF}

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

procedure TWCHttpServer.DoReadData(aConnection : TWCRefConnection);
Var
  Con : TWCAppConnection;
begin
  Con:=TWCAppConnection.CreateRefered(Self, nil, aConnection);
  Con.OnRequestError:=@HandleRequestError;

  CreateConnectionThread(Con);
end;

procedure TWCHttpServer.DoSendData(aConnection: TWCRefConnection);
begin
  AddLinearJob(TWCHttpRefSendDataJob.Create(aConnection));
end;

destructor TWCHttpServer.Destroy;
begin
  if Assigned(FThreadPool) then FThreadPool.Free;
  FPoolsLocker.Free;
  FSSLLocker.Free;
  FIPBlocked.Free;
  inherited Destroy;
end;

procedure TWCHttpServer.CoolDownIP(const vIP : String; forMinutes : Integer);
var obj : TWCIPBlocked;
begin
  obj := FIPBlocked.FindIP(vIP);
  if assigned(obj) then
    obj.CoolDown(forMinutes) else
  begin
    obj := TWCIPBlocked.Create(vIP);
    obj.CoolDown(forMinutes);
    FIPBlocked.Push_back(obj);
  end;
end;

procedure TWCHttpServer.UnfreezeIPs(forSecs : Integer);
begin
  FIPBlocked.Unfreeze(forSecs);
  FIPBlocked.FreeUnblocked;
end;

{ TWCHttpServerHandler }

procedure TWCHttpServerHandler.HandleRequestError(Sender: TObject; E: Exception
  );
begin
  Try
    Application.DoError('Error (%s) handling request : %s',[E.ClassName, E.Message]);
  except
    // Do not let errors escape
  end;
end;

function TWCHttpServerHandler.CreateServer: TEmbeddedAbsHttpServer;
begin
  Result:=TWCHttpServer.Create(Self);
end;

function TWCHttpServerHandler.GetWCServer : TWCHttpServer;
begin
  Result := TWCHttpServer(HTTPServer);
end;

{$IFDEF SERVER_RPC_MODE}

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

procedure TWebClient.CompressString(AResponse : TWCResponse; const S : String;
  ignoreLimits : Boolean);
var
  StrBuffer : TBufferedStream;
  L : Longint;
begin
  L := Length(S);
  if ((L > Application.CompressLimit) or ignoreLimits) and
     ({$IFDEF ALLOW_STREAM_GZIP} AcceptGzip or {$ENDIF}AcceptDeflate) then
  begin
    StrBuffer := TBufferedStream.Create;
    StrBuffer.SetPtr(@(S[1]), L);
    CompressStream(AResponse, StrBuffer, L, true, true);
  end  else
    AResponse.Content:=S;
end;

procedure TWebClient.CompressStream(AResponse : TWCResponse; Str : TStream;
  StrSize : Int64; OwnStream : Boolean; ignoreLimits : Boolean);
begin
  {$IFDEF ALLOW_STREAM_GZIP}
  if AcceptGzip then
  begin
    AResponse.EncodeContent(cSgzip, Str, StrSize, OwnStream, ignoreLimits);
  end else
  {$ENDIF}
  if AcceptDeflate then
  begin
    AResponse.EncodeContent(cSdeflate, Str, StrSize, OwnStream, ignoreLimits);
  end else
  begin
    AResponse.ContentStream := Str;
    AResponse.FreeContentStream := OwnStream;
  end;
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
  if Result and Assigned(WebContainer) and WebContainer.Verbose then
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

{$ENDIF}

{ TWebCacheCollection }

function TWebCacheCollection.GetCache(const index: String): TWebCachedItem;
begin
  Result := TWebCachedItem(FHash[index]);
end;

function TWebCacheCollection.GetCacheSizeLimit : QWord;
begin
  Result := FCacheSizeLimit.Value;
end;

procedure TWebCacheCollection.SetCacheSizeLimit(AValue : QWord);
begin
  if FCacheSizeLimit.Value <> AValue then
  begin
    FCacheSizeLimit.Value := AValue;
    RebuildWebCache;
  end;
end;

procedure TWebCacheCollection.PushCachedSize(NewSize : QWord);
begin
  FCachedSize.IncValue(NewSize);
end;

procedure TWebCacheCollection.ExtractCachedSize(OldSize : QWord);
begin
  FCachedSize.DecValue(OldSize);
end;

function RebuildWebCacheSortFunc(obj1: TObject; obj2 : TObject) : Integer;
var aa1, aa2 : integer;
    sz1, sz2 : QWord;
begin
  aa1 := TWebCachedItem(obj1).AccessAge;
  aa2 := TWebCachedItem(obj2).AccessAge;
  if (aa1 >= 60) and
     (aa2 < 60) then
    Result := 1
  else
  if (aa1 < 60) and
     (aa2 >= 60) then
    Result := -1
  else
  begin
    sz1 := TWebCachedItem(obj1).TotalCachedSize;
    sz2 := TWebCachedItem(obj2).TotalCachedSize;
    if (aa1 >= 60) and
       (aa2 >= 60) then
    begin
      Result := CompareValue(sz1, sz2);
    end
    else
    begin
      Result := CompareValue(sz2, sz1);
    end;
  end;
end;

procedure TWebCacheCollection.RebuildWebCache;
var
  FL : TFastList;
  it : integer;
begin
  if (FCacheSizeLimit.Value < FCachedSize.Value) and
     (Count > 1) then
  begin
    FL := TFastList.Create;
    Lock;
    try
      for it := 0 to Count-1 do
        FL.Add(Item[it]);

      FL.SortList(@RebuildWebCacheSortFunc);
      it := 0;
      while FCacheSizeLimit.Value < FCachedSize.Value do
      begin
        TWebCachedItem(FL[it]).Clear;
        Inc(it);
        if (it > 9) or (it >= (FL.Count-1)) then break;
      end;
    finally
      UnLock;
      FL.Free;
    end;
  end;
end;

constructor TWebCacheCollection.Create;
begin
  inherited Create;
  FHash := TStringHashList.Create(true);
  FCacheSizeLimit := TThreadQWord.Create($20000000);
  FCachedSize := TThreadQWord.Create(0);
end;

destructor TWebCacheCollection.Destroy;
begin
  FHash.Free;
  FCacheSizeLimit.Free;
  FCachedSize.Free;
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
  FCachedSize.Value := 0;
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
  if Assigned(WCServer) then WCServer.StopThreads;
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

procedure TWCHTTPApplication.SoftCloseIP(Obj : TObject; ptrIP : Pointer);
begin
  if SameText(WCSocketAddrToString(TWCSocketReference(Obj).Socket.RemoteAddress),
              Pchar(ptrIP)) then
  begin
    DoInfo('Ip %s is blocked and closed', [StrPas(Pchar(ptrIP))]);
    TWCSocketReference(Obj).SoftClose;
  end;
end;

procedure TWCHTTPApplication.DoOnConfigChanged(Sender: TWCConfigRecord);
var JTJ : TJobToJobWait;
begin
  if not VarIsNull(Sender.Value) then
  begin
    case Sender.HashName of
      CFG_SHUTDOWN :
        NeedShutdown := Sender.Value;
      CFG_RESTART :
        NeedRestartServerSocket := Sender.Value;
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
      CFG_IO_THREAD_CNT :
        MaxIOThreads := Sender.Value;
      //openssl
      CFG_SSL_VER : begin
        if ConfigChangeHalt then
        begin
          WCServer.FSSLLocker.Lock;
          try
            WCServer.SSLType := ExSSLStringToType(Sender.Value);
          finally
            WCServer.FSSLLocker.UnLock;
          end;
        end;
      end;
      CFG_SSL_CIPHER : begin
        WCServer.FSSLLocker.Lock;
        try
          WCServer.CertificateData.CipherList := Sender.Value;
        finally
          WCServer.FSSLLocker.UnLock;
        end;
      end;
      CFG_PRIVATE_KEY    :
        WCServer.PrivateKey := Sender.Value;
      CFG_CERTIFICATE    :
        WCServer.Certificate := Sender.Value;
      CFG_TLSKEY_LOG     :
        WCServer.SSLMasterKeyLog := Sender.Value;
      CFG_ALPN_USE_HTTP2 : begin
        WCServer.FSSLLocker.Lock;
        try
          WCServer.AlpnList.Clear;
          if Sender.Value then WCServer.AlpnList.Add('h2');
          WCServer.AlpnList.Add('http/1.1');
        finally
          WCServer.FSSLLocker.UnLock;
        end;
      end;
      //clients
      CFG_CLIENT_COOKIE_MAX_AGE :
         ClientCookieMaxAge := Sender.Value;
      CFG_CLIENT_TIMEOUT :
         ClientTimeOut := Sender.Value;
      CFG_CLIENT_ALLOW_ENCODE :
         ClientAllowEncode := Sender.Value;
      CFG_CLIENT_VERBOSE :
      {$IFDEF SERVER_RPC_MODE}
         if Assigned(WebContainer) then
           WebContainer.Verbose := Sender.Value{$ENDIF};
      //http2
      CFG_H2SET_HEADER_TABLE_SIZE,
      CFG_H2SET_MAX_CONCURRENT_STREAMS,
      CFG_H2SET_INITIAL_WINDOW_SIZE,
      CFG_H2SET_MAX_FRAME_SIZE,
      CFG_H2SET_MAX_HEADER_LIST_SIZE: begin
        WCServer.HTTP2Settings.Add((Sender.HashName shr 4) and $0f, Sender.Value);
      end;
      {$IFDEF WC_WEB_SOCKETS}
      CFG_WEBSOCKET_SUB_PROTO:
        WCServer.WebSocketSettings.SubProtocols := Sender.Value;
      {$ENDIF}
      //Web files
      CFG_COMPRESS_LIMIT :
        CompressLimit := Sender.Value;
      CFG_CACHE_LIMIT_SIZE :
        WebFilesCacheLimit := Sender.Value;
      CFG_IGNORE_FILES :
        WebFilesIgnore := Sender.Value;
      CFG_EXCLUDE_IGNORE_FILES :
        WebFilesExcludeIgnore := Sender.Value;
      //TimeStamps
      CFG_TIMESTAMPLOG :
        TimeStampLog.Enabled := Sender.Value;
      CFG_TIMESTAMPLOG_PERIOD :
        TimeStampLog.Period := Sender.Value;
      CFG_TIMESTAMPLOG_FORMAT :
        TimeStampLog.Format := Sender.Value;
    else
      FAppHelpers.DoHelp(TWCHTTPAppConfigRecordHelper, Sender);
    end;
  end;
end;

procedure TWCHTTPApplication.DoOnLoggerException(Sender: TObject; E: Exception);
begin
  DoError(E.Message);
end;

procedure TWCHTTPApplication.DoGetModule(Sender: TObject;
  {%H-}ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := nil;
end;

constructor TWCHTTPApplication.Create(AOwner: TComponent);
var I : integer;
begin
  inherited Create(AOwner);

  FAppHelpers := TWCHTTPAppHelpers.Create;

  FStartStamp := 0;
  FConfig := nil;
  FWebFilesIgnoreRx := nil;
  FWebFilesExceptIgnoreRx := nil;
  FTimeStampLog := TWCTimeStampLog.Create;

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

  {$IFDEF SERVER_RPC_MODE}
  FWebClientClass := TWebClient;
  {$ENDIF}
end;

destructor TWCHTTPApplication.Destroy;
begin
  FAppHelpers.DoHelp(TWCHTTPAppDoneHelper, nil);
  DoInfo('Server stopped');
  {$ifdef NOGUI}{$IFDEF UNIX}
  GWidgetHelper.Free;
  {$endif}{$endif}
  // first we need to dec references to all referenced objects
  // wait all jobs and kill threads
  StopThreads;
  {$IFDEF SERVER_RPC_MODE}
  // dec references to all clients
  WebContainer.ClearDeadClients;
  {$ENDIF}
  // dec references to all connections
  if assigned(WCServer) then begin
    WCServer.RefConnections.CloseAll;
    WCServer.RefConnections.RemoveDeadConnections(GetTickCount64, 0);
  end;
  // clear cache (referenced streams)
  if assigned(WebContainer) then
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

  FAppHelpers.Free;
  FTimeStampLog.Free;
  inherited Destroy;
end;

procedure TWCHTTPApplication.DoOnIdle({%H-}sender: TObject);
var Stamp : TWCTimeStampObj;
begin
  {$ifdef NOGUI} {$IFDEF UNIX}
  GWidgetHelper.ProcessMessages;
  {$endif}  {$endif}
  Stamp := TWCTimeStampObj.Create(GetTickCount64);
  try
    if (Stamp.Tick - FMTime) >= 10000 then  //every 10 secs
    begin
      WCServer.UnfreezeIPs((Stamp.Tick - FMTime) div 1000);
      FMTime := Stamp.Tick;
      if assigned(FConfig) then FConfig.Sync(false);
      WebContainer.DoMaintainingStep;
      GarbageCollector.CleanDead;
      FSocketsReferences.CleanDead;
      FTimeStampLog.TryToLog(Stamp);
    end;

    FAppHelpers.DoHelp(TWCHTTPAppIdleHelper, Stamp);
    //
    WCServer.RefConnections.Idle(Stamp.Tick);
    //
  finally
    Stamp.Free;
  end;
  Sleep(5);
  if FNeedShutdown.Value then
  begin
    WCServer.Active := false;
    Terminate;
  end;
end;

function TWCHTTPApplication.GetCacheLimit : QWord;
begin
  Result := WebContainer.CacheSizeLimit;
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

function TWCHTTPApplication.GetWCServer : TWCHttpServer;
begin
  Result := GetWebHandler.GetWCServer;
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

function TWCHTTPApplication.GetMaxIOThreads : Byte;
begin
  if Assigned(WCServer) and
     Assigned(WCServer.RefConnections) then
    Result := WCServer.RefConnections.MaxIOThreads else
    Result := 0;
end;

function TWCHTTPApplication.GetMimeLoc: String;
begin
  Result := FMimeLoc.Value;
end;

function TWCHTTPApplication.GetNeedShutdown: Boolean;
begin
  Result := FNeedShutdown.Value;
end;

function TWCHTTPApplication.GetNeedRestartServerSocket : Boolean;
begin
  if Assigned(WCServer) then
    Result := WCServer.NeedServerSocketRestart;
end;

procedure TWCHTTPApplication.SetNeedRestartServerSocket(AValue : Boolean);
begin
  if Assigned(WCServer) then
    WCServer.NeedServerSocketRestart := AValue;
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

procedure TWCHTTPApplication.SetCacheLimit(AValue : QWord);
begin
  WebContainer.CacheSizeLimit := AValue;
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
    {$IFDEF SERVER_RPC_MODE}
    if assigned(WebContainer) then
      WebContainer.Sessions.DefaultTimeOutMinutes:= AValue;
    {$ENDIF}
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
    FAppHelpers.DoHelp(TWCHTTPAppConfigInitHelper, FConfig);
  end;
  FConfig.Sync(true);
end;

procedure TWCHTTPApplication.SetJobToJobWait(AValue: TJobToJobWait);
begin
  if assigned(WCServer) and
     Assigned(WCServer.FThreadPool) then begin
     WCServer.FThreadPool.ThreadJobToJobWait := AValue;
     FThreadJobToJobWait.Value := WCServer.FThreadPool.ThreadJobToJobWait;
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
  if assigned(WCServer) and
     assigned(WCServer.FThreadPool) then
     WCServer.FThreadPool.SortedThreadsCount:=aValue;
end;

procedure TWCHTTPApplication.SetMaxPrepareThreads(AValue: Byte);
begin
  if AValue = 0 then AValue := 1;
  if AValue > WC_MAX_PREP_THREADS then AValue := WC_MAX_PREP_THREADS;
  if FMaxPrepareThreads.Value=AValue then Exit;

  FMaxPrepareThreads.Value:=AValue;
  if assigned(WCServer) and
     assigned(WCServer.FThreadPool) then
     WCServer.FThreadPool.LinearThreadsCount:=aValue;
end;

procedure TWCHTTPApplication.SetMaxIOThreads(AValue : Byte);
begin
  if AValue = 0 then AValue := 1;
  if AValue > WC_MAX_IO_THREADS then AValue := WC_MAX_IO_THREADS;

  if assigned(WCServer) and
     assigned(WCServer.RefConnections) then
     WCServer.RefConnections.MaxIOThreads := AValue;
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
    if assigned(WCServer) then
      WCServer.SSLLoc := ExtractFilePath(ExeName) + AValue;
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
        if assigned(WCServer) then
          WCServer.SSLLoc:= loc + FSSLLoc.Value;
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

procedure TWCHTTPApplication.SendError(AResponse: TAbsHTTPConnectionResponse; errno: Integer);
begin
  AResponse.Code:=errno;
  AResponse.Content:='';
  if not AResponse.HeadersSent then
    AResponse.SendHeaders;
end;

procedure TWCHTTPApplication.AddHelper(Hlp : TWCHTTPAppHelper);
begin
  FAppHelpers.Push_back(Hlp);
end;

procedure TWCHTTPApplication.CoolDownIP(const vIP : String; forMinutes : Integer
  );
begin
  WCServer.CoolDownIP(vIP, forMinutes);
  SocketsCollector.DoForAllEx(@SoftCloseIP, PChar(vIP));
end;

procedure TWCHTTPApplication.Initialize;
begin
  inherited Initialize;

  FReferences := TNetReferenceList.Create;
  FSocketsReferences :=TNetReferenceList.Create;
  WebContainer := TWebClientsContainer.Create;
  GetWebHandler.OnAcceptIdle:= @DoOnIdle;
  GetWebHandler.AcceptIdleTimeout:=5;
  WCServer.RefConnections.GarbageCollector := FReferences;

  FStartStamp:= GetTickCount64;
  DoInfo('Server initialized');
  FAppHelpers.DoHelp(TWCHTTPAppInitHelper, nil);
end;

function TWCHTTPApplication.GetWebHandler: TWCHttpServerHandler;
begin
  Result := TWCHttpServerHandler(WebHandler);
end;

function TWCHTTPApplication.InitializeAbstractWebHandler: TWebHandler;
begin
  Result := TWCHttpServerHandler.Create(Self);
end;

function TWCHTTPApplication.GetTimeSecFromStart: Cardinal;
begin
  Result := (GetTickCount64 - FStartStamp) div 1000;
end;

function TWCHTTPApplication.CreateRefMemoryStream : TRefMemoryStream;
begin
  Result := TRefMemoryStream.Create;
  Application.GarbageCollector.Add(Result);
end;

function TWCHTTPApplication.CreateSizedRefMemoryStream(aSz : PtrInt
  ) : TRefMemoryStream;
begin
  Result := TRefMemoryStream.Create(aSz);
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

function TWebCachedItem.GetAccessAge : Integer;
begin
  Lock;
  try
    Result := SecondsBetween(Now, FAccessTime);
  finally
    UnLock;
  end;
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

function TWebCachedItem.GetTotalCachedSize : QWord;
begin
  Lock;
  try
    Result := FSize + FDeflateSize + FGzipSize;
  finally
    UnLock;
  end;
end;

constructor TWebCachedItem.Create(aOwner : TWebCacheCollection; const aLoc,
  aURI : String);
begin
  inherited Create;
  FOwner := aOwner;
  FCacheControl := TThreadUtf8String.Create('');
  FCharset := TThreadUtf8String.Create('');
  FNeedToCompress := TThreadBoolean.Create(false);
  FCache := nil;
  FSize := 0;
  FDeflateCache := nil;
  FDeflateSize := 0;
  {$IFDEF ALLOW_STREAM_GZIP}
  FGzipCache := nil;
  FGzipSize := 0;
  {$ENDIF}
  Clear;
  FLoc := aLoc;
  FURI := aURI;
  FDataTime := EncodeDate(1990, 1, 1);
  FAccessTime := EncodeDate(1990, 1, 1);
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
  if FGzipSize > 0 then begin
    FOwner.ExtractCachedSize(FGzipSize);
    FGzipSize := 0;
  end;
  {$ENDIF}
  if Assigned(FDeflateCache) then FDeflateCache.DecReference;
  FDeflateCache := nil;
  if FDeflateSize > 0 then begin
    FOwner.ExtractCachedSize(FDeflateSize);
    FDeflateSize := 0;
  end;
  FCache := Application.CreateRefMemoryStream;
  if FSize > 0 then begin
    FOwner.ExtractCachedSize(FSize);
    FSize := 0;
  end;
  FDataTime := EncodeDate(1990, 1, 1);
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
    FAccessTime := Now;
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
          TExtMemoryStream(FCache.Stream).SetSize(FSize);
          FCache.Stream.Position := 0;
          FCache.Stream.CopyFrom(F, FSize);
        finally
          F.Free;
        end;
        FOwner.PushCachedSize(FSize);
        if FNeedToCompress.Value then
        begin
          FCache.Lock;
          try
            FDeflateCache := Application.CreateRefMemoryStream;
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
            end else
               FOwner.PushCachedSize(FDeflateSize);
            {$IFDEF ALLOW_STREAM_GZIP}
            FGzipCache := Application.CreateRefMemoryStream;
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
            end else
               FOwner.PushCachedSize(FGzipSize);
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

function TWebClientsContainer.GetCacheSizeLimit : QWord;
begin
  Result := FCachedPages.CacheSizeLimit;
end;

procedure TWebClientsContainer.SetCacheSizeLimit(AValue : QWord);
begin
  FCachedPages.CacheSizeLimit := AValue;
end;

{$IFDEF SERVER_RPC_MODE}
procedure TWebClientsContainer.ClientRemove(Sender: TObject);
var aClient : TWebClient;
begin
  aClient := TWebClient(Sender);

  PREP_ClientStop.Execute([aClient.CUID]);
  if assigned(Application) and Verbose then
    Application.DoInfo('Client removed ' + aClient.CUID);
end;

function TWebClientsContainer.GetVerbose : Boolean;
begin
  Result := FVerbose.Value;
end;

function TWebClientsContainer.OnGenSessionID({%H-}aSession: TSqliteWebSession
  ): String;
var CID : Cardinal;
begin
  CID := FCurCID.ID;
  Result := EncodeIntToB64(CID, 4) + '-' +
            EncodeInt64ToB64(QWord(TimeStampToMSecs(DateTimeToTimeStamp(Now))), 8);
  PREP_ClientSetLastCUID.Execute([CID+1]);
end;

procedure TWebClientsContainer.SetVerbose(AValue : Boolean);
begin
  FVerbose.Value := AValue;
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

procedure TWebClientsContainer.OnCreateNewSession(Sender: TObject);
var ARequest : TWCRequest;
  Session : TSqliteWebSession;
  Con : TWCAppConnection;
  IpV4, IpV6 : String;
  SocketAddr : TSockAddr;
begin
  Session := TSqliteWebSession(Sender);
  try
    ARequest := TWCRequest(Session.Request);
    Con := ARequest.GetConnection;

    SocketAddr := WCGetRemoteAddress(Con.Socket.Handle);

    IpV4 := WCSocketAddrToString(SocketAddr);
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

function TWebClientsContainer.AddClient(ARequest: TAbsHTTPConnectionRequest;
  const ClientID: String): TWebClient;
begin
  Result := FConnectedClients.GetClient(ClientID);
  if not assigned(Result) then
  begin
    Result := Application.WebClientClass.Create(FConnectedClients, ClientID);
    Result.OnRemove:= @ClientRemove;
    Result.AcceptGzip:=  Pos(cSgzip, ARequest.GetHeader(hhAcceptEncoding)) > 0;
    Result.AcceptDeflate:=Pos(cSdeflate, ARequest.GetHeader(hhAcceptEncoding)) > 0;
    if Verbose then
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
{$ENDIF}

procedure TWebClientsContainer.DoMaintainingStep;
begin
  {$IFDEF SERVER_RPC_MODE}
  ClearDeadClients; // delete all timeouted clients
  if Application.NetDebugMode then
    PREP_DeleteOldNetRequests.Execute([]);
  Clients.IdleLiveClients;  // refresh all clients with no syn connection to
                            // prevent memory overflows
//  SeqSheduleStep;   // shedule clients seq
  {$ENDIF}
  FCachedPages.RebuildWebCache;
end;

constructor TWebClientsContainer.Create;
begin
  inherited Create;

  FCachedPages := TWebCacheCollection.Create;

  {$IFDEF SERVER_RPC_MODE}
  GetWebCachedItem(Application.MainURI);
  FVerbose := TThreadBoolean.Create(true);
  FConnectedClients := TWebClients.Create(Self);

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
  {$ENDIF}
end;

destructor TWebClientsContainer.Destroy;
begin
  FCachedPages.Free;
  {$IFDEF SERVER_RPC_MODE}
  FConnectedClients.Free;
  FClientsDB.Free;
  FCurCID.Free;
  FVerbose.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TWebClientsContainer.ClearCache;
begin
  FCachedPages.Clear;
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
    Result := TWebCachedItem.Create(FCachedPages, aLoc, aURI);
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
