{
 wcNetworking:
   Classes and other routings to deal with referenced connections,
   sockets, frames and streams

   Part of WCHTTPServer project

   Copyright (c) 2020-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcNetworking;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$ifdef linux}
{$define socket_epoll_mode}
{$define socket_epoll_accept}
{.$define socket_select_mode}
{$ifdef socket_epoll_accept}
{$define use_epoll}
{$endif}
{$ifdef socket_epoll_mode}
{$define use_epoll}
{$endif}
{$else}
{$define socket_select_mode}
{$endif}

interface

uses
  {$ifdef wiki_docs}
  commonutils,fpcweb,
  {$endif}

  Classes, SysUtils,
  ECommonObjs, OGLFastList,
  fphttp, HTTPDefs, httpprotocol, AbstractHTTPServer, CustAbsHTTPApp,
  BufferedStream, ExtMemoryStream,
  ssockets,
  sockets
  {$ifdef unix}
    ,BaseUnix,Unix
  {$endif}
  {$ifdef linux}{$ifdef use_epoll}
    ,Linux
  {$endif}{$endif}
  {$ifdef windows}
    ,winsock2, windows
  {$endif};

const
  WC_INITIAL_READ_BUFFER_SIZE = 4096;

type

  TWCRefConnection = class;
  TWCConnectionRequest = class;
  TWCRefConnections = class;

  TWCConnectionState = (wcCONNECTED, wcHALFCLOSED, wcDROPPED, wcDEAD);
  TWCProtocolVersion = (wcUNK, wcHTTP1, wcHTTP1_1, wcHTTP2
                       {$IFDEF WC_WEB_SOCKETS}, wcWebSocket{$ENDIF});
  TWCConsumeResult   = (wccrOK, wccrProtocolError, wccrNoData,
                        wccrWrongProtocol, wccrSocketError);

  { TWCHTTPRefProtoFrame }

  TWCRefProtoFrame = class
  public
    procedure SaveToStream(Str : TStream); virtual; abstract;
    function Memory : Pointer; virtual; abstract;
    function Size : Int64; virtual; abstract;
  end;

  { TWCStringFrame }

  TWCStringFrame = class(TWCRefProtoFrame)
  private
    FStr : String;
  public
    constructor Create(const S : String);
    procedure SaveToStream(Str : TStream); override;
    procedure SetStrLength(aNewLen : PtrInt);
    function Memory : Pointer; override;
    function Size : Int64; override;
  end;

  { TWCStringsFrame }

  TWCStringsFrame = class(TWCRefProtoFrame)
  private
    Strm : TExtMemoryStream;
  public
    constructor Create(Strs : TStrings);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Memory : Pointer; override;
    function Size : Int64; override;
    property Stream : TExtMemoryStream read Strm;
  end;

  { TWCStreamFrame }

  TWCStreamFrame = class(TWCRefProtoFrame)
  private
    FStrm : TStream;
  public
    constructor Create(Strm: TStream; Sz: Cardinal; Owned: Boolean);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Memory : Pointer; override;
    function Size : Int64; override;
    property Stream : TStream read FStrm;
  end;

  { TWCRefStreamFrame }

  TWCRefStreamFrame = class(TWCRefProtoFrame)
  private
    FStrm : TReferencedStream;
    Fsz, Fpos : Int64;
  public
    constructor Create(Strm : TReferencedStream; Pos, Sz : Int64);
    constructor Create(Strm : TReferencedStream); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Memory : Pointer; override;
    function Size : Int64; override;
  end;

  { TWCRequestRefWrapper }

  TWCRequestRefWrapper = class(TNetAutoReferencedObject)
  public
    procedure CopyToHTTP1Request(aReq : TWCConnectionRequest); virtual; abstract;
    function GetReqContentStream : TStream; virtual; abstract;
    function IsReqContentStreamOwn : Boolean; virtual; abstract;
    procedure Release; virtual;
  end;

  TWCContentType = (wctAny, wctUtf8String, wctWideString, wctRawString, wctBlob);

  { TWCContent }

  TWCContent = class(TAbsHTTPCombinedContent)
  private
    FContentType : TWCContentType;
    FRequestRef  : TWCRequestRefWrapper;
    procedure SetRequestRef(AValue: TWCRequestRefWrapper);
  protected
    function  UpdateStreamValue : TStream; override;
  public
    constructor Create; override;
    property RequestRef : TWCRequestRefWrapper read FRequestRef write
                                                               SetRequestRef;
    property ContentType : TWCContentType read FContentType write FContentType;
    function AsWideString : WideString;
    destructor Destroy; override;
  end;

  { TWCConnectionRequest }

  TWCConnectionRequest = class(TAbsHTTPConnectionRequest)
  private
    function GetWCContent: TWCContent;
  public
    constructor Create; override;
    property WCContent : TWCContent read GetWCContent;
  end;

  TSocketStates = TAtomicCardinal;

  { TWCSocketReference }

  TWCSocketReference = class(TNetReferencedObject)
  private
    {$ifdef socket_select_mode}
    FReadFDSet,
      FWriteFDSet,
      FErrorFDSet: PFDSet;
    FWaitTime : TTimeVal;
    {$endif}
    FSocket : TSocketStream;
    FSocketStates: TSocketStates;
  protected
    {$ifdef socket_select_mode}
    procedure GetSocketStates;
    {$endif}
    procedure SetCanRead;
    procedure SetCanSend;
    procedure SetEOF;
    procedure PushError;

    function  Readable : Boolean;
    function  Sendable : Boolean;
    function  ReadyToRead : Boolean;
    function  ReadyToSend : Boolean;
    function  IsEOF : Boolean;
    function  HasErrors : Boolean;
    function  HasNoErrors : Boolean;
    function  StartReading : Boolean;
    procedure StopReading;
    function  StartSending : Boolean;
    procedure StopSending;

  public
    constructor Create(aSocket : TSocketStream);
    destructor Destroy; override;

    function Write(const Buffer; Size : Integer) : Integer;
    function Read(var Buffer; Size : Integer) : Integer;

    procedure SoftClose;

    property Socket : TSocketStream read FSocket;
    property States : TSocketStates read FSocketStates;
  end;

  TRefReadSendData = procedure (aConnection : TWCRefConnection) of object;

  { TWCConnection }

  TWCConnection = class(TAbsHTTPConnection)
  private
    FRefCon  : TWCRefConnection;
    FRefRequest  : TWCRequestRefWrapper;
    FSocketRef : TWCSocketReference;
    procedure SetRefCon(AValue: TWCRefConnection);
    procedure SetRefRequest(AValue: TWCRequestRefWrapper);
  protected
    procedure DoSocketAttach(ASocket : TSocketStream); override;
    function  GetSocket : TSocketStream; override;
  public
    Constructor Create(AServer : TAbsCustomHTTPServer; ASocket : TSocketStream); override;
    Constructor CreateRefered(AServer : TAbsCustomHTTPServer;
                                   ASockRef : TWCSocketReference;
                                   AConRef : TWCRefConnection); virtual;
    destructor Destroy; override;
    procedure IncSocketReference;
    procedure DecSocketReference;
    property SocketReference : TWCSocketReference read FSocketRef;
    property RefCon: TWCRefConnection read FRefCon write SetRefCon;
    property RefRequest: TWCRequestRefWrapper read FRefRequest write SetRefRequest;
  end;

  { TThreadSafeConnectionState }

  TThreadSafeConnectionState = class(TThreadSafeObject)
  private
    FState : TWCConnectionState;
    function GetConnState: TWCConnectionState;
    procedure SetConnState(id : TWCConnectionState);
  public
    constructor Create(avalue : TWCConnectionState);
    property Value : TWCConnectionState read GetConnState write SetConnState;
  end;

  {$ifdef SOCKET_EPOLL_MODE}
  TWCIOCandidateType = (ctSend, ctRead, ctError);

  { TThreadCandidate }

  TThreadCandidate = class
  private
    FValues : Array [TWCIOCandidateType] of TAtomicInteger;
    function GetValue(Pos : TWCIOCandidateType) : Integer;
    procedure SetValue(Pos : TWCIOCandidateType; AValue : Integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Value[Pos : TWCIOCandidateType] : Integer read GetValue
                                                               write SetValue;
    procedure DecValue(Pos : TWCIOCandidateType);
    procedure IncValue(Pos : TWCIOCandidateType);
  end;

  {$endif}

  { TWCRefConnection }

  TWCRefConnection = class(TNetAutoReferencedObject)
  private
    FOwner : TWCRefConnections;
    FConnectionState : TThreadSafeConnectionState;
    FReadBuffer, FWriteBuffer : TThreadPointer;
    FReadBufferSize, FWriteBufferSize : Cardinal;
    FReadTailSize, FWriteTailSize : Integer;
    FSocket : Cardinal;
    FTimeStamp : TAtomicQWord;
    FFramesToSend : TThreadSafeFastSeq;
    FSocketRef : TWCSocketReference;
    FReadData : TRefReadSendData;
    FSendData : TRefReadSendData;
    FDataSending, FDataReading : TAtomicBoolean;
    {$IFDEF SOCKET_EPOLL_MODE}
    FIsIOCandidate : TThreadCandidate;
    procedure IncIOCandidate(aType : TWCIOCandidateType);
    procedure DecIOCandidate(aType : TWCIOCandidateType);
    function  IsIOCandidate(aType : TWCIOCandidateType) : Boolean;
    {$endif}

    function GetConnectionState: TWCConnectionState;
    procedure SetConnectionState(CSt: TWCConnectionState);
    function ReadyToReadWrite : Boolean;
    function ReadyToRead : Boolean;
    function ReadyToWrite : Boolean;
    procedure Refresh(const TS: QWord); virtual;
    function TryToConsumeFrames : Boolean;
    function TryToSendFrames(const TS: Qword) : Boolean;
  protected
    FLastError : Cardinal;
    FErrorData : Pointer;
    FErrorDataSize : Cardinal;
    procedure InitializeBuffers;
    function GetInitialReadBufferSize : Cardinal; virtual; abstract;
    function GetInitialWriteBufferSize : Cardinal; virtual; abstract;
    function CanExpandWriteBuffer(aCurSize, aNeedSize : Cardinal) : Boolean; virtual; abstract;
    function RequestsWaiting : Boolean; virtual; abstract;
    function NextFrameToSend(it: TIteratorObject): TIteratorObject; virtual;
    procedure AfterFrameSent({%H-}fr: TWCRefProtoFrame); virtual; abstract;

    { utilities to work with readbuffer and socket }
    function TruncReadBuffer(S : TBufferedStream) : Int64;
    function ReadMore(Buffered : TBufferedStream;
                      WriteAt  : Int64) : Int64;
    function LoadMoreData(Buffered : TBufferedStream;
                          WriteTo : TBufferedStream;
                          FallBackPos,
                          ExtraSize,
                          Offset : Int64) : Boolean;

    property ReadBuffer : TThreadPointer read FReadBuffer;
    property ReadBufferSize : Cardinal read FReadBufferSize;
    property ReadTailSize : Integer read FReadTailSize write FReadTailSize;
    property WriteBuffer : TThreadPointer read FWriteBuffer;
    property FramesToSend : TThreadSafeFastSeq read FFramesToSend;
    property SocketRef : TWCSocketReference read FSocketRef;
    property Owner: TWCRefConnections read FOwner;
  public
    constructor Create(aOwner: TWCRefConnections;
        aSocket: TWCSocketReference;
        aReadData, aSendData: TRefReadSendData); virtual;
    procedure ConsumeNextFrame(Mem : TBufferedStream); virtual; abstract;
    procedure ReleaseRead(ConsumeResult: TWCConsumeResult); virtual;
    procedure SendFrames; virtual;
    function  HasFramesToSend : Boolean;
    destructor Destroy; override;
    class function Protocol : TWCProtocolVersion; virtual; abstract;
    procedure PushFrame(fr : TWCRefProtoFrame); virtual; overload;
    procedure PushFrame(const S : String); overload;
    procedure PushFrame(Strm : TStream; Sz : Cardinal; Owned : Boolean); overload;
    procedure PushFrame(Strs : TStrings); overload;
    procedure PushFrame(Strm : TReferencedStream); overload;
    procedure PushFrameFront(fr : TWCRefProtoFrame);
    function TryToIdleStep(const TS: Qword): Boolean; virtual;
    function ConnectionAvaible: Boolean;
    property Socket : Cardinal read FSocket;
    // lifetime in seconds
    function GetLifeTime(const TS : QWord): Cardinal;
    // error
    property LastError : Cardinal read FLastError;
    property ErrorDataSize : Cardinal read FErrorDataSize;
    property ErrorData : Pointer read FErrorData;
    //
    property ConnectionState : TWCConnectionState read GetConnectionState write SetConnectionState;
  end;

  { TWCHTTP11Connection }

  TWCHTTP11Connection = class(TWCRefConnection)
  protected
    function GetInitialReadBufferSize : Cardinal; override;
    function GetInitialWriteBufferSize : Cardinal; override;
    function CanExpandWriteBuffer({%H-}aCurSize, aNeedSize : Cardinal) : Boolean; override;
    function RequestsWaiting: Boolean; override;
    procedure AfterFrameSent({%H-}fr: TWCRefProtoFrame); override;
  public
    constructor Create(aOwner: TWCRefConnections;
        aSocket: TWCSocketReference;
        aReadData, aSendData: TRefReadSendData); override;
    procedure ConsumeNextFrame({%H-}Mem : TBufferedStream); override;
    class function Protocol : TWCProtocolVersion; override;
  end;

  {$ifdef use_epoll}
  PEpollEvent = ^epoll_event;
  TEpollEvent = epoll_event;
  PEpollData = ^epoll_data;
  TEpollData = epoll_data;
  {$endif}

  { TWCProtocolHelper }

  TWCProtocolHelper = class(TNetCustomLockedObject)
  private
    FProtocol : TWCProtocolVersion;
  public
    constructor Create(aProtocol : TWCProtocolVersion); overload;
    constructor Create; virtual; overload;
  end;

  TWCProtocolHelperClass = class of TWCProtocolHelper;

  TWCCustomHttpServer = class;
  {$ifdef SOCKET_EPOLL_ACCEPT}
  TWCEpollAcceptThread = class;

  { TWCAcceptanceObj }

  TWCAcceptanceObj = class
  private
    FSocket : TSocket;
  public
    constructor Create(aSocket : Tsocket);
    property Socket : TSocket read FSocket;
  end;

  { TWCAcceptancePool }

  TWCAcceptancePool = class(specialize TThreadSafeFastBaseSeq<TWCAcceptanceObj>);
  {$endif}

  {$ifdef socket_epoll_mode}

  TWCEpollIOThread = class;

  { TWCIOCandidate }

  TWCIOCandidate = class
  private
    FCon : TWCRefConnection;
    FType : TWCIOCandidateType;
  public
    constructor Create(aCon : TWCRefConnection; aType : TWCIOCandidateType);
    destructor Destroy; override;
    property Con : TWCRefConnection read FCon;
    property CType : TWCIOCandidateType read FType;
  end;

  { TWCIOPool }

  TWCIOPool = class(specialize TThreadSafeFastBaseSeq<TWCIOCandidate>);

  {$endif}
  TWCIOThread = class;

  { TWCRefConnections }

  TWCRefConnections = class(TThreadSafeFastSeq)
  private
    FLastUsedConnection : TIteratorObject;
    FMaintainStamp : QWord;
    FGarbageCollector : TNetAutoReferenceList;
    FServer       : TWCCustomHttpServer;

    FNeedToRemoveDeadConnections : TAtomicBoolean;
    FHelpers : Array [TWCProtocolVersion] of TWCProtocolHelper;
    //Threads
    FIOThreads : Array of TWCIOThread;
    FMaxIOThreads : TThreadInteger;
    FLiveIOThreads : TAtomicInteger;
    //EPoll
    {$ifdef SOCKET_EPOLL_ACCEPT}
    FLiveEpollAcceptThreads : TAtomicInteger;
    FEpollAcceptThreads : Array of TWCEpollAcceptThread;
    FAcceptancePool : TWCAcceptancePool;
    {$endif}
    {$ifdef socket_epoll_mode}
    FLiveEpollIOThreads : TAtomicInteger;
    FEpollIOThreads:  Array of TWCEpollIOThread;
    FEpollReadFD: THandle;   // this one monitors LT style for READ
    FEpollFD : THandle;       // this one monitors ET style for other
    FIOPool  : TWCIOPool;
    FTmpIOPool  : TWCIOPool;
    FIOEvent : PRTLEvent;
    procedure SendIOEvent;
    procedure AttachEpoll(ACon : TWCRefConnection);
    procedure RemoveEpoll(ACon : TWCRefConnection);
    procedure RemoveIOPoll(ACon : TWCRefConnection);
    procedure ResetReadingEPoll(ACon : TWCRefConnection);
    procedure RemoveEpollIOThread(aThread : TWCEpollIOThread);
    procedure TryIO(aCon : TWCRefConnection; aType : TWCIOCandidateType);
    procedure AccumIO(aCon : TWCRefConnection; aType : TWCIOCandidateType);
    procedure FlashIO;
    function IsConnEqual(aConn: TObject; data: pointer): Boolean;
    {$endif}
    {$ifdef SOCKET_EPOLL_ACCEPT}
    procedure RemoveEpollAcceptThread(aThread : TWCEpollAcceptThread);
    procedure RestartServerSocket;
    procedure TryAccept(aSocket : Tsocket);
    function DoAccept : Boolean;
    {$endif}
    procedure ConnectionDoIdle(O : TObject; data: Pointer);
    procedure IdleConnections(const TS : QWord);
    procedure RepairIOThreads;
    procedure RemoveIOThread(aThread : TWCIOThread);
    function GetProtocolHelper(Id : TWCProtocolVersion): TWCProtocolHelper;
    function IsConnDead(aConn: TObject; data: pointer): Boolean;
    procedure AfterConnExtracted(aObj : TObject);
    function GetMaxIOThreads : Integer;
    procedure SetMaxIOThreads(AValue : Integer);
    procedure StopIOThreads;
    procedure SetServer(aServer : TWCCustomHttpServer);
    function  IOWait : Boolean;
  protected
    procedure HandleNetworkError(E : Exception); virtual;
    procedure HandleNetworkLog(const S : String); virtual;
    procedure HandleNetworkLog(const S : String; const Params : Array of Const); virtual;
  public
    constructor Create(aGarbageCollector : TNetAutoReferenceList;
                                         aServer: TWCCustomHttpServer);
    destructor  Destroy; override;
    procedure   AddConnection(FConn : TWCRefConnection);
    function    GetByHandle(aSocket : Cardinal) : TWCRefConnection;
    procedure   RemoveDeadConnections(const TS: QWord; MaxLifeTime: Cardinal);
    procedure   Idle(const TS: QWord);
    function    IdleSocketsIO(const TS: QWord) : Boolean;
    procedure   RegisterProtocolHelper(Id : TWCProtocolVersion;
                                          aHelper : TWCProtocolHelperClass);
    procedure   PushSocketError;
    procedure   CloseAll;
    property    Protocol[Id : TWCProtocolVersion] : TWCProtocolHelper read
                                         GetProtocolHelper;
    property    GarbageCollector : TNetAutoReferenceList read FGarbageCollector write
                                         FGarbageCollector;
    property    MaxIOThreads : Integer read GetMaxIOThreads write SetMaxIOThreads;
  end;

  { TWCIOThread }

  TWCIOThread = class(TThread)
  private
    FOwner : TWCRefConnections;
  protected
    procedure HandleNetworkError(E : Exception); virtual;
  public
    procedure Execute; override;
    constructor Create(aOwner : TWCRefConnections); reintroduce;
  end;

  {$ifdef use_epoll}

  { TWCEpollThread }

  TWCEpollThread = class(TWCIOThread)
  private
    FTimeout: cInt;
    FInternalPoll : THandle;
    FFdEventPipe : TFilDes;
  public
    constructor Create(aOwner : TWCRefConnections; aTimeOut : cInt); reintroduce;

    procedure InitPoll(aSz : cint); virtual;
    function  InitEventPipe : THandle;
    procedure SendEventMsg(const aBuffer; aBufSize : Integer);
    function  ReadEventMsg(var aBuffer; aBufSize : Integer) : Integer;
    procedure Stop;

    destructor Destroy; override;
  end;

  {$ifdef socket_epoll_mode}
  { TWCEpollIOThread }

  TWCEpollIOThread = class(TWCEpollThread)
  private
    FEvents: array of TEpollEvent;
    FEventsRead: array of TEpollEvent;
    FEpollLocker : TNetCustomLockedObject;
  public
    procedure Execute; override;
    constructor Create(aOwner : TWCRefConnections; aTimeOut : cInt); reintroduce;
    procedure InitPoll(aSz : cint); override;
    procedure Attach({%H-}ACon : TWCRefConnection);
    procedure Inflate;
    destructor Destroy; override;
  end;
  {$endif}

  {$ifdef SOCKET_EPOLL_ACCEPT}
  { TWCEpollAcceptThread }

  TWCEpollAcceptThread = class(TWCEpollThread)
  private
    FServerSocket : TAbsInetServer;
  public
    constructor Create(aOwner : TWCRefConnections; aTimeOut : cInt); reintroduce;
    procedure Execute; override;
    procedure SetServerSocket(aSock : TAbsInetServer);
  end;
  {$endif}

  EServerSocketError = class(ESocketError);

  {$endif}

  { TWCCustomHttpServer }

  TWCCustomHttpServer = class(TEmbeddedAbsHttpServer)
  private
    FRefConnections : TWCRefConnections;
    FServerActive : Boolean;
    FNeedServerSocketRestart : TThreadBoolean;
    function GetAbsConnections : Integer;
    function GetIncomingConnections : Integer;
    function GetNeedServerSocketRestart : Boolean;
    procedure SetNeedServerSocketRestart(AValue : Boolean);
  protected
    procedure HandleNetworkError(E : Exception); virtual;abstract;
    procedure HandleNetworkLog(const S : String); virtual;abstract;
    procedure HandleNetworkLog(const S : String; const Params : Array of Const); virtual;abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateServerSocket; override;
    procedure StartServerSocket; override;
    procedure StopServerSocket; override;
  public
    property  ConnectedSocketsCount : Integer read GetAbsConnections;
    property  IncomingSocketsCount : Integer read GetIncomingConnections;
    property  RefConnections : TWCRefConnections read FRefConnections;
    property  ServerActive : Boolean read FServerActive;
    property  NeedServerSocketRestart : Boolean read GetNeedServerSocketRestart
                                                write SetNeedServerSocketRestart;
  end;

const
  SOCKET_ACCEPT_TIMEOUT_MS = 10000;
  SOCKET_READ_TIMEOUT_MS   = 10000;

implementation

const
  HTTP1_INITIAL_WRITE_BUFFER_SIZE = $FFFF;
  HTTP1_MAX_WRITE_BUFFER_SIZE     = $9600000;
  MAX_IO_THREADS_DEFAULT = 1;
  MAX_IO_CHUNCK = 8;
{$ifdef socket_epoll_mode}
  EPOLLRDHUP = $2000;
  BASE_SIZE = 100;
  EPOLL_MAX_IO_TIMEOUT = -1;
  EPOLL_MAX_IO_THREADS = 1;
{$endif}
{$ifdef SOCKET_EPOLL_ACCEPT}
  EPOLL_MAX_ACCEPT_TIMEOUT = 5000;
  EPOLL_MAX_ACCEPT_THREADS = 1;
{$endif}
{$ifdef unix}
  sleep_timeout0 : Ttimespec = (tv_sec:0; tv_nsec:1000);
  sleep_timeout1 : Ttimespec = (tv_sec:0; tv_nsec:1000000);
  sleep_timeout10 : Ttimespec = (tv_sec:0; tv_nsec:10000000);
  sleep_timeout100 : Ttimespec = (tv_sec:0; tv_nsec:100000000);
{$endif}

  SS_CAN_READ : Cardinal = $00000001;
  SS_CAN_SEND : Cardinal = $00000002;
  SS_READING  : Cardinal = $00000004;
  SS_SENDING  : Cardinal = $00000008;
  SS_ERROR    : Cardinal = $00000010;
  SS_EOF      : Cardinal = $00000020;

type
  TWCLifeTimeChecker = record
    CurTime : QWord;
    MaxLifeTime : Cardinal;
  end;

PWCLifeTimeChecker = ^TWCLifeTimeChecker;

{$ifdef windows}
procedure lsleep0; inline;
begin
  Sleep(0);
end;

procedure lsleep1; inline;
begin
  Sleep(1);
end;

procedure lsleep10; inline;
begin
  Sleep(10);
end;

procedure lsleep100; inline;
begin
  Sleep(100);
end;

{$else ifdef unix}
procedure lsleep0; inline;
begin
  fpnanosleep(@sleep_timeout0,nil);
end;

procedure lsleep1; inline;
begin
  fpnanosleep(@sleep_timeout1,nil);
end;

procedure lsleep10; inline;
begin
  fpnanosleep(@sleep_timeout10,nil);
end;

procedure lsleep100; inline;
begin
  fpnanosleep(@sleep_timeout100,nil);
end;

{$endif}

{ TWCCustomHttpServer }

function TWCCustomHttpServer.GetNeedServerSocketRestart : Boolean;
begin
  Result := FNeedServerSocketRestart.Value;
end;

function TWCCustomHttpServer.GetAbsConnections : Integer;
begin
  Result := ConnectionCount;
end;

function TWCCustomHttpServer.GetIncomingConnections : Integer;
begin
  {$ifdef SOCKET_EPOLL_ACCEPT}
  Result := RefConnections.FAcceptancePool.Count;
  {$else}
  // Not supported
  Result := 0;
  {$endif}
end;

procedure TWCCustomHttpServer.SetNeedServerSocketRestart(AValue : Boolean);
begin
  FNeedServerSocketRestart.Value := AValue;
end;

constructor TWCCustomHttpServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  QueueSize := 100;
  FNeedServerSocketRestart := TThreadBoolean.Create(false);
  FRefConnections := TWCRefConnections.Create(nil, Self);
end;

destructor TWCCustomHttpServer.Destroy;
begin
  FRefConnections.Free;
  FNeedServerSocketRestart.Free;
  inherited Destroy;
end;

procedure TWCCustomHttpServer.CreateServerSocket;
begin
  inherited CreateServerSocket;
  AllowHandlerAccept := true; //false;
end;

procedure TWCCustomHttpServer.StartServerSocket;

procedure DoStartServerSocket;
begin
  IpServer.Bind;
  IpServer.SetNonBlocking;
  IpServer.Listen;
  FServerActive := true;
  FRefConnections.SetServer(Self);
end;

begin
  try
    DoStartServerSocket();
{$ifdef SOCKET_EPOLL_ACCEPT}
    while FServerActive do
    begin
      IpServer.OnIdle(Self);
      if NeedServerSocketRestart and FServerActive then
      begin
        FRefConnections.StopIOThreads;
        try
          FreeServerSocket;
        except
        end;
        CreateServerSocket;
        IpServer.QueueSize:=Self.QueueSize;
        IpServer.ReuseAddress:=true;
        DoStartServerSocket();

        NeedServerSocketRestart := false;
      end;
    end;
{$ELSE}
    IpServer.StartAccepting;
{$ENDIF}
  except
    on e : Exception do begin
      HandleNetworkError(e);
      FServerActive := false;
    end;
  end;
end;

procedure TWCCustomHttpServer.StopServerSocket;
begin
  inherited StopServerSocket;
  FRefConnections.SetServer(nil);
  FServerActive := false;
end;

{ TWCIOThread }

procedure TWCIOThread.Execute;
var FStandBy : Word;
begin
  try
    FStandBy := 0;
    while not Terminated do
    begin
      if FOwner.IOWait then
      begin
        if FStandBy < $FFFF then
          inc(FStandBy);
        if (FStandBy and $FF00) > 0 then
        begin
          lsleep100;
        end else
        if (FStandBy and $FFF0) > 0 then
        begin
          lsleep10;
        end else
        {$ifdef SOCKET_SELECT_MODE}
          lsleep1;
        {$else}
          lsleep0;
        {$endif}
        Continue;
      end;
      FStandBy := 0;
      FOwner.IdleSocketsIO(GetTickCount64);
      {$ifdef SOCKET_SELECT_MODE}
      lsleep1;
      {$else}
      lsleep0;
      {$endif}
    end;
  finally
    FOwner.RemoveIOThread(Self);
  end;
end;

constructor TWCIOThread.Create(aOwner : TWCRefConnections);
begin
  inherited Create(true);
  FOwner := aOwner;
  FreeOnTerminate := true;
end;

procedure TWCIOThread.HandleNetworkError(E : Exception);
begin
  if Assigned(FOwner) then
    FOwner.HandleNetworkError(E);
end;

{$ifdef USE_EPOLL}

{ TWCEpollThread }

constructor TWCEpollThread.Create(aOwner : TWCRefConnections; aTimeOut : cInt);
begin
  inherited Create(aOwner);
  FTimeout := aTimeOut;
  FFdEventPipe[0] := -1;
  FFdEventPipe[1] := -1;
end;

procedure TWCEpollThread.InitPoll(aSz : cint);
begin
  FInternalPoll := epoll_create(aSz);
  if (FInternalPoll < 0) then
    raise ESocketError.CreateFmt('Unable to create epoll: %d', [fpgeterrno]);
end;

function TWCEpollThread.InitEventPipe : THandle;
var
  lEvent : TEpollEvent;
  err : cInt;
begin
  err := AssignPipe(FFdEventPipe[0], FFdEventPipe[1]);
  if err < 0 then
    raise ESocketError.CreateFmt('Error on pipe create %d', [err]);

  Result := FFdEventPipe[0];

  lEvent.data.fd := Result;
  lEvent.events  := EPOLLIN;
  err := epoll_ctl(FInternalPoll, EPOLL_CTL_ADD, Result, @lEvent);
  if (err < 0) then
    raise ESocketError.CreateFmt('Error on signal event add to epoll %d', [err]);
end;

procedure TWCEpollThread.SendEventMsg(const aBuffer; aBufSize : Integer);
begin
  FpWrite(FFdEventPipe[1], @aBuffer, aBufSize);
end;

function TWCEpollThread.ReadEventMsg(var aBuffer; aBufSize : Integer) : Integer;
begin
  Result := FpRead(FFdEventPipe[0], @aBuffer, aBufSize);
end;

procedure TWCEpollThread.Stop;
var b : Byte;
begin
  b := SIGTERM;
  SendEventMsg(b, Sizeof(b));
  Terminate;
end;

destructor TWCEpollThread.Destroy;
begin
  if FInternalPoll >= 0 then
    FpClose(FInternalPoll);
  if FFdEventPipe[0] >= 0 then
    FpClose(FFdEventPipe[0]);
  if FFdEventPipe[1] >= 0 then
    FpClose(FFdEventPipe[1]);
  inherited Destroy;
end;
{$endif}

{$ifdef SOCKET_EPOLL_ACCEPT}

{ TWCAcceptanceObj }

constructor TWCAcceptanceObj.Create(aSocket : Tsocket);
begin
  FSocket := aSocket;
end;

{ TWCEpollAcceptThread }

constructor TWCEpollAcceptThread.Create(aOwner : TWCRefConnections;
  aTimeOut : cInt);
begin
  inherited Create(aOwner, aTimeOut);
  FServerSocket := nil;
end;

procedure TWCEpollAcceptThread.Execute;
var ServerChanges, err, i, L, v : Integer;
    Addr : TINetSockAddr;
    FSocket : THandle;
    ServerEvents : Array [0..15] of TEpollEvent;
    listenPip : THandle;
    msg : Byte;
begin
  try
    listenPip := InitEventPipe;

    try
      while not Terminated do
      begin
        if not assigned(FServerSocket) then
        begin
          lsleep10;
          Continue;
        end;
        L := sizeof(v);
        if (fpgetsockopt(FServerSocket.Socket, SOL_SOCKET, SO_ACCEPTCONN, @v, @L) < 0) then
        begin
          err := fpgeterrno;
          raise EServerSocketError.CreateFmt('Server socket fpgetsockopt error %d', [err]);
        end
        else
        if (v <= 0) then
          raise EServerSocketError.CreateFmt('Server socket is dead %d', [FServerSocket.Socket]);

        repeat
          ServerChanges := epoll_wait(FInternalPoll, @(ServerEvents[0]), 16, FTimeout);
          err := fpgeterrno;
        until (ServerChanges >= 0) or ((err <> 0) and (err <> ESysEINTR)) or Terminated;
        if ServerChanges < 0 then
        begin
          If err = ESysEINTR then
          begin
            lsleep1;
            Continue;
          end else
          if err <> 0 then
            raise EServerSocketError.CreateFmt('Error on server epoll %d', [err])
        end;

        if ServerChanges > 0 then
        with FOwner do
        begin
          err := 0;
          for i := 0 to ServerChanges-1 do
          begin
            if ServerEvents[i].Data.fd = listenPip then
            begin
              err := ReadEventMsg(msg, Sizeof(msg));
              if err < 0 then
                raise ESocketError.CreateFmt('Error on event read %d', [err]);
              if msg = SIGTERM then
              begin
                Terminate;
                Break;
              end;
            end else
            if ServerEvents[i].Data.fd = FServerSocket.Socket then
            begin
              try
                while true do
                begin
                  L:=SizeOf(Addr);

                  FSocket:=Sockets.fpAccept(FServerSocket.Socket,@Addr,@L);
                  If (FSocket<0) then
                  begin
                    err:=SocketError;
                    If (err = ESysEWOULDBLOCK) or (err = ESysEAGAIN) then
                    begin
                      Break;
                    end else
                      Raise ESocketError.Create(seAcceptFailed, [FServerSocket.Socket,
                                                                 err]);
                  end else
                    FOwner.TryAccept(FSocket);

                  lsleep0;
                end;
              except
                on E : Exception do
                  HandleNetworkError(E);
              end;
            end;
          end;
        end;
        lsleep0;
      end;

    except
      on E : EServerSocketError do
      begin
        HandleNetworkError(E);
        FOwner.RestartServerSocket;
      end;
      on E : Exception do HandleNetworkError(E);
    end;

  finally
    FOwner.RemoveEPollAcceptThread(Self);
  end;
end;

procedure TWCEpollAcceptThread.SetServerSocket(aSock : TAbsInetServer);
var
  lEvent : TEpollEvent;
begin
  FServerSocket := aSock;
  if not Assigned(aSock) then Exit;
  lEvent.events := EPOLLIN;// or EPOLLET;
  lEvent.data.fd := aSock.Socket;
  if epoll_ctl(FInternalPoll, EPOLL_CTL_ADD, aSock.Socket, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add Server socket to master epoll FD: %d', [fpGetErrno]);
end;
{$endif}

{$ifdef socket_epoll_mode}

{ TThreadCandidate }

function TThreadCandidate.GetValue(Pos : TWCIOCandidateType) : Integer;
begin
  Result := FValues[Pos].Value;
end;

procedure TThreadCandidate.SetValue(Pos : TWCIOCandidateType; AValue : Integer);
begin
  FValues[Pos].Value := AValue;
end;

constructor TThreadCandidate.Create;
var
  cd : TWCIOCandidateType;
begin
  inherited Create;

  for cd := Low(TWCIOCandidateType) to High(TWCIOCandidateType) do
  begin
    FValues[cd] := TAtomicInteger.Create(0);
  end;
end;

destructor TThreadCandidate.Destroy;
var
  cd : TWCIOCandidateType;
begin
  for cd := Low(TWCIOCandidateType) to High(TWCIOCandidateType) do
  begin
    FValues[cd].Free;
  end;

  inherited Destroy;
end;

procedure TThreadCandidate.DecValue(Pos : TWCIOCandidateType);
begin
  FValues[Pos].DecValue;
end;

procedure TThreadCandidate.IncValue(Pos : TWCIOCandidateType);
begin
  FValues[Pos].IncValue;
end;

{ TWCIOCandidate }

constructor TWCIOCandidate.Create(aCon : TWCRefConnection;
  aType : TWCIOCandidateType);
begin
  aCon.IncReference;
  FCon := aCon;
  FType := aType;
  FCon.IncIOCandidate(FType);
end;

destructor TWCIOCandidate.Destroy;
begin
  FCon.DecIOCandidate(FType);
  FCon.DecReference;
  inherited Destroy;
end;

{ TWCEpollIOThread }

procedure TWCEpollIOThread.Execute;
var
  i, MasterChanges, Changes, ReadChanges, m, err: Integer;
  Temp, TempRead: TWCRefConnection;
  MasterEvents: array[0..2] of TEpollEvent;
  aCount : Integer;
  TS : QWord;
  listenPip : THandle;
  msg : Byte;
begin
  try
    listenPip := InitEventPipe;

    try
      While Not Terminated do
      begin
        Changes := 0;
        ReadChanges := 0;

        repeat
          MasterChanges := epoll_wait(FInternalPoll, @(MasterEvents[0]), 3, FTimeout);
          err := fpgeterrno;
        until (MasterChanges >= 0) or ((err <> 0) and (err <> ESysEINTR)) or Terminated;
        if MasterChanges < 0 then
        begin
          If err = ESysEINTR then
          begin
            lsleep10;
            Continue;
          end else
            raise ESocketError.CreateFmt('Error on epoll %d', [err]);
        end;

        if MasterChanges > 0 then
        with FOwner do
        begin
          FEpollLocker.Lock;
          try
            aCount := Length(FEvents);
          finally
            FEpollLocker.UnLock;
          end;
          err := 0;
          for i := 0 to MasterChanges - 1 do begin
            if (MasterEvents[i].Data.fd = listenPip) then
            begin
              msg := 0;
              err := ReadEventMsg(msg, Sizeof(msg));
              if err < 0 then
                raise ESocketError.CreateFmt('Error on event read %d', [err]);
              if msg = SIGTERM then
              begin
                Terminate;
                Break;
              end;
            end
            else
            if (MasterEvents[i].Data.fd = FEpollFD) then
            begin
              Changes := epoll_wait(FEpollFD, @FEvents[0], aCount, 0);
              if Changes < 0 then begin
                err := fpgeterrno;
                If (err <> ESysEINTR) then
                   raise ESocketError.CreateFmt('Error on epoll %d', [err]);
              end;
            end
            else
            if (MasterEvents[i].Data.fd = FEpollReadFD) then
            begin
              ReadChanges := epoll_wait(FEpollReadFD, @FEventsRead[0], aCount, 0);
              if ReadChanges < 0 then begin
                err := fpgeterrno;
                If (err <> ESysEINTR) then
                   raise ESocketError.CreateFmt('Error on epoll %d', [err]);
              end;
            end;
          end;
          if (Changes > 0) or (ReadChanges > 0) then
          begin
            TS := GetTickCount64;
            m := Changes;
            if ReadChanges > m then m := ReadChanges;
            for i := 0 to m - 1 do begin
              Temp := nil;
              if i < Changes then begin
                Temp := TWCRefConnection(FEvents[i].data.ptr);

                if  ((FEvents[i].events and EPOLLERR) = EPOLLERR) then
                begin
                  Temp.SocketRef.PushError;
                  FOwner.AccumIO(Temp, ctError);
                end else
                if  ((FEvents[i].events and EPOLLOUT) = EPOLLOUT) then
                begin
                  Temp.SocketRef.SetCanSend;
                  FOwner.AccumIO(Temp, ctSend);
                  Temp.TryToSendFrames(TS);
                end;
              end; // writes

              if i < ReadChanges then begin
                TempRead := TWCRefConnection(FEventsRead[i].data.ptr);

                if  ((FEventsRead[i].events and (EPOLLHUP or EPOLLRDHUP)) > 0) then
                  TempRead.SocketRef.SetEOF;

                if  ((FEventsRead[i].events and (EPOLLIN or EPOLLPRI)) > 0) then
                begin
                  TempRead.SocketRef.SetCanRead;
                  FOwner.AccumIO(TempRead, ctRead);
                  TempRead.TryToConsumeFrames;
                end;
              end; // reads
            end;

            FOwner.FlashIO;
          end;
          lsleep0;
        end;
      end;
    except
      on E : Exception do HandleNetworkError(E);
    end;

  finally
    FOwner.RemoveEpollIOThread(Self);
  end;
end;

constructor TWCEpollIOThread.Create(aOwner : TWCRefConnections; aTimeOut : cInt
  );
begin
  inherited Create(aOwner, aTimeOut);
  FEpollLocker := TNetCustomLockedObject.Create;
  Inflate;
end;

procedure TWCEpollIOThread.InitPoll(aSz : cint);
var
  lEvent : TEpollEvent;
begin
  inherited InitPoll(aSz);
  lEvent.events := EPOLLIN or EPOLLOUT or EPOLLPRI or EPOLLERR or EPOLLRDHUP or EPOLLHUP or EPOLLET;
  lEvent.data.fd := FOwner.FEpollFD;
  if epoll_ctl(FInternalPoll, EPOLL_CTL_ADD, FOwner.FEpollFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
  lEvent.data.fd := FOwner.FEpollReadFD;
  if epoll_ctl(FInternalPoll, EPOLL_CTL_ADD, FOwner.FEpollReadFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
end;

procedure TWCEpollIOThread.Attach({%H-}ACon : TWCRefConnection);
begin
  if FOwner.Count > High(FEvents) then
    Inflate;
end;

procedure TWCEpollIOThread.Inflate;
var
  OldLength: Integer;
begin
  FEpollLocker.Lock;
  try
    OldLength := Length(FEvents);
    if OldLength > 1 then
      SetLength(FEvents, Sqr(OldLength))
    else
      SetLength(FEvents, BASE_SIZE);
    SetLength(FEventsRead, Length(FEvents));
  finally
    FEpollLocker.UnLock;
  end;
end;

destructor TWCEpollIOThread.Destroy;
begin
  SetLength(FEvents, 0);
  SetLength(FEventsRead, 0);
  FEpollLocker.Free;
  inherited Destroy;
end;

{$endif}

{ TWCProtocolHelper }

constructor TWCProtocolHelper.Create(aProtocol: TWCProtocolVersion);
begin
  inherited Create;
  FProtocol := aProtocol;
end;

constructor TWCProtocolHelper.Create;
begin
  inherited Create;
  FProtocol := wcUNK;
end;

{ TWCConnectionRequest }

function TWCConnectionRequest.GetWCContent: TWCContent;
begin
  Result := TWCContent(ContentObject);
end;

constructor TWCConnectionRequest.Create;
begin
  inherited Create;
  ContentClass := TWCContent;
end;

{ TWCContent }

procedure TWCContent.SetRequestRef(AValue: TWCRequestRefWrapper);
begin
  if FRequestRef=AValue then Exit;
  if Assigned(FRequestRef) then FRequestRef.DecReference;
  FRequestRef:=AValue;
  if Assigned(FRequestRef) then FRequestRef.IncReference;
end;

function TWCContent.UpdateStreamValue: TStream;
begin
  if assigned(FRequestRef) then
  begin
    Result := FRequestRef.GetReqContentStream;
    OwnStream := not FRequestRef.IsReqContentStreamOwn;
  end else
  begin
    Result := TExtMemoryStream.Create;
    OwnStream := true;
  end;
end;

constructor TWCContent.Create;
begin
  inherited Create;
  FRequestRef := nil;
  FContentType := wctAny;
end;

function TWCContent.AsWideString: WideString;
begin
  Result := WideString(RawString)
end;

destructor TWCContent.Destroy;
begin
  RequestRef := nil;
  inherited Destroy;
end;

{ TWCRequestRefWrapper }

procedure TWCRequestRefWrapper.Release;
begin
  DecReference;
end;

{ TWCRefStreamFrame }

constructor TWCRefStreamFrame.Create(Strm: TReferencedStream;
                    Pos, Sz: Int64 );
begin
  Strm.IncReference;
  FStrm := Strm;
  Fsz:= Sz;
  Fpos:=Pos;
end;

constructor TWCRefStreamFrame.Create(Strm: TReferencedStream);
begin
  Strm.IncReference;
  FStrm := Strm;
  Fsz:= Strm.Stream.Size;
  Fpos:=0;
end;

destructor TWCRefStreamFrame.Destroy;
begin
  FStrm.DecReference;
  inherited Destroy;
end;

procedure TWCRefStreamFrame.SaveToStream(Str: TStream);
begin
  FStrm.WriteTo(Str, Fpos, FSz);
end;

function TWCRefStreamFrame.Memory : Pointer;
begin
  if FStrm.Stream is TCustomMemoryStream then
  begin
    Result := TCustomMemoryStream(FStrm.Stream).Memory;
  end else
    Result := nil;
end;

function TWCRefStreamFrame.Size: Int64;
begin
  Result := Fsz;
end;

{ TWCStringFrame }

constructor TWCStringFrame.Create(const S: String);
begin
  FStr := S;
end;

procedure TWCStringFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(FStr[1], Size);
end;

procedure TWCStringFrame.SetStrLength(aNewLen : PtrInt);
begin
  SetLength(FStr, aNewLen);
end;

function TWCStringFrame.Memory : Pointer;
begin
  Result := Pointer(@(FStr[1]));
end;

function TWCStringFrame.Size: Int64;
begin
  Result := Length(FStr);
end;

{ TWCStringsFrame }

constructor TWCStringsFrame.Create(Strs: TStrings);
begin
  Strm := TExtMemoryStream.Create;
  Strs.SaveToStream(Strm);
end;

destructor TWCStringsFrame.Destroy;
begin
  Strm.Free;
  inherited Destroy;
end;

procedure TWCStringsFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(Strm.Memory^, Strm.Size);
end;

function TWCStringsFrame.Memory : Pointer;
begin
  Result := Strm.Memory;
end;

function TWCStringsFrame.Size: Int64;
begin
  Result := Strm.Size;
end;

{ TWCStreamFrame }

constructor TWCStreamFrame.Create(Strm: TStream; Sz: Cardinal;
  Owned: Boolean);
begin
  if not Assigned(Strm) then
  begin
    FStrm := TExtMemoryStream.Create(Sz);
  end else
  if Owned then FStrm := Strm else
  begin
     FStrm := TExtMemoryStream.Create(Sz);
     FStrm.CopyFrom(Strm, Sz);
     FStrm.Position:=0;
  end;
end;

destructor TWCStreamFrame.Destroy;
begin
  FStrm.Free;
  inherited Destroy;
end;

procedure TWCStreamFrame.SaveToStream(Str: TStream);
begin
  Str.CopyFrom(FStrm, 0);
end;

function TWCStreamFrame.Memory : Pointer;
begin
  if FStrm is TCustomMemoryStream then
  begin
    Result := TCustomMemoryStream(FStrm).Memory;
  end else
    Result := nil;
end;

function TWCStreamFrame.Size: Int64;
begin
  Result := FStrm.Size;
end;

{ TWCHTTP11Connection }

function TWCHTTP11Connection.GetInitialReadBufferSize: Cardinal;
begin
  Result := 0;
end;

function TWCHTTP11Connection.GetInitialWriteBufferSize: Cardinal;
begin
  Result := HTTP1_INITIAL_WRITE_BUFFER_SIZE;
end;

function TWCHTTP11Connection.CanExpandWriteBuffer(aCurSize,
  aNeedSize: Cardinal): Boolean;
begin
  Result := aNeedSize < HTTP1_MAX_WRITE_BUFFER_SIZE;
end;

function TWCHTTP11Connection.RequestsWaiting: Boolean;
begin
  Result := False;
end;

procedure TWCHTTP11Connection.AfterFrameSent({%H-}fr: TWCRefProtoFrame);
begin
 //
end;

procedure TWCHTTP11Connection.ConsumeNextFrame({%H-}Mem: TBufferedStream);
begin
  // do nothing for now
end;

constructor TWCHTTP11Connection.Create(aOwner: TWCRefConnections;
  aSocket: TWCSocketReference; aReadData, aSendData: TRefReadSendData);
begin
  inherited Create(aOwner, aSocket, aReadData, aSendData);
  InitializeBuffers;
end;

class function TWCHTTP11Connection.Protocol: TWCProtocolVersion;
begin
  Result := wcHTTP1_1;
end;

{ TThreadSafeConnectionState }

function TThreadSafeConnectionState.GetConnState: TWCConnectionState;
begin
  lock;
  try
    Result := FState;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeConnectionState.SetConnState(id: TWCConnectionState);
begin
  lock;
  try
    FState := id;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeConnectionState.Create(avalue: TWCConnectionState);
begin
  inherited Create;
  FState:= aValue;
end;

{ TWCSocketReference }

{$ifdef socket_select_mode}
{$ifdef windows}
const SOCKET_FDSET_SIZE = Sizeof(Cardinal) + Sizeof(TSocket) * 2;
{$endif}
{$ifdef linux}
const SOCKET_FDSET_SIZE = Sizeof(TFDSet);
{$endif}
{$endif}

constructor TWCSocketReference.Create(aSocket: TSocketStream);
begin
  inherited Create;
  FSocket := aSocket;
  {$ifdef socket_select_mode}
  FReadFDSet := GetMem(SOCKET_FDSET_SIZE);
  FWriteFDSet:= GetMem(SOCKET_FDSET_SIZE);
  FErrorFDSet:= GetMem(SOCKET_FDSET_SIZE);
  FWaitTime.tv_sec := 0;
  FWaitTime.tv_usec := 1000;
  {$endif}
  FSocketStates:= TSocketStates.Create(SS_CAN_READ or SS_CAN_SEND);
end;

destructor TWCSocketReference.Destroy;
begin
  if assigned(FSocket) then FreeAndNil(FSocket);
  {$ifdef socket_select_mode}
  FreeMem(Pointer(FReadFDSet),  SOCKET_FDSET_SIZE);
  FreeMem(Pointer(FWriteFDSet), SOCKET_FDSET_SIZE);
  FreeMem(Pointer(FErrorFDSet), SOCKET_FDSET_SIZE);
  {$endif}
  FSocketStates.Free;
  inherited Destroy;
end;

{$ifdef socket_select_mode}
procedure TWCSocketReference.GetSocketStates;
var
  n : integer;
  err : integer;
begin
  Lock;
  try
    {$ifdef unix}
    fpFD_ZERO(FReadFDSet^);
    fpFD_ZERO(FWriteFDSet^);
    fpFD_ZERO(FErrorFDSet^);
    fpFD_SET(Socket.Handle, FReadFDSet^);
    fpFD_SET(Socket.Handle, FWriteFDSet^);
    fpFD_SET(Socket.Handle, FErrorFDSet^);
    n := fpSelect(Socket.Handle+1, FReadFDSet, FWriteFDSet, FErrorFDSet, @FWaitTime);
    {$endif}
    {$ifdef windows}
    FReadFDSet^.fd_count := 1;
    FReadFDSet^.fd_array[0]  := Socket.Handle;
    FWriteFDSet^.fd_count := 1;
    FWriteFDSet^.fd_array[0] := Socket.Handle;
    FErrorFDSet^.fd_count := 1;
    FErrorFDSet^.fd_array[0] := Socket.Handle;
    n := Select(Socket.Handle+1, FReadFDSet, FWriteFDSet, FErrorFDSet, @FWaitTime);
    FSocketStates.AndValue(not (SS_CAN_READ or SS_CAN_SEND));
    {$endif}
    if n < 0 then
    begin
      err := socketerror;
      if (err = EsockENOTSOCK) then
      begin
        PushError;
        Raise ESocketError.Create(seListenFailed,[Socket.Handle,err]);
      end;
    end else
    if n > 0 then
    begin
      {$ifdef windows}
      if FD_ISSET(Socket.Handle, FErrorFDSet^) then
         FSocketStates.OrValue(SS_ERROR) else
      begin
        if FD_ISSET(Socket.Handle, FReadFDSet^) then
           FSocketStates.OrValue(SS_CAN_READ);
        if FD_ISSET(Socket.Handle, FWriteFDSet^) then
           FSocketStates.OrValue(SS_CAN_SEND);
      end;
      {$endif}
      {$ifdef unix}
      if fpFD_ISSET(Socket.Handle, FErrorFDSet^)>0 then
         FSocketStates.OrValue(SS_ERROR) else
      begin
        if fpFD_ISSET(Socket.Handle, FReadFDSet^)>0 then
           FSocketStates.OrValue(SS_CAN_READ);
        if fpFD_ISSET(Socket.Handle, FWriteFDSet^)>0 then
           FSocketStates.OrValue(SS_CAN_SEND);
      end;
      {$endif}
    end;
  finally
    UnLock;
  end;
end;
{$endif}

procedure TWCSocketReference.SetCanRead;
begin
  FSocketStates.OrValue(SS_CAN_READ);
end;

procedure TWCSocketReference.SetCanSend;
begin
  FSocketStates.OrValue(SS_CAN_SEND);
end;

procedure TWCSocketReference.SetEOF;
begin
  FSocketStates.OrValue(SS_EOF);
end;

function TWCSocketReference.ReadyToRead: Boolean;
begin
  Result := (FSocketStates.Value and (SS_CAN_READ or SS_READING or
              SS_SENDING or SS_ERROR)) = SS_CAN_READ;
end;

function TWCSocketReference.ReadyToSend: Boolean;
begin
  Result := (FSocketStates.Value and (SS_CAN_SEND or SS_READING or
              SS_SENDING or SS_ERROR)) = SS_CAN_SEND;
end;

function TWCSocketReference.IsEOF : Boolean;
begin
  Result := (FSocketStates.Value and SS_EOF) > 0;
end;

function TWCSocketReference.Readable: Boolean;
begin
  Result := (FSocketStates.Value and (SS_CAN_READ or SS_ERROR)) = SS_CAN_READ;
end;

function TWCSocketReference.Sendable: Boolean;
begin
  Result := (FSocketStates.Value and (SS_CAN_SEND or SS_ERROR)) = SS_CAN_SEND;
end;

function TWCSocketReference.HasErrors: Boolean;
begin
  Result := (FSocketStates.Value and SS_ERROR) > 0;
end;

function TWCSocketReference.HasNoErrors: Boolean;
begin
  Result := not HasErrors;
end;

function TWCSocketReference.StartReading : Boolean;
begin
  Lock;
  try
    if ReadyToRead then begin
      FSocketStates.OrValue(SS_READING);
      Result := true;
    end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.StopReading;
begin
  FSocketStates.AndValue(not SS_READING);
end;

function TWCSocketReference.StartSending: Boolean;
begin
  Lock;
  try
    if ReadyToSend then begin
      FSocketStates.OrValue(SS_SENDING);
      Result := true;
    end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.StopSending;
begin
  FSocketStates.AndValue(not SS_SENDING);
end;

procedure TWCSocketReference.PushError;
begin
  FSocketStates.OrValue(SS_ERROR);
end;

function TWCSocketReference.Write(const Buffer; Size: Integer): Integer;
begin
  if StartSending then
  begin
    try
      Result := FSocket.Write(Buffer, Size);
      {$IFDEF unix}
      if (Result <= 0) and (errno = ESysEAGAIN) then
      {$ENDIF}
      begin
        FSocketStates.AndValue(not SS_CAN_SEND);
      end;
      StopSending;
    except
      on E : ESocketError do begin
       StopSending;
       Result := -1;
       PushError;
       Raise;
      end;
    end;
  end else Result := 0;
end;

function TWCSocketReference.Read(var Buffer; Size: Integer): Integer;
begin
  if StartReading then
  begin
    try
      Result := FSocket.Read(Buffer, Size);
      {$IFDEF unix}
      if (Result < Size) and (errno <> ESysEAGAIN) then
      {$ENDIF}
      begin
        FSocketStates.AndValue(not SS_CAN_READ);
      end;

      if IsEOF then
        PushError;

      StopReading;
    except
      on E : ESocketError do begin
       StopReading;
       Result := -1;
       PushError;
       Raise;
      end;
    end;
  end else Result := 0;
end;

procedure TWCSocketReference.SoftClose;
begin
  PushError; // soft closing
end;

{ TWCConnection }

procedure TWCConnection.SetRefCon(AValue: TWCRefConnection);
begin
  if FRefCon=AValue then Exit;
  if assigned(FRefCon) then
    FRefCon.DecReference;
  SetRefRequest(nil);
  FRefCon:=AValue;
end;

procedure TWCConnection.SetRefRequest(AValue : TWCRequestRefWrapper);
begin
  if FRefRequest=AValue then Exit;
  if Assigned(FRefRequest) then FRefRequest.Release;  //release here!
  FRefRequest:=AValue;
end;

procedure TWCConnection.DoSocketAttach(ASocket: TSocketStream);
begin
  FSocketRef := TWCSocketReference.Create(ASocket);
end;

function TWCConnection.GetSocket: TSocketStream;
begin
  Result := FSocketRef.Socket;
end;

constructor TWCConnection.Create(AServer: TAbsCustomHTTPServer;
  ASocket: TSocketStream);
begin
  FSocketRef:= nil;
  inherited Create(AServer, ASocket);
  FRefCon:=nil;
  FRefRequest:=nil;
end;

constructor TWCConnection.CreateRefered(AServer : TAbsCustomHTTPServer;
  ASockRef : TWCSocketReference; AConRef : TWCRefConnection);
begin
  inherited Create(AServer, nil);
  if (not Assigned(ASockRef)) and Assigned(AConRef) then
    ASockRef := AConRef.SocketRef;
  FSocketRef := ASockRef;
  ASockRef.IncReference;
  FRefCon := AConRef;
  if assigned(AConRef) then AConRef.IncReference;
  FRefRequest:=nil;
end;

destructor TWCConnection.Destroy;
begin
  SetRefCon(nil);
  if assigned(FSocketRef) then
  begin
    FSocketRef.DecReference;
    if not FSocketRef.HasReferences then
    begin
      FSocketRef := nil;
    end;
  end;
  FSocketRef := nil;
  inherited Destroy;
end;

procedure TWCConnection.IncSocketReference;
begin
  if assigned(FSocketRef) then FSocketRef.IncReference;
end;

procedure TWCConnection.DecSocketReference;
begin
  if assigned(FSocketRef) then FSocketRef.DecReference;
end;

{ TWCRefConnections }

function TWCRefConnections.IsConnDead(aConn: TObject; data: pointer
  ): Boolean;
begin
  with TWCRefConnection(aConn) do
  Result := (GetLifeTime(PWCLifeTimeChecker(data)^.CurTime) >= PWCLifeTimeChecker(data)^.MaxLifeTime) or
            (not ConnectionAvaible);
end;

function TWCRefConnections.GetProtocolHelper(Id : TWCProtocolVersion
  ): TWCProtocolHelper;
begin
  Result := FHelpers[Id];
end;

procedure TWCRefConnections.AfterConnExtracted(aObj: TObject);
begin
  {$ifdef SOCKET_EPOLL_MODE}
  RemoveEpoll(TWCRefConnection(aObj));
  RemoveIOPoll(TWCRefConnection(aObj));
  {$endif}
  TWCRefConnection(aObj).FFramesToSend.Clean;
  TWCRefConnection(aObj).DecReference;
end;

constructor TWCRefConnections.Create(aGarbageCollector : TNetAutoReferenceList;
  aServer : TWCCustomHttpServer);
var v : TWCProtocolVersion;
  i : integer;
begin
  inherited Create;
  FServer := aServer;
  FNeedToRemoveDeadConnections := TAtomicBoolean.Create(false);
  FMaintainStamp := GetTickCount64;
  FLastUsedConnection := nil;
  FGarbageCollector := aGarbageCollector;
  For V := Low(TWCProtocolVersion) to High(TWCProtocolVersion) do
    FHelpers[V] := nil;

  {$ifdef SOCKET_EPOLL_MODE}
  FIOPool := TWCIOPool.Create;
  FTmpIOPool := TWCIOPool.Create;
  FIOEvent := RTLEventCreate;

  FLiveEpollIOThreads := TAtomicInteger.Create(EPOLL_MAX_IO_THREADS);
  FEpollFD := epoll_create(BASE_SIZE);
  FEpollReadFD := epoll_create(BASE_SIZE);
  if (FEPollFD < 0) or (FEpollReadFD < 0) then
    raise ESocketError.CreateFmt('Unable to create epoll: %d', [fpgeterrno]);

  SetLength(FEpollIOThreads, FLiveEpollIOThreads.Value);
  for i := 0 to High(FEpollIOThreads) do
  begin
    FEpollIOThreads[i] := TWCEpollIOThread.Create(Self, EPOLL_MAX_IO_TIMEOUT);
    FEpollIOThreads[i].InitPoll(3);
    FEpollIOThreads[i].Start;
  end;
  {$endif}

  {$ifdef SOCKET_EPOLL_ACCEPT}
  FAcceptancePool := TWCAcceptancePool.Create;
  FLiveEpollAcceptThreads := TAtomicInteger.Create(EPOLL_MAX_ACCEPT_THREADS);
  SetLength(FEpollAcceptThreads, FLiveEpollAcceptThreads.Value);
  for i := 0 to High(FEpollAcceptThreads) do
  begin
    FEpollAcceptThreads[i] := TWCEpollAcceptThread.Create(Self, EPOLL_MAX_ACCEPT_TIMEOUT);
    FEpollAcceptThreads[i].InitPoll(2);
    FEpollAcceptThreads[i].Start;
  end;
  {$endif}

  FMaxIOThreads := TThreadInteger.Create(MAX_IO_THREADS_DEFAULT);
  FLiveIOThreads := TAtomicInteger.Create(MAX_IO_THREADS_DEFAULT);
  FMaxIOThreads.Lock;
  try
    SetLength(FIOThreads, FMaxIOThreads.Value);
    for i := 0 to High(FIOThreads) do
    begin
      FIOThreads[i] := TWCIOThread.Create(Self);
      FIOThreads[i].Start;
    end;
  finally
    FMaxIOThreads.UnLock;
  end;
end;

destructor TWCRefConnections.Destroy;
var P :TIteratorObject;
    v : TWCProtocolVersion;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCRefConnection(P.Value).DecReference;
      P := P.Next;
    end;
    ExtractAll;
  finally
    UnLock;
  end;

  StopIOThreads;

  {$ifdef SOCKET_EPOLL_MODE}
  fpClose(FEpollFD);
  fpClose(FEpollReadFD);
  FLiveEpollIOThreads.Free;
  FIOPool.Free;
  FTmpIOPool.Free;
  RTLEventDestroy(FIOEvent);
  {$endif}

  {$ifdef SOCKET_EPOLL_ACCEPT}
  FLiveEpollAcceptThreads.Free;
  FAcceptancePool.Free;
  {$endif}

  FLiveIOThreads.Free;
  FMaxIOThreads.Free;
  FNeedToRemoveDeadConnections.Free;
  For V := Low(TWCProtocolVersion) to High(TWCProtocolVersion) do
  begin
    if Assigned(FHelpers[V]) then FreeAndNil(FHelpers[V]);
  end;
  inherited Destroy;
end;

procedure TWCRefConnections.AddConnection(FConn: TWCRefConnection);
begin
  Push_back(FConn);
  {$ifdef socket_epoll_mode}
  AttachEpoll(FConn);
  {$endif}
end;

function TWCRefConnections.GetByHandle(aSocket: Cardinal): TWCRefConnection;
var P :TIteratorObject;
begin
  Result := nil;
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if (TWCRefConnection(P.Value).Socket = aSocket) and
         (TWCRefConnection(P.Value).ConnectionAvaible) then
      begin
        Result := TWCRefConnection(P.Value);
        Result.IncReference;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCRefConnections.RemoveDeadConnections(const TS : QWord;
  MaxLifeTime: Cardinal);
var LifeTime : TWCLifeTimeChecker;
begin
  FNeedToRemoveDeadConnections.Value := false;
  LifeTime.CurTime := TS;
  LifeTime.MaxLifeTime := MaxLifeTime;
  FMaintainStamp := TS;
  ExtractObjectsByCriteria(@IsConnDead, @AfterConnExtracted, @LifeTime);
end;

procedure TWCRefConnections.Idle(const TS : QWord);
begin
  IdleConnections(TS);

  if ((TS - FMaintainStamp) div 1000 > 10) or
     (FNeedToRemoveDeadConnections.Value) then
    RemoveDeadConnections(TS, 120);

  RepairIOThreads();
end;

function TWCRefConnections.IdleSocketsIO(const TS : QWord) : Boolean;
{$ifdef SOCKET_EPOLL_MODE}

var C : TWCIOCandidate;
    CType : TWCIOCandidateType;
    Con : TWCRefConnection;
    i : integer;
    b1, b2 : Boolean;
begin
  {$ifdef SOCKET_EPOLL_ACCEPT}
  Result := DoAccept;
  {$ELSE}
  Result := false;
  {$endif}
  i := 0;
  while i < MAX_IO_CHUNCK do
  begin
    C := FIOPool.PopValue;
    if Assigned(C) then
    begin
      SendIOEvent;
      try
        Con := C.Con;
        CType := C.CType;
        Con.IncReference;
      finally
        C.Free;
      end;
      try
        if (Con.ConnectionAvaible) then
        begin
          if Con.FSocketRef.HasErrors then
            Con.ConnectionState:= wcDROPPED else
          begin
            case CType of
              ctSend : begin
                if (not Con.TryToSendFrames(TS)) and Con.ReadyToWrite then
                begin
                  AccumIO(Con, ctSend);
                end;
              end;
              ctRead : begin
                b1 := Con.ReadyToRead or Con.RequestsWaiting;
                b2 := Con.FSocketRef.Readable;

                if (not Con.TryToConsumeFrames) and b1 and b2 then
                  AccumIO(Con, ctRead);
              end;
            end;

            Result := true;
          end;
        end;
      finally
        Con.DecReference;
      end;
      Inc(i);
    end else
      Break;
    lsleep0();
  end;

  FlashIO;
end;
{$ELSE}
var C : TWCRefConnection;
    i, len : integer;
begin
  {$ifdef SOCKET_EPOLL_ACCEPT}
  Result := DoAccept;
  {$ELSE}
  Result := false;
  {$endif}

  len := Count;
  if len > MAX_IO_CHUNCK then len := MAX_IO_CHUNCK;

  i := 0;
  while i < len do
  begin
    C := TWCRefConnection(PopValue);
    if Assigned(C) then
    begin
      if (C.ConnectionAvaible) then
      begin
        if C.FSocketRef.HasErrors then
          C.ConnectionState:= wcDROPPED else
        begin
          if Assigned(C.FSocketRef) and C.ReadyToReadWrite then
          begin
            try
              C.FSocketRef.GetSocketStates;
            except
              on e : ESocketError do ; //catch error
            end;
            if C.FSocketRef.HasErrors then
            begin
              Result := false;
              C.ConnectionState:= wcDROPPED;
              Exit;
            end;
            C.TryToSendFrames(TS);
            C.TryToConsumeFrames;
            Result := true;
          end;
        end;
      end;
      Push_back(C);
      Inc(i);
    end else
      Break;
    lsleep0();
  end;
end;
{$Endif}

procedure TWCRefConnections.RegisterProtocolHelper(Id: TWCProtocolVersion;
  aHelper: TWCProtocolHelperClass);
begin
  if Assigned(FHelpers[Id]) then FreeAndNil(FHelpers[Id]);
  if Assigned(aHelper) then
    FHelpers[Id] := aHelper.Create;
end;

procedure TWCRefConnections.PushSocketError;
begin
  FNeedToRemoveDeadConnections.Value := True;
end;

procedure TWCRefConnections.CloseAll;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if (TWCRefConnection(P.Value).ConnectionAvaible) then
        TWCRefConnection(P.Value).ConnectionState := wcDROPPED;

      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

function TWCRefConnections.GetMaxIOThreads : Integer;
begin
  Result := FMaxIOThreads.Value;
end;

procedure TWCRefConnections.SetMaxIOThreads(AValue : Integer);
var i, m : integer;
begin
  if (FMaxIOThreads.Value <> AValue) then
  begin
    FMaxIOThreads.Lock;
    try
      FMaxIOThreads.Value := AValue;
      if Length(FIOThreads) > AValue then
      begin
        for i := AValue to High(FIOThreads) do
        if Assigned(FIOThreads[i]) then
          FIOThreads[i].Terminate;

        SetLength(FIOThreads, AValue);
      end else
      begin
        m := Length(FIOThreads);
        SetLength(FIOThreads, AValue);
        for i := m to (AValue - 1) do
        begin
          FIOThreads[i] := TWCIOThread.Create(Self);
          FIOThreads[i].Start;
          FLiveIOThreads.IncValue;
        end;
      end;
    finally
      FMaxIOThreads.UnLock;
    end;
  end;
end;

procedure TWCRefConnections.StopIOThreads;
var i : integer;
begin
  {$ifdef SOCKET_EPOLL_MODE}
  for i := 0 to high(FIOThreads) do
    FIOThreads[i].Terminate;

  while FLiveIOThreads.Value > 0 do
  begin
    SendIOEvent;
    Sleep(1);
  end;
  {$else}
  for i := 0 to high(FIOThreads) do
  if Assigned(FIOThreads) then
  begin
    FIOThreads[i].Terminate;
    FIOThreads[i].WaitFor;
  end;
  {$endif}
  {$ifdef SOCKET_EPOLL_MODE}
  for i := 0 to high(FEpollIOThreads) do
  if Assigned(FEpollIOThreads[i]) then
    FEpollIOThreads[i].Stop;
  {$endif}
  {$ifdef SOCKET_EPOLL_ACCEPT}
  for i := 0 to high(FEpollAcceptThreads) do
  if Assigned(FEpollAcceptThreads[i]) then
  begin
    FEpollAcceptThreads[i].Stop;
    FEpollAcceptThreads[i].WaitFor;
  end;
  {$endif}
  lsleep0;
end;

procedure TWCRefConnections.SetServer(aServer : TWCCustomHttpServer);
{$ifdef SOCKET_EPOLL_ACCEPT}
var i : integer;
{$endif}
begin
  FServer := aServer;
  {$ifdef SOCKET_EPOLL_ACCEPT}
  for i := 0 to high(FEpollAcceptThreads) do
  if Assigned(FEpollAcceptThreads[i]) then begin
    if Assigned(aServer) then
      FEpollAcceptThreads[i].SetServerSocket(aServer.IpServer) else
      FEpollAcceptThreads[i].SetServerSocket(nil);
  end;
  {$endif}
end;

function TWCRefConnections.IOWait : Boolean;
begin
  {$ifdef SOCKET_EPOLL_MODE}
  RTLEventWaitFor(FIOEvent);
  Result := (FIOPool.Count = 0)
  {$ELSE}
  Result := (Count = 0)
  {$endif}
  {$ifdef SOCKET_EPOLL_ACCEPT} and (FAcceptancePool.Count = 0) {$endif};
end;

procedure TWCRefConnections.HandleNetworkError(E : Exception);
begin
  if Assigned(FServer) then
    FServer.HandleNetworkError(E);
end;

procedure TWCRefConnections.HandleNetworkLog(const S : String);
begin
  if Assigned(FServer) then
    FServer.HandleNetworkLog(S);
end;

procedure TWCRefConnections.HandleNetworkLog(const S : String;
  const Params : array of const);
begin
  if Assigned(FServer) then
    FServer.HandleNetworkLog(S, Params);
end;

{$ifdef SOCKET_EPOLL_MODE}

procedure TWCRefConnections.SendIOEvent;
begin
  RTLEventSetEvent(FIOEvent);
end;

procedure TWCRefConnections.AttachEpoll(ACon : TWCRefConnection);
var
  i : integer;
  lEvent: TEpollEvent;
  aHandle : THandle;
begin
  ACon.IncReference;
  ACon.SocketRef.IncReference;
  aHandle := ACon.SocketRef.FSocket.Handle;

  lEvent.events := EPOLLET or EPOLLOUT or EPOLLERR;
  lEvent.data.ptr := ACon;
  if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll: %d', [fpgeterrno]);
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLRDHUP or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, aHandle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll: %d', [fpgeterrno]);
  for i := 0 to high(FEpollIOThreads) do
    FEpollIOThreads[i].Attach(ACon);

  TryIO(ACon, ctRead);
  TryIO(ACon, ctSend);
end;

procedure TWCRefConnections.RemoveEpoll(ACon : TWCRefConnection);
var
  aHandle : THandle;
begin
  try
    aHandle := ACon.SocketRef.FSocket.Handle;
    epoll_ctl(FEpollFD, EPOLL_CTL_DEL, aHandle, nil);
    epoll_ctl(FEpollReadFD, EPOLL_CTL_DEL, aHandle, nil);
  finally
    ACon.SocketRef.DecReference;
    ACon.DecReference;
  end;
end;

procedure TWCRefConnections.RemoveIOPoll(ACon : TWCRefConnection);
begin
  FIOPool.EraseObjectsByCriteria(@IsConnEqual, ACon);
end;

procedure TWCRefConnections.ResetReadingEPoll(ACon : TWCRefConnection);
var
  lEvent: TEpollEvent;
  err : Integer;
begin
  lEvent.data.ptr := ACon;
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLRDHUP or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  ACon.SocketRef.Lock;
  try
    if epoll_ctl(FEpollReadFD, EPOLL_CTL_MOD, ACon.SocketRef.FSocket.Handle, @lEvent) < 0 then
    begin
      err := fpgeterrno;
      if (err <> ESysENOENT) then
        raise ESocketError.CreateFmt('Error modify handle in epoll: %d', [fpgeterrno]);
    end;
  finally
    ACon.SocketRef.UnLock;
  end;
end;

procedure TWCRefConnections.RemoveEpollIOThread(aThread : TWCEpollIOThread);
var i : integer;
begin
//  FLiveEpollIOThreads.Lock;
  try
    for i := 0 to High(FEpollIOThreads) do
    if FEpollIOThreads[i] = aThread then
    begin
      FEpollIOThreads[i] := nil;
      FLiveEpollIOThreads.DecValue;
    end;
  finally
//    FLiveEpollIOThreads.UnLock;
  end;
end;

procedure TWCRefConnections.TryIO(aCon : TWCRefConnection;
  aType : TWCIOCandidateType);
begin
  if aCon.IsIOCandidate(aType) then
  begin
    FIOPool.Push_back(TWCIOCandidate.Create(aCon, aType));
  end;
  SendIOEvent;
end;

procedure TWCRefConnections.AccumIO(aCon : TWCRefConnection;
  aType : TWCIOCandidateType);
begin
  if aCon.IsIOCandidate(aType) then
  begin
    FTmpIOPool.Push_back(TWCIOCandidate.Create(aCon, aType));
  end;
end;

procedure TWCRefConnections.FlashIO();
var
  c : TWCIOCandidate;
begin
  while true do
  begin
    c := FTmpIOPool.PopValue;
    if assigned(c) then
    begin
      FIOPool.Push_back(c);
    end else
      break;
  end;
  if FIOPool.Count > 0 then
    SendIOEvent;
end;

function TWCRefConnections.IsConnEqual(aConn : TObject; data : pointer
  ) : Boolean;
begin
  Result := aConn = TObject(data);
end;

{$endif}

{$ifdef SOCKET_EPOLL_ACCEPT}

procedure TWCRefConnections.RestartServerSocket;
begin
  if Assigned(FServer) then
    FServer.NeedServerSocketRestart := true;
end;

procedure TWCRefConnections.TryAccept(aSocket : Tsocket);
begin
  FAcceptancePool.Push_back(TWCAcceptanceObj.Create(aSocket));
  SendIOEvent;
end;

function TWCRefConnections.DoAccept : Boolean;
var
  Stream : TSocketStream;
  aSocket : TWCAcceptanceObj;
  cnt : integer;
begin
  Result := false;
  try
    cnt := 0;
    while cnt < MAX_IO_CHUNCK do
    begin
      aSocket := FAcceptancePool.PopValue;
      if not Assigned(aSocket) then break;

      SendIOEvent;
      try
        Stream := nil;
        if FServer.ipServer.NonBlocking then
          fpfcntl(aSocket.Socket, F_SETFD, fpfcntl(aSocket.Socket, F_GetFD, 0) or O_NONBLOCK);
        Stream := FServer.ipServer.SockToStream(aSocket.Socket);
        if Assigned(Stream) then begin
          Stream.IOTimeout := SOCKET_READ_TIMEOUT_MS;
          FServer.ipServer.DoConnect(Stream);
          Stream := nil;
        end;
        Result := true;
      finally
        aSocket.Free;
        if Assigned(Stream) then
          FreeAndNil(Stream);
      end;
      lsleep0();
      inc(cnt);
    end;
  except
    on E : Exception do
      HandleNetworkError(E);
  end;
end;

procedure TWCRefConnections.RemoveEpollAcceptThread(aThread : TWCEpollAcceptThread);
var i : integer;
begin
//  FLiveEpollAcceptThreads.Lock;
  try
    for i := 0 to High(FEpollAcceptThreads) do
    if FEpollAcceptThreads[i] = aThread then
    begin
      FEpollAcceptThreads[i] := nil;
      FLiveEpollAcceptThreads.DecValue;
    end;
  finally
//    FLiveEpollAcceptThreads.UnLock;
  end;
end;

{$endif}

procedure TWCRefConnections.ConnectionDoIdle(O : TObject; data : Pointer);
begin
  TWCRefConnection(O).TryToIdleStep(PQWord(data)^);
end;

procedure TWCRefConnections.RemoveIOThread(aThread : TWCIOThread);
var i : integer;
begin
  FMaxIOThreads.Lock;
  try
    for i := 0 to High(FIOThreads) do
    if FIOThreads[i] = aThread then
    begin
      FIOThreads[i] := nil;
      FLiveIOThreads.DecValue;
    end;
  finally
    FMaxIOThreads.UnLock;
  end;
end;

procedure TWCRefConnections.RepairIOThreads;
var i : integer;
begin
  FMaxIOThreads.Lock;
  try
    if FLiveIOThreads.Value < Length(FIOThreads) then
    begin
      for i := 0 to High(FIOThreads) do
      begin
        if not Assigned(FIOThreads[i]) then
        begin
          FIOThreads[i] := TWCIOThread.Create(Self);
          FIOThreads[i].Start;
          FLiveIOThreads.IncValue;
        end;
      end;
    end;
  finally
    FMaxIOThreads.UnLock;
  end;

  {$IFDEF SOCKET_EPOLL_MODE}
  if FLiveEpollIOThreads.Value < Length(FEpollIOThreads) then
  begin
    for i := 0 to High(FEpollIOThreads) do
    begin
      if not Assigned(FEpollIOThreads[i]) then
      begin
        FEpollIOThreads[i] := TWCEpollIOThread.Create(Self, EPOLL_MAX_IO_TIMEOUT);
        FEpollIOThreads[i].InitPoll(3);
        FEpollIOThreads[i].Start;
        FLiveEpollIOThreads.IncValue;
      end;
    end;
  end;
  {$ENDIF}
  {$IFDEF SOCKET_EPOLL_ACCEPT}
  if FLiveEpollAcceptThreads.Value < Length(FEpollAcceptThreads) then
  begin
    for i := 0 to High(FEpollAcceptThreads) do
    begin
      if not Assigned(FEpollAcceptThreads[i]) then
      begin
        FEpollAcceptThreads[i] := TWCEpollAcceptThread.Create(Self, EPOLL_MAX_ACCEPT_TIMEOUT);
        FEpollAcceptThreads[i].InitPoll(2);
        if Assigned(FServer) then
          FEpollAcceptThreads[i].SetServerSocket(FServer.IpServer);
        FEpollAcceptThreads[i].Start;
        FLiveEpollAcceptThreads.IncValue;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TWCRefConnections.IdleConnections(const TS : QWord);
begin
  DoForAllEx(@ConnectionDoIdle, @TS);
end;

{ TWCRefConnection }

procedure TWCRefConnection.SetConnectionState(CSt: TWCConnectionState);
begin
  if ConnectionState <> CSt then
  begin
    FConnectionState.Value := Cst;
    if Cst = wcDROPPED then begin
      FSocketRef.PushError;
      FOwner.PushSocketError;
    end;
  end;
end;

function TWCRefConnection.GetConnectionState: TWCConnectionState;
begin
  Result := FConnectionState.Value;
end;

function TWCRefConnection.ReadyToReadWrite: Boolean;
begin
  Result := ReadyToRead or ReadyToWrite;
end;

function TWCRefConnection.ReadyToRead : Boolean;
begin
  Result := (not FDataReading.Value);
end;

function TWCRefConnection.ReadyToWrite : Boolean;
begin
  Result := (not FDataSending.Value) and
            ((FFramesToSend.Count > 0) or (FWriteTailSize > 0));
end;

function TWCRefConnection.GetLifeTime(const TS: QWord): Cardinal;
begin
  Result := (Int64(TS) - Int64(FTimeStamp.Value)) div 1000;
end;

procedure TWCRefConnection.Refresh(const TS: QWord);
begin
  FTimeStamp.Value := TS;
end;

constructor TWCRefConnection.Create(aOwner: TWCRefConnections;
  aSocket: TWCSocketReference;
  aReadData, aSendData: TRefReadSendData);
var TS : QWord;
begin
  inherited Create;
  FReadBuffer := nil;
  FWriteBuffer := nil;
  FOwner := aOwner;
  FSocketRef := aSocket;
  FSocketRef.IncReference;
  FSocket:= FSocketRef.Socket.Handle;
  TS := GetTickCount64;
  FTimeStamp := TAtomicQWord.Create(TS);
  FReadTailSize := 0;
  FWriteTailSize:= 0;
  FReadData := aReadData;
  FSendData := aSendData;
  FDataSending := TAtomicBoolean.Create(false);
  FDataReading := TAtomicBoolean.Create(false);
  FFramesToSend := TThreadSafeFastSeq.Create;
  FConnectionState := TThreadSafeConnectionState.Create(wcCONNECTED);
  {$IFDEF SOCKET_EPOLL_MODE}
  FIsIOCandidate := TThreadCandidate.Create();
  FIsIOCandidate.Value[ctRead] := 0;
  FIsIOCandidate.Value[ctSend] := 0;
  {$endif}
end;

{$IFDEF SOCKET_EPOLL_MODE}
procedure TWCRefConnection.IncIOCandidate(aType : TWCIOCandidateType);
begin
  FIsIOCandidate.IncValue(aType);
end;

procedure TWCRefConnection.DecIOCandidate(aType : TWCIOCandidateType);
begin
  FIsIOCandidate.DecValue(aType);
end;

function TWCRefConnection.IsIOCandidate(aType : TWCIOCandidateType) : Boolean;
begin
  Result := FIsIOCandidate.Value[aType] <= 0;
end;
{$ENDIF}

procedure TWCRefConnection.ReleaseRead(ConsumeResult : TWCConsumeResult);
begin
  FDataReading.Value := false;
  {$ifdef SOCKET_EPOLL_MODE}
  try
    FOwner.ResetReadingEPoll(Self);
    if (FSocketRef.Readable or RequestsWaiting) then
      FOwner.TryIO(Self, ctRead);
  except
    on ESocketError do ConsumeResult := wccrSocketError;
  end;
  {$endif}
  if ConsumeResult = wccrOK then
    Refresh(GetTickCount64);
end;

destructor TWCRefConnection.Destroy;
begin
  FConnectionState.Value:= wcDEAD;
  if assigned(FSocketRef) then
    FSocketRef.DecReference;
  FFramesToSend.Free;
  if assigned(FReadBuffer) then
    FReadBuffer.Free;
  if assigned(FWriteBuffer) then
    FWriteBuffer.Free;
  FTimeStamp.Free;
  FConnectionState.Free;
  FDataSending.Free;
  FDataReading.Free;
  {$IFDEF SOCKET_EPOLL_MODE}
  FIsIOCandidate.Free;
  {$endif}
  inherited Destroy;
end;

procedure TWCRefConnection.PushFrame(fr: TWCRefProtoFrame);
begin
  FFramesToSend.Push_back(fr);
  {$IFDEF SOCKET_EPOLL_MODE}
  FOwner.TryIO(Self, ctSend);
  {$ENDIF}
end;

procedure TWCRefConnection.PushFrame(const S: String);
begin
  PushFrame(TWCStringFrame.Create(S));
end;

procedure TWCRefConnection.PushFrame(Strm: TStream; Sz: Cardinal;
  Owned: Boolean);
begin
  PushFrame(TWCStreamFrame.Create(Strm, Sz, Owned));
end;

procedure TWCRefConnection.PushFrame(Strs: TStrings);
begin
  PushFrame(TWCStringsFrame.Create(Strs));
end;

procedure TWCRefConnection.PushFrame(Strm: TReferencedStream);
begin
  PushFrame(TWCRefStreamFrame.Create(Strm));
end;

procedure TWCRefConnection.PushFrameFront(fr: TWCRefProtoFrame);
begin
  FFramesToSend.Push_front(fr);
  {$IFDEF SOCKET_EPOLL_MODE}
  FOwner.TryIO(Self, ctSend);
  {$ENDIF}
end;

procedure TWCRefConnection.SendFrames;
var fr : TWCRefProtoFrame;
    it, nit : TIteratorObject;
    WrBuf : TBufferedStream;
    Sz : Integer;
    CurBuffer : Pointer;
    FrameCanSend : Boolean;
begin
  WrBuf := TBufferedStream.Create;
  try
      FWriteBuffer.Lock;
      try
        CurBuffer := FWriteBuffer.Value;
        WrBuf.SetPtr(Pointer(CurBuffer + FWriteTailSize),
                                             FWriteBufferSize - FWriteTailSize);
        FFramesToSend.Lock;
        try
          it := FFramesToSend.ListBegin;
          repeat
             fr := nil;
             it := NextFrameToSend(it);
             if Assigned(it) then begin

               FrameCanSend := true;

               if (TWCRefProtoFrame(it.Value).Size >
                                         (WrBuf.Size - WrBuf.Position)) then
               begin
                 Sz := WrBuf.Position + TWCRefProtoFrame(it.Value).Size;
                 if CanExpandWriteBuffer(FWriteBufferSize, Sz) then
                 begin
                   CurBuffer:= ReAllocMem(CurBuffer, Sz);
                   FWriteBuffer.Realloc(CurBuffer);
                   FWriteBufferSize := Sz;
                   Sz := WrBuf.Position;
                   WrBuf.SetPtr(Pointer(CurBuffer + FWriteTailSize),
                                               FWriteBufferSize - FWriteTailSize);
                   WrBuf.Position := Sz;
                 end else
                   FrameCanSend := false
               end;

               if FrameCanSend then
               begin
                 fr := TWCRefProtoFrame(it.Value);
                 nit := it.Next; FFramesToSend.Extract(it); it := nit;
                 fr.SaveToStream(WrBuf);
                 AfterFrameSent(fr);
                 fr.Free;
               end;
             end;
          until not assigned(fr);
        finally
          FFramesToSend.UnLock;
        end;
        try
          if ((WrBuf.Position > 0) or (FWriteTailSize > 0)) then
          begin
            Sz := FSocketRef.Write(CurBuffer^, WrBuf.Position + FWriteTailSize);
            if (Sz < WrBuf.Position) and (FSocketRef.HasNoErrors) then
            begin
              if Sz < 0 then
                 Sz := 0; // ignore non-fatal errors. rollback to tail
              FWriteTailSize := WrBuf.Position + FWriteTailSize - Sz;
              if Sz > 0 then
                Move(Pointer(CurBuffer + Sz)^, CurBuffer^, FWriteTailSize);
            end else begin
              FWriteTailSize:= 0;
            end;
          end;
        except
          on E: ESocketError do
            ConnectionState:= wcDROPPED;
        end;
      finally
        FWriteBuffer.UnLock;
      end;
  finally
    WrBuf.Free;
    FDataSending.Value := false;
  end;

  {$ifdef SOCKET_EPOLL_MODE}
  if ((FFramesToSend.Count > 0) or (FWriteTailSize > 0)) then
     FOwner.TryIO(Self, ctSend);
  {$ENDIF}

  if (FFramesToSend.Count = 0) and
     (FWriteTailSize = 0) and
     (ConnectionState = wcHALFCLOSED) then
  begin
    ConnectionState := wcDROPPED;
  end;
end;

function TWCRefConnection.HasFramesToSend : Boolean;
begin
  Result := FFramesToSend.Count > 0;
end;

function TWCRefConnection.TryToIdleStep(const TS : Qword): Boolean;
begin
  {$IFDEF SOCKET_EPOLL_MODE}
  if HasFramesToSend then
    FOwner.TryIO(Self, ctSend);
  {$ENDIF}
  Result := true;
end;

function TWCRefConnection.ConnectionAvaible: Boolean;
begin
  Result := ConnectionState in [wcCONNECTED, wcHALFCLOSED];
end;

function TWCRefConnection.TryToConsumeFrames : Boolean;
begin
  //FDataReading.Lock;
  try
    if (FSocketRef.ReadyToRead or RequestsWaiting) and
        ReadyToRead and
        assigned(FReadData) then
    begin
      FDataReading.Value := True;
      FReadData(Self);
      Result := true;
    end else
    begin
      Result := false;
    end;
  finally
    //FDataReading.UnLock;
  end;
end;

function TWCRefConnection.TryToSendFrames(const TS : Qword) : Boolean;
begin
  //FDataSending.Lock;
  try
    if FSocketRef.ReadyToSend and
        ReadyToWrite and
        Assigned(FSendData) then
    begin
      FDataSending.Value := True;
      Refresh(TS);
      FSendData(Self);
      Result := true;
    end else
      Result := false;
  finally
    //FDataSending.UnLock;
  end;
end;

procedure TWCRefConnection.InitializeBuffers;
var SZ : Cardinal;
begin
  SZ := GetInitialReadBufferSize;
  if SZ > 0 then
    FReadBuffer := TThreadPointer.Create(SZ) else
    FReadBuffer := nil;
  FReadBufferSize:= SZ;
  SZ := GetInitialWriteBufferSize;
  if SZ > 0 then
    FWriteBuffer := TThreadPointer.Create(SZ) else
    FWriteBuffer := nil;
  FWriteBufferSize:= SZ;
end;

function TWCRefConnection.NextFrameToSend(it: TIteratorObject
  ): TIteratorObject;
begin
  Result := it;
end;

function TWCRefConnection.TruncReadBuffer(S : TBufferedStream) : Int64;
begin
  Result := S.Size - S.Position;
  if (Result > 0) and (S.Position > 0) then
     Move(PByte(ReadBuffer.Value)[S.Position], ReadBuffer.Value^, Result);
end;

function TWCRefConnection.ReadMore(Buffered : TBufferedStream;
                                   WriteAt : Int64) : Int64;
var R, AddSz : Int64;
    Src : Pointer;
begin
  AddSz := FReadBufferSize - WriteAt;
  R := AddSz;
  Result := WriteAt;
  While (AddSz > 0) and (R > 0) do
  begin
    if Buffered.Position < Buffered.Size then
    begin
      R := Buffered.Size - Buffered.Position;
      Src := Pointer(Buffered.Memory + Buffered.Position);
      if AddSz < R then R := AddSz;
      Move(Src^, PByte(FReadBuffer.Value)[Result], R);
      Buffered.Position := Buffered.Position + R;
    end
    else
    begin
      R:=FSocketRef.Read(PByte(FReadBuffer.Value)[Result], AddSz);
      If R <= 0 then
        break;
    end;
    if R > 0 then begin
      Dec(AddSz, R);
      Inc(Result, R);
    end else break;
  end;
end;

function TWCRefConnection.LoadMoreData(
  Buffered : TBufferedStream;
  WriteTo  : TBufferedStream;
  FallBackPos, ExtraSize, Offset : Int64) : Boolean;
var L : Int64;
begin
  Result := True;
  if (WriteTo.Size - WriteTo.Position) < ExtraSize then
  begin
    WriteTo.Position := FallBackPos;
    L := TruncReadBuffer(WriteTo);
    L := ReadMore(Buffered, L);
    if L < 0 then
    begin
      L := 0;
      Result := false;
    end;
    if (L - Offset) < ExtraSize then begin
      WriteTo.Position := 0;
      WriteTo.Size := L;
      Exit(false);
    end;
    WriteTo.SetPtr(ReadBuffer.Value, L);
    WriteTo.Position:= Offset;
  end;
end;

end.
