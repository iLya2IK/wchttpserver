{
 wcNetworking:
   Classes and other routings to deal with referenced connections,
   sockets, frames and streams

   Part of WCHTTPServer project

   Copyright (c) 2020-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcNetworking;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$ifdef linux}
{epoll mode appeared thanks to the lNet project
 CopyRight (C) 2004-2008 Ales Katona}
{$define socket_epoll_mode}
{.$define socket_select_mode}
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
  {$ifdef linux}{$ifdef socket_epoll_mode}
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

  TWCRequestRefWrapper = class(TNetReferencedObject)
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

  TSocketState = (ssCanRead, ssCanSend, ssReading, ssSending, ssError);
  TSocketStates = set of TSocketState;

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
  public
    constructor Create(aSocket : TSocketStream);
    destructor Destroy; override;

    {$ifdef socket_select_mode}
    procedure GetSocketStates;
    {$endif}
    procedure SetCanRead;
    procedure SetCanSend;
    procedure PushError;

    function  CanRead : Boolean;
    function  CanSend : Boolean;
    function  HasErrors : Boolean;
    function  HasNoErrors : Boolean;
    function  StartReading : Boolean;
    procedure StopReading;
    function  StartSending : Boolean;
    procedure StopSending;

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

  { TWCRefConnection }

  TWCRefConnection = class(TNetReferencedObject)
  private
    FOwner : TWCRefConnections;
    FConnectionState : TThreadSafeConnectionState;
    FReadBuffer, FWriteBuffer : TThreadPointer;
    FReadBufferSize, FWriteBufferSize : Cardinal;
    FReadTailSize, FWriteTailSize : Integer;
    FSocket : Cardinal;
    FTimeStamp, FReadStamp, FWriteStamp : TThreadQWord;
    FReadDelay, FWriteDelay : TThreadInteger;
    FFramesToSend : TThreadSafeFastSeq;
    FSocketRef : TWCSocketReference;
    FReadData : TRefReadSendData;
    FSendData : TRefReadSendData;
    FDataSending, FDataReading : TThreadBoolean;

    function GetConnectionState: TWCConnectionState;
    procedure SetConnectionState(CSt: TWCConnectionState);
    function ReadyToReadWrite(const TS : QWord) : Boolean;
    function ReadyToRead(const TS : QWord) : Boolean;
    function ReadyToWrite(const TS : QWord) : Boolean;
    procedure Refresh(const TS: QWord); virtual;
    procedure TryToConsumeFrames(const TS: Qword);
    procedure TryToSendFrames(const TS: Qword);
    procedure HoldDelayValue(aDelay : TThreadInteger);
    procedure RelaxDelayValue(aDelay : TThreadInteger);
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

  {$ifdef socket_epoll_mode}
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
  {$ifdef socket_epoll_mode}
  TWCEpollAcceptThread = class;
  TWCEpollIOThread = class;
  {$endif}
  TWCIOThread = class;

  { TWCRefConnections }

  TWCRefConnections = class(TThreadSafeFastSeq)
  private
    FLastUsedConnection : TIteratorObject;
    FMaintainStamp : QWord;
    FGarbageCollector : TNetReferenceList;
    FNeedToRemoveDeadConnections : TThreadBoolean;
    FHelpers : Array [TWCProtocolVersion] of TWCProtocolHelper;
    FIOThreads : Array of TWCIOThread;
    FMaxIOThreads : TThreadInteger;
    FServer       : TWCCustomHttpServer;
    {$ifdef socket_epoll_mode}
    FEpollThreads : TThreadInteger;
    FEpollIOThreads:  Array of TWCEpollIOThread;
    FEpollAcceptThread : TWCEpollAcceptThread;
    FEpollReadFD: THandle;   // this one monitors LT style for READ
    FEpollFD: THandle;       // this one monitors ET style for other
    procedure AttachEpoll(ACon : TWCRefConnection);
    function GetMaxIOThreads : Integer;
    procedure RemoveEpoll(ACon : TWCRefConnection);
    procedure ResetReadingEPoll(ACon : TWCRefConnection);
    procedure RemoveEpollAcceptThread(aThread : TWCEpollAcceptThread);
    procedure RemoveEpollIOThread(aThread : TWCEpollIOThread);
    {$endif}
    procedure RepairIOThreads;
    procedure RemoveIOThread(aThread : TWCIOThread);
    function GetProtocolHelper(Id : TWCProtocolVersion): TWCProtocolHelper;
    function IsConnDead(aConn: TObject; data: pointer): Boolean;
    procedure AfterConnExtracted(aObj : TObject);
    procedure SetMaxIOThreads(AValue : Integer);
    procedure SetServer(aServer : TWCCustomHttpServer);
  protected
    procedure HandleNetworkError(E : Exception); virtual;
  public
    constructor Create(aGarbageCollector : TNetReferenceList;
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
    property    GarbageCollector : TNetReferenceList read FGarbageCollector write
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

  {$ifdef socket_epoll_mode}

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

  { TWCCustomHttpServer }

  TWCCustomHttpServer = class(TEmbeddedAbsHttpServer)
  private
    FRefConnections : TWCRefConnections;
    FServerActive : Boolean;
  protected
    procedure HandleNetworkError(E : Exception); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateServerSocket; override;
    procedure StartServerSocket; override;
    procedure StopServerSocket; override;
  public
    property  RefConnections : TWCRefConnections read FRefConnections;
    property  ServerActive : Boolean read FServerActive;
  end;

implementation

const
  HTTP1_INITIAL_WRITE_BUFFER_SIZE = $FFFF;
  HTTP1_MAX_WRITE_BUFFER_SIZE     = $9600000;
{$ifdef socket_epoll_mode}
  BASE_SIZE = 100;
{$endif}

type
  TWCLifeTimeChecker = record
    CurTime : QWord;
    MaxLifeTime : Cardinal;
  end;

PWCLifeTimeChecker = ^TWCLifeTimeChecker;

{ TWCCustomHttpServer }

constructor TWCCustomHttpServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  QueueSize := 100;
  FRefConnections := TWCRefConnections.Create(nil, Self);
end;

destructor TWCCustomHttpServer.Destroy;
begin
  FRefConnections.Free;
  inherited Destroy;
end;

procedure TWCCustomHttpServer.CreateServerSocket;
begin
  inherited CreateServerSocket;
  AllowHandlerAccept := true; //false;
end;

procedure TWCCustomHttpServer.StartServerSocket;
begin
  IpServer.Bind;
  IpServer.SetNonBlocking;
  {$ifdef SOCKET_EPOLL_MODE}
  IpServer.Listen;
  FServerActive := true;
  FRefConnections.SetServer(Self);
  while FServerActive do
    IpServer.OnIdle(Self);
  {$else}
  IpServer.StartAccepting;
  {$endif}
end;

procedure TWCCustomHttpServer.StopServerSocket;
begin
  inherited StopServerSocket;
  FServerActive := false;
end;

procedure TWCCustomHttpServer.HandleNetworkError({%H-}E : Exception);
begin
  //do nothing
end;

{ TWCIOThread }

procedure TWCIOThread.Execute;
begin
  try
    while not Terminated do
    begin
      if FOwner.Count = 0 then
      begin
        Sleep(10);
        Continue;
      end;
      try
        if FOwner.IdleSocketsIO(GetTickCount64) then
          Sleep(0) else
          Sleep(1);
      except
        on E : Exception do HandleNetworkError(E);
      end;
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

{$ifdef socket_epoll_mode}

{ TWCEpollAcceptThread }

constructor TWCEpollAcceptThread.Create(aOwner : TWCRefConnections;
  aTimeOut : cInt);
begin
  inherited Create(aOwner, aTimeOut);
  FServerSocket := nil;
end;

procedure TWCEpollAcceptThread.Execute;
var ServerChanges, err, i, L : Integer;
    Addr : TINetSockAddr;
    FSocket : THandle;
    Stream : TSocketStream;
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
          Sleep(10);
          Continue;
        end;
        ServerChanges := epoll_wait(FInternalPoll, @(ServerEvents[0]), 16, FTimeout);
        if ServerChanges < 0 then
          err := fpgeterrno else
          err := 0;

        if (err <> 0) and (err <> ESysEINTR) then
          raise ESocketError.CreateFmt('Error on server epoll %d', [err])
        else
        with FOwner do
        if ServerChanges > 0 then
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
              repeat
                try
                  Stream := nil;
                  L:=SizeOf(Addr);
                  FSocket:=Sockets.fpAccept(FServerSocket.Socket,@Addr,@L);
                  err:=SocketError;
                  If (FSocket<0) then
                  begin
                    If (err = ESysEWOULDBLOCK) or (err = ESysEAGAIN) then
                    begin
                      Break;
                    end else
                      Raise ESocketError.Create(seAcceptFailed, [FServerSocket.Socket,
                                                                 err]);
                  end else
                  begin
                    if FServerSocket.NonBlocking then
                      fpfcntl(FSocket, F_SETFD, fpfcntl(FSocket, F_GetFD, 0) or O_NONBLOCK);
                    Stream := FServerSocket.SockToStream(FSocket);
                    if Assigned(Stream) then
                      FServerSocket.DoConnect(Stream);
                  end;
                  Sleep(0);
                except
                  on E : Exception do begin
                    HandleNetworkError(E);
                    if Assigned(Stream) then
                      FreeAndNil(Stream) else
                      Stream := nil;
                  end;
                end;
              until not Assigned(Stream);
            end;
          end;
        end;
        Sleep(0);
      end;

    except
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
  lEvent.events := EPOLLIN;// or EPOLLET;
  lEvent.data.fd := aSock.Socket;
  if epoll_ctl(FInternalPoll, EPOLL_CTL_ADD, aSock.Socket, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add Server socket to master epoll FD: %d', [fpGetErrno]);
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

        MasterChanges := epoll_wait(FInternalPoll, @(MasterEvents[0]), 3, FTimeout);
        if MasterChanges < 0 then
          err := fpgeterrno else
          err := 0;

        TS := GetTickCount64;

        if MasterChanges <= 0 then
        begin
          if (err <> 0) and (err <> ESysEINTR) then
            raise ESocketError.CreateFmt('Error on epoll %d', [err]) else
            Sleep(1);
        end else
        with FOwner do
        begin
          err := 0;
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
              repeat
                Changes := epoll_wait(FEpollFD, @FEvents[0], aCount, 0);
                if Changes < 0 then
                  err := fpgeterrno;
              until (Changes >= 0) or (err <> ESysEINTR);
            end
            else
            if (MasterEvents[i].Data.fd = FEpollReadFD) then
              repeat
                ReadChanges := epoll_wait(FEpollReadFD, @FEventsRead[0], aCount, 0);
                if ReadChanges < 0 then
                  err := fpgeterrno;
              until (ReadChanges >= 0) or (err <> ESysEINTR);
          end;
          if (Changes < 0) or (ReadChanges < 0) then
          begin
            if (err <> 0) and (err <> ESysEINTR) then
              raise ESocketError.CreateFmt('Error on epoll %d', [err]);
          end else
          begin
            m := Changes;
            if ReadChanges > m then m := ReadChanges;
            for i := 0 to m - 1 do begin
              Temp := nil;
              if i < Changes then begin
                Temp := TWCRefConnection(FEvents[i].data.ptr);

                if  ((FEvents[i].events and EPOLLERR) = EPOLLERR) then
                    Temp.SocketRef.PushError;

                if  ((FEvents[i].events and EPOLLOUT) = EPOLLOUT) then
                begin
                  Temp.SocketRef.SetCanSend;
                  Temp.TryToSendFrames(TS);
                end;
              end; // writes

              if i < ReadChanges then begin
                TempRead := TWCRefConnection(FEventsRead[i].data.ptr);

                if  ((FEventsRead[i].events and (EPOLLIN or EPOLLHUP or EPOLLPRI)) > 0) then
                begin
                  TempRead.SocketRef.SetCanRead;
                  TempRead.TryToConsumeFrames(TS);
                end;
              end; // reads
            end;
          end;
          Sleep(0);
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
  lEvent.events := EPOLLIN or EPOLLOUT or EPOLLPRI or EPOLLERR or EPOLLHUP or EPOLLET;
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
    raise ESocketError.CreateFmt('Error on event signal add to poll %d', [err]);
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
  //FD_ZERO(FReadFDSet);
  //FD_ZERO(FWriteFDSet);
  //FD_ZERO(FErrorFDSet);
  FWaitTime.tv_sec := 0;
  FWaitTime.tv_usec := 1000;
  {$endif}
  FSocketStates:=[ssCanSend, ssCanRead];
end;

destructor TWCSocketReference.Destroy;
begin
  if assigned(FSocket) then FreeAndNil(FSocket);
  {$ifdef socket_select_mode}
  FreeMem(Pointer(FReadFDSet),  SOCKET_FDSET_SIZE);
  FreeMem(Pointer(FWriteFDSet), SOCKET_FDSET_SIZE);
  FreeMem(Pointer(FErrorFDSet), SOCKET_FDSET_SIZE);
  {$endif}
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
    {$endif}
    FSocketStates:= FSocketStates - [ssCanRead, ssCanSend];
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
         FSocketStates:=FSocketStates + [ssError] else begin
        if FD_ISSET(Socket.Handle, FReadFDSet^) then
           FSocketStates:=FSocketStates + [ssCanRead];
        if FD_ISSET(Socket.Handle, FWriteFDSet^) then
           FSocketStates:=FSocketStates + [ssCanSend];
      end;
      {$endif}
      {$ifdef unix}
      if fpFD_ISSET(Socket.Handle, FErrorFDSet^)>0 then
         FSocketStates:=FSocketStates + [ssError] else begin
        if fpFD_ISSET(Socket.Handle, FReadFDSet^)>0 then
           FSocketStates:=FSocketStates + [ssCanRead];
        if fpFD_ISSET(Socket.Handle, FWriteFDSet^)>0 then
           FSocketStates:=FSocketStates + [ssCanSend];
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
  Lock;
  try
    FSocketStates:=FSocketStates + [ssCanRead];
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.SetCanSend;
begin
  Lock;
  try
    FSocketStates:=FSocketStates + [ssCanSend];
  finally
    UnLock;
  end;
end;

function TWCSocketReference.CanRead: Boolean;
begin
  Lock;
  try
    Result := ([ssCanRead, ssReading,
                ssSending, ssError] * FSocketStates) = [ssCanRead];
  finally
    UnLock;
  end;
end;

function TWCSocketReference.CanSend: Boolean;
begin
  Lock;
  try
    Result := ([ssCanSend, ssReading,
                ssSending, ssError] * FSocketStates) = [ssCanSend];
  finally
    UnLock;
  end;
end;

function TWCSocketReference.HasErrors: Boolean;
begin
  Lock;
  try
    Result := ssError in FSocketStates;
  finally
    UnLock;
  end;
end;

function TWCSocketReference.HasNoErrors: Boolean;
begin
  Result := not HasErrors;
end;

function TWCSocketReference.StartReading : Boolean;
begin
  Lock;
  try
    if CanRead then begin
      FSocketStates := FSocketStates + [ssReading];
      Result := true;
    end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.StopReading;
begin
  Lock;
  try
    FSocketStates := FSocketStates - [ssReading];
  finally
    UnLock;
  end;
end;

function TWCSocketReference.StartSending: Boolean;
begin
  Lock;
  try
    if CanSend then begin
      FSocketStates := FSocketStates + [ssSending];
      Result := true;
    end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.StopSending;
begin
  Lock;
  try
    FSocketStates := FSocketStates - [ssSending];
  finally
    UnLock;
  end;
end;

procedure TWCSocketReference.PushError;
begin
  Lock;
  try
    FSocketStates := FSocketStates + [ssError];
  finally
    UnLock;
  end;
end;

function TWCSocketReference.Write(const Buffer; Size: Integer): Integer;
begin
  if StartSending then
  try
    try
      Result := FSocket.Write(Buffer, Size);
      {$IFDEF SOCKET_EPOLL_MODE}
      if (Result <= 0) and (errno = ESysEAGAIN) then
      {$ENDIF}
      begin
        Lock;
        try
          FSocketStates := FSocketStates - [ssCanSend];
        finally
          UnLock;
        end;
      end;
    except
      on E : ESocketError do begin
       Result := -1;
       PushError;
       Raise;
      end;
    end;
  finally
    StopSending;
  end else Result := 0;
end;

function TWCSocketReference.Read(var Buffer; Size: Integer): Integer;
begin
  if StartReading then
  try
    try
      Result := FSocket.Read(Buffer, Size);
      {$IFDEF SOCKET_EPOLL_MODE}
      if (Result < Size) or (errno = ESysEAGAIN) then
      {$ENDIF}
      begin
        Lock;
        try
          FSocketStates := FSocketStates - [ssCanRead];
        finally
          UnLock;
        end;
      end;
    except
      on E : ESocketError do begin
       Result := -1;
       PushError;
       Raise;
      end;
    end;
  finally
    StopReading;
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
  if assigned(FSocketRef) then FSocketRef.DecReference;
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
  {$endif}
  TWCRefConnection(aObj).FFramesToSend.Clean;
  TWCRefConnection(aObj).DecReference;
end;

constructor TWCRefConnections.Create(aGarbageCollector : TNetReferenceList;
  aServer : TWCCustomHttpServer);
var v : TWCProtocolVersion;
  i : integer;
begin
  inherited Create;
  FServer := aServer;
  FNeedToRemoveDeadConnections := TThreadBoolean.Create(false);
  FMaintainStamp := GetTickCount64;
  FLastUsedConnection := nil;
  FGarbageCollector := aGarbageCollector;
  For V := Low(TWCProtocolVersion) to High(TWCProtocolVersion) do
    FHelpers[V] := nil;

  {$ifdef SOCKET_EPOLL_MODE}
  FEpollThreads := TThreadInteger.Create(2);
  FEpollFD := epoll_create(BASE_SIZE);
  FEpollReadFD := epoll_create(BASE_SIZE);
  if (FEPollFD < 0) or (FEpollReadFD < 0) then
    raise ESocketError.CreateFmt('Unable to create epoll: %d', [fpgeterrno]);

  SetLength(FEpollIOThreads, FEpollThreads.Value - 1);
  for i := 0 to High(FEpollIOThreads) do
  begin
    FEpollIOThreads[i] := TWCEpollIOThread.Create(Self, 3000);
    FEpollIOThreads[i].InitPoll(3);
    FEpollIOThreads[i].Start;
  end;
  FEpollAcceptThread := TWCEpollAcceptThread.Create(Self, 5000);
  FEpollAcceptThread.InitPoll(2);
  FEpollAcceptThread.Start;
  {$endif}

  FMaxIOThreads := TThreadInteger.Create(1);
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
    i : integer;
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
  for i := 0 to high(FIOThreads) do
  if Assigned(FIOThreads) then
    FIOThreads[i].Terminate;

  {$ifdef SOCKET_EPOLL_MODE}
  for i := 0 to high(FEpollIOThreads) do
  if Assigned(FEpollIOThreads[i]) then
    FEpollIOThreads[i].Stop;
  if Assigned(FEpollAcceptThread) then
  begin
    FEpollAcceptThread.Stop;
    FEpollAcceptThread.WaitFor;
  end;
  Sleep(0);
  fpClose(FEpollFD);
  fpClose(FEpollReadFD);
  FEpollThreads.Free;
  {$endif}

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
  if ((TS - FMaintainStamp) div 1000 > 10) or
     (FNeedToRemoveDeadConnections.Value) then
    RemoveDeadConnections(TS, 120);

  RepairIOThreads();
end;

function TWCRefConnections.IdleSocketsIO(const TS : QWord) : Boolean;
var C : TWCRefConnection;
    i : integer;
begin
  Result := false;
  if Count = 0 then Exit;

  i := 0;
  while i < 15 do
  begin
    C := TWCRefConnection(PopValue);
    if Assigned(C) then
    begin
      if (C.ConnectionAvaible) then
      begin
        if C.FSocketRef.HasErrors then
          C.ConnectionState:= wcDROPPED else
        if C.TryToIdleStep(TS) then
        begin
          Result := True;
        end;
      end;
      Push_back(C);
      Inc(i);
    end else
      Break;
  end;
end;

{
// old non-effective code
var P : TIteratorObject;
    C : TWCRefConnection;
    i : integer;
begin
  Result := false;
  if Count = 0 then Exit;

  Lock;
  try
    P := ListBegin;
    if assigned(FLastUsedConnection) then
    while assigned(P) do
    begin
      if (P = FLastUsedConnection) then begin
        break;
      end else begin
        P := P.Next;
        if not assigned(P) then begin
          P := ListBegin;
          FLastUsedConnection := nil;
          break;
        end;
      end;
    end;
    i := 0;
    while assigned(P) and (i < 15) do
    begin
      C := TWCRefConnection(P.Value);
      if (C.ConnectionAvaible) then
      begin
        if C.FSocketRef.HasErrors then
          C.ConnectionState:= wcDROPPED else
        if C.TryToIdleStep(TS) then
        begin
          Result := True;
          FLastUsedConnection := P.Next;
          Inc(i);
        end;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;   }

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
        end;
      end;
    finally
      FMaxIOThreads.UnLock;
    end;
  end;
end;

procedure TWCRefConnections.SetServer(aServer : TWCCustomHttpServer);
begin
  FServer := aServer;
  {$ifdef SOCKET_EPOLL_MODE}
  if Assigned(FEpollAcceptThread) then
    FEpollAcceptThread.SetServerSocket(aServer.IpServer);
  {$endif}
end;

procedure TWCRefConnections.HandleNetworkError(E : Exception);
begin
  if Assigned(FServer) then
    FServer.HandleNetworkError(E);
end;

{$ifdef SOCKET_EPOLL_MODE}

procedure TWCRefConnections.AttachEpoll(ACon : TWCRefConnection);
var
  i : integer;
  lEvent: TEpollEvent;
  aHandle : THandle;
begin
  ACon.SocketRef.IncReference;
  aHandle := ACon.SocketRef.FSocket.Handle;

  lEvent.events := EPOLLET or EPOLLOUT or EPOLLERR;
  lEvent.data.ptr := ACon;
  if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll: %d', [fpgeterrno]);
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, aHandle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll: %d', [fpgeterrno]);
  for i := 0 to high(FEpollIOThreads) do
    FEpollIOThreads[i].Attach(ACon);
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
  end;
end;

procedure TWCRefConnections.ResetReadingEPoll(ACon : TWCRefConnection);
var
  lEvent: TEpollEvent;
  err : Integer;
begin
  lEvent.data.ptr := ACon;
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP or EPOLLET or EPOLLONESHOT;
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
  FEpollThreads.Lock;
  try
    for i := 0 to High(FEpollIOThreads) do
    if FEpollIOThreads[i] = aThread then
    begin
      FEpollIOThreads[i] := nil;
      FEpollThreads.DecValue;
    end;
  finally
    FEpollThreads.UnLock;
  end;
end;

procedure TWCRefConnections.RemoveEpollAcceptThread(aThread : TWCEpollAcceptThread);
begin
  FEpollThreads.Lock;
  try
    if FEpollAcceptThread = aThread then
    begin
      FEpollAcceptThread := nil;
      FEpollThreads.DecValue;
    end;
  finally
    FEpollThreads.UnLock;
  end;
end;

{$endif}


procedure TWCRefConnections.RemoveIOThread(aThread : TWCIOThread);
var i : integer;
begin
  FMaxIOThreads.Lock;
  try
    for i := 0 to High(FIOThreads) do
    if FIOThreads[i] = aThread then
    begin
      FIOThreads[i] := nil;
      FMaxIOThreads.DecValue;
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
    if FMaxIOThreads.Value < Length(FIOThreads) then
    begin
      for i := 0 to High(FIOThreads) do
      begin
        if not Assigned(FIOThreads[i]) then
        begin
          FIOThreads[i] := TWCIOThread.Create(Self);
          FIOThreads[i].Start;
        end;
      end;
    end;
  finally
    FMaxIOThreads.UnLock;
  end;

  {$IFDEF SOCKET_EPOLL_MODE}
  FEpollThreads.Lock;
  try
    if FEpollThreads.Value < (Length(FEpollIOThreads) + 1) then
    begin
      for i := 0 to High(FEpollIOThreads) do
      begin
        if not Assigned(FEpollIOThreads[i]) then
        begin
          FEpollIOThreads[i] := TWCEpollIOThread.Create(Self, -1);
          FEpollIOThreads[i].InitPoll(3);
          FEpollIOThreads[i].Start;
          FEpollThreads.IncValue;
        end;
      end;
      if not Assigned(FEpollAcceptThread) then
      begin
        FEpollAcceptThread := TWCEpollAcceptThread.Create(Self, -1);
        FEpollAcceptThread.InitPoll(2);
        if Assigned(FServer) then
          FEpollAcceptThread.SetServerSocket(FServer.IpServer);
        FEpollAcceptThread.Start;
        FEpollThreads.IncValue;
      end;
    end;
  finally
    FEpollThreads.UnLock;
  end;
  {$ENDIF}
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

function TWCRefConnection.ReadyToReadWrite(const TS: QWord): Boolean;
begin
  Result := ReadyToRead(TS) or ReadyToWrite(TS);
end;

function TWCRefConnection.ReadyToRead(const TS: QWord): Boolean;
begin
  Result := ((Int64(TS) - Int64(FReadStamp.Value)) >= FReadDelay.Value) and
            (not FDataReading.Value);
end;

function TWCRefConnection.ReadyToWrite(const TS: QWord): Boolean;
begin
  Result := ((Int64(TS) - Int64(FWriteStamp.Value)) >= FWriteDelay.Value) and
            (not FDataSending.Value) and
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
  FTimeStamp := TThreadQWord.Create(TS);
  FReadTailSize := 0;
  FWriteTailSize:= 0;
  FReadData := aReadData;
  FSendData := aSendData;
  FDataSending := TThreadBoolean.Create(false);
  FDataReading := TThreadBoolean.Create(false);
  FReadStamp := TThreadQWord.Create(TS);
  FWriteStamp:= TThreadQWord.Create(TS);
  FReadDelay := TThreadInteger.Create(0);
  FWriteDelay := TThreadInteger.Create(0);
  FFramesToSend := TThreadSafeFastSeq.Create;
  FConnectionState := TThreadSafeConnectionState.Create(wcCONNECTED);
end;

procedure TWCRefConnection.ReleaseRead(ConsumeResult : TWCConsumeResult);
begin
  FDataReading.Value := false;
  {$ifdef SOCKET_EPOLL_MODE}
  try
    FOwner.ResetReadingEPoll(Self);
  except
    on ESocketError do ConsumeResult := wccrSocketError;
  end;
  {$endif}
  if (ConsumeResult in [wccrProtocolError, wccrWrongProtocol,
                        wccrSocketError]) then
  begin
    HoldDelayValue(FReadDelay);
  end else begin
    if ConsumeResult = wccrOK then
    begin
      Refresh(GetTickCount64);
      RelaxDelayValue(FReadDelay);
    end;
  end;
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
  FReadStamp.Free;
  FWriteStamp.Free;
  FTimeStamp.Free;
  FConnectionState.Free;
  FDataSending.Free;
  FDataReading.Free;
  FReadDelay.Free;
  FWriteDelay.Free;
  inherited Destroy;
end;

procedure TWCRefConnection.PushFrame(fr: TWCRefProtoFrame);
begin
  FFramesToSend.Push_back(fr);
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
              if Sz < 0 then Sz := 0; // ignore non-fatal errors. rollback to tail
              FWriteTailSize := WrBuf.Position + FWriteTailSize - Sz;
              if Sz > 0 then
                Move(Pointer(CurBuffer + Sz)^, CurBuffer^, FWriteTailSize);
              HoldDelayValue(FWriteDelay);
            end else begin
              FWriteTailSize:= 0;
              RelaxDelayValue(FWriteDelay);
            end;
          end;
        except
          on E: ESocketError do ConnectionState:= wcDROPPED;
        end;
      finally
        FWriteBuffer.UnLock;
      end;
  finally
    WrBuf.Free;
    FDataSending.Value := false;
  end;
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
  if Assigned(FSocketRef) and ReadyToReadWrite(TS) then
  begin
    {$ifdef socket_select_mode}
    try
      FSocketRef.GetSocketStates;
    except
      on e : ESocketError do ; //catch error
    end;
    if FSocketRef.HasErrors then
    begin
      Result := false;
      ConnectionState:= wcDROPPED;
      Exit;
    end;
    {$endif}
    TryToSendFrames(TS);
    TryToConsumeFrames(TS);
    Result := true;
  end;
end;

function TWCRefConnection.ConnectionAvaible: Boolean;
begin
  Result := ConnectionState in [wcCONNECTED, wcHALFCLOSED];
end;

procedure TWCRefConnection.TryToConsumeFrames(const TS : Qword);
begin
  FDataReading.Lock;
  try
    if (FSocketRef.CanRead or RequestsWaiting) and
        ReadyToRead(TS) and
        assigned(FReadData) then
    begin
      FDataReading.Value := True;
      FReadStamp.Value := TS;
      FReadData(Self);
    end;
  finally
    FDataReading.UnLock;
  end;
end;

procedure TWCRefConnection.TryToSendFrames(const TS : Qword);
begin
  FDataSending.Lock;
  try
    if FSocketRef.CanSend and
       ReadyToWrite(TS) and
       Assigned(FSendData) then
    begin
      FDataSending.Value := True;
      Refresh(TS);
      FWriteStamp.Value  := TS;
      FSendData(Self);
    end;
  finally
    FDataSending.UnLock;
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

procedure TWCRefConnection.HoldDelayValue(aDelay: TThreadInteger);
begin
  aDelay.Lock;
  try
    if aDelay.Value = 0 then
      aDelay.Value := 4 else
    begin
      aDelay.Value := aDelay.Value * 2;
      if (aDelay.Value > 512) then aDelay.Value := 512;
    end;
  finally
    aDelay.UnLock;
  end;
end;

procedure TWCRefConnection.RelaxDelayValue(aDelay: TThreadInteger);
begin
  aDelay.Lock;
  try
    if (aDelay.Value > 4) then aDelay.Value := 4 else
    aDelay.Value := aDelay.Value div 2;
  finally
    aDelay.UnLock;
  end;
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
