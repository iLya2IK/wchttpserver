{
 WCHTTP2Server:
   Classes and other routings to deal with HTTP2 connections,
   frames and streams
   plus cross-protocols conversions HTTP2 <-> HTTP1.1 for
   fpHTTP/fpweb compability

   Part of WCHTTPServer project

   Copyright (c) 2020-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wchttp2server;

{$mode objfpc}{$H+}
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
  Classes, SysUtils,
  ECommonObjs, OGLFastList,
  fphttp, HTTPDefs, httpprotocol, abstracthttpserver,
  BufferedStream,
  ssockets,
  sockets,
  {$ifdef unix}
    BaseUnix,Unix,
  {$endif}
  {$ifdef linux}{$ifdef socket_epoll_mode}
    Linux,
  {$endif}{$endif}
  {$ifdef windows}
    winsock2, windows,
  {$endif}
  uhpack,
  http2consts,
  http2http1conv;

const
  WC_INITIAL_READ_BUFFER_SIZE = 4096;

type

  TWCHTTPStreams = class;
  TWCHTTP2Connection = class;
  TWCHTTPStream = class;

  TWCConnectionState = (wcCONNECTED, wcDROPPED, wcDEAD);
  TWCProtocolVersion = (wcUNK, wcHTTP1, wcHTTP2);

  { TWCHTTP2FrameHeader }

  TWCHTTP2FrameHeader = class
  public
    PayloadLength : Integer; //24 bit
    FrameType : Byte;
    FrameFlag : Byte;
    StreamID  : Cardinal;
    Reserved  : Byte;
    procedure LoadFromStream(Str : TStream);
    procedure SaveToStream(Str : TStream);
  end;

  { TWCHTTP2ProtoFrame }

  TWCHTTP2ProtoFrame = class
  public
    procedure SaveToStream(Str : TStream); virtual; abstract;
    function Size : Int64; virtual; abstract;
  end;
  
  { TWCHTTP2Frame }
  
  TWCHTTP2Frame = class(TWCHTTP2ProtoFrame)
  public
    Header  : TWCHTTP2FrameHeader;
    Payload : Pointer;  
    constructor Create(aFrameType : Byte; 
                       StrID : Cardinal; 
                       aFrameFlags : Byte; 
                       aData : Pointer; 
                       aDataSize : Cardinal);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTP2AdvFrame }

  TWCHTTP2AdvFrame = class(TWCHTTP2ProtoFrame)
  public
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTP2UpgradeResponseFrame }

  TWCHTTP2UpgradeResponseFrame = class(TWCHTTP2ProtoFrame)
  private
    FMode : THTTP2OpenMode;
  public
    constructor Create(Mode : THTTP2OpenMode);
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTP2Block }

  TWCHTTP2Block = class
  private
    FCurDataBlock  : Pointer;
    FDataBlockSize : Integer;
    FConnection    : TWCHTTP2Connection;
    FStream        : TWCHTTPStream;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); virtual;
    destructor Destroy; override;
    // avaible data
    procedure PushData(Data : Pointer; sz : Cardinal); overload;
    procedure PushData(Strm: TStream; startAt: Int64); overload;
    procedure PushData(Strings: TStrings); overload;
    property  DataBlock : Pointer read FCurDataBlock;
    property  DataBlockSize : Integer read FDataBlockSize;
  end;

  { TWCHTTP2SerializeStream }

  TWCHTTP2SerializeStream = class(TStream)
  private
    FConn  : TWCHTTP2Connection;
    FStrmID  : Cardinal;
    FCurFrame : TWCHTTP2Frame;
    FFirstFrameType, FNextFramesType : Byte;
    FFlags, FFinalFlags : Byte;
    FRestFrameSz : Longint;
    FChuncked : Boolean;
    FFirstFramePushed : Boolean;
  public
    constructor Create(aConn: TWCHTTP2Connection; aStrm: Cardinal;
                       aFirstFrameType : Byte;
                       aNextFramesType : Byte;
                       aFlags, aFinalFlags: Byte);
    function Write(const Buffer; Count: Longint): Longint; override;
    destructor Destroy; override;

    property FirstFrameType : Byte read FFirstFrameType write FFirstFrameType;
    property NextFramesType : Byte read FNextFramesType write FNextFramesType;
    property Flags : Byte read FFlags write FFlags;
    property FinalFlags : Byte read FFinalFlags write FFinalFlags;
    property Chuncked : Boolean read FChuncked write FChuncked;
  end;

  { TThreadSafeHPackEncoder }

  TThreadSafeHPackEncoder = class(TNetReferencedObject)
  private
    FEncoder : THPackEncoder;
  public
    constructor Create(TableSize : Cardinal);
    destructor Destroy; override;
    procedure EncodeHeader(aOutStream: TStream;
                       const aName: RawByteString;
                       const aValue: RawByteString; const aSensitive: Boolean);
  end;

  { TThreadSafeHPackDecoder }

  TThreadSafeHPackDecoder = class(TNetReferencedObject)
  private
    FDecoder : THPackDecoder;
    function GetDecodedHeaders: THPackHeaderTextList;
  public
    constructor Create(HeadersListSize, TableSize: Cardinal);
    destructor Destroy; override;
    procedure Decode(aStream: TStream);
    function  EndHeaderBlockTruncated: Boolean;
    property  DecodedHeaders: THPackHeaderTextList read GetDecodedHeaders;
  end;

  { TWCHTTP2ResponseHeaderPusher }

  TWCHTTP2ResponseHeaderPusher = class
  private
    FMem : TMemoryStream;
    FHPackEncoder : TThreadSafeHPackEncoder;
  protected
    property HPackEncoder : TThreadSafeHPackEncoder read FHPackEncoder write FHPackEncoder;
  public
    constructor Create(aHPackEncoder : TThreadSafeHPackEncoder);
    destructor Destroy; override;
    procedure PushHeader(const H, V : String); virtual; abstract;
    procedure PushAll(R: TResponse);
  end;

  { TWCHTTP2BufResponseHeaderPusher }

  TWCHTTP2BufResponseHeaderPusher = class(TWCHTTP2ResponseHeaderPusher)
  private
    FBuf : Pointer;
    FCapacity : Cardinal;
    FSize : Cardinal;
    FBufGrowValue : Cardinal;
  public
    constructor Create(aHPackEncoder : TThreadSafeHPackEncoder;
                       aBuffer : Pointer;
                       aBufferSize,
                       aBufGrowValue : Cardinal);
    procedure PushHeader(const H, V : String); override;
    property Buffer : Pointer read FBuf;
    property Size : Cardinal read FSize;
  end;

  { TWCHTTP2StrmResponseHeaderPusher }

  TWCHTTP2StrmResponseHeaderPusher = class(TWCHTTP2ResponseHeaderPusher)
  private
    FStrm : TStream;
  public
    constructor Create(aHPackEncoder : TThreadSafeHPackEncoder; aStrm : TStream);
    procedure PushHeader(const H, V : String); override;
  end;

  { TWCHTTP2Response }

  TWCHTTP2Response = class(TWCHTTP2Block)
  private
    FCurHeadersBlock : Pointer;
    FHeadersBlockSize : Longint;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); override;
    destructor Destroy; override;
    procedure CopyFromHTTP1Response(R : TResponse);
    procedure Close;
    procedure SerializeResponse;
    procedure SerializeHeaders(closeStrm: Boolean);
    procedure SerializeData(closeStrm: Boolean);
    procedure SerializeResponseHeaders(R : TResponse; closeStrm: Boolean);
    procedure SerializeResponseData(R : TResponse; closeStrm: Boolean);
  end;

  { TWCHTTP2Request }

  TWCHTTP2Request = class(TWCHTTP2Block)
  private
    FComplete : Boolean;
    FResponse : TWCHTTP2Response;
    FHeaders  : THPackHeaderTextList;
    function GetResponse: TWCHTTP2Response;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); override;
    destructor Destroy; override;
    procedure CopyHeaders(aHPackDecoder : TThreadSafeHPackDecoder);
    procedure CopyToHTTP1Request(ARequest : TRequest);
    property  Response : TWCHTTP2Response read GetResponse;
    property  Complete : Boolean read FComplete write FComplete;
  end;

  { TWCHTTPStream }

  TWCHTTPStream = class(TNetReferencedObject)
  private
    FID : Cardinal;
    FConnection : TWCHTTP2Connection;
    FStreamState : THTTP2StreamState;
    FCurRequest : TWCHTTP2Request;
    FPriority : Byte;
    FRecursedPriority : ShortInt;
    FParentStream : Cardinal;
    FFinishedCode : Cardinal;
    FWaitingForContinueFrame : Boolean;
    FWaitingRemoteStream : Cardinal;
    FHeadersComplete : Boolean;
    FResponseProceed : Boolean;
    function GetRecursedPriority: Byte;
    function GetResponseProceed: Boolean;
    procedure ResetRecursivePriority;
    procedure PushRequest;
    procedure SetResponseProceed(AValue: Boolean);
    procedure SetWaitingForContinueFrame(AValue: Boolean);
    procedure UpdateState(Head : TWCHTTP2FrameHeader);
  protected
    property WaitingForContinueFrame : Boolean read FWaitingForContinueFrame write
                                         SetWaitingForContinueFrame;
    procedure PushData(Data : Pointer; sz : Cardinal);
    procedure FinishHeaders(aDecoder : TThreadSafeHPackDecoder);
  public
    constructor Create(aConnection : TWCHTTP2Connection; aStreamID : Cardinal);
    destructor Destroy; override;
    procedure Release;
    property ID : Cardinal read FID;
    property StreamState : THTTP2StreamState read FStreamState;
    property ParentStream : Cardinal read FParentStream;
    property Priority :  Byte read FPriority;
    property RecursedPriority : Byte read GetRecursedPriority;
    // avaible request
    function RequestReady : Boolean;
    property Request : TWCHTTP2Request read FCurRequest;
    property ResponseProceed : Boolean read GetResponseProceed write SetResponseProceed;
  end;

  TSocketState = (ssCanRead, ssCanSend, ssReading, ssSending, ssError);
  TSocketStates = set of TSocketState;

  { TWCHTTPSocketReference }

  TWCHTTPSocketReference = class(TNetReferencedObject)
  private
    {$ifdef socket_select_mode}
    FReadFDSet,
      FWriteFDSet,
      FErrorFDSet: PFDSet;
    FWaitTime : TTimeVal;
    {$endif}
    FSocket : TSocketStream;
    FSocketStates: TSocketStates;
    FReadingLocks, FWritingLocks : Integer;
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
    function  StartReading : Boolean;
    procedure StopReading;
    function  StartSending : Boolean;
    procedure StopSending;

    function Write(const Buffer; Size : Integer) : Integer;
    function Read(var Buffer; Size : Integer) : Integer;
    property Socket : TSocketStream read FSocket;
    property States : TSocketStates read FSocketStates;
  end;

  THttp2SocketConsume = procedure (SockRef : TWCHTTPSocketReference) of object;
  THttp2SendData = procedure (aConnection : TWCHTTP2Connection) of object;

  { TWCHTTPConnection }

  TWCHTTPConnection = class(TAbsHTTPConnection)
  private
    FHTTP2Con  : TWCHTTP2Connection;
    FHTTP2Str  : TWCHTTPStream;
    FSocketRef : TWCHTTPSocketReference;
    procedure SetHTTP2Con(AValue: TWCHTTP2Connection);
    procedure SetHTTP2Stream(AValue: TWCHTTPStream);
  protected
    procedure DoSocketAttach(ASocket : TSocketStream); override;
    function  GetSocket : TSocketStream; override;
  public
    Constructor Create(AServer : TAbsCustomHTTPServer; ASocket : TSocketStream); override;
    Constructor CreateRefered(AServer : TAbsCustomHTTPServer; ASocketRef : TWCHTTPSocketReference); virtual;
    destructor Destroy; override;
    procedure IncSocketReference;
    procedure DecSocketReference;
    property SocketReference : TWCHTTPSocketReference read FSocketRef;
    property HTTP2Con: TWCHTTP2Connection read FHTTP2Con write SetHTTP2Con;
    property HTTP2Str: TWCHTTPStream read FHTTP2Str write SetHTTP2Stream;
  end;

  { TThreadSafeConnSettings }

  TThreadSafeConnSettings = class(TThreadSafeObject)
  private
    FConSettings : Array [1..HTTP2_SETTINGS_MAX] of Cardinal;
    function GetConnSetting(id : Word): Cardinal;
    procedure SetConnSetting(id : Word; AValue: Cardinal);
  public
    property ConnSettings[id : Word] : Cardinal read GetConnSetting write SetConnSetting; default;
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

  { TWCHTTP2Connection }

  TWCHTTP2Connection = class(TNetReferencedObject)
  private
    FConnectionState : TThreadSafeConnectionState;
    FGarbageCollector : TNetReferenceList;
    FReadBuffer, FWriteBuffer : TThreadPointer;
    FReadBufferSize, FWriteBufferSize : Cardinal;
    FReadTailSize, FWriteTailSize : Integer;
    FSocket : Cardinal;
    FTimeStamp, FReadStamp, FWriteStamp : TThreadQWord;
    FLastStreamID : Cardinal;
    FStreams : TWCHTTPStreams;
    FConSettings : TThreadSafeConnSettings;
    FFramesToSend : TThreadSafeFastSeq;
    FErrorData : Pointer;
    FErrorDataSize : Cardinal;
    FLastError : Cardinal;
    FErrorStream : Cardinal;
    FSocketRef : TWCHTTPSocketReference;
    FSocketConsume : THttp2SocketConsume;
    FSendData      : THttp2SendData;
    FDataSending  : TThreadBoolean;
    FHPackDecoder : TThreadSafeHPackDecoder;
    FHPackEncoder : TThreadSafeHPackEncoder;

    function GetConnectionState: TWCConnectionState;
    procedure SetConnectionState(CSt: TWCConnectionState);
    function ReadyToReadWrite(const TS : QWord) : Boolean;
    function ReadyToRead(const TS : QWord) : Boolean;
    function ReadyToWrite(const TS : QWord) : Boolean;
    function AddNewStream(aStreamID: Cardinal): TWCHTTPStream;
    function GetConnSetting(id : Word): Cardinal;
    function GetLifeTime: Cardinal;
    procedure Refresh;
    procedure TryToConsumeFrames(const TS: Qword);
    procedure TryToSendFrames(const TS: Qword);
  protected
    procedure ResetHPack;
    procedure InitHPack;
    property  CurHPackDecoder : TThreadSafeHPackDecoder read FHPackDecoder;
    property  CurHPackEncoder : TThreadSafeHPackEncoder read FHPackEncoder;
  public
    constructor Create(aGarbageCollector: TNetReferenceList;
        aSocket: TWCHTTPSocketReference; aOpenningMode: THTTP2OpenMode;
        aSocketConsume: THttp2SocketConsume; aSendData: THttp2SendData);
    procedure ConsumeNextFrame(Mem : TBufferedStream);
    procedure SendFrames;
    destructor Destroy; override;
    class function CheckProtocolVersion(Data : Pointer; sz : integer) :
                                             TWCProtocolVersion;
    procedure PushFrame(aFrameType : Byte;
                        StrID : Cardinal;
                        aFrameFlags : Byte;
                        aData : Pointer;
                        aDataSize : Cardinal); overload;
    procedure PushFrame(fr : TWCHTTP2ProtoFrame); overload;
    function PopRequestedStream : TWCHTTPStream;
    function TryToIdleStep(const TS: Qword): Boolean;
    property Socket : Cardinal read FSocket;
    // lifetime in seconds
    property LifeTime : Cardinal read GetLifeTime;
    property Streams : TWCHTTPStreams read FStreams;
    // error
    property LastError : Cardinal read FLastError;
    property ErrorStream : Cardinal read FErrorStream;
    property ErrorDataSize : Cardinal read FErrorDataSize;
    property ErrorData : Pointer read FErrorData;
    //
    property ConnectionState : TWCConnectionState read GetConnectionState write SetConnectionState;
    property ConnSettings[id : Word] : Cardinal read GetConnSetting;
  end;

  { TWCHTTPStreams }

  TWCHTTPStreams = class(TThreadSafeFastSeq)
  private
    function IsStreamClosed(aStrm: TObject; data: pointer): Boolean;
    procedure AfterStrmExtracted(aObj : TObject);
  public
    destructor Destroy; override;
    function GetByID(aID : Cardinal) : TWCHTTPStream;
    function GetNextStreamWithRequest : TWCHTTPStream;
    function HasStreamWithRequest: Boolean;
    procedure CloseOldIdleStreams(aMaxId : Cardinal);
    procedure RemoveClosedStreams;
  end;

  {$ifdef socket_epoll_mode}
  PEpollEvent = ^epoll_event;
  TEpollEvent = epoll_event;
  PEpollData = ^epoll_data;
  TEpollData = epoll_data;
  {$endif}

  { TWCHTTP2Connections }

  TWCHTTP2Connections = class(TThreadSafeFastSeq)
  private
    FLastUsedConnection : TIteratorObject;
    FMaintainStamp : QWord;
    {$ifdef socket_epoll_mode}
    FTimeout: cInt;
    FEvents: array of TEpollEvent;
    FEventsRead: array of TEpollEvent;
    FEpollReadFD: THandle;   // this one monitors LT style for READ
    FEpollFD: THandle;       // this one monitors ET style for other
    FEpollMasterFD: THandle; // this one monitors the first two
    FEpollLocker : TNetCustomLockedObject;
    procedure AddSocketEpoll(ASocket : TWCHTTPSocketReference);
    procedure RemoveSocketEpoll(ASocket : TWCHTTPSocketReference);
    procedure Inflate;
    procedure CallActionEpoll(aCount : Integer);
    {$endif}
    function IsConnDead(aConn: TObject; data: pointer): Boolean;
    procedure AfterConnExtracted(aObj : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  AddConnection(FConn : TWCHTTP2Connection);
    function   GetByHandle(aSocket : Cardinal) : TWCHTTP2Connection;
    procedure  RemoveDeadConnections(MaxLifeTime : Cardinal);
    procedure  Idle;
  end;

implementation

uses uhpackimp;

const
  HTTP1HeadersAllowed = [$0A,$0D,$20,$21,$24,$25,$27..$39,$40..$5A,$61..$7A];
{$ifdef socket_epoll_mode}
  BASE_SIZE = 100;
{$endif}

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

{ TThreadSafeConnSettings }

function TThreadSafeConnSettings.GetConnSetting(id : Word): Cardinal;
begin
  lock;
  try
    Result := FConSettings[id];
  finally
    UnLock;
  end;
end;

procedure TThreadSafeConnSettings.SetConnSetting(id : Word; AValue: Cardinal);
begin
  lock;
  try
    FConSettings[id] := AValue;
  finally
    UnLock;
  end;
end;

{ TThreadSafeHPackEncoder }

constructor TThreadSafeHPackEncoder.Create(TableSize: Cardinal);
begin
  inherited Create;
  FEncoder := THPackEncoder.Create(TableSize);
end;

destructor TThreadSafeHPackEncoder.Destroy;
begin
  FEncoder.Free;
  inherited Destroy;
end;

procedure TThreadSafeHPackEncoder.EncodeHeader(aOutStream: TStream;
  const aName: RawByteString; const aValue: RawByteString;
  const aSensitive: Boolean);
begin
  Lock;
  try
    FEncoder.EncodeHeader(aOutStream, aName, aValue, aSensitive);
  finally
    UnLock;
  end;
end;

{ TThreadSafeHPackDecoder }

function TThreadSafeHPackDecoder.GetDecodedHeaders: THPackHeaderTextList;
begin
  Lock;
  try
    Result := FDecoder.DecodedHeaders;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeHPackDecoder.Create(HeadersListSize, TableSize: Cardinal
  );
begin
  Inherited Create;
  FDecoder := THPackDecoder.Create(HeadersListSize, TableSize);
end;

destructor TThreadSafeHPackDecoder.Destroy;
begin
  FDecoder.Free;
  inherited Destroy;
end;

procedure TThreadSafeHPackDecoder.Decode(aStream: TStream);
begin
  Lock;
  try
    FDecoder.Decode(aStream);
  finally
    UnLock;
  end;
end;

function TThreadSafeHPackDecoder.EndHeaderBlockTruncated: Boolean;
begin
  Lock;
  try
    Result := FDecoder.EndHeaderBlockTruncated;
  finally
    UnLock;
  end;
end;

{ TWCHTTPSocketReference }

constructor TWCHTTPSocketReference.Create(aSocket: TSocketStream);
begin
  inherited Create;
  FSocket := aSocket;
  {$ifdef socket_select_mode}
  {$ifdef windows}
  FReadFDSet:= GetMem(Sizeof(Cardinal) + Sizeof(TSocket)*1);
  FWriteFDSet:= GetMem(Sizeof(Cardinal) + Sizeof(TSocket)*1);
  FErrorFDSet:= GetMem(Sizeof(Cardinal) + Sizeof(TSocket)*1);
  {$endif}
  {$ifdef linux}
  FReadFDSet:= GetMem(Sizeof(TFDSet));
  FWriteFDSet:= GetMem(Sizeof(TFDSet));
  FErrorFDSet:= GetMem(Sizeof(TFDSet));
  {$endif}
  //FD_ZERO(FReadFDSet);
  //FD_ZERO(FWriteFDSet);
  //FD_ZERO(FErrorFDSet);
  FWaitTime.tv_sec := 0;
  FWaitTime.tv_usec := 1000;
  {$endif}
  FSocketStates:=[ssCanSend, ssCanRead];
  FReadingLocks := 0;
  FWritingLocks := 0;
end;

destructor TWCHTTPSocketReference.Destroy;
begin
  if assigned(FSocket) then FreeAndNil(FSocket);
  {$ifdef socket_select_mode}
  FreeMem(FReadFDSet);
  FreeMem(FWriteFDSet);
  FreeMem(FErrorFDSet);
  {$endif}
  inherited Destroy;
end;

{$ifdef socket_select_mode}
procedure TWCHTTPSocketReference.GetSocketStates;
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
    FSocketStates:=[];
    if n < 0 then
    begin
      err := socketerror;
      if (err = EsockENOTSOCK) then
      begin
        FSocketStates := FSocketStates + [ssError];
        Raise ESocketError.Create(seListenFailed,[Socket.Handle,err]);
      end;
    end else
    if n > 0 then
    begin
      {$ifdef windows}
      if FD_ISSET(Socket.Handle, FReadFDSet^) then
         FSocketStates:=FSocketStates + [ssCanRead];
      if FD_ISSET(Socket.Handle, FWriteFDSet^) then
         FSocketStates:=FSocketStates + [ssCanSend];
      if FD_ISSET(Socket.Handle, FErrorFDSet^) then
         FSocketStates:=FSocketStates + [ssError];
      {$endif}
      {$ifdef unix}
      if fpFD_ISSET(Socket.Handle, FReadFDSet^)>0 then
         FSocketStates:=FSocketStates + [ssCanRead];
      if fpFD_ISSET(Socket.Handle, FWriteFDSet^)>0 then
         FSocketStates:=FSocketStates + [ssCanSend];
      if fpFD_ISSET(Socket.Handle, FErrorFDSet^)>0 then
         FSocketStates:=FSocketStates + [ssError];
      {$endif}
    end;
  finally
    UnLock;
  end;
end;
{$endif}

procedure TWCHTTPSocketReference.SetCanRead;
begin
  Lock;
  try
    FSocketStates:=FSocketStates + [ssCanRead];
  finally
    UnLock;
  end;
end;

procedure TWCHTTPSocketReference.SetCanSend;
begin
  Lock;
  try
    FSocketStates:=FSocketStates + [ssCanSend];
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.CanRead: Boolean;
begin
  Lock;
  try
    Result := (not (ssReading in FSocketStates)) and
              (not (ssSending in FSocketStates)) and
              (not (ssError in FSocketStates)) and
              (ssCanRead in FSocketStates);
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.CanSend: Boolean;
begin
  Lock;
  try
    Result := (not (ssReading in FSocketStates)) and
              (not (ssSending in FSocketStates)) and
              (not (ssError in FSocketStates)) and
              (ssCanSend in FSocketStates);
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.StartReading : Boolean;
begin
  Lock;
  try
  if CanRead then begin
    Inc(FReadingLocks);
    if FReadingLocks > 0 then begin
      FSocketStates := FSocketStates + [ssReading];
      Result := true;
    end else Result := false;
  end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPSocketReference.StopReading;
begin
  Lock;
  try
    Dec(FReadingLocks);
    if FReadingLocks <= 0 then
      FSocketStates := FSocketStates - [ssReading];
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.StartSending: Boolean;
begin
  Lock;
  try
  if CanSend then begin
    Inc(FWritingLocks);
    if FWritingLocks > 0 then begin
      FSocketStates := FSocketStates + [ssSending];
      Result := true;
    end else Result := false;
  end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPSocketReference.StopSending;
begin
  Lock;
  try
    Dec(FWritingLocks);
    if FWritingLocks <= 0 then
      FSocketStates := FSocketStates - [ssSending];
  finally
    UnLock;
  end;
end;

procedure TWCHTTPSocketReference.PushError;
begin
  Lock;
  try
    FSocketStates := FSocketStates + [ssError];
  finally
    UnLock;
  end;
end;

const
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
{$IFDEF WINDOWS}
function IsBlockError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = ESysEWOULDBLOCK) or (anError = ESysENOBUFS) or
            (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_WRITE);
end;
{$else}
function IsBlockError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = ESysEWOULDBLOCK) or (anError = ESysENOBUFS) or
            (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_WRITE);
end;
{$endif}

function TWCHTTPSocketReference.Write(const Buffer; Size: Integer): Integer;
begin
  if StartSending then
  try
    Result := FSocket.Write(Buffer, Size);
    Lock;
    try
      FSocketStates := FSocketStates - [ssCanSend];
    finally
      UnLock;
    end;
  finally
    StopSending;
  end else Result := 0;
end;

function TWCHTTPSocketReference.Read(var Buffer; Size: Integer): Integer;
begin
  if StartReading then
  try
    Result := FSocket.Read(Buffer, Size);
    Lock;
    try
      FSocketStates := FSocketStates - [ssCanRead];
    finally
      UnLock;
    end;
  finally
    StopReading;
  end else Result := 0;
end;

{ TWCHTTPConnection }

procedure TWCHTTPConnection.SetHTTP2Con(AValue: TWCHTTP2Connection);
begin
  if FHTTP2Con=AValue then Exit;
  if assigned(FHTTP2Con) then
  begin
    FHTTP2Con.DecReference;
  end;
  SetHTTP2Stream(nil);
  FHTTP2Con:=AValue;
end;

procedure TWCHTTPConnection.SetHTTP2Stream(AValue: TWCHTTPStream);
begin
  if FHTTP2Str=AValue then Exit;
  if Assigned(FHTTP2Str) then FHTTP2Str.Release;  //release here!
  FHTTP2Str:=AValue;
end;

procedure TWCHTTPConnection.DoSocketAttach(ASocket: TSocketStream);
begin
  FSocketRef := TWCHTTPSocketReference.Create(ASocket);
end;

function TWCHTTPConnection.GetSocket: TSocketStream;
begin
  Result := FSocketRef.Socket;
end;

constructor TWCHTTPConnection.Create(AServer: TAbsCustomHTTPServer;
  ASocket: TSocketStream);
begin
  FSocketRef:= nil;
  inherited Create(AServer, ASocket);
  FHTTP2Con:=nil;
  FHTTP2Str:=nil;
end;

constructor TWCHTTPConnection.CreateRefered(AServer: TAbsCustomHTTPServer;
  ASocketRef: TWCHTTPSocketReference);
begin
  inherited Create(AServer, nil);
  FSocketRef := ASocketRef;
  ASocketRef.IncReference;
  FHTTP2Con:=nil;
  FHTTP2Str:=nil;
end;

destructor TWCHTTPConnection.Destroy;
begin
  SetHTTP2Con(nil);
  if assigned(FSocketRef) then FSocketRef.DecReference;
  FSocketRef := nil;
  inherited Destroy;
end;

procedure TWCHTTPConnection.IncSocketReference;
begin
  if assigned(FSocketRef) then FSocketRef.IncReference;
end;

procedure TWCHTTPConnection.DecSocketReference;
begin
  if assigned(FSocketRef) then FSocketRef.DecReference;
end;

{ TWCHTTP2UpgradeResponseFrame }

constructor TWCHTTP2UpgradeResponseFrame.Create(Mode: THTTP2OpenMode);
begin
  FMode:= Mode;
end;

procedure TWCHTTP2UpgradeResponseFrame.SaveToStream(Str: TStream);
var Buffer : Pointer;
    BufferSize : Cardinal;
begin
  case FMode of
    h2oUpgradeToH2C : begin
      Buffer := @(HTTP2UpgradeBlockH2C[1]);
      BufferSize:= HTTP2UpgradeBlockH2CSize;
    end;
    h2oUpgradeToH2 : begin
      Buffer := @(HTTP2UpgradeBlockH2[1]);
      BufferSize:= HTTP2UpgradeBlockH2Size;
    end;
  end;
  Str.WriteBuffer(Buffer^, BufferSize);
end;

function TWCHTTP2UpgradeResponseFrame.Size: Int64;
begin
  case FMode of
    h2oUpgradeToH2C : begin
      Result:= HTTP2UpgradeBlockH2CSize;
    end;
    h2oUpgradeToH2 : begin
      Result:= HTTP2UpgradeBlockH2Size;
    end;
  else
    Result := 0;
  end;
end;

{ TWCHTTP2AdvFrame }

procedure TWCHTTP2AdvFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(HTTP2Preface, H2P_PREFACE_SIZE);
end;

function TWCHTTP2AdvFrame.Size: Int64;
begin
  Result := H2P_PREFACE_SIZE;
end;

{ TWCHTTP2StrmResponseHeaderPusher }

constructor TWCHTTP2StrmResponseHeaderPusher.Create(
  aHPackEncoder: TThreadSafeHPackEncoder; aStrm: TStream);
begin
  inherited Create(aHPackEncoder);
  FStrm := aStrm;
end;

procedure TWCHTTP2StrmResponseHeaderPusher.PushHeader(const H, V: String);
begin
  FMem.Position:=0;
  FHPackEncoder.EncodeHeader(FMem, H, V, false);
  FStrm.WriteBuffer(FMem.Memory^, FMem.Position);
end;

{ TWCHTTP2BufResponseHeaderPusher }

constructor TWCHTTP2BufResponseHeaderPusher.Create(
  aHPackEncoder: TThreadSafeHPackEncoder; aBuffer: Pointer; aBufferSize,
  aBufGrowValue: Cardinal);
begin
  inherited Create(aHPackEncoder);
  FBuf := aBuffer;
  FCapacity:= aBufferSize;
  FBufGrowValue := aBufGrowValue;
  FSize := 0;
end;

procedure TWCHTTP2BufResponseHeaderPusher.PushHeader(const H, V: String);

procedure ExpandHeadersBuffer;
begin
  FCapacity := FCapacity + FBufGrowValue;
  FBuf := ReAllocMem(FBuf, FCapacity);
end;

begin
  FMem.Position:=0;
  FHPackEncoder.EncodeHeader(FMem, H, V, false);
  if FMem.Position + FSize > FCapacity then
    ExpandHeadersBuffer;
  Move(FMem.Memory, PByte(FBuf)[FSize], FMem.Position);
  Inc(FSize, FMem.Position);
end;

{ TWCHTTP2ResponseHeaderPusher }

constructor TWCHTTP2ResponseHeaderPusher.Create(
  aHPackEncoder: TThreadSafeHPackEncoder);
begin
  aHPackEncoder.IncReference;
  FHPackEncoder := aHPackEncoder;
  FMem := TMemoryStream.Create;
  FMem.SetSize(4128);
end;

destructor TWCHTTP2ResponseHeaderPusher.Destroy;
begin
  FHPackEncoder.DecReference;
  FMem.Free;
  inherited Destroy;
end;

procedure TWCHTTP2ResponseHeaderPusher.PushAll(R: TResponse);
var h1 : THeader;
    h2 : THTTP2Header;
    v  : String;
    i : integer;
begin
  FHPackEncoder.Lock;
  try
    PushHeader(HTTP2HeaderStatus, Inttostr(R.Code));
    //PushHeader(HTTP2HeaderVersion, HTTP2VersionId);
    h1 := hhUnknown;
    while h1 < High(THeader) do
    begin
      Inc(h1);
      if R.HeaderIsSet(h1) then
        PushHeader(LowerCase(HTTPHeaderNames[h1]), R.GetHeader(h1));
    end;
    h2 := hh2Status;
    while h2 < High(THTTP2Header) do
    begin
      inc(h2);
      v := R.GetCustomHeader(HTTP2AddHeaderNames[h2]);
      if Length(v) > 0 then
         PushHeader(HTTP2AddHeaderNames[h2], v);
    end;
    for i := 0 to R.Cookies.Count-1 do
      PushHeader(LowerCase(HTTP2AddHeaderNames[hh2SetCookie]),
                                                    R.Cookies[i].AsString);
  finally
    FHPackEncoder.UnLock;
  end;
end;

{ TWCHTTP2SerializeStream }

constructor TWCHTTP2SerializeStream.Create(aConn: TWCHTTP2Connection;
  aStrm: Cardinal; aFirstFrameType: Byte; aNextFramesType: Byte; aFlags,
  aFinalFlags: Byte);
begin
  Inherited Create;
  FStrmID := aStrm;
  FConn := aConn;
  FConn.IncReference;
  FFlags := aFlags;
  FFinalFlags := aFinalFlags;
  FFirstFrameType:= aFirstFrameType;
  FNextFramesType:= aNextFramesType;
  FCurFrame := nil;
  FRestFrameSz := 0;
  FChuncked := false;
  FFirstFramePushed := false;
end;

function TWCHTTP2SerializeStream.Write(const Buffer; Count: Longint): Longint;
var B, Src : Pointer;
    Sz, BSz, MaxSize : Longint;
begin
  Src := @Buffer;
  Result := Count;
  Sz := Count;
  MaxSize := FConn.ConnSettings[H2SET_MAX_FRAME_SIZE];
  if (Sz > MaxSize) and (FChuncked) then Exit(-1);
  while Sz > 0 do begin
    if Assigned(FCurFrame) and
       ((FRestFrameSz = 0) or
        (FChuncked and (FRestFrameSz < Sz))) then
    begin
      FConn.PushFrame(FCurFrame);
      FCurFrame := nil;
    end;

    if not Assigned(FCurFrame) then
    begin
      if Sz > MaxSize then Bsz := MaxSize else Bsz := Sz;
      B := GetMem(MaxSize);
      if FFirstFramePushed then
        FCurFrame := TWCHTTP2Frame.Create(FNextFramesType, FStrmID, FFlags, B, Bsz)
      else begin
        FCurFrame := TWCHTTP2Frame.Create(FFirstFrameType, FStrmID, FFlags, B, Bsz);
        FFirstFramePushed:= true;
      end;
      FRestFrameSz := MaxSize - Bsz;
    end else
    begin
      BSz := Sz;
      if BSz > FRestFrameSz then
      begin
         BSz := FRestFrameSz;
         FRestFrameSz := 0;
      end else
         Dec(FRestFrameSz, BSz);
      B := Pointer(FCurFrame.Payload + FCurFrame.Header.PayloadLength);
      Inc(FCurFrame.Header.PayloadLength, Bsz);
    end;
    Move(Src^, B^, BSz);
    Inc(Src, BSz);
    Dec(Sz, BSz);
  end;
end;

destructor TWCHTTP2SerializeStream.Destroy;
begin
  if assigned(FCurFrame) then begin
    FCurFrame.Header.FrameFlag := FFinalFlags;
    FConn.PushFrame(FCurFrame);
  end;
  FConn.DecReference;
  inherited Destroy;
end;

{ TWCHTTP2Response }

constructor TWCHTTP2Response.Create(aConnection: TWCHTTP2Connection;
  aStream: TWCHTTPStream);
begin
  inherited Create(aConnection, aStream);
  FCurHeadersBlock:= nil;
end;

destructor TWCHTTP2Response.Destroy;
begin
  if assigned(FCurHeadersBlock) then FreeMemAndNil(FCurHeadersBlock);
  inherited Destroy;
end;

procedure TWCHTTP2Response.CopyFromHTTP1Response(R: TResponse);
var pusher : TWCHTTP2BufResponseHeaderPusher;
    Capacity : Cardinal;
begin
  Capacity := FConnection.ConnSettings[H2SET_MAX_FRAME_SIZE];
  FCurHeadersBlock := ReAllocMem(FCurHeadersBlock, Capacity);
  FHeadersBlockSize:=0;
  FConnection.InitHPack;
  pusher := TWCHTTP2BufResponseHeaderPusher.Create(FConnection.CurHPackEncoder,
                                                   FCurHeadersBlock,
                                                   Capacity,
                                                   Capacity);
  try
    pusher.pushall(R);
    FCurHeadersBlock := pusher.Buffer;
    FHeadersBlockSize:= pusher.Size;
  finally
    pusher.Free;
  end;
end;

procedure TWCHTTP2Response.Close;
//var er : PCardinal;
begin
  FConnection.PushFrame(H2FT_DATA, FStream.ID, H2FL_END_STREAM, nil, 0);
{  er := GetMem(H2P_RST_STREAM_FRAME_SIZE);
  er^ := NTOBE(H2E_NO_ERROR);
  FConnection.PushFrame(H2FT_RST_STREAM, FStream.ID, 0, er, H2P_RST_STREAM_FRAME_SIZE); }
end;

procedure TWCHTTP2Response.SerializeResponse;
begin
  SerializeHeaders(FDataBlockSize = 0);
  if FDataBlockSize > 0 then
     SerializeData(true);
end;

procedure TWCHTTP2Response.SerializeHeaders(closeStrm : Boolean);
var
  sc : TWCHTTP2SerializeStream;
begin
  if Assigned(FCurHeadersBlock) then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream.Id,
                                         H2FT_HEADERS,
                                         H2FT_CONTINUATION,
                                         0,
                                         H2FL_END_HEADERS or
                                         (Ord(closeStrm) * H2FL_END_STREAM));
    try
      sc.WriteBuffer(FCurHeadersBlock^, FHeadersBlockSize);
    finally
      sc.Free;
    end;
    FreeMemAndNil(FCurHeadersBlock);
    FHeadersBlockSize:=0;
  end;
  // after headers serialized close stream
  if closeStrm then
    FStream.FStreamState := h2ssCLOSED;
end;

procedure TWCHTTP2Response.SerializeData(closeStrm : Boolean);
var
  sc : TWCHTTP2SerializeStream;
begin
  // serialize in group of data chunck with max_frame_size
  // then remove fdatablock
  if assigned(FCurDataBlock) and (FDataBlockSize > 0) then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream.Id,
                                         H2FT_DATA,
                                         H2FT_DATA,
                                         0,
                                         (Ord(closeStrm) * H2FL_END_STREAM));
    try
      sc.WriteBuffer(FCurDataBlock^, FDataBlockSize);
    finally
      sc.Free;
    end;
  end;
  FDataBlockSize:=0;
  // after data serialized close stream
  if closeStrm then
    FStream.FStreamState := h2ssCLOSED;
end;

procedure TWCHTTP2Response.SerializeResponseHeaders(R: TResponse;
  closeStrm: Boolean);
var sc : TWCHTTP2SerializeStream;
    pusher : TWCHTTP2StrmResponseHeaderPusher;
begin
  if Assigned(FCurHeadersBlock) then FreeMemAndNil(FCurHeadersBlock);
  FHeadersBlockSize:=0;

  sc := TWCHTTP2SerializeStream.Create(FConnection,
                                       FStream.ID,
                                       H2FT_HEADERS,
                                       H2FT_CONTINUATION,
                                       0,
                                       H2FL_END_HEADERS or
                                       (Ord(closeStrm) * H2FL_END_STREAM));
  FConnection.InitHPack;
  pusher := TWCHTTP2StrmResponseHeaderPusher.Create(FConnection.CurHPackEncoder,
                                                    sc);
  try
    sc.Chuncked := true;
    pusher.PushAll(R);
  finally
    sc.Free;
    pusher.Free;
  end;
  // after data serialized close stream
  if closeStrm then
    FStream.FStreamState := h2ssCLOSED;
end;

procedure TWCHTTP2Response.SerializeResponseData(R: TResponse;
  closeStrm: Boolean);
var sc : TWCHTTP2SerializeStream;
begin
  if Assigned(FCurDataBlock) then FreeMemAndNil(FCurDataBlock);
  FDataBlockSize:=0;

  if R.ContentLength > 0 then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream.ID,
                                             H2FT_DATA,
                                             H2FT_DATA,
                                             0,
                                             (Ord(closeStrm) * H2FL_END_STREAM));
    try
      if assigned(R.ContentStream) then
      begin
        sc.CopyFrom(R.ContentStream, R.ContentStream.Size);
      end else
      begin
        R.Contents.SaveToStream(sc);
      end;
    finally
      sc.Free;
    end;
  end;
  // after data serialized close stream
  if closeStrm then
    FStream.FStreamState := h2ssCLOSED;
end;

{ TWCHTTP2Request }

function TWCHTTP2Request.GetResponse: TWCHTTP2Response;
begin
  if Assigned(FResponse) then Exit(FResponse);
  FResponse := TWCHTTP2Response.Create(FConnection, FStream);
  Result := FResponse;
end;

constructor TWCHTTP2Request.Create(aConnection : TWCHTTP2Connection;
                                   aStream : TWCHTTPStream);
begin
  inherited Create(aConnection, aStream);
  FComplete := false;
  FResponse := nil;
  FHeaders  := THPackHeaderTextList.Create;
end;

destructor TWCHTTP2Request.Destroy;
begin
  if assigned(FResponse) then FreeAndNil(FResponse);
  FHeaders.Free;
  inherited Destroy;
end;

procedure TWCHTTP2Request.CopyHeaders(aHPackDecoder: TThreadSafeHPackDecoder);
var i : integer;
    p : PHPackHeaderTextItem;
begin
  aHPackDecoder.IncReference;
  aHPackDecoder.Lock;
  try
    FHeaders.Clear;
    for i := 0 to aHPackDecoder.DecodedHeaders.Count-1 do
    begin
      P := aHPackDecoder.DecodedHeaders[i];
      FHeaders.Add(P^.HeaderName, P^.HeaderValue, P^.IsSensitive);
    end;
    aHPackDecoder.DecodedHeaders.Clear;
  finally
    aHPackDecoder.UnLock;
    aHPackDecoder.DecReference;
  end;
end;

procedure TWCHTTP2Request.CopyToHTTP1Request(ARequest: TRequest);
var
  i, j : integer;
  h : PHTTPHeader;
  v : PHPackHeaderTextItem;
  S : String;
begin
  if Complete then
  begin
    try
      for i := 0 to FHeaders.Count-1 do
      begin
        v := FHeaders[i];
        h := GetHTTPHeaderType(v^.HeaderName);
        if assigned(h) then
        begin
          if h^.h2 <> hh2Unknown then
          begin
            case h^.h2 of
              hh2Method : ARequest.Method := v^.HeaderValue;
              hh2Path   : begin
                ARequest.URL:= v^.HeaderValue;
                S:=ARequest.URL;
                j:=Pos('?',S);
                if (j>0) then
                  S:=Copy(S,1,j-1);
                If (Length(S)>1) and (S[1]<>'/') then
                  S:='/'+S
                else if S='/' then
                  S:='';
                ARequest.PathInfo:=S;
              end;
              hh2Authority, hh2Scheme, hh2Status : ;
              hh2Cookie : begin
                ARequest.CookieFields.Add(v^.HeaderValue);
              end
            else
              ARequest.SetCustomHeader(HTTP2AddHeaderNames[h^.h2], v^.HeaderValue);
            end;
          end else
          if h^.h1 <> hhUnknown then
          begin
            ARequest.SetHeader(h^.h1, v^.HeaderValue);
          end else
            ARequest.SetCustomHeader(v^.HeaderName, v^.HeaderValue);
        end;
      end;
      if FDataBlockSize > 0 then begin
        SetLength(S, FDataBlockSize);
        Move(FCurDataBlock^, S[1], FDataBlockSize);
        ARequest.Content:=S;
      end;
    finally
      //
    end;
  end;
end;
  
{ TWCHTTP2Frame }

constructor TWCHTTP2Frame.Create(aFrameType : Byte; 
                     StrID : Cardinal; 
                     aFrameFlags : Byte; 
                     aData : Pointer; 
                     aDataSize : Cardinal);
begin
  Header := TWCHTTP2FrameHeader.Create;
  Header.FrameType := aFrameType;
  Header.FrameFlag := aFrameFlags;
  Header.PayloadLength := aDataSize;
  Header.StreamID := StrID;  
  Payload := aData;
end;
                     
destructor TWCHTTP2Frame.Destroy;
begin
  Header.Free;
  if assigned(Payload) then Freemem(Payload);
  inherited Destroy;
end;
                     
procedure TWCHTTP2Frame.SaveToStream(Str : TStream);
begin
  Header.SaveToStream(Str);
  if Header.PayloadLength > 0 then 
     Str.WriteBuffer(Payload^, Header.PayloadLength);
end;

function TWCHTTP2Frame.Size: Int64;
begin
  Result := H2P_FRAME_HEADER_SIZE + Header.PayloadLength;
end;

{ TWCHTTP2FrameHeader }

procedure TWCHTTP2FrameHeader.LoadFromStream(Str: TStream);
var FrameHeader : Array [0..H2P_FRAME_HEADER_SIZE-1] of Byte;
begin
  // read header
  Str.Read(FrameHeader, H2P_FRAME_HEADER_SIZE);
  // format frame
  PayloadLength := (FrameHeader[0] shl 16) or
                   (FrameHeader[1] shl 8) or
                    FrameHeader[2];
  FrameType:= FrameHeader[3];
  FrameFlag:= FrameHeader[4];
  StreamID := BEtoN(PCardinal(@(FrameHeader[5]))^) and H2P_STREAM_ID_MASK;
end;

procedure TWCHTTP2FrameHeader.SaveToStream(Str: TStream);
var FrameHeader : Array [0..H2P_FRAME_HEADER_SIZE-1] of Byte;
    PL24 : Cardinal;
begin
  // format frame
  // 0x00a2b3c4 << 8 --> 0xa2b3c400 (0x00c4b3a2 in LE)
  // NtoBE(0x00c4b3a2) --> 0xa2b3c400
  PL24 := PayloadLength shl 8;
  // write first most significant 3 bytes
  Move(NtoBE(PL24), FrameHeader[0], H2P_PAYLOAD_LEN_SIZE);
  FrameHeader[3] := FrameType;
  FrameHeader[4] := FrameFlag;
  Move(NtoBE(StreamID), FrameHeader[5], H2P_STREAM_ID_SIZE);

  // write header
  Str.Write(FrameHeader, H2P_FRAME_HEADER_SIZE);
end;

{ TWCHTTP2Block }

procedure TWCHTTP2Block.PushData(Data: Pointer; sz: Cardinal);
begin
  if sz = 0 then Exit;
  
  if not Assigned(FCurDataBlock) then
     FCurDataBlock:=GetMem(Sz) else
     FCurDataBlock:=ReAllocMem(FCurDataBlock, sz + FDataBlockSize);

  Move(Data^, PByte(FCurDataBlock)[FDataBlockSize], sz);

  Inc(FDataBlockSize, sz);
end;

procedure TWCHTTP2Block.PushData(Strm: TStream; startAt : Int64);
var sz : Int64;
begin
  Strm.Position:= startAt;
  sz := Strm.Size - startAt;
  if Sz > 0 then
  begin
    if not Assigned(FCurDataBlock) then
       FCurDataBlock:=GetMem(Sz) else
       FCurDataBlock:=ReAllocMem(FCurDataBlock, sz + FDataBlockSize);

    Strm.Read(PByte(FCurDataBlock)[FDataBlockSize], sz);

    Inc(FDataBlockSize, sz);
  end;
end;

procedure TWCHTTP2Block.PushData(Strings: TStrings);
var ToSend : String;
    L : LongInt;
begin
  ToSend := Strings.Text;
  L := Length(ToSend);
  PushData(Pointer(@(ToSend[1])), L);
end;

constructor TWCHTTP2Block.Create(aConnection: TWCHTTP2Connection;
  aStream: TWCHTTPStream);
begin
  FCurDataBlock := nil;
  FDataBlockSize := 0;
  FConnection := aConnection;
  if Assigned(FConnection) then FConnection.IncReference;
  FStream := aStream;
  if Assigned(FStream) then FStream.IncReference;
end;

destructor TWCHTTP2Block.Destroy;
begin
  if Assigned(FCurDataBlock) then FreeMem(FCurDataBlock);
  if Assigned(FStream) then  FStream.DecReference;
  if Assigned(FConnection) then  FConnection.DecReference;
  inherited Destroy;
end;

{ TWCHTTP2Connections }

{$ifdef SOCKET_EPOLL_MODE}

procedure TWCHTTP2Connections.Inflate;
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

procedure TWCHTTP2Connections.CallActionEpoll(aCount: Integer);
var
  i, MasterChanges, Changes, ReadChanges, m, err: Integer;
  Temp, TempRead: TWCHTTPSocketReference;
  MasterEvents: array[0..1] of TEpollEvent;
begin
  Changes := 0;
  ReadChanges := 0;

  repeat
    MasterChanges := epoll_wait(FEpollMasterFD, @MasterEvents[0], 2, FTimeout);
    err := SocketError;
  until (MasterChanges >= 0) or (err <> ESysEINTR);

  if MasterChanges <= 0 then
  begin
    if (MasterChanges < 0) and (err <> 0) and (err <> ESysEINTR) then
      raise ESocketError.CreateFmt('Error on epoll %d', [err]);
  end else
  begin
    FEpollLocker.Lock;
    try
      err := 0;
      for i := 0 to MasterChanges - 1 do begin
        if MasterEvents[i].Data.fd = FEpollFD then
        begin
          repeat
            Changes := epoll_wait(FEpollFD, @FEvents[0], aCount, 0);
            err := SocketError;
          until (Changes >= 0) or (err <> ESysEINTR);
        end
        else
          repeat
            ReadChanges := epoll_wait(FEpollReadFD, @FEventsRead[0], aCount, 0);
            err := SocketError;
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
            Temp := TWCHTTPSocketReference(FEvents[i].data.ptr);

            if  ((FEvents[i].events and EPOLLOUT) = EPOLLOUT) then
                Temp.SetCanSend;

            if  ((FEvents[i].events and EPOLLERR) = EPOLLERR) then
                Temp.PushError;// 'Handle error' + Inttostr(SocketError));
          end; // writes

          if i < ReadChanges then begin
            TempRead := TWCHTTPSocketReference(FEventsRead[i].data.ptr);

            if  ((FEventsRead[i].events and (EPOLLIN or EPOLLHUP or EPOLLPRI)) > 0) then
                TempRead.SetCanRead;
          end; // reads
        end;
      end;
    finally
      FEpollLocker.UnLock;
    end;
  end;
end;

procedure TWCHTTP2Connections.AddSocketEpoll(ASocket: TWCHTTPSocketReference);
var
  lEvent: TEpollEvent;
begin
  ASocket.IncReference;
  lEvent.events := {EPOLLET or }EPOLLOUT or EPOLLERR;
  lEvent.data.ptr := ASocket;
  if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll', [SocketError]);
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll', [SocketError]);
  if Count > High(FEvents) then
    Inflate;
end;

procedure TWCHTTP2Connections.RemoveSocketEpoll(ASocket: TWCHTTPSocketReference);
begin
  try
    epoll_ctl(FEpollFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
    epoll_ctl(FEpollReadFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
  finally
    ASocket.DecReference;
  end;
end;

{$endif}

function TWCHTTP2Connections.IsConnDead(aConn: TObject; data: pointer
  ): Boolean;
begin
  Result := (TWCHTTP2Connection(aConn).LifeTime > PInteger(data)^) or
            (TWCHTTP2Connection(aConn).ConnectionState <> wcCONNECTED);
end;

procedure TWCHTTP2Connections.AfterConnExtracted(aObj: TObject);
begin
  {$ifdef SOCKET_EPOLL_MODE}
  RemoveSocketEpoll(TWCHTTP2Connection(aObj).FSocketRef);
  {$endif}
  TWCHTTP2Connection(aObj).DecReference;
end;

constructor TWCHTTP2Connections.Create;
{$ifdef SOCKET_EPOLL_MODE}
var lEvent : TEpollEvent;
{$endif}
begin
  inherited Create;
  FMaintainStamp := GetTickCount64;
  FLastUsedConnection := nil;
  {$ifdef SOCKET_EPOLL_MODE}
  FEpollLocker := TNetCustomLockedObject.Create;
  Inflate;
  FTimeout := 1;
  FEpollFD := epoll_create(BASE_SIZE);
  FEpollReadFD := epoll_create(BASE_SIZE);
  FEpollMasterFD := epoll_create(2);
  if (FEPollFD < 0) or (FEpollReadFD < 0) or (FEpollMasterFD < 0) then
    raise ESocketError.CreateFmt('Unable to create epoll: %d', [fpgeterrno]);
  lEvent.events := EPOLLIN or EPOLLOUT or EPOLLPRI or EPOLLERR or EPOLLHUP;// or EPOLLET;
  lEvent.data.fd := FEpollFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
  lEvent.data.fd := FEpollReadFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollReadFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
  {$endif}
end;

destructor TWCHTTP2Connections.Destroy;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCHTTP2Connection(P.Value).DecReference;
      P := P.Next;
    end;
    ExtractAll;
  finally
    UnLock;
  end;
  {$ifdef SOCKET_EPOLL_MODE}
  fpClose(FEpollFD);
  FEpollLocker.Free;
  {$endif}
  inherited Destroy;
end;

procedure TWCHTTP2Connections.AddConnection(FConn: TWCHTTP2Connection);
begin
  Push_back(FConn);
  {$ifdef socket_epoll_mode}
  AddSocketEpoll(FConn.FSocketRef);
  {$endif}
end;

function TWCHTTP2Connections.GetByHandle(aSocket: Cardinal): TWCHTTP2Connection;
var P :TIteratorObject;
begin
  Result := nil;
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if (TWCHTTP2Connection(P.Value).Socket = aSocket) and
         (TWCHTTP2Connection(P.Value).ConnectionState = wcCONNECTED) then
      begin
        Result := TWCHTTP2Connection(P.Value);
        Result.IncReference;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCHTTP2Connections.RemoveDeadConnections(MaxLifeTime: Cardinal);
begin
  ExtractObjectsByCriteria(@IsConnDead, @AfterConnExtracted, @MaxLifeTime);
end;

procedure TWCHTTP2Connections.Idle;
var TS : QWord;
    P :TIteratorObject;
    i : integer;
begin
  TS := GetTickCount64;
  if (TS - FMaintainStamp) div 1000 > 60 then
  begin
    RemoveDeadConnections(120);
    FMaintainStamp := TS;
  end;
  {$ifdef SOCKET_EPOLL_MODE}
  CallActionEpoll(Count);
  {$endif}
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
          break;
        end;
      end;
    end;
    i := 0;
    while assigned(P) do
    begin
      if (TWCHTTP2Connection(P.Value).ConnectionState = wcCONNECTED) then
      begin
        if TWCHTTP2Connection(P.Value).TryToIdleStep(TS) then
        begin
          FLastUsedConnection := P.Next;
          inc(i);
          if i > 2 then break;
        end;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

{ TWCHTTP2Connection }

procedure TWCHTTP2Connection.SetConnectionState(CSt: TWCConnectionState);
begin
  if ConnectionState <> CSt then
  begin
    FConnectionState.Value := Cst;
    if Cst = wcDROPPED then
      FSocketRef.PushError;
  end;
end;

function TWCHTTP2Connection.GetConnectionState: TWCConnectionState;
begin
  Result := FConnectionState.Value;
end;

function TWCHTTP2Connection.ReadyToReadWrite(const TS: QWord): Boolean;
begin
  Result := ReadyToRead(TS) or ReadyToWrite(TS);
end;

function TWCHTTP2Connection.ReadyToRead(const TS: QWord): Boolean;
begin
  Result := (TS - FReadStamp.Value) > 10;
end;

function TWCHTTP2Connection.ReadyToWrite(const TS: QWord): Boolean;
begin
  Result := ((TS - FWriteStamp.Value) > 10) and (not FDataSending.Value);
end;

function TWCHTTP2Connection.AddNewStream(aStreamID : Cardinal): TWCHTTPStream;
begin
  Result := TWCHTTPStream.Create(Self, aStreamID);
  FStreams.Push_back(Result);
  Refresh;
end;

function TWCHTTP2Connection.GetConnSetting(id : Word): Cardinal;
begin
  Result := FConSettings[id];
end;

function TWCHTTP2Connection.GetLifeTime: Cardinal;
begin
  Result := (GetTickCount64 - FTimeStamp.Value) div 1000;
end;

procedure TWCHTTP2Connection.Refresh;
begin
  FTimeStamp.Value := GetTickCount64;
end;

constructor TWCHTTP2Connection.Create(aGarbageCollector: TNetReferenceList;
  aSocket: TWCHTTPSocketReference; aOpenningMode: THTTP2OpenMode;
  aSocketConsume: THttp2SocketConsume;
  aSendData : THttp2SendData);
var i : integer;
    CSet : PHTTP2SettingsPayload;
begin
  inherited Create;
  FGarbageCollector := aGarbageCollector;
  FStreams := TWCHTTPStreams.Create;
  FSocketRef := aSocket;
  FSocketRef.IncReference;
  FSocket:= FSocketRef.Socket.Handle;
  FTimeStamp := TThreadQWord.Create(GetTickCount64);
  FReadTailSize := 0;
  FWriteTailSize:= 0;
  FLastStreamID := 0;
  FSocketConsume:= aSocketConsume;
  FSendData := aSendData;
  FDataSending := TThreadBoolean.Create(false);
  FReadStamp := TThreadQWord.Create(GetTickCount64);
  FWriteStamp:= TThreadQWord.Create(GetTickCount64);
  FConSettings := TThreadSafeConnSettings.Create;
  for i := 1 to HTTP2_SETTINGS_MAX do
    FConSettings[i] := HTTP2_SET_INITIAL_VALUES[i];
  if assigned(HTTP2ServerSettings) then
  for i := 1 to HTTP2ServerSettingsSize div H2P_SETTINGS_BLOCK_SIZE do
  begin
    FConSettings[PHTTP2SettingsPayload(HTTP2ServerSettings)^[i-1].Identifier] :=
        PHTTP2SettingsPayload(HTTP2ServerSettings)^[i-1].Value
  end;
  FReadBuffer := TThreadPointer.Create(FConSettings[H2SET_INITIAL_WINDOW_SIZE]);
  FWriteBuffer := TThreadPointer.Create(FConSettings[H2SET_INITIAL_WINDOW_SIZE]);
  FWriteBufferSize:= FConSettings[H2SET_INITIAL_WINDOW_SIZE];
  FReadBufferSize:= FConSettings[H2SET_INITIAL_WINDOW_SIZE];
  FFramesToSend := TThreadSafeFastSeq.Create;
  FConnectionState := TThreadSafeConnectionState.Create(wcCONNECTED);
  // send initial settings frame
  if aOpenningMode in [h2oUpgradeToH2C, h2oUpgradeToH2] then
    PushFrame(TWCHTTP2UpgradeResponseFrame.Create(aOpenningMode));
  Cset := nil;
  if HTTP2ServerSettingsSize > 0 then
  begin
    CSet := GetMem(HTTP2ServerSettingsSize);
    Move(HTTP2ServerSettings^, CSet^, HTTP2ServerSettingsSize);
  end;
  PushFrame(TWCHTTP2Frame.Create(H2FT_SETTINGS, 0, 0,
                                 CSet,
                                 HTTP2ServerSettingsSize));
end;

procedure TWCHTTP2Connection.ConsumeNextFrame(Mem: TBufferedStream);
var
  ReadLoc, MemSz : Int64;

function ReadMore(WriteAt, AddSz : Int64) : Int64;
var R : Integer;
   Src : Pointer;
begin
  R := AddSz;
  Result := WriteAt;
  While (AddSz > 0) and (R > 0) do
  begin
    if ReadLoc < MemSz then
    begin
      R := MemSz - ReadLoc;
      Src := Pointer(Mem.Memory + Mem.Position + ReadLoc);
      if AddSz < R then R := AddSz;
      Move(Src^, PByte(FReadBuffer.Value)[Result], R);
    end
    else
    begin
      R:=FSocketRef.Read(PByte(FReadBuffer.Value)[Result], AddSz);
      If R < 0 then
        break;
    end;
    if R > 0 then begin
      Dec(AddSz, R);
      Inc(ReadLoc, R);
      Inc(Result, R);
    end else break;
  end;
end;

var
  Sz, fallbackpos, L, R : Int64;
  err : Byte;
  Buffer : Pointer;
  FrameHeader : TWCHTTP2FrameHeader;
  S : TBufferedStream;
  Str, RemoteStr : TWCHTTPStream;

function TruncReadBuffer : Int64;
begin
  Result := S.Size - S.Position;
  if Result > 0 then
     Move(PByte(FReadBuffer)[S.Position], FReadBuffer.Value^, Result);
end;

function ProceedHeadersPayload(Strm : TWCHTTPStream; aSz : Cardinal) : Byte;
var readbuf : TBufferedStream;
    aDecoder : TThreadSafeHPackDecoder;
begin
  Result := H2E_NO_ERROR;
  //hpack here
  InitHPack;
  aDecoder := CurHPackDecoder;
  aDecoder.IncReference;
  readbuf := TBufferedStream.Create;
  try
    readbuf.SetPointer(Pointer(S.Memory + S.Position),
                       aSz);
    try
      aDecoder.Decode(readbuf);
      if (FrameHeader.FrameFlag and H2FL_END_HEADERS) > 0 then
      begin
        if aDecoder.EndHeaderBlockTruncated then
           Result := H2E_COMPRESSION_ERROR;
        Strm.FinishHeaders(aDecoder);
      end;
    except
      on e : Exception do
        Result := H2E_COMPRESSION_ERROR;
    end;
  finally
    readbuf.Free;
    aDecoder.DecReference;
  end;
end;

procedure CheckStreamAfterState(Strm : TWCHTTPStream);
begin
  if (FrameHeader.FrameFlag and H2FL_END_STREAM) > 0 then
  begin
    if Strm.FStreamState = h2ssOPEN then
    begin
       Strm.FStreamState := h2ssHLFCLOSEDRem;
       Strm.PushRequest;
    end;
  end;
end;

var B : Byte;
    DataSize : Integer;
    RemoteID : Cardinal;
    SetID : Word;
    SetValue : Cardinal;
begin
  Str := nil; RemoteStr := nil;
  if assigned(Mem) then begin
    MemSz := Mem.Size - Mem.Position;
    Sz := MemSz;
  end else begin
    Sz := WC_INITIAL_READ_BUFFER_SIZE;
  end;

  FReadBuffer.Lock;
  try
    FrameHeader := TWCHTTP2FrameHeader.Create;
    S := TBufferedStream.Create;
    try
      if Sz > (FReadBufferSize - FReadTailSize) then
         Sz := (FReadBufferSize - FReadTailSize);
      if Sz = 0 then
      begin
        err := H2E_READ_BUFFER_OVERFLOW;
        exit;
      end;
      ReadLoc := 0;
      Sz := ReadMore(FReadTailSize, Sz);
      if ReadLoc = 0 then begin
        err := H2E_INTERNAL_ERROR;
        Exit;
      end;
      S.SetPointer(FReadBuffer.Value, Sz);

      err := H2E_NO_ERROR;
      while true do
      begin
        if (S.Size - S.Position) < H2P_FRAME_HEADER_SIZE then
        begin
          L := TruncReadBuffer;
          Sz := ReadMore(L, Sz);
          if Sz < H2P_FRAME_HEADER_SIZE then begin
            fallbackpos := 0;
            err := H2E_PARSE_ERROR;
            break;
          end;
          S.SetPointer(FReadBuffer.Value, Sz);
        end;
        fallbackpos := S.Position;
        // read header
        FrameHeader.LoadFromStream(S);
        // find stream
        if assigned(Str) then Str.DecReference;
        if assigned(RemoteStr) then RemoteStr.DecReference;
        RemoteStr := nil;
        if FrameHeader.StreamID > 0 then
        begin
          if FrameHeader.StreamID <= FLastStreamID then
          begin
            Str := FStreams.GetByID(FrameHeader.StreamID);
            if not Assigned(Str) then
            begin
              err := H2E_FLOW_CONTROL_ERROR;
              break;
            end;
          end else begin
            FLastStreamID := FrameHeader.StreamID;
            if FrameHeader.FrameType in [H2FT_DATA, H2FT_HEADERS,
                                         H2FT_CONTINUATION] then
               FStreams.CloseOldIdleStreams(FLastStreamID);
            Str := AddNewStream(FLastStreamID);
            Str.IncReference;
          end;
          if Assigned(Str) then Str.UpdateState(FrameHeader);
        end else
          Str := nil;

        R := FConSettings[H2SET_MAX_FRAME_SIZE];

        if (FrameHeader.PayloadLength + H2P_FRAME_HEADER_SIZE) > R then
        begin
          err := H2E_FRAME_SIZE_ERROR;
          break;
        end;

        if (FrameHeader.PayloadLength > (S.Size - S.Position)) then
        begin
          // try to load rest octets for frame
          S.Position := fallbackpos; // truncate to the begining of the frame
          L := TruncReadBuffer;
          if L <> H2P_FRAME_HEADER_SIZE then
          begin
            err := H2E_INTERNAL_ERROR;
            break;
          end;
          Sz := ReadMore(H2P_FRAME_HEADER_SIZE, R);
          if (Sz - H2P_FRAME_HEADER_SIZE) < FrameHeader.PayloadLength then
          begin
            fallbackpos := 0;
            err := H2E_PARSE_ERROR;
            break;
          end;
          S.SetPointer(FReadBuffer.Value, Sz);
          S.Position:= H2P_FRAME_HEADER_SIZE; // to payload begining
        end;
        if err = H2E_NO_ERROR then
        begin
          if Assigned(Str) and
             Str.FWaitingForContinueFrame and
             (FrameHeader.FrameType <> H2FT_CONTINUATION) then
          begin
            err := H2E_PROTOCOL_ERROR;
            break;
          end;
          if (not Assigned(Str)) and
             (FrameHeader.FrameType in [H2FT_DATA,
                                        H2FT_CONTINUATION,
                                        H2FT_HEADERS,
                                        H2FT_PRIORITY,
                                        H2FT_RST_STREAM,
                                        H2FT_PUSH_PROMISE]) then
          begin
            err := H2E_PROTOCOL_ERROR; // sec.6.1-6.4,6.6
            break;
          end;
          if Assigned(Str) and
             (FrameHeader.FrameType in [H2FT_PING, H2FT_SETTINGS]) then
          begin
            err := H2E_PROTOCOL_ERROR; // sec.6.5, 6.7
            break;
          end;
          // payload fully loaded
          case FrameHeader.FrameType of
            H2FT_DATA : begin
              if not (Str.StreamState in [h2ssOPEN, h2ssHLFCLOSEDLoc]) then
              begin
                err := H2E_STREAM_CLOSED;
                break;
              end;
              DataSize := FrameHeader.PayloadLength;
              if FrameHeader.FrameFlag and H2FL_PADDED > 0 then
              begin
                B := 0;
                S.Read(B, H2P_PADDING_OCTET_SIZE);
                DataSize := DataSize - B;
              end;
              if DataSize < 0 then begin
                err := H2E_INTERNAL_ERROR;
                break;
              end;
              //
              Str.PushData(Pointer(S.Memory + S.Position), DataSize);
              S.Position := S.Position + FrameHeader.PayloadLength;
              CheckStreamAfterState(Str);
            end;
            H2FT_HEADERS : begin
              if not (Str.StreamState in [h2ssIDLE,
                                          h2ssRESERVEDLoc,
                                          h2ssOPEN,
                                          h2ssHLFCLOSEDRem]) then
              begin
                err := H2E_STREAM_CLOSED;
                break;
              end;
              DataSize := FrameHeader.PayloadLength;
              if FrameHeader.FrameFlag and H2FL_PADDED > 0 then
              begin
                B := 0;
                S.Read(B, H2P_PADDING_OCTET_SIZE);
                DataSize := DataSize - B;
              end;
              if FrameHeader.FrameFlag and H2FL_PRIORITY > 0 then
              begin
                B := 0;
                S.Read(Str.FParentStream, H2P_STREAM_ID_SIZE);
                Str.FParentStream := BETON(Str.FParentStream) and H2P_STREAM_ID_MASK;
                S.Read(Str.FPriority, H2P_PRIORITY_WEIGHT_SIZE);
                Str.ResetRecursivePriority;
                DataSize := DataSize - H2P_PRIORITY_FRAME_SIZE;
              end;
              if DataSize < 0 then begin
                err := H2E_INTERNAL_ERROR;
                break;
              end;
              err := ProceedHeadersPayload(Str, DataSize);
              if err <> H2E_NO_ERROR then break;
              Str.WaitingForContinueFrame := (FrameHeader.FrameFlag and
                                              H2FL_END_HEADERS) = 0;
              // END_STREAM react here
              CheckStreamAfterState(Str);
            end;
            H2FT_PUSH_PROMISE : begin
              if not (Str.StreamState in [h2ssOPEN,
                                          h2ssHLFCLOSEDLoc]) then
              begin
                err := H2E_STREAM_CLOSED;
                break;
              end;
              DataSize := FrameHeader.PayloadLength;
              if FrameHeader.FrameFlag and H2FL_PADDED > 0 then
              begin
                B := 0;
                S.Read(B, H2P_PADDING_OCTET_SIZE);
                DataSize := DataSize - B;
              end;
              if DataSize < H2P_STREAM_ID_SIZE then begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              RemoteID := 0;
              S.Read(RemoteID, H2P_STREAM_ID_SIZE);
              RemoteID := BETON(RemoteID);
              DataSize := DataSize - H2P_STREAM_ID_SIZE;
              if RemoteID = 0 then
              begin
                err := H2E_PROTOCOL_ERROR;
                break;
              end;
              RemoteStr := FStreams.GetByID(RemoteID);
              if assigned(RemoteStr) then
              begin
                if not (RemoteStr.StreamState = h2ssIDLE) then
                begin
                  err := H2E_PROTOCOL_ERROR;
                  break;
                end;
              end else
              if RemoteID <= FLastStreamID then
              begin
                err := H2E_FLOW_CONTROL_ERROR;
                break;
              end else begin
                FLastStreamID := RemoteID;
                RemoteStr := AddNewStream(FLastStreamID);
                RemoteStr.IncReference;
                RemoteStr.FStreamState:=h2ssRESERVEDRem;
              end;
              err := ProceedHeadersPayload(RemoteStr, DataSize);
              if err <> H2E_NO_ERROR then break;
              Str.WaitingForContinueFrame := (FrameHeader.FrameFlag and
                                              H2FL_END_HEADERS = 0);
              RemoteStr.WaitingForContinueFrame := Str.WaitingForContinueFrame;
              Str.FWaitingRemoteStream := RemoteID;
            end;
            H2FT_CONTINUATION : begin
                if not Str.FWaitingForContinueFrame then
                begin
                  err := H2E_PROTOCOL_ERROR;
                  break;
                end;
                if Str.FWaitingRemoteStream <> Str.FID then
                begin
                  RemoteStr := FStreams.GetByID(Str.FWaitingRemoteStream);
                  if not assigned(RemoteStr) then
                  begin
                    err := H2E_STREAM_CLOSED;
                    break;
                  end;
                  if not RemoteStr.FWaitingForContinueFrame then
                  begin
                    err := H2E_INTERNAL_ERROR;
                    break;
                  end;
                  err := ProceedHeadersPayload(RemoteStr, FrameHeader.PayloadLength);
                  if err <> H2E_NO_ERROR then break;
                end else
                begin
                  err := ProceedHeadersPayload(Str, FrameHeader.PayloadLength);
                  if err <> H2E_NO_ERROR then break;
                end;
                Str.WaitingForContinueFrame := (FrameHeader.FrameFlag and
                                                H2FL_END_HEADERS = 0);
                if assigned(RemoteStr) then begin
                  RemoteStr.WaitingForContinueFrame := Str.WaitingForContinueFrame;
                  if not Str.FWaitingForContinueFrame then
                    Str.FWaitingRemoteStream := Str.FID;
                  CheckStreamAfterState(RemoteStr);
                end else
                  CheckStreamAfterState(Str);
              end;
            H2FT_PRIORITY : begin
              if FrameHeader.PayloadLength <> H2P_PRIORITY_FRAME_SIZE then begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              S.Read(Str.FParentStream, H2P_STREAM_ID_SIZE);
              Str.FParentStream := BETON(Str.FParentStream) and H2P_STREAM_ID_MASK;
              S.Read(Str.FPriority, H2P_PRIORITY_WEIGHT_SIZE);
              Str.ResetRecursivePriority;
            end;
            H2FT_RST_STREAM : begin
              if not Assigned(Str) then
              begin
                err := H2E_PROTOCOL_ERROR;
                break;
              end;
              if FrameHeader.PayloadLength <> H2P_RST_STREAM_FRAME_SIZE then begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              S.Read(Str.FFinishedCode, H2P_ERROR_CODE_SIZE);
              Str.FFinishedCode := BETON(Str.FFinishedCode);
              Str.FStreamState := h2ssCLOSED;
            end;
            H2FT_SETTINGS : begin
              if (FrameHeader.FrameFlag and H2FL_ACK) > 0 then
              begin
                if FrameHeader.PayloadLength > 0 then
                begin
                  err := H2E_FRAME_SIZE_ERROR;
                  break;
                end;
              end else
              begin
                if FrameHeader.PayloadLength mod H2P_SETTINGS_BLOCK_SIZE > 0 then
                begin
                  err := H2E_FRAME_SIZE_ERROR;
                  break;
                end;

                DataSize := FrameHeader.PayloadLength;

                while DataSize > 0 do
                begin
                  SetId := 0;
                  SetValue:= 0;
                  S.Read(SetId, H2P_SETTINGS_ID_SIZE);
                  SetId := BETON(SetId);
                  S.Read(SetValue, H2P_SETTINGS_VALUE_SIZE);
                  SetValue := BETON(SetValue);
                  if (SetId >= 1) and (SetId <= HTTP2_SETTINGS_MAX) then
                  begin
                    if FConSettings[SetID] <> SetValue then
                    begin
                      FConSettings[SetID] := SetValue;
                      case SetID of
                        H2SET_HEADER_TABLE_SIZE,
                        H2SET_MAX_HEADER_LIST_SIZE: ResetHPack;
                      end;
                    end;
                  end;
                  Dec(DataSize, H2P_SETTINGS_BLOCK_SIZE);
                end;

                // send ack settings frame
                PushFrame(H2FT_SETTINGS, 0, H2FL_ACK, nil, 0);
              end;
            end;
            H2FT_WINDOW_UPDATE : begin
              if FrameHeader.PayloadLength <> H2P_WINDOW_INC_SIZE then
              begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              S.Read(DataSize, H2P_WINDOW_INC_SIZE);
              DataSize := BETON(DataSize);
              if DataSize <= 0 then begin
                   err := H2E_PROTOCOL_ERROR;
                   break;
              end else
              if (DataSize > $ffff{?}) then begin
                   //err := H2E_FLOW_CONTROL_ERROR;
                   //break;
              end else begin
               // do nothing for yet
               // realloc the readbuffer?
              end;
            end;
            H2FT_GOAWAY : begin
              if FrameHeader.PayloadLength < H2P_GOAWAY_MIN_SIZE then
              begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              S.Read(FErrorStream, H2P_STREAM_ID_SIZE);
              FErrorStream := BETON(FErrorStream) and H2P_STREAM_ID_MASK;
              S.Read(FLastError, H2P_ERROR_CODE_SIZE);
              FLastError := BETON(FLastError);
              if FrameHeader.PayloadLength > H2P_GOAWAY_MIN_SIZE then begin
                 FErrorDataSize := FrameHeader.PayloadLength - H2P_GOAWAY_MIN_SIZE;
                 if assigned(FErrorData) then
                    FErrorData := ReallocMem(FErrorData, FErrorDataSize) else
                    FErrorData := GetMem(FErrorDataSize);
                 S.Read(FErrorData^, FErrorDataSize);
              end;
              // drop down connection
              ConnectionState := wcDROPPED;
              break;
            end;
            H2FT_PING : begin
              if FrameHeader.PayloadLength < H2P_PING_SIZE then
              begin
                err := H2E_FRAME_SIZE_ERROR;
                break;
              end;
              Buffer := GetMem(H2P_PING_SIZE);
              //fill ping buffer
              S.Read(Buffer^, H2P_PING_SIZE);
              PushFrame(H2FT_PING, 0, H2FL_ACK, Buffer, H2P_PING_SIZE);
            end;
            else
            begin
              err := H2E_PROTOCOL_ERROR;
              break;
            end;
          end;
         if err = H2E_NO_ERROR then
            S.Position := fallbackpos + H2P_FRAME_HEADER_SIZE + FrameHeader.PayloadLength;
        end;
        if (err > H2E_NO_ERROR) or (S.Position >= S.Size) then begin
          break;
        end;
      end;

      if (S.Position < S.Size) and (err = H2E_PARSE_ERROR) then
      begin
        FReadTailSize := S.Size - S.Position;
        TruncReadBuffer;
        err := H2E_NO_ERROR;
      end else
        FReadTailSize := 0;
    finally
      S.Free;
      if assigned(RemoteStr) then RemoteStr.DecReference;
      if assigned(Str) then Str.DecReference;
      if assigned(FrameHeader) then FrameHeader.Free;
      if err <> H2E_NO_ERROR then
      begin
        //send error
        Buffer := GetMem(H2P_GOAWAY_MIN_SIZE);
        //fill goaway buffer
        PHTTP2GoawayPayload(Buffer)^.LastStreamID := NTOBE(FLastStreamID);
        PHTTP2GoawayPayload(Buffer)^.ErrorCode    := NTOBE(err);
        PushFrame(H2FT_GOAWAY, 0, 0, Buffer, H2P_GOAWAY_MIN_SIZE);
      end;
    end;
  finally
    FReadBuffer.UnLock;
  end;
end;

destructor TWCHTTP2Connection.Destroy;
begin
  FConnectionState.Value:= wcDEAD;
  if assigned(FSocketRef) then FSocketRef.DecReference;
  FStreams.Free;
  FFramesToSend.Free;
  ResetHPack;
  FReadBuffer.Free;
  FWriteBuffer.Free;
  FConSettings.Free;
  FReadStamp.Free;
  FWriteStamp.Free;
  FTimeStamp.Free;
  FConnectionState.Free;
  FDataSending.Free;
  inherited Destroy;
end;

class function TWCHTTP2Connection.CheckProtocolVersion(Data: Pointer; sz: integer
  ): TWCProtocolVersion;
begin
  if sz >= H2P_PREFACE_SIZE then
  begin
    if CompareByte(Data^, HTTP2Preface[0], H2P_PREFACE_SIZE) = 0 then
    begin
      Result:=wcHTTP2;
    end else
    begin
      if (PByteArray(Data)^[0] in HTTP1HeadersAllowed) then
        Result:=wcHTTP1 else
        Result:=wcUNK; // other protocol
    end;
  end else Result:= wcUNK;
end;

procedure TWCHTTP2Connection.PushFrame(aFrameType: Byte; StrID: Cardinal;
  aFrameFlags: Byte; aData: Pointer; aDataSize: Cardinal);
begin
  PushFrame(TWCHTTP2Frame.Create(aFrameType, StrID, aFrameFlags, aData, aDataSize));
end;

procedure TWCHTTP2Connection.PushFrame(fr: TWCHTTP2ProtoFrame);
begin
  FFramesToSend.Push_back(fr);
  Refresh;
end;

procedure TWCHTTP2Connection.SendFrames;
var fr : TWCHTTP2ProtoFrame;
    it : TIteratorObject;
    WrBuf : TBufferedStream;
    Sz : Integer;
    CurBuffer : Pointer;
begin
  WrBuf := TBufferedStream.Create;
  try
    FWriteBuffer.Lock;
    try
      CurBuffer := FWriteBuffer.Value;
      WrBuf.SetPointer(Pointer(CurBuffer + FWriteTailSize),
                                             FWriteBufferSize - FWriteTailSize);
      FFramesToSend.Lock;
      try
        repeat
           fr := nil;
           it := FFramesToSend.ListBegin;
           if Assigned(it) then begin
             if TWCHTTP2ProtoFrame(it.Value).Size <=
                                       (WrBuf.Size - WrBuf.Position) then
             begin
               fr := TWCHTTP2ProtoFrame(FFramesToSend.PopValue);
               if Assigned(fr) then begin
                 fr.SaveToStream(WrBuf);
               end;
             end;
           end;
        until not assigned(fr);
      finally
        FFramesToSend.UnLock;
      end;
      if (WrBuf.Position > 0) or (FWriteTailSize > 0) then
      begin
        try
          Sz := FSocketRef.Write(CurBuffer^, WrBuf.Position + FWriteTailSize);
          if Sz < WrBuf.Position then
          begin
            if Sz < 0 then Sz := 0; // ignore non-fatal errors. rollback to tail
            FWriteTailSize := WrBuf.Position - Sz;
            Move(Pointer(CurBuffer + Sz)^, CurBuffer^, FWriteTailSize);
          end else FWriteTailSize:= 0;
        except
          ConnectionState:= wcDROPPED;
        end;
      end;
    finally
      FWriteBuffer.UnLock;
    end;
  finally
    WrBuf.Free;
    FDataSending.Value := false;
    Refresh;
  end;
end;

function TWCHTTP2Connection.PopRequestedStream: TWCHTTPStream;
begin
  Lock;
  try
    if ConnectionState = wcCONNECTED then
    begin
      Result := FStreams.GetNextStreamWithRequest;
      if assigned(Result) then
        Result.ResponseProceed := true;
    end else Result := nil;
  finally
    UnLock;
  end;
end;

function TWCHTTP2Connection.TryToIdleStep(const TS : Qword): Boolean;
begin
  Result := false;
  if assigned(FSocketRef) and ReadyToReadWrite(TS) then
  begin
    {$ifdef socket_select_mode}
    FSocketRef.GetSocketStates;
    {$endif}
    if ssError in FSocketRef.States then
    begin
      ConnectionState:= wcDROPPED;
      Exit;
    end;
    TryToConsumeFrames(TS);
    TryToSendFrames(TS);
    Result := true;
  end;
end;

procedure TWCHTTP2Connection.TryToConsumeFrames(const TS: Qword);
begin
  if (ConnectionState = wcCONNECTED) and
     (FSocketRef.CanRead or FStreams.HasStreamWithRequest) and
      ReadyToRead(TS) and
      assigned(FSocketConsume) then
  begin
    FReadStamp.Value := TS;
    try
      FSocketConsume(FSocketRef);
    except
      ConnectionState := wcDROPPED;
    end;
  end;
end;

procedure TWCHTTP2Connection.TryToSendFrames(const TS: Qword);
begin
  if (ConnectionState = wcCONNECTED) and
       FSocketRef.CanSend and
       (FFramesToSend.Count > 0) and
       ReadyToWrite(TS) then
  begin
    FDataSending.Value := True;
    FWriteStamp.Value  := TS;
    try
      if assigned(FSendData) then
        FSendData(Self) else
        SendFrames;
    except
      ConnectionState := wcDROPPED;
    end;
  end;
end;

procedure TWCHTTP2Connection.ResetHPack;
begin
  if Assigned(FHPackEncoder) then begin
     FHPackEncoder.DecReference;
     FHPackEncoder := nil;
  end;
  if Assigned(FHPackDecoder) then begin
     FHPackDecoder.DecReference;
     FHPackDecoder := nil;
  end;
end;

procedure TWCHTTP2Connection.InitHPack;
begin
  if not Assigned(FHPackEncoder) then begin
     FHPackEncoder := TThreadSafeHPackEncoder.Create(ConnSettings[H2SET_HEADER_TABLE_SIZE]);
     FGarbageCollector.Add(FHPackEncoder);
  end;
  if not assigned(FHPackDecoder) then begin
     FHPackDecoder :=
       TThreadSafeHPackDecoder.Create(ConnSettings[H2SET_MAX_HEADER_LIST_SIZE],
                            ConnSettings[H2SET_HEADER_TABLE_SIZE]);
     FGarbageCollector.Add(FHPackDecoder);
  end;
end;

{ TWCHTTPStreams }

function TWCHTTPStreams.IsStreamClosed(aStrm: TObject; data: pointer): Boolean;
begin
  Result := (TWCHTTPStream(aStrm).StreamState = h2ssCLOSED);
end;

procedure TWCHTTPStreams.AfterStrmExtracted(aObj: TObject);
begin
  TWCHTTPStream(aObj).DecReference;
end;

destructor TWCHTTPStreams.Destroy;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCHTTPStream(P.Value).DecReference;
      P := P.Next;
    end;
    ExtractAll;
  finally
    UnLock;
  end;
  inherited Destroy;
end;

function TWCHTTPStreams.GetByID(aID: Cardinal): TWCHTTPStream;
var P : TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if TWCHTTPStream(P.Value).ID = aID then
      begin
        Result := TWCHTTPStream(P.Value);
        Result.IncReference;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

function TWCHTTPStreams.GetNextStreamWithRequest: TWCHTTPStream;
var P : TIteratorObject;
begin
  Result := nil;
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if TWCHTTPStream(P.Value).RequestReady then
      begin
        Result := TWCHTTPStream(P.Value);
        Result.IncReference;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

function TWCHTTPStreams.HasStreamWithRequest: Boolean;
var P : TIteratorObject;
begin
  Result := false;
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if TWCHTTPStream(P.Value).RequestReady then
      begin
        Result := true;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPStreams.CloseOldIdleStreams(aMaxId: Cardinal);
var NP, P : TIteratorObject;
begin
  // close all idle stream with id less than aMaxId
  // sec.5.1.1 IRF7540
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      NP := P.Next;
      if (TWCHTTPStream(P.Value).ID < aMaxId) and
         (TWCHTTPStream(P.Value).StreamState = h2ssIDLE) then
      begin
        Erase(P);
      end;
      P := NP;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPStreams.RemoveClosedStreams;
begin
  ExtractObjectsByCriteria(@IsStreamClosed, @AfterStrmExtracted, nil);
end;

{ TWCHTTPStream }

function TWCHTTPStream.GetRecursedPriority: Byte;
begin
  if FRecursedPriority < 0 then begin
    Result := FPriority; // todo: calc priority here
    FRecursedPriority:= Result;
  end else Result := FRecursedPriority;
end;

function TWCHTTPStream.GetResponseProceed: Boolean;
begin
  Lock;
  try
    Result := FResponseProceed;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPStream.ResetRecursivePriority;
begin
  FRecursedPriority := -1;
end;

procedure TWCHTTPStream.PushRequest;
begin
  FCurRequest.Complete := true;
end;

procedure TWCHTTPStream.SetResponseProceed(AValue: Boolean);
begin
  Lock;
  try
    if FResponseProceed=AValue then Exit;
    FResponseProceed:=AValue;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPStream.SetWaitingForContinueFrame(AValue: Boolean);
begin
  if FWaitingForContinueFrame=AValue then Exit;
  FWaitingForContinueFrame:=AValue;
  FHeadersComplete:=not AValue;
end;

procedure TWCHTTPStream.UpdateState(Head: TWCHTTP2FrameHeader);
begin
  case FStreamState of
    h2ssIDLE : begin
     if Head.FrameType = H2FT_HEADERS then
        FStreamState := h2ssOPEN;
     end;
    h2ssRESERVEDRem : begin
     if Head.FrameType = H2FT_HEADERS then
        FStreamState := h2ssHLFCLOSEDLoc;
    end;
  end;
end;

procedure TWCHTTPStream.PushData(Data: Pointer; sz: Cardinal);
begin
  FCurRequest.PushData(Data, sz);
end;

procedure TWCHTTPStream.FinishHeaders(aDecoder: TThreadSafeHPackDecoder);
begin
  FHeadersComplete := true;
  FCurRequest.CopyHeaders(aDecoder);
end;

constructor TWCHTTPStream.Create(aConnection: TWCHTTP2Connection;
  aStreamID: Cardinal);
begin
  inherited Create;
  FID := aStreamID;
  FConnection := aConnection;
  FStreamState:=h2ssIDLE;
  FRecursedPriority:=-1;
  FFinishedCode := H2E_NO_ERROR;
  FWaitingForContinueFrame := false;
  FWaitingRemoteStream := aStreamID;
  FHeadersComplete := false;
  FCurRequest := TWCHTTP2Request.Create(FConnection, Self);
  FResponseProceed := false;
end;

destructor TWCHTTPStream.Destroy;
begin
  if assigned(FCurRequest) then FreeAndNil(FCurRequest);
  inherited Destroy;
end;

procedure TWCHTTPStream.Release;
var er : PCardinal;
begin
  if FStreamState <> h2ssCLOSED then begin
    er := GetMem(H2P_RST_STREAM_FRAME_SIZE);
    er^ := NTOBE(H2E_NO_ERROR);
    FConnection.PushFrame(H2FT_RST_STREAM, FID, 0, er, H2P_RST_STREAM_FRAME_SIZE);
    FStreamState := h2ssCLOSED;
  end;
  DecReference;
end;

function TWCHTTPStream.RequestReady: Boolean;
begin
  Result := FCurRequest.Complete and
            FHeadersComplete and
            (not FResponseProceed);
end;

end.
