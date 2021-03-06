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
  Classes, SysUtils,
  ECommonObjs, OGLFastList,
  fphttp, HTTPDefs, httpprotocol, AbstractHTTPServer,
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
    property Socket : TSocketStream read FSocket;
    property States : TSocketStates read FSocketStates;
  end;

  TRefSocketConsume = procedure (SockRef : TWCSocketReference) of object;
  TRefSendData = procedure (aConnection : TWCRefConnection) of object;

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
    Constructor CreateRefered(AServer : TAbsCustomHTTPServer; ASocketRef : TWCSocketReference); virtual;
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
    FSocketConsume : TRefSocketConsume;
    FSendData      : TRefSendData;
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
        aSocketConsume: TRefSocketConsume; aSendData: TRefSendData); virtual;
    procedure ConsumeNextFrame(Mem : TBufferedStream); virtual; abstract;
    procedure ReleaseRead(ConsumeResult: TWCConsumeResult); virtual;
    procedure SendFrames; virtual;
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
        aSocketConsume: TRefSocketConsume; aSendData: TRefSendData); override;
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

  { TWCRefConnections }

  TWCRefConnections = class(TThreadSafeFastSeq)
  private
    FLastUsedConnection : TIteratorObject;
    FMaintainStamp : QWord;
    FGarbageCollector : TNetReferenceList;
    FNeedToRemoveDeadConnections : TThreadBoolean;
    FHelpers : Array [TWCProtocolVersion] of TWCProtocolHelper;
    {$ifdef socket_epoll_mode}
    FTimeout: cInt;
    FEvents: array of TEpollEvent;
    FEventsRead: array of TEpollEvent;
    FEpollReadFD: THandle;   // this one monitors LT style for READ
    FEpollFD: THandle;       // this one monitors ET style for other
    FEpollMasterFD: THandle; // this one monitors the first two
    FEpollLocker : TNetCustomLockedObject;
    procedure AddSocketEpoll(ASocket : TWCSocketReference);
    procedure RemoveSocketEpoll(ASocket : TWCSocketReference);
    procedure ResetReadingSocket(ASocket : TWCSocketReference);
    procedure Inflate;
    procedure CallActionEpoll(aCount : Integer);
    {$endif}
    function GetProtocolHelper(Id : TWCProtocolVersion): TWCProtocolHelper;
    function IsConnDead(aConn: TObject; data: pointer): Boolean;
    procedure AfterConnExtracted(aObj : TObject);
  public
    constructor Create(aGarbageCollector : TNetReferenceList);
    destructor  Destroy; override;
    procedure   AddConnection(FConn : TWCRefConnection);
    function    GetByHandle(aSocket : Cardinal) : TWCRefConnection;
    procedure   RemoveDeadConnections(const TS: QWord; MaxLifeTime: Cardinal);
    procedure   Idle(const TS: QWord);
    procedure   RegisterProtocolHelper(Id : TWCProtocolVersion;
                                          aHelper : TWCProtocolHelperClass);
    procedure   PushSocketError;
    procedure   CloseAll;
    property    Protocol[Id : TWCProtocolVersion] : TWCProtocolHelper read
                                         GetProtocolHelper;
    property    GarbageCollector : TNetReferenceList read FGarbageCollector write
                                         FGarbageCollector;
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
  aSocket: TWCSocketReference; aSocketConsume: TRefSocketConsume;
  aSendData: TRefSendData);
begin
  inherited Create(aOwner, aSocket, aSocketConsume, aSendData);
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

constructor TWCConnection.CreateRefered(AServer: TAbsCustomHTTPServer;
  ASocketRef: TWCSocketReference);
begin
  inherited Create(AServer, nil);
  FSocketRef := ASocketRef;
  ASocketRef.IncReference;
  FRefCon:=nil;
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

{$ifdef SOCKET_EPOLL_MODE}

procedure TWCRefConnections.Inflate;
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

procedure TWCRefConnections.CallActionEpoll(aCount: Integer);
var
  i, MasterChanges, Changes, ReadChanges, m, err: Integer;
  Temp, TempRead: TWCSocketReference;
  MasterEvents: array[0..1] of TEpollEvent;
begin
  if aCount <= 0 then exit;

  Changes := 0;
  ReadChanges := 0;

  repeat
    MasterChanges := epoll_wait(FEpollMasterFD, @MasterEvents[0], 2, FTimeout);
    err := fpgeterrno;
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
            err := fpgeterrno;
          until (Changes >= 0) or (err <> ESysEINTR);
        end
        else
          repeat
            ReadChanges := epoll_wait(FEpollReadFD, @FEventsRead[0], aCount, 0);
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
            Temp := TWCSocketReference(FEvents[i].data.ptr);

            if  ((FEvents[i].events and EPOLLOUT) = EPOLLOUT) then
                Temp.SetCanSend;

            if  ((FEvents[i].events and EPOLLERR) = EPOLLERR) then
                Temp.PushError;// 'Handle error' + Inttostr(SocketError));
          end; // writes

          if i < ReadChanges then begin
            TempRead := TWCSocketReference(FEventsRead[i].data.ptr);

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

procedure TWCRefConnections.AddSocketEpoll(ASocket: TWCSocketReference);
var
  lEvent: TEpollEvent;
begin
  ASocket.IncReference;
  lEvent.events := EPOLLET or EPOLLOUT or EPOLLERR;
  lEvent.data.ptr := ASocket;
  if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll', [SocketError]);
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error adding handle to epoll', [SocketError]);
  if Count > High(FEvents) then
    Inflate;
end;

procedure TWCRefConnections.RemoveSocketEpoll(ASocket: TWCSocketReference);
begin
  try
    epoll_ctl(FEpollFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
    epoll_ctl(FEpollReadFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
  finally
    ASocket.DecReference;
  end;
end;

procedure TWCRefConnections.ResetReadingSocket(
  ASocket: TWCSocketReference);
var
  lEvent: TEpollEvent;
begin
  lEvent.data.ptr := ASocket;
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_MOD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error modify handle in epoll', [SocketError]);
end;

{$endif}

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
  RemoveSocketEpoll(TWCRefConnection(aObj).FSocketRef);
  {$endif}
  TWCRefConnection(aObj).FFramesToSend.Clean;
  TWCRefConnection(aObj).DecReference;
end;

constructor TWCRefConnections.Create(aGarbageCollector: TNetReferenceList);
var v : TWCProtocolVersion;
{$ifdef SOCKET_EPOLL_MODE}
    lEvent : TEpollEvent;
{$endif}
begin
  inherited Create;
  FNeedToRemoveDeadConnections := TThreadBoolean.Create(false);
  FMaintainStamp := GetTickCount64;
  FLastUsedConnection := nil;
  FGarbageCollector := aGarbageCollector;
  For V := Low(TWCProtocolVersion) to High(TWCProtocolVersion) do
    FHelpers[V] := nil;
  {$ifdef SOCKET_EPOLL_MODE}
  FEpollLocker := TNetCustomLockedObject.Create;
  Inflate;
  FTimeout := 1;
  FEpollFD := epoll_create(BASE_SIZE);
  FEpollReadFD := epoll_create(BASE_SIZE);
  FEpollMasterFD := epoll_create(2);
  if (FEPollFD < 0) or (FEpollReadFD < 0) or (FEpollMasterFD < 0) then
    raise ESocketError.CreateFmt('Unable to create epoll: %d', [fpgeterrno]);
  lEvent.events := EPOLLIN or EPOLLOUT or EPOLLPRI or EPOLLERR or EPOLLHUP or EPOLLET;
  lEvent.data.fd := FEpollFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
  lEvent.data.fd := FEpollReadFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollReadFD, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Unable to add FDs to master epoll FD: %d', [fpGetErrno]);
  {$endif}
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
  {$ifdef SOCKET_EPOLL_MODE}
  fpClose(FEpollFD);
  FEpollLocker.Free;
  {$endif}
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
  AddSocketEpoll(FConn.FSocketRef);
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
var P :TIteratorObject;
    i : integer;
begin
  if ((TS - FMaintainStamp) div 1000 > 10) or
     (FNeedToRemoveDeadConnections.Value) then
    RemoveDeadConnections(TS, 120);

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
      if (TWCRefConnection(P.Value).ConnectionAvaible) then
      begin
        if TWCRefConnection(P.Value).FSocketRef.HasErrors then
          TWCRefConnection(P.Value).ConnectionState:= wcDROPPED else
        if TWCRefConnection(P.Value).TryToIdleStep(TS) then
        begin
          FLastUsedConnection := P.Next;
          inc(i);
          if i > 15 then break;
        end;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

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
  Result := ((Int64(TS) - Int64(FReadStamp.Value)) > FReadDelay.Value) and
            (not FDataReading.Value);
end;

function TWCRefConnection.ReadyToWrite(const TS: QWord): Boolean;
begin
  Result := ((Int64(TS) - Int64(FWriteStamp.Value)) > FWriteDelay.Value) and
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
  aSocketConsume: TRefSocketConsume; aSendData: TRefSendData);
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
  FSocketConsume:= aSocketConsume;
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
  FOwner.ResetReadingSocket(FSocketRef);
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
  if assigned(FSocketRef) then FSocketRef.DecReference;
  FFramesToSend.Free;
  if assigned(FReadBuffer) then FReadBuffer.Free;
  if assigned(FWriteBuffer) then FWriteBuffer.Free;
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

function TWCRefConnection.TryToIdleStep(const TS : Qword): Boolean;
begin
  Result := false;
  if assigned(FSocketRef) and ReadyToReadWrite(TS) then
  begin
    {$ifdef socket_select_mode}
    try
      FSocketRef.GetSocketStates;
    except
      on e : ESocketError do ; //catch error
    end;
    if FSocketRef.HasErrors then
    begin
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

procedure TWCRefConnection.TryToConsumeFrames(const TS: Qword);
begin
  if (FSocketRef.CanRead or RequestsWaiting) and
      ReadyToRead(TS) and
      assigned(FSocketConsume) then
  begin
    FDataReading.Value := True;
    FReadStamp.Value := TS;
    FSocketConsume(FSocketRef);
  end;
end;

procedure TWCRefConnection.TryToSendFrames(const TS: Qword);
begin
  if FSocketRef.CanSend and
     ReadyToWrite(TS) then
  begin
    FDataSending.Value := True;
    Refresh(TS);
    FWriteStamp.Value  := TS;
    if assigned(FSendData) then
       FSendData(Self) else
       SendFrames;
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
      aDelay.Value := 16 else
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
    if (aDelay.Value > 16) then aDelay.Value := 16 else
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
