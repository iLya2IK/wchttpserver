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
  {$ifdef DEBUG_STAT}
  wcdebug_vars,
  {$endif}
  uhpack,
  http2consts,
  http2http1conv;

const
  WC_INITIAL_READ_BUFFER_SIZE = 4096;

type

  TWCHTTPStreams = class;
  TWCHTTPRefConnection = class;
  TWCHTTP2Connection = class;
  TWCHTTPRefConnections = class;
  TWCHTTPStream = class;
  TWCHTTP2ServerSettings = class;

  TWCConnectionState = (wcCONNECTED, wcHALFCLOSED, wcDROPPED, wcDEAD);
  TWCProtocolVersion = (wcUNK, wcHTTP1, wcHTTP1_1, wcHTTP2
                       {$IFDEF WC_WEB_SOCKETS}, wcWebSocket{$ENDIF});

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

  { TWCHTTPRefProtoFrame }

  TWCHTTPRefProtoFrame = class
  public
    procedure SaveToStream(Str : TStream); virtual; abstract;
    function Size : Int64; virtual; abstract;
  end;

  { TWCHTTPStringFrame }

  TWCHTTPStringFrame = class(TWCHTTPRefProtoFrame)
  private
    FStr : String;
  public
    constructor Create(const S : String);
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTPStringsFrame }

  TWCHTTPStringsFrame = class(TWCHTTPRefProtoFrame)
  private
    Strm : TMemoryStream;
  public
    constructor Create(Strs : TStrings);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTPStreamFrame }

  TWCHTTPStreamFrame = class(TWCHTTPRefProtoFrame)
  private
    FStrm : TStream;
  public
    constructor Create(Strm: TStream; Sz: Cardinal; Owned: Boolean);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTPRefStreamFrame }

  TWCHTTPRefStreamFrame = class(TWCHTTPRefProtoFrame)
  private
    FStrm : TReferencedStream;
    Fsz, Fpos : Int64;
  public
    constructor Create(Strm : TReferencedStream; Pos, Sz : Int64);
    constructor Create(Strm : TReferencedStream); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  TWCHTTP2Frame = class(TWCHTTPRefProtoFrame)
  public
    Header  : TWCHTTP2FrameHeader;
    Stream  : TWCHTTPStream;
    constructor Create(aFrameType: Byte; aStr: TWCHTTPStream; aFrameFlags: Byte);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;
  
  { TWCHTTP2DataFrame }
  
  TWCHTTP2DataFrame = class(TWCHTTP2Frame)
  public
    Payload : Pointer;
    OwnPayload : Boolean;
    constructor Create(aFrameType: Byte;
      aStream: TWCHTTPStream; aFrameFlags: Byte;
      aData: Pointer; aDataSize: Cardinal; aOwnPayload: Boolean = true); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
  end;

  { TWCHTTP2RefFrame }

  TWCHTTP2RefFrame = class(TWCHTTP2Frame)
  public
    FStrm : TReferencedStream;
    Fpos : Int64;
    constructor Create(aFrameType : Byte;
                       aStream: TWCHTTPStream;
                       aFrameFlags : Byte;
                       aData : TReferencedStream;
                       aStrmPos : Int64;
                       aDataSize : Cardinal);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
  end;

  { TWCHTTP2AdvFrame }

  TWCHTTP2AdvFrame = class(TWCHTTPRefProtoFrame)
  public
    procedure SaveToStream(Str : TStream); override;
    function Size : Int64; override;
  end;

  { TWCHTTP2UpgradeResponseFrame }

  TWCHTTP2UpgradeResponseFrame = class(TWCHTTPRefProtoFrame)
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
    FData          : TMemoryStream;
    FConnection    : TWCHTTP2Connection;
    FStream        : TWCHTTPStream;
    function GetDataBlock : TMemoryStream;
    function GetDataBlockSize : Integer;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); virtual;
    destructor Destroy; override;
    // avaible data
    procedure PushData(aData : Pointer; sz : Cardinal); overload;
    procedure PushData(Strm: TStream; startAt: Int64); overload;
    procedure PushData(Strings: TStrings); overload;
    property  Stream : TWCHTTPStream read FStream;
    property  Data : TMemoryStream read GetDataBlock;
    property  DataBlockSize : Integer read GetDataBlockSize;
  end;

  { TWCHTTP2SerializeStream }

  TWCHTTP2SerializeStream = class(TStream)
  private
    FConn  : TWCHTTP2Connection;
    FStream  : TWCHTTPStream;
    FCurFrame : TWCHTTP2DataFrame;
    FFirstFrameType, FNextFramesType : Byte;
    FFlags, FFinalFlags : Byte;
    FRestFrameSz : Longint;
    FChuncked : Boolean;
    FFirstFramePushed : Boolean;
  public
    constructor Create(aConn: TWCHTTP2Connection;
                       aStrm: TWCHTTPStream;
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
    function Malformed: Boolean;
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
    procedure PushAll(R: TAbsHTTPConnectionResponse);
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
    FResponsePushed  : Boolean;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); override;
    destructor Destroy; override;
    procedure CopyFromHTTP1Response(R : TAbsHTTPConnectionResponse);
    procedure Close;
    procedure PushResponse;
    procedure SerializeResponse;
    procedure SerializeHeaders(closeStrm: Boolean);
    procedure SerializeData(closeStrm: Boolean);
    procedure SerializeResponseHeaders(R : TAbsHTTPConnectionResponse; closeStrm: Boolean);
    procedure SerializeResponseData(R : TAbsHTTPConnectionResponse; closeStrm: Boolean);
    procedure SerializeRefStream(R: TReferencedStream; closeStrm: Boolean);
    property ResponsePushed : Boolean read FResponsePushed;
  end;

  { TWCHTTP2Request }

  TWCHTTP2Request = class(TWCHTTP2Block)
  private
    FComplete : Boolean;
    FResponse : TWCHTTP2Response;
    FHeaders  : THPackHeaderTextList;
    function GetResponse: TWCHTTP2Response;
    function GetResponsePushed: Boolean;
  public
    constructor Create(aConnection : TWCHTTP2Connection;
                       aStream : TWCHTTPStream); override;
    destructor Destroy; override;
    procedure CopyHeaders(aHPackDecoder : TThreadSafeHPackDecoder);
    procedure CopyToHTTP1Request(ARequest : TAbsHTTPConnectionRequest);
    property  Response : TWCHTTP2Response read GetResponse;
    property  ResponsePushed : Boolean read GetResponsePushed;
    property  Complete : Boolean read FComplete write FComplete;
  end;

  { TThreadSafeWindowSize }

  TThreadSafeWindowSize = class(TThreadInteger)
  private
    FBlocked    : Boolean;
    function GetBlocked: Boolean;
    function GetSize: Int32;
  public
    constructor Create(InitialSendSize : Int32);
    procedure Update(aValue : Int32);
    function Send(aValue : Int32) : Boolean;
    procedure Block;
    procedure UnBlock;
    property Size : Int32 read GetSize;
    property Blocked : Boolean read GetBlocked;
  end;

  { TWCReferencedObject }

  TWCRequestRefWrapper = class(TNetReferencedObject)
  public
    function GetReqContentStream : TStream; virtual; abstract;
    function IsReqContentStreamOwn : Boolean; virtual; abstract;
    procedure Release; virtual;
  end;

  { TWCHTTPStream }

  TWCHTTPStream = class(TWCRequestRefWrapper)
  private
    FID : Cardinal;
    FConnection : TWCHTTP2Connection;
    FStreamState : THTTP2StreamState;
    FCurRequest : TWCHTTP2Request;
    FSendWindow : TThreadSafeWindowSize; // no recv window for streams
    FPriority : Byte;
    FRecursedPriority : ShortInt;
    FParentStream : Cardinal;
    FFinishedCode : Cardinal;
    FWaitingForContinueFrame : Boolean;
    FWaitingRemoteStream : Cardinal;
    FHeadersComplete : Boolean;
    FResponseProceed : Boolean;
    function GetCurResponse : TWCHTTP2Response;
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
    function FinishHeaders(aDecoder: TThreadSafeHPackDecoder): Byte;
  public
    constructor Create(aConnection : TWCHTTP2Connection; aStreamID : Cardinal);
    destructor Destroy; override;
    procedure Release; override;
    property ID : Cardinal read FID;
    property StreamState : THTTP2StreamState read FStreamState;
    property ParentStream : Cardinal read FParentStream;
    property Priority :  Byte read FPriority;
    property RecursedPriority : Byte read GetRecursedPriority;
    // avaible request
    function GetReqContentStream : TStream; override;
    function IsReqContentStreamOwn : Boolean; override;
    function RequestReady : Boolean;
    property Request : TWCHTTP2Request read FCurRequest;
    property Response : TWCHTTP2Response read GetCurResponse;
    property ResponseProceed : Boolean read GetResponseProceed write SetResponseProceed;
    property SendWindow : TThreadSafeWindowSize read FSendWindow;
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

  THttpRefSocketConsume = procedure (SockRef : TWCHTTPSocketReference) of object;
  THttpRefSendData = procedure (aConnection : TWCHTTPRefConnection) of object;

  { TWCHTTPConnection }

  TWCHTTPConnection = class(TAbsHTTPConnection)
  private
    FHTTPRefCon  : TWCHTTPRefConnection;
    FRefRequest  : TWCRequestRefWrapper;
    FSocketRef : TWCHTTPSocketReference;
    procedure SetHTTPRefCon(AValue: TWCHTTPRefConnection);
    procedure SetRefRequest(AValue: TWCRequestRefWrapper);
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
    property HTTPRefCon: TWCHTTPRefConnection read FHTTPRefCon write SetHTTPRefCon;
    property RefRequest: TWCRequestRefWrapper read FRefRequest write SetRefRequest;
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

  { TWCHTTPRefConnection }

  TWCHTTPRefConnection = class(TNetReferencedObject)
  private
    FOwner : TWCHTTPRefConnections;
    FConnectionState : TThreadSafeConnectionState;
    FReadBuffer, FWriteBuffer : TThreadPointer;
    FReadBufferSize, FWriteBufferSize : Cardinal;
    FReadTailSize, FWriteTailSize : Integer;
    FSocket : Cardinal;
    FTimeStamp, FReadStamp, FWriteStamp : TThreadQWord;
    FReadDelay, FWriteDelay : TThreadInteger;
    FFramesToSend : TThreadSafeFastSeq;
    FErrorData : Pointer;
    FErrorDataSize : Cardinal;
    FLastError : Cardinal;
    FSocketRef : TWCHTTPSocketReference;
    FSocketConsume : THttpRefSocketConsume;
    FSendData      : THttpRefSendData;
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
    procedure InitializeBuffers;
    function GetInitialReadBufferSize : Cardinal; virtual; abstract;
    function GetInitialWriteBufferSize : Cardinal; virtual; abstract;
    function CanExpandWriteBuffer(aCurSize, aNeedSize : Cardinal) : Boolean; virtual; abstract;
    function RequestsWaiting : Boolean; virtual; abstract;
    function NextFrameToSend(it: TIteratorObject): TIteratorObject; virtual;
    procedure AfterFrameSent({%H-}fr: TWCHTTPRefProtoFrame); virtual; abstract;

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
    property SocketRef : TWCHTTPSocketReference read FSocketRef;
    property Owner: TWCHTTPRefConnections read FOwner;
  public
    constructor Create(aOwner: TWCHTTPRefConnections;
        aSocket: TWCHTTPSocketReference;
        aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData); virtual;
    procedure ConsumeNextFrame(Mem : TBufferedStream); virtual; abstract;
    procedure ReleaseRead(WithSuccess: Boolean); virtual;
    procedure SendFrames; virtual;
    destructor Destroy; override;
    class function CheckProtocolVersion(Data : Pointer; sz : integer) :
                                             TWCProtocolVersion;
    class function Protocol : TWCProtocolVersion; virtual; abstract;
    procedure PushFrame(fr : TWCHTTPRefProtoFrame); virtual; overload;
    procedure PushFrame(const S : String); overload;
    procedure PushFrame(Strm : TStream; Sz : Cardinal; Owned : Boolean); overload;
    procedure PushFrame(Strs : TStrings); overload;
    procedure PushFrame(Strm : TReferencedStream); overload;
    procedure PushFrameFront(fr : TWCHTTPRefProtoFrame);
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

  TWCHTTP11Connection = class(TWCHTTPRefConnection)
  protected
    function GetInitialReadBufferSize : Cardinal; override;
    function GetInitialWriteBufferSize : Cardinal; override;
    function CanExpandWriteBuffer({%H-}aCurSize, aNeedSize : Cardinal) : Boolean; override;
    function RequestsWaiting: Boolean; override;
    procedure AfterFrameSent({%H-}fr: TWCHTTPRefProtoFrame); override;
  public
    constructor Create(aOwner: TWCHTTPRefConnections;
        aSocket: TWCHTTPSocketReference;
        aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData); override;
    procedure ConsumeNextFrame({%H-}Mem : TBufferedStream); override;
    class function Protocol : TWCProtocolVersion; override;
  end;

  { TWCHTTP2Connection }

  TWCHTTP2Connection = class(TWCHTTPRefConnection)
  private
    FLastStreamID : Cardinal;
    FStreams : TWCHTTPStreams;
    FConSettings : TThreadSafeConnSettings;
    FErrorStream : Cardinal;
    FHPackDecoder : TThreadSafeHPackDecoder;
    FHPackEncoder : TThreadSafeHPackEncoder;
    FSendWindow   : TThreadSafeWindowSize;
    FDataConsumed : Cardinal;

    function AddNewStream(aStreamID: Cardinal): TWCHTTPStream;
    function GetConnSetting(id : Word): Cardinal;
    function GetHTTP2Settings: TWCHTTP2ServerSettings;
  protected
    procedure ResetHPack;
    procedure InitHPack;
    property  CurHPackDecoder : TThreadSafeHPackDecoder read FHPackDecoder;
    property  CurHPackEncoder : TThreadSafeHPackEncoder read FHPackEncoder;
    function GetInitialReadBufferSize : Cardinal; override;
    function GetInitialWriteBufferSize : Cardinal; override;
    function CanExpandWriteBuffer({%H-}aCurSize, {%H-}aNeedSize : Cardinal) : Boolean; override;
    function RequestsWaiting: Boolean; override;
    function NextFrameToSend(it: TIteratorObject): TIteratorObject;override;
    procedure AfterFrameSent(fr: TWCHTTPRefProtoFrame); override;
    procedure SendUpdateWindow(Strm : TWCHTTPStream);
  public
    constructor Create(aOwner: TWCHTTPRefConnections;
        aSocket: TWCHTTPSocketReference; aOpenningMode: THTTP2OpenMode;
        aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData); overload;
    class function Protocol : TWCProtocolVersion; override;
    procedure ConsumeNextFrame(Mem : TBufferedStream); override;
    destructor Destroy; override;
    procedure PushFrame(aFrameType : Byte;
                        aStream : TWCHTTPStream;
                        aFrameFlags : Byte;
                        aData : Pointer;
                        aDataSize : Cardinal;
                        aOwnPayload : Boolean = true); overload;
    procedure PushFrame(aFrameType : Byte;
                        aStream : TWCHTTPStream;
                        aFrameFlags : Byte;
                        aData : TReferencedStream;
                        aStrmPos : Int64;
                        aDataSize : Cardinal); overload;
    procedure PushFrameFront(aFrameType : Byte;
                        aStream : TWCHTTPStream;
                        aFrameFlags : Byte;
                        aData : Pointer;
                        aDataSize : Cardinal;
                        aOwnPayload : Boolean = true); overload;
    function PopRequestedStream : TWCHTTPStream;
    function TryToIdleStep(const TS: Qword): Boolean; override;
    procedure ResetStream(aSID, aError: Cardinal);
    procedure GoAway(aError : Cardinal);
    property HTTP2Settings : TWCHTTP2ServerSettings read GetHTTP2Settings;
    property Streams : TWCHTTPStreams read FStreams;
    // error
    property ErrorStream : Cardinal read FErrorStream;
    //
    property ConnSettings[id : Word] : Cardinal read GetConnSetting;
  end;

  { TWCClosedStreams }

  TWCClosedStreams = class
  private
    FStartFrom, FEndAt : Cardinal;
  public
    constructor Create(SID : Cardinal);
    function Expand(SID : Cardinal) : Boolean;
    function MergeRight(n : TWCClosedStreams) : Boolean;
    function Contain(SID : Cardinal) : Boolean;
    property StartFrom : Cardinal read FStartFrom;
  end;

  { TWCHTTPStreams }

  TWCHTTPStreams = class(TThreadSafeFastSeq)
  private
    FClosedStreams : TThreadSafeFastSeq;
    procedure AddClosedStream(SID : Cardinal);
    function IsStreamClosed(aStrm: TObject; {%H-}data: pointer): Boolean;
    procedure AfterStrmExtracted(aObj : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function  IsStreamInClosedArch(SID : Cardinal) : Boolean;
    function  GetByID(aID : Cardinal) : TWCHTTPStream;
    function  GetNextStreamWithRequest : TWCHTTPStream;
    function  HasStreamWithRequest: Boolean;
    procedure CloseOldIdleStreams(aMaxId : Cardinal);
    procedure AdjustWindowSize(Delta : Int32);
    procedure RemoveClosedStreams;
  end;

  { TWCHTTP2ServerSettings }

  TWCHTTP2ServerSettings = class(TNetCustomLockedObject)
  private
    HTTP2ServerSettings : PHTTP2SettingsPayload;
    HTTP2ServerSettingsSize : Cardinal;
    function GetCount: Integer;
    function GetSetting(index : integer): THTTP2SettingsBlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Add(Id : Word; Value : Cardinal);
    function GetByID(Id : Word; DefaultValue : Cardinal) : Cardinal;
    function CopySettingsToMem(var Mem : Pointer) : Integer;
    property Count : Integer read GetCount;
    property Setting[index : integer] : THTTP2SettingsBlock read GetSetting; default;
  end;

  {$ifdef socket_epoll_mode}
  PEpollEvent = ^epoll_event;
  TEpollEvent = epoll_event;
  PEpollData = ^epoll_data;
  TEpollData = epoll_data;
  {$endif}

  { TWCHTTPREfConnections }

  TWCHTTPRefConnections = class(TThreadSafeFastSeq)
  private
    FLastUsedConnection : TIteratorObject;
    FMaintainStamp : QWord;
    FGarbageCollector : TNetReferenceList;
    FNeedToRemoveDeadConnections : TThreadBoolean;
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
    procedure ResetReadingSocket(ASocket : TWCHTTPSocketReference);
    procedure Inflate;
    procedure CallActionEpoll(aCount : Integer);
    {$endif}
    function IsConnDead(aConn: TObject; data: pointer): Boolean;
    procedure AfterConnExtracted(aObj : TObject);
  public
    constructor Create(aGarbageCollector : TNetReferenceList);
    destructor  Destroy; override;
    procedure   AddConnection(FConn : TWCHTTPRefConnection);
    function    GetByHandle(aSocket : Cardinal) : TWCHTTPRefConnection;
    procedure   RemoveDeadConnections(const TS: QWord; MaxLifeTime: Cardinal);
    procedure   Idle(const TS: QWord);
    procedure   PushSocketError;
    procedure   CloseAll;
    property    GarbageCollector : TNetReferenceList read FGarbageCollector write
                                         FGarbageCollector;
  end;

  { TWCHTTP2RefConnections }

  TWCHTTP2RefConnections = class(TWCHTTPRefConnections)
  private
    FSettings : TWCHTTP2ServerSettings;
  protected
    function CheckStreamID(SID : Cardinal) : Boolean; virtual; abstract;
    function CheckHeaders({%H-}Decoder : TThreadSafeHPackDecoder;
                          const {%H-}PseudoHeaders : THTTP2PseudoHeaders) : Cardinal; virtual; abstract;
  public
    constructor Create(aGarbageCollector : TNetReferenceList);
    destructor Destroy; override;
    property HTTP2Settings : TWCHTTP2ServerSettings read FSettings;
  end;

  { TWCHTTP2ServerRefConnections }

  TWCHTTP2ServerRefConnections = class(TWCHTTP2RefConnections)
  protected
    function CheckStreamID(SID : Cardinal) : Boolean; override;
    function CheckHeaders({%H-}Decoder : TThreadSafeHPackDecoder;
                          const PseudoHeaders : THTTP2PseudoHeaders) : Cardinal; override;
  end;

  { TWCHTTP2ClientRefConnections }

  TWCHTTP2ClientRefConnections = class(TWCHTTP2RefConnections)
  protected
    function CheckStreamID(SID : Cardinal) : Boolean; override;
    function CheckHeaders({%H-}Decoder : TThreadSafeHPackDecoder;
                          const {%H-}PseudoHeaders : THTTP2PseudoHeaders) : Cardinal; override;
  end;

implementation

uses uhpackimp;

const
  HTTP1HeadersAllowed = [$0A,$0D,$20,$21,$24,$25,$27..$39,$40..$5A,$61..$7A];
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

{ TWCRequestRefWrapper }

procedure TWCRequestRefWrapper.Release;
begin
  DecReference;
end;

{ TWCClosedStreams }

constructor TWCClosedStreams.Create(SID : Cardinal);
begin
  FStartFrom := SID;
  FEndAt := SID;
end;

function TWCClosedStreams.Expand(SID : Cardinal) : Boolean;
begin
  if (Int32(SID) - Int32(FEndAt)) <= 2 then begin
    FEndAt := SID;
    Result := true;
  end else
  if (Int32(FStartFrom) - Int32(SID)) <= 2 then begin
    FStartFrom := SID;
    Result := true;
  end
  else
    Result := false;
end;

function TWCClosedStreams.MergeRight(n : TWCClosedStreams) : Boolean;
begin
  if (Int32(n.FStartFrom) - Int32(FEndAt)) <= 2 then begin
    FEndAt := n.FEndAt;
    Result := true;
  end else
    Result := false;
end;

function TWCClosedStreams.Contain(SID : Cardinal) : Boolean;
begin
  Result := (SID >= FStartFrom) and (SID <= FEndAt);
end;

{ TWCHTTP2ClientRefConnections }

function TWCHTTP2ClientRefConnections.CheckStreamID(SID: Cardinal): Boolean;
begin
  Result := (SID and $00000001) = 0;
end;

function TWCHTTP2ClientRefConnections.CheckHeaders(
  Decoder: TThreadSafeHPackDecoder; const PseudoHeaders: THTTP2PseudoHeaders
  ): Cardinal;
begin
  Result := H2E_NO_ERROR;
end;

{ TWCHTTP2ServerRefConnections }

function TWCHTTP2ServerRefConnections.CheckStreamID(SID: Cardinal): Boolean;
begin
  Result := (SID and $00000001) > 0;
end;

function TWCHTTP2ServerRefConnections.CheckHeaders(
  {%H-}Decoder: TThreadSafeHPackDecoder; const PseudoHeaders: THTTP2PseudoHeaders
  ): Cardinal;
begin
  // server-specific check
  if Length(PseudoHeaders[hh2Status]) > 0 then
    Exit(H2E_PROTOCOL_ERROR);
  if (Length(PseudoHeaders[hh2Path]) = 0) and
      (SameStr(PseudoHeaders[hh2Scheme], 'http') or
       SameStr(PseudoHeaders[hh2Scheme], 'https')) then
    Exit(H2E_PROTOCOL_ERROR);
  if (Length(PseudoHeaders[hh2Scheme]) = 0) then
    Exit(H2E_PROTOCOL_ERROR);
  Exit(H2E_NO_ERROR);
end;

{ TWCHTTP2RefConnections }

constructor TWCHTTP2RefConnections.Create(aGarbageCollector: TNetReferenceList);
begin
  inherited Create(aGarbageCollector);
  FSettings := TWCHTTP2ServerSettings.Create;
end;

destructor TWCHTTP2RefConnections.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

{ TThreadSafeWindowSize }

function TThreadSafeWindowSize.GetSize: Int32;
begin
  Result := Value;
end;

function TThreadSafeWindowSize.GetBlocked: Boolean;
begin
  Lock;
  try
    Result := FBlocked;
  finally
    UnLock;
  end;
end;

constructor TThreadSafeWindowSize.Create(InitialSendSize: Int32);
begin
  inherited Create(InitialSendSize);
  FBlocked := false;
end;

procedure TThreadSafeWindowSize.Update(aValue: Int32);
begin
  Lock;
  try
    IncValue(aValue);
    if Value > 0 then FBlocked := false;
  finally
    UnLock;
  end;
end;

function TThreadSafeWindowSize.Send(aValue: Int32): Boolean;
begin
  Lock;
  try
    if Value >= aValue then
    begin
      DecValue(aValue);
      Result := true;
    end else
      Result := false;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeWindowSize.Block;
begin
  Lock;
  try
    FBlocked:=true;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeWindowSize.UnBlock;
begin
  Lock;
  try
    FBlocked:=false;
  finally
    UnLock;
  end;
end;

{ TWCHTTPRefStreamFrame }

constructor TWCHTTPRefStreamFrame.Create(Strm: TReferencedStream;
                    Pos, Sz: Int64 );
begin
  Strm.IncReference;
  FStrm := Strm;
  Fsz:= Sz;
  Fpos:=Pos;
end;

constructor TWCHTTPRefStreamFrame.Create(Strm: TReferencedStream);
begin
  Strm.IncReference;
  FStrm := Strm;
  Fsz:= Strm.Stream.Size;
  Fpos:=0;
end;

destructor TWCHTTPRefStreamFrame.Destroy;
begin
  FStrm.DecReference;
  inherited Destroy;
end;

procedure TWCHTTPRefStreamFrame.SaveToStream(Str: TStream);
begin
  FStrm.WriteTo(Str, Fpos, FSz);
end;

function TWCHTTPRefStreamFrame.Size: Int64;
begin
  Result := Fsz;
end;

{ TWCHTTPStringFrame }

constructor TWCHTTPStringFrame.Create(const S: String);
begin
  FStr := S;
end;

procedure TWCHTTPStringFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(FStr[1], Size);
end;

function TWCHTTPStringFrame.Size: Int64;
begin
  Result := Length(FStr);
end;

{ TWCHTTPStringsFrame }

constructor TWCHTTPStringsFrame.Create(Strs: TStrings);
begin
  Strm := TMemoryStream.Create;
  Strs.SaveToStream(Strm);
end;

destructor TWCHTTPStringsFrame.Destroy;
begin
  Strm.Free;
  inherited Destroy;
end;

procedure TWCHTTPStringsFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(Strm.Memory^, Strm.Size);
end;

function TWCHTTPStringsFrame.Size: Int64;
begin
  Result := Strm.Size;
end;

{ TWCHTTPStreamFrame }

constructor TWCHTTPStreamFrame.Create(Strm: TStream; Sz: Cardinal;
  Owned: Boolean);
begin
  if Owned then FStrm := Strm else
  begin
     FStrm := TMemoryStream.Create;
     FStrm.CopyFrom(Strm, Sz);
     FStrm.Position:=0;
  end;
end;

destructor TWCHTTPStreamFrame.Destroy;
begin
  FStrm.Free;
  inherited Destroy;
end;

procedure TWCHTTPStreamFrame.SaveToStream(Str: TStream);
begin
  Str.CopyFrom(FStrm, 0);
end;

function TWCHTTPStreamFrame.Size: Int64;
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

procedure TWCHTTP11Connection.AfterFrameSent({%H-}fr: TWCHTTPRefProtoFrame);
begin
 //
end;

procedure TWCHTTP11Connection.ConsumeNextFrame({%H-}Mem: TBufferedStream);
begin
  // do nothing for now
end;

constructor TWCHTTP11Connection.Create(aOwner: TWCHTTPRefConnections;
  aSocket: TWCHTTPSocketReference; aSocketConsume: THttpRefSocketConsume;
  aSendData: THttpRefSendData);
begin
  inherited Create(aOwner, aSocket, aSocketConsume, aSendData);
  InitializeBuffers;
end;

class function TWCHTTP11Connection.Protocol: TWCProtocolVersion;
begin
  Result := wcHTTP1_1;
end;

{ TWCHTTP2ServerSettings }

function TWCHTTP2ServerSettings.GetCount: Integer;
begin
  Lock;
  try
    Result := HTTP2ServerSettingsSize div H2P_SETTINGS_BLOCK_SIZE;
  finally
    UnLock;
  end;
end;

function TWCHTTP2ServerSettings.GetSetting(index : integer
  ): THTTP2SettingsBlock;
begin
  Lock;
  try
    Result := HTTP2ServerSettings^[index];
  finally
    UnLock;
  end;
end;

constructor TWCHTTP2ServerSettings.Create;
begin
  inherited Create;
  HTTP2ServerSettings := nil;
  HTTP2ServerSettingsSize := 0;
end;

destructor TWCHTTP2ServerSettings.Destroy;
begin
  if assigned(HTTP2ServerSettings) then Freemem(HTTP2ServerSettings);
  inherited Destroy;
end;

procedure TWCHTTP2ServerSettings.Reset;
begin
  Lock;
  try
    if assigned(HTTP2ServerSettings) then FreeMem(HTTP2ServerSettings);
    HTTP2ServerSettings := GetMem(HTTP2_SETTINGS_MAX_SIZE);
    HTTP2ServerSettingsSize := 0;
  finally
    UnLock;
  end;
end;

procedure TWCHTTP2ServerSettings.Add(Id: Word; Value: Cardinal);
var l, Sz : Integer;
    S : PHTTP2SettingsPayload;
begin
  Lock;
  try
    if not Assigned(HTTP2ServerSettings) then
      Reset;
    S := HTTP2ServerSettings;
    Sz := HTTP2ServerSettingsSize div H2P_SETTINGS_BLOCK_SIZE;
    for l := 0 to Sz - 1 do
    begin
      if S^[l].Identifier = Id then begin
        S^[l].Value := Value;
        Exit;
      end;
    end;
    if HTTP2ServerSettingsSize < HTTP2_SETTINGS_MAX_SIZE then
    begin
      S^[Sz].Identifier := Id;
      S^[Sz].Value := Value;
      Inc(HTTP2ServerSettingsSize, H2P_SETTINGS_BLOCK_SIZE);
    end;
  finally
    UnLock;
  end;
end;

function TWCHTTP2ServerSettings.GetByID(Id: Word; DefaultValue: Cardinal
  ): Cardinal;
var l, Sz : Integer;
    S : PHTTP2SettingsPayload;
begin
  Result := DefaultValue;
  Lock;
  try
    S := HTTP2ServerSettings;
    Sz := HTTP2ServerSettingsSize div H2P_SETTINGS_BLOCK_SIZE;
    for l := 0 to Sz - 1 do
    begin
      if S^[l].Identifier = Id then begin
        Result := S^[l].Value;
        Exit;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TWCHTTP2ServerSettings.CopySettingsToMem(var Mem: Pointer): Integer;
begin
  Lock;
  try
    Result := HTTP2ServerSettingsSize;
    if HTTP2ServerSettingsSize > 0 then
    begin
      Mem := GetMem(HTTP2ServerSettingsSize);
      Move(HTTP2ServerSettings^, Mem^, HTTP2ServerSettingsSize);
    end else Mem := nil;
  finally
    UnLock;
  end;
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

function TThreadSafeHPackDecoder.Malformed: Boolean;
begin
  Lock;
  try
    Result := FDecoder.EndHeaderBlockTruncated;
    if not Result then
    begin
      Result := FDecoder.DecodedHeaders.Count = 0;
    end;
  finally
    UnLock;
  end;
end;

{ TWCHTTPSocketReference }

{$ifdef socket_select_mode}
{$ifdef windows}
const SOCKET_FDSET_SIZE = Sizeof(Cardinal) + Sizeof(TSocket) * 2;
{$endif}
{$ifdef linux}
const SOCKET_FDSET_SIZE = Sizeof(TFDSet);
{$endif}
{$endif}

constructor TWCHTTPSocketReference.Create(aSocket: TSocketStream);
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

destructor TWCHTTPSocketReference.Destroy;
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
    Result := ([ssCanRead, ssReading,
                ssSending, ssError] * FSocketStates) = [ssCanRead];
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.CanSend: Boolean;
begin
  Lock;
  try
    Result := ([ssCanSend, ssReading,
                ssSending, ssError] * FSocketStates) = [ssCanSend];
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.HasErrors: Boolean;
begin
  Lock;
  try
    Result := ssError in FSocketStates;
  finally
    UnLock;
  end;
end;

function TWCHTTPSocketReference.HasNoErrors: Boolean;
begin
  Result := not HasErrors;
end;

function TWCHTTPSocketReference.StartReading : Boolean;
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

procedure TWCHTTPSocketReference.StopReading;
begin
  Lock;
  try
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
      FSocketStates := FSocketStates + [ssSending];
      Result := true;
    end else Result := false;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPSocketReference.StopSending;
begin
  Lock;
  try
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

function TWCHTTPSocketReference.Write(const Buffer; Size: Integer): Integer;
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

function TWCHTTPSocketReference.Read(var Buffer; Size: Integer): Integer;
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

{ TWCHTTPConnection }

procedure TWCHTTPConnection.SetHTTPRefCon(AValue: TWCHTTPRefConnection);
begin
  if FHTTPRefCon=AValue then Exit;
  if assigned(FHTTPRefCon) then
    FHTTPRefCon.DecReference;
  SetRefRequest(nil);
  FHTTPRefCon:=AValue;
end;

procedure TWCHTTPConnection.SetRefRequest(AValue : TWCRequestRefWrapper);
begin
  if FRefRequest=AValue then Exit;
  if Assigned(FRefRequest) then FRefRequest.Release;  //release here!
  FRefRequest:=AValue;
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
  FHTTPRefCon:=nil;
  FRefRequest:=nil;
end;

constructor TWCHTTPConnection.CreateRefered(AServer: TAbsCustomHTTPServer;
  ASocketRef: TWCHTTPSocketReference);
begin
  inherited Create(AServer, nil);
  FSocketRef := ASocketRef;
  ASocketRef.IncReference;
  FHTTPRefCon:=nil;
  FRefRequest:=nil;
end;

destructor TWCHTTPConnection.Destroy;
begin
  SetHTTPRefCon(nil);
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
  else
    Buffer := nil;
    BufferSize := 0;
  end;
  if assigned(Buffer) then
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
  Move(FMem.Memory^, PByte(FBuf)[FSize], FMem.Position);
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

procedure TWCHTTP2ResponseHeaderPusher.PushAll(R: TAbsHTTPConnectionResponse);
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
  aStrm: TWCHTTPStream; aFirstFrameType: Byte; aNextFramesType: Byte; aFlags,
  aFinalFlags: Byte);
begin
  Inherited Create;
  FStream := aStrm;
  if assigned(FStream) then FStream.IncReference;
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
  if FFirstFrameType in HTTP2_FLOW_CONTROL_FRAME_TYPES then
  begin
    if FConn.FSendWindow.Size < MaxSize then
      MaxSize := FConn.FSendWindow.Size;
    if Assigned(FStream) then
    begin
      if (FStream.FSendWindow.Size < MaxSize) then
        MaxSize := FStream.FSendWindow.Size;
    end;
    if MaxSize < HTTP2_MIN_MAX_FRAME_SIZE then
       MaxSize := HTTP2_MIN_MAX_FRAME_SIZE;
  end;
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
      FRestFrameSz := MaxSize - Bsz;
      B := GetMem(MaxSize);
      if FFirstFramePushed then
        FCurFrame := TWCHTTP2DataFrame.Create(FNextFramesType, FStream, FFlags, B, Bsz)
      else begin
        FCurFrame := TWCHTTP2DataFrame.Create(FFirstFrameType, FStream, FFlags, B, Bsz);
        FFirstFramePushed:= true;
      end;
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
  if Assigned(FStream) then FStream.DecReference;
  FConn.DecReference;
  inherited Destroy;
end;

{ TWCHTTP2Response }

constructor TWCHTTP2Response.Create(aConnection: TWCHTTP2Connection;
  aStream: TWCHTTPStream);
begin
  inherited Create(aConnection, aStream);
  FResponsePushed := false;
  FCurHeadersBlock:= nil;
end;

destructor TWCHTTP2Response.Destroy;
begin
  if assigned(FCurHeadersBlock) then FreeMemAndNil(FCurHeadersBlock);
  inherited Destroy;
end;

procedure TWCHTTP2Response.CopyFromHTTP1Response(R: TAbsHTTPConnectionResponse);
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
//var er : PHTTP2RstStreamPayload;
begin
  FConnection.PushFrame(H2FT_DATA, FStream, H2FL_END_STREAM, nil, 0);
  {er := GetMem(H2P_RST_STREAM_FRAME_SIZE);
  er^.ErrorCode := H2E_NO_ERROR;
  FConnection.PushFrame(H2FT_RST_STREAM, FStream.ID, 0, er, H2P_RST_STREAM_FRAME_SIZE); }
end;

procedure TWCHTTP2Response.PushResponse;
begin
  FResponsePushed := true;
end;

procedure TWCHTTP2Response.SerializeResponse;
begin
  SerializeHeaders(DataBlockSize = 0);
  if DataBlockSize > 0 then
     SerializeData(true);
end;

procedure TWCHTTP2Response.SerializeHeaders(closeStrm : Boolean);
var
  sc : TWCHTTP2SerializeStream;
begin
  if Assigned(FCurHeadersBlock) then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream,
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
    if closeStrm then
      PushResponse;
  end;
end;

procedure TWCHTTP2Response.SerializeData(closeStrm : Boolean);
var
  sc : TWCHTTP2SerializeStream;
begin
  // serialize in group of data chunck with max_frame_size
  // then remove fdatablock
  if (DataBlockSize > 0) then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream,
                                         H2FT_DATA,
                                         H2FT_DATA,
                                         0,
                                         (Ord(closeStrm) * H2FL_END_STREAM));
    try
      sc.WriteBuffer(FData.Memory^, DataBlockSize);
    finally
      sc.Free;
    end;
    if closeStrm then
      PushResponse;
  end;
  FData.Clear;
end;

procedure TWCHTTP2Response.SerializeResponseHeaders(R: TAbsHTTPConnectionResponse;
  closeStrm: Boolean);
var sc : TWCHTTP2SerializeStream;
    pusher : TWCHTTP2StrmResponseHeaderPusher;
begin
  if Assigned(FCurHeadersBlock) then FreeMemAndNil(FCurHeadersBlock);
  FHeadersBlockSize:=0;

  sc := TWCHTTP2SerializeStream.Create(FConnection,
                                       FStream,
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
    if closeStrm then
      PushResponse;
  finally
    sc.Free;
    pusher.Free;
  end;
end;

procedure TWCHTTP2Response.SerializeResponseData(R: TAbsHTTPConnectionResponse;
  closeStrm: Boolean);
var sc : TWCHTTP2SerializeStream;
begin
  FData.Clear;

  if R.ContentLength > 0 then
  begin
    sc := TWCHTTP2SerializeStream.Create(FConnection, FStream,
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
      if closeStrm then
        PushResponse;
    finally
      sc.Free;
    end;
  end;
end;

procedure TWCHTTP2Response.SerializeRefStream(R: TReferencedStream;
  closeStrm: Boolean);
var BSz, MaxSize : Longint;
    CurFrame : TWCHTTP2RefFrame;
    Pos, Size : Int64;
begin
  R.IncReference;
  try
    Pos := 0;
    Size := R.Stream.Size;
    CurFrame := nil;
    MaxSize := FConnection.ConnSettings[H2SET_MAX_FRAME_SIZE];
    if FConnection.FSendWindow.Size < MaxSize then
      MaxSize := FConnection.FSendWindow.Size;
    if Assigned(FStream) then
    begin
      if (FStream.FSendWindow.Size < MaxSize) then
        MaxSize := FStream.FSendWindow.Size;
    end;
    if MaxSize < HTTP2_MIN_MAX_FRAME_SIZE then
       MaxSize := HTTP2_MIN_MAX_FRAME_SIZE;
    while Size > 0 do begin
      if Assigned(CurFrame) then
      begin
        FConnection.PushFrame(CurFrame);
        CurFrame := nil;
      end;
      if Size > MaxSize then Bsz := MaxSize else Bsz := Size;
      CurFrame := TWCHTTP2RefFrame.Create(H2FT_DATA, FStream, 0, R, Pos, Bsz);
      Inc(Pos, BSz);
      Dec(Size, BSz);
    end;
    if assigned(CurFrame) then begin
      if closeStrm then
        CurFrame.Header.FrameFlag := H2FL_END_STREAM;
      FConnection.PushFrame(CurFrame);
    end;
    if closeStrm then
      PushResponse;
  finally
    R.DecReference;
  end;
end;

{ TWCHTTP2Request }

function TWCHTTP2Request.GetResponse: TWCHTTP2Response;
begin
  if Assigned(FResponse) then Exit(FResponse);
  FResponse := TWCHTTP2Response.Create(FConnection, FStream);
  Result := FResponse;
end;

function TWCHTTP2Request.GetResponsePushed: Boolean;
begin
  if Assigned(FResponse) then
  begin
    Result := FResponse.ResponsePushed;
  end else Result := false;
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

procedure TWCHTTP2Request.CopyToHTTP1Request(ARequest: TAbsHTTPConnectionRequest);
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
    finally
      //
    end;
  end;
end;
  
{ TWCHTTP2Frame }

constructor TWCHTTP2Frame.Create(aFrameType: Byte;
  aStr: TWCHTTPStream;
  aFrameFlags: Byte);
begin
  Header := TWCHTTP2FrameHeader.Create;
  Header.FrameType := aFrameType;
  Header.FrameFlag := aFrameFlags;
  Header.PayloadLength := 0;
  Stream := aStr;
  if assigned(Stream) then
  begin
    Stream.IncReference;
    Header.StreamID := Stream.ID;
  end else
    Header.StreamID := 0;
end;
                     
destructor TWCHTTP2Frame.Destroy;
begin
  Header.Free;
  if assigned(Stream) then Stream.DecReference;
  inherited Destroy;
end;
                     
procedure TWCHTTP2Frame.SaveToStream(Str : TStream);
begin
  Header.SaveToStream(Str);
end;

function TWCHTTP2Frame.Size: Int64;
begin
  Result := H2P_FRAME_HEADER_SIZE + Header.PayloadLength;
end;

{ TWCHTTP2DataFrame }

constructor TWCHTTP2DataFrame.Create(aFrameType: Byte; aStream: TWCHTTPStream;
  aFrameFlags: Byte; aData: Pointer; aDataSize: Cardinal; aOwnPayload: Boolean);
begin
  inherited Create(aFrameType, aStream, aFrameFlags);
  Header.PayloadLength := aDataSize;
  Payload:= aData;
  OwnPayload:= aOwnPayload;
end;

destructor TWCHTTP2DataFrame.Destroy;
begin
  if Assigned(Payload) and OwnPayload then Freemem(Payload);
  inherited Destroy;
end;

procedure TWCHTTP2DataFrame.SaveToStream(Str: TStream);
begin
  inherited SaveToStream(Str);
  if Header.PayloadLength > 0 then
    Str.Write(Payload^, Header.PayloadLength);
end;

{ TWCHTTP2RefFrame }

constructor TWCHTTP2RefFrame.Create(aFrameType: Byte; aStream: TWCHTTPStream;
  aFrameFlags: Byte; aData: TReferencedStream; aStrmPos: Int64;
  aDataSize: Cardinal);
begin
  inherited Create(aFrameType, aStream, aFrameFlags);
  Header.PayloadLength := aDataSize;
  aData.IncReference;
  FStrm := aData;
  Fpos:= aStrmPos;
end;

destructor TWCHTTP2RefFrame.Destroy;
begin
  FStrm.DecReference;
  inherited Destroy;
end;

procedure TWCHTTP2RefFrame.SaveToStream(Str: TStream);
begin
  inherited SaveToStream(Str);
  if Header.PayloadLength > 0 then
    FStrm.WriteTo(Str, Fpos, Header.PayloadLength)
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

procedure TWCHTTP2Block.PushData(aData: Pointer; sz: Cardinal);
var cursize : int64;
begin
  if sz = 0 then Exit;

  cursize := FData.Size;
  FData.Size := cursize + sz;

  Move(aData^, PByte(FData.Memory)[cursize], Sz);
end;

procedure TWCHTTP2Block.PushData(Strm: TStream; startAt : Int64);
var sz : Int64;
    cursize : int64;
begin
  Strm.Position:= startAt;
  sz := Strm.Size - startAt;
  if Sz > 0 then
  begin
    cursize := FData.Size;
    FData.Size := cursize + sz;

    Strm.Read(PByte(FData.Memory)[cursize], Sz);
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

function TWCHTTP2Block.GetDataBlock : TMemoryStream;
begin
  Result := FData;
end;

function TWCHTTP2Block.GetDataBlockSize : Integer;
begin
  Result := FData.Size;
end;

constructor TWCHTTP2Block.Create(aConnection: TWCHTTP2Connection;
  aStream: TWCHTTPStream);
begin
  FData := TMemoryStream.Create;
  FConnection := aConnection;
  FStream := aStream;
end;

destructor TWCHTTP2Block.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

{ TWCHTTPRefConnections }

{$ifdef SOCKET_EPOLL_MODE}

procedure TWCHTTPRefConnections.Inflate;
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

procedure TWCHTTPRefConnections.CallActionEpoll(aCount: Integer);
var
  i, MasterChanges, Changes, ReadChanges, m, err: Integer;
  Temp, TempRead: TWCHTTPSocketReference;
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

procedure TWCHTTPRefConnections.AddSocketEpoll(ASocket: TWCHTTPSocketReference);
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

procedure TWCHTTPRefConnections.RemoveSocketEpoll(ASocket: TWCHTTPSocketReference);
begin
  try
    epoll_ctl(FEpollFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
    epoll_ctl(FEpollReadFD, EPOLL_CTL_DEL, ASocket.FSocket.Handle, nil);
  finally
    ASocket.DecReference;
  end;
end;

procedure TWCHTTPRefConnections.ResetReadingSocket(
  ASocket: TWCHTTPSocketReference);
var
  lEvent: TEpollEvent;
begin
  lEvent.data.ptr := ASocket;
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP or EPOLLET or EPOLLONESHOT;
  if epoll_ctl(FEpollReadFD, EPOLL_CTL_MOD, ASocket.FSocket.Handle, @lEvent) < 0 then
    raise ESocketError.CreateFmt('Error modify handle in epoll', [SocketError]);
end;

{$endif}

function TWCHTTPRefConnections.IsConnDead(aConn: TObject; data: pointer
  ): Boolean;
begin
  with TWCHTTPRefConnection(aConn) do
  Result := (GetLifeTime(PWCLifeTimeChecker(data)^.CurTime) >= PWCLifeTimeChecker(data)^.MaxLifeTime) or
            (not ConnectionAvaible);
end;

procedure TWCHTTPRefConnections.AfterConnExtracted(aObj: TObject);
begin
  {$ifdef SOCKET_EPOLL_MODE}
  RemoveSocketEpoll(TWCHTTPRefConnection(aObj).FSocketRef);
  {$endif}
  TWCHTTPRefConnection(aObj).FFramesToSend.Clean;
  TWCHTTPRefConnection(aObj).DecReference;
end;

constructor TWCHTTPRefConnections.Create(aGarbageCollector: TNetReferenceList);
{$ifdef SOCKET_EPOLL_MODE}
var lEvent : TEpollEvent;
{$endif}
begin
  inherited Create;
  FNeedToRemoveDeadConnections := TThreadBoolean.Create(false);
  FMaintainStamp := GetTickCount64;
  FLastUsedConnection := nil;
  FGarbageCollector := aGarbageCollector;
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

destructor TWCHTTPRefConnections.Destroy;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCHTTPRefConnection(P.Value).DecReference;
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
  inherited Destroy;
end;

procedure TWCHTTPRefConnections.AddConnection(FConn: TWCHTTPRefConnection);
begin
  Push_back(FConn);
  {$ifdef socket_epoll_mode}
  AddSocketEpoll(FConn.FSocketRef);
  {$endif}
end;

function TWCHTTPRefConnections.GetByHandle(aSocket: Cardinal): TWCHTTPRefConnection;
var P :TIteratorObject;
begin
  Result := nil;
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if (TWCHTTPRefConnection(P.Value).Socket = aSocket) and
         (TWCHTTPRefConnection(P.Value).ConnectionAvaible) then
      begin
        Result := TWCHTTPRefConnection(P.Value);
        Result.IncReference;
        Break;
      end;
      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPRefConnections.RemoveDeadConnections(const TS : QWord;
  MaxLifeTime: Cardinal);
var LifeTime : TWCLifeTimeChecker;
begin
  FNeedToRemoveDeadConnections.Value := false;
  LifeTime.CurTime := TS;
  LifeTime.MaxLifeTime := MaxLifeTime;
  FMaintainStamp := TS;
  ExtractObjectsByCriteria(@IsConnDead, @AfterConnExtracted, @LifeTime);
end;

procedure TWCHTTPRefConnections.Idle(const TS : QWord);
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
      if (TWCHTTPRefConnection(P.Value).ConnectionAvaible) then
      begin
        if TWCHTTPRefConnection(P.Value).FSocketRef.HasErrors then
          TWCHTTPRefConnection(P.Value).ConnectionState:= wcDROPPED else
        if TWCHTTPRefConnection(P.Value).TryToIdleStep(TS) then
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

procedure TWCHTTPRefConnections.PushSocketError;
begin
  FNeedToRemoveDeadConnections.Value := True;
end;

procedure TWCHTTPRefConnections.CloseAll;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      if (TWCHTTPRefConnection(P.Value).ConnectionAvaible) then
        TWCHTTPRefConnection(P.Value).ConnectionState := wcDROPPED;

      P := P.Next;
    end;
  finally
    UnLock;
  end;
end;

{ TWCHTTPRefConnection }

procedure TWCHTTPRefConnection.SetConnectionState(CSt: TWCConnectionState);
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

function TWCHTTPRefConnection.GetConnectionState: TWCConnectionState;
begin
  Result := FConnectionState.Value;
end;

function TWCHTTPRefConnection.ReadyToReadWrite(const TS: QWord): Boolean;
begin
  Result := ReadyToRead(TS) or ReadyToWrite(TS);
end;

function TWCHTTPRefConnection.ReadyToRead(const TS: QWord): Boolean;
begin
  Result := ((Int64(TS) - Int64(FReadStamp.Value)) > FReadDelay.Value) and
            (not FDataReading.Value);
end;

function TWCHTTPRefConnection.ReadyToWrite(const TS: QWord): Boolean;
begin
  Result := ((Int64(TS) - Int64(FWriteStamp.Value)) > FWriteDelay.Value) and
            (not FDataSending.Value) and
            ((FFramesToSend.Count > 0) or (FWriteTailSize > 0));
end;

function TWCHTTPRefConnection.GetLifeTime(const TS: QWord): Cardinal;
begin
  Result := (Int64(TS) - Int64(FTimeStamp.Value)) div 1000;
end;

procedure TWCHTTPRefConnection.Refresh(const TS: QWord);
begin
  FTimeStamp.Value := TS;
end;

constructor TWCHTTPRefConnection.Create(aOwner: TWCHTTPRefConnections;
  aSocket: TWCHTTPSocketReference;
  aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData);
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

procedure TWCHTTPRefConnection.ReleaseRead(WithSuccess : Boolean);
begin
  FDataReading.Value := false;
  {$ifdef SOCKET_EPOLL_MODE}
  FOwner.ResetReadingSocket(FSocketRef);
  {$endif}
  if WithSuccess then
  begin
    Refresh(GetTickCount64);
    RelaxDelayValue(FReadDelay);
  end else
    HoldDelayValue(FReadDelay);
end;

destructor TWCHTTPRefConnection.Destroy;
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

class function TWCHTTPRefConnection.CheckProtocolVersion(Data: Pointer; sz: integer
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

procedure TWCHTTPRefConnection.PushFrame(fr: TWCHTTPRefProtoFrame);
begin
  FFramesToSend.Push_back(fr);
end;

procedure TWCHTTPRefConnection.PushFrame(const S: String);
begin
  PushFrame(TWCHTTPStringFrame.Create(S));
end;

procedure TWCHTTPRefConnection.PushFrame(Strm: TStream; Sz: Cardinal;
  Owned: Boolean);
begin
  PushFrame(TWCHTTPStreamFrame.Create(Strm, Sz, Owned));
end;

procedure TWCHTTPRefConnection.PushFrame(Strs: TStrings);
begin
  PushFrame(TWCHTTPStringsFrame.Create(Strs));
end;

procedure TWCHTTPRefConnection.PushFrame(Strm: TReferencedStream);
begin
  PushFrame(TWCHTTPRefStreamFrame.Create(Strm));
end;

procedure TWCHTTPRefConnection.PushFrameFront(fr: TWCHTTPRefProtoFrame);
begin
  FFramesToSend.Push_front(fr);
end;

procedure TWCHTTPRefConnection.SendFrames;
var fr : TWCHTTPRefProtoFrame;
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
      WrBuf.SetPointer(Pointer(CurBuffer + FWriteTailSize),
                                             FWriteBufferSize - FWriteTailSize);
      FFramesToSend.Lock;
      try
        it := FFramesToSend.ListBegin;
        repeat
           fr := nil;
           it := NextFrameToSend(it);
           if Assigned(it) then begin

             FrameCanSend := true;

             if (TWCHTTPRefProtoFrame(it.Value).Size >
                                       (WrBuf.Size - WrBuf.Position)) then
             begin
               Sz := WrBuf.Position + TWCHTTPRefProtoFrame(it.Value).Size;
               if CanExpandWriteBuffer(FWriteBufferSize, Sz) then
               begin
                 CurBuffer:= ReAllocMem(CurBuffer, Sz);
                 FWriteBuffer.Realloc(CurBuffer);
                 FWriteBufferSize := Sz;
                 Sz := WrBuf.Position;
                 WrBuf.SetPointer(Pointer(CurBuffer + FWriteTailSize),
                                             FWriteBufferSize - FWriteTailSize);
                 WrBuf.Position := Sz;
               end else
                 FrameCanSend := false
             end;

             if FrameCanSend then
             begin
               fr := TWCHTTPRefProtoFrame(it.Value);
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

function TWCHTTPRefConnection.TryToIdleStep(const TS : Qword): Boolean;
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

function TWCHTTPRefConnection.ConnectionAvaible: Boolean;
begin
  Result := ConnectionState in [wcCONNECTED, wcHALFCLOSED];
end;

procedure TWCHTTPRefConnection.TryToConsumeFrames(const TS: Qword);
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

procedure TWCHTTPRefConnection.TryToSendFrames(const TS: Qword);
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

procedure TWCHTTPRefConnection.InitializeBuffers;
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

procedure TWCHTTPRefConnection.HoldDelayValue(aDelay: TThreadInteger);
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

procedure TWCHTTPRefConnection.RelaxDelayValue(aDelay: TThreadInteger);
begin
  aDelay.Lock;
  try
    if (aDelay.Value > 16) then aDelay.Value := 16 else
    aDelay.Value := aDelay.Value div 2;
  finally
    aDelay.UnLock;
  end;
end;

function TWCHTTPRefConnection.NextFrameToSend(it: TIteratorObject
  ): TIteratorObject;
begin
  Result := it;
end;

function TWCHTTPRefConnection.TruncReadBuffer(S : TBufferedStream) : Int64;
begin
  Result := S.Size - S.Position;
  if (Result > 0) and (S.Position > 0) then
     Move(PByte(ReadBuffer.Value)[S.Position], ReadBuffer.Value^, Result);
end;

function TWCHTTPRefConnection.ReadMore(Buffered : TBufferedStream;
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

function TWCHTTPRefConnection.LoadMoreData(
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
    WriteTo.SetPointer(ReadBuffer.Value, L);
    WriteTo.Position:= Offset;
  end;
end;

{ TWCHTTP2Connection }

function TWCHTTP2Connection.AddNewStream(aStreamID : Cardinal): TWCHTTPStream;
{$IFDEF DEBUG_STAT}
var R : Integer;
{$ENDIF}
begin
  {$IFDEF DEBUG_STAT}
  R := FStreams.Count;
  if R > DEBUG_GLOBALS_LONGWORD[DG_MAX_CONCURRENT_STREAMS] then
     DEBUG_GLOBALS_LONGWORD[DG_MAX_CONCURRENT_STREAMS] := R;
  {$ENDIF}
  Result := TWCHTTPStream.Create(Self, aStreamID);
  FStreams.Push_back(Result);
  FOwner.GarbageCollector.Add(Result);
end;

function TWCHTTP2Connection.GetConnSetting(id : Word): Cardinal;
begin
  Result := FConSettings[id];
end;

function TWCHTTP2Connection.GetHTTP2Settings: TWCHTTP2ServerSettings;
begin
  Result := TWCHTTP2RefConnections(FOwner).HTTP2Settings;
end;

constructor TWCHTTP2Connection.Create(aOwner: TWCHTTPRefConnections;
  aSocket: TWCHTTPSocketReference; aOpenningMode: THTTP2OpenMode;
  aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData);
var i, Sz : integer;
    CSet : PHTTP2SettingsPayload;
begin
  inherited Create(aOwner, aSocket, aSocketConsume, aSendData);
  FStreams := TWCHTTPStreams.Create;
  FLastStreamID := 0;
  FConSettings := TThreadSafeConnSettings.Create;
  for i := 1 to HTTP2_SETTINGS_MAX do
    FConSettings[i] := HTTP2_SET_INITIAL_VALUES[i];
  HTTP2Settings.Lock;
  try
    with HTTP2Settings do
    for i := 0 to Count-1 do
    begin
      FConSettings[Setting[i].Identifier] := Setting[i].Value;
    end;
  finally
    HTTP2Settings.UnLock;
  end;
  InitializeBuffers;
  FSendWindow := TThreadSafeWindowSize.Create(FConSettings[H2SET_INITIAL_WINDOW_SIZE]);
  FDataConsumed := 0;
  // send initial settings frame
  if aOpenningMode in [h2oUpgradeToH2C, h2oUpgradeToH2] then
    PushFrame(TWCHTTP2UpgradeResponseFrame.Create(aOpenningMode));
  Sz := HTTP2Settings.CopySettingsToMem(Cset);
  PushFrame(TWCHTTP2DataFrame.Create(H2FT_SETTINGS, nil, 0, CSet,  Sz));
end;

class function TWCHTTP2Connection.Protocol: TWCProtocolVersion;
begin
  Result := wcHTTP2;
end;

procedure TWCHTTP2Connection.ConsumeNextFrame(Mem: TBufferedStream);
var
  Sz, fallbackpos : Int64;
  err : Byte;
  Buffer : Pointer;
  FrameHeader : TWCHTTP2FrameHeader;
  S : TBufferedStream;
  Str, RemoteStr : TWCHTTPStream;

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
        if aDecoder.Malformed then
           Result := H2E_COMPRESSION_ERROR else
           Result := Strm.FinishHeaders(aDecoder);
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
    RemoteID, CV : Cardinal;
    WV: Word;
    SettFrame : THTTP2SettingsBlock;
    CurStreamClosed, Flag : Boolean;
begin
  Str := nil; RemoteStr := nil;
  if assigned(Mem) then begin
    if (Mem.Size - Mem.Position) = 0 then Exit;
  end else Exit;

  FReadBuffer.Lock;
  try
    FrameHeader := TWCHTTP2FrameHeader.Create;
    S := TBufferedStream.Create;
    try
      Sz := ReadBufferSize - ReadTailSize;
      if Sz <= 0 then
      begin
        err := H2E_READ_BUFFER_OVERFLOW;
        exit;
      end;
      Sz := ReadMore(Mem, FReadTailSize);
      if Sz = FReadTailSize then begin
        err := H2E_INTERNAL_ERROR;
        Exit;
      end;
      S.SetPointer(FReadBuffer.Value, Sz);

      err := H2E_NO_ERROR;
      while true do
      begin
        fallbackpos := S.Position;
        if not LoadMoreData(Mem, S, fallbackpos, H2P_FRAME_HEADER_SIZE, 0) then
        begin
          err := H2E_PARSE_ERROR;
          break;
        end;
        // read header
        FrameHeader.LoadFromStream(S);
        // find stream
        if assigned(Str) then Str.DecReference;
        if assigned(RemoteStr) then RemoteStr.DecReference;
        Str := nil;
        RemoteStr := nil;
        CurStreamClosed := false;

        if FrameHeader.StreamID > 0 then
        begin
          if not TWCHTTP2RefConnections(FOwner).CheckStreamID(FrameHeader.StreamID) then
          begin
            err := H2E_PROTOCOL_ERROR;
            break;
          end;
          if FrameHeader.StreamID <= FLastStreamID then
          begin
            Str := FStreams.GetByID(FrameHeader.StreamID);
            if not Assigned(Str) then
            begin
              CurStreamClosed := FStreams.IsStreamInClosedArch(FrameHeader.StreamID);
              if not CurStreamClosed then
              begin
                err := H2E_PROTOCOL_ERROR;
                break;
              end;
            end else
              CurStreamClosed := (Str.StreamState = h2ssCLOSED);
          end else begin
            FLastStreamID := FrameHeader.StreamID;
            if FrameHeader.FrameType in [H2FT_DATA, H2FT_HEADERS,
                                         H2FT_CONTINUATION] then
               FStreams.CloseOldIdleStreams(FLastStreamID);
            if (FStreams.Count >= FConSettings[H2SET_MAX_CONCURRENT_STREAMS]) then
            begin
              err := H2E_REFUSED_STREAM;
              break;
            end;
            Str := AddNewStream(FLastStreamID);
            Str.IncReference;
          end;
          if Assigned(Str) then Str.UpdateState(FrameHeader);
        end else
          Str := nil;

        if Assigned(Str) and
           Str.FWaitingForContinueFrame and
           (FrameHeader.FrameType <> H2FT_CONTINUATION) then
        begin
          err := H2E_PROTOCOL_ERROR;
          break;
        end;
        if (not Assigned(Str)) and
           (not CurStreamClosed) and
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
        if (Assigned(Str) or (FrameHeader.StreamID > 0)) and
           (FrameHeader.FrameType in [H2FT_PING,
                                      H2FT_SETTINGS,
                                      H2FT_GOAWAY]) then
        begin
          err := H2E_PROTOCOL_ERROR; // sec.6.5, 6.7
          break;
        end;

        if Http2IsFrameKnown(FrameHeader.FrameType) then
        begin
          if Assigned(Str) and
             (Str.FStreamState = h2ssIDLE) and
             not (FrameHeader.FrameType in [H2FT_HEADERS, H2FT_PRIORITY]) then
          begin
            err := H2E_PROTOCOL_ERROR; // sec.5.1
            break;
          end;
          if ((Assigned(Str) and (Str.FStreamState in [h2ssHLFCLOSEDRem])) or
              CurStreamClosed) and
             not (FrameHeader.FrameType in [H2FT_WINDOW_UPDATE,
                                            H2FT_PRIORITY,
                                            H2FT_RST_STREAM]) then
          begin
            if (FrameHeader.FrameType = H2FT_CONTINUATION) then
            begin
              if not (Assigned(Str) and Str.WaitingForContinueFrame) then
              begin
                err := H2E_PROTOCOL_ERROR; // sec.6.10
                break;
              end;
            end else
            begin
             err := H2E_STREAM_CLOSED; // sec.5.1
             break;
            end;
          end;
        end;

        Sz := FConSettings[H2SET_MAX_FRAME_SIZE];
        case FrameHeader.FrameType of
          H2FT_PING :
            Flag := FrameHeader.PayloadLength <> H2P_PING_SIZE;
          H2FT_WINDOW_UPDATE :
            Flag := FrameHeader.PayloadLength <> H2P_WINDOW_INC_SIZE;
          H2FT_RST_STREAM :
            Flag := FrameHeader.PayloadLength <> H2P_RST_STREAM_FRAME_SIZE;
          H2FT_PRIORITY :
            Flag := FrameHeader.PayloadLength <> H2P_PRIORITY_FRAME_SIZE;
          H2FT_SETTINGS :
            if (FrameHeader.FrameFlag and H2FL_ACK) > 0 then
              Flag := FrameHeader.PayloadLength > 0
            else
              Flag := (FrameHeader.PayloadLength mod H2P_SETTINGS_BLOCK_SIZE) > 0;
          H2FT_GOAWAY :
            Flag := FrameHeader.PayloadLength < H2P_GOAWAY_MIN_SIZE;
        else
          Flag := FrameHeader.PayloadLength > Sz;
        end;
        if Flag then
        begin
          err := H2E_FRAME_SIZE_ERROR;
          break;
        end;

        if not LoadMoreData(Mem, S, fallbackpos,
                                 FrameHeader.PayloadLength,
                                 H2P_FRAME_HEADER_SIZE) then
        begin
          err := H2E_PARSE_ERROR;
          break;
        end;
        if err = H2E_NO_ERROR then
        begin
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
                err := H2E_PROTOCOL_ERROR;
                break;
              end;
              Inc(FDataConsumed, FrameHeader.PayloadLength);
              //
              Str.PushData(Pointer(S.Memory + S.Position), DataSize);
              S.Position := S.Position + FrameHeader.PayloadLength;
              CheckStreamAfterState(Str);
            end;
            H2FT_HEADERS : begin
              if not (Str.StreamState in [h2ssIDLE,
                                          h2ssRESERVEDLoc,
                                          h2ssOPEN]) then
              begin
                err := H2E_STREAM_CLOSED;
                break;
              end;
              if Str.FHeadersComplete then
              begin
                err := H2E_PROTOCOL_ERROR;
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
                DataSize := DataSize - H2P_PRIORITY_FRAME_SIZE;
                if (Str.FParentStream = Str.ID) then
                begin
                  err := H2E_PROTOCOL_ERROR;
                  break;
                end;
                Str.ResetRecursivePriority;
              end;
              if DataSize < 0 then begin
                err := H2E_PROTOCOL_ERROR;
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
              S.Read(CV, H2P_STREAM_ID_SIZE);
              if Assigned(Str) then
              begin
                Str.FParentStream := BETON(CV) and H2P_STREAM_ID_MASK;
                if (Str.FParentStream = Str.ID) then
                begin
                  err := H2E_PROTOCOL_ERROR;
                  break;
                end;
              end;
              S.Read(B, H2P_PRIORITY_WEIGHT_SIZE);
              if Assigned(Str) then
              begin
                Str.FPriority := B;
                Str.ResetRecursivePriority;
              end;
            end;
            H2FT_RST_STREAM : begin
              S.Read(CV, H2P_ERROR_CODE_SIZE);
              if Assigned(Str) then
              begin
                Str.FFinishedCode := BETON(CV);
                Str.FStreamState := h2ssCLOSED;
              end;
            end;
            H2FT_SETTINGS : begin
              DataSize := FrameHeader.PayloadLength;

              while DataSize >= H2P_SETTINGS_BLOCK_SIZE do
              begin
                S.Read(SettFrame, H2P_SETTINGS_BLOCK_SIZE);
                WV := SettFrame.Identifier;
                if (WV >= 1) and
                   (WV <= HTTP2_SETTINGS_MAX) then
                begin
                  if FConSettings[WV] <> SettFrame.Value then
                  begin
                    case WV of
                      H2SET_HEADER_TABLE_SIZE,
                      H2SET_MAX_HEADER_LIST_SIZE: ResetHPack;
                      H2SET_INITIAL_WINDOW_SIZE : begin
                        if SettFrame.Value > HTTP2_MAX_WINDOW_UPDATE then
                        begin
                          err := H2E_FLOW_CONTROL_ERROR;
                          break;
                        end;
                        Streams.AdjustWindowSize(Int32(SettFrame.Value) -
                                                  Int32(FConSettings[WV]));
                      end;
                      H2SET_ENABLE_PUSH :
                        if SettFrame.Value > HTTP2_MAX_ENABLE_PUSH then
                        begin
                          err := H2E_PROTOCOL_ERROR;
                          break;
                        end;
                      H2SET_MAX_FRAME_SIZE :
                        if (SettFrame.Value < HTTP2_MIN_MAX_FRAME_SIZE) or
                           (SettFrame.Value > HTTP2_MAX_MAX_FRAME_SIZE) then
                        begin
                          err := H2E_PROTOCOL_ERROR;
                          break;
                        end;
                    end;
                    FConSettings[WV] := SettFrame.Value;
                  end;
                end;
                Dec(DataSize, H2P_SETTINGS_BLOCK_SIZE);
              end;

              // send ack settings frame
              if (FrameHeader.FrameFlag and H2FL_ACK) = 0 then
                PushFrame(H2FT_SETTINGS, Str, H2FL_ACK, nil, 0);
            end;
            H2FT_WINDOW_UPDATE : begin
              S.Read(DataSize, H2P_WINDOW_INC_SIZE);
              DataSize := BETON(DataSize);
              if DataSize = 0 then begin
                err := H2E_PROTOCOL_ERROR;
                break;
              end else
              if (DataSize < 0) then begin
                err := H2E_FLOW_CONTROL_ERROR;
                break;
              end else begin
                if assigned(Str) then begin
                  if (Int32(HTTP2_MAX_WINDOW_UPDATE) - Str.SendWindow.Size) < DataSize then
                  begin
                    err := H2E_FLOW_CONTROL_ERROR;
                    break;
                  end else
                    Str.SendWindow.Update(DataSize);
                end else begin
                  if (Int32(HTTP2_MAX_WINDOW_UPDATE) - FSendWindow.Size) < DataSize then
                  begin
                    err := H2E_FLOW_CONTROL_ERROR;
                    break;
                  end else
                    FSendWindow.Update(DataSize);
                end;
              end;
            end;
            H2FT_GOAWAY : begin
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
              Buffer := GetMem(H2P_PING_SIZE);
              //fill ping buffer
              S.Read(Buffer^, H2P_PING_SIZE);
              if (FrameHeader.FrameFlag and H2FL_ACK) = 0 then
                PushFrame(H2FT_PING, nil, H2FL_ACK, Buffer, H2P_PING_SIZE) else
                FreeMem(Buffer);
            end;
            else
            begin
              //Implementations MUST ignore and discard any frame that
              //has a type that is unknown. RFC 7540 4.1
            end;
          end;
        end;
        if err in [H2E_NO_ERROR, H2E_FLOW_CONTROL_ERROR] then
          S.Position := fallbackpos + H2P_FRAME_HEADER_SIZE + FrameHeader.PayloadLength;
        if (err <> H2E_NO_ERROR) or (S.Position >= S.Size) then
          break;
      end;

      if S.Position < S.Size then
      begin
        FReadTailSize := S.Size - S.Position;
        TruncReadBuffer(S);
      end else
        FReadTailSize := 0;

      if FDataConsumed >
         (HTTP2Settings.GetByID(H2SET_INITIAL_WINDOW_SIZE,
                                HTTP2_INITIAL_WINDOW_SIZE) div 10) then
      begin
        SendUpdateWindow(Str);
      end;
    finally
      S.Free;
      if not (err in [H2E_READ_BUFFER_OVERFLOW, H2E_PARSE_ERROR, H2E_NO_ERROR]) then
      begin
        if Assigned(Str) then
        begin
          Str.FFinishedCode := err;
          Str.Release;
          Str := nil;
          if not (err in [H2E_FLOW_CONTROL_ERROR]) then
          begin
            GoAway(err);
          end;
        end else
          GoAway(err);
      end;
      if assigned(RemoteStr) then RemoteStr.DecReference;
      if assigned(Str) then Str.DecReference;
      if assigned(FrameHeader) then FrameHeader.Free;
    end;
  finally
    FReadBuffer.UnLock;
  end;
end;

destructor TWCHTTP2Connection.Destroy;
begin
  FStreams.Free;
  ResetHPack;
  FConSettings.Free;
  if assigned(FErrorData) then FreeMem(FErrorData);
  FSendWindow.Free;
  inherited Destroy;
end;

procedure TWCHTTP2Connection.PushFrame(aFrameType: Byte; aStream: TWCHTTPStream;
  aFrameFlags: Byte; aData: Pointer; aDataSize: Cardinal; aOwnPayload: Boolean);
begin
  PushFrame(TWCHTTP2DataFrame.Create(aFrameType, aStream, aFrameFlags, aData,
                                             aDataSize, aOwnPayload));
end;

procedure TWCHTTP2Connection.PushFrame(aFrameType: Byte; aStream: TWCHTTPStream;
  aFrameFlags: Byte; aData: TReferencedStream; aStrmPos: Int64;
  aDataSize: Cardinal);
begin
  PushFrame(TWCHTTP2RefFrame.Create(aFrameType, aStream, aFrameFlags, aData,
                                             aStrmPos, aDataSize));
end;

procedure TWCHTTP2Connection.PushFrameFront(aFrameType: Byte;
  aStream: TWCHTTPStream; aFrameFlags: Byte; aData: Pointer;
  aDataSize: Cardinal; aOwnPayload: Boolean);
begin
  PushFrameFront(TWCHTTP2DataFrame.Create(aFrameType, aStream, aFrameFlags, aData,
                                             aDataSize, aOwnPayload));
end;

function TWCHTTP2Connection.PopRequestedStream: TWCHTTPStream;
begin
  Lock;
  try
    if ConnectionState = wcCONNECTED then
    begin
      Result := FStreams.GetNextStreamWithRequest;
    end else Result := nil;
  finally
    UnLock;
  end;
end;

function TWCHTTP2Connection.TryToIdleStep(const TS: Qword): Boolean;
begin
  Result:=inherited TryToIdleStep(TS);
  FStreams.RemoveClosedStreams;
end;

procedure TWCHTTP2Connection.ResetStream(aSID, aError: Cardinal);
var S : TWCHTTPStream;
begin
  S := FStreams.GetByID(aSID);
  if assigned(S) then
  begin
    S.FFinishedCode:=aError;
    S.Release;
  end;
end;

procedure TWCHTTP2Connection.GoAway(aError: Cardinal);
var Buffer : PHTTP2GoawayPayload;
begin
  //send error
  Buffer := GetMem(H2P_GOAWAY_MIN_SIZE);
  //fill goaway buffer
  Buffer^.LastStreamID := FLastStreamID;
  Buffer^.ErrorCode    := aError;
  try
    PushFrame(H2FT_GOAWAY, nil, 0, Buffer, H2P_GOAWAY_MIN_SIZE);
    ConnectionState := wcHALFCLOSED;
  except
    FreeMem(Buffer);
    raise;
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
     FOwner.GarbageCollector.Add(FHPackEncoder);
  end;
  if not assigned(FHPackDecoder) then begin
     FHPackDecoder :=
       TThreadSafeHPackDecoder.Create(ConnSettings[H2SET_MAX_HEADER_LIST_SIZE],
                            ConnSettings[H2SET_HEADER_TABLE_SIZE]);
     FOwner.GarbageCollector.Add(FHPackDecoder);
  end;
end;

function TWCHTTP2Connection.GetInitialReadBufferSize: Cardinal;
begin
  Result := FConSettings[H2SET_INITIAL_WINDOW_SIZE];
end;

function TWCHTTP2Connection.GetInitialWriteBufferSize: Cardinal;
begin
  Result := FConSettings[H2SET_INITIAL_WINDOW_SIZE];
end;

function TWCHTTP2Connection.CanExpandWriteBuffer({%H-}aCurSize,
  {%H-}aNeedSize: Cardinal): Boolean;
begin
  Result := false;
end;

function TWCHTTP2Connection.RequestsWaiting: Boolean;
begin
  Result :=  FStreams.HasStreamWithRequest;
end;

function TWCHTTP2Connection.NextFrameToSend(it : TIteratorObject): TIteratorObject;
var AvaibleSendWindow : Int32;

function CanSend(fr : TWCHTTP2Frame) : Boolean;
var nfr : TWCHTTP2RefFrame;
begin
  if fr.Header.FrameType in HTTP2_FLOW_CONTROL_FRAME_TYPES then
  begin
    if fr.Stream.FSendWindow.Blocked then Exit(false);
    AvaibleSendWindow := fr.Stream.FSendWindow.Size;
    if FSendWindow.Size < AvaibleSendWindow then
      AvaibleSendWindow := FSendWindow.Size;
    Result := (AvaibleSendWindow >= fr.Header.PayloadLength);
    if (not Result) and (fr is TWCHTTP2RefFrame) and
       (AvaibleSendWindow > 0) then
    begin
      nfr := TWCHTTP2RefFrame.Create(fr.Header.FrameType,
                                     fr.Stream,
                                     fr.Header.FrameFlag and (not H2FL_END_STREAM),
                                     TWCHTTP2RefFrame(fr).FStrm,
                                     TWCHTTP2RefFrame(fr).Fpos,
                                     AvaibleSendWindow);
      Inc(TWCHTTP2RefFrame(fr).Fpos, AvaibleSendWindow);
      Dec(fr.Header.PayloadLength, AvaibleSendWindow);
      it := FFramesToSend.InsertBefore(it, nfr);
      Result := true;
    end;
    if not Result then
      fr.Stream.FSendWindow.Block;
  end else Result := true;
end;

var Str : TWCHTTPStream;
begin
  while Assigned(it) do
  begin
    if not CanSend(TWCHTTP2Frame(it.Value)) then
    begin
      Str := TWCHTTP2Frame(it.Value).Stream;
      repeat
        it := it.Next;
        if Assigned(it) and (TWCHTTP2Frame(it.Value).Stream <> Str) then
        begin
          break;
        end;
      until not Assigned(it);
    end else
      Break;
  end;
  Result := it;
end;

procedure TWCHTTP2Connection.AfterFrameSent(fr: TWCHTTPRefProtoFrame);
begin
  if fr is TWCHTTP2Frame then
  begin
    if (TWCHTTP2Frame(fr).Header.FrameType in [H2FT_HEADERS,
                                               H2FT_DATA]) and
       ((TWCHTTP2Frame(fr).Header.FrameFlag and H2FL_END_STREAM) > 0) then
       TWCHTTP2Frame(fr).Stream.FStreamState := h2ssCLOSED;

    if TWCHTTP2Frame(fr).Header.FrameType in HTTP2_FLOW_CONTROL_FRAME_TYPES then
    begin
      if Assigned(TWCHTTP2Frame(fr).Stream) then
        TWCHTTP2Frame(fr).Stream.FSendWindow.Send(TWCHTTP2Frame(fr).Header.PayloadLength);
      FSendWindow.Send(TWCHTTP2Frame(fr).Header.PayloadLength);
    end;
  end;
end;

procedure TWCHTTP2Connection.SendUpdateWindow(Strm: TWCHTTPStream);
var pv : PHTTP2WindowUpdatePayload;
    vtosend : Int32;
begin
  if FDataConsumed > 0 then
  begin
    if FDataConsumed > HTTP2_MAX_WINDOW_UPDATE then
      vtosend:= HTTP2_MAX_WINDOW_UPDATE else
      vtosend:= FDataConsumed;
    pv := GetMem(H2P_WINDOW_INC_SIZE);
    pv^.WindowSize := vtosend;
    PushFrame(H2FT_WINDOW_UPDATE, nil, 0, pv, H2P_WINDOW_INC_SIZE);
    if Assigned(Strm) then begin
      pv := GetMem(H2P_WINDOW_INC_SIZE);
      pv^.WindowSize := vtosend;
      PushFrame(H2FT_WINDOW_UPDATE, Strm, 0, pv, H2P_WINDOW_INC_SIZE);
    end;
    Dec(FDataConsumed, vtosend);
  end;
end;

{ TWCHTTPStreams }

procedure TWCHTTPStreams.AddClosedStream(SID: Cardinal);
var it, nit : TIteratorObject;
begin
  FClosedStreams.Lock;
  try
    nit := nil;
    it := FClosedStreams.ListBegin;
    while Assigned(it) do
    begin
      if TWCClosedStreams(it.Value).Contain(SID) then
      begin
        Break;
      end;
      if TWCClosedStreams(it.Value).Expand(SID) then
      begin
        nit := it;
        Break;
      end;
      if TWCClosedStreams(it.Value).StartFrom > SID then
      begin
        nit := FClosedStreams.InsertBefore(it, TWCClosedStreams.Create(SID));
        break;
      end;
      it := it.Next;
    end;
    if Assigned(nit) then
    begin
      //one-direction merging pass
      it := FClosedStreams.ListBegin;
      while Assigned(it) do
      begin
        nit := it.Next;
        if Assigned(nit) then
        begin
          if TWCClosedStreams(it.Value).MergeRight(TWCClosedStreams(nit.Value)) then
          begin
            FClosedStreams.Erase(nit);
          end else
            it := nit;
        end else
          it := nil;
      end;
    end else
      FClosedStreams.Push_back(TWCClosedStreams.Create(SID));
  finally
    FClosedStreams.UnLock;
  end;
end;

function TWCHTTPStreams.IsStreamClosed(aStrm: TObject; {%H-}data: pointer): Boolean;
begin
  Result := (TWCHTTPStream(aStrm).StreamState = h2ssCLOSED);
end;

procedure TWCHTTPStreams.AfterStrmExtracted(aObj: TObject);
begin
  AddClosedStream(TWCHTTPStream(aObj).ID);
  TWCHTTPStream(aObj).DecReference;
end;

constructor TWCHTTPStreams.Create;
begin
  inherited Create;
  FClosedStreams := TThreadSafeFastSeq.Create;
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
  FClosedStreams.Free;
  inherited Destroy;
end;

function TWCHTTPStreams.IsStreamInClosedArch(SID: Cardinal): Boolean;
var it : TIteratorObject;
begin
  Result := false;
  FClosedStreams.Lock;
  try
    it := FClosedStreams.ListBegin;
    while Assigned(it) do
    begin
      if TWCClosedStreams(it.Value).Contain(SID) then
        Exit(True);

      it := it.Next;
    end;
  finally
    FClosedStreams.UnLock;
  end;
end;

function TWCHTTPStreams.GetByID(aID: Cardinal): TWCHTTPStream;
var P : TIteratorObject;
begin
  Result := nil;
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
        Result.ResponseProceed := true;
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
        TWCHTTPStream(P.Value).DecReference;
        Extract(P);
      end;
      P := NP;
    end;
  finally
    UnLock;
  end;
end;

procedure TWCHTTPStreams.AdjustWindowSize(Delta: Int32);
var P : TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCHTTPStream(P.Value).FSendWindow.Update(Delta);
      P := P.Next;
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

function TWCHTTPStream.GetCurResponse : TWCHTTP2Response;
begin
  Result := FCurRequest.Response;
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

function TWCHTTPStream.FinishHeaders(aDecoder: TThreadSafeHPackDecoder) : Byte;
var i : integer;
    p : PHPackHeaderTextItem;
    PseudoHeaders : Boolean;
    h2 : THTTP2Header;
    PHValues : THTTP2PseudoHeaders = ('', '', '', '', '');
begin
  Result := H2E_NO_ERROR;
  FHeadersComplete := true;
  //check headers
  //according RFC 7540 8.1. HTTP Request/Response Exchange
  //sec.8.1.2
  aDecoder.IncReference;
  aDecoder.Lock;
  try
    PseudoHeaders := true;
    for i := 0 to aDecoder.DecodedHeaders.Count-1 do
    begin
      P := aDecoder.DecodedHeaders[i];
      if not SameStr(LowerCase(P^.HeaderName), P^.HeaderName) then
      begin
        Result := H2E_PROTOCOL_ERROR;
        Exit;
      end;
      h2 := HTTP2HeaderType(P^.HeaderName);
      if HTTP2HeaderIsPseudo(h2) then
      begin
        if PseudoHeaders then begin
          if h2 in [hh2Method..hh2Status] then
          begin
            if Length(PHValues[h2]) > 0 then
            begin
              Result := H2E_PROTOCOL_ERROR;
              Exit;
            end else
            begin
              PHValues[h2] := P^.HeaderValue;
            end;
          end;
        end else
        begin
          Result := H2E_PROTOCOL_ERROR;
          Exit
        end;
      end else
      begin
        if P^.HeaderName[1] = ':' then
        begin
          Result := H2E_PROTOCOL_ERROR;
          Exit;
        end;
        if SameStr(p^.HeaderName, 'te') and
           (not SameStr(p^.HeaderValue, 'trailers')) then
        begin
          Result := H2E_PROTOCOL_ERROR;
          Exit;
        end;
        if PseudoHeaders then PseudoHeaders := false;
      end;
    end;

    //specific checks
    Result := TWCHTTP2RefConnections(FConnection.FOwner).CheckHeaders(aDecoder, PHValues);
  finally
    aDecoder.UnLock;
    aDecoder.DecReference;
  end;
  if Result = H2E_NO_ERROR then
    FCurRequest.CopyHeaders(aDecoder);
end;

constructor TWCHTTPStream.Create(aConnection: TWCHTTP2Connection;
  aStreamID: Cardinal);
begin
  inherited Create;
  FID := aStreamID;
  FConnection := aConnection;
  FStreamState:= h2ssIDLE;
  FRecursedPriority:=-1;
  FFinishedCode := H2E_NO_ERROR;
  FWaitingForContinueFrame := false;
  FWaitingRemoteStream := aStreamID;
  FHeadersComplete := false;
  FSendWindow := TThreadSafeWindowSize.Create(aConnection.ConnSettings[H2SET_INITIAL_WINDOW_SIZE]);
  FCurRequest := TWCHTTP2Request.Create(FConnection, Self);
  FResponseProceed := false;
end;

destructor TWCHTTPStream.Destroy;
begin
  if assigned(FCurRequest) then FreeAndNil(FCurRequest);
  FSendWindow.Free;
  inherited Destroy;
end;

procedure TWCHTTPStream.Release;
var er : PHTTP2RstStreamPayload;
begin
  if (FStreamState <> h2ssCLOSED) then
  begin
    if (FFinishedCode <> H2E_NO_ERROR) then
    begin
      er := GetMem(H2P_RST_STREAM_FRAME_SIZE);
      er^.ErrorCode := FFinishedCode;
      FConnection.PushFrame(H2FT_RST_STREAM, Self, 0, er, H2P_RST_STREAM_FRAME_SIZE);
      FStreamState := h2ssCLOSED;
    end else
    begin
      if not FCurRequest.ResponsePushed then // some error occured -
                              // stream released but no response frames pushed
                              // stream is zombie and need to be closed
      begin
        FStreamState := h2ssCLOSED;
      end;
    end;
  end;
  inherited Release;
end;

function TWCHTTPStream.GetReqContentStream: TStream;
begin
  if assigned(FCurRequest) then
  begin
    if FCurRequest.DataBlockSize > 0 then
    begin
     Result := FCurRequest.Data;
    end else
     Result := nil;
  end else
     Result := nil;
end;

function TWCHTTPStream.IsReqContentStreamOwn: Boolean;
begin
  Result := true;
end;

function TWCHTTPStream.RequestReady: Boolean;
begin
  Result := FCurRequest.Complete and
            FHeadersComplete and
            (not FResponseProceed);
end;

end.
