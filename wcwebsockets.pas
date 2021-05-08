{
 WCWebSockets:
   Classes and other routings to deal with WebSocket connections,
   frames plus cross-protocols conversions WS <-> HTTP1.1 for
   fpHTTP/fpweb compability

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcwebsockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wchttp2server, BufferedStream,
  ECommonObjs, OGLFastList,
  websocketconsts,
  fphttp, HTTPDefs;

type

  { TWebSocketUpgradeOptions }

  TWebSocketUpgradeOptions = class(TNetCustomLockedObject)
  private
    FInitialRequest : TRequest;
    FSecWebSocketAccept: AnsiString;
    FSecWebSocketProtocol: AnsiString;
    FSecWebSocketProtocol_Allow: AnsiString;
    FSecWebSocketKey: AnsiString;
    FSecWebSocketVersion: AnsiString;
    FSecWebSocketExts : AnsiString;
    FNumVersion : Word;
  public
    constructor Create(aRequest : TRequest; const aSubProtocol : String;
                                aCurVersion : Word);
    destructor Destroy; override;
    property Key: AnsiString read FSecWebSocketKey;
    property Accept: AnsiString read FSecWebSocketAccept;
    property ProtocolAllow: AnsiString read FSecWebSocketProtocol_Allow;
    property Protocol: AnsiString read FSecWebSocketProtocol;
    property Version: AnsiString read FSecWebSocketVersion;
    property Extensions: AnsiString read FSecWebSocketExts;
    property NumVersion : Word read FNumVersion;
    property Request : TRequest read FInitialRequest;
  end;

  { TWCWSUpgradeHandshakeFrame }

  TWCWSUpgradeHandshakeFrame = class(TWCHTTPRefProtoFrame)
  private
    FHandshakeStr : AnsiString;
  public
    constructor Create(aOptions : TWebSocketUpgradeOptions); overload;
    procedure SaveToStream(Str : TStream); override;
    function  Size : Int64; override;
  end;

  { TWCWSFrameHeader }

  TWCWSFrameHeader = class
  private
    FFin          : Boolean;
    FRSVMask      : Byte;
    FOpCode       : TWebSocketOpCode;
    FMask         : Boolean;
    FActHeaderSz  : Byte;
    FPayloadLen   : Int64;
    FMaskingKey   : Cardinal;
    FParseError   : Boolean;
    FHeaderToSend : PWebSocketFrameHeader;
    procedure PrepareToSend;
    procedure Initialize(aIsFin : Boolean;
                         aRSVMask : Byte;
                         aOpCode : TWebSocketOpCode;
                         aIsMasked : Boolean;
                         aPayloadSize : Int64;
                         aMaskingKey : Cardinal);
  public
    constructor Create; overload;
    constructor Create(Str : TStream); overload;
    constructor Create(aIsFin : Boolean;
                       aRSVMask : Byte;
                       aOpCode : TWebSocketOpCode;
                       aIsMasked : Boolean;
                       aPayloadSize : Int64;
                       aMaskingKey : Cardinal); overload;
    constructor Create(aOpCode : TWebSocketOpCode;
                       aPayloadSize : Int64); overload;
    procedure LoadFromStream(Str : TStream);
    procedure SaveToStream(Str : TStream);
    destructor Destroy; override;
    property OpCode : TWebSocketOpCode read FOpCode;
    property PayloadLength : Int64 read FPayloadLen;
    property ActualHeaderSize : Byte read FActHeaderSz;
    property MaskingKey : Cardinal read FMaskingKey;
    property IsMasked : Boolean read FMask;
    property IsFin : Boolean read FFin;
    property RSVMask : Byte read FRSVMask;
    property ParseError : Boolean read FParseError;
  end;

  { TWCWSFrameHeaderFrame }

  TWCWSFrameHeaderFrame = class(TWCHTTPRefProtoFrame)
  private
    FHeader : TWCWSFrameHeader;
  public
    constructor Create(aHeader : TWCWSFrameHeader);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function  Size : Int64; override;
  end;

  { TWCWSControlFrame }

  TWCWSControlFrame = class(TWCWSFrameHeaderFrame)
  private
    FBuffer     : Pointer;
    FBufferSize : Cardinal;
    FOwnBuffer  : Boolean;
  public
    constructor Create(aHeader : TWCWSFrameHeader;
                       aBuffer : Pointer;
                       aBufferSize : Cardinal;
                       aOwnBuffer : Boolean = true); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function  Size : Int64; override;
  end;

  { TWCWSFrameHolder }

  TWCWSFrameHolder = class(TWCWSFrameHeaderFrame)
  private
    FFrame     : TWCHTTPRefProtoFrame;
  public
    constructor Create(aHeader : TWCWSFrameHeader;
                       aFrame : TWCHTTPRefProtoFrame); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function  Size : Int64; override;
  end;

  { TWCWSChunck }

  TWCWSChunck = class(TWCRequestRefWrapper)
  private
    FOpCode     : TWebSocketOpCode;
    FExtensions : PWebSocketAppliedExts;
    FOptions    : TWebSocketUpgradeOptions;
  public
    constructor Create(aOptions : TWebSocketUpgradeOptions;
                       aOpCode : TWebSocketOpCode;
                       const Exts : Array of PWebSocketAppliedExt); virtual;
    destructor Destroy; override;
    function GetReqContentStream : TStream; override;
    function IsReqContentStreamOwn : Boolean; override;
    property OpCode : TWebSocketOpCode read FOpCode;
    property Options : TWebSocketUpgradeOptions read FOptions;
  end;

  { TWCWSIncomingChunck }

  TWCWSIncomingChunck = class(TWCWSChunck)
  private
    FData       : TMemoryStream;
    FComplete   : Boolean;
    function GetTotalSize: Int64;
  public
    constructor Create(aOptions : TWebSocketUpgradeOptions;
                       aOpCode : TWebSocketOpCode;
                       const Exts : Array of PWebSocketAppliedExt); override;
    destructor Destroy; override;
    function GetReqContentStream : TStream; override;
    function IsReqContentStreamOwn : Boolean; override;
    procedure Complete;
    property IsComplete : Boolean read FComplete;
    property TotalSize : Int64 read GetTotalSize;
    property Data : TMemoryStream read FData;
    procedure CopyToHTTP1Request(aReq1 : TRequest);
    procedure PushData(aBuffer: Pointer;
                       aSize: Cardinal;
                       aMasked : Boolean;
                       aMaskingKey : Cardinal); overload;
    function DataToUtf8String : String;
  end;

  { TWCWSIncomingChuncks }

  TWCWSIncomingChuncks = class(TThreadSafeFastSeq)
  public
    function  HasReadyChunck : Boolean;
    function  PopReadyChunck : TWCWSIncomingChunck;
    function  GetActualChunck : TWCWSIncomingChunck;
    procedure PushChunck(aChunck : TWCWSIncomingChunck);
    destructor Destroy; override;
  end;

  { TWCWebSocketConnection }

  TWCWebSocketConnection = class(TWCHTTPRefConnection)
  private
    FInChuncks  : TWCWSIncomingChuncks;
    FOptions    : TWebSocketUpgradeOptions;
    FLastError  : Word;
    function AddNewIncomingChunck(aOpCode : TWebSocketOpCode;
      const aExts : array of PWebSocketAppliedExt) : TWCWSIncomingChunck;
  protected
    function GetInitialReadBufferSize : Cardinal; override;
    function GetInitialWriteBufferSize : Cardinal; override;
    function CanExpandWriteBuffer({%H-}aCurSize, aNeedSize : Cardinal) : Boolean; override;
    function RequestsWaiting: Boolean; override;
    procedure AfterFrameSent({%H-}fr: TWCHTTPRefProtoFrame); override;
  public
    constructor Create(aOwner: TWCHTTPRefConnections;
        aOpenMode : TWebSocketUpgradeOptions;
        aSocket: TWCHTTPSocketReference;
        aSocketConsume: THttpRefSocketConsume; aSendData: THttpRefSendData); overload;
    destructor Destroy; override;
    procedure ConsumeNextFrame({%H-}Mem : TBufferedStream); override;
    procedure PushFrame(fr : TWCHTTPRefProtoFrame); override;
    procedure PushFrame(aOpCode: TWebSocketOpCode;
                        aBuffer: Pointer;
                        aSize: Cardinal;
                        aOwnBuffer: Boolean=true); overload;
    procedure Close(aError : Word; const aReason : UTF8String);
    function  PopReadyChunck : TWCWSIncomingChunck;
    property LastError : Word read FLastError;
    property Options : TWebSocketUpgradeOptions read FOptions;
    class function Protocol : TWCProtocolVersion; override;
    class function TryToUpgradeFromHTTP(aReq : TRequest) : TWebSocketUpgradeOptions; virtual;
  end;

var WCWS_MAX_PAYLOAD_SIZE : Int64 = $200000;

implementation

uses wcutils;

const
  WEBSOCKET_INITIAL_READ_BUFFER_SIZE  = $FFFF;
  WEBSOCKET_INITIAL_WRITE_BUFFER_SIZE = $FFFF;
  WEBSOCKET_MAX_WRITE_BUFFER_SIZE     = $9600000;

{ TWebSocketUpgradeOptions }

constructor TWebSocketUpgradeOptions.Create(aRequest : TRequest;
  const aSubProtocol : String; aCurVersion : Word);
begin
  inherited Create;
  FSecWebSocketProtocol_Allow := aRequest.GetCustomHeader(WS_Sec_WebSocket_Protocol);
  FSecWebSocketProtocol := aSubProtocol;
  FSecWebSocketKey := aRequest.GetCustomHeader(WS_Sec_WebSocket_Key);
  FSecWebSocketVersion := aRequest.GetCustomHeader(WS_Sec_WebSocket_Version);
  FNumVersion := aCurVersion;
  FSecWebSocketAccept := GetWebSocketAcceptKey(FSecWebSocketKey);
  //todo : implement extensions
  FSecWebSocketExts := '';//aReq.GetCustomHeader(WS_Sec_WebSocket_Extensions);
  FInitialRequest := TRequest.Create;
  CopyHTTPRequest(FInitialRequest, aRequest);
end;

destructor TWebSocketUpgradeOptions.Destroy;
begin
  if assigned(FInitialRequest) then FInitialRequest.Free;
  inherited Destroy;
end;

{ TWCWSFrameHolder }

constructor TWCWSFrameHolder.Create(aHeader: TWCWSFrameHeader;
  aFrame: TWCHTTPRefProtoFrame);
begin
  inherited Create(aHeader);
  FFrame := aFrame;
end;

destructor TWCWSFrameHolder.Destroy;
begin
  FFrame.Free;
  inherited Destroy;
end;

procedure TWCWSFrameHolder.SaveToStream(Str: TStream);
begin
  inherited SaveToStream(Str);
  FFrame.SaveToStream(Str);
end;

function TWCWSFrameHolder.Size: Int64;
begin
  Result:=(inherited Size) + FFrame.Size;
end;

{ TWCWSControlFrame }

constructor TWCWSControlFrame.Create(aHeader: TWCWSFrameHeader;
  aBuffer: Pointer; aBufferSize: Cardinal; aOwnBuffer: Boolean);
begin
  inherited Create(aHeader);
  FBuffer := aBuffer;
  FBufferSize:= aBufferSize;
  FOwnBuffer:= aOwnBuffer;
end;

destructor TWCWSControlFrame.Destroy;
begin
  if FOwnBuffer and assigned(FBuffer) then FreeMemAndNil(FBuffer);
  inherited Destroy;
end;

procedure TWCWSControlFrame.SaveToStream(Str: TStream);
begin
  inherited SaveToStream(Str);  //write header
  if assigned(FBuffer) then
    Str.WriteBuffer(FBuffer^, FBufferSize);
end;

function TWCWSControlFrame.Size: Int64;
begin
  Result:=(inherited Size) + FBufferSize;
end;

{ TWCWSUpgradeHandshakeFrame }

constructor TWCWSUpgradeHandshakeFrame.Create(aOptions: TWebSocketUpgradeOptions
  );
begin
  FHandshakeStr := Format(WebSocket_Handshake_Response, [aOptions.Accept,
                                                         aOptions.Protocol,
                                                         aOptions.Extensions]);
end;

procedure TWCWSUpgradeHandshakeFrame.SaveToStream(Str: TStream);
begin
  Str.WriteBuffer(FHandshakeStr[1], Size);
end;

function TWCWSUpgradeHandshakeFrame.Size: Int64;
begin
  Result := Length(FHandshakeStr);
end;

{ TWCWSChunck }

constructor TWCWSChunck.Create(aOptions : TWebSocketUpgradeOptions;
  aOpCode : TWebSocketOpCode; const Exts : array of PWebSocketAppliedExt);
begin
  inherited Create;
  FOptions := aOptions;
  FOpCode := aOpCode;
  FExtensions := nil;
end;

destructor TWCWSChunck.Destroy;
begin
  if Assigned(FExtensions) then FreeMemAndNil(FExtensions);
  inherited Destroy;
end;

function TWCWSChunck.GetReqContentStream: TStream;
begin
  Result := nil;
end;

function TWCWSChunck.IsReqContentStreamOwn: Boolean;
begin
  Result := false;
end;

{ TWCWSFrameHeader }

procedure TWCWSFrameHeader.PrepareToSend;
var
  pl, offs : Byte;
  w : word;
  i64 : int64;
  dw : Cardinal;
begin
  if not Assigned(FHeaderToSend) then
  begin
    FHeaderToSend := GetMem(WSP_MAX_FRAME_HEADER_SIZE);
    FActHeaderSz := WSP_MIN_FRAME_HEADER_SIZE;
    FHeaderToSend^[0] := ((Byte(FFin) shl 7) and $80) or
            ((FRSVMask shl 4) and $70) or
            ((FOpCode) and $0f);
    if FPayloadLen < 126 then
      pl := FPayloadLen
    else
    if FPayloadLen <= Int64(High(Word)) then
      pl := 126 else
      pl := 127;
    FHeaderToSend^[1] := ((Byte(FMask) shl 7) and $80) or
            (pl and $7f);

    if (pl = 126) then
    begin
      w := NtoBE(Word(FPayloadLen));
      Move(w, FHeaderToSend^[2], 2);
      offs := 4;
      Inc(FActHeaderSz, 2);
    end else
    if (pl = 127) then
    begin
      i64 := NtoBE(FPayloadLen);
      Move(i64, FHeaderToSend^[2], 8);
      offs := 10;
      Inc(FActHeaderSz, 8);
    end else
      offs := 2;
    if IsMasked then
    begin
      dw := NtoBE(FMaskingKey);
      Move(dw, FHeaderToSend^[offs], 4);
      Inc(FActHeaderSz, 4);
    end;
  end;
end;

procedure TWCWSFrameHeader.Initialize(aIsFin: Boolean; aRSVMask: Byte;
  aOpCode: TWebSocketOpCode; aIsMasked: Boolean; aPayloadSize: Int64;
  aMaskingKey: Cardinal);
begin
  FHeaderToSend := nil;
  FParseError := false;
  FFin:= aIsFin;
  FRSVMask:= aRSVMask;
  FOpCode:= aOpCode;
  FMask:= aIsMasked;
  FPayloadLen:= aPayloadSize;
  FMaskingKey:= aMaskingKey;
  PrepareToSend;
end;

constructor TWCWSFrameHeader.Create;
begin
  FHeaderToSend := nil;
  FParseError := true;
end;

constructor TWCWSFrameHeader.Create(Str: TStream);
begin
  FHeaderToSend := nil;
  LoadFromStream(Str);
end;

constructor TWCWSFrameHeader.Create(aIsFin: Boolean; aRSVMask: Byte;
  aOpCode: TWebSocketOpCode; aIsMasked: Boolean; aPayloadSize: Int64;
  aMaskingKey: Cardinal);
begin
  Initialize(aIsFin, aRSVMask, aOpCode, aIsMasked, aPayloadSize, aMaskingKey);
end;

constructor TWCWSFrameHeader.Create(aOpCode: TWebSocketOpCode;
  aPayloadSize: Int64);
begin
  Initialize(true, 0, aOpCode, false, aPayloadSize, 0);
end;

procedure TWCWSFrameHeader.LoadFromStream(Str: TStream);
var
  w  : word;
begin
  FParseError := false;
  FActHeaderSz := WSP_MIN_FRAME_HEADER_SIZE;
  Str.Read(w, WSP_MIN_FRAME_HEADER_SIZE);
  FFin := (w and $80) > 0;
  FRSVMask := (w and $70) shr 4;
  FOpCode := (w and $0F);
  FMask := (w and $8000) > 0;
  FPayloadLen := (w and $7F00) shr 8;
  if FMask then Inc(FActHeaderSz, WSP_MASKING_KEY_SIZE);
  if FPayloadLen = 126 then
    Inc(FActHeaderSz, 2) else
  if FPayloadLen = 127 then
    Inc(FActHeaderSz, 8);
  if ((Str.Position + FActHeaderSz - WSP_MIN_FRAME_HEADER_SIZE) > Str.Size) then
  begin
    FParseError := true;
    Exit;
  end;
  if FPayloadLen = 126 then
  begin
    Str.Read(w, sizeof(word));
    FPayloadLen := BEtoN(w);
  end else
  if FPayloadLen = 127 then
  begin
    Str.Read(FPayloadLen, sizeof(Int64));
    FPayloadLen := BEtoN(FPayloadLen);
  end;
  if FMask then
  begin
    Str.Read(FMaskingKey, Sizeof(Cardinal));
    FMaskingKey := BEtoN(FMaskingKey);
  end;
end;

procedure TWCWSFrameHeader.SaveToStream(Str: TStream);
begin
  PrepareToSend;
  Str.Write(FHeaderToSend^, FActHeaderSz);
end;

destructor TWCWSFrameHeader.Destroy;
begin
  if assigned(FHeaderToSend) then FreeMemAndNil(FHeaderToSend);
  inherited Destroy;
end;

{ TWCWSFrameHeaderFrame }

constructor TWCWSFrameHeaderFrame.Create(aHeader: TWCWSFrameHeader);
begin
  FHeader := aHeader;
end;

destructor TWCWSFrameHeaderFrame.Destroy;
begin
  FHeader.Free;
  inherited Destroy;
end;

procedure TWCWSFrameHeaderFrame.SaveToStream(Str: TStream);
begin
  FHeader.SaveToStream(Str);
end;

function TWCWSFrameHeaderFrame.Size: Int64;
begin
  Result := FHeader.ActualHeaderSize;
end;

{ TWCWSIncomingChunck }

function TWCWSIncomingChunck.GetTotalSize: Int64;
begin
  Result := Data.Size;
end;

constructor TWCWSIncomingChunck.Create(aOptions : TWebSocketUpgradeOptions;
  aOpCode : TWebSocketOpCode; const Exts : array of PWebSocketAppliedExt);
begin
  inherited Create(aOptions, aOpCode, Exts);
  FData := TMemoryStream.Create;
end;

destructor TWCWSIncomingChunck.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TWCWSIncomingChunck.GetReqContentStream: TStream;
begin
  Result:=FData;
end;

function TWCWSIncomingChunck.IsReqContentStreamOwn: Boolean;
begin
  Result:=true;
end;

procedure TWCWSIncomingChunck.Complete;
begin
  FComplete := true;
end;

procedure TWCWSIncomingChunck.CopyToHTTP1Request(aReq1 : TRequest);
begin
  aReq1.Method := 'GET';
  Options.Lock;
  try
    CopyHTTPRequest(aReq1, Options.Request);
  finally
    Options.UnLock;
  end;
  aReq1.ContentLength := TotalSize;
end;

procedure TWCWSIncomingChunck.PushData(aBuffer: Pointer; aSize: Cardinal;
  aMasked: Boolean; aMaskingKey: Cardinal);
var rs : integer;
    {$IFDEF CPU64}
    BEMaskKeyQ : QWord;
    {$ENDIF}
    Loc, WrLoc : Pointer;
    BEMaskKey : Cardinal;
begin
  if aMasked then
  begin
    FData.SetSize(FData.Position + aSize);
    rs := aSize;
    BEMaskKey := NtoBE(aMaskingKey);
    Loc := aBuffer;
    WrLoc := Pointer(FData.Memory + FData.Position);
    {$IFDEF CPU64}
    BEMaskKeyQ := QWord(BEMaskKey) or ((QWord(BEMaskKey) shl 32) and $ffffffff00000000);
    while rs > 7 do
    begin
      PQWord(wrloc)^ := PQWord(loc)^ xor BEMaskKeyQ;
      Inc(PQWord(wrloc));
      Inc(PQWord(loc));
      Dec(rs, 8);
    end;
    {$ENDIF}
    while rs > 3 do
    begin
      PCardinal(wrloc)^ := PCardinal(loc)^ xor BEMaskKey;
      Inc(PCardinal(wrloc));
      Inc(PCardinal(loc));
      Dec(rs, 4);
    end;
    while rs > 0 do
    begin
      Dec(rs);
      PByte(wrloc)^ := PByte(loc)^ xor ((BEMaskKey shr (rs shl 3)) and $FF);
      Inc(PByte(wrloc));
      Inc(PByte(loc));
    end;
    FData.Position := FData.Position + aSize;
  end else
    FData.WriteBuffer(aBuffer^, aSize);
end;

function TWCWSIncomingChunck.DataToUtf8String : String;
begin
  SetLength(Result, FData.Size);
  Move(FData.Memory^, Result[1], FData.Size);
end;

{ TWCWSIncomingChuncks }

function TWCWSIncomingChuncks.HasReadyChunck : Boolean;
var
  it : TIteratorObject;
begin
  Lock;
  try
    It := ListBegin;
    while Assigned(It) do
    begin
     if TWCWSIncomingChunck(It.Value).IsComplete then
     begin
       Result := true;
       Exit;
     end;
     It := It.Next;
    end;
    Result := false;
  finally
    UnLock;
  end;
end;

function TWCWSIncomingChuncks.PopReadyChunck: TWCWSIncomingChunck;
var
  it : TIteratorObject;
begin
  Lock;
  try
    It := ListBegin;
    while Assigned(It) do
    begin
     if TWCWSIncomingChunck(It.Value).IsComplete then
     begin
       Result := TWCWSIncomingChunck(It.Value);
       Result.DecReference;
       Extract(It);
       Exit;
     end;
     It := It.Next;
    end;
    Result := nil;
  finally
    UnLock;
  end;
end;

function TWCWSIncomingChuncks.GetActualChunck: TWCWSIncomingChunck;
var
  it : TIteratorObject;
begin
  Lock;
  try
    It := ListBegin;
    while Assigned(It) do
    begin
     if not TWCWSIncomingChunck(It.Value).IsComplete then
     begin
       Result := TWCWSIncomingChunck(It.Value);
       Result.IncReference;
       Exit;
     end;
     It := It.Next;
    end;
    Result := nil;
  finally
    UnLock;
  end;
end;

procedure TWCWSIncomingChuncks.PushChunck(aChunck: TWCWSIncomingChunck);
begin
  Push_back(aChunck);
end;

destructor TWCWSIncomingChuncks.Destroy;
var P :TIteratorObject;
begin
  Lock;
  try
    P := ListBegin;
    while assigned(P) do
    begin
      TWCWSIncomingChunck(P.Value).DecReference;
      P := P.Next;
    end;
    ExtractAll;
  finally
    UnLock;
  end;
  inherited Destroy;
end;

{ TWCWebSocketConnection }

function TWCWebSocketConnection.GetInitialReadBufferSize : Cardinal;
begin
  Result := WEBSOCKET_INITIAL_READ_BUFFER_SIZE;
end;

function TWCWebSocketConnection.GetInitialWriteBufferSize : Cardinal;
begin
  Result := WEBSOCKET_INITIAL_WRITE_BUFFER_SIZE;
end;

function TWCWebSocketConnection.CanExpandWriteBuffer(aCurSize,
  aNeedSize : Cardinal) : Boolean;
begin
  Result := aNeedSize < WEBSOCKET_MAX_WRITE_BUFFER_SIZE;
end;

function TWCWebSocketConnection.RequestsWaiting : Boolean;
begin
  Result := FInChuncks.HasReadyChunck;
end;

procedure TWCWebSocketConnection.AfterFrameSent(fr : TWCHTTPRefProtoFrame);
begin
  // do nothing
end;

function TWCWebSocketConnection.AddNewIncomingChunck(aOpCode : TWebSocketOpCode;
  const aExts : Array of PWebSocketAppliedExt) : TWCWSIncomingChunck;
begin
  Result := TWCWSIncomingChunck.Create(FOptions, aOpCode, aExts);
  Result.IncReference;
  FInChuncks.PushChunck(Result);
  Owner.GarbageCollector.Add(Result);
end;

constructor TWCWebSocketConnection.Create(aOwner : TWCHTTPRefConnections;
  aOpenMode : TWebSocketUpgradeOptions; aSocket : TWCHTTPSocketReference;
  aSocketConsume : THttpRefSocketConsume; aSendData : THttpRefSendData);
begin
  inherited Create(aOwner, aSocket, aSocketConsume, aSendData);
  InitializeBuffers;

  FOptions := aOpenMode;
  FInChuncks := TWCWSIncomingChuncks.Create;

  {send handshake}
  if assigned(aOpenMode) then begin
    PushFrame(TWCWSUpgradeHandshakeFrame.Create(FOptions));
  end;
end;

destructor TWCWebSocketConnection.Destroy;
begin
  FInChuncks.Free;
  if assigned(FOptions) then FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TWCWebSocketConnection.ConsumeNextFrame(Mem : TBufferedStream);
var
  Sz, fallbackpos: Int64;
  err : Word;
  Buffer : Pointer;
  FrameHeader : TWCWSFrameHeader;
  S : TBufferedStream;
  ActualChunck : TWCWSIncomingChunck;
  Flag : Boolean;

var DataSize : Integer;
begin
  ActualChunck := nil;
  if assigned(Mem) then begin
    if (Mem.Size - Mem.Position) = 0 then Exit;
  end else Exit;

  ReadBuffer.Lock;
  try
    FrameHeader := TWCWSFrameHeader.Create;
    S := TBufferedStream.Create;
    try
      Sz := ReadBufferSize - ReadTailSize;
      if Sz <= 0 then
      begin
        err := WSE_MESSAGE_TOO_BIG;
        exit;
      end;
      Sz := ReadMore(Mem, ReadTailSize);
      if Sz = ReadTailSize then begin
        err := WSE_UNEXPECTED;
        Exit;
      end;
      S.SetPointer(ReadBuffer.Value, Sz);

      err := WSE_NO_ERROR;
      while true do
      begin
        fallbackpos := S.Position;
        if not LoadMoreData(Mem, S, fallbackpos, WSP_MIN_FRAME_HEADER_SIZE, 0) then
        begin
          err := WSE_PROTOCOL_ERROR;
          break;
        end;
        // read header
        FrameHeader.LoadFromStream(S);
        // check error here: is header loaded fully?
        if FrameHeader.ParseError then
        begin
          if not LoadMoreData(Mem, S, fallbackpos, FrameHeader.ActualHeaderSize, 0) then
          begin
            err := WSE_PARSE_ERROR;
            break;
          end else
          begin
            FrameHeader.LoadFromStream(S);
            if FrameHeader.ParseError then
            begin
              err := WSE_PARSE_ERROR;
              break;
            end;
          end;
        end;

        if not WebSocketIsFrameKnown(FOptions.NumVersion,
                                     FrameHeader.OpCode) then
        begin
          err := WSE_NON_ACCEPTABLE_DATA;
          break;
        end;

        if assigned(ActualChunck) then ActualChunck.DecReference;
        ActualChunck := FInChuncks.GetActualChunck;

        if (not Assigned(ActualChunck)) and
           (FrameHeader.OpCode = WSP_OPCODE_CONTINUE) then
        begin
          err := WSE_PROTOCOL_ERROR;
          break;
        end;

        if (Assigned(ActualChunck)) and
           (FrameHeader.OpCode > WSP_OPCODE_CONTINUE) then
        begin
          err := WSE_PROTOCOL_ERROR;
          break;
        end;

        case FrameHeader.OpCode of
          WSP_OPCODE_CLOSE :
            Flag := FrameHeader.PayloadLength < WSP_CLOSE_MIN_SIZE;
        else
          Flag := false;
        end;
        if Flag then
        begin
          err := WSE_PROTOCOL_ERROR;
          break;
        end;

        if FrameHeader.PayloadLength > WCWS_MAX_PAYLOAD_SIZE then
        begin
          err := WSE_MESSAGE_TOO_BIG;
          break;
        end;

        if not LoadMoreData(Mem, S, fallbackpos,
                            FrameHeader.PayloadLength,
                            FrameHeader.ActualHeaderSize) then
        begin
          err := WSE_PARSE_ERROR;
          break;
        end;

        if err = WSE_NO_ERROR then
        begin
          // payload fully loaded
          case FrameHeader.OpCode of
            WSP_OPCODE_TEXT,
            WSP_OPCODE_BINARY : begin
              ActualChunck := AddNewIncomingChunck(FrameHeader.OpCode, []);
              ActualChunck.IncReference;
              ActualChunck.PushData(Pointer(S.Memory + S.Position),
                                    FrameHeader.PayloadLength,
                                    FrameHeader.IsMasked,
                                    FrameHeader.MaskingKey);
              if FrameHeader.IsFin then begin
                ActualChunck.Complete;
                ActualChunck.DecReference;
                ActualChunck := nil;
              end;
            end;
            WSP_OPCODE_CONTINUE : begin
              DataSize := FrameHeader.PayloadLength;
              ActualChunck.PushData(Pointer(S.Memory + S.Position), DataSize,
                                    FrameHeader.IsMasked,
                                    FrameHeader.MaskingKey);
              if FrameHeader.IsFin then begin
                ActualChunck.Complete;
                ActualChunck.DecReference;
                ActualChunck := nil;
              end;
            end;
            WSP_OPCODE_CLOSE : begin
              S.Read(FLastError, WSP_CLOSE_MIN_SIZE);
              FLastError := BETON(FLastError);
              if FrameHeader.PayloadLength > WSP_CLOSE_MIN_SIZE then begin
                {FErrorDataSize := FrameHeader.PayloadLength - WSP_CLOSE_MIN_SIZE;
                 if assigned(FErrorData) then
                    FErrorData := ReallocMem(FErrorData, FErrorDataSize) else
                    FErrorData := GetMem(FErrorDataSize);
                 S.Read(FErrorData^, FErrorDataSize);}
              end;
              // drop down connection
              ConnectionState := wcDROPPED;
              break;
            end;
            WSP_OPCODE_PING : begin
              Buffer := GetMem(FrameHeader.PayloadLength);
              //fill ping buffer
              S.Read(Buffer^, FrameHeader.PayloadLength);
              PushFrame(WSP_OPCODE_PONG, Buffer, FrameHeader.PayloadLength);
            end;
            else
            begin
              //Ignore
            end;
          end;
        end;
        if err = WSE_NO_ERROR then
          S.Position := fallbackpos + FrameHeader.ActualHeaderSize + FrameHeader.PayloadLength;
        if (err <> WSE_NO_ERROR) or (S.Position >= S.Size) then
          break;
      end;

      if S.Position < S.Size then
      begin
        ReadTailSize := S.Size - S.Position;
        TruncReadBuffer(S);
      end else
        ReadTailSize := 0;
    finally
      S.Free;
      if (err <> WSE_PARSE_ERROR) and (err <> WSE_NO_ERROR) then
      begin
        Close(err, '');
      end;
      if assigned(ActualChunck) then ActualChunck.DecReference;
      if assigned(FrameHeader) then FrameHeader.Free;
    end;
  finally
    ReadBuffer.UnLock;
  end;
end;

procedure TWCWebSocketConnection.PushFrame(fr: TWCHTTPRefProtoFrame);
var hfr : TWCWSFrameHeaderFrame;
    aOpCode : TWebSocketOpCode;
begin
  //push header frame first, then payload
  if (fr is TWCWSFrameHeaderFrame) or (fr is TWCWSUpgradeHandshakeFrame) then
    inherited PushFrame(fr) else
  begin
    if (fr is TWCHTTPStringFrame) or
       (fr is TWCHTTPStringsFrame) then
      aOpCode := WSP_OPCODE_TEXT else
      aOpCode := WSP_OPCODE_BINARY;
    hfr := TWCWSFrameHolder.Create(TWCWSFrameHeader.Create(aOpCode, fr.Size), fr);
    inherited PushFrame(hfr);
  end;
end;

procedure TWCWebSocketConnection.PushFrame(aOpCode: TWebSocketOpCode;
  aBuffer: Pointer; aSize: Cardinal; aOwnBuffer : Boolean = true);
begin
  PushFrame(TWCWSControlFrame.Create(TWCWSFrameHeader.Create(aOpCode, aSize),
                                     aBuffer, aSize, aOwnBuffer));
end;

procedure TWCWebSocketConnection.Close(aError : Word; const aReason : UTF8String
  );
var Buffer : PWSClosePayload;
    PayloadLen : Cardinal;
begin
  //send error
  PayloadLen := WSP_CLOSE_MIN_SIZE + Length(aReason);
  Buffer := GetMem(PayloadLen);
  //fill close buffer
  Buffer^.ErrorCode    := aError;
  if Length(aReason) > 0 then
    Move(aReason[1], PByte(Buffer)[3], Length(aReason));
  try
    PushFrame(WSP_OPCODE_CLOSE, Buffer, PayloadLen);
    ConnectionState := wcHALFCLOSED;
  except
    FreeMem(Buffer);
    raise;
  end;
end;

function TWCWebSocketConnection.PopReadyChunck : TWCWSIncomingChunck;
begin
  Result := FInChuncks.PopReadyChunck;
end;

class function TWCWebSocketConnection.Protocol : TWCProtocolVersion;
begin
  Result := wcWebSocket;
end;

class function TWCWebSocketConnection.TryToUpgradeFromHTTP(aReq : TRequest
  ) : TWebSocketUpgradeOptions;
var subproto : ansistring;
    cur_version : word;
begin
  Result := nil;
  if not SameStr(aReq.Method, 'GET') then
    Exit;
  if not (SameStr(aReq.GetCustomHeader('Upgrade'), WebSocket_Protocol) or
          SameStr(aReq.Upgrade, WebSocket_Protocol)) then
    Exit;
  if not WebSocketCheckVersionValid(aReq.GetCustomHeader(WS_Sec_WebSocket_Version),
                                    cur_version) then
    Exit;
  if Length(aReq.GetCustomHeader(WS_Sec_WebSocket_Key)) < WSP_MIN_KEY_LEN then
    Exit;

  //todo: create handler to determine protocol subversion from selector
  subproto := 'chat';

  Result := TWebSocketUpgradeOptions.Create(aReq, subproto, cur_version);
end;

end.

