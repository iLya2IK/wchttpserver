{
 wcWebsocketCon:
   Classes and other routings to deal with WebSocket connections,
   frames plus cross-protocols conversions WS <-> HTTP1.1 for
   fpHTTP/fpweb compability

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcWebsocketCon;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef wiki_docs}
  commonutils,fpcweb,
  {$endif}
  Classes, SysUtils,
  wcNetworking,
  BufferedStream, ExtMemoryStream,
  ECommonObjs, OGLFastList,
  WebsocketConsts,
  fphttp, HTTPDefs, HTTP1Utils, httpprotocol,
  zlib, fpjson;

type

  TWCWebSocketSettings = class;
  TWCWebSocketConnection = class;

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
    FOfferExts, FResponseExts : PWebSocketExts;
    FDeflateExt : PWebSocketExt;
    function GetOfferExt(index : integer): PWebSocketExt;
    function GetOfferExtByName(const index : ansistring) : PWebSocketExt;
    function GetOfferExtCount: Integer;
    function GetResponseExt(index : integer): PWebSocketExt;
    function GetResponseExtByName(const index : ansistring) : PWebSocketExt;
    function GetResponseExtCount: Integer;
  public
    constructor Create(aRequest : TRequest; const aSubProtocol : String;
                                aCurVersion : Word;
                                aSettings : TWCWebSocketSettings);
    destructor Destroy; override;
    property Key: AnsiString read FSecWebSocketKey;
    property Accept: AnsiString read FSecWebSocketAccept;
    property ProtocolAllow: AnsiString read FSecWebSocketProtocol_Allow;
    property Protocol: AnsiString read FSecWebSocketProtocol;
    property Version: AnsiString read FSecWebSocketVersion;
    property Extensions: AnsiString read FSecWebSocketExts;
    property NumVersion : Word read FNumVersion;
    property Request : TRequest read FInitialRequest;
    property OfferExtCount : Integer read GetOfferExtCount;
    property OfferExt[index : integer] : PWebSocketExt read GetOfferExt;
    property OfferExtByName[const index : ansistring] : PWebSocketExt read GetOfferExtByName;
    property ResponseExtCount : Integer read GetResponseExtCount;
    property ResponseExt[index : integer] : PWebSocketExt read GetResponseExt;
    property ResponseExtByName[const index : ansistring] : PWebSocketExt read GetResponseExtByName;
    //
    property DeflateExt : PWebSocketExt read FDeflateExt;
  end;

  { TWCWSUpgradeHandshakeFrame }

  TWCWSUpgradeHandshakeFrame = class(TWCRefProtoFrame)
  private
    FHandshakeStr : AnsiString;
  public
    constructor Create(aOptions : TWebSocketUpgradeOptions); overload;
    procedure SaveToStream(Str : TStream); override;
    function  Size : Int64; override;
    function  Memory : Pointer; override;
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

  TWCWSFrameHeaderFrame = class(TWCRefProtoFrame)
  private
    FHeader : TWCWSFrameHeader;
  public
    constructor Create(aHeader : TWCWSFrameHeader);
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function  Memory : Pointer; override;
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
    function  Memory : Pointer; override;
    function  Size : Int64; override;
  end;

  { TWCWSFrameHolder }

  TWCWSFrameHolder = class(TWCWSFrameHeaderFrame)
  private
    FFrame     : TWCRefProtoFrame;
  public
    constructor Create(aHeader : TWCWSFrameHeader;
                       aFrame : TWCRefProtoFrame); overload;
    destructor Destroy; override;
    procedure SaveToStream(Str : TStream); override;
    function  Memory : Pointer; override;
    function  Size : Int64; override;
  end;

  { TWCWSChunck }

  TWCWSChunck = class(TWCRequestRefWrapper)
  private
    FOpCode     : TWebSocketOpCode;
    FRSV        : Byte;
    FConnection : TWCWebSocketConnection;
    function GetOptions : TWebSocketUpgradeOptions;
  public
    constructor Create(aConn : TWCWebSocketConnection;
                       aOpCode : TWebSocketOpCode;
                       aRSV : Byte); virtual;
    destructor Destroy; override;
    function GetReqContentStream : TStream; override;
    function IsReqContentStreamOwn : Boolean; override;
    property OpCode : TWebSocketOpCode read FOpCode;
    property RSV : Byte read FRSV;
    property Options : TWebSocketUpgradeOptions read GetOptions;
    property Connection : TWCWebSocketConnection read FConnection;
  end;

  { TWCWSIncomingChunck }

  TWCWSIncomingChunck = class(TWCWSChunck)
  private
    FData       : TExtMemoryStream;
    FComplete   : Boolean;
    function GetTotalSize: Int64;
  public
    constructor Create(aConn : TWCWebSocketConnection;
                       aOpCode : TWebSocketOpCode;
                       aRSV : Byte); override;
    destructor Destroy; override;
    function GetReqContentStream : TStream; override;
    function IsReqContentStreamOwn : Boolean; override;
    procedure Complete;
    property IsComplete : Boolean read FComplete;
    property TotalSize : Int64 read GetTotalSize;
    property Data : TExtMemoryStream read FData;
    procedure CopyToHTTP1Request(aReq1 : TWCConnectionRequest); override;
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

  { TWCWebSocketSettings }

  TWCWebSocketSettings = class(TNetCustomLockedObject)
  private
    FSubProtocols : TThreadStringList;
    function GetSubProtocols : String;
    procedure SetSubProtocols(const AValue : String);
  public
    constructor Create;
    destructor Destroy; override;

    function  DefineSubProtocol(const ClientsList : AnsiString) : AnsiString;

    procedure ConfigExtFromJson(const aExt : AnsiString; obj : TJSONObject);
    property  SubProtocols : String read GetSubProtocols write SetSubProtocols;
  end;

  { TWCWebSocketHelper }

  TWCWebSocketHelper = class(TWCProtocolHelper)
  private
    FSettings : TWCWebSocketSettings;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Settings : TWCWebSocketSettings read FSettings;
  end;

  TWCWebSocketServerHelper = class(TWCWebSocketHelper)
  end;

  { TThreadSafeZlibStream }

  TThreadSafeZlibStream = class(TNetCustomLockedObject)
  private
    FStream       : z_streamp;
    FWindowSize   : Smallint;
    FBufferSize   : PtrInt;
    FBuffer       : PByte;
    FbufferInc    : PtrInt;
    FEnabled      : Boolean;
    function GetInitialized : Boolean;
  public
    constructor Create; overload;
    function Init(windowsize : SmallInt) : Integer; virtual; abstract;
    function Next(aBuffer : Pointer; Sz : PtrInt;
                       out NewCount : PtrInt) : Integer; virtual; abstract;
    procedure  Finish; virtual; abstract;
    property Buffer : PByte read FBuffer;
    property Initialized : Boolean read GetInitialized;
    property Enabled : Boolean read FEnabled write FEnabled;
    destructor Destroy; override;
  end;

  { TThreadSafeInflate }

  TThreadSafeInflate = class(TThreadSafeZlibStream)
  public
    function Init(windowsize : SmallInt) : Integer; override;
    function Next(aBuffer : Pointer; Sz : PtrInt;
                       out NewCount : PtrInt) : Integer; override;
    procedure Finish; override;
  end;

  { TThreadSafeDeflate }

  TThreadSafeDeflate = class(TThreadSafeZlibStream)
  public
    function Init(windowsize : SmallInt) : Integer; override;
    function Next(aBuffer : Pointer; Sz : PtrInt;
                       out NewCount : PtrInt) : Integer; override;
    procedure Finish; override;
  end;

  { TWCWebSocketConnection }

  TWCWebSocketConnection = class(TWCRefConnection)
  private
    FInChuncks     : TWCWSIncomingChuncks;
    FOptions       : TWebSocketUpgradeOptions;
    FInflateStream : TThreadSafeInflate;
    FDeflateStream : TThreadSafeDeflate;
    function AddNewIncomingChunck(aOpCode : TWebSocketOpCode;
                                  aRSV : Byte) : TWCWSIncomingChunck;
    function GetWebSocketSettings: TWCWebSocketSettings;
    function InitZLibStream(Strm : TThreadSafeZlibStream; aExt : PWebSocketExt
      ) : Integer;
    procedure DecodeStream(aStream : TExtMemoryStream);
    procedure EncodeFramePayload(aFrame : TWCWSFrameHolder);
  protected
    function GetInitialReadBufferSize : Cardinal; override;
    function GetInitialWriteBufferSize : Cardinal; override;
    function CanExpandWriteBuffer({%H-}aCurSize, aNeedSize : Cardinal) : Boolean; override;
    function RequestsWaiting: Boolean; override;
    procedure AfterFrameSent({%H-}fr: TWCRefProtoFrame); override;
  public
    constructor Create(aOwner: TWCRefConnections;
        aOpenMode : TWebSocketUpgradeOptions;
        aSocket: TWCSocketReference;
        aReadData, aSendData: TRefReadSendData); overload;
    destructor Destroy; override;
    procedure ConsumeNextFrame({%H-}Mem : TBufferedStream); override;
    procedure PushFrame(fr : TWCRefProtoFrame); override;
    procedure PushFrame(aOpCode: TWebSocketOpCode;
                        aBuffer: Pointer;
                        aSize: Cardinal;
                        aOwnBuffer: Boolean=true); overload;
    procedure Close(aError : Word; const aReason : UTF8String);
    function  PopReadyChunck : TWCWSIncomingChunck;
    property InflateStream : TThreadSafeInflate read FInflateStream;
    property DeflateStream : TThreadSafeDeflate read FDeflateStream;
    property Options : TWebSocketUpgradeOptions read FOptions;
    property WebSocketSettings : TWCWebSocketSettings read GetWebSocketSettings;
    class function Protocol : TWCProtocolVersion; override;
    class function TryToUpgradeFromHTTP(aReq : TRequest;
                                        aSettings : TWCWebSocketSettings) : TWebSocketUpgradeOptions; virtual;
  end;

var WCWS_MAX_PAYLOAD_SIZE : Int64 = $200000;

implementation

uses wcutils;

const
  WEBSOCKET_INITIAL_READ_BUFFER_SIZE  = $FFFF;
  WEBSOCKET_INITIAL_WRITE_BUFFER_SIZE = $FFFF;
  WEBSOCKET_MAX_WRITE_BUFFER_SIZE     = $9600000;

{ TThreadSafeDeflate }

function TThreadSafeDeflate.Init(windowsize : SmallInt) : Integer;
begin
  Lock;
  try
    if not Initialized then
    begin
      FStream := AllocMem(sizeof(z_stream));
      FStream^.zalloc := nil;
      FStream^.zfree := nil;
      FStream^.opaque := nil;
      FStream^.avail_in := 0;
      FStream^.next_in := nil;

      if windowsize < 0 then FWindowSize := 0 else
                             FWindowSize := windowsize;
      if FWindowSize = 0 then windowsize := 15;
      Result := deflateInit2(FStream^, Z_DEFAULT_COMPRESSION,
                                       Z_DEFLATED, -windowsize,
                                       7, Z_DEFAULT_STRATEGY);
      if Result <> Z_OK then Exit;
    end else
      Result := Z_OK;
  finally
    UnLock;
  end;
end;

function TThreadSafeDeflate.Next(aBuffer : Pointer; Sz : PtrInt; out
  NewCount : PtrInt) : Integer;
const CHUNCK_START_SZ = 1024;
      CHUNCK_MAX_SZ   = 4096;
var have : PtrInt;
begin
  Lock;
  try
    if FWindowSize = 0 then deflateReset(FStream^);
    FStream^.avail_in := Sz;
    FStream^.next_in := aBuffer;

    NewCount := 0;
    if not Assigned(FBuffer) then
    begin
      FBuffer := AllocMem(CHUNCK_START_SZ);
      FBufferSize :=  CHUNCK_START_SZ;
      FBufferInc  :=  CHUNCK_START_SZ;
    end;

    while true do begin
      FStream^.avail_out := FBufferSize;
      FStream^.next_out := @(FBuffer[NewCount]);
      Result := deflate(FStream^,  Z_SYNC_FLUSH);
      if Result = Z_STREAM_ERROR then Exit;
      if not ((Result = Z_BUF_ERROR) or (Result in [Z_OK, Z_STREAM_END])) then
      begin
        Finish;
        Exit;
      end;
      have := FBufferSize - FStream^.avail_out;
      Inc(NewCount, have);
      if (FStream^.avail_out = 0) or (Result = Z_BUF_ERROR) then begin
        FBufferSize := FBufferSize + FBufferInc;
        FBuffer := ReAllocMem(FBuffer, FBufferSize);
        FBufferInc := FBufferInc shl 1;
        if FBufferInc > CHUNCK_MAX_SZ then
           FBufferInc := CHUNCK_MAX_SZ;
      end else begin
        Result := Z_OK;
        Break;
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeDeflate.Finish;
begin
  Lock;
  try
    if Assigned(FStream) then
    begin
      zlib.deflateEnd(FStream^);
      FreeMemAndNil(FStream);
    end;
  finally
    UnLock;
  end;
end;

{ TThreadSafeZlibStream }

function TThreadSafeZlibStream.GetInitialized : Boolean;
begin
  Lock;
  try
    Result := Assigned(FStream);
  finally
    UnLock;
  end;
end;

constructor TThreadSafeZlibStream.Create;
begin
  inherited Create;
  FStream := nil;
  FBuffer := nil;
  FBufferInc := 1;
  FBufferSize := 0;
  FWindowSize := 0;
  FEnabled := false;
end;

destructor TThreadSafeZlibStream.Destroy;
begin
  if Assigned(FBuffer) then
    FreeMemAndNil(FBuffer);
  Finish;
  inherited Destroy;
end;

{ TThreadSafeInflate }

function TThreadSafeInflate.Init(windowsize : SmallInt) : Integer;
begin
  Lock;
  try
    if not Initialized then
    begin
      FStream := AllocMem(sizeof(z_stream));
      FStream^.zalloc := nil;
      FStream^.zfree := nil;
      FStream^.opaque := nil;
      FStream^.avail_in := 0;
      FStream^.next_in := nil;

      if windowsize < 0 then FWindowSize := 0 else
                             FWindowSize := windowsize;
      if FWindowSize = 0 then windowsize := 15;
      Result := inflateInit2(FStream^, -windowsize);
      if Result <> Z_OK then Exit;
    end else
      Result := Z_OK;
  finally
    UnLock;
  end;
end;

function TThreadSafeInflate.Next(aBuffer : Pointer; Sz : PtrInt; out
  NewCount : PtrInt) : Integer;
const CHUNCK_START_SZ = 1024;
      CHUNCK_MAX_SZ   = 65536;
var have : PtrInt;
begin
  Lock;
  try
    if FWindowSize = 0 then inflateReset(FStream^);
    FStream^.avail_in := Sz;
    FStream^.next_in := aBuffer;

    NewCount := 0;
    if not Assigned(FBuffer) then
    begin
      FBuffer := AllocMem(CHUNCK_START_SZ);
      FBufferSize :=  CHUNCK_START_SZ;
      FBufferInc  :=  CHUNCK_START_SZ;
    end;

    while true do begin
      FStream^.avail_out := FBufferSize;
      FStream^.next_out := @(FBuffer[NewCount]);
      Result := inflate(FStream^,  Z_NO_FLUSH);
      if Result = Z_STREAM_ERROR then Exit;
      if not ((Result = Z_BUF_ERROR) or (Result in [Z_OK, Z_STREAM_END])) then
      begin
        Finish;
        Exit;
      end;
      have := FBufferSize - FStream^.avail_out;
      Inc(NewCount, have);
      if (FStream^.avail_out = 0) or (Result = Z_BUF_ERROR) then begin
        FBufferSize := FBufferSize + FBufferInc;
        FBuffer := ReAllocMem(FBuffer, FBufferSize);
        FBufferInc := FBufferInc shl 1;
        if FBufferInc > CHUNCK_MAX_SZ then
           FBufferInc := CHUNCK_MAX_SZ;
      end else begin
        Result := Z_OK;
        Break;
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeInflate.Finish;
begin
  Lock;
  try
    if Assigned(FStream) then
    begin
      zlib.inflateEnd(FStream^);
      FreeMemAndNil(FStream);
    end;
  finally
    UnLock;
  end;
end;

{ TWCWebSocketHelper }

constructor TWCWebSocketHelper.Create;
begin
  inherited Create(wcWebSocket);
  FSettings := TWCWebSocketSettings.Create;
end;

destructor TWCWebSocketHelper.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

function doResponsePMCDeflate(Ext : PWebSocketExt; {%H-}Data : Pointer) : AnsiString;
begin
  Result := WSEX_PMCEDEFLATE;
  if Assigned(Ext^.OptByName(WSEX_PMCED_CLIENT_NO_TAKEOVER)) then
    Result := Result + ';' + WSEX_PMCED_CLIENT_NO_TAKEOVER;
  if Assigned(Ext^.OptByName(WSEX_PMCED_SERVER_NO_TAKEOVER)) then
    Result := Result + ';' + WSEX_PMCED_SERVER_NO_TAKEOVER;
end;

{ TWCWebSocketSettings }

function TWCWebSocketSettings.GetSubProtocols : String;
begin
  Result :=  FSubProtocols.DelimitedText;
end;

procedure TWCWebSocketSettings.SetSubProtocols(const AValue : String);
begin
  FSubProtocols.DelimitedText := AValue;
end;

constructor TWCWebSocketSettings.Create;
begin
  inherited Create;
  FSubProtocols := TThreadStringList.Create;
  FSubProtocols.Delimiter := ',';
  WebSocketRegisterExt(WSEX_PMCEDEFLATE, @doResponsePMCDeflate, Self);
end;

destructor TWCWebSocketSettings.Destroy;
begin
  WebSocketUnregisterAllExts;
  FSubProtocols.Free;
  inherited Destroy;
end;

function TWCWebSocketSettings.DefineSubProtocol(const ClientsList : AnsiString
  ) : AnsiString;
var clientsProtos : TStringList;
    i, j : integer;
    s : AnsiString;
begin
  Result := '';
  if Length(ClientsList) = 0 then Exit;
  clientsProtos := TStringList.Create;
  try
    clientsProtos.Delimiter := ',';
    clientsProtos.DelimitedText := ClientsList;
    for i := 0 to FSubProtocols.Count-1 do
    begin
      s := Trim(FSubProtocols[i]);
      for j := 0 to clientsProtos.Count-1 do
      begin
        if SameStr(s, Trim(clientsProtos[j])) then
        begin
          Result := s;
          Exit;
        end;
      end;
    end;
  finally
    clientsProtos.Free;
  end;
end;

procedure TWCWebSocketSettings.ConfigExtFromJson(const aExt : AnsiString;
  obj : TJSONObject);
var extObj : PWebSocketExtRegistered;
    en : Boolean;
begin
  if not assigned(obj) then exit;
  extObj := WebSocketGetExt(aExt);
  if Assigned(extObj) then
  begin
    en := obj.Get('enabled', true);
    extObj^.Enabled := en;
  end;
end;

{ TWebSocketUpgradeOptions }

function TWebSocketUpgradeOptions.GetOfferExt(index : integer): PWebSocketExt;
begin
  if assigned(FOfferExts) then
    Result := FOfferExts^.Exts^[index] else
    Result := nil;
end;

function TWebSocketUpgradeOptions.GetOfferExtByName(const index : ansistring
  ) : PWebSocketExt;
begin
  if assigned(FOfferExts) then
    Result := FOfferExts^.ExtByName(index) else
    Result := nil;
end;

function TWebSocketUpgradeOptions.GetOfferExtCount: Integer;
begin
  if assigned(FOfferExts) then
    Result := FOfferExts^.Len else
    Result := 0;
end;

function TWebSocketUpgradeOptions.GetResponseExt(index : integer
  ) : PWebSocketExt;
begin
  if assigned(FResponseExts) then
    Result := FResponseExts^.Exts^[index] else
    Result := nil;
end;

function TWebSocketUpgradeOptions.GetResponseExtByName(const index : ansistring
  ) : PWebSocketExt;
begin
  if assigned(FResponseExts) then
    Result := FResponseExts^.ExtByName(index) else
    Result := nil;
end;

function TWebSocketUpgradeOptions.GetResponseExtCount : Integer;
begin
  if assigned(FResponseExts) then
    Result := FResponseExts^.Len else
    Result := 0;
end;

constructor TWebSocketUpgradeOptions.Create(aRequest: TRequest;
  const aSubProtocol: String; aCurVersion: Word; aSettings: TWCWebSocketSettings
  );
begin
  inherited Create;
  FOfferExts := nil;
  FResponseExts := nil;
  FSecWebSocketProtocol_Allow := aRequest.GetCustomHeader(WS_Sec_WebSocket_Protocol);
  FSecWebSocketProtocol := aSubProtocol;
  FSecWebSocketKey := aRequest.GetCustomHeader(WS_Sec_WebSocket_Key);
  FSecWebSocketVersion := aRequest.GetCustomHeader(WS_Sec_WebSocket_Version);
  FNumVersion := aCurVersion;
  FSecWebSocketAccept := GetWebSocketAcceptKey(FSecWebSocketKey);
  //aSettings.Exts | WS_Sec_WebSocket_Extensions
  FSecWebSocketExts := WebSocketGetResponseExt(FOfferExts,
                    aRequest.GetCustomHeader(WS_Sec_WebSocket_Extensions));
  If Length(FSecWebSocketExts) > 0 then
     ParseWebSocketExts(FResponseExts, FSecWebSocketExts);

  FDeflateExt := ResponseExtByName[WSEX_PMCEDEFLATE];

  FInitialRequest := TRequest.Create;
  CopyHTTPRequest(FInitialRequest, aRequest);
end;

destructor TWebSocketUpgradeOptions.Destroy;
begin
  if assigned(FInitialRequest) then FInitialRequest.Free;
  if assigned(FOfferExts) then DoneWebSocketExts(FOfferExts, False);
  if assigned(FResponseExts) then DoneWebSocketExts(FResponseExts, False);
  inherited Destroy;
end;

{ TWCWSFrameHolder }

constructor TWCWSFrameHolder.Create(aHeader: TWCWSFrameHeader;
  aFrame: TWCRefProtoFrame);
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

function TWCWSFrameHolder.Memory : Pointer;
begin
  Result := FFrame.Memory;
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

function TWCWSControlFrame.Memory : Pointer;
begin
  Result := nil;
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

function TWCWSUpgradeHandshakeFrame.Memory : Pointer;
begin
  Result := nil;
end;

{ TWCWSChunck }

function TWCWSChunck.GetOptions : TWebSocketUpgradeOptions;
begin
  Result := FConnection.Options;
end;

constructor TWCWSChunck.Create(aConn : TWCWebSocketConnection;
  aOpCode : TWebSocketOpCode; aRSV : Byte);
begin
  inherited Create;
  FConnection := aConn;
  FOpCode := aOpCode;
  FRSV := aRSV;
end;

destructor TWCWSChunck.Destroy;
begin
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
  if assigned(FHeaderToSend) then FreeMemAndNil(FHeaderToSend);
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

function TWCWSFrameHeaderFrame.Memory : Pointer;
begin
  Result := nil;
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

constructor TWCWSIncomingChunck.Create(aConn : TWCWebSocketConnection;
  aOpCode : TWebSocketOpCode; aRSV : Byte);
begin
  inherited Create(aConn, aOpCode, aRSV);
  FData := TExtMemoryStream.Create;
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
var aDataSize : PtrInt;
begin
  aDataSize := TotalSize;
  //The message is concatenated and complete
  //Working with extensions
  if Connection.InflateStream.Enabled then
  begin
    if ((FRSV and $4) > 0) and (aDataSize > 0) then
    begin
      { Append 4 octets of 0x00 0x00 0xff 0xff to the
        tail end of the payload of the message. }
      FData.SetSize(aDataSize + WSEX_PMCED_FINAL_OCTETS_LEN);
      Move(WSEX_PMCED_FINAL_OCTETS[0],
           Pointer(FData.Memory + aDataSize)^,
           WSEX_PMCED_FINAL_OCTETS_LEN);
      Connection.DecodeStream(FData);
    end;
  end;
  FComplete := true;
end;

procedure TWCWSIncomingChunck.CopyToHTTP1Request(
  aReq1: TWCConnectionRequest);
begin
  aReq1.Method := HTTPGETMethod;
  Options.Lock;
  try
    CopyHTTPRequest(aReq1, Options.Request);
  finally
    Options.UnLock;
  end;
  aReq1.ContentLength := TotalSize;
  aReq1.WCContent.RequestRef := Self;
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
      PByte(wrloc)^ := PByte(loc)^ xor Byte(BEMaskKey and $ff);
      BEMaskKey := ((BEMaskKey and $ffffff) shr 8);
      Inc(PByte(wrloc));
      Inc(PByte(loc));
      Dec(rs);
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

procedure TWCWebSocketConnection.AfterFrameSent(fr : TWCRefProtoFrame);
begin
  // do nothing
end;

function TWCWebSocketConnection.AddNewIncomingChunck(
  aOpCode : TWebSocketOpCode; aRSV : Byte) : TWCWSIncomingChunck;
begin
  Result := TWCWSIncomingChunck.Create(Self, aOpCode, aRSV);
  Result.IncReference;
  FInChuncks.PushChunck(Result);
  Owner.GarbageCollector.Add(Result);
end;

function TWCWebSocketConnection.GetWebSocketSettings: TWCWebSocketSettings;
begin
  Result := TWCWebSocketHelper(Owner.Protocol[wcWebSocket]).Settings;
end;

function TWCWebSocketConnection.InitZLibStream(Strm : TThreadSafeZlibStream;
  aExt : PWebSocketExt) : Integer;
var
  aWindowStr, aTakeOverStr : AnsiString;
  windowsize : Smallint;
  opt : PWebSocketExtOption;
  inExt : PWebSocketExt;
begin
  if not Assigned(aExt) then Exit(Z_VERSION_ERROR);
  if not Strm.Initialized then
  begin
    if Strm is TThreadSafeInflate then
    begin
      aWindowStr := WSEX_PMCED_CLIENT_MAX_WINDOW;
      aTakeOverStr := WSEX_PMCED_CLIENT_NO_TAKEOVER;
    end else
    begin
      aWindowStr := WSEX_PMCED_SERVER_MAX_WINDOW;
      aTakeOverStr := WSEX_PMCED_SERVER_NO_TAKEOVER;
    end;
    opt := aExt^.OptByName(aWindowStr);
    if assigned(opt) then
      windowsize := opt^.AsInteger(15) else
    begin
      windowsize := 0;
      opt := aExt^.OptByName(aTakeOverStr);
      if not Assigned(opt) then
      begin
        inExt := Options.OfferExtByName[WSEX_PMCEDEFLATE];
        if Assigned(inExt) then
        begin
          opt := inExt^.OptByName(aWindowStr);
          if Assigned(opt) then
          begin
            windowsize := opt^.AsInteger(15);
          end else
            windowsize := 15;
        end;
      end;
    end;
    Result := Strm.Init(windowsize);
  end else
    Result := Z_OK;
end;

procedure TWCWebSocketConnection.DecodeStream(aStream : TExtMemoryStream);
var ret : integer;
    NewCount : PtrInt;
begin
  ret := Z_OK;
  FInflateStream.Lock;
  try
    try
      ret := InitZLibStream(FInflateStream, Options.DeflateExt);
      if ret <> Z_OK then Exit;

      try
        ret := FInflateStream.Next(aStream.Memory, aStream.Size, NewCount);
      finally
        if ret = Z_OK then
          aStream.CopyMemory(FInflateStream.Buffer^, NewCount);
      end;
    except
      Close(WSE_WRONG_DATA_FORMAT, '');
    end;
  finally
    FInflateStream.UnLock;
    if ret <> Z_OK then
      Close(WSE_WRONG_DATA_FORMAT, '');
  end;
end;

procedure TWCWebSocketConnection.EncodeFramePayload(aFrame : TWCWSFrameHolder);
var ret      : integer;
    NewCount : PtrInt;
    Stream   : TStream;
    TmpBuf   : Pointer;
    fr       : TWCRefProtoFrame;
begin
  ret := Z_OK;
  FDeflateStream.Lock;
  try
    try
      fr := aFrame.FFrame;
      if not Assigned(fr) then Exit;

      ret := InitZLibStream(FDeflateStream, Options.DeflateExt);
      if ret <> Z_OK then Exit;

      if (fr.Size < 8) or
         ((FDeflateStream.FWindowSize = 0) and
          (fr.Size < 256)) then Exit;

      try
        if Assigned(fr.Memory) then
          ret := FDeflateStream.Next(fr.Memory, fr.Size, NewCount) else
        begin
          Stream := TBufferedStream.Create;
          try
            TmpBuf := GetMem(fr.Size);
            TBufferedStream(Stream).SetPtr(TmpBuf, fr.Size);
            fr.SaveToStream(Stream);
            ret := FDeflateStream.Next(TBufferedStream(Stream).Memory,
                                       fr.Size, NewCount);
          finally
            Stream.Free;
            FreeMemAndNil(TmpBuf);
          end;
        end;
      finally
        if ret = Z_OK then
        begin
          Dec(NewCount, WSEX_PMCED_FINAL_OCTETS_LEN);
          if NewCount > 0 then
          begin
            if (fr is TWCStreamFrame) and
               ((TWCStreamFrame(fr).Stream is TExtMemoryStream) or
                (TWCStreamFrame(fr).Stream is TMemoryStream)) then
            begin
              Stream := TWCStreamFrame(fr).Stream;
            end else
            if fr is TWCStringsFrame then
            begin
              Stream := TWCStringsFrame(fr).Stream;
            end else
            if fr is TWCStringFrame then
            begin
              TWCStringFrame(fr).SetStrLength(NewCount);
              Move(FDeflateStream.Buffer^, fr.Memory^, NewCount);
              Stream := nil;
            end else
            begin
              Fr.Free;
              fr := TWCStreamFrame.Create(nil, NewCount, true);
              Stream := TWCStreamFrame(fr).Stream;
              aFrame.FFrame := fr;
            end;

            if (Stream is TExtMemoryStream) then
              TExtMemoryStream(Stream).CopyMemory(FDeflateStream.Buffer^, NewCount)
            else
            if (Stream is TMemoryStream) then
            begin
              TMemoryStream(Stream).SetSize(NewCount);
              TMemoryStream(Stream).Position := 0;
              TMemoryStream(Stream).WriteBuffer(FDeflateStream.Buffer^, NewCount);
              TMemoryStream(Stream).Position := 0;
            end;

            with aFrame.FHeader do
              Initialize(IsFin, RSVMask or $4, OpCode, IsMasked, fr.Size, 0);
          end;
        end;
      end;
    except
      Close(WSE_WRONG_DATA_FORMAT, '');
    end;
  finally
    FDeflateStream.UnLock;
    if ret <> Z_OK then
      Close(WSE_WRONG_DATA_FORMAT, '');
  end;
end;

constructor TWCWebSocketConnection.Create(aOwner : TWCRefConnections;
  aOpenMode : TWebSocketUpgradeOptions; aSocket : TWCSocketReference;
  aReadData, aSendData : TRefReadSendData);
begin
  inherited Create(aOwner, aSocket, aReadData, aSendData);
  InitializeBuffers;

  FOptions := aOpenMode;
  FInChuncks := TWCWSIncomingChuncks.Create;

  FInflateStream := TThreadSafeInflate.Create;
  FInflateStream.Enabled := Assigned(FOptions.DeflateExt);
  FDeflateStream := TThreadSafeDeflate.Create;
  FDeflateStream.Enabled := Assigned(FOptions.DeflateExt);

  {send handshake}
  if assigned(aOpenMode) then
    PushFrame(TWCWSUpgradeHandshakeFrame.Create(FOptions));
end;

destructor TWCWebSocketConnection.Destroy;
begin
  FInChuncks.Free;
  if assigned(FOptions) then FreeAndNil(FOptions);
  FreeAndNil(FInflateStream);
  FreeAndNil(FDeflateStream);
  inherited Destroy;
end;

procedure TWCWebSocketConnection.ConsumeNextFrame(Mem : TBufferedStream);
var
  Sz, fallbackpos: Int64;
  err, w : Word;
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
      S.SetPtr(ReadBuffer.Value, Sz);

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
              ActualChunck := AddNewIncomingChunck(FrameHeader.OpCode,
                                                   FrameHeader.RSVMask);
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
              S.Read(w, WSP_CLOSE_MIN_SIZE);
              FLastError := BETON(w);
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

procedure TWCWebSocketConnection.PushFrame(fr: TWCRefProtoFrame);
var hfr : TWCWSFrameHeaderFrame;
    aOpCode : TWebSocketOpCode;
begin
  //push header frame first, then payload
  if (fr is TWCWSFrameHeaderFrame) or (fr is TWCWSUpgradeHandshakeFrame) then
    inherited PushFrame(fr) else
  begin
    if (fr is TWCStringFrame) or
       (fr is TWCStringsFrame) then
      aOpCode := WSP_OPCODE_TEXT else
      aOpCode := WSP_OPCODE_BINARY;
    hfr := TWCWSFrameHolder.Create(TWCWSFrameHeader.Create(aOpCode, fr.Size), fr);
    if FDeflateStream.Enabled then
      EncodeFramePayload(TWCWSFrameHolder(hfr));
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

class function TWCWebSocketConnection.TryToUpgradeFromHTTP(aReq: TRequest;
  aSettings: TWCWebSocketSettings): TWebSocketUpgradeOptions;
var subproto : ansistring;
    cur_version : word;
begin
  Result := nil;
  if not SameStr(aReq.Method, HTTPGETMethod) then
    Exit;
  if not (SameStr(aReq.GetCustomHeader(HeaderUpgrade), WebSocket_Protocol) or
          SameStr(aReq.Upgrade, WebSocket_Protocol)) then
    Exit;
  if not WebSocketCheckVersionValid(aReq.GetCustomHeader(WS_Sec_WebSocket_Version),
                                    cur_version) then
    Exit;
  if Length(aReq.GetCustomHeader(WS_Sec_WebSocket_Key)) < WSP_MIN_KEY_LEN then
    Exit;

  subproto := aSettings.DefineSubProtocol(aReq.GetCustomHeader(WS_Sec_WebSocket_Protocol));
  if Length(subproto) = 0 then
    Exit;

  Result := TWebSocketUpgradeOptions.Create(aReq, subproto, cur_version, aSettings);
end;

end.

