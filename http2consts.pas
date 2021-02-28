{
 HTTP2Consts: Basic HTTP2 protocol constants and structures
   according RFC 7540

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit http2consts;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses uhpack;

type
THTTP2StreamState = (h2ssIDLE, h2ssOPEN,
                     h2ssRESERVEDRem,
                     h2ssRESERVEDLoc,
                     h2ssHLFCLOSEDRem,
                     h2ssHLFCLOSEDLoc,
                     h2ssCLOSED);
THTTP2OpenMode = (h2oUpgradeToH2C, h2oUpgradeToH2,
                  h2oPrefaceMode);
					   
{ THTTP2GoawayPayload }

THTTP2GoawayPayload = packed record
private
  field1 : Cardinal; //LastStreamID
  field2 : Cardinal; //ErrorCode
  function GetStrID: Cardinal;
  function GetErrorCode: Cardinal;
  procedure SetStrID(AValue: Cardinal);
  procedure SetErrorCode(AValue: Cardinal);
public
  property LastStreamID : Cardinal read GetStrID write SetStrID;
  property ErrorCode : Cardinal read GetErrorCode write SetErrorCode;
end;
PHTTP2GoawayPayload = ^THTTP2GoawayPayload;

{ THTTP2SettingsBlock }

THTTP2SettingsBlock = packed record
private
  field1 : Word;     // Identifier
  field2 : Cardinal; // Value
  function GetId: Word;
  function GetValue: Cardinal;
  procedure SetId(AValue: Word);
  procedure SetValue(AValue: Cardinal);
public
  property Identifier : Word read GetId write SetId;
  property Value : Cardinal read GetValue write SetValue;
end;

PHTTP2SettingsBlock = ^THTTP2SettingsBlock;

THTTP2SettingsPayload = Array [0..$ff] of THTTP2SettingsBlock;
PHTTP2SettingsPayload = ^THTTP2SettingsPayload;

{ THTTP2RstStreamPayload }

THTTP2RstStreamPayload = packed record
private
  field1  : Cardinal; // ErrorCode
  function GetErrorCode: Cardinal;
  procedure SetErrorCode(AValue: Cardinal);
public
  property ErrorCode : Cardinal read GetErrorCode write SetErrorCode;
end;
PHTTP2RstStreamPayload = ^THTTP2RstStreamPayload;

{ THTTP2PriorityPayload }

THTTP2PriorityPayload = packed record
private
   field1 : Cardinal; // StreamDepend
   function GetStreamDepend : Cardinal;
   procedure SetStreamDepend(AValue : Cardinal);
public
   Weight : Byte;
   property StreamDepend : Cardinal read GetStreamDepend write SetStreamDepend;
end;
PHTTP2PriorityPayload = ^THTTP2PriorityPayload;

THTTP2PayloadHeadPadded = record
   PadLength : Byte;
end;
PHTTP2PayloadHeadPadded = ^THTTP2PayloadHeadPadded;

THTTP2HeadersPayloadHeadPadded = THTTP2PayloadHeadPadded;
PHTTP2HeadersPayloadHeadPadded = ^THTTP2PayloadHeadPadded;
THTTP2HeadersPayloadHeadPriority = THTTP2PriorityPayload;
PHTTP2HeadersPayloadHeadPriority = ^THTTP2PriorityPayload;

THTTP2HeadersPayloadHeadPaddedPriority = packed record
   Padded	 : THTTP2HeadersPayloadHeadPadded;
   Priority	 : THTTP2HeadersPayloadHeadPriority;
end;
PHTTP2HeadersPayloadHeadPaddedPriority = ^THTTP2HeadersPayloadHeadPaddedPriority;

THTTP2DataPayloadHeadPadded = THTTP2PayloadHeadPadded;
PHTTP2DataPayloadHeadPadded = ^THTTP2DataPayloadHeadPadded;

const
  HTTP2VersionId : AnsiString = 'HTTP/2.0';

  HTTP2UpgradeBlockH2C : AnsiString = 'HTTP/1.1 101 Switching Protocols'#13#10+
                                      'Connection: Upgrade'#13#10+
                                      'Upgrade: h2c'#13#10#13#10;
  HTTP2UpgradeBlockH2  : AnsiString = 'HTTP/1.1 101 Switching Protocols'#13#10+
                                      'Connection: Upgrade'#13#10+
                                      'Upgrade: h2'#13#10#13#10;
  HTTP2UpgradeBlockH2CSize : Cardinal = 88;
  HTTP2UpgradeBlockH2Size : Cardinal = 87;

  HTTP2Preface : Array of Byte = ($50,$52,$49,$20,$2a,$20,$48,$54,$54,$50,$2f,
                                  $32,$2e,$30,$0d,$0a,$0d,$0a,$53,$4d,$0d,$0a,
                                  $0d,$0a);								  								  
  H2P_PREFACE_SIZE = 24;
  H2P_FRAME_HEADER_SIZE = 9;
  
  H2P_STREAM_ID_MASK = $7fffffff;
  H2P_PRIORITY_FRAME_SIZE = 5;
  H2P_PRIORITY_WEIGHT_SIZE = 1;
  H2P_RST_STREAM_FRAME_SIZE = 4;
  H2P_PAYLOAD_LEN_SIZE = 3;
  H2P_STREAM_ID_SIZE = 4;
  H2P_ERROR_CODE_SIZE = 4;
  H2P_SETTINGS_BLOCK_SIZE = 6;
  H2P_SETTINGS_ID_SIZE = 2;
  H2P_SETTINGS_VALUE_SIZE = 4;
  H2P_PADDING_OCTET_SIZE = 1;  
  H2P_WINDOW_INC_SIZE = 4;
  H2P_GOAWAY_MIN_SIZE = 8;
  H2P_PING_SIZE = 8;

  H2P_MAX_WINDOW_UPDATE  = Integer($7FFFFFFF);
  H2P_MAX_ENABLE_PUSH    = 1;
  H2P_MAX_MAX_FRAME_SIZE = Integer($FFFFFF);
  H2P_MIN_MAX_FRAME_SIZE = Integer($4000);


  H2FT_DATA                    = Byte($00);
  H2FT_HEADERS                 = Byte($01);
  H2FT_PRIORITY                = Byte($02);
  H2FT_RST_STREAM              = Byte($03);
  H2FT_SETTINGS                = Byte($04);
  H2FT_PUSH_PROMISE            = Byte($05);
  H2FT_PING                    = Byte($06);
  H2FT_GOAWAY                  = Byte($07);
  H2FT_WINDOW_UPDATE           = Byte($08);
  H2FT_CONTINUATION            = Byte($09);
  H2FT_MAX_KNOWN               = H2FT_CONTINUATION;

  H2SET_HEADER_TABLE_SIZE      = Byte($01);
  H2SET_ENABLE_PUSH            = Byte($02);
  H2SET_MAX_CONCURRENT_STREAMS = Byte($03);
  H2SET_INITIAL_WINDOW_SIZE    = Byte($04);
  H2SET_MAX_FRAME_SIZE         = Byte($05);
  H2SET_MAX_HEADER_LIST_SIZE   = Byte($06);
  HTTP2_SETTINGS_MAX           = H2SET_MAX_HEADER_LIST_SIZE;
  HTTP2_SETTINGS_MAX_SIZE      = HTTP2_SETTINGS_MAX * H2P_SETTINGS_BLOCK_SIZE;

  HTTP2_SET_INITIAL_VALUES : Array [1..HTTP2_SETTINGS_MAX] of Cardinal =
                             (4096, 1, $ffffffff, $ffff, H2P_MIN_MAX_FRAME_SIZE, HPACK_MAX_HEADER_SIZE);
							 
  H2FL_END_STREAM              = Byte($01);
  H2FL_ACK                     = Byte($01);
  H2FL_END_HEADERS             = Byte($04);
  H2FL_PADDED                  = Byte($08);
  H2FL_PRIORITY                = Byte($20);							 

  H2E_NO_ERROR            = Byte($00); { Graceful shutdown }
  H2E_PROTOCOL_ERROR      = Byte($01); { Protocol error detected }
  H2E_INTERNAL_ERROR      = Byte($02); { Implementation fault }
  H2E_FLOW_CONTROL_ERROR  = Byte($03); { Flow-control limits exceeded }
  H2E_SETTINGS_TIMEOUT    = Byte($04); { Settings not acknowledged }
  H2E_STREAM_CLOSED       = Byte($05); { Frame received for closed stream }
  H2E_FRAME_SIZE_ERROR    = Byte($06); { Frame size incorrect }
  H2E_REFUSED_STREAM      = Byte($07); { Stream not processed }
  H2E_CANCEL              = Byte($08); { Stream cancelled }
  H2E_COMPRESSION_ERROR   = Byte($09); { Compression state not updated}
  H2E_CONNECT_ERROR       = Byte($0a); { TCP connection error for CONNECT method }
  H2E_ENHANCE_YOUR_CALM   = Byte($0b); { Processing capacity exceeded }
  H2E_INADEQUATE_SECURITY = Byte($0c); { Negotiated TLS parameters not acceptable }
  H2E_HTTP_1_1_REQUIRED   = Byte($0d); { Use HTTP/1.1 for the request}
  H2E_PARSE_ERROR         = Byte($ff);
  H2E_READ_BUFFER_OVERFLOW= Byte($fe);

function Http2IsFrameKnown(aFrameType : Byte) : Boolean;

Implementation

function Http2IsFrameKnown(aFrameType: Byte): Boolean;
begin
  Result := aFrameType <= H2FT_MAX_KNOWN;
end;

{ THTTP2RstStreamPayload }

function THTTP2RstStreamPayload.GetErrorCode: Cardinal;
begin
  Result := BEtoN(field1);
end;

procedure THTTP2RstStreamPayload.SetErrorCode(AValue: Cardinal);
begin
  field1 := NtoBE(AValue);
end;

{ THTTP2PriorityPayload }

function THTTP2PriorityPayload.GetStreamDepend: Cardinal;
begin
  Result := BEtoN(field1);
end;

procedure THTTP2PriorityPayload.SetStreamDepend(AValue: Cardinal);
begin
  field1 := NtoBE(AValue);
end;

{ THTTP2SettingsBlock }

function THTTP2SettingsBlock.GetId: Word;
begin
  Result := BEtoN(field1);
end;

function THTTP2SettingsBlock.GetValue: Cardinal;
begin
  Result := BEtoN(field2);
end;

procedure THTTP2SettingsBlock.SetId(AValue: Word);
begin
  field1 := NtoBE(AValue);
end;

procedure THTTP2SettingsBlock.SetValue(AValue: Cardinal);
begin
  field2 := NtoBE(AValue);
end;

{ THTTP2GoawayPayload }

function THTTP2GoawayPayload.GetStrID: Cardinal;
begin
  Result := BEtoN(field1);
end;

function THTTP2GoawayPayload.GetErrorCode: Cardinal;
begin
  Result := BEtoN(field2);
end;

procedure THTTP2GoawayPayload.SetStrID(AValue: Cardinal);
begin
  field1 := NtoBE(AValue);
end;

procedure THTTP2GoawayPayload.SetErrorCode(AValue: Cardinal);
begin
  field2 := NtoBE(AValue);
end;

end.

