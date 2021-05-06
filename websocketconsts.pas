unit websocketconsts;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses SysUtils, base64, sha1, Classes;

type
TWebSocketOpCode = Byte;
TWebSocketApplayedExt = AnsiString;

{ TWSClosePayload }

TWSClosePayload = packed record
private
  field1 : Word; //ErrorCode
  function GetErrorCode: Word;
  procedure SetErrorCode(AValue: Word);
public
  property ErrorCode : Word read GetErrorCode write SetErrorCode;
end;
PWSClosePayload = ^TWSClosePayload;

TWebSocketExtID = Cardinal;
TWebSocketExtOption = record
  Name  : AnsiString;
  Value : Variant;
end;
PWebSocketExtOption = ^TWebSocketExtOption;
TWebSocketExtOptions = Array [0..255] of PWebSocketExtOption;
PWebSocketExtOptions = ^TWebSocketExtOptions;

{ TWebSocketAppliedExt }

TWebSocketAppliedExt = record
private
  class operator Initialize(var aRec: TWebSocketAppliedExt);
  class operator Finalize(var aRec: TWebSocketAppliedExt);
public
  id : TWebSocketExtID;
  OptionsCount : Integer;
  Options : PWebSocketExtOptions;
end;
PWebSocketAppliedExt = ^TWebSocketAppliedExt;
TWebSocketAppliedExts = Array [0..255] of PWebSocketAppliedExt;
PWebSocketAppliedExts = ^TWebSocketAppliedExts;

TWebSocketFrameHeader = Array [0..13] of Byte;
PWebSocketFrameHeader = ^TWebSocketFrameHeader;

const
WSP_CLOSE_MIN_SIZE          = Cardinal(Sizeof(Word));
WSP_MIN_FRAME_HEADER_SIZE   = 2;
WSP_MAX_FRAME_HEADER_SIZE   = 14;
WSP_MASKING_KEY_SIZE        = 4;
WSP_MIN_KEY_LEN             = 22;


WSP_OPCODE_CONTINUE = Byte($0);
WSP_OPCODE_TEXT     = Byte($1);
WSP_OPCODE_BINARY   = Byte($2);
WSP_OPCODE_CLOSE    = Byte($8);
WSP_OPCODE_PING     = Byte($9);
WSP_OPCODE_PONG     = Byte($A);

WSE_NO_ERROR            = Word($0000);
WSE_NORMAL              = Word($1000);
WSE_GOING_AWAY          = Word($1001);
WSE_PROTOCOL_ERROR      = Word($1002);
WSE_NON_ACCEPTABLE_DATA = Word($1003);
WSE_WRONG_DATA_FORMAT   = Word($1007);
WSE_POLICY_VIOLATION    = Word($1008);
WSE_MESSAGE_TOO_BIG     = Word($1009);
WSE_EXT_EXPECTED        = Word($1010);
WSE_UNEXPECTED          = Word($1011);
WSE_PARSE_ERROR         = Word($1011);//eq  WSE_UNEXPECTED

WebSocket_Protocol = Ansistring('websocket');
WebSocket_Protocol_Version_RFC6455 = 13;
WebSocket_Protocol_Version_RFC6455_Str = '13';
WebSocket_Protocol_Versions_High = 0;
WebSocket_Versions : Array [0..WebSocket_Protocol_Versions_High] of Word =
                     (WebSocket_Protocol_Version_RFC6455);
WebSocket_Versions_Str : Array [0..WebSocket_Protocol_Versions_High] of AnsiString =
                     (WebSocket_Protocol_Version_RFC6455_Str);

WS_Sec_WebSocket_Version    = AnsiString('Sec-WebSocket-Version');
WS_Sec_WebSocket_Key        = AnsiString('Sec-WebSocket-Key');
WS_Sec_WebSocket_Protocol   = AnsiString('Sec-WebSocket-Protocol');
WS_Sec_WebSocket_Extensions = AnsiString('Sec-WebSocket-Extensions');

WebSocket_GUID = AnsiString('258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
WebSocket_Handshake_Response = AnsiString(
                               'HTTP/1.1 101 Switching Protocols'#13#10+
                               'Upgrade: websocket'#13#10+
                               'Connection: Upgrade'#13#10+
                               'Sec-WebSocket-Accept: %s'#13#10+
                               'Sec-WebSocket-Protocol: %s'#13#10+
                               'Sec-WebSocket-Extensions: %s'#13#10#13#10);

function WebSocketIsFrameKnown(aProtoVer : Word;
                               aOpCode : TWebSocketOpCode) : Boolean;
function WebSocketCheckVersionValid(const aProtoVer : AnsiString;
                                          out curVerNum : Word) : Boolean;
function GetWebSocketAcceptKey(const aKey: AnsiString): AnsiString;

implementation

function GetWebSocketAcceptKey(const aKey: AnsiString): AnsiString;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
  sha1: TSHA1Digest;
begin
  sha1 := SHA1String(aKey + WebSocket_GUID);
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.Write(sha1, 20);
    finally
      Encoder.Free;
    end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

function WebSocketIsFrameKnown(aProtoVer : Word;
                               aOpCode: TWebSocketOpCode): Boolean;
begin
  case aProtoVer of
  WebSocket_Protocol_Version_RFC6455 :
    begin
      Result := aOpCode in [WSP_OPCODE_CONTINUE, WSP_OPCODE_TEXT,
                            WSP_OPCODE_BINARY, WSP_OPCODE_CLOSE,
                            WSP_OPCODE_PING, WSP_OPCODE_PONG];
    end;
  else
    Result := false;
  end;
end;

function WebSocketCheckVersionValid(const aProtoVer : AnsiString;
                                          out curVerNum : Word) : Boolean;
var i : Integer;
begin
  for i := 0 to WebSocket_Protocol_Versions_High do
  begin
    if SameText(aProtoVer, WebSocket_Versions_Str[i]) then
    begin
      curVerNum := WebSocket_Versions[i];
      Exit(true);
    end;
  end;
  Result := false;
  curVerNum := 0;
end;

{ TWebSocketAppliedExt }

class operator TWebSocketAppliedExt.Initialize(var aRec : TWebSocketAppliedExt);
begin
  aRec.OptionsCount := 0;
  aRec.Options := nil;
end;

class operator TWebSocketAppliedExt.Finalize(var aRec : TWebSocketAppliedExt);
var i : integer;
begin
  if Assigned(aRec.Options) then
  begin
    for i := 0 to aRec.OptionsCount-1 do
    begin
      FreeMem(aRec.Options^[i]);
    end;
    FreeMem(aRec.Options);
  end;
end;

{ TWSClosePayload }

function TWSClosePayload.GetErrorCode: Word;
begin
  Result := BEtoN(field1);
end;

procedure TWSClosePayload.SetErrorCode(AValue: Word);
begin
  field1 := NtoBE(AValue);
end;

end.

