{
 WebSocketConsts: Basic websocket protocol constants, structures and utils
   according RFC 6455, RFC 7692

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit websocketconsts;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses SysUtils, base64, sha1, Classes, Variants;

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

{ TWebSocketExtOption }

TWebSocketExtOption = record
private
  FName  : PChar;
  FValue : PVariant;
  function GetName : String;
  function GetValue : Variant;
  procedure SetName(AValue : String);
  procedure SetValue(AValue : Variant);
public
  procedure Init;
  procedure Done;
  procedure SetVariantStr(const Str : String);
  property Name : String read GetName write SetName;
  property Value : Variant read GetValue write SetValue;
  function AsInteger(aDefault : Integer) : Integer;
  function AsString(const aDefault: AnsiString): AnsiString;
end;
PWebSocketExtOption = ^TWebSocketExtOption;
TWebSocketExtOptions = Array [0..255] of PWebSocketExtOption;
PWebSocketExtOptions = ^TWebSocketExtOptions;

{ TWebSocketExt }

TWebSocketExt = record
private
  class operator Initialize(var aRec: TWebSocketExt);
  class operator Finalize(var aRec: TWebSocketExt);
public
  id : TWebSocketExtID;
  Temp : Boolean;
  OptionsCount : Integer;
  Options : PWebSocketExtOptions;
  procedure Init;
  procedure Done;
  procedure PushOption(Opt : PWebSocketExtOption);
  function  OptByName(const aName : AnsiString) : PWebSocketExtOption;
  function Name : AnsiString;
end;
PWebSocketExt = ^TWebSocketExt;
TWebSocketExtList = Array [0..255] of PWebSocketExt;
PWebSocketExtList = ^TWebSocketExtList;

{ TWebSocketExts }

TWebSocketExts = packed record
private
  class operator Initialize(var aRec: TWebSocketExts);
  class operator Finalize(var aRec: TWebSocketExts);
public
  Len : Integer;
  Exts : PWebSocketExtList;
  procedure Init;
  procedure Done(OnlyTemp : Boolean);
  procedure PushExt(Ext : PWebSocketExt);
  function  ExtByName(const aName : AnsiString) : PWebSocketExt;
end;
PWebSocketExts = ^TWebSocketExts;

PWebSocketExtCallback = function (aIncomingExt : PWebSocketExt;
                                  aData        : Pointer) : AnsiString;

TWebSocketExtRegistered = packed record
  ID       : TWebSocketExtID;
  Name     : AnsiString;
  Callback : PWebSocketExtCallback;
  Data     : Pointer;
  Enabled  : Boolean;
end;
PWebSocketExtRegistered = ^TWebSocketExtRegistered;

TWebSocketFrameHeader = Array [0..13] of Byte;
PWebSocketFrameHeader = ^TWebSocketFrameHeader;

const
WSP_CLOSE_MIN_SIZE          = Cardinal(Sizeof(Word));
WSP_MIN_FRAME_HEADER_SIZE   = 2;
WSP_MAX_FRAME_HEADER_SIZE   = 14;
WSP_MASKING_KEY_SIZE        = 4;
WSP_MIN_KEY_LEN             = 22;

WSEX_MIN_EXTS_SIZE          = SizeOf(Integer) + SizeOf(PWebSocketExtList);
WSEX_NOTREGISTERED          = 0;

WSEX_PMCEDEFLATE              = AnsiString('permessage-deflate');
WSEX_PMCED_SERVER_NO_TAKEOVER = AnsiString('server_no_context_takeover');
WSEX_PMCED_CLIENT_NO_TAKEOVER = AnsiString('client_no_context_takeover');
WSEX_PMCED_SERVER_MAX_WINDOW  = AnsiString('server_max_window_bits');
WSEX_PMCED_CLIENT_MAX_WINDOW  = AnsiString('client_max_window_bits');
WSEX_PMCED_FINAL_OCTETS       : Array [0..3] of Byte = ($00, $00, $FF, $FF);
WSEX_PMCED_FINAL_OCTETS_LEN   = 4;

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

//WebSocket extensions
function  InitWebSocketExts : PWebSocketExts;
procedure DoneWebSocketExts(var aExts : PWebSocketExts; OnlyTemp : Boolean);
procedure ParseWebSocketExts(var aExts : PWebSocketExts;
                                          const aStringToParse : AnsiString);
procedure PushWebSocketExt(var aExts : PWebSocketExts; aExt : PWebSocketExt);
function  WebSocketGetExt(const Str : AnsiString) : PWebSocketExtRegistered;
function  WebSocketGetExtId(const Str : AnsiString) : TWebSocketExtID;
procedure WebSocketRegisterExt(const aName : AnsiString;
                                          aCallback : PWebSocketExtCallback;
                                          aData : Pointer);
function WebSocketGetResponseExt(var aInputExts : PWebSocketExts;
                                     const aIncoming : AnsiString) : AnsiString;
procedure WebSocketUnregisterAllExts;

implementation

uses http1utils;

var ExtsRegistered : Array of TWebSocketExtRegistered;

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
      Encoder.Write(PByte(@sha1)^, 20);
    finally
      Encoder.Free;
    end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

function InitWebSocketExts: PWebSocketExts;
begin
  Result := AllocMem(WSEX_MIN_EXTS_SIZE);
  Result^.Init;
end;

procedure DoneWebSocketExts(var aExts: PWebSocketExts; OnlyTemp: Boolean);
begin
  if not Assigned(aExts) then Exit;
  aExts^.Done(OnlyTemp);
  FreeMemAndNil(aExts);
end;

procedure ParseWebSocketExts(var aExts : PWebSocketExts;
  const aStringToParse : AnsiString);
const EXT_SEPS = [',',';',#0];
      EXT_STOP = [',',#0];
Var
  P : Integer;
  S, V : AnsiString;
  C : AnsiChar;
  ParseStep : Byte;
  Normal, StrFinished, EscChar : Boolean;
  Ext : PWebSocketExt;
  Opt : PWebSocketExtOption;
begin
  if not Assigned(aExts) then
    aExts := InitWebSocketExts;

  try
    S := aStringToParse + #0;
    ParseStep := 0;
    V := '';
    P := 1;
    Normal := true;
    StrFinished := false;
    Ext := nil;
    Opt := nil;
    While (Length(S) >= P) and (Normal) do
    begin
      C := S[P];
      if (C <> ' ') or ((ParseStep = 4) and not StrFinished) then
      begin
        case ParseStep of
        0 : begin //searching token (extension-token)
              if not assigned(Ext) then
              begin
                Ext := GetMem(Sizeof(TWebSocketExt));
                Ext^.Init;
              end;
              if HTTP1IsTokenChar(C) then
              begin
                V := V + C;
              end else
              if C in EXT_SEPS then
              begin
                Ext^.id := WebSocketGetExtId(V);
                V := '';
                if Ext^.id = WSEX_NOTREGISTERED then
                begin
                  FreeMemAndNil(Ext);
                end else
                if C in EXT_STOP then
                begin
                  aExts^.PushExt(Ext);
                  Ext := nil;
                  if C = #0 then
                    ParseStep := 5
                  //ParseStep eq 0, search next extension-token
                end
                else
                  ParseStep := 1;
              end else
                Normal := false;
            end;
        1 : begin
              if not assigned(Opt) then begin
                Opt := GetMem(Sizeof(TWebSocketExtOption));
                Opt^.Init;
              end;
              if HTTP1IsTokenChar(C) then
              begin
                V := V + C;
              end else
              if C in ['=',',',';',#0] then
              begin
                Opt^.Name := V;
                V := '';
                if C = '=' then
                  ParseStep := 2
                else
                begin
                  Opt^.Value := null;
                  Ext^.PushOption(Opt);
                  Opt := nil;
                  if C in EXT_STOP then
                  begin
                    aExts^.PushExt(Ext);
                    Ext := nil;
                    if C = #0 then
                      ParseStep := 5
                    else
                      ParseStep := 0;
                  end else
                    ParseStep := 1;
                end;
              end else
                Normal := false
            end;
        2 : begin
              if HTTP1IsTokenChar(C) then
              begin
                V := V + C;
                ParseStep := 3;
              end else
              if C = '"' then
              begin
                EscChar := false;
                StrFinished := false;
                ParseStep := 4;
              end else
                Normal := false
            end;
        3 : begin
              if HTTP1IsTokenChar(C) then
              begin
                V := V + C;
              end else
              if C in EXT_SEPS then
              begin
                Opt^.SetVariantStr( V );
                V := '';
                Ext^.PushOption(Opt);
                Opt := nil;
                if C in EXT_STOP then
                begin
                  aExts^.PushExt(Ext);
                  Ext := nil;
                  if C = #0 then
                    ParseStep := 5
                  else
                    ParseStep := 0;
                end else
                  ParseStep := 1;
              end else
                Normal := false;
            end;
        4 : begin
              if (C <> '"') and (not StrFinished) then
              begin
                if C = '\' then
                begin
                  if EscChar then
                  begin
                    V := V + C;
                    EscChar := false;
                  end else
                    EscChar := true;
                end else
                  V := V + C;
              end else
              if C in EXT_SEPS then
              begin
                Opt^.SetVariantStr( V );
                V := '';
                Ext^.PushOption(Opt);
                Opt := nil;
                if C in EXT_STOP then
                begin
                  aExts^.PushExt(Ext);
                  Ext := nil;
                  if C = #0 then
                    ParseStep := 5
                  else
                    ParseStep := 0;
                end else
                  ParseStep := 1;
              end else
              if C = '"' then
              begin
                if EscChar then
                begin
                  V := V + C;
                  EscChar := false;
                end else
                begin
                  if StrFinished then
                    Normal := false
                  else
                    StrFinished := true;
                end;
              end else
                Normal := false;
            end;
        end;
      end;

      Inc(P);
    end;
  if Normal then
  begin
    if Assigned(Ext) then
    begin
      if Assigned(Opt) then
      begin
        if Length(V) > 0 then
          Opt^.Value := V else
          Opt^.Value := null;
        Ext^.PushOption(Opt);
        Opt := nil;
      end;
      PushWebSocketExt(aExts, Ext);
      Ext := nil;
    end;
  end;
  finally
    if Assigned(Ext) then begin
      Ext^.Done;
      FreeMemAndNil(Ext);
    end;
    if Assigned(Opt) then begin
      Opt^.Done;
      FreeMemAndNil(Opt);
    end;
  end;
end;

procedure PushWebSocketExt(var aExts : PWebSocketExts; aExt : PWebSocketExt);
begin
  if not Assigned(aExts) then aExts := InitWebSocketExts;
  aExts^.PushExt(aExt);
end;

function WebSocketGetExt(const Str : AnsiString) : PWebSocketExtRegistered;
var i : integer;
begin
  for i := 0 to High(ExtsRegistered) do
  begin
    if SameStr(Str, ExtsRegistered[i].Name) then
    begin
      Exit(@(ExtsRegistered[i]));
    end;
  end;
  Result := nil;
end;

function WebSocketGetExtId(const Str : AnsiString) : TWebSocketExtID;
var extObj : PWebSocketExtRegistered;
begin
  extObj := WebSocketGetExt(Str);
  if assigned(extObj) and extObj^.Enabled then
    Result := extObj^.ID else
    Result := WSEX_NOTREGISTERED;
end;

procedure WebSocketRegisterExt(const aName : AnsiString;
  aCallback : PWebSocketExtCallback; aData : Pointer);
var
  extObj : PWebSocketExtRegistered;
  aid : TWebSocketExtID;
begin
  extObj := WebSocketGetExt(aName);
  if not Assigned(extObj) then
  begin
    aid := Length(ExtsRegistered) + 1;
    SetLength(ExtsRegistered, aid);
    with ExtsRegistered[aid - 1] do
    begin
      ID := aid;
      Name := aName;
      Callback := aCallback;
      Data := aData;
      Enabled := true;
    end;
  end else
  begin
    extObj^.Callback := aCallback;
    extObj^.Data := aData;
  end;
end;

function WebSocketGetResponseExt(var aInputExts : PWebSocketExts;
                                     const aIncoming : AnsiString) : AnsiString;
var i : integer;
    S : AnsiString;
begin
  if Length(aIncoming) > 0 then
  begin
    DoneWebSocketExts(aInputExts, false);
    ParseWebSocketExts(aInputExts, aIncoming);
  end;
  if not Assigned(aInputExts) then Exit('');

  Result := '';
  for i := 0 to aInputExts^.Len-1 do
  with ExtsRegistered[aInputExts^.Exts^[i]^.id - 1] do
  begin
    if Assigned(Callback) then
    begin
      S := Callback(aInputExts^.Exts^[i], Data);
      if Length(S) > 0 then
      begin
        if Length(Result) > 0 then Result := Result + ',' + S else
                                   Result := S;
      end;
    end;
  end;
end;

procedure WebSocketUnregisterAllExts;
begin
  SetLength(ExtsRegistered, 0);
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

{ TWebSocketExtOption }

function TWebSocketExtOption.GetName : String;
var Sz : Cardinal;
begin
  Sz := PCardinal(PByte(FName) - SizeOf(Cardinal))^ - SizeOf(Cardinal);
  SetLength(Result, Sz);
  Move(FName^, Result[1], Sz);
end;

function TWebSocketExtOption.GetValue : Variant;
begin
  Result := FValue^;
end;

procedure TWebSocketExtOption.SetName(AValue : String);
var Sz : Cardinal;
begin
  if Assigned(FName) then StrDispose(FName);
  Sz := Length(aValue);
  FName := StrAlloc(Sz);
  Move(AValue[1], PByte(FName)^, Sz);
end;

procedure TWebSocketExtOption.SetValue(AValue : Variant);
begin
  FValue^ := AValue;
end;

procedure TWebSocketExtOption.Init;
begin
  FName := StrAlloc(0);
  FValue := AllocMem(SizeOf(Variant));
end;

procedure TWebSocketExtOption.Done;
begin
  StrDispose(FName);
  FValue^ := null;
  Dispose(FValue);
end;

procedure TWebSocketExtOption.SetVariantStr(const Str : String);
var B : Boolean;
    I : Integer;
    I64 : Int64;
    F : Double;
begin
  if TryStrToInt(Str, I) then
     FValue^ := I else
  if TryStrToInt64(Str, I64) then
    FValue^ := I64 else
  if TryStrToFloat(Str, F) then
     FValue^ := F else
  if TryStrToBool(Str, B) then
    FValue^ := B else
    FValue^ := Str;
end;

function TWebSocketExtOption.AsInteger(aDefault: Integer): Integer;
begin
  if VarIsNull(FValue^) or (not VarIsOrdinal(FValue^)) then
     Result := aDefault else
     Result := FValue^;
end;

function TWebSocketExtOption.AsString(const aDefault: AnsiString): AnsiString;
begin
  if VarIsNull(FValue^) then Result := aDefault else
     Result := VarToStr(FValue^);
end;

{ TWebSocketExts }

class operator TWebSocketExts.Initialize(var aRec : TWebSocketExts);
begin
  aRec.Init;
end;

class operator TWebSocketExts.Finalize(var aRec : TWebSocketExts);
begin
  aRec.Done(false);
end;

procedure TWebSocketExts.Init;
begin
  Len := 0;
  Exts := nil;
end;

procedure TWebSocketExts.Done(OnlyTemp : Boolean);
var i : integer;
begin
  if Assigned(Exts) then
  begin
    for i := 0 to Len-1 do
    begin
      if (Exts^[i]^.Temp) or (not OnlyTemp) then
      begin
       Exts^[i]^.Done;
       FreeMem(Exts^[i]);
      end;
    end;
    FreeMem(Exts);
  end;
end;

procedure TWebSocketExts.PushExt(Ext : PWebSocketExt);
begin
  if not Assigned(Exts) then
  begin
    Exts := AllocMem(SizeOf(PWebSocketExt));
  end else
    Exts := ReAllocMem(Exts, Sizeof(PWebSocketExt) * (Len + 1));
  Exts^[Len] := Ext;
  Inc(Len);
end;

function TWebSocketExts.ExtByName(const aName : AnsiString) : PWebSocketExt;
var i : integer;
begin
  for i := 0 to Len - 1 do
  begin
    if SameStr(aName, Exts^[i]^.Name) then
    begin
      Exit(Exts^[i]);
    end;
  end;
  Result := nil;
end;

{ TWebSocketExt }

class operator TWebSocketExt.Initialize(var aRec : TWebSocketExt);
begin
  aRec.Init;
end;

class operator TWebSocketExt.Finalize(var aRec : TWebSocketExt);
begin
  aRec.Done;
end;

procedure TWebSocketExt.Init;
begin
  Temp := false;
  OptionsCount := 0;
  Options := nil;
end;

procedure TWebSocketExt.Done;
var i : integer;
begin
  if Assigned(Options) then
  begin
    for i := 0 to OptionsCount-1 do
    begin
      Options^[i]^.Done;
      FreeMem(Options^[i]);
    end;
    FreeMem(Options);
  end;
end;

procedure TWebSocketExt.PushOption(Opt : PWebSocketExtOption);
begin
  if not Assigned(Options) then
  begin
    Options := AllocMem(SizeOf(PWebSocketExtOption));
  end else
    Options := ReAllocMem(Options, Sizeof(PWebSocketExtOption) * (OptionsCount + 1));
  Options^[OptionsCount] := Opt;
  Inc(OptionsCount);
end;

function TWebSocketExt.OptByName(const aName : AnsiString
  ) : PWebSocketExtOption;
var i : integer;
begin
  for i := 0 to OptionsCount-1 do
  begin
    if SameStr(aName, Options^[i]^.Name) then
    begin
      Exit(Options^[i]);
    end;
  end;
  Result := nil;
end;

function TWebSocketExt.Name : AnsiString;
begin
  Result := ExtsRegistered[Id - 1].Name;
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

