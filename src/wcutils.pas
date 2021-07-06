{
 WCUtils:
   Utilities for working with strings and json parameters

   Part of WCHTTPServer project

   Copyright (c) 2020-2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wcutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils,
  OGLFastVariantHash,
  HTTPDefs, httpprotocol, sockets,
  jsonscanner, jsonparser, fpjson,
  variants;

Type
  TParamsVariantArray = Array [0..99] of Variant;
  PParamsVariantArray = ^TParamsVariantArray;


function DecodeJsonParams(const H : String; const P1S, P2S : String;
                          out Par1, Par2 : Variant;
                          const Def1, Def2 : Variant) : Boolean; overload;
function DecodeJsonParams(const H : String; const PS : String;
                          out Par : Variant;
                          const Def : Variant) : Boolean; overload;
function DecodeJsonParams(const H : String;
                          const PARS : Array of String;
                          VALS : PParamsVariantArray;
                          const Def : Array of Variant) : Boolean; overload;
function DecodeJsonParams(const H : String;
                          const PARS : Array of String;
                          VALS : TFastHashList;
                          const Def : Array of Variant) : Boolean; overload;
procedure DecodeParamsWithDefault(Values : TStrings;
                                  const PARS: array of String;
                                  const H : String;
                                  VALS : TFastHashList;
                                  const Def : Array of Variant);
function EncodeIntToSID(value : Cardinal; Digits : integer) : String;
function EncodeInt64ToSID(value : QWORD; Digits : integer) : String;
function DecodeSIDToInt(const value : String) : Cardinal;
procedure CopyHTTPRequest(dest, src : TRequest);
function WCGetLocalAddress(Handle : Cardinal): sockets.TSockAddr;
Function WCSocketAddrToString(ASocketAddr: TSockAddr): String;
Function WCGetGetRemoteAddress(Handle : Cardinal): sockets.TSockAddr;
Function WCTryStrToHost(const AddrAndPort : AnsiString;
                                  out Addr : AnsiString;
                                  out AddrPort : LongWord) : Boolean;

Implementation

const sidEncodeTable : Array [0..63] of Char =
                                       ('0','1','2','3','4','5','6','7','8','9',
                                        'a','b','c','d','e','f','g','h','i','j',
                                        'k','l','m','n','o','p','q','r','s','t',
                                        'u','v','w','x','y','z','A','B','C','D',
                                        'E','F','G','H','I','J','K','L','M','N',
                                        'O','P','Q','R','S','T','U','V','W','X',
                                        'Y','Z','.','_');

function EncodeIntToSID(value : Cardinal; Digits : integer) : String;
var i: integer;
begin
 If Digits=0 then
   Digits:=1;
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := sidEncodeTable[value and 63];
   value := value shr 6;
  end ;
 while value <> 0 do begin
   result := sidEncodeTable[value and 63] + result;
   value := value shr 6;
 end;
end;

function EncodeInt64ToSID(value : QWORD; Digits : integer) : String;
var i: integer;
begin
 If Digits=0 then
   Digits:=1;
 SetLength(result, digits);
 for i := 0 to digits - 1 do
  begin
   result[digits - i] := sidEncodeTable[value and 63];
   value := value shr 6;
  end ;
 while value <> 0 do begin
   result := sidEncodeTable[value and 63] + result;
   value := value shr 6;
 end;
end;

function DecodeSIDToInt(const value : String) : Cardinal;
var i, k : integer;
    C : AnsiChar;
begin
  Result := 0;
  k := 0;
  for i := Length(value) downto 1 do
  begin
    C := value[i];
    if (C >= '0') and (C <= '9') then Result := Result or ((Ord(C) - Ord('0')) shl k) else
    if (C >= 'a') and (C <= 'z') then Result := Result or ((Ord(C) - Ord('a') + 10) shl k) else
    if (C >= 'A') and (C <= 'Z') then Result := Result or ((Ord(C) - Ord('A') + 36) shl k) else
    if C = '~' then Result := Result or (62 shl k) else
    if C = '_' then Result := Result or (63 shl k);
    Inc(k, 6);
  end;
end;

function WCGetLocalAddress(Handle : Cardinal): sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(sockets.TSockAddr);
  if fpGetSockName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function WCGetGetRemoteAddress(Handle : Cardinal): sockets.TSockAddr;
var
  len: LongInt;
begin
  len := SizeOf(sockets.TSockAddr);
  if fpGetPeerName(Handle, @Result, @len) <> 0 then
    FillChar(Result, SizeOf(Result), 0);
end;

function WCTryStrToHost(const AddrAndPort : AnsiString; out Addr : AnsiString;
  out AddrPort : LongWord) : Boolean;
var p : longint;
    S : AnsiString;
begin
  if length(AddrAndPort) = 0 then Exit(false);
  p := Pos(':', AddrAndPort);
  if P > 0 then begin
    Addr := Copy(AddrAndPort, 1, P - 1);
    S := Copy(AddrAndPort, P + 1, Length(AddrAndPort));
    if TryStrToDWord(S, AddrPort) then
    begin
      Exit(true);
    end else
      Exit(false);
  end else
  begin
    Addr := AddrAndPort;
    Result := True;
  end;
end;

Function WCSocketAddrToString(ASocketAddr: TSockAddr): String;
begin
  if ASocketAddr.sa_family = AF_INET then
    Result := NetAddrToStr(ASocketAddr.sin_addr)
  else // no ipv6 support yet
    Result := '';
end;

procedure CopyHTTPRequest(dest, src : TRequest);
var h : THeader;
begin
  if not Assigned(src) then Exit;
  dest.URL := src.URL;
  dest.PathInfo := src.PathInfo;
  dest.QueryString := src.QueryString;
  for h := hhAccept to high(THeader) do
  begin
    dest.SetHeader(h, src.GetHeader(h));
  end;
  dest.CookieFields.Assign(src.CookieFields);
  dest.CustomHeaders.Assign(src.CustomHeaders);
end;

function CheckJsonDataParam(jsonData : TJSONData; out V : Variant;
                                     const Def : Variant) : Boolean; inline;
begin
  if not assigned(jsonData) then Exit(False);
  If jsonData.JSONType = jtObject then
    V := TJSONObject(jsonData).AsJSON else
    V := jsonData.Value;
  if VarIsNull(V) then V := Def;
  Result := (((VarIsNumeric(V) and VarIsNumeric(Def)) or
             (VarIsStr(V) and VarIsStr(Def))));
end;

function DecodeJsonParams(const H : String; const P1S, P2S : String;
                              out Par1, Par2 : Variant;
                              const Def1, Def2 : Variant) : Boolean;
var jsonObj: TJSONObject;
    jsonData1, jsonData2 : TJSONData;
begin
  Result := false;
  try
    try
      jsonObj:= TJSONObject(GetJSON(H));
      if assigned(jsonObj) then
      begin
        jsonData1 := jsonObj.Find(P1S);
        jsonData2 := jsonObj.Find(P2S);
        if Assigned(jsonData1) and Assigned(jsonData2) then
          Result := CheckJsonDataParam(jsonData1, Par1, Def1) and
                    CheckJsonDataParam(jsonData2, Par2, Def2);
      end;
    except
      //do nothing : Result := false;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

function DecodeJsonParams(const H : String; const PS: String; out
  Par: Variant; const Def: Variant) : Boolean;
var jsonObj: TJSONObject;
    jsonData : TJSONData;
begin
  Result := false;
  try
    try
      jsonObj:= TJSONObject(GetJSON(H));
      if assigned(jsonObj) then
      begin
        jsonData := jsonObj.Find(PS);
        if Assigned(jsonData) then
          Result := CheckJsonDataParam(jsonData, Par, Def);
      end;
    except
      //do nothing : Result := false;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

function DecodeJsonParams(const H : String; const PARS : array of String;
  VALS : PParamsVariantArray; const Def : array of Variant) : Boolean;
var jsonObj: TJSONObject;
    jsonData : Array of TJSONData;
    i : integer;
begin
  Result := false;
  if Length(PARS) <> Length(Def) then Exit;
  try
    try
      jsonObj:= TJSONObject(GetJSON(H));
      if assigned(jsonObj) then
      begin
        SetLength(jsonData, Length(PARS));
        for i := 0 to High(Pars) do
        begin
          jsonData[i] := jsonObj.Find(PARS[i]);
          if not Assigned(jsonData[i]) then Exit;
        end;
        Result := true;
        for i := 0 to High(Pars) do
        if Result then
          Result := CheckJsonDataParam(jsonData[i], VALS^[i], Def[i]);
      end;
    except
      //do nothing : Result := false;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

function DecodeJsonParams(const H: String; const PARS: array of String;
  VALS: TFastHashList; const Def: array of Variant): Boolean;
var jsonObj: TJSONObject;
    jsonData : Array of TJSONData;
    i, k : integer;
    V : Variant;
begin
  Result := false;
  if Length(PARS) <> Length(Def) then Exit;
  try
    try
      jsonObj:= TJSONObject(GetJSON(H));
      if assigned(jsonObj) then
      begin
        SetLength(jsonData, Length(PARS));
        for i := 0 to High(Pars) do
        begin
          jsonData[i] := jsonObj.Find(PARS[i]);
          if not Assigned(jsonData[i]) then Exit;
        end;

        for i := 0 to High(Pars) do
        begin
          Result := CheckJsonDataParam(jsonData[i], V, Def[i]);

          if Result then
          begin
            k := VALS.FindIndexOf(i);
            if k < 0 then k := VALS.Add(i, V) else
              VALS.AtPosPt[k]^.Data := V;
          end else
            Break;
        end;
      end;
    except
      //do nothing : Result := false;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

function StrToVariant(const Str : String) : Variant;
var B : Boolean;
    I : Integer;
    I64 : Int64;
    F : Double;
begin
  if TryStrToInt(Str, I) then
     Result := I else
  if TryStrToInt64(Str, I64) then
     Result := I64 else
  if TryStrToFloat(Str, F) then
     Result := F else
  if TryStrToBool(Str, B) then
     Result := B else
     Result := Str;
end;

procedure DecodeParamsWithDefault(Values : TStrings;
  const PARS : array of String; const H : String; VALS : TFastHashList;
  const Def : array of Variant);
var jsonObj: TJSONObject;
    jsonData : Array of TJSONData;
    i, k : integer;
    V : Variant;
begin
  if Length(PARS) <> Length(Def) then Exit;
  try
    try
      jsonObj:= TJSONObject(GetJSON(H));
    except
      //do nothing
      jsonObj := nil;
    end;
    SetLength(jsonData, Length(PARS));

    if Assigned(jsonObj) then
    for i := 0 to High(Pars) do
       jsonData[i] := jsonObj.Find(PARS[i]);

    for i := 0 to High(Pars) do
    begin
      if not CheckJsonDataParam(jsonData[i], V, Def[i]) then
      begin
        if assigned(Values) then
        begin
          if Length(Values.Values[PARS[i]]) > 0 then
          begin
            V := StrToVariant( Values.Values[PARS[i]] );
            if not (((VarIsNumeric(V) and VarIsNumeric(Def[i])) or
                    (VarIsStr(V) and VarIsStr(Def[i])))) then
              V := Def[i];
          end else
            V := Def[i];
        end else
          V := Def[i];
      end;
      k := VALS.FindIndexOf(i);
      if k < 0 then k := VALS.Add(i, V) else
        VALS.AtPosPt[k]^.Data := V;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

end.
