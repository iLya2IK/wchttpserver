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
  HTTPDefs, httpprotocol,
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
function EncodeIntToSID(value : Cardinal; Digits : integer) : String;
function EncodeInt64ToSID(value : QWORD; Digits : integer) : String;
function DecodeSIDToInt(const value : String) : Cardinal;
procedure CopyHTTPRequest(dest, src : TRequest);

Implementation

const sidEncodeTable : Array [0..63] of Char =
                                       ('0','1','2','3','4','5','6','7','8','9',
                                        'a','b','c','d','e','f','g','h','i','j',
                                        'k','l','m','n','o','p','q','r','s','t',
                                        'u','v','w','x','y','z','A','B','C','D',
                                        'E','F','G','H','I','J','K','L','M','N',
                                        'O','P','Q','R','S','T','U','V','W','X',
                                        'Y','Z','~','_');

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

function DecodeJsonParams(const H : String; const P1S, P2S : String;
                              out Par1, Par2 : Variant;
                              const Def1, Def2 : Variant) : Boolean;
var jsonObj: TJSONObject;
  jsonData1, jsonData2 : TJSONData;
begin
  Result := false;
  try
    jsonObj:= TJSONObject(GetJSON(H));
    if assigned(jsonObj) then
    begin
      jsonData1 := jsonObj.Find(P1S);
      jsonData2 := jsonObj.Find(P2S);
      if Assigned(jsonData1) and Assigned(jsonData2) then
      begin
        If jsonData1.JSONType = jtObject then
          Par1 := TJSONObject(jsonData1).AsJSON else
          Par1 := jsonData1.Value;
        if VarIsNull(Par1) then Par1 := Def1;
        If jsonData2.JSONType = jtObject then
          Par2 := TJSONObject(jsonData2).AsJSON else
          Par2 := jsonData2.Value;
        if VarIsNull(Par2) then Par2 := Def2;
      end;
      Result := (((VarIsNumeric(Par2) and VarIsNumeric(Def2)) or
                  (VarIsStr(Par2) and VarIsStr(Def2)))) and
                (((VarIsNumeric(Par1) and VarIsNumeric(Def1)) or
                  (VarIsStr(Par1) and VarIsStr(Def1)))) ;
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
    jsonObj:= TJSONObject(GetJSON(H));
    if assigned(jsonObj) then
    begin
      jsonData := jsonObj.Find(PS);
      if Assigned(jsonData) then
      begin
        If jsonData.JSONType = jtObject then
          Par := TJSONObject(jsonData).AsJSON else
          Par := jsonData.Value;
        if VarIsNull(Par) then Par := Def;
        Result := ((VarIsNumeric(Par) and VarIsNumeric(Def)) or
                   (VarIsStr(Par) and VarIsStr(Def)));
      end;
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
      begin
        If jsonData[i].JSONType = jtObject then
          VALS^[i] := TJSONObject(jsonData[i]).AsJSON else
          VALS^[i] := jsonData[i].Value;
        if VarIsNull(VALS^[i]) then VALS^[i] := Def[i];
        Result := Result and (((VarIsNumeric(VALS^[i]) and VarIsNumeric(Def[i])) or
                               (VarIsStr(VALS^[i]) and VarIsStr(Def[i]))));
      end;
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
      begin
        If jsonData[i].JSONType = jtObject then
          V := TJSONObject(jsonData[i]).AsJSON else
          V := jsonData[i].Value;
        if VarIsNull(V) then V := Def[i];
        Result := Result and (((VarIsNumeric(V) and VarIsNumeric(Def[i])) or
                               (VarIsStr(V) and VarIsStr(Def[i]))));
        if Result then
        begin
          k := VALS.FindIndexOf(I);
          if k < 0 then k := VALS.Add(i, V) else
            VALS.AtPosPt[k]^.Data := V;
        end;
      end;
    end;
  finally
    if assigned(jsonObj) then FreeAndNil(jsonObj);
  end;
end;

end.
