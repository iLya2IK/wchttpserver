{
 HTTP2Http1Conv:
   HTTP2 to HTTP1.1 and backward conversions
   according RFC 7540, 7541

   Part of WCHTTP2Server project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit http2http1conv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, httpprotocol;

type
  THTTP2Header = (hh2Unknown,
                  hh2Authority,
                  hh2Method,
                  hh2Path,
                  hh2Scheme,
                  hh2Status,
                  hh2Version,
                  //
                  hh2Cookie,
                  hh2SetCookie,
                  //
                  hh2ExtAcceptCAO,
                  hh2ExtContentDisp,
                  hh2ExtRefresh,
                  hh2ExtStrictTransportSec);
  THTTP2HeadersArray = Array [THTTP2Header] of String;

  THTTPHeader = record
    h1 : THeader;
    h2 : THTTP2Header;
  end;

  PHTTPHeader = ^THTTPHeader;

const
  HTTP2HeaderAuthority = ':authority';
  HTTP2HeaderMethod    = ':method';
  HTTP2HeaderPath      = ':path';
  HTTP2HeaderScheme    = ':scheme';
  HTTP2HeaderStatus    = ':status';
  HTTP2HeaderVersion   = ':version';
  // additional headers present in RFC7541 but epscent in httpprotocol unit
  HTTPExtAcceptCAO     = 'access-control-allow-origin';
  HTTPExtContentDisp   = 'content-disposition';
  HTTPExtRefresh       = 'refresh';
  HTTPExtStrictTransportSec = 'strict-transport-security';


HTTP2AddHeaderNames : THTTP2HeadersArray
               = ('',HTTP2HeaderAuthority,
                  HTTP2HeaderMethod, HTTP2HeaderPath,
                  HTTP2HeaderScheme, HTTP2HeaderStatus,
                  HTTP2HeaderVersion,
                  //
                  HeaderCookie,
                  HeaderSetCookie,
                  //
                  HTTPExtAcceptCAO,
                  HTTPExtContentDisp,
                  HTTPExtRefresh,
                  HTTPExtStrictTransportSec);

Function HTTP2HeaderName(AHeader : THTTP2Header) : String;
Function HTTP2HeaderType(const AHeader : String) : THTTP2Header;
function GetHTTPHeaderType(const AHeader: String): PHTTPHeader;

implementation

function HTTP2HeaderName(AHeader: THTTP2Header): String;
begin
  Result:=HTTP2AddHeaderNames[AHeader];
end;

function HTTP2HeaderType(const AHeader: String): THTTP2Header;
begin
  Result:=High(THTTP2Header);
  While (Result>hh2Unknown) and (CompareText(HTTP2AddHeaderNames[Result],AHeader)<>0) do
    Result:=Pred(Result);
end;

var H2H1Headers : TStringToPointerTree;

function GetHTTPHeaderType(const AHeader: String): PHTTPHeader;
begin
  Result := H2H1Headers.Values[AHeader];
end;

function aHeader(ah1 : THeader; ah2 : THTTP2Header) : PHTTPHeader; overload;
begin
  Result := GetMem(Sizeof(THTTPHeader));
  Result^.h1 := ah1;
  Result^.h2 := ah2;
end;

function aHeader(ah1 : THeader) : PHTTPHeader; overload;
begin
  Result := GetMem(Sizeof(THTTPHeader));
  Result^.h1 := ah1;
  Result^.h2 := hh2Unknown;
end;

function aHeader(ah2 : THTTP2Header) : PHTTPHeader; overload;
begin
  Result := GetMem(Sizeof(THTTPHeader));
  Result^.h1 := hhUnknown;
  Result^.h2 := ah2;
end;

procedure InitializeH2H1Headers;
begin
  H2H1Headers := TStringToPointerTree.Create(false);
  H2H1Headers.FreeValues:=True;

  H2H1Headers.Values[HTTP2HeaderAuthority]  := aHeader(hh2Authority);
  H2H1Headers.Values[HTTP2HeaderMethod]     := aHeader(hh2Method);
  H2H1Headers.Values[HTTP2HeaderPath]       := aHeader(hh2Path);
  H2H1Headers.Values[HTTP2HeaderScheme]     := aHeader(hh2Scheme);
  H2H1Headers.Values[HTTP2HeaderStatus]     := aHeader(hh2Status);
  //
  H2H1Headers.Values[HeaderAccept]          := aHeader(hhAccept);
  H2H1Headers.Values[HeaderAcceptCharset]   := aHeader(hhAcceptCharset);
  H2H1Headers.Values[HeaderAcceptEncoding]  := aHeader(hhAcceptEncoding);
  H2H1Headers.Values[HeaderAcceptLanguage]  := aHeader(hhAcceptLanguage);
  H2H1Headers.Values[HeaderAcceptRanges]    := aHeader(hhAcceptRanges);
  H2H1Headers.Values[HeaderAge]             := aHeader(hhAge);
  H2H1Headers.Values[HeaderAllow]           := aHeader(hhAllow);
  H2H1Headers.Values[HeaderAuthorization]   := aHeader(hhAuthorization);
  H2H1Headers.Values[HeaderCacheControl]    := aHeader(hhCacheControl);
  H2H1Headers.Values[HeaderConnection]      := aHeader(hhConnection);
  H2H1Headers.Values[HeaderContentEncoding] := aHeader(hhContentEncoding);
  H2H1Headers.Values[HeaderContentLanguage] := aHeader(hhContentLanguage);
  H2H1Headers.Values[HeaderContentLength]   := aHeader(hhContentLength);
  H2H1Headers.Values[HeaderContentLocation] := aHeader(hhContentLocation);
  H2H1Headers.Values[HeaderContentMD5]      := aHeader(hhContentMD5);
  H2H1Headers.Values[HeaderContentRange]    := aHeader(hhContentRange);
  H2H1Headers.Values[HeaderContentType]     := aHeader(hhContentType);
  H2H1Headers.Values[HeaderDate]            := aHeader(hhDate);
  H2H1Headers.Values[HeaderETag]            := aHeader(hhETag);
  H2H1Headers.Values[HeaderExpires]         := aHeader(hhExpires);
  H2H1Headers.Values[HeaderExpect]          := aHeader(hhExpect);
  H2H1Headers.Values[HeaderFrom]            := aHeader(hhFrom);
  H2H1Headers.Values[HeaderHost]            := aHeader(hhHost);
  H2H1Headers.Values[HeaderIfMatch]         := aHeader(hhIfMatch);
  H2H1Headers.Values[HeaderIfModifiedSince] := aHeader(hhIfModifiedSince);
  H2H1Headers.Values[HeaderIfNoneMatch]     := aHeader(hhIfNoneMatch);
  H2H1Headers.Values[HeaderIfRange]         := aHeader(hhIfRange);
  H2H1Headers.Values[HeaderIfUnModifiedSince] := aHeader(hhIfUnModifiedSince);
  H2H1Headers.Values[HeaderLastModified]    := aHeader(hhLastModified);
  H2H1Headers.Values[HeaderLocation]        := aHeader(hhLocation);
  H2H1Headers.Values[HeaderMaxForwards]     := aHeader(hhMaxForwards);
  H2H1Headers.Values[HeaderPragma]          := aHeader(hhPragma);
  H2H1Headers.Values[HeaderProxyAuthenticate] :=  aHeader(hhProxyAuthenticate);
  H2H1Headers.Values[HeaderProxyAuthorization] := aHeader(hhProxyAuthorization);
  H2H1Headers.Values[HeaderRange]            := aHeader(hhRange);
  H2H1Headers.Values[HeaderReferer]          := aHeader(hhReferer);
  H2H1Headers.Values[HeaderRetryAfter]       := aHeader(hhRetryAfter);
  H2H1Headers.Values[HeaderServer]           := aHeader(hhServer);
  H2H1Headers.Values[HeaderTE]               := aHeader(hhTE);
  H2H1Headers.Values[HeaderTrailer]          := aHeader(hhTrailer);
  H2H1Headers.Values[HeaderTransferEncoding] := aHeader(hhTransferEncoding);
  H2H1Headers.Values[HeaderUpgrade]          := aHeader(hhUpgrade);
  H2H1Headers.Values[HeaderUserAgent]        := aHeader(hhUserAgent);
  H2H1Headers.Values[HeaderVary]             := aHeader(hhVary);
  H2H1Headers.Values[HeaderVia]              := aHeader(hhVia);
  H2H1Headers.Values[HeaderWarning]          := aHeader(hhWarning);
  H2H1Headers.Values[HeaderWWWAuthenticate]  := aHeader(hhWWWAuthenticate);
  //
  H2H1Headers.Values[HeaderXRequestedWith]   := aHeader(hhUnknown);
  H2H1Headers.Values[HeaderCookie]           := aHeader(hh2Cookie);
  H2H1Headers.Values[HeaderSetCookie]        := aHeader(hh2SetCookie);
  //
  H2H1Headers.Values[HTTPExtAcceptCAO]       := aHeader(hh2ExtAcceptCAO);
  H2H1Headers.Values[HTTPExtContentDisp]     := aHeader(hh2ExtContentDisp);
  H2H1Headers.Values[HTTPExtRefresh]         := aHeader(hh2ExtRefresh);
  H2H1Headers.Values[HTTPExtStrictTransportSec] := aHeader(hh2ExtStrictTransportSec);
end;

procedure DisposeH2H1Headers;
begin
  FreeAndNil(H2H1Headers);
end;

initialization
  InitializeH2H1Headers;

finalization
  DisposeH2H1Headers;

end.

