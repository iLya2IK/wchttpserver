{
 HTTP1Utils: Utilities for working with HTTP1 strings and characters

   Part of WCHTTPServer project

   Copyright (c) 2021 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit HTTP1Utils;

{$mode objfpc}{$H+}

interface

function HTTP1IsTokenChar(C : AnsiChar): Boolean;

const
  HTTP1HeadersAllowed = [$0A,$0D,$20,$21,$24,$25,$27..$39,$40..$5A,$61..$7A];
  HTTP1CHAR = [AnsiChar($00)..AnsiChar($7F)];
  HTTP1UPALPHA  = ['A'..'Z'];
  HTTP1LOALPHA  = ['a'..'z'];
  HTTP1ALPHA    = HTTP1UPALPHA + HTTP1LOALPHA;
  HTTP1DIGIT    = ['0'..'9'];
  HTTP1CTL      = [AnsiChar(0)..AnsiChar(31), AnsiChar(127)];
  HTTP1CR       = AnsiChar(13);
  HTTP1LF       = AnsiChar(10);
  HTTP1SP       = AnsiChar(32);
  HTTP1HT       = AnsiChar(9);
  HTTP1QUOTE    = AnsiChar(34);
  HTTP1CRLF     = AnsiString(#13#10);
  HTTP1Separators = [AnsiChar('('),AnsiChar(')'),
                     AnsiChar('<'),AnsiChar('>'),AnsiChar('@'),
                     AnsiChar(','),AnsiChar(';'),AnsiChar(':'),
                     AnsiChar('\'),AnsiChar('"'),AnsiChar('/'),
                     AnsiChar('['),AnsiChar(']'),AnsiChar('?'),AnsiChar('='),
                     AnsiChar('{'),AnsiChar('}'),
                     AnsiChar(HTTP1SP),AnsiChar(HTTP1HT)];
  HTTPGETMethod  = AnsiString('GET');
  HTTPPOSTMethod = AnsiString('POST');
  HTTPPUTMethod  = AnsiString('PUT');

  HTTPUpgradeSubHeader = AnsiString('upgrade');
  HTTPKeepAliveSubHeader = AnsiString('keep-alive');

Function HTTP1GetStatusCode(ACode: Integer) : String;

implementation

function HTTP1IsTokenChar(C : AnsiChar): Boolean;
begin
  Result := (C in HTTP1CHAR) and not (C in (HTTP1CTL + HTTP1Separators));
end;

Function HTTP1GetStatusCode(ACode: Integer) : String;
begin
  Case ACode of
    100 :  Result:='Continue';
    101 :  Result:='Switching Protocols';
    200 :  Result:='OK';
    201 :  Result:='Created';
    202 :  Result:='Accepted';
    203 :  Result:='Non-Authoritative Information';
    204 :  Result:='No Content';
    205 :  Result:='Reset Content';
    206 :  Result:='Partial Content';
    300 :  Result:='Multiple Choices';
    301 :  Result:='Moved Permanently';
    302 :  Result:='Found';
    303 :  Result:='See Other';
    304 :  Result:='Not Modified';
    305 :  Result:='Use Proxy';
    307 :  Result:='Temporary Redirect';
    400 :  Result:='Bad Request';
    401 :  Result:='Unauthorized';
    402 :  Result:='Payment Required';
    403 :  Result:='Forbidden';
    404 :  Result:='Not Found';
    405 :  Result:='Method Not Allowed';
    406 :  Result:='Not Acceptable';
    407 :  Result:='Proxy Authentication Required';
    408 :  Result:='Request Time-out';
    409 :  Result:='Conflict';
    410 :  Result:='Gone';
    411 :  Result:='Length Required';
    412 :  Result:='Precondition Failed';
    413 :  Result:='Request Entity Too Large';
    414 :  Result:='Request-URI Too Large';
    415 :  Result:='Unsupported Media Type';
    416 :  Result:='Requested range not satisfiable';
    417 :  Result:='Expectation Failed';
    418 :  Result:='I''m a teapot';
    421 :  Result:='Misdirected Request';
    422 :  Result:='Unprocessable Entity';
    423 :  Result:='Locked';
    424 :  Result:='Failed Dependency';
    425 :  Result:='Too Early';
    426 :  Result:='Upgrade Required';
    428 :  Result:='Precondition Required';
    429 :  Result:='Too Many Requests';
    431 :  Result:='Request Header Fields Too Large';
    451 :  Result:='Unavailable For Legal Reasons';

    500 :  Result:='Internal Server Error';
    501 :  Result:='Not Implemented';
    502 :  Result:='Bad Gateway';
    503 :  Result:='Service Unavailable';
    504 :  Result:='Gateway Time-out';
    505 :  Result:='HTTP Version not supported';
  else
    Result:='Unknown status';
  end;
end;

end.

