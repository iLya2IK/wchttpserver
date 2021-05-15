unit http1utils;

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

implementation

function HTTP1IsTokenChar(C : AnsiChar): Boolean;
begin
  Result := (C in HTTP1CHAR) and not (C in (HTTP1CTL + HTTP1Separators));
end;

end.

